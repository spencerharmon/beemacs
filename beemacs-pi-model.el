;;; beemacs-pi-model.el --- Provider/model selection for the pi harness -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; pi supports 15+ providers (anthropic, openai, google, ...), each offering
;; several models, and never hardcodes a single "the" model.  This module
;; queries a running/short-lived `pi' RPC process (`beemacs-pi.el') for its
;; currently available providers/models, offers a `completing-read'
;; selection over the flattened "PROVIDER/MODEL" set, and persists the
;; chosen provider/model as this install's default -- while still letting a
;; single `beemacs-pi-chat-mode' buffer override that default for just its
;; own session.
;;
;; RPC protocol (newline-delimited JSON, matching `beemacs-pi-sessions.el's
;; `list_sessions'/`session_list' convention):
;;
;;   {"type":"list_models"}                              (request)
;;   {"type":"model_list",
;;    "providers":[{"provider":"anthropic",
;;                  "models":["claude-opus-4","claude-sonnet-4"]},
;;                 {"provider":"openai",
;;                  "models":["gpt-5"]}]}                 (response)
;;
;; `beemacs-pi-model-list' spawns a short-lived RPC process (via
;; `beemacs-pi-start'), sends `list_models', waits synchronously for the
;; `model_list' reply, tears the process down, and returns the parsed
;; `providers' vector converted to a list of alists.
;;
;; `beemacs-pi-model-select' flattens that list to "PROVIDER/MODEL"
;; candidate strings, prompts via `completing-read', and either:
;;   - records the selection as this buffer's per-session override (when
;;     called from a `beemacs-pi-chat-mode' buffer, or with a non-nil
;;     SESSION-ONLY argument), or
;;   - persists it as the per-install default (`beemacs-pi-model-persist-file'),
;;     overwriting whatever default was there before.
;;
;; `beemacs-pi-model-current' returns the effective provider/model cons for
;; the current context: the current buffer's session override if set,
;; otherwise the persisted per-install default, otherwise nil (meaning "let
;; pi pick its own default").

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'beemacs-pi)
(require 'beemacs-persistence)

(defgroup beemacs-pi-model nil
  "Provider/model selection for the pi harness."
  :group 'beemacs
  :prefix "beemacs-pi-model-")

(defcustom beemacs-pi-model-persist-file
  (expand-file-name "beemacs-pi-model-default.el" user-emacs-directory)
  "File used to persist this install's default provider/model choice.

One s-expression: a cons of (PROVIDER . MODEL) strings, or nil if no
default has ever been chosen."
  :group 'beemacs-pi-model
  :type 'file)

(defcustom beemacs-pi-model-list-timeout 5
  "Seconds to wait for a `model_list' reply to a `list_models' request."
  :group 'beemacs-pi-model
  :type 'number)

(define-error 'beemacs-pi-model-error "beemacs pi model operation failed")

(defvar-local beemacs-pi-model--session-override nil
  "This buffer's per-session (PROVIDER . MODEL) override, or nil.

Set by `beemacs-pi-model-select' when called with a non-nil SESSION-ONLY
argument, or automatically when called from a `beemacs-pi-chat-mode'
buffer. Takes priority over the persisted per-install default for any
lookup via `beemacs-pi-model-current' run with that buffer current.")

;;; Persistence (per-install default)

(defun beemacs-pi-model--load-default ()
  "Return the persisted default (PROVIDER . MODEL) cons, or nil if none/unreadable.

Reads via the shared `beemacs-persistence-read-file' helper rather than
duplicating the file-read logic."
  (beemacs-persistence-read-file beemacs-pi-model-persist-file #'consp))

(defun beemacs-pi-model--save-default (provider-model)
  "Persist PROVIDER-MODEL (a (PROVIDER . MODEL) cons) as this install's default.

Writes via the shared `beemacs-persistence-write-file' helper rather than
duplicating the file-write logic."
  (beemacs-persistence-write-file beemacs-pi-model-persist-file provider-model))

(defun beemacs-pi-model-default ()
  "Return the persisted per-install default (PROVIDER . MODEL) cons, or nil."
  (beemacs-pi-model--load-default))

(defun beemacs-pi-model-set-default (provider model)
  "Persist PROVIDER/MODEL as this install's default provider/model."
  (beemacs-pi-model--save-default (cons provider model)))

(defun beemacs-pi-model-current ()
  "Return the effective (PROVIDER . MODEL) cons for the current buffer.

Prefers `beemacs-pi-model--session-override' (this buffer's own override,
if any) over the persisted per-install default. Returns nil when neither
is set -- meaning the caller should let `pi' fall back to its own default
rather than force a choice."
  (or beemacs-pi-model--session-override
      (beemacs-pi-model--load-default)))

;;; Listing

(defun beemacs-pi-model--provider-name (record)
  "Return the `provider' field of provider RECORD (an alist)."
  (alist-get 'provider record))

(defun beemacs-pi-model--provider-models (record)
  "Return the `models' field of provider RECORD as a list of strings."
  (append (alist-get 'models record) nil))

(defun beemacs-pi-model-list (&optional executable)
  "Fetch the list of available provider/model records via a one-shot RPC
round trip.

Spawns a `pi' RPC process (EXECUTABLE, or `beemacs-pi-executable'), sends
`{\"type\":\"list_models\"}', waits up to `beemacs-pi-model-list-timeout'
seconds for a `{\"type\":\"model_list\",\"providers\":[...]}' reply, tears
the process down, and returns the `providers' vector converted to a list
of alists, each with `provider' and `models' keys.

Signals `beemacs-pi-model-error' if the process never replies within the
timeout, or if it exits/errors instead."
  (let* ((beemacs-pi-executable (or executable beemacs-pi-executable))
         (reply nil)
         (handle (beemacs-pi-start
                  (lambda (event)
                    (when (equal (alist-get 'type event) "model_list")
                      (setq reply event))))))
    (unwind-protect
        (progn
          (beemacs-pi-send handle '((type . "list_models")))
          (with-timeout (beemacs-pi-model-list-timeout nil)
            (while (not reply)
              (accept-process-output nil 0.05)))
          (unless reply
            (signal 'beemacs-pi-model-error
                    (list "timed out waiting for pi model_list reply")))
          (append (alist-get 'providers reply) nil))
      (beemacs-pi-stop handle))))

(defun beemacs-pi-model--candidates (providers)
  "Flatten PROVIDERS (as returned by `beemacs-pi-model-list') to an alist.

Each element of the returned alist is (\"PROVIDER/MODEL\" . (PROVIDER
. MODEL)), suitable as `completing-read' COLLECTION with the cons
recoverable afterwards via `assoc'."
  (cl-loop for record in providers
           for provider = (beemacs-pi-model--provider-name record)
           append (cl-loop for model in (beemacs-pi-model--provider-models record)
                            collect (cons (format "%s/%s" provider model)
                                          (cons provider model)))))

;;; Selection

;;;###autoload
(defun beemacs-pi-model-select (&optional session-only)
  "Query pi for its available providers/models and select one.

Fetches the current provider/model list via `beemacs-pi-model-list',
prompts with `completing-read' over the flattened \"PROVIDER/MODEL\"
candidates, and records the selection.

When SESSION-ONLY is non-nil, or this command is invoked from a
`beemacs-pi-chat-mode' buffer, the selection overrides the default for
THIS buffer only (`beemacs-pi-model--session-override'), leaving the
persisted per-install default untouched. Otherwise the selection is
persisted as the new per-install default via
`beemacs-pi-model-set-default', used by every session that has no
per-session override of its own.

Returns the selected (PROVIDER . MODEL) cons."
  (interactive "P")
  (let* ((providers (beemacs-pi-model-list))
         (candidates (beemacs-pi-model--candidates providers)))
    (unless candidates
      (user-error "pi reported no available providers/models"))
    (let* ((choice (completing-read "Select pi provider/model: "
                                     (mapcar #'car candidates) nil t))
           (provider-model (cdr (assoc choice candidates))))
      (if (or session-only (derived-mode-p 'beemacs-pi-chat-mode))
          (setq beemacs-pi-model--session-override provider-model)
        (beemacs-pi-model-set-default (car provider-model) (cdr provider-model)))
      (message "beemacs-pi-model: selected %s/%s%s"
               (car provider-model) (cdr provider-model)
               (if (or session-only (derived-mode-p 'beemacs-pi-chat-mode))
                   " (this session only)"
                 " (new install default)"))
      provider-model)))

(provide 'beemacs-pi-model)

;;; beemacs-pi-model.el ends here
