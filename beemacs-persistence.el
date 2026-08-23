;;; beemacs-persistence.el --- Shared per-install persisted state for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; A single per-install persistence store for beemacs, covering:
;;
;;   - the configured beehived endpoint override (`beemacs-persistence-endpoint');
;;   - the pi harness configuration -- executable path and default
;;     provider/model (`beemacs-persistence-pi-executable',
;;     `beemacs-persistence-pi-default-model'); and
;;   - a bounded MRU (most-recently-used) list per resource KIND (a symbol
;;     such as `submodule', `session', or `pi-session'), via
;;     `beemacs-persistence-mru' / `beemacs-persistence-record-mru'.
;;
;; All of it is loaded/saved as one plain s-expression alist in
;; `beemacs-persistence-file', read/written via `beemacs-persistence--load'
;; and `beemacs-persistence--save'.
;;
;; `beemacs-pi-sessions.el' (per-install pi-session MRU) and
;; `beemacs-pi-model.el' (per-install default provider/model) each keep
;; their own historical persist-file customization variable for backward
;; compatibility and independent test isolation, but their read/write
;; bodies are now thin wrappers over the generic
;; `beemacs-persistence-read-file' / `beemacs-persistence-write-file'
;; helpers below rather than duplicating the `with-temp-buffer' /
;; `with-temp-file' logic -- this module owns that logic exactly once.
;;
;; This module also installs a header-line version indicator
;; (`beemacs-version', from `beemacs.el') in every beemacs buffer, so an
;; operator can glance at any beemacs window and confirm they are running
;; fresh code after an upgrade. It is installed generically (matched by
;; major-mode name prefix "beemacs-") so it applies uniformly without
;; every mode definition having to opt in individually.

;;; Code:

(require 'cl-lib)
(require 'beemacs-transport)

(defgroup beemacs-persistence nil
  "Shared per-install persisted state for beemacs."
  :group 'beemacs
  :prefix "beemacs-persistence-")

(defcustom beemacs-persistence-file
  (expand-file-name "beemacs-persistence.el" user-emacs-directory)
  "File used to persist this install's shared beemacs state.

One s-expression: an alist with keys `endpoint', `pi-executable',
`pi-default-model', and `mru-KIND' (one entry per MRU resource kind, e.g.
`mru-submodule', `mru-session', `mru-pi-session')."
  :group 'beemacs-persistence
  :type 'file)

(defcustom beemacs-persistence-mru-limit 20
  "Default maximum number of entries kept in a persisted MRU list."
  :group 'beemacs-persistence
  :type 'integer)

;;; Generic s-expression file helpers -- the one place beemacs reads/writes
;;; a persisted Lisp value to disk. Other modules (`beemacs-pi-sessions',
;;; `beemacs-pi-model') build their own persist files on top of these
;;; rather than duplicating the `with-temp-buffer'/`with-temp-file' logic.

(defun beemacs-persistence-read-file (file &optional predicate)
  "Return the s-expression persisted in FILE, or nil if none/unreadable.

If PREDICATE is non-nil, the parsed value is returned only when
`(funcall PREDICATE value)' is non-nil; otherwise nil is returned, same as
an unreadable/absent file."
  (when (file-readable-p file)
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents file)
          (let ((data (read (current-buffer))))
            (if predicate
                (when (funcall predicate data) data)
              data)))
      (error nil))))

(defun beemacs-persistence-write-file (file value)
  "Persist VALUE (any `prin1'-readable s-expression) to FILE."
  (with-temp-file file
    (prin1 value (current-buffer))))

;;; Shared store (endpoint, pi config, per-kind MRU)

(defun beemacs-persistence--load ()
  "Return the persisted shared-store alist, or nil if none/unreadable."
  (beemacs-persistence-read-file beemacs-persistence-file #'listp))

(defun beemacs-persistence--save (state)
  "Persist STATE (an alist) as the shared store."
  (beemacs-persistence-write-file beemacs-persistence-file state))

(defun beemacs-persistence-get (key &optional default)
  "Return the persisted value for KEY in the shared store, or DEFAULT."
  (let* ((state (beemacs-persistence--load))
         (cell (assq key state)))
    (if cell (cdr cell) default)))

(defun beemacs-persistence-set (key value)
  "Persist VALUE for KEY in the shared store, and return VALUE."
  (let* ((state (beemacs-persistence--load))
         (state (cons (cons key value) (assq-delete-all key state))))
    (beemacs-persistence--save state))
  value)

;;; Endpoint

(defun beemacs-persistence-endpoint ()
  "Return this install's persisted beehived endpoint override, or nil."
  (beemacs-persistence-get 'endpoint))

(defun beemacs-persistence-set-endpoint (endpoint)
  "Persist ENDPOINT as this install's beehived endpoint override.

Also applies it immediately by setting `beemacs-endpoint', so the change
takes effect for the rest of this session without a restart."
  (beemacs-persistence-set 'endpoint endpoint)
  (setq beemacs-endpoint endpoint))

(defun beemacs-persistence-apply-endpoint ()
  "Apply this install's persisted endpoint override (if any) to `beemacs-endpoint'.

Called once when `beemacs-persistence' loads, so a persisted override
from a previous session takes effect without any further action."
  (when-let* ((endpoint (beemacs-persistence-endpoint)))
    (setq beemacs-endpoint endpoint)))

;;; Pi configuration

(defun beemacs-persistence-pi-executable ()
  "Return this install's persisted `pi' executable path, or nil."
  (beemacs-persistence-get 'pi-executable))

(defun beemacs-persistence-set-pi-executable (path)
  "Persist PATH as this install's `pi' executable path."
  (beemacs-persistence-set 'pi-executable path))

(defun beemacs-persistence-pi-default-model ()
  "Return this install's persisted default (PROVIDER . MODEL) cons, or nil."
  (beemacs-persistence-get 'pi-default-model))

(defun beemacs-persistence-set-pi-default-model (provider-model)
  "Persist PROVIDER-MODEL (a (PROVIDER . MODEL) cons) as the install default."
  (beemacs-persistence-set 'pi-default-model provider-model))

;;; Bounded MRU, generic over a resource KIND

(defun beemacs-persistence--mru-key (kind)
  "Return the shared-store alist key used for KIND's persisted MRU list."
  (intern (format "mru-%s" kind)))

(defun beemacs-persistence-mru (kind)
  "Return the persisted MRU list for KIND, most-recent first, or nil."
  (beemacs-persistence-get (beemacs-persistence--mru-key kind)))

(defun beemacs-persistence-record-mru (kind id &optional limit)
  "Record ID as the most-recently-used KIND entry in the persisted MRU.

Moves ID to the front, dedupes, and truncates to LIMIT (or
`beemacs-persistence-mru-limit'). Returns the updated list."
  (let* ((current (beemacs-persistence-mru kind))
         (updated (cons id (delete id (copy-sequence current))))
         (limit (or limit beemacs-persistence-mru-limit)))
    (when (> (length updated) limit)
      (setq updated (cl-subseq updated 0 limit)))
    (beemacs-persistence-set (beemacs-persistence--mru-key kind) updated)
    updated))

;;; Version header-line indicator

(defun beemacs-persistence--header-line-string ()
  "Return the header-line string identifying the running `beemacs-version'."
  (format " beemacs %s" (if (boundp 'beemacs-version)
                            (symbol-value 'beemacs-version)
                          "?")))

(defun beemacs-persistence--maybe-install-header-line ()
  "Install the beemacs version header line in the current beemacs buffer.

Applies to any buffer whose `major-mode' name starts with \"beemacs-\",
so every beemacs major mode picks this up automatically without having
to opt in individually. Added to `after-change-major-mode-hook'."
  (when (and (symbolp major-mode)
             (string-prefix-p "beemacs-" (symbol-name major-mode)))
    (setq header-line-format (beemacs-persistence--header-line-string))))

;;;###autoload
(define-minor-mode beemacs-persistence-header-line-mode
  "Global minor mode installing a `beemacs-version' header line.

Every buffer whose major mode name starts with \"beemacs-\" gets a
one-line header showing the running beemacs version, so operators can
confirm they are on fresh code after an upgrade."
  :global t
  :group 'beemacs-persistence
  (if beemacs-persistence-header-line-mode
      (add-hook 'after-change-major-mode-hook
                #'beemacs-persistence--maybe-install-header-line)
    (remove-hook 'after-change-major-mode-hook
                 #'beemacs-persistence--maybe-install-header-line)))

(beemacs-persistence-header-line-mode 1)
(beemacs-persistence-apply-endpoint)

(provide 'beemacs-persistence)

;;; beemacs-persistence.el ends here
