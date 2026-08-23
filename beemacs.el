;;; beemacs.el --- Emacs front-end for beehive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;; Author: beehive swarm
;; Maintainer: beehive swarm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/spencerharmon/beemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; beemacs is an Emacs interface to a running `beehived' HTTP server — the
;; coordination layer for an autonomous self-improvement swarm (see the
;; `beehive' project).  It aims to be feature-complete with the beehive web
;; UI: browsing submodules, plans/tasks, ROIs, docs, live session
;; transcripts, human (NEEDS-HUMAN) escalation resolution, the agentic
;; editor, dances, hygiene, secrets, and swarm stats — all from Emacs.
;;
;; This file is the package entry point.  It requires the per-concern
;; modules and defines package-wide constants used across them.  Concrete
;; functionality lives in:
;;   - beemacs-transport.el — HTTP transport to a beehived instance
;;   - beemacs-api.el       — typed request/response wrappers over transport
;;   - beemacs-render.el    — buffer rendering / display logic
;;
;; Load the package with `use-package' + `straight.el':
;;
;;   (use-package beemacs
;;     :straight (beemacs :type git :host github
;;                         :repo "spencerharmon/beemacs")
;;     :custom (beemacs-endpoint "http://127.0.0.1:8080"))

;;; Code:

(require 'tabulated-list)
(require 'beemacs-transport)
(require 'beemacs-api)
(require 'beemacs-render)

(defconst beemacs-version "0.1.0"
  "Current version of beemacs.")

(defgroup beemacs nil
  "Emacs front-end for beehive."
  :group 'tools
  :prefix "beemacs-")

;; --- Plan view (beemacs-plan-view) -----------------------------------------
;;
;; A navigable, read-only buffer of a submodule's PLAN.md tasks (id/status/
;; weight/deps/claim state), mirroring GET /submodule/{name}/plan.json.  It
;; never writes PLAN.md directly -- purely a viewer over `beemacs-api-plan'/
;; `beemacs-render-plan-rows'.  RET on a row opens the task's linked change
;; doc (falling back to its live session when no doc is linked yet) in a
;; plain read-only buffer.

(defvar-local beemacs--plan-submodule nil
  "Submodule name backing the current `beemacs-plan-mode' buffer.")

(defvar-local beemacs--plan-data nil
  "Last-fetched `beemacs-api-plan' payload backing the current buffer.

Kept alongside the rendered `tabulated-list-entries' so RET can resolve a
row back to its full task alist (DocHref/SessionHref) via
`beemacs-render-plan-find-item' without re-fetching from beehived.")

(defvar beemacs-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'beemacs-plan-open-at-point)
    (define-key map (kbd "g") #'beemacs-plan-refresh)
    map)
  "Keymap for `beemacs-plan-mode'.")

(define-derived-mode beemacs-plan-mode tabulated-list-mode "Beemacs-Plan"
  "Major mode for a read-only, navigable view of a submodule's PLAN.md tasks.

Mirrors GET /submodule/{name}/plan.json -- task id/status/weight/deps/
claim state.  Never writes PLAN.md directly: `\\[beemacs-plan-refresh]'
re-fetches and re-renders, and `\\[beemacs-plan-open-at-point]' (RET)
opens the task at point's linked change doc or session in a separate
buffer, but no command in this mode ever mutates the submodule's plan."
  (setq tabulated-list-format
        [("ID" 28 t) ("Status" 16 t) ("Wt" 4 t) ("Deps" 24 t) ("Claim" 20 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun beemacs-plan-refresh ()
  "Refetch and re-render the current `beemacs-plan-mode' buffer.

Signals an error when called outside a `beemacs-plan-mode' buffer."
  (interactive)
  (unless beemacs--plan-submodule
    (error "Not a beemacs-plan-mode buffer"))
  (setq beemacs--plan-data (beemacs-api-plan beemacs--plan-submodule))
  (setq tabulated-list-entries (beemacs-render-plan-rows beemacs--plan-data))
  (tabulated-list-print t))

(defun beemacs-plan--visit-href (href buffer-name)
  "Fetch HREF from the connected beehived instance and display it read-only.

Renders the raw response body into a freshly emptied buffer named
BUFFER-NAME in `view-mode', then displays it.  Used by
`beemacs-plan-open-at-point' to open a task's linked change doc or
session page -- a plain read-only viewer, never an editing surface."
  (let ((body (beemacs-transport-get href)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert body)
        (goto-char (point-min)))
      (view-mode 1)
      (pop-to-buffer (current-buffer)))))

(defun beemacs-plan-open-at-point ()
  "Open the change doc, or else the session, for the task at point.

Looks the row up via `beemacs-render-plan-find-item' against the
buffer's last-fetched `beemacs--plan-data', then visits its `DocHref'
when linked, falling back to its `SessionHref' when unlinked. Signals a
`user-error' when the task at point has neither. Purely a read-only
navigation affordance -- never writes PLAN.md."
  (interactive)
  (let* ((id (tabulated-list-get-id))
         (item (and id (beemacs-render-plan-find-item beemacs--plan-data id))))
    (unless item
      (error "No task at point"))
    (let ((doc-href (alist-get 'DocHref item))
          (session-href (alist-get 'SessionHref item)))
      (cond
       ((and (stringp doc-href) (not (string-empty-p doc-href)))
        (beemacs-plan--visit-href doc-href (format "*beemacs-doc: %s*" id)))
       ((and (stringp session-href) (not (string-empty-p session-href)))
        (beemacs-plan--visit-href session-href (format "*beemacs-session: %s*" id)))
       (t (user-error "No doc or session linked for task %s" id))))))

;;;###autoload
(defun beemacs-plan-view (name)
  "Open a read-only, navigable buffer of submodule NAME's PLAN.md tasks.

Mirrors GET /submodule/{name}/plan.json: each row shows a task's id,
status, weight, deps, and unified claim state. `\\[beemacs-plan-refresh]'
(g) re-fetches; RET on a row opens that task's linked change doc (or
live session, absent a doc) in a plain read-only buffer. Never writes
PLAN.md directly."
  (interactive "sSubmodule name: ")
  (let ((data (beemacs-api-plan name)))
    (with-current-buffer (get-buffer-create (format "*beemacs-plan: %s*" name))
      (beemacs-plan-mode)
      (setq beemacs--plan-submodule name)
      (setq beemacs--plan-data data)
      (setq tabulated-list-entries (beemacs-render-plan-rows data))
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(provide 'beemacs)

;;; beemacs.el ends here
