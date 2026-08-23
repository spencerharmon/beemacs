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

(require 'beemacs-transport)
(require 'beemacs-api)
(require 'beemacs-render)
(require 'beemacs-editor)
(require 'beemacs-pi)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'tabulated-list)
(require 'diff-mode)

(defconst beemacs-version "0.1.0"
  "Current version of beemacs.")

(defgroup beemacs nil
  "Emacs front-end for beehive."
  :group 'tools
  :prefix "beemacs-")

;;; Docs browser

(defvar-local beemacs-docs--submodule nil
  "Submodule name the current `beemacs-docs-mode' buffer is browsing.")

(define-derived-mode beemacs-docs-mode tabulated-list-mode "Beemacs-Docs"
  "Major mode listing a submodule's docs/ change-record files.

Mirrors the beehived web UI's doc explorer (`GET /submodule/{name}/docs').
\\{beemacs-docs-mode-map}"
  (setq tabulated-list-format [("Name" 40 t) ("Dir" 20 t) ("Path" 0 nil)])
  (setq tabulated-list-sort-key (cons "Path" nil))
  (tabulated-list-init-header))

(defun beemacs-docs-refresh ()
  "Refetch and redisplay the current `beemacs-docs-mode' buffer's docs list."
  (interactive)
  (unless (derived-mode-p 'beemacs-docs-mode)
    (user-error "Not in a beemacs-docs-mode buffer"))
  (let* ((name beemacs-docs--submodule)
         (data (beemacs-api-docs name)))
    (setq tabulated-list-entries
          (beemacs-render-doc-rows (alist-get 'docs data)))
    (tabulated-list-print t)))

(defun beemacs-docs-open-at-point ()
  "Open the change doc at point in a read-only buffer with its raw content."
  (interactive)
  (unless (derived-mode-p 'beemacs-docs-mode)
    (user-error "Not in a beemacs-docs-mode buffer"))
  (let ((path (tabulated-list-get-id)))
    (unless path
      (user-error "No doc at point"))
    (let* ((name beemacs-docs--submodule)
           (data (beemacs-api-doc name path))
           (buf (get-buffer-create (format "*beemacs-doc: %s/%s*" name path))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (or (alist-get 'body data) ""))
          (goto-char (point-min)))
        (view-mode 1)
        (setq buffer-read-only t))
      (pop-to-buffer buf))))

(defvar beemacs-docs-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-docs-refresh)
    (define-key map (kbd "RET") #'beemacs-docs-open-at-point)
    map)
  "Keymap for `beemacs-docs-mode'.")

;;;###autoload
(defun beemacs-docs-view (name)
  "Browse submodule NAME's docs/ change-record files."
  (interactive "sSubmodule name: ")
  (let* ((data (beemacs-api-docs name))
         (buf (get-buffer-create (format "*beemacs-docs: %s*" name))))
    (with-current-buffer buf
      (beemacs-docs-mode)
      (setq beemacs-docs--submodule name)
      (setq tabulated-list-entries
            (beemacs-render-doc-rows (alist-get 'docs data)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; Branches / commit browser

(defvar-local beemacs-branches--submodule nil
  "Submodule name the current `beemacs-branches-mode' buffer is browsing.")

(define-derived-mode beemacs-branches-mode tabulated-list-mode "Beemacs-Branches"
  "Major mode listing a submodule's commit history.

Mirrors the beehived web UI's branch view (`GET /submodule/{name}/branches').
\\{beemacs-branches-mode-map}"
  (setq tabulated-list-format [("SHA" 12 nil) ("Author" 16 t)
                                ("Date" 12 t) ("Subject" 50 nil)
                                ("Task" 20 t)])
  (setq tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(defun beemacs-branches-refresh ()
  "Refetch and redisplay the current `beemacs-branches-mode' buffer's commits."
  (interactive)
  (unless (derived-mode-p 'beemacs-branches-mode)
    (user-error "Not in a beemacs-branches-mode buffer"))
  (let* ((name beemacs-branches--submodule)
         (data (beemacs-api-branches name)))
    (setq tabulated-list-entries
          (beemacs-render-branch-rows (alist-get 'commits data)))
    (tabulated-list-print t)))

(defun beemacs-branches-open-at-point ()
  "Open the commit at point as a `diff-mode' PLAN.md diff buffer."
  (interactive)
  (unless (derived-mode-p 'beemacs-branches-mode)
    (user-error "Not in a beemacs-branches-mode buffer"))
  (let ((sha (tabulated-list-get-id)))
    (unless sha
      (user-error "No commit at point"))
    (beemacs-commit-view beemacs-branches--submodule sha)))

(defvar beemacs-branches-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-branches-refresh)
    (define-key map (kbd "RET") #'beemacs-branches-open-at-point)
    map)
  "Keymap for `beemacs-branches-mode'.")

;;;###autoload
(defun beemacs-branches-view (name)
  "Browse submodule NAME's commit history."
  (interactive "sSubmodule name: ")
  (let* ((data (beemacs-api-branches name))
         (buf (get-buffer-create (format "*beemacs-branches: %s*" name))))
    (with-current-buffer buf
      (beemacs-branches-mode)
      (setq beemacs-branches--submodule name)
      (setq tabulated-list-entries
            (beemacs-render-branch-rows (alist-get 'commits data)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;;###autoload
(defun beemacs-commit-view (name sha)
  "Show submodule NAME's PLAN.md diff at commit SHA in a `diff-mode' buffer.

Mirrors the beehived web UI's commit view (`GET
/submodule/{name}/commit/{sha}'), rendering the same before/after PLAN.md
content as a unified diff computed client-side (see
`beemacs-render-unified-diff') -- no PLAN.md is ever written."
  (interactive "sSubmodule name: \nsCommit sha: ")
  (let* ((data (beemacs-api-commit name sha))
         (before (or (alist-get 'plan_before data) ""))
         (after (or (alist-get 'plan_after data) ""))
         (buf (get-buffer-create (format "*beemacs-commit: %s/%s*" name sha))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (beemacs-render-unified-diff before after "PLAN.md"))
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t)
      (view-mode 1))
    (pop-to-buffer buf)))

;;; Skills registry browser

(define-derived-mode beemacs-skills-mode tabulated-list-mode "Beemacs-Skills"
  "Major mode listing the hive-wide skills/dances registry.

Mirrors the beehived web UI's combined hygiene+dances page (`GET
/skills.json', pre-rename URL redirecting to `/hygiene').
\\{beemacs-skills-mode-map}"
  (setq tabulated-list-format [("Name" 30 t) ("Title" 30 t) ("Summary" 0 nil)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defun beemacs-skills-refresh ()
  "Refetch and redisplay the current `beemacs-skills-mode' buffer's skills."
  (interactive)
  (unless (derived-mode-p 'beemacs-skills-mode)
    (user-error "Not in a beemacs-skills-mode buffer"))
  (let ((data (beemacs-api-skills)))
    (setq tabulated-list-entries
          (beemacs-render-skill-rows (alist-get 'dances data)))
    (tabulated-list-print t)))

(defun beemacs-skills-open-at-point ()
  "Open the skill at point's full record in a read-only buffer.

The `skills.json' registry carries no separate detail endpoint -- a
skill's Name/Title/Summary/Destructive/ReportOnly identity fields (the
same `dancePanel' fields shown by the list's row) ARE its full content,
so this renders all of them into one read-only buffer."
  (interactive)
  (unless (derived-mode-p 'beemacs-skills-mode)
    (user-error "Not in a beemacs-skills-mode buffer"))
  (let ((row (tabulated-list-get-entry)))
    (unless row
      (user-error "No skill at point"))
    (let* ((name (aref row 0))
           (title (aref row 1))
           (summary (aref row 2))
           (buf (get-buffer-create (format "*beemacs-skill: %s*" name))))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Name: %s\n" name))
          (insert (format "Title: %s\n\n" title))
          (insert summary)
          (insert "\n")
          (goto-char (point-min)))
        (view-mode 1)
        (setq buffer-read-only t))
      (pop-to-buffer buf))))

(defvar beemacs-skills-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-skills-refresh)
    (define-key map (kbd "RET") #'beemacs-skills-open-at-point)
    map)
  "Keymap for `beemacs-skills-mode'.")

;;;###autoload
(defun beemacs-skills-view ()
  "Browse the hive-wide skills/dances registry.

Unlike the submodule-scoped views above, this is a single hive-wide
listing -- `GET /skills.json' takes no submodule name."
  (interactive)
  (let* ((data (beemacs-api-skills))
         (buf (get-buffer-create "*beemacs-skills*")))
    (with-current-buffer buf
      (beemacs-skills-mode)
      (setq tabulated-list-entries
            (beemacs-render-skill-rows (alist-get 'dances data)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'beemacs)

;;; beemacs.el ends here
