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
(require 'beemacs-streaming)
(require 'beemacs-api)
(require 'beemacs-render)
(require 'beemacs-editor)
(require 'beemacs-env)
(require 'beemacs-pi)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'beemacs-pi-model)
(require 'beemacs-session)
(require 'beemacs-stats)
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

;;; Plan browser

(defvar-local beemacs-plan--submodule nil
  "Submodule name the current `beemacs-plan-mode' buffer is browsing.")

(defvar-local beemacs-plan--items nil
  "Alist of task ID -> its decoded `PlanItem' alist for the current buffer.

Populated by `beemacs-plan-refresh'/`beemacs-plan-view' from the latest
`beemacs-api-plan' fetch, so `beemacs-plan-open-at-point' can resolve a
row back to its `DocHref'/`Running'/`SessionHref' fields without a
second network round-trip -- `tabulated-list-entries' only carries the
display columns.")

(define-derived-mode beemacs-plan-mode tabulated-list-mode "Beemacs-Plan"
  "Major mode listing a submodule's live PLAN.md tasks, read-only.

Mirrors the beehived web UI's plan view (`GET /submodule/{name}/plan'):
one row per task with its id/status/weight/deps/unified claim state.
Never writes `PLAN.md' -- this is a pure read projection, always fetched
fresh from `beemacs-api-plan'. `RET' opens the task's linked change doc
(when resolved) and, when a honeybee is currently working the task,
reports its live session.
\\{beemacs-plan-mode-map}"
  (setq tabulated-list-format [("ID" 30 t) ("Status" 16 t)
                                ("Weight" 8 t) ("Deps" 30 t)
                                ("Claim" 20 t)])
  (setq tabulated-list-sort-key (cons "ID" nil))
  (tabulated-list-init-header))

(defun beemacs-plan-refresh ()
  "Refetch and redisplay the current `beemacs-plan-mode' buffer's tasks."
  (interactive)
  (unless (derived-mode-p 'beemacs-plan-mode)
    (user-error "Not in a beemacs-plan-mode buffer"))
  (let* ((name beemacs-plan--submodule)
         (data (beemacs-api-plan name))
         (items (append (alist-get 'Items (alist-get 'plan data)) nil)))
    (setq beemacs-plan--items
          (mapcar (lambda (it) (cons (alist-get 'ID it) it)) items))
    (setq tabulated-list-entries (beemacs-render-plan-rows items))
    (tabulated-list-print t)))

(defun beemacs-plan--doc-file (item)
  "Return ITEM's docs/-relative doc file path, or nil when unresolved.

Prefers `DocHref' (`/submodule/<name>/doc/<rel>', only ever set when the
doc actually resolves to a real file -- see `resolveDocHref'), stripping
everything up to and including the final \"/doc/\" segment to recover
REL; falls back to the raw `Doc' design-doc-convention field (which may
name a doc that does not yet exist) only when no `DocHref' was resolved."
  (let ((href (alist-get 'DocHref item))
        (doc (alist-get 'Doc item)))
    (cond
     ((and (stringp href) (string-match "/doc/\\(.+\\)\\'" href))
      (match-string 1 href))
     ((and (stringp doc) (not (string-empty-p doc))) doc)
     (t nil))))

(defun beemacs-plan-open-at-point ()
  "Open the task at point's change doc, and report its live session.

Fetches and shows the doc via `beemacs-api-doc' (mirroring
`beemacs-docs-open-at-point') when the row's `DocHref'/`Doc' resolves to
one; when the task is currently `Running' (a honeybee session actively
holds it -- the same claim-freshness union the dashboard/sessions views
use), also reports the live `SessionHref' so the session can be found
even though a dedicated session-transcript buffer is not yet wired
in (see `beemacs-session-stream'). Never touches `PLAN.md'."
  (interactive)
  (unless (derived-mode-p 'beemacs-plan-mode)
    (user-error "Not in a beemacs-plan-mode buffer"))
  (let* ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No task at point"))
    (let* ((item (alist-get id beemacs-plan--items nil nil #'equal))
           (name beemacs-plan--submodule)
           (doc-file (and item (beemacs-plan--doc-file item)))
           (running (and item (beemacs-render--json-true-p (alist-get 'Running item))))
           (session-href (and item (alist-get 'SessionHref item))))
      (when doc-file
        (let* ((data (beemacs-api-doc name doc-file))
               (buf (get-buffer-create (format "*beemacs-doc: %s/%s*" name doc-file))))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (or (alist-get 'body data) ""))
              (goto-char (point-min)))
            (view-mode 1)
            (setq buffer-read-only t))
          (pop-to-buffer buf)))
      (when (and running (stringp session-href) (not (string-empty-p session-href)))
        (message "Task %s is live -- session: %s" id session-href))
      (unless (or doc-file running)
        (message "Task %s has no linked doc and no live session" id)))))

(defvar beemacs-plan-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-plan-refresh)
    (define-key map (kbd "RET") #'beemacs-plan-open-at-point)
    map)
  "Keymap for `beemacs-plan-mode'.")

;;;###autoload
(defun beemacs-plan-view (name)
  "Browse submodule NAME's live PLAN.md tasks: id/status/weight/deps/claim.

Mirrors the beehived web UI's plan view (`GET /submodule/{name}/plan'),
sourced from `beemacs-api-plan' (the `beehive:beemacs-json-api' plan.json
endpoint) -- the exact same claim/running state and doc links the plan
page and the runner's own task selection use. `g' re-fetches live; `RET'
on a row opens that task's linked change doc and, when a honeybee is
actively working it, reports its live session. Never writes `PLAN.md'."
  (interactive "sSubmodule name: ")
  (let* ((data (beemacs-api-plan name))
         (items (append (alist-get 'Items (alist-get 'plan data)) nil))
         (buf (get-buffer-create (format "*beemacs-plan: %s*" name))))
    (with-current-buffer buf
      (beemacs-plan-mode)
      (setq beemacs-plan--submodule name)
      (setq beemacs-plan--items
            (mapcar (lambda (it) (cons (alist-get 'ID it) it)) items))
      (setq tabulated-list-entries (beemacs-render-plan-rows items))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; Swarm dashboard

(define-derived-mode beemacs-dashboard-mode tabulated-list-mode "Beemacs-Dashboard"
  "Major mode for the hive-wide swarm overview buffer.

Mirrors the beehived web UI's dashboard (`GET /dashboard.json'): one row
per tracked submodule, its state/ROI stamp, pending/human task counts,
active deploy env, whether a pass is currently working it, and its live
honeybee count. `RET' drills into `beemacs-submodule-view'.
\\{beemacs-dashboard-mode-map}"
  (setq tabulated-list-format [("Name" 20 t) ("State" 12 t)
                                ("Pending" 8 t) ("Human" 6 t)
                                ("Env" 12 t) ("Working" 8 t)
                                ("Bees" 5 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defun beemacs-dashboard-refresh ()
  "Refetch and redisplay the current `beemacs-dashboard-mode' buffer.

Re-fetches `beemacs-api-dashboard' live -- the dashboard is never a
static snapshot, since a swarm's per-submodule state (pending/human
counts, active passes, bee count) changes continuously as honeybees
work."
  (interactive)
  (unless (derived-mode-p 'beemacs-dashboard-mode)
    (user-error "Not in a beemacs-dashboard-mode buffer"))
  (let ((data (beemacs-api-dashboard)))
    (setq tabulated-list-entries
          (beemacs-render-dashboard-rows (alist-get 'subs data)))
    (tabulated-list-print t)))

(defun beemacs-dashboard-open-at-point ()
  "Drill into the submodule at point via `beemacs-submodule-view'."
  (interactive)
  (unless (derived-mode-p 'beemacs-dashboard-mode)
    (user-error "Not in a beemacs-dashboard-mode buffer"))
  (let ((name (tabulated-list-get-id)))
    (unless name
      (user-error "No submodule at point"))
    (beemacs-submodule-view name)))

(defvar beemacs-dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-dashboard-refresh)
    (define-key map (kbd "RET") #'beemacs-dashboard-open-at-point)
    map)
  "Keymap for `beemacs-dashboard-mode'.")

;;;###autoload
(defun beemacs-dashboard ()
  "Open the hive-wide swarm overview buffer.

Lists every tracked submodule with its state, ROI stamp status, pending/
human task counts, active deploy env, whether a pass is currently
working it, and its live honeybee count -- sourced from the
`beehive:beemacs-json-api' dashboard JSON endpoint (`beemacs-api-
dashboard'), the same data the HTML dashboard renders. `g' re-fetches
live; `RET' on a row drills into `beemacs-submodule-view' for that
submodule."
  (interactive)
  (let* ((data (beemacs-api-dashboard))
         (buf (get-buffer-create "*beemacs-dashboard*")))
    (with-current-buffer buf
      (beemacs-dashboard-mode)
      (setq tabulated-list-entries
            (beemacs-render-dashboard-rows (alist-get 'subs data)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; Submodule drill-in hub

(defvar-local beemacs-submodule-view--name nil
  "Submodule name the current `beemacs-submodule-view-mode' buffer is showing.")

(define-derived-mode beemacs-submodule-view-mode special-mode "Beemacs-Submodule"
  "Major mode for a per-submodule drill-in hub buffer.

Mirrors the beehived web UI's submodule explorer (`GET /submodule/{name}'):
a summary card (state, ROI stamp, pending/human counts, active env, live
honeybee count) plus navigation into the plan/docs/sessions/branches/env/
secrets/roi sub-views. Wired as `beemacs-dashboard''s RET target.
\\{beemacs-submodule-view-mode-map}")

(defun beemacs-submodule-view--insert-summary (name)
  "Insert NAME's summary card into the current buffer at point.

Sourced from `beemacs-api-dashboard-submodule' -- the same per-submodule
card (state/stamp/pending/human/env/working/bees) the HTML dashboard
renders. A submodule absent from the dashboard payload (e.g. one not yet
tracked, or a transient fetch gap) renders a placeholder line rather than
erroring, so the hub's navigation section below is always reachable."
  (let ((summary (beemacs-api-dashboard-submodule name)))
    (if summary
        (insert
         (format "State: %s\n" (or (alist-get 'State summary) ""))
         (format "ROI stamp: %s\n" (or (alist-get 'Stamp summary) ""))
         (format "Pending: %s   Human: %s\n"
                 (or (alist-get 'Pending summary) 0)
                 (or (alist-get 'Human summary) 0))
         (format "Env: %s   Active bees: %s   Working: %s\n"
                 (or (alist-get 'Env summary) "")
                 (or (alist-get 'Bees summary) 0)
                 (if (alist-get 'Working summary) "yes" "no")))
      (insert "(no dashboard summary available for this submodule)\n"))))

(defun beemacs-submodule-view-refresh ()
  "Refetch and redisplay the current `beemacs-submodule-view-mode' buffer."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (let ((name beemacs-submodule-view--name)
        (inhibit-read-only t))
    (erase-buffer)
    (insert (format "Submodule: %s\n\n" name))
    (beemacs-submodule-view--insert-summary name)
    (insert "\nNavigation:\n")
    (insert "  [p] Plan       [o] ROI        [d] Docs\n")
    (insert "  [b] Branches   [s] Sessions   [e] Env\n")
    (insert "  [S] Secrets    [g] Refresh\n")
    (goto-char (point-min))))

(defun beemacs-submodule-view-plan ()
  "Show the submodule's PLAN.md tasks (mirrors `GET /submodule/{name}/plan').

Drills into the navigable `beemacs-plan-mode' buffer (`beemacs-plan-
view'): one row per task with its id/status/weight/deps/unified claim
state, `RET' opening the task's linked change doc + live session."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (beemacs-plan-view beemacs-submodule-view--name))

(defun beemacs-submodule-view-roi ()
  "Show the submodule's raw ROI.md content (mirrors `GET /roi/{name}')."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (let* ((name beemacs-submodule-view--name)
         (data (beemacs-api-roi name))
         (buf (get-buffer-create (format "*beemacs-roi: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (or (alist-get 'body data) ""))
        (goto-char (point-min)))
      (view-mode 1)
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defun beemacs-submodule-view-docs ()
  "Drill into the submodule's docs/ change-record explorer."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (beemacs-docs-view beemacs-submodule-view--name))

(defun beemacs-submodule-view-branches ()
  "Drill into the submodule's commit/branches explorer."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (beemacs-branches-view beemacs-submodule-view--name))

(defun beemacs-submodule-view-secrets ()
  "Show the submodule's own secrets key-name listing (never values)."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (let* ((name beemacs-submodule-view--name)
         (keys (append (beemacs-api-secrets-for name) nil))
         (buf (get-buffer-create (format "*beemacs-secrets: %s*" name))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Secrets for %s:\n\n" name))
        (if keys
            (dolist (k keys) (insert (format "  %s\n" k)))
          (insert "  (none)\n"))
        (goto-char (point-min)))
      (view-mode 1)
      (setq buffer-read-only t))
    (pop-to-buffer buf)))

(defun beemacs-submodule-view-sessions ()
  "Drill into the submodule's session list.

The `beehive:beemacs-json-api' contract has not yet added a
`sessions.json' endpoint (only `GET /submodule/{name}/sessions' HTML) to
enumerate session branches, so this cannot yet list them structurally; it
prompts for a branch name directly and opens it with
`beemacs-session-view' (`beemacs-session.el'), which streams that session's
transcript live via the existing SSE endpoint (`GET
/submodule/{name}/session/{branch}/stream') and renders a recorded session's
full transcript the same way. Listing branches without a manual name still
awaits a `sessions.json' endpoint — visit `/submodule/NAME/sessions' in a
browser to find one meanwhile."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (let ((branch (read-string
                 (format "Session branch for %s: " beemacs-submodule-view--name))))
    (if (string-empty-p branch)
        (message "Sessions drill-in: enumerating branches still pending a sessions.json endpoint (see beemacs-session-stream); visit /submodule/%s/sessions in a browser meanwhile"
                 beemacs-submodule-view--name)
      (beemacs-session-view beemacs-submodule-view--name branch))))

(defun beemacs-submodule-view-env ()
  "Drill into the submodule's deploy-env view.

The `beehive:beemacs-json-api' contract has not yet added an `env.json'
endpoint (only `GET /submodule/{name}/env' HTML), so this drill-in cannot
render structured data from JSON today; it reports that gap rather than
scraping HTML or faking a result."
  (interactive)
  (unless (derived-mode-p 'beemacs-submodule-view-mode)
    (user-error "Not in a beemacs-submodule-view-mode buffer"))
  (message "Env drill-in pending an env.json endpoint (see beemacs-instruction-env); visit /submodule/%s/env in a browser meanwhile"
           beemacs-submodule-view--name))

(defvar beemacs-submodule-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "g" #'beemacs-submodule-view-refresh)
    (define-key map "p" #'beemacs-submodule-view-plan)
    (define-key map "o" #'beemacs-submodule-view-roi)
    (define-key map "d" #'beemacs-submodule-view-docs)
    (define-key map "b" #'beemacs-submodule-view-branches)
    (define-key map "s" #'beemacs-submodule-view-sessions)
    (define-key map "e" #'beemacs-submodule-view-env)
    (define-key map "S" #'beemacs-submodule-view-secrets)
    map)
  "Keymap for `beemacs-submodule-view-mode'.")

;;;###autoload
(defun beemacs-submodule-view (name)
  "Open NAME's drill-in hub buffer: summary + sub-view navigation.

Mirrors the beehived web UI's submodule explorer (`GET /submodule/{name}');
wired as `beemacs-dashboard''s RET target so a dashboard row drills
straight into this hub."
  (interactive "sSubmodule name: ")
  (let ((buf (get-buffer-create (format "*beemacs-submodule: %s*" name))))
    (with-current-buffer buf
      (beemacs-submodule-view-mode)
      (setq beemacs-submodule-view--name name)
      (beemacs-submodule-view-refresh))
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
