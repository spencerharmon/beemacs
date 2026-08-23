;;; beemacs-human.el --- NEEDS-HUMAN escalation resolution for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Emacs client for beehived's NEEDS-HUMAN resolution workflow -- the JSON
;; `/human.json'/`/api/human/*' surface (`internal/web/jsonapi.go',
;; `internal/web/humanresolve.go') that mirrors the web UI's `/human' list
;; and `/human/{sub}/{id}' resolve page.  `beemacs-human-list' lists every
;; NEEDS-HUMAN task across every tracked submodule (`GET /human.json');
;; `RET' on a row opens `beemacs-human-resolve-view', a buffer combining the
;; task's blocker context with the AI resolution agent's live transcript
;; (mirroring `beemacs-editor.el''s chat/diff/merge pattern, applied to the
;; resolution-agent session rather than a file edit) -- message the
;; resolver, view its diff stat, and drive resolve/publish/discard through
;; the SAME sanctioned backend flow the web UI uses:
;;
;;   POST /api/human/{sub}/{id}/session          -- open/resume a session
;;   GET  /api/human/{sub}/{id}/panel/{sid}       -- poll the live panel
;;   POST /api/human/{sub}/{id}/message/{sid}     -- chat with the resolver
;;   POST /api/human/{sub}/{id}/publish/{sid}     -- land its changes on main
;;   POST /api/human/{sub}/{id}/discard/{sid}     -- drop its unpublished work
;;   POST /api/human/{sub}/{id}/resolve           -- flip NEEDS-HUMAN -> TODO
;;
;; The final "resolve" flip is the deterministic `plan.Task.Resolve' +
;; `publishMain' write path (`humanResolveApply'/`apiHumanResolve') --
;; never a direct `PLAN.md'/`ROI.md' edit from this client, exactly as the
;; web UI's own "Mark resolved" button works.  This module never reaches
;; for a beehive-layer file directly; every mutation goes through the
;; typed `beemacs-api-human-*' wrappers.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'beemacs-api)
(require 'beemacs-render)

(defgroup beemacs-human nil
  "NEEDS-HUMAN escalation resolution for beemacs."
  :group 'beemacs
  :prefix "beemacs-human-")

;;; Hive-wide NEEDS-HUMAN task listing

(define-derived-mode beemacs-human-list-mode tabulated-list-mode "Beemacs-Human"
  "Major mode listing every NEEDS-HUMAN task across tracked submodules.

Mirrors the beehived web UI's `/human' list page (`GET /human.json').
`RET' opens the task's resolution workspace (`beemacs-human-resolve-view').
\\{beemacs-human-list-mode-map}"
  (setq tabulated-list-format
        [("Submodule" 16 t) ("Task ID" 30 t) ("Description" 40 t)
         ("Category" 14 t) ("Reason" 0 nil)])
  (setq tabulated-list-sort-key (cons "Submodule" nil))
  (tabulated-list-init-header))

(defun beemacs-human-list-refresh ()
  "Refetch and redisplay the current `beemacs-human-list-mode' buffer's rows."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-list-mode)
    (user-error "Not in a beemacs-human-list-mode buffer"))
  (let* ((data (beemacs-api-human))
         (tasks (alist-get 'tasks data)))
    (setq tabulated-list-entries (beemacs-render-human-rows tasks))
    (tabulated-list-print t)))

(defun beemacs-human-list-open-at-point ()
  "Open the resolution workspace for the NEEDS-HUMAN task at point."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-list-mode)
    (user-error "Not in a beemacs-human-list-mode buffer"))
  (let ((key (tabulated-list-get-id)))
    (unless key
      (user-error "No task at point"))
    (beemacs-human-resolve-view (car key) (cdr key))))

(defvar beemacs-human-list-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-human-list-refresh)
    (define-key map (kbd "RET") #'beemacs-human-list-open-at-point)
    map)
  "Keymap for `beemacs-human-list-mode'.")

;;;###autoload
(defun beemacs-human-list ()
  "Browse every NEEDS-HUMAN task across every tracked submodule.

Mirrors the beehived web UI's `/human' list page, sourced from
`beemacs-api-human' (`GET /human.json'). `g' re-fetches live; `RET' on a
row opens that task's resolution workspace
(`beemacs-human-resolve-view')."
  (interactive)
  (let* ((data (beemacs-api-human))
         (tasks (alist-get 'tasks data))
         (buf (get-buffer-create "*beemacs-human*")))
    (with-current-buffer buf
      (beemacs-human-list-mode)
      (setq tabulated-list-entries (beemacs-render-human-rows tasks))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

;;; Per-task resolution workspace

(defvar-local beemacs-human-resolve--sub nil
  "The submodule name this `beemacs-human-resolve-mode' buffer resolves.")

(defvar-local beemacs-human-resolve--id nil
  "The NEEDS-HUMAN task id this `beemacs-human-resolve-mode' buffer resolves.")

(defvar-local beemacs-human-resolve--sid nil
  "The resolution-agent session id backing this buffer, or nil once discarded.")

(defvar-local beemacs-human-resolve--task nil
  "The task-context alist (`desc'/`body'/`deps'/`reason'/`category') this
buffer was opened for, from `beemacs-api-human-task'.")

(defun beemacs-human-resolve--buffer-name (sub id)
  "Return the resolve-workspace buffer name for submodule SUB's task ID."
  (format "*beemacs-human: %s/%s*" sub id))

(defun beemacs-human-resolve--find-buffer (sub id)
  "Return the live `beemacs-human-resolve-mode' buffer for SUB/ID, or nil."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'beemacs-human-resolve-mode)
                       (equal beemacs-human-resolve--sub sub)
                       (equal beemacs-human-resolve--id id))))
              (buffer-list)))

(defun beemacs-human-resolve--insert-log (fmt &rest args)
  "Append a formatted line built from FMT and ARGS to the current buffer.

Assumes the caller has already made the buffer writable for the duration
of the insertion (see callers), matching `beemacs-editor--insert-log'."
  (goto-char (point-max))
  (insert (apply #'format fmt args) "\n"))

(defvar-local beemacs-human-resolve--transcript-start nil
  "Marker at the start of the buffer's live transcript/status section, set
once by `beemacs-human-resolve-view' right after the static task-context
header is inserted, so later panel refreshes replace only the transcript
without disturbing the header above it.")

(defun beemacs-human-resolve--render-panel (data)
  "Render panel DATA (as returned by the `beemacs-api-human-panel'/session/
message/publish/discard family) into the current buffer, replacing its
transcript-and-status section while leaving the task-context header
above it untouched.

The header/transcript boundary is the buffer-local marker left by
`beemacs-human-resolve-view' at the end of the context section; this
never re-renders the task's static desc/body/reason, only the live
session state, so repeated polling never duplicates the header."
  (let ((log (append (alist-get 'Log data) nil))
        (stat (alist-get 'Stat data))
        (has-change (alist-get 'HasChange data))
        (busy (alist-get 'Busy data))
        (published (alist-get 'Published data))
        (err (alist-get 'Error data)))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (delete-region (marker-position beemacs-human-resolve--transcript-start)
                      (point-max))
      (goto-char (point-max))
      (beemacs-human-resolve--insert-log "")
      (beemacs-human-resolve--insert-log "--- Resolution agent transcript ---")
      (if (null log)
          (beemacs-human-resolve--insert-log "(no messages yet)")
        (dolist (turn log)
          (beemacs-human-resolve--insert-log
           "[%s] %s" (or (alist-get 'role turn) (alist-get 'Role turn) "")
           (or (alist-get 'text turn) (alist-get 'Text turn) ""))))
      (beemacs-human-resolve--insert-log "")
      (beemacs-human-resolve--insert-log "Diffstat: %s"
                                          (if (and (stringp stat) (not (string-empty-p stat)))
                                              stat "(no changes)"))
      (beemacs-human-resolve--insert-log
       "[state: %s%s%s]"
       (if (eq busy t) "busy" "idle")
       (if (eq has-change t) ", has-change" "")
       (if (eq published t) ", published" ""))
      (when (and (stringp err) (not (string-empty-p err)))
        (beemacs-human-resolve--insert-log "[error] %s" err)))))

;;;###autoload
(defun beemacs-human-resolve-view (sub id)
  "Open SUB's NEEDS-HUMAN task ID's resolution workspace.

Fetches the task's blocker context via `beemacs-api-human-task'
(desc/body/deps/reason/category), opens (or resumes) its AI resolution
agent session via `beemacs-api-human-session' (mirroring the web UI's
`/human/{sub}/{id}' resolve page), and renders both the static context
and the session's live transcript/diffstat/status into one buffer.
Reopening an already-open workspace for the same SUB/ID reuses its
existing buffer/session rather than opening a duplicate.

`m' messages the resolver (`beemacs-human-resolve-message'), `g'
re-polls the live panel (`beemacs-human-resolve-refresh'), `p' publishes
its committed changes to main (`beemacs-human-resolve-publish'), `d'
discards its unpublished work (`beemacs-human-resolve-discard'), `r'
marks the task resolved (`beemacs-human-resolve-apply', the sanctioned
NEEDS-HUMAN -> TODO flip), and `q' quits the buffer."
  (interactive "sSubmodule name: \nsTask id: ")
  (let* ((existing (beemacs-human-resolve--find-buffer sub id))
         (task (beemacs-api-human-task sub id))
         (session-data (beemacs-api-human-session sub id))
         (sid (or (alist-get 'sid session-data) (alist-get 'SessID session-data)))
         (buf (or existing (get-buffer-create (beemacs-human-resolve--buffer-name sub id)))))
    (with-current-buffer buf
      (beemacs-human-resolve-mode)
      (setq beemacs-human-resolve--sub sub
            beemacs-human-resolve--id id
            beemacs-human-resolve--sid sid
            beemacs-human-resolve--task task)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beemacs-human-resolve--insert-log "NEEDS-HUMAN: %s/%s" sub id)
        (beemacs-human-resolve--insert-log "Category: %s" (or (alist-get 'category task) ""))
        (beemacs-human-resolve--insert-log "Reason:   %s" (or (alist-get 'reason task) ""))
        (beemacs-human-resolve--insert-log "")
        (beemacs-human-resolve--insert-log "Description: %s" (or (alist-get 'desc task) ""))
        (when (and (alist-get 'body task) (not (string-empty-p (alist-get 'body task))))
          (beemacs-human-resolve--insert-log "")
          (beemacs-human-resolve--insert-log "%s" (alist-get 'body task)))
        (setq beemacs-human-resolve--transcript-start (point-max-marker))
        (beemacs-human-resolve--render-panel session-data)))
    (pop-to-buffer buf)
    (message "beemacs-human: opened resolution workspace for %s/%s (session %s)" sub id sid)
    buf))

(defun beemacs-human-resolve-refresh ()
  "Re-poll and redisplay the current buffer's resolution-agent panel.

Calls `beemacs-api-human-panel' with the buffer's tracked session id --
the same live poll the web UI's resolve page performs while a turn is in
flight."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-resolve-mode)
    (user-error "Not in a beemacs-human-resolve-mode buffer"))
  (unless beemacs-human-resolve--sid
    (user-error "No active resolution session for this buffer"))
  (let* ((data (beemacs-api-human-panel
                beemacs-human-resolve--sub beemacs-human-resolve--id
                beemacs-human-resolve--sid)))
    (let ((inhibit-read-only t))
      (beemacs-human-resolve--render-panel data))
    data))

(defun beemacs-human-resolve-message (message)
  "Send MESSAGE to the current buffer's resolution agent and run one turn.

Calls `beemacs-api-human-message', mirroring the web UI's resolve-page
chat box; the returned panel (transcript/diffstat/status) replaces the
buffer's live section, so the resolver's actual reply is always what is
shown, never an assumed echo."
  (interactive
   (list (read-string (format "Message to resolver for %s/%s: "
                               beemacs-human-resolve--sub beemacs-human-resolve--id))))
  (unless (derived-mode-p 'beemacs-human-resolve-mode)
    (user-error "Not in a beemacs-human-resolve-mode buffer"))
  (unless beemacs-human-resolve--sid
    (user-error "No active resolution session for this buffer"))
  (when (string-empty-p message)
    (user-error "Message must not be empty"))
  (let ((data (beemacs-api-human-message
               beemacs-human-resolve--sub beemacs-human-resolve--id
               beemacs-human-resolve--sid message)))
    (let ((inhibit-read-only t))
      (beemacs-human-resolve--render-panel data))
    (message "beemacs-human %s/%s: turn complete"
             beemacs-human-resolve--sub beemacs-human-resolve--id)
    data))

(defun beemacs-human-resolve-publish ()
  "Publish the current buffer's resolution agent's committed changes to main.

Calls `beemacs-api-human-publish', mirroring the web UI's Publish
button; the response panel's `error' key (distinct from a transport-level
`beemacs-api-error') is surfaced via `message' since the server itself
returns 2xx with an embedded error on a publish failure (see
`apiHumanPublish')."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-resolve-mode)
    (user-error "Not in a beemacs-human-resolve-mode buffer"))
  (unless beemacs-human-resolve--sid
    (user-error "No active resolution session for this buffer"))
  (let* ((data (beemacs-api-human-publish
                beemacs-human-resolve--sub beemacs-human-resolve--id
                beemacs-human-resolve--sid))
         (err (alist-get 'error data)))
    (let ((inhibit-read-only t))
      (beemacs-human-resolve--render-panel data))
    (if (and (stringp err) (not (string-empty-p err)))
        (message "beemacs-human %s/%s: publish failed: %s"
                 beemacs-human-resolve--sub beemacs-human-resolve--id err)
      (message "beemacs-human %s/%s: published to main"
                beemacs-human-resolve--sub beemacs-human-resolve--id))
    data))

(defun beemacs-human-resolve-discard ()
  "Discard the current buffer's resolution agent's unpublished work.

Calls `beemacs-api-human-discard', mirroring the web UI's Discard
button; when the task is still `NEEDS-HUMAN' the server opens a fresh
session, whose id this command adopts so the buffer keeps working the
same task with a clean slate, exactly like reopening the workspace."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-resolve-mode)
    (user-error "Not in a beemacs-human-resolve-mode buffer"))
  (unless beemacs-human-resolve--sid
    (user-error "No active resolution session for this buffer"))
  (let* ((data (beemacs-api-human-discard
                beemacs-human-resolve--sub beemacs-human-resolve--id
                beemacs-human-resolve--sid))
         (new-sid (alist-get 'sid data)))
    (setq beemacs-human-resolve--sid (and (stringp new-sid) new-sid))
    (let ((inhibit-read-only t))
      (beemacs-human-resolve--render-panel
       (if beemacs-human-resolve--sid
           (beemacs-api-human-panel beemacs-human-resolve--sub beemacs-human-resolve--id
                                     beemacs-human-resolve--sid)
         data)))
    (message "beemacs-human %s/%s: discarded%s"
             beemacs-human-resolve--sub beemacs-human-resolve--id
             (if beemacs-human-resolve--sid " (new session opened)" " (task no longer blocked)"))
    data))

(defun beemacs-human-resolve-apply ()
  "Mark the current buffer's task resolved: NEEDS-HUMAN -> TODO.

Calls `beemacs-api-human-resolve', the JSON mirror of the deterministic
`plan.Task.Resolve' + `publishMain' flow (`humanResolveApply') -- never a
direct `PLAN.md'/`ROI.md' edit from this client, exactly the sanctioned
backend flow the web UI's \"Mark resolved\" button drives. Prompts for
confirmation first since this is the terminal action that unblocks the
swarm on this task; a non-`NEEDS-HUMAN' task (already resolved elsewhere)
surfaces the server's real conflict message via `beemacs-api-error'."
  (interactive)
  (unless (derived-mode-p 'beemacs-human-resolve-mode)
    (user-error "Not in a beemacs-human-resolve-mode buffer"))
  (when (yes-or-no-p (format "Mark %s/%s resolved (NEEDS-HUMAN -> TODO)? "
                              beemacs-human-resolve--sub beemacs-human-resolve--id))
    (beemacs-api-human-resolve beemacs-human-resolve--sub beemacs-human-resolve--id)
    (message "beemacs-human %s/%s: resolved (NEEDS-HUMAN -> TODO)"
              beemacs-human-resolve--sub beemacs-human-resolve--id)))

(defvar beemacs-human-resolve-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "m" #'beemacs-human-resolve-message)
    (define-key map "g" #'beemacs-human-resolve-refresh)
    (define-key map "p" #'beemacs-human-resolve-publish)
    (define-key map "d" #'beemacs-human-resolve-discard)
    (define-key map "r" #'beemacs-human-resolve-apply)
    (define-key map "q" #'kill-current-buffer)
    map)
  "Keymap for `beemacs-human-resolve-mode'.")

(define-derived-mode beemacs-human-resolve-mode special-mode "Beemacs-Human-Resolve"
  "Major mode for a NEEDS-HUMAN task's resolution workspace.

Combines the task's static blocker context (desc/body/reason/category)
with the AI resolution agent's live transcript/diffstat/status, mirroring
the web UI's `/human/{sub}/{id}' resolve page: `m' messages the resolver
(`beemacs-human-resolve-message'), `g' re-polls the live panel
(`beemacs-human-resolve-refresh'), `p' publishes its committed changes to
main (`beemacs-human-resolve-publish'), `d' discards its unpublished work
(`beemacs-human-resolve-discard'), `r' marks the task resolved
(`beemacs-human-resolve-apply', the sanctioned NEEDS-HUMAN -> TODO flip),
and `q' kills the buffer.
\\{beemacs-human-resolve-mode-map}"
  (setq buffer-read-only t))

(provide 'beemacs-human)

;;; beemacs-human.el ends here
