;;; beemacs-editor.el --- Agentic editor client for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Emacs client for beehived's agentic editor -- the JSON `/api/editor/*'
;; write/poll surface documented in `repo/docs/api-contract.md' and audited
;; by the `beemacs-api-contract' task. Mirrors the web UI's `/editor/{id}'
;; chat page: open a session for a repo-relative file, converse with the
;; edit agent, view its proposed diff, and merge (or abandon) the result --
;; every command reports the server's real JSON response, never a guess.
;;
;; Server routes this module drives (`internal/web/editor.go',
;; `internal/web/web.go'):
;;   POST /api/editor            -- open a session for a file
;;   GET  /api/editor/{id}       -- poll state/log (not yet wrapped here;
;;                                  every write call below already returns
;;                                  the post-call state)
;;   POST /api/editor/{id}/chat  -- send a chat message, get the reply
;;   POST /api/editor/{id}/merge -- merge the session's edits to main
;;   GET  /api/editor/{id}/diff  -- fetch the base/proposed file content
;;
;; There is deliberately no `/api/editor/{id}/close' JSON route (only the
;; HTML `POST /editor/{id}/close' exists, which just redirects a browser);
;; `beemacs-editor-close' is therefore a client-side-only cleanup of the
;; local chat buffer -- see its docstring.

;;; Code:

(require 'cl-lib)
(require 'diff-mode)
(require 'beemacs-transport)
(require 'beemacs-api)
(require 'beemacs-render)

(defgroup beemacs-editor nil
  "Agentic editor client for beemacs."
  :group 'beemacs
  :prefix "beemacs-editor-")

(defvar-local beemacs-editor--id nil
  "The beehived editor session id this `beemacs-editor-mode' buffer wraps.")

(defvar-local beemacs-editor--file nil
  "The repo-relative file this `beemacs-editor-mode' buffer's session edits.")

(defvar-local beemacs-editor--branch nil
  "The git branch beehived created for this editor session's edits.")

(defvar-local beemacs-editor--state nil
  "The last-known server-reported state string for this editor session.")

(defun beemacs-editor--buffer-name (id file)
  "Return the chat buffer name for editor session ID editing FILE."
  (format "*beemacs-editor: %s (%s)*" id file))

(defun beemacs-editor--find-buffer (id)
  "Return the live `beemacs-editor-mode' buffer for session ID, or nil."
  (cl-find-if (lambda (buf)
                (with-current-buffer buf
                  (and (derived-mode-p 'beemacs-editor-mode)
                       (equal beemacs-editor--id id))))
              (buffer-list)))

(defun beemacs-editor--get-buffer (id)
  "Return the live `beemacs-editor-mode' buffer for session ID.

Signals `user-error' if no such buffer is open -- every command below
operates on a session that `beemacs-editor-open' already created a buffer
for, so a missing buffer means the session was opened in a different
Emacs (or its buffer was killed) and the id is stale to this client."
  (or (beemacs-editor--find-buffer id)
      (user-error "No open beemacs-editor buffer for session %s" id)))

(defun beemacs-editor--insert-log (fmt &rest args)
  "Append a formatted line built from FMT and ARGS to the current buffer.

Assumes `beemacs-editor-mode' has already made the buffer writable for
the duration of the insertion (see callers)."
  (goto-char (point-max))
  (insert (apply #'format fmt args) "\n"))

;;;###autoload
(defun beemacs-editor-open (file)
  "Open a beehived agentic editor session for FILE and its chat buffer.

Calls `POST /api/editor' with `{\"file\": FILE}' (`beemacs-api-json-post',
mirroring `apiEditorOpen' in `internal/web/editor.go'), which opens (or
resumes) an edit session for the repo-relative path FILE and returns
`{\"id\", \"file\", \"branch\", \"state\"}'. Pops to a fresh
`beemacs-editor-mode' buffer for the session, named after its id and file,
reporting the real session id/branch/state -- never a placeholder.

Interactively, prompts for FILE (a repo-relative path, matching the
`?path='/`?file=' the web UI's edit-with-AI links pass)."
  (interactive "sFile to edit (repo-relative path): ")
  (let* ((resp (beemacs-api-json-post "/api/editor" `((file . ,file))))
         (id (alist-get 'id resp))
         (session-file (or (alist-get 'file resp) file))
         (branch (alist-get 'branch resp))
         (state (alist-get 'state resp))
         (buf (get-buffer-create (beemacs-editor--buffer-name id session-file))))
    (with-current-buffer buf
      (beemacs-editor-mode)
      (setq beemacs-editor--id id
            beemacs-editor--file session-file
            beemacs-editor--branch branch
            beemacs-editor--state state)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (beemacs-editor--insert-log "Editor session %s" id)
        (beemacs-editor--insert-log "File:   %s" session-file)
        (beemacs-editor--insert-log "Branch: %s" (or branch ""))
        (beemacs-editor--insert-log "State:  %s" (or state ""))
        (beemacs-editor--insert-log "")
        (beemacs-editor--insert-log "Type `C' to send a chat message, `d' to view the diff,")
        (beemacs-editor--insert-log "`m' to merge, `q' to close this buffer.")))
    (pop-to-buffer buf)
    (message "beemacs-editor: opened session %s for %s (state: %s)" id session-file state)
    id))

(defun beemacs-editor-chat (message)
  "Send MESSAGE to the current buffer's beehived editor session.

Calls `POST /api/editor/{id}/chat' with `{\"message\": MESSAGE}'
(`beemacs-api-json-post', mirroring `apiEditorChat'), which runs the edit
agent's turn synchronously and returns `{\"reply\", \"state\", \"merged\"}'.
Appends both the sent MESSAGE and the agent's real reply to the buffer,
updates the tracked session state, and reports it -- never assumes the
turn succeeded without the server's own reply text."
  (interactive
   (list (read-string (format "Message to editor %s: " beemacs-editor--id))))
  (unless (derived-mode-p 'beemacs-editor-mode)
    (user-error "Not in a beemacs-editor-mode buffer"))
  (when (string-empty-p message)
    (user-error "Message must not be empty"))
  (let* ((id beemacs-editor--id)
         (resp (beemacs-api-json-post (format "/api/editor/%s/chat" id)
                                        `((message . ,message))))
         (reply (alist-get 'reply resp))
         (state (alist-get 'state resp))
         (merged (alist-get 'merged resp)))
    (let ((inhibit-read-only t))
      (beemacs-editor--insert-log "")
      (beemacs-editor--insert-log "> %s" message)
      (beemacs-editor--insert-log "%s" (or reply ""))
      (beemacs-editor--insert-log "[state: %s%s]" (or state "")
                                    (if (eq merged t) ", merged" "")))
    (setq beemacs-editor--state state)
    (message "beemacs-editor %s: %s" id (or state ""))
    resp))

(defun beemacs-editor-diff ()
  "Show the current buffer's editor session's proposed diff in `diff-mode'.

Calls `GET /api/editor/{id}/diff' (`beemacs-api-json-request', mirroring
`apiEditorDiff'), which returns `{\"base\", \"proposed\", \"state\"}' --
the file's content before and after the agent's in-progress edits.
Renders a unified diff between them with `beemacs-render-unified-diff'
(the same hermetic, no-external-`diff'-process renderer the commit
browser uses) into a dedicated read-only `diff-mode' buffer, labeled with
the session's file path."
  (interactive)
  (unless (derived-mode-p 'beemacs-editor-mode)
    (user-error "Not in a beemacs-editor-mode buffer"))
  (let* ((id beemacs-editor--id)
         (file beemacs-editor--file)
         (resp (beemacs-api-json-request (format "/api/editor/%s/diff" id)))
         (base (or (alist-get 'base resp) ""))
         (proposed (or (alist-get 'proposed resp) ""))
         (state (alist-get 'state resp))
         (buf (get-buffer-create (format "*beemacs-editor-diff: %s*" id))))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (beemacs-render-unified-diff base proposed file))
        (goto-char (point-min)))
      (diff-mode)
      (setq buffer-read-only t)
      (view-mode 1))
    (pop-to-buffer buf)
    (message "beemacs-editor %s: diff fetched (state: %s)" id (or state ""))
    resp))

(defun beemacs-editor-merge (&optional confirm)
  "Merge the current buffer's beehived editor session's edits to main.

Calls `POST /api/editor/{id}/merge' with `{\"confirm\": CONFIRM}'
(`beemacs-api-json-post', mirroring `apiEditorMerge'), which merges the
session's edit branch. Without a prefix argument CONFIRM is nil/false,
matching the server default-block on a protected deletion
(`editor.ErrDeleteNeedsConfirm', surfaced as HTTP 409); with a prefix
argument (`C-u M-x beemacs-editor-merge') CONFIRM is t, authorizing that
deletion. Reports the real post-merge `state' the server returns; a 409
block surfaces its message via `beemacs-api-json-post''s normal
`beemacs-api-error' signal (re-run with a prefix arg to authorize)."
  (interactive "P")
  (unless (derived-mode-p 'beemacs-editor-mode)
    (user-error "Not in a beemacs-editor-mode buffer"))
  (let* ((id beemacs-editor--id)
         (resp (beemacs-api-json-post (format "/api/editor/%s/merge" id)
                                        `((confirm . ,(if confirm t :json-false)))))
         (state (alist-get 'state resp)))
    (let ((inhibit-read-only t))
      (beemacs-editor--insert-log "")
      (beemacs-editor--insert-log "[merge] state: %s" (or state "")))
    (setq beemacs-editor--state state)
    (message "beemacs-editor %s: merged (state: %s)" id (or state ""))
    resp))

(defun beemacs-editor-close ()
  "Close the current buffer's local editor session view.

There is no JSON `/api/editor/{id}/close' route on beehived today (only
the HTML `POST /editor/{id}/close', which redirects a browser to `/' --
see `repo/docs/api-contract.md''s HTML-vs-JSON split) -- so this command
is deliberately client-side-only: it kills the local chat buffer and
reports that no server-side teardown call was made, rather than silently
pretending to have closed a session it cannot actually reach. The
session itself remains open server-side (reachable again via its id
through the HTML `/editor/{id}' page, or a future `beemacs-editor-open'
on the same file) until beehived's own session lifecycle closes it."
  (interactive)
  (unless (derived-mode-p 'beemacs-editor-mode)
    (user-error "Not in a beemacs-editor-mode buffer"))
  (let ((id beemacs-editor--id))
    (kill-buffer (current-buffer))
    (message "beemacs-editor %s: local buffer closed (no JSON close endpoint exists; server session unaffected)" id)))

(defvar beemacs-editor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "C" #'beemacs-editor-chat)
    (define-key map "d" #'beemacs-editor-diff)
    (define-key map "m" #'beemacs-editor-merge)
    (define-key map "q" #'beemacs-editor-close)
    map)
  "Keymap for `beemacs-editor-mode'.")

(define-derived-mode beemacs-editor-mode special-mode "Beemacs-Editor"
  "Major mode for a beehived agentic editor chat session.

Mirrors the beehived web UI's `/editor/{id}' chat page over the JSON
`/api/editor/*' surface: `C' chats with the edit agent
(`beemacs-editor-chat'), `d' views its proposed diff
(`beemacs-editor-diff'), `m' merges the edits (`beemacs-editor-merge'),
and `q' closes the local buffer (`beemacs-editor-close').
\\{beemacs-editor-mode-map}"
  (setq buffer-read-only t))

(provide 'beemacs-editor)

;;; beemacs-editor.el ends here
