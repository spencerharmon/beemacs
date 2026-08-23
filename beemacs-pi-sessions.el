;;; beemacs-pi-sessions.el --- Session selector for pi's session tree -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; A selector over `pi's session tree, built on top of `beemacs-pi.el's RPC
;; transport and `beemacs-pi-chat.el's streaming agent buffer.
;;
;; pi's RPC mode answers a `list_sessions' request with a `session_list'
;; message enumerating every known session as a flat list of records, each
;; carrying enough information to reconstruct the tree client-side:
;;
;;   {"type":"list_sessions"}                          (request)
;;   {"type":"session_list",
;;    "sessions":[{"id":"s1","label":"...","parent":null,"updated":"..."},
;;                {"id":"s2","label":"...","parent":"s1","updated":"..."}]}
;;                                                      (response)
;;
;; `beemacs-pi-sessions-list' spawns a short-lived RPC process (via
;; `beemacs-pi-start'), sends `list_sessions', waits synchronously for the
;; `session_list' reply, tears the process down, and returns the parsed
;; session records (parent-sorted so a session always appears after its
;; parent -- the shape `beemacs-pi-sessions--build-tree' expects).
;;
;; `beemacs-pi-sessions-open' pops a `tabulated-list-mode' buffer rendering
;; that tree (each row indented by its depth, most-recently-updated first
;; within a sibling group) with three actions:
;;
;;   RET / r  `beemacs-pi-sessions-resume'   -- resume the session at point
;;            in a new `beemacs-pi-chat-mode' buffer (a `{"type":"resume",
;;            "id":ID}' request sent immediately after the chat buffer's
;;            `pi' process starts).
;;   c        `beemacs-pi-sessions-continue' -- like resume, but sends
;;            `{"type":"continue","id":ID}' -- pi's convention for picking
;;            the session back up mid-turn rather than replaying it from
;;            the top.
;;   b        `beemacs-pi-sessions-branch'   -- fork a NEW session from the
;;            one at point (`{"type":"branch","from":ID}'), landing in its
;;            own chat buffer once pi assigns the new session an id.
;;
;; Every resume/continue/branch records the target session in a bounded
;; per-install MRU persisted to `beemacs-pi-sessions-persist-file' (a plain
;; s-expression: a list of session ids, most-recent first, capped at
;; `beemacs-pi-sessions-mru-limit'), read/written via `beemacs-persistence's
;; generic `beemacs-persistence-read-file'/`beemacs-persistence-write-file'
;; helpers rather than duplicating that logic.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tabulated-list)
(require 'beemacs-pi)
(require 'beemacs-pi-chat)
(require 'beemacs-persistence)

(defgroup beemacs-pi-sessions nil
  "Session selector over pi's session tree."
  :group 'beemacs
  :prefix "beemacs-pi-sessions-")

(defcustom beemacs-pi-sessions-persist-file
  (expand-file-name "beemacs-pi-sessions-mru.el" user-emacs-directory)
  "File used to persist the bounded MRU of recently visited pi sessions.

One s-expression: a list of session id strings, most-recent first.  Shares
its shape with beemacs-persistence's shared MRU store so that workstream
can later absorb this file rather than requiring a format migration."
  :group 'beemacs-pi-sessions
  :type 'file)

(defcustom beemacs-pi-sessions-mru-limit 20
  "Maximum number of session ids kept in the persisted MRU list."
  :group 'beemacs-pi-sessions
  :type 'integer)

(defcustom beemacs-pi-sessions-list-timeout 5
  "Seconds to wait for a `session_list' reply to a `list_sessions' request."
  :group 'beemacs-pi-sessions
  :type 'number)

(define-error 'beemacs-pi-sessions-error "beemacs pi session operation failed")

(defvar-local beemacs-pi-sessions--records nil
  "The flat list of session records last rendered by this selector buffer.")

;;; MRU persistence

(defun beemacs-pi-sessions--load-mru ()
  "Return the persisted MRU list of session ids, or nil if none/unreadable.

Reads via the shared `beemacs-persistence-read-file' helper rather than
duplicating the file-read logic."
  (beemacs-persistence-read-file beemacs-pi-sessions-persist-file #'listp))

(defun beemacs-pi-sessions--save-mru (ids)
  "Persist IDS (a list of session id strings) to `beemacs-pi-sessions-persist-file'.

Writes via the shared `beemacs-persistence-write-file' helper rather than
duplicating the file-write logic."
  (beemacs-persistence-write-file beemacs-pi-sessions-persist-file ids))

(defun beemacs-pi-sessions-record-visit (id)
  "Record ID as the most-recently-visited pi session in the persisted MRU.

Moves ID to the front, dedupes, and truncates to
`beemacs-pi-sessions-mru-limit'."
  (let* ((current (beemacs-pi-sessions--load-mru))
         (updated (cons id (delete id (copy-sequence current)))))
    (when (> (length updated) beemacs-pi-sessions-mru-limit)
      (setq updated (cl-subseq updated 0 beemacs-pi-sessions-mru-limit)))
    (beemacs-pi-sessions--save-mru updated)
    updated))

(defun beemacs-pi-sessions-mru ()
  "Return the persisted MRU list of session ids, most-recent first."
  (or (beemacs-pi-sessions--load-mru) '()))

;;; Listing

(defun beemacs-pi-sessions--session-id (record)
  "Return the `id' field of session RECORD (an alist)."
  (alist-get 'id record))

(defun beemacs-pi-sessions--session-parent (record)
  "Return the `parent' field of session RECORD, or nil for a root session."
  (let ((parent (alist-get 'parent record)))
    (unless (or (null parent) (eq parent :null) (equal parent ""))
      parent)))

(defun beemacs-pi-sessions--session-label (record)
  "Return a human-readable label for session RECORD."
  (or (alist-get 'label record) (beemacs-pi-sessions--session-id record)))

(defun beemacs-pi-sessions--session-updated (record)
  "Return the `updated' timestamp field of session RECORD, or nil."
  (alist-get 'updated record))

(defun beemacs-pi-sessions-list (&optional executable)
  "Fetch the flat list of pi session records via a one-shot RPC round trip.

Spawns a `pi' RPC process (EXECUTABLE, or `beemacs-pi-executable'), sends
`{\"type\":\"list_sessions\"}', waits up to
`beemacs-pi-sessions-list-timeout' seconds for a `{\"type\":\"session_list\",
\"sessions\":[...]}' reply, tears the process down, and returns the
`sessions' vector converted to a list of alists.

Signals `beemacs-pi-sessions-error' if the process never replies within
the timeout, or if it exits/errors instead."
  (let* ((beemacs-pi-executable (or executable beemacs-pi-executable))
         (reply nil)
         (handle (beemacs-pi-start
                  (lambda (event)
                    (when (equal (alist-get 'type event) "session_list")
                      (setq reply event))))))
    (unwind-protect
        (progn
          (beemacs-pi-send handle '((type . "list_sessions")))
          (with-timeout (beemacs-pi-sessions-list-timeout nil)
            (while (not reply)
              (accept-process-output nil 0.05)))
          (unless reply
            (signal 'beemacs-pi-sessions-error
                    (list "timed out waiting for pi session_list reply")))
          (append (alist-get 'sessions reply) nil))
      (beemacs-pi-stop handle))))

;;; Tree building / rendering

(defun beemacs-pi-sessions--build-tree (records)
  "Return RECORDS ordered as a depth-first walk of the parent/child tree.

Each element of the result is a cons of (DEPTH . RECORD).  Root sessions
(no parent, or a parent not present in RECORDS) come first, each followed
immediately by its descendants; siblings are ordered most-recently-updated
first using `beemacs-pi-sessions--session-updated' (string collation is
adequate for ISO-8601-shaped timestamps, and stable otherwise)."
  (let ((by-parent (make-hash-table :test 'equal))
        (ids (make-hash-table :test 'equal))
        (out nil))
    (dolist (r records)
      (puthash (beemacs-pi-sessions--session-id r) t ids))
    (dolist (r records)
      (let* ((parent (beemacs-pi-sessions--session-parent r))
             (key (if (and parent (gethash parent ids)) parent :root)))
        (puthash key (cons r (gethash key by-parent)) by-parent)))
    (maphash (lambda (k v)
               (puthash k (sort v (lambda (a b)
                                     (string> (or (beemacs-pi-sessions--session-updated a) "")
                                              (or (beemacs-pi-sessions--session-updated b) ""))))
                        by-parent))
             by-parent)
    (cl-labels ((walk (key depth)
                  (dolist (r (gethash key by-parent))
                    (push (cons depth r) out)
                    (walk (beemacs-pi-sessions--session-id r) (1+ depth)))))
      (walk :root 0))
    (nreverse out)))

(defun beemacs-pi-sessions--entries ()
  "Return `tabulated-list-entries' rendering `beemacs-pi-sessions--records'."
  (mapcar
   (lambda (depth-record)
     (let* ((depth (car depth-record))
            (record (cdr depth-record))
            (id (beemacs-pi-sessions--session-id record))
            (label (beemacs-pi-sessions--session-label record))
            (updated (or (beemacs-pi-sessions--session-updated record) "")))
       (list record
             (vector (concat (make-string (* 2 depth) ?\s) label)
                     id
                     updated))))
   (beemacs-pi-sessions--build-tree beemacs-pi-sessions--records)))

(defun beemacs-pi-sessions--current-id ()
  "Return the session id of the row at point, or signal a user error."
  (let ((record (tabulated-list-get-id)))
    (unless record
      (user-error "No session at point"))
    (beemacs-pi-sessions--session-id record)))

;;;###autoload
(defun beemacs-pi-sessions-open ()
  "Pop a selector buffer rendering pi's session tree.

Fetches the current session list via `beemacs-pi-sessions-list' and
renders it as an indented tree, most-recently-updated siblings first.
`g' refreshes; `RET'/`r' resumes, `c' continues, and `b' branches the
session at point (see this file's Commentary)."
  (interactive)
  (let ((records (beemacs-pi-sessions-list)))
    (with-current-buffer (get-buffer-create "*beemacs-pi-sessions*")
      (beemacs-pi-sessions-mode)
      (setq beemacs-pi-sessions--records records)
      (setq tabulated-list-entries #'beemacs-pi-sessions--entries)
      (tabulated-list-print t)
      (pop-to-buffer (current-buffer)))))

(defun beemacs-pi-sessions-refresh ()
  "Re-fetch the session list and redraw the current selector buffer."
  (interactive)
  (unless (derived-mode-p 'beemacs-pi-sessions-mode)
    (user-error "Not in a beemacs-pi-sessions-mode buffer"))
  (setq beemacs-pi-sessions--records (beemacs-pi-sessions-list))
  (tabulated-list-print t))

;;; Actions

(defun beemacs-pi-sessions--open-chat-with (id request-type extra label-suffix)
  "Open a chat buffer for session ID, sending a REQUEST-TYPE message first.

REQUEST-TYPE is \"resume\", \"continue\", or \"branch\"; EXTRA is an alist
merged into the outbound request alongside `(type . REQUEST-TYPE)'.
LABEL-SUFFIX is appended to the chat buffer's session label. Records ID in
the persisted MRU via `beemacs-pi-sessions-record-visit'."
  (beemacs-pi-sessions-record-visit id)
  (let ((buf (beemacs-pi-chat-open (concat id label-suffix))))
    (with-current-buffer buf
      (beemacs-pi-send beemacs-pi-chat--handle
                        (append (list (cons 'type request-type)) extra)))
    buf))

(defun beemacs-pi-sessions-resume ()
  "Resume the pi session at point in a new chat buffer."
  (interactive)
  (let ((id (beemacs-pi-sessions--current-id)))
    (beemacs-pi-sessions--open-chat-with id "resume" `((id . ,id)) "")))

(defun beemacs-pi-sessions-continue ()
  "Continue the pi session at point (mid-turn) in a new chat buffer."
  (interactive)
  (let ((id (beemacs-pi-sessions--current-id)))
    (beemacs-pi-sessions--open-chat-with id "continue" `((id . ,id)) "")))

(defun beemacs-pi-sessions-branch ()
  "Branch a new pi session from the session at point in a new chat buffer."
  (interactive)
  (let ((id (beemacs-pi-sessions--current-id)))
    (beemacs-pi-sessions--open-chat-with id "branch" `((from . ,id)) " (branch)")))

(defvar beemacs-pi-sessions-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map (kbd "RET") #'beemacs-pi-sessions-resume)
    (define-key map "r" #'beemacs-pi-sessions-resume)
    (define-key map "c" #'beemacs-pi-sessions-continue)
    (define-key map "b" #'beemacs-pi-sessions-branch)
    (define-key map "g" #'beemacs-pi-sessions-refresh)
    map)
  "Keymap for `beemacs-pi-sessions-mode'.")

(define-derived-mode beemacs-pi-sessions-mode tabulated-list-mode "Beemacs-Pi-Sessions"
  "Major mode listing pi's session tree with resume/continue/branch actions.
\\{beemacs-pi-sessions-mode-map}"
  (setq tabulated-list-format
        [("Session" 40 t) ("Id" 20 t) ("Updated" 20 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(provide 'beemacs-pi-sessions)

;;; beemacs-pi-sessions.el ends here
