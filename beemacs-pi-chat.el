;;; beemacs-pi-chat.el --- Streaming pi agent buffer for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; The user-facing streaming agent buffer built on top of `beemacs-pi.el's
;; RPC transport (`beemacs-pi-rpc-transport').  `beemacs-pi-chat-open' spawns
;; a `pi' RPC child process and pops a dedicated `beemacs-pi-chat-mode'
;; buffer that renders pi's turn/token/tool-call event stream live as it
;; arrives on the process filter -- no polling, no buffering the whole turn
;; before displaying anything.
;;
;; Event schema (newline-delimited JSON objects, one per `beemacs-pi.el'
;; message, each carrying a `type' key):
;;
;;   {"type":"turn_start"}
;;     A new agent turn has begun (in response to a prompt or steer message
;;     this buffer sent).
;;   {"type":"token","text":"..."}
;;     One chunk of the assistant's streamed reply text (markdown source);
;;     appended verbatim to the transcript as it arrives.
;;   {"type":"tool_call","id":"...","name":"...","input":{...}}
;;     The agent is invoking a tool.  ID correlates this call with its
;;     eventual `tool_result'.
;;   {"type":"tool_result","id":"...","output":"..."}
;;     The result of a previously announced `tool_call', matched by ID.
;;   {"type":"turn_end"}
;;     The current turn has finished; the buffer is idle again and may
;;     accept a new prompt.
;;   {"type":"error","message":"..."}
;;     The agent (or the pi process itself) reported an error; the turn is
;;     considered finished.
;;
;; Outbound messages this module sends to the child:
;;
;;   {"type":"prompt","text":"..."}  -- start a new turn (buffer was idle).
;;   {"type":"steer","text":"..."}   -- steer an in-flight turn (buffer was
;;                                     mid-turn); the agent incorporates it
;;                                     without waiting for `turn_end'.
;;   {"type":"abort"}                -- cancel the in-flight turn outright.
;;
;; Killing a `beemacs-pi-chat-mode' buffer always cleanly tears down its `pi'
;; child process via `beemacs-pi-stop' (a `kill-buffer-hook'), so a stray
;; `pi' process never outlives its buffer.
;;
;; Assistant text is markdown source; when the `markdown-mode' package
;; happens to be available (`require'd softly, never a hard dependency of
;; this package) its font-lock keywords are layered on for readability. Its
;; absence never breaks anything -- the transcript is always at least
;; readable plain text.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'beemacs-pi)

(defgroup beemacs-pi-chat nil
  "Streaming pi agent buffer for beemacs."
  :group 'beemacs
  :prefix "beemacs-pi-chat-")

(defvar-local beemacs-pi-chat--handle nil
  "This buffer's `beemacs-pi-process' handle wrapping the `pi' RPC child.")

(defvar-local beemacs-pi-chat--turn-active nil
  "Non-nil while this buffer's `pi' process is mid-turn (between
`turn_start' and `turn_end'/`error').")

(defvar-local beemacs-pi-chat--pending-tools nil
  "Alist of (ID . NAME) for `tool_call' events awaiting their `tool_result'.")

(defvar-local beemacs-pi-chat--name nil
  "The label this `beemacs-pi-chat-mode' buffer's title was created with.")

(defun beemacs-pi-chat--buffer-name (name)
  "Return the chat buffer name for agent session NAME."
  (format "*beemacs-pi-chat: %s*" name))

(defun beemacs-pi-chat--insert (fmt &rest args)
  "Append a formatted string built from FMT and ARGS at the end of the buffer.

Does not add a trailing newline -- callers control their own line breaks so
streamed `token' text can be appended mid-line without spurious breaks."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (insert (apply #'format fmt args)))
    (goto-char (point-max))))

(defun beemacs-pi-chat--handle-event (buffer event)
  "Dispatch parsed JSON EVENT for BUFFER's `beemacs-pi-chat-mode' transcript.

EVENT is an alist with a `type' key; see this file's Commentary for the
full event schema. Runs with BUFFER current regardless of which buffer was
selected when the underlying process filter fired."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let ((type (alist-get 'type event)))
        (cond
         ((equal type "turn_start")
          (setq beemacs-pi-chat--turn-active t)
          (beemacs-pi-chat--insert "\n"))
         ((equal type "token")
          (beemacs-pi-chat--insert "%s" (or (alist-get 'text event) "")))
         ((equal type "tool_call")
          (let ((id (alist-get 'id event))
                (name (alist-get 'name event))
                (input (alist-get 'input event)))
            (push (cons id name) beemacs-pi-chat--pending-tools)
            (beemacs-pi-chat--insert
             "\n[tool-call] %s%s\n" (or name "")
             (if input (format " %s" (json-encode input)) ""))))
         ((equal type "tool_result")
          (let* ((id (alist-get 'id event))
                 (tool-name (alist-get id beemacs-pi-chat--pending-tools nil nil #'equal))
                 (output (alist-get 'output event)))
            (setq beemacs-pi-chat--pending-tools
                  (assoc-delete-all id beemacs-pi-chat--pending-tools #'equal))
            (beemacs-pi-chat--insert
             "[tool-result] %s -> %s\n" (or tool-name id) (or output ""))))
         ((equal type "turn_end")
          (setq beemacs-pi-chat--turn-active nil)
          (beemacs-pi-chat--insert "\n"))
         ((equal type "error")
          (setq beemacs-pi-chat--turn-active nil)
          (beemacs-pi-chat--insert "\n[error] %s\n" (or (alist-get 'message event) "")))
         (t
          (beemacs-pi-chat--insert "\n[unknown event] %s\n" (json-encode event))))))))

(defun beemacs-pi-chat--stop-process ()
  "Cleanly stop the current buffer's `pi' process, if any (kill-buffer-hook)."
  (when (and beemacs-pi-chat--handle
             (beemacs-pi-alive-p beemacs-pi-chat--handle))
    (beemacs-pi-stop beemacs-pi-chat--handle)))

;;;###autoload
(defun beemacs-pi-chat-open (name)
  "Spawn a `pi' RPC process and pop a streaming agent buffer labeled NAME.

Starts the child via `beemacs-pi-start' with an `on-message' callback that
dispatches every parsed event to `beemacs-pi-chat--handle-event' for this
buffer, so the transcript renders live as messages arrive rather than after
a whole turn completes. Registers a buffer-local `kill-buffer-hook' that
tears the process down via `beemacs-pi-stop' so killing the buffer never
leaks a `pi' child."
  (interactive "sAgent session name: ")
  (let* ((buf (get-buffer-create (beemacs-pi-chat--buffer-name name))))
    (with-current-buffer buf
      (beemacs-pi-chat-mode)
      (setq beemacs-pi-chat--name name)
      (let ((this-buf buf))
        (setq beemacs-pi-chat--handle
              (beemacs-pi-start
               (lambda (event) (beemacs-pi-chat--handle-event this-buf event)))))
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "pi agent: %s\n" name))
        (insert "Type `s' to send a prompt, `a' to abort the in-flight turn, `q' to close.\n")))
    (pop-to-buffer buf)
    (message "beemacs-pi-chat: opened agent session %s" name)
    buf))

(defun beemacs-pi-chat-send (text)
  "Send TEXT to the current buffer's `pi' process as a prompt or a steer.

When no turn is in flight, sends `{\"type\":\"prompt\",\"text\":TEXT}' to
start a new turn; when a turn IS in flight, sends
`{\"type\":\"steer\",\"text\":TEXT}' instead so the running agent
incorporates it without the caller having to wait for `turn_end' first --
this is the buffer's steering support. Echoes TEXT into the transcript
either way so the user's own input is part of the visible history."
  (interactive
   (list (read-string (format "Message to %s: " (or beemacs-pi-chat--name "pi")))))
  (unless (derived-mode-p 'beemacs-pi-chat-mode)
    (user-error "Not in a beemacs-pi-chat-mode buffer"))
  (unless beemacs-pi-chat--handle
    (user-error "No pi process for this buffer"))
  (when (string-empty-p text)
    (user-error "Message must not be empty"))
  (let ((steering beemacs-pi-chat--turn-active))
    (beemacs-pi-chat--insert "\n%s> %s\n" (if steering "(steer) " "") text)
    (beemacs-pi-send beemacs-pi-chat--handle
                      `((type . ,(if steering "steer" "prompt")) (text . ,text)))))

(defun beemacs-pi-chat-abort ()
  "Abort the current buffer's in-flight `pi' turn.

Sends `{\"type\":\"abort\"}' to the child unconditionally -- an abort with
no turn in flight is simply ignored by the agent side -- rather than
silently no-oping locally, since only the agent can know whether a turn is
truly still running."
  (interactive)
  (unless (derived-mode-p 'beemacs-pi-chat-mode)
    (user-error "Not in a beemacs-pi-chat-mode buffer"))
  (unless beemacs-pi-chat--handle
    (user-error "No pi process for this buffer"))
  (beemacs-pi-chat--insert "\n[abort requested]\n")
  (beemacs-pi-send beemacs-pi-chat--handle '((type . "abort"))))

(defvar beemacs-pi-chat-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "s" #'beemacs-pi-chat-send)
    (define-key map "a" #'beemacs-pi-chat-abort)
    (define-key map "q" #'kill-current-buffer)
    map)
  "Keymap for `beemacs-pi-chat-mode'.")

(define-derived-mode beemacs-pi-chat-mode special-mode "Beemacs-Pi-Chat"
  "Major mode for a streaming pi agent buffer.

Renders pi's turn/token/tool-call RPC event stream live: `s' sends a prompt
(or steers an in-flight turn, see `beemacs-pi-chat-send'), `a' aborts the
in-flight turn (`beemacs-pi-chat-abort'), and `q' kills the buffer, which
always cleanly tears down the underlying `pi' child process first (see
`beemacs-pi-chat--stop-process' on `kill-buffer-hook'). Assistant text is
markdown source, decorated with `markdown-mode' font-lock when that
package happens to be available (never a hard dependency).
\\{beemacs-pi-chat-mode-map}"
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'beemacs-pi-chat--stop-process nil t)
  (when (require 'markdown-mode nil t)
    (when (boundp 'markdown-mode-font-lock-keywords)
      (setq-local font-lock-defaults '(markdown-mode-font-lock-keywords)))))

(provide 'beemacs-pi-chat)

;;; beemacs-pi-chat.el ends here
