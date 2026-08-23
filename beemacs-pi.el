;;; beemacs-pi.el --- pi child-process transport for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Foundation of the pi-agent-harness workstream: spawn and manage a `pi'
;; child process, talking to it over its RPC mode (newline-delimited JSON
;; over stdin/stdout) as the primary transport, falling back to pi's
;; one-shot JSON print mode for a single request/response exchange when RPC
;; is unavailable or undesired.  This module never screen-scrapes pi's TUI
;; output -- every interaction is either RPC JSON framing or a one-shot JSON
;; print invocation.
;;
;; Process lifecycle:
;;   - `beemacs-pi-start' spawns a pi RPC process via `make-process' and
;;     returns a process object usable with the other functions here.
;;   - `beemacs-pi-alive-p' / `beemacs-pi-health-check' answer whether the
;;     process is still alive and responsive.
;;   - `beemacs-pi-send' writes one JSON request line to a running RPC
;;     process's stdin.
;;   - `beemacs-pi-stop' performs a clean shutdown (closing stdin first,
;;     then killing the process if it does not exit on its own).
;;   - `beemacs-pi-run-oneshot' invokes pi in one-shot JSON print mode
;;     (`pi --print --output-format json <prompt>' by convention) for a
;;     single non-interactive exchange, used when a long-lived RPC process
;;     is not warranted.
;;
;; Errors are surfaced through the `beemacs-pi-error' error symbol rather
;; than being silently swallowed -- a failed spawn, a non-zero one-shot
;; exit, or malformed JSON from the child all signal it with a descriptive
;; message so callers (and the user, via the normal Emacs error surface)
;; see what actually went wrong.

;;; Code:

(require 'json)
(require 'cl-lib)
(require 'beemacs-persistence)

(defgroup beemacs-pi nil
  "pi child-process transport for beemacs."
  :group 'beemacs
  :prefix "beemacs-pi-")

(defcustom beemacs-pi-executable "pi"
  "Path to the `pi' executable used to spawn RPC and one-shot processes.

May be a bare command name resolved via `exec-path'/`PATH', or an absolute
path to a specific `pi' binary."
  :group 'beemacs-pi
  :type 'string)

(defun beemacs-pi-set-executable (path)
  "Persist PATH as this install's `pi' executable, and apply it immediately.

Uses `beemacs-persistence-set-pi-executable' so the choice survives to
the next session."
  (beemacs-persistence-set-pi-executable path)
  (setq beemacs-pi-executable path))

(defun beemacs-pi--apply-persisted-executable ()
  "Apply this install's persisted `pi' executable path, if any, at load time."
  (when-let* ((path (beemacs-persistence-pi-executable)))
    (setq beemacs-pi-executable path)))

(defcustom beemacs-pi-rpc-args '("--rpc")
  "Extra command-line arguments passed to `pi' when starting an RPC process."
  :group 'beemacs-pi
  :type '(repeat string))

(defcustom beemacs-pi-oneshot-args '("--print" "--output-format" "json")
  "Extra command-line arguments passed to `pi' for a one-shot JSON request.

The prompt/request text itself is appended after these arguments by
`beemacs-pi-run-oneshot'."
  :group 'beemacs-pi
  :type '(repeat string))

(defcustom beemacs-pi-process-name "beemacs-pi"
  "Base name used for `pi' RPC processes started by `beemacs-pi-start'."
  :group 'beemacs-pi
  :type 'string)

(define-error 'beemacs-pi-error "beemacs pi process failed")

(defvar-local beemacs-pi--buffer-accum nil
  "Ignored; placeholder to keep `defvar-local' parity with other modules.")

(cl-defstruct (beemacs-pi-process
               (:constructor beemacs-pi-process--create))
  "Handle wrapping a spawned `pi' RPC child process.

PROC is the underlying Emacs process object.  BUFFER accumulates raw
stdout bytes as they arrive so multi-chunk JSON lines can be reassembled.
PENDING holds the not-yet-newline-terminated tail of BUFFER.  ON-MESSAGE,
when non-nil, is called with one parsed JSON value each time a complete
newline-delimited JSON message arrives on stdout."
  proc
  buffer
  pending
  on-message)

(defun beemacs-pi--parse-json (string)
  "Parse JSON STRING into an elisp structure using alists/vectors.

Signals `beemacs-pi-error' (rather than a raw `json-error') when STRING is
not valid JSON, so a malformed line from a misbehaving `pi' process is
surfaced as a real, actionable error instead of an opaque parser failure."
  (condition-case err
      (let ((json-object-type 'alist)
            (json-array-type 'vector)
            (json-key-type 'symbol))
        (json-read-from-string string))
    (error
     (signal 'beemacs-pi-error
             (list (format "malformed JSON from pi: %s (%s)"
                            string (error-message-string err)))))))

(defun beemacs-pi--filter (proc-handle)
  "Return a process filter function closing over PROC-HANDLE.

The filter accumulates raw output on `beemacs-pi-process-buffer', splits
it on newlines (pi's RPC framing is newline-delimited JSON, one message
per line), parses each complete line as JSON, and -- when
`beemacs-pi-process-on-message' is set -- invokes it with the parsed
value.  A parse failure on one line signals `beemacs-pi-error' but does
not stop the filter from processing subsequent lines correctly, since the
malformed line has already been consumed from the pending buffer."
  (lambda (_proc chunk)
    (setf (beemacs-pi-process-buffer proc-handle)
          (concat (or (beemacs-pi-process-pending proc-handle) "") chunk))
    (let ((lines (split-string (beemacs-pi-process-buffer proc-handle) "\n")))
      ;; The last element is either "" (buffer ended on a newline) or an
      ;; incomplete tail to keep pending for the next chunk.
      (setf (beemacs-pi-process-pending proc-handle) (car (last lines)))
      (dolist (line (butlast lines))
        (unless (string-empty-p (string-trim line))
          (let ((parsed (beemacs-pi--parse-json line)))
            (when (beemacs-pi-process-on-message proc-handle)
              (funcall (beemacs-pi-process-on-message proc-handle) parsed))))))))

(defun beemacs-pi-start (&optional on-message)
  "Spawn a `pi' RPC child process and return a `beemacs-pi-process' handle.

Runs `beemacs-pi-executable' with `beemacs-pi-rpc-args' via `make-process',
wiring stdin/stdout as pipes.  ON-MESSAGE, if given, is called with each
parsed JSON message the child writes to stdout (see `beemacs-pi--filter').

Signals `beemacs-pi-error' if the process cannot be started at all (e.g.
the executable is missing) -- `make-process' itself signals a generic
`file-missing'/`error' in that case, which this wraps for a consistent
error surface across every failure mode in this module."
  (let* ((proc-handle (beemacs-pi-process--create
                        :buffer "" :pending "" :on-message on-message))
         (name (generate-new-buffer-name (format " *%s*" beemacs-pi-process-name))))
    (condition-case err
        (let ((proc (make-process
                     :name beemacs-pi-process-name
                     :buffer (generate-new-buffer name)
                     :command (cons beemacs-pi-executable beemacs-pi-rpc-args)
                     :connection-type 'pipe
                     :noquery t
                     :filter (beemacs-pi--filter proc-handle))))
          (setf (beemacs-pi-process-proc proc-handle) proc)
          proc-handle)
      (error
       (signal 'beemacs-pi-error
               (list (format "failed to start pi (%s): %s"
                              beemacs-pi-executable (error-message-string err))))))))

(defun beemacs-pi-alive-p (proc-handle)
  "Return non-nil if PROC-HANDLE's underlying process is still running."
  (and (beemacs-pi-process-proc proc-handle)
       (process-live-p (beemacs-pi-process-proc proc-handle))))

(defun beemacs-pi-health-check (proc-handle)
  "Return non-nil if PROC-HANDLE is alive and its process status is `run'.

This is a cheap, synchronous liveness probe -- it does not round-trip a
request through the child, it only inspects the OS-level process state
via `process-status'.  A caller wanting to confirm the child is actually
RESPONSIVE (not merely alive) should send a request via `beemacs-pi-send'
and wait for a reply on ON-MESSAGE."
  (and (beemacs-pi-alive-p proc-handle)
       (eq (process-status (beemacs-pi-process-proc proc-handle)) 'run)))

(defun beemacs-pi-send (proc-handle request)
  "Send REQUEST (an elisp value JSON-encodable via `json-encode') to PROC-HANDLE.

Encodes REQUEST as one JSON line (newline-terminated, matching pi's RPC
framing) and writes it to the child's stdin via `process-send-string'.

Signals `beemacs-pi-error' if PROC-HANDLE's process is not alive."
  (unless (beemacs-pi-alive-p proc-handle)
    (signal 'beemacs-pi-error
            (list "cannot send to pi: process is not running")))
  (process-send-string
   (beemacs-pi-process-proc proc-handle)
   (concat (json-encode request) "\n")))

(defun beemacs-pi-stop (proc-handle &optional timeout)
  "Cleanly shut down PROC-HANDLE's `pi' child process.

Attempts a graceful exit first by closing the child's stdin
(`process-send-eof'), then waits up to TIMEOUT seconds (default 2) for
the process to exit on its own.  If it has not exited by then, sends
SIGTERM via `interrupt-process' and, failing that within another short
grace period, `kill-process' as a last resort.  Safe to call on an
already-dead PROC-HANDLE (a no-op in that case)."
  (let ((timeout (or timeout 2)))
    (when (beemacs-pi-alive-p proc-handle)
      (let ((proc (beemacs-pi-process-proc proc-handle)))
        (condition-case nil
            (process-send-eof proc)
          (error nil))
        (with-timeout (timeout nil)
          (while (process-live-p proc)
            (accept-process-output proc 0.1)))
        (when (process-live-p proc)
          (condition-case nil
              (interrupt-process proc)
            (error nil))
          (with-timeout (timeout nil)
            (while (process-live-p proc)
              (accept-process-output proc 0.1))))
        (when (process-live-p proc)
          (kill-process proc))
        (when (buffer-live-p (process-buffer proc))
          (kill-buffer (process-buffer proc)))))))

(defun beemacs-pi-run-oneshot (prompt &optional executable)
  "Run `pi' once in one-shot JSON print mode with PROMPT, return parsed JSON.

Invokes EXECUTABLE (or `beemacs-pi-executable') synchronously with
`beemacs-pi-oneshot-args' plus PROMPT appended, via `call-process', and
parses its stdout as a single JSON document -- pi's one-shot print mode,
used as the secondary transport when a long-lived RPC process is not
warranted (a single request/response exchange).  This never inspects or
scrapes pi's interactive TUI; it only shells out to the documented
non-interactive JSON-print invocation.

Signals `beemacs-pi-error' if the process exits non-zero, or if its
stdout is not valid JSON."
  (let ((executable (or executable beemacs-pi-executable)))
    (with-temp-buffer
      (let ((exit-code
             (condition-case err
                 (apply #'call-process executable nil t nil
                        (append beemacs-pi-oneshot-args (list prompt)))
               (error
                (signal 'beemacs-pi-error
                        (list (format "failed to run pi (%s): %s"
                                      executable (error-message-string err))))))))
        (unless (eq exit-code 0)
          (signal 'beemacs-pi-error
                  (list (format "pi exited with status %s: %s"
                                exit-code (string-trim (buffer-string))))))
        (beemacs-pi--parse-json (string-trim (buffer-string)))))))

(beemacs-pi--apply-persisted-executable)

(provide 'beemacs-pi)

;;; beemacs-pi.el ends here
