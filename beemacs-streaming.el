;;; beemacs-streaming.el --- Generic SSE client for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Generic Server-Sent-Events (SSE) client primitive built on top of
;; `beemacs-transport''s connection conventions (base endpoint, URL
;; building).  This module owns the streaming half of talking to
;; `beehived': `beemacs-sse-connect' opens a long-lived HTTP GET against a
;; `text/event-stream' endpoint via `url-retrieve' and installs a process
;; filter that incrementally parses `data:' frames out of the raw byte
;; stream as chunks arrive -- including a frame whose bytes are split
;; across two (or more) separate process-filter invocations, which is
;; reassembled correctly rather than dropped or corrupted.  Each complete
;; SSE event invokes the caller's callback with the event's decoded data
;; string.  `beemacs-sse-abort' tears the connection down (killing the
;; process and its buffer) with no leaked process left behind.
;;
;; The SSE wire format handled here (RFC 8895 "Server-Sent Events"):
;;   - Frames are separated by a blank line (bare "\n" after any
;;     already-consumed "\r").
;;   - Within a frame, one or more "data: <payload>" lines carry the
;;     event's payload; multiple `data:' lines in one frame are joined
;;     with "\n", matching the spec's multi-line-data behavior.
;;   - `event:', `id:', `retry:' lines and ":"-prefixed comment lines are
;;     recognized and skipped (this primitive surfaces only the data
;;     payload to CALLBACK; a future primitive can layer named-event
;;     dispatch on top if a caller ever needs it).
;;
;; The chunk-reassembly logic lives in `beemacs-sse--feed', a pure(ish)
;; function of "old pending text + new chunk" that returns the new
;; pending text and invokes CALLBACK once per complete frame -- this is
;; deliberately separated from the process/network plumbing so it can be
;; unit-tested directly with synthetic multi-chunk byte sequences (see
;; `beemacs-tests.el') without spinning up a real HTTP server.

;;; Code:

(require 'url)
(require 'cl-lib)
(require 'beemacs-transport)

(defgroup beemacs-streaming nil
  "Server-Sent-Events streaming client for beemacs."
  :group 'beemacs
  :prefix "beemacs-sse-")

(define-error 'beemacs-sse-error "beemacs SSE stream failed")

(cl-defstruct (beemacs-sse-connection
               (:constructor beemacs-sse-connection--create))
  "Handle for one open SSE connection.

PROC is the underlying Emacs process object driving the HTTP request
(the process behind the `url-retrieve' response buffer).  BUFFER is that
response buffer.  PENDING holds not-yet-dispatched partial SSE text (the
tail of the byte stream that has not yet formed a complete frame,
including a `data:' line split across chunk boundaries).
DATA-LINES accumulates the `data:' payload lines seen so far within the
CURRENT (not-yet-terminated) frame, so a frame's multiple `data:' lines
can be joined once the frame's terminating blank line arrives.
HEADERS-DONE-P is non-nil once the raw HTTP response header block has
been consumed and stripped from the byte stream, so only body bytes are
fed to the SSE frame parser.  CALLBACK is invoked with one decoded event
payload string per complete frame.  ABORTED-P guards against any further
callback invocation or double-cleanup once `beemacs-sse-abort' has run."
  proc
  buffer
  (pending "")
  (data-lines nil)
  (headers-done-p nil)
  callback
  (aborted-p nil))

(defun beemacs-sse--strip-headers (text)
  "Split TEXT into (HEADERS-DONE-P . REMAINDER).

If TEXT contains the blank line terminating an HTTP response's header
block (a bare newline following an optional carriage return, i.e. the
first \"\\n\\n\" or \"\\r\\n\\r\\n\"), HEADERS-DONE-P is non-nil and
REMAINDER is the body bytes following that blank line.  Otherwise
HEADERS-DONE-P is nil and REMAINDER is nil (TEXT is retained unmodified
by the caller as still-pending header bytes)."
  (let ((idx (or (string-match "\r\n\r\n" text)
                 (string-match "\n\n" text))))
    (if idx
        (let ((match-len (length (match-string 0 text))))
          (cons t (substring text (+ idx match-len))))
      (cons nil nil))))

(defun beemacs-sse--dispatch-line (line conn)
  "Interpret one complete SSE LINE (no trailing newline) against CONN.

A \"data:\" (or \"data\") line's payload is appended to CONN's pending
DATA-LINES.  A blank line terminates the current frame: if any data
lines were accumulated, they are joined with \"\\n\" and CONN's CALLBACK
is invoked with the joined string, then the accumulator is cleared.
`event:', `id:', `retry:' lines and \":\"-prefixed comment lines are
recognized and otherwise ignored by this generic primitive."
  (let ((line (string-remove-suffix "\r" line)))
    (cond
     ((string-empty-p line)
      (when (beemacs-sse-connection-data-lines conn)
        (let ((payload (mapconcat #'identity
                                   (nreverse (beemacs-sse-connection-data-lines conn))
                                   "\n")))
          (setf (beemacs-sse-connection-data-lines conn) nil)
          (unless (beemacs-sse-connection-aborted-p conn)
            (funcall (beemacs-sse-connection-callback conn) payload)))))
     ((or (string-prefix-p "data:" line) (string-equal line "data"))
      (let ((payload (if (string-prefix-p "data: " line)
                          (substring line 6)
                        (substring line (length "data:")))))
        (push payload (beemacs-sse-connection-data-lines conn))))
     ;; event:, id:, retry:, and ":"-prefixed comment lines: recognized,
     ;; not surfaced by this generic data-only primitive.
     (t nil))))

(defun beemacs-sse--feed (conn chunk)
  "Feed newly-arrived CHUNK bytes into CONN, dispatching complete frames.

Handles a `data:' frame (or any other SSE line) whose bytes are split
across CHUNK and a previous call's leftover CONN pending text: the two
are concatenated before re-splitting on newlines, so a split is always
reassembled correctly regardless of where the boundary fell.  Any
trailing incomplete line (no terminating newline yet) is kept in CONN's
PENDING for the next call."
  (let ((text (concat (beemacs-sse-connection-pending conn) chunk)))
    (unless (beemacs-sse-connection-headers-done-p conn)
      (let ((split (beemacs-sse--strip-headers text)))
        (if (car split)
            (progn
              (setf (beemacs-sse-connection-headers-done-p conn) t)
              (setq text (cdr split)))
          (setf (beemacs-sse-connection-pending conn) text)
          (setq text nil))))
    (when text
      ;; Split into raw lines; the LAST element is either "" (TEXT ended
      ;; exactly on a newline, i.e. nothing left pending) or the
      ;; not-yet-newline-terminated tail to carry over to the next call
      ;; (which is how a `data:' line split mid-payload across chunk
      ;; boundaries gets reassembled). Only the lines before it are
      ;; complete and safe to dispatch now.
      (let ((lines (split-string text "\n" nil)))
        (dolist (line (butlast lines))
          (beemacs-sse--dispatch-line line conn))
        (setf (beemacs-sse-connection-pending conn) (or (car (last lines)) ""))))))

(defun beemacs-sse--filter (conn)
  "Return a process filter function closing over CONN.

Feeds every chunk written by the process to `beemacs-sse--feed'; a
no-op once CONN has been aborted (so bytes arriving after abort, if any
manage to race the process teardown, are silently dropped rather than
firing a callback on a torn-down connection)."
  (lambda (_proc chunk)
    (unless (beemacs-sse-connection-aborted-p conn)
      (beemacs-sse--feed conn chunk))))

(defun beemacs-sse-connect (path callback &optional endpoint)
  "Open an SSE stream at PATH against ENDPOINT (or `beemacs-endpoint').

Issues an HTTP GET via `url-retrieve' and installs a process filter (see
`beemacs-sse--filter') on the resulting process that incrementally
parses `text/event-stream' frames out of the raw response bytes as they
arrive, invoking CALLBACK with each event's decoded data-payload string
as soon as its terminating blank line is seen -- CALLBACK may be invoked
many times over the life of the connection, including firing again after
a frame that had to be reassembled across two or more chunk boundaries.

Returns a `beemacs-sse-connection' handle; pass it to
`beemacs-sse-abort' to stop the stream.  Signals `beemacs-sse-error' if
the underlying request could not even be started."
  (let* ((url (beemacs-transport--url path endpoint))
         (conn (beemacs-sse-connection--create :callback callback)))
    (condition-case err
        (let ((buffer (url-retrieve
                       url
                       (lambda (_status)
                         ;; The connection closed (server EOF, or an
                         ;; error `url-retrieve' reports via STATUS);
                         ;; nothing further to feed once this runs.
                         (setf (beemacs-sse-connection-aborted-p conn) t))
                       nil t t)))
          (unless buffer
            (signal 'beemacs-sse-error
                    (list (format "failed to open SSE stream for %s" url))))
          (setf (beemacs-sse-connection-buffer conn) buffer)
          (let ((proc (get-buffer-process buffer)))
            (setf (beemacs-sse-connection-proc conn) proc)
            (when proc
              (set-process-filter proc (beemacs-sse--filter conn))))
          conn)
      (beemacs-sse-error (signal (car err) (cdr err)))
      (error
       (signal 'beemacs-sse-error
               (list (format "failed to open SSE stream for %s: %s"
                             url (error-message-string err))))))))

(defun beemacs-sse-abort (conn)
  "Stop the SSE stream held by CONN, leaking no process or buffer.

Marks CONN aborted (so any in-flight filter invocation becomes a no-op),
kills the underlying process if still live, and kills its buffer if
still live.  Safe to call more than once, and safe to call on a CONN
whose process already exited on its own."
  (setf (beemacs-sse-connection-aborted-p conn) t)
  (let ((proc (beemacs-sse-connection-proc conn)))
    (when (and proc (process-live-p proc))
      (delete-process proc)))
  (let ((buffer (beemacs-sse-connection-buffer conn)))
    (when (buffer-live-p buffer)
      (kill-buffer buffer))))

(provide 'beemacs-streaming)

;;; beemacs-streaming.el ends here
