;;; beemacs-session.el --- Live honeybee session transcript buffer -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; `beemacs-session-view' opens a buffer that renders a honeybee session's
;; transcript, live-streaming it while the session is still running --
;; beemacs's analogue of cavemacs's streaming-chat-buffer feature. It talks to
;; `beehived''s existing SSE endpoint (`GET
;; /submodule/{name}/session/{branch}/stream', already implemented server-side
;; per the `beehive:beemacs-json-api' task's Commentary -- no polling
;; stand-in is needed) via `beemacs-sse-connect' (`beemacs-streaming.el').
;;
;; The server re-renders the WHOLE transcript through its shared
;; "transcript_pane" template on every frame (never a token-level delta), so
;; each SSE frame this buffer receives is the complete, current, sanitized
;; HTML for the transcript so far; a RECORDED (already-finished) session's
;; first (and only) frame IS the whole transcript, followed immediately by the
;; server's "end" event -- so the very same code path here renders a live
;; session incrementally and a recorded session in one shot, with no special
;; casing required.  Frames are converted from HTML to a readable Emacs buffer
;; via `shr-insert-document' (`shr', built into Emacs) rather than displaying
;; raw markup.
;;
;; The server signals three kinds of SSE frames on this endpoint
;; (`internal/web/sessions.go' `sessionStream'/`writeSSEEvent'/`writeSSEData'):
;;   - an unnamed ("message") event whose data is the rendered transcript pane
;;     HTML -- always non-empty (the template unconditionally wraps its output
;;     in `<div id="session-pane">...</div>');
;;   - a named "sync" event carrying a JSON staleness-banner payload, sent only
;;     when following an off-box remote;
;;   - a named "end" event with an always-EMPTY data payload, sent exactly once
;;     right before the server closes the stream (session finished or a read
;;     error occurred).
;; `beemacs-streaming.el's `beemacs-sse-connect' is a deliberately generic,
;; event-name-agnostic primitive (it surfaces only the decoded `data:' payload,
;; treating `event:' as an ignored line -- see its Commentary) rather than a
;; session-transcript-specific one, so this module distinguishes the three
;; cases purely from the payload shape it receives, without needing to extend
;; that lower-level primitive: an EMPTY payload is the "end" frame (the
;; transcript template body is never itself empty, so this is unambiguous); a
;; payload beginning with "{" is the JSON "sync" banner (recorded, but not yet
;; surfaced in the buffer -- a future refinement could show it as a header
;; line); anything else is a transcript-pane HTML frame to render.
;;
;; Auto-scroll behavior: before each frame is rendered, whether point sits
;; exactly at `point-max' is recorded; if so, point is moved to the new
;; `point-max' after the frame renders (standard tail -f auto-scroll).
;; Otherwise point is left at its exact prior character offset -- since each
;; frame only ever APPENDS new content to the transcript (never rewrites
;; earlier text), a reader who has moved point back to an earlier part of the
;; transcript keeps reading the exact same spot undisturbed by later frames,
;; exactly the "auto-scrolling unless point moved back" behavior cavemacs's
;; streaming buffer has.
;;
;; Killing a `beemacs-session-view-mode' buffer always cleanly aborts its SSE
;; connection via `beemacs-sse-abort' (a `kill-buffer-hook'), so a stray HTTP
;; connection never outlives its buffer.

;;; Code:

(require 'cl-lib)
(require 'shr)
(require 'beemacs-streaming)

(defgroup beemacs-session nil
  "Live honeybee session transcript buffer for beemacs."
  :group 'beemacs
  :prefix "beemacs-session-")

(defvar-local beemacs-session-view--conn nil
  "This buffer's `beemacs-sse-connection' handle, or nil once the stream ended.")

(defvar-local beemacs-session-view--name nil
  "The submodule name this session-view buffer was opened for.")

(defvar-local beemacs-session-view--branch nil
  "The session branch this session-view buffer was opened for.")

(defun beemacs-session-view--buffer-name (name branch)
  "Return the session-view buffer name for submodule NAME's session BRANCH."
  (format "*beemacs-session: %s/%s*" name branch))

(defun beemacs-session-view--render-html (html)
  "Render HTML (a string) to a propertized string via `shr-insert-document'."
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max)))
          ;; A wide fixed character width so `shr' fills to a generous line
          ;; length rather than whatever narrow width happens to be active.
          (shr-width most-positive-fixnum)
          ;; Pixel-based fill measurement (`shr-use-fonts' non-nil, the
          ;; default) depends on real font metrics that a headless/batch
          ;; Emacs (as ERT runs in) does not have, which otherwise makes shr
          ;; wrap after nearly every word; character-based fill is correct
          ;; and portable in both an interactive frame and batch mode.
          (shr-use-fonts nil))
      (erase-buffer)
      (shr-insert-document dom))
    (buffer-string)))

(defun beemacs-session-view--replace-content (buffer text)
  "Replace BUFFER's contents with TEXT, preserving the reading position.

If point sat exactly at `point-max' before the replacement (the common
\"following the live end\" case), it is left at the new `point-max' too, so
the buffer keeps auto-scrolling as new frames arrive. Otherwise point is
left at the SAME character offset it had before -- since each frame is the
full transcript re-rendered with new content only ever appended at the end
(never inserted earlier), a reader who moved point back to an earlier part
of the transcript keeps reading the exact same spot, undisturbed by later
frames, instead of being yanked back to the bottom."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (let* ((inhibit-read-only t)
             (was-at-end (= (point) (point-max)))
             (old-point (point)))
        (erase-buffer)
        (insert text)
        (goto-char (if was-at-end (point-max) (min old-point (point-max))))
        (dolist (window (get-buffer-window-list buffer nil t))
          (set-window-point window (point)))))))

(defun beemacs-session-view--stop (buffer)
  "Mark BUFFER's stream as ended: abort the connection and clear the handle."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when beemacs-session-view--conn
        (beemacs-sse-abort beemacs-session-view--conn)
        (setq beemacs-session-view--conn nil)))))

(defun beemacs-session-view--handle-frame (buffer payload)
  "Dispatch one decoded SSE PAYLOAD string for BUFFER's session transcript.

See this file's Commentary for how the three server frame shapes (transcript
HTML, the \"sync\" JSON banner, and the empty \"end\" marker) are told apart."
  (when (buffer-live-p buffer)
    (cond
     ((string-empty-p payload)
      ;; The server's "end" frame: the stream is finished (session ended, or
      ;; a transcript read error the caller already logged server-side).
      (beemacs-session-view--stop buffer))
     ((string-prefix-p "{" payload)
      ;; The "sync" staleness-banner JSON; not yet surfaced in the buffer.
      nil)
     (t
      (beemacs-session-view--replace-content
       buffer (beemacs-session-view--render-html payload))))))

(defun beemacs-session-view--stop-on-kill ()
  "Abort the current buffer's SSE connection, if any (`kill-buffer-hook')."
  (when beemacs-session-view--conn
    (beemacs-sse-abort beemacs-session-view--conn)
    (setq beemacs-session-view--conn nil)))

;;;###autoload
(defun beemacs-session-view (name branch)
  "Open a buffer streaming submodule NAME's session BRANCH transcript live.

Connects to `GET /submodule/NAME/session/BRANCH/stream' via
`beemacs-sse-connect' and renders every frame through `shr' as it arrives
(see this file's Commentary). A recorded (already-finished) session renders
its full transcript in the same single code path, since the server sends the
complete transcript as its first frame followed immediately by its \"end\"
frame. Killing the buffer always cleanly tears the SSE connection down."
  (interactive "sSubmodule name: \nsSession branch: ")
  (let ((buf (get-buffer-create (beemacs-session-view--buffer-name name branch))))
    (with-current-buffer buf
      (beemacs-session-view-mode)
      (setq beemacs-session-view--name name)
      (setq beemacs-session-view--branch branch)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Connecting to session %s/%s...\n" name branch)))
      (let ((this-buf buf))
        (setq beemacs-session-view--conn
              (beemacs-sse-connect
               (format "/submodule/%s/session/%s/stream" name branch)
               (lambda (payload)
                 (beemacs-session-view--handle-frame this-buf payload))))))
    (pop-to-buffer buf)
    buf))

(defvar beemacs-session-view-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map "q" #'kill-current-buffer)
    map)
  "Keymap for `beemacs-session-view-mode'.")

(define-derived-mode beemacs-session-view-mode special-mode "Beemacs-Session"
  "Major mode for a live-streaming honeybee session transcript buffer.

Renders the session transcript's shared \"transcript_pane\" HTML via `shr',
auto-scrolling unless point has been moved back (see
`beemacs-session-view--replace-content'). `q' kills the buffer, which always
cleanly aborts the underlying SSE connection first (see
`beemacs-session-view--stop-on-kill' on `kill-buffer-hook').
\\{beemacs-session-view-mode-map}"
  (setq buffer-read-only t)
  (add-hook 'kill-buffer-hook #'beemacs-session-view--stop-on-kill nil t))

(provide 'beemacs-session)

;;; beemacs-session.el ends here
