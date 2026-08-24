;;; beemacs-transient.el --- Top-level transient menu + shared keymap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; The single entry point that ties every landed beemacs capability
;; together, a la cavemacs:
;;
;;   - `beemacs-menu' -- a top-level `transient' prefix exposing every
;;     landed swarm view (dashboard/plan/docs/branches/skills/stats/
;;     secrets/human/dances/merge) AND the pi agent buffer/session
;;     selector, grouped for discoverability.
;;   - `beemacs-shared-mode' -- a minor mode, turned on automatically in
;;     every beemacs major mode, providing ONE consistent cross-buffer
;;     keymap: refresh / drill-in / act / stream / abort. Each verb is
;;     resolved per-buffer, by major mode, to the EXISTING command that
;;     already implements it (see `beemacs-shared-mode--dispatch-table')
;;     -- this file wires, it never reimplements.
;;
;; Every `beemacs-*' command remains independently reachable via `M-x'
;; (each already carries its own docstring); `beemacs-menu' and
;; `beemacs-shared-mode' are a discoverability layer on top, not a
;; replacement.
;;
;; `transient' ships with Emacs from 28 on and is required unconditionally
;; here; `beemacs-menu' is the only thing that needs it, so a build predating
;; 28 loses only this file's discoverability layer, not the rest of beemacs.

;;; Code:

(require 'transient)
(require 'beemacs-render)
(require 'beemacs-human)
(require 'beemacs-stats)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'beemacs-pi-model)
(require 'beemacs-session)

(defgroup beemacs-transient nil
  "Top-level transient menu and shared cross-buffer keymap for beemacs."
  :group 'beemacs
  :prefix "beemacs-")

;;; Shared keymap: refresh / drill-in / act / stream / abort

(defvar beemacs-shared-mode--dispatch-table
  '((beemacs-docs-mode
     :refresh beemacs-docs-refresh
     :drill   beemacs-docs-open-at-point)
    (beemacs-branches-mode
     :refresh beemacs-branches-refresh
     :drill   beemacs-branches-open-at-point)
    (beemacs-plan-mode
     :refresh beemacs-plan-refresh
     :drill   beemacs-plan-open-at-point)
    (beemacs-dashboard-mode
     :refresh beemacs-dashboard-refresh
     :drill   beemacs-dashboard-open-at-point)
    (beemacs-submodule-view-mode
     :refresh beemacs-submodule-view-refresh
     :drill   beemacs-submodule-view-plan
     :stream  beemacs-submodule-view-sessions
     :act     beemacs-submodule-view-roi)
    (beemacs-roi-edit-mode
     :act     beemacs-roi-edit-publish)
    (beemacs-secrets-view-mode
     :refresh beemacs-secrets-view-refresh
     :act     beemacs-secrets-view-set-key)
    (beemacs-skills-mode
     :refresh beemacs-skills-refresh
     :drill   beemacs-skills-open-at-point
     :act     beemacs-skills-apply-at-point)
    (beemacs-dance-plan-mode
     :refresh beemacs-dance-plan-refresh
     :act     beemacs-dance-plan-apply)
    (beemacs-session-view-mode
     :abort   kill-current-buffer)
    (beemacs-human-list-mode
     :refresh beemacs-human-list-refresh
     :drill   beemacs-human-list-open-at-point)
    (beemacs-human-resolve-mode
     :refresh beemacs-human-resolve-refresh
     :act     beemacs-human-resolve-apply
     :abort   beemacs-human-resolve-discard)
    (beemacs-stats-mode
     :refresh beemacs-stats-refresh
     :drill   beemacs-stats-open-at-point))
  "Alist of (MAJOR-MODE . PLIST) resolving each shared verb -- `:refresh',
`:drill', `:act', `:stream', `:abort' -- to the EXISTING command in that
major mode which already implements it. `beemacs-shared-mode' consults
this table; it never reimplements the underlying behaviour. A major mode
or verb absent here has no shared binding for that verb in that buffer.")

(defun beemacs-shared--dispatch (verb)
  "Resolve VERB (one of `:refresh' `:drill' `:act' `:stream' `:abort') for
the current buffer's major mode via `beemacs-shared-mode--dispatch-table'
and call the resolved command interactively. Signals `user-error' when
the current major mode has no entry, or its entry has no binding for
VERB -- a real \"not applicable in this buffer\", never a silent no-op."
  (let* ((entry (assq major-mode beemacs-shared-mode--dispatch-table))
         (fn (and entry (plist-get (cdr entry) verb))))
    (unless fn
      (user-error "beemacs: %s has no `%s' action" major-mode
                  (substring (symbol-name verb) 1)))
    (call-interactively fn)))

(defun beemacs-shared-refresh ()
  "Refresh the current beemacs buffer (dispatches by major mode)."
  (interactive)
  (beemacs-shared--dispatch :refresh))

(defun beemacs-shared-drill-in ()
  "Drill into the row/entry at point (dispatches by major mode)."
  (interactive)
  (beemacs-shared--dispatch :drill))

(defun beemacs-shared-act ()
  "Perform this buffer's primary write action (dispatches by major mode)."
  (interactive)
  (beemacs-shared--dispatch :act))

(defun beemacs-shared-stream ()
  "Open the live session stream related to this buffer (dispatches by
major mode)."
  (interactive)
  (beemacs-shared--dispatch :stream))

(defun beemacs-shared-abort ()
  "Abort/close the current beemacs buffer's live connection or process
(dispatches by major mode)."
  (interactive)
  (beemacs-shared--dispatch :abort))

(defvar beemacs-shared-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c g") #'beemacs-shared-refresh)
    (define-key map (kbd "C-c RET") #'beemacs-shared-drill-in)
    (define-key map (kbd "C-c a") #'beemacs-shared-act)
    (define-key map (kbd "C-c v") #'beemacs-shared-stream)
    (define-key map (kbd "C-c k") #'beemacs-shared-abort)
    (define-key map (kbd "C-c C-m") #'beemacs-menu)
    map)
  "Shared keymap installed by `beemacs-shared-mode' in every beemacs
buffer: `C-c g' refresh, `C-c RET' drill-in, `C-c a' act, `C-c v' stream,
`C-c k' abort, `C-c C-m' the top-level `beemacs-menu'. Bound entirely
under the `C-c' prefix so it layers over -- and never shadows -- each
mode's own single-letter bindings, including editable buffers like
`beemacs-roi-edit-mode' where a bare letter must still self-insert.")

;;;###autoload
(define-minor-mode beemacs-shared-mode
  "Minor mode providing the shared refresh/drill-in/act/stream/abort
keymap common to every beemacs buffer. Turned on automatically in every
beemacs major mode; see `beemacs-shared-mode-map' and
`beemacs-shared-mode--dispatch-table'."
  :lighter " Beemacs"
  :keymap beemacs-shared-mode-map)

(dolist (hook '(beemacs-docs-mode-hook
                beemacs-branches-mode-hook
                beemacs-plan-mode-hook
                beemacs-dashboard-mode-hook
                beemacs-submodule-view-mode-hook
                beemacs-roi-edit-mode-hook
                beemacs-secrets-view-mode-hook
                beemacs-skills-mode-hook
                beemacs-dance-plan-mode-hook
                beemacs-session-view-mode-hook
                beemacs-human-list-mode-hook
                beemacs-human-resolve-mode-hook
                beemacs-stats-mode-hook))
  (add-hook hook #'beemacs-shared-mode))

;;; Top-level transient menu

;;;###autoload (autoload 'beemacs-menu "beemacs-transient" nil t)
(transient-define-prefix beemacs-menu ()
  "Top-level beemacs menu: every landed swarm view and pi agent surface.

Wires existing `beemacs-*' commands only -- each suffix below just calls
the command that already implements that surface (`beemacs-dashboard',
`beemacs-pi-chat-open', etc.); see each command's own docstring for what
it does. Also reachable from any beemacs buffer via `C-c C-m'
(`beemacs-shared-mode-map')."
  ["Swarm views"
   ("d" "Dashboard (all submodules)" beemacs-dashboard)
   ("s" "Submodule hub (plan/roi/docs/branches/sessions/env/secrets)"
    beemacs-submodule-view)
   ("p" "Plan tasks" beemacs-plan-view)
   ("o" "Docs (change record)" beemacs-docs-view)
   ("b" "Branches / commit history" beemacs-branches-view)
   ("k" "Skills / dances registry" beemacs-skills-view)
   ("t" "Swarm stats" beemacs-stats-view)
   ("e" "Secrets (per-submodule/global)" beemacs-secrets-view)
   ("h" "NEEDS-HUMAN escalations" beemacs-human-list)]
  ["Pi agent"
   ("c" "Open/resume pi agent chat buffer" beemacs-pi-chat-open)
   ("S" "Pi sessions (mru/resume/continue/branch)" beemacs-pi-sessions-open)
   ("M" "Select default pi model" beemacs-pi-model-select)]
  ["Actions"
   ("D" "Plan a named dance" beemacs-dance-plan)
   ("A" "Apply a named dance" beemacs-dance-apply)
   ("m" "Merge branch into submodule's tracked branch" beemacs-merge)]
  ["Other"
   ("q" "Quit" transient-quit-one)])

(provide 'beemacs-transient)

;;; beemacs-transient.el ends here
