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
;; `transient' ships with Emacs from 28 on but this package declares
;; `Package-Requires: ((emacs "27.1"))', and even on 28+ a mismatched/older
;; `transient' release can be missing `transient-define-prefix' (the macro
;; this file's menu needs). So `transient' is loaded WITH a soft failure
;; (`require' ... NOERROR) and every entry point that depends on it degrades
;; gracefully instead of erroring at load time: `beemacs-menu' becomes a
;; `user-error'-signalling stub (still bound, still autoloadable, so `M-x
;; beemacs-menu' and `C-c C-m' report a clear "transient unavailable"
;; instead of a raw void-function/unbound-variable at load time), and
;; `beemacs-shared-mode-map's `C-c C-m' binding is simply omitted. Every
;; other landed beemacs command remains fully usable regardless -- this
;; file's menu is a discoverability layer on top, never a hard dependency.

;;; Code:

(require 'transient nil t)
(require 'beemacs-render)
(require 'beemacs-human)
(require 'beemacs-stats)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'beemacs-pi-model)
(require 'beemacs-session)

(declare-function beemacs-menu "beemacs-transient")

(defgroup beemacs-transient nil
  "Top-level transient menu and shared cross-buffer keymap for beemacs."
  :group 'beemacs
  :prefix "beemacs-")

(defconst beemacs-transient--available-p
  (and (featurep 'transient) (fboundp 'transient-define-prefix))
  "Non-nil when a `transient' new enough to define `beemacs-menu' is loaded.

Nil either when `transient' failed to load at all, or when it loaded but
is too old/mismatched to provide `transient-define-prefix' -- both cases
`beemacs-menu' degrades to a `user-error' stub for (see below) instead of
erroring at `require' time.")

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
(if beemacs-transient--available-p
    (transient-define-prefix beemacs-menu ()
      "Top-level beemacs menu: every landed swarm view and pi agent surface.

Wires existing `beemacs-*' commands only -- each suffix below just calls
the command that already implements that surface (`beemacs-dashboard',
`beemacs', etc.); see each command's own docstring for what
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
       ("c" "Start pi agent session at repo root" beemacs)
       ("C" "Open/resume a NAMED pi agent chat buffer" beemacs-pi-chat-open)
       ("S" "Pi sessions (mru/resume/continue/branch)" beemacs-sessions)
       ("M" "Select default pi model" beemacs-pi-model-select)]
      ["Actions"
       ("D" "Plan a named dance" beemacs-dance-plan)
       ("A" "Apply a named dance" beemacs-dance-apply)
       ("m" "Merge branch into submodule's tracked branch" beemacs-merge)]
      ["Other"
       ("q" "Quit" transient-quit-one)])
  (defun beemacs-menu ()
    "Stub standing in for the top-level transient menu.

`transient' is unavailable or too old/mismatched to provide
`transient-define-prefix' in this Emacs -- see
`beemacs-transient--available-p'. Every other `beemacs-*' command remains
independently reachable via `M-x'; only this discoverability menu is
disabled. Signals `user-error' instead of erroring at load time."
    (interactive)
    (user-error
     "beemacs: `transient' unavailable/too old for `beemacs-menu' (every other beemacs command still works via M-x)")))

(provide 'beemacs-transient)

;;; beemacs-transient.el ends here
