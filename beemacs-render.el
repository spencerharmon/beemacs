;;; beemacs-render.el --- Buffer rendering for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Buffer/display rendering logic for beemacs.  This module takes the typed
;; data structures produced by `beemacs-api.el' and renders them into Emacs
;; buffers (lists, detail views, streaming session transcripts, etc.).  It
;; owns no HTTP or JSON parsing.

;;; Code:

(require 'cl-lib)

(defgroup beemacs-render nil
  "Buffer rendering for beemacs."
  :group 'beemacs
  :prefix "beemacs-render-")

(defun beemacs-render-submodule-names (submodules)
  "Return a list of submodule name strings from SUBMODULES (a vector of alists).

Each element of SUBMODULES is expected to be an alist with a `name' key."
  (mapcar (lambda (sm) (alist-get 'name sm)) (append submodules nil)))

(defun beemacs-render-plan-item-claim (item)
  "Return the unified claim-state string for ITEM.

ITEM is one task alist from a `beemacs-api-plan' payload's `Items'
vector (internal/web.PlanItem, JSON-decoded). Mirrors
`internal/web.PlanItem.Claim': \"active <session>\" when the claim is
fresh (`Active'), \"stale <session>\" when it is past the TTL
(`Stale'), or \"\" when the task is unclaimed. `Active'/`Stale' decode as
the symbol `t' or `:json-false' via `json-read-from-string'."
  (cond
   ((eq (alist-get 'Active item) t)
    (format "active %s" (or (alist-get 'Session item) "")))
   ((eq (alist-get 'Stale item) t)
    (format "stale %s" (or (alist-get 'Session item) "")))
   (t "")))

(defun beemacs-render-plan-item-deps (item)
  "Return a comma-joined dependency-id string for ITEM.

ITEM is one task alist from a `beemacs-api-plan' payload's `Items'
vector; `Deps' is a JSON array of dependency-id strings (a bare local id
or a `sm:taskid' cross-submodule reference), decoded as a vector."
  (mapconcat #'identity (append (alist-get 'Deps item) nil) ","))

(defun beemacs-render-plan-rows (plan-data)
  "Project PLAN-DATA into `tabulated-list-entries'-shaped rows.

PLAN-DATA is the parsed JSON body of GET /submodule/{name}/plan.json (see
`beemacs-api-plan'): an alist with a `plan' key whose `Items' is a vector
of task alists. Returns a list of `(ID [ID STATUS WEIGHT DEPS CLAIM])'
entries -- ID doubles as the tabulated-list row key, letting
`beemacs-render-plan-find-item' look the full task alist back up from a
row without re-fetching."
  (let* ((plan (alist-get 'plan plan-data))
         (items (append (alist-get 'Items plan) nil)))
    (mapcar
     (lambda (item)
       (let ((id (alist-get 'ID item)))
         (list id
               (vector (or id "")
                       (or (alist-get 'Status item) "")
                       (number-to-string (or (alist-get 'Weight item) 0))
                       (beemacs-render-plan-item-deps item)
                       (beemacs-render-plan-item-claim item)))))
     items)))

(defun beemacs-render-plan-find-item (plan-data id)
  "Return the task alist for ID within PLAN-DATA, or nil if absent.

PLAN-DATA is a `beemacs-api-plan' payload; ID is a task id string as
carried by a `tabulated-list-entries' row key from
`beemacs-render-plan-rows'. Used to resolve a row back to its full task
alist (DocHref/SessionHref etc.) for the RET-to-open affordance without
re-fetching from beehived."
  (let* ((plan (alist-get 'plan plan-data))
         (items (append (alist-get 'Items plan) nil)))
    (cl-find id items :key (lambda (item) (alist-get 'ID item)) :test #'equal)))

(provide 'beemacs-render)

;;; beemacs-render.el ends here
