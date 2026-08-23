;;; beemacs-stats.el --- Swarm stats view for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; `beemacs-stats' buffer rendering swarm stats + Prometheus-backed metrics,
;; the same aggregate honeybee-performance numbers (per-submodule
;; throughput, delivered/honeybee yield, active-now, stranded branches,
;; per-model breakdown) the web UI's `/stats' view shows.  Sourced entirely
;; from `beemacs-api-stats' (the JSON contract's `GET /stats.json'), never a
;; direct Prometheus query or scraped HTML.

;;; Code:

(require 'cl-lib)
(require 'tabulated-list)
(require 'beemacs-api)
(require 'beemacs-render)

(defgroup beemacs-stats nil
  "Swarm stats view for beemacs."
  :group 'beemacs
  :prefix "beemacs-stats-")

(define-derived-mode beemacs-stats-mode tabulated-list-mode "Beemacs-Stats"
  "Major mode listing hive-wide honeybee-performance stats.

Mirrors the beehived web UI's `/stats' page (`GET /stats.json').
\\{beemacs-stats-mode-map}"
  (setq tabulated-list-format
        [("Name" 16 t) ("Delivered" 10 t) ("Honeybees" 10 t)
         ("Active" 8 t) ("Stranded" 9 t) ("Yield" 8 t)])
  (setq tabulated-list-sort-key (cons "Name" nil))
  (tabulated-list-init-header))

(defvar-local beemacs-stats--total nil
  "The `total' alist from the most recent `beemacs-api-stats' fetch.")

(defun beemacs-stats-refresh ()
  "Refetch and redisplay the current `beemacs-stats-mode' buffer's rows."
  (interactive)
  (unless (derived-mode-p 'beemacs-stats-mode)
    (user-error "Not in a beemacs-stats-mode buffer"))
  (let ((data (beemacs-api-stats)))
    (setq beemacs-stats--total (alist-get 'total data))
    (setq tabulated-list-entries
          (beemacs-render-stat-rows (alist-get 'subs data)))
    (tabulated-list-print t)))

(defun beemacs-stats-open-at-point ()
  "Open the per-model breakdown for the submodule at point.

The `stats.json' payload's `subStat' entries carry a `Models' vector
(per-agent-model delivered/honeybee/yield figures) with no separate
detail endpoint -- the row at point already carries everything needed,
so this renders it into a read-only buffer without a second request."
  (interactive)
  (unless (derived-mode-p 'beemacs-stats-mode)
    (user-error "Not in a beemacs-stats-mode buffer"))
  (let ((id (tabulated-list-get-id)))
    (unless id
      (user-error "No submodule at point"))
    (let* ((data (beemacs-api-stats))
           (subs (append (alist-get 'subs data) nil))
           (entry (cl-find-if (lambda (s) (equal (alist-get 'Name s) id)) subs))
           (buf (get-buffer-create (format "*beemacs-stats: %s*" id))))
      (unless entry
        (user-error "No stats entry for %s" id))
      (with-current-buffer buf
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Name: %s\n" (or (alist-get 'Name entry) "")))
          (insert (format "Delivered: %s\n" (or (alist-get 'DeliveredTasks entry) 0)))
          (insert (format "Honeybees: %s\n" (or (alist-get 'Honeybees entry) 0)))
          (insert (format "Active now: %s\n" (or (alist-get 'ActiveNow entry) 0)))
          (insert (format "Stranded: %s\n" (or (alist-get 'Stranded entry) 0)))
          (insert (format "Yield: %.1f%%\n\n"
                          (or (alist-get 'DeliveredPerBeePct entry) 0)))
          (insert "Models:\n")
          (let ((models (append (alist-get 'Models entry) nil)))
            (if (null models)
                (insert "  (none)\n")
              (dolist (m models)
                (insert (format "  %-30s delivered=%-4s honeybees=%-4s yield=%.1f%%\n"
                                (or (alist-get 'Model m) "")
                                (or (alist-get 'DeliveredTasks m) 0)
                                (or (alist-get 'Honeybees m) 0)
                                (or (alist-get 'DeliveredPerBeePct m) 0))))))
          (goto-char (point-min)))
        (view-mode 1)
        (setq buffer-read-only t))
      (pop-to-buffer buf))))

(defvar beemacs-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map tabulated-list-mode-map)
    (define-key map "g" #'beemacs-stats-refresh)
    (define-key map (kbd "RET") #'beemacs-stats-open-at-point)
    map)
  "Keymap for `beemacs-stats-mode'.")

;;;###autoload
(defun beemacs-stats-view ()
  "Browse hive-wide honeybee-performance stats.

Unlike the submodule-scoped views, this is a single hive-wide listing --
`GET /stats.json' takes no submodule name."
  (interactive)
  (let* ((data (beemacs-api-stats))
         (buf (get-buffer-create "*beemacs-stats*")))
    (with-current-buffer buf
      (beemacs-stats-mode)
      (setq beemacs-stats--total (alist-get 'total data))
      (setq tabulated-list-entries
            (beemacs-render-stat-rows (alist-get 'subs data)))
      (tabulated-list-print t))
    (pop-to-buffer buf)))

(provide 'beemacs-stats)

;;; beemacs-stats.el ends here
