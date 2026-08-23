;;; beemacs-render.el --- Buffer rendering for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Buffer/display rendering logic for beemacs.  This module takes the typed
;; data structures produced by `beemacs-api.el' and renders them into Emacs
;; buffers (lists, detail views, streaming session transcripts, etc.).  It
;; owns no HTTP or JSON parsing.

;;; Code:

(defgroup beemacs-render nil
  "Buffer rendering for beemacs."
  :group 'beemacs
  :prefix "beemacs-render-")

(defun beemacs-render-submodule-names (submodules)
  "Return a list of submodule name strings from SUBMODULES (a vector of alists).

Each element of SUBMODULES is expected to be an alist with a `name' key."
  (mapcar (lambda (sm) (alist-get 'name sm)) (append submodules nil)))

(provide 'beemacs-render)

;;; beemacs-render.el ends here
