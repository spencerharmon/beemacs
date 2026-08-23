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

(defun beemacs-render-doc-rows (docs)
  "Return `tabulated-list-entries'-shaped rows from DOCS.

DOCS is a vector of alists as returned under the `docs' key of
`beemacs-api-docs', each carrying `Path'/`Name'/`Dir'/`Href' keys (the
capitalized Go field names of `DocEntry', which has no json tags). Each
row is `(PATH [NAME DIR PATH])', keyed by the docs/-relative PATH (unique
per submodule), with NAME and DIR (\"\" for a top-level file) as the
display columns and PATH itself carried through as the third column so a
caller can resolve the row back to its doc without a second lookup."
  (mapcar (lambda (d)
            (let ((path (alist-get 'Path d)))
              (list path (vector (or (alist-get 'Name d) "")
                                  (or (alist-get 'Dir d) "")
                                  (or path "")))))
          (append docs nil)))

(defun beemacs-render-branch-rows (commits)
  "Return `tabulated-list-entries'-shaped rows from COMMITS.

COMMITS is a vector of alists as returned under the `commits' key of
`beemacs-api-branches', each carrying the capitalized `Commit' struct
fields (`SHA', `Subject', `Author', `Date', `DocTask', ...). Each row is
`(SHA [SHA AUTHOR DATE SUBJECT DOCTASK])', keyed by the (short) commit
SHA; DOCTASK is the linked change-doc task id, or \"\" when the commit
carries no `Beehive:' stamp."
  (mapcar (lambda (c)
            (let ((sha (alist-get 'SHA c)))
              (list sha (vector (or sha "")
                                 (or (alist-get 'Author c) "")
                                 (or (alist-get 'Date c) "")
                                 (or (alist-get 'Subject c) "")
                                 (or (alist-get 'DocTask c) "")))))
          (append commits nil)))

(defun beemacs-render-diff-lines (before-text after-text)
  "Return a per-line diff between BEFORE-TEXT and AFTER-TEXT.

Splits both strings on newlines and computes a classic dynamic-programming
longest-common-subsequence alignment (no external `diff' program
involved, so this is hermetic and needs nothing beyond Emacs itself).
Returns a list of `(TAG . LINE)' conses in BEFORE-TEXT/AFTER-TEXT order,
TAG one of the symbols `same', `del' (present only in BEFORE-TEXT), or
`add' (present only in AFTER-TEXT)."
  (let* ((a (vconcat (split-string before-text "\n")))
         (b (vconcat (split-string after-text "\n")))
         (m (length a))
         (n (length b))
         (dp (make-vector (1+ m) nil)))
    (dotimes (i (1+ m)) (aset dp i (make-vector (1+ n) 0)))
    (cl-loop for i from (1- m) downto 0 do
             (cl-loop for j from (1- n) downto 0 do
                      (aset (aref dp i) j
                            (if (equal (aref a i) (aref b j))
                                (1+ (aref (aref dp (1+ i)) (1+ j)))
                              (max (aref (aref dp (1+ i)) j)
                                   (aref (aref dp i) (1+ j)))))))
    (let ((i 0) (j 0) result)
      (while (and (< i m) (< j n))
        (cond
         ((equal (aref a i) (aref b j))
          (push (cons 'same (aref a i)) result)
          (setq i (1+ i) j (1+ j)))
         ((>= (aref (aref dp (1+ i)) j) (aref (aref dp i) (1+ j)))
          (push (cons 'del (aref a i)) result)
          (setq i (1+ i)))
         (t
          (push (cons 'add (aref b j)) result)
          (setq j (1+ j)))))
      (while (< i m)
        (push (cons 'del (aref a i)) result)
        (setq i (1+ i)))
      (while (< j n)
        (push (cons 'add (aref b j)) result)
        (setq j (1+ j)))
      (nreverse result))))

(defun beemacs-render-unified-diff (before-text after-text &optional path)
  "Render a `diff-mode'-displayable unified diff of BEFORE-TEXT to AFTER-TEXT.

PATH (default \"PLAN.md\") labels the a/ and b/ file headers. Built purely
from `beemacs-render-diff-lines' -- no external `diff' program is
invoked -- as a single hunk spanning the whole file, which is sufficient
for `diff-mode' to fontify added/removed/context lines (the hunk header
line counts are irrelevant to fontification and are computed here only to
produce a well-formed `@@ -a,m +b,n @@' header)."
  (let* ((path (or path "PLAN.md"))
         (lines (beemacs-render-diff-lines before-text after-text))
         (a-count (cl-count-if (lambda (l) (memq (car l) '(same del))) lines))
         (b-count (cl-count-if (lambda (l) (memq (car l) '(same add))) lines)))
    (concat (format "--- a/%s\n" path)
            (format "+++ b/%s\n" path)
            (format "@@ -1,%d +1,%d @@\n" a-count b-count)
            (mapconcat
             (lambda (l)
               (concat (pcase (car l) ('same " ") ('del "-") ('add "+"))
                       (cdr l)))
             lines "\n"))))

(provide 'beemacs-render)

;;; beemacs-render.el ends here
