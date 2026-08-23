;;; beemacs-tests.el --- ERT test suite for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; ERT unit test suite for beemacs.  Run in batch mode with:
;;
;;   emacs -Q --batch -L . -l ert -l beemacs-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'beemacs)

(ert-deftest beemacs-test-version-defined ()
  "Smoke test: `beemacs-version' is defined and looks like a version string."
  (should (stringp beemacs-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" beemacs-version)))

(ert-deftest beemacs-test-transport-url-builder ()
  "The transport URL builder joins base and path without double slashes."
  (let ((beemacs-server-url "http://example.com:8080/"))
    (should (equal (beemacs-transport--url "/foo")
                   "http://example.com:8080/foo"))))

(ert-deftest beemacs-test-render-submodule-names ()
  "The render layer extracts submodule names from API-shaped data."
  (should (equal (beemacs-render-submodule-names
                  (vector '((name . "beemacs")) '((name . "beehive"))))
                 '("beemacs" "beehive"))))

(provide 'beemacs-tests)

;;; beemacs-tests.el ends here
