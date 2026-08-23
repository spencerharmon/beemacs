;;; beemacs-tests.el --- ERT test suite for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; ERT unit test suite for beemacs.  Run in batch mode with:
;;
;;   emacs -Q --batch -L . -l ert -l beemacs-tests.el \
;;     -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'beemacs)
(require 'beemacs-api)

(ert-deftest beemacs-test-version-defined ()
  "Smoke test: `beemacs-version' is defined and looks like a version string."
  (should (stringp beemacs-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" beemacs-version)))

(ert-deftest beemacs-test-transport-url-builder ()
  "The transport URL builder joins base and path without double slashes."
  (let ((beemacs-endpoint "http://example.com:8080/"))
    (should (equal (beemacs-transport--url "/foo")
                   "http://example.com:8080/foo"))))

(ert-deftest beemacs-test-transport-url-builder-endpoint-override ()
  "A per-call ENDPOINT argument overrides `beemacs-endpoint'."
  (let ((beemacs-endpoint "http://default.example:8080"))
    (should (equal (beemacs-transport--url "foo" "http://override.example:9090/")
                   "http://override.example:9090/foo"))))

(defun beemacs-test--mock-call (status body &optional headers)
  "Return a mock replacement for `beemacs-transport--call'.

The mock ignores its URL argument and always returns `(STATUS HEADERS BODY)'."
  (lambda (_url) (list status (or headers '(("content-type" . "application/json"))) body)))

(ert-deftest beemacs-test-request-success ()
  "`beemacs-request' returns the structured (status headers body) triple on 2xx."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"ok\":true}")))
    (let ((result (beemacs-request "/submodules")))
      (should (equal (nth 0 result) 200))
      (should (equal (nth 2 result) "{\"ok\":true}")))))

(ert-deftest beemacs-test-request-non-2xx-signals ()
  "`beemacs-request' signals `beemacs-http-error' on a non-2xx response."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 404 "not found")))
    (should-error (beemacs-request "/missing") :type 'beemacs-http-error)))

(ert-deftest beemacs-test-request-connection-failure-signals ()
  "`beemacs-request' signals `beemacs-http-error' when the transport call fails."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (should-error (beemacs-request "/submodules") :type 'beemacs-http-error)))

(ert-deftest beemacs-test-request-endpoint-override ()
  "`beemacs-request' passes a per-call ENDPOINT through to the built URL."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url) (list 200 nil "ok"))))
      (beemacs-request "/foo" "http://override.example:9090")
      (should (equal seen-url "http://override.example:9090/foo")))))

(ert-deftest beemacs-test-transport-get-returns-body-on-success ()
  "`beemacs-transport-get' returns only the body string on success."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "hello")))
    (should (equal (beemacs-transport-get "/foo") "hello"))))

(ert-deftest beemacs-test-transport-get-non-2xx-signals ()
  "`beemacs-transport-get' signals `beemacs-http-error' on a non-2xx response."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 500 "boom")))
    (should-error (beemacs-transport-get "/foo") :type 'beemacs-http-error)))

(ert-deftest beemacs-test-json-request-well-formed ()
  "`beemacs-api-json-request' parses a well-formed JSON 2xx body."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"abc\",\"file\":\"foo.el\",\"busy\":false}")))
    (let ((result (beemacs-api-json-request "/api/editor/abc")))
      (should (equal (alist-get 'id result) "abc"))
      (should (equal (alist-get 'file result) "foo.el"))
      (should (eq (alist-get 'busy result) :json-false)))))

(ert-deftest beemacs-test-json-request-malformed-signals ()
  "`beemacs-api-json-request' signals `beemacs-api-error' on malformed JSON."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{not valid json")))
    (should-error (beemacs-api-json-request "/api/editor/abc")
                  :type 'beemacs-api-error)))

(ert-deftest beemacs-test-json-request-http-error-with-json-body-surfaces-detail ()
  "A non-2xx response with a JSON `error' field surfaces that message.

Mirrors beehived's `writeJSON' convention (internal/web/editor.go,
jsonapi.go): every JSON handler reports a failure as
`{\"error\": \"<message>\"}' alongside the non-2xx status."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 404 "{\"error\":\"no such session\"}")))
    (let ((err (should-error (beemacs-api-json-request "/api/editor/missing")
                              :type 'beemacs-api-error)))
      (should (string-match-p "no such session" (error-message-string err))))))

(ert-deftest beemacs-test-json-request-http-error-without-json-body ()
  "A non-2xx response with a non-JSON body falls back to the transport message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 500 "internal server error")))
    (let ((err (should-error (beemacs-api-json-request "/api/editor/abc")
                              :type 'beemacs-api-error)))
      (should (string-match-p "non-2xx response 500" (error-message-string err))))))

(ert-deftest beemacs-test-json-request-connection-failure-signals ()
  "`beemacs-api-json-request' signals `beemacs-api-error' on connection failure."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (let ((err (should-error (beemacs-api-json-request "/api/editor/abc")
                              :type 'beemacs-api-error)))
      (should (string-match-p "connection refused" (error-message-string err))))))

(ert-deftest beemacs-test-render-submodule-names ()
  "The render layer extracts submodule names from API-shaped data."
  (should (equal (beemacs-render-submodule-names
                  (vector '((name . "beemacs")) '((name . "beehive"))))
                 '("beemacs" "beehive"))))

(ert-deftest beemacs-test-render-doc-rows ()
  "The render layer builds tabulated-list rows from a docs.json-shaped payload."
  (let ((docs (vector '((Path . "bee-x-x.md") (Name . "bee-x-x.md") (Dir . "") (Href . "/h1"))
                       '((Path . "tasks/foo.md") (Name . "foo.md") (Dir . "tasks") (Href . "/h2")))))
    (should (equal (beemacs-render-doc-rows docs)
                   '(("bee-x-x.md" ["bee-x-x.md" "" "bee-x-x.md"])
                     ("tasks/foo.md" ["foo.md" "tasks" "tasks/foo.md"]))))))

(ert-deftest beemacs-test-render-branch-rows ()
  "The render layer builds tabulated-list rows from a branches.json-shaped payload."
  (let ((commits (vector '((SHA . "abc123") (Author . "swarm") (Date . "2026-08-23")
                            (Subject . "did a thing") (DocTask . "beemacs-foo"))
                          '((SHA . "def456") (Author . "swarm") (Date . "2026-08-22")
                            (Subject . "did another thing") (DocTask . "")))))
    (should (equal (beemacs-render-branch-rows commits)
                   '(("abc123" ["abc123" "swarm" "2026-08-23" "did a thing" "beemacs-foo"])
                     ("def456" ["def456" "swarm" "2026-08-22" "did another thing" ""]))))))

(ert-deftest beemacs-test-render-diff-lines-pure-add ()
  "A pure addition tags only new lines `add', existing lines `same'."
  (should (equal (beemacs-render-diff-lines "a\nb" "a\nb\nc")
                 '((same . "a") (same . "b") (add . "c")))))

(ert-deftest beemacs-test-render-diff-lines-pure-delete ()
  "A pure deletion tags only removed lines `del', remaining lines `same'."
  (should (equal (beemacs-render-diff-lines "a\nb\nc" "a\nc")
                 '((same . "a") (del . "b") (same . "c")))))

(ert-deftest beemacs-test-render-diff-lines-replace ()
  "A line replaced in place shows as a delete immediately followed by an add."
  (should (equal (beemacs-render-diff-lines "a\nb\nc" "a\nB\nc")
                 '((same . "a") (del . "b") (add . "B") (same . "c")))))

(ert-deftest beemacs-test-render-diff-lines-identical ()
  "Identical text yields an all-`same' diff."
  (should (equal (beemacs-render-diff-lines "a\nb" "a\nb")
                 '((same . "a") (same . "b")))))

(ert-deftest beemacs-test-render-unified-diff-shape ()
  "The unified diff renders a/b headers, a single hunk header, and +/- lines."
  (let ((out (beemacs-render-unified-diff "a\nb" "a\nb\nc" "PLAN.md")))
    (should (equal out "--- a/PLAN.md\n+++ b/PLAN.md\n@@ -1,2 +1,3 @@\n a\n b\n+c"))))

(ert-deftest beemacs-test-api-docs-path ()
  "`beemacs-api-docs' hits the docs.json endpoint for the given submodule."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"docs\":[]}"))))
      (let ((result (beemacs-api-docs "beemacs")))
        (should (string-suffix-p "/submodule/beemacs/docs.json" seen-url))
        (should (equal (alist-get 'name result) "beemacs"))))))

(ert-deftest beemacs-test-api-doc-path ()
  "`beemacs-api-doc' hits the doc.json/{file} endpoint with FILE appended."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"file\":\"a.md\",\"body\":\"hi\"}"))))
      (let ((result (beemacs-api-doc "beemacs" "a.md")))
        (should (string-suffix-p "/submodule/beemacs/doc.json/a.md" seen-url))
        (should (equal (alist-get 'body result) "hi"))))))

(ert-deftest beemacs-test-api-branches-path-no-params ()
  "`beemacs-api-branches' omits query params when OFFSET/LIMIT are nil."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"commits\":[]}"))))
      (beemacs-api-branches "beemacs")
      (should (string-suffix-p "/submodule/beemacs/branches.json" seen-url)))))

(ert-deftest beemacs-test-api-branches-path-with-params ()
  "`beemacs-api-branches' passes OFFSET/LIMIT through as query params."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"commits\":[]}"))))
      (beemacs-api-branches "beemacs" 50 25)
      (should (string-suffix-p "/submodule/beemacs/branches.json?offset=50&limit=25" seen-url)))))

(ert-deftest beemacs-test-api-commit-path ()
  "`beemacs-api-commit' hits the commit.json/{sha} endpoint with SHA appended."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"sha\":\"abc\",\"plan_before\":\"a\",\"plan_after\":\"b\"}"))))
      (let ((result (beemacs-api-commit "beemacs" "abc")))
        (should (string-suffix-p "/submodule/beemacs/commit.json/abc" seen-url))
        (should (equal (alist-get 'plan_before result) "a"))
        (should (equal (alist-get 'plan_after result) "b"))))))

(provide 'beemacs-tests)

;;; beemacs-tests.el ends here
