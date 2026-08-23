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
(require 'beemacs-editor)
(require 'beemacs-env)
(require 'beemacs-pi)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'beemacs-pi-model)
(require 'beemacs-streaming)

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

(ert-deftest beemacs-test-render-dashboard-rows ()
  "The render layer builds tabulated-list rows from a dashboard.json-shaped payload."
  (let ((subs (vector '((Name . "beemacs") (State . "idle") (Stamp . "abc")
                        (Pending . 2) (Human . 0) (Env . "blue")
                        (Working . t) (Bees . 1))
                       '((Name . "beehive") (State . "idle") (Stamp . "def")
                         (Pending . 0) (Human . 1) (Env . "") (Working . :json-false)
                         (Bees . 0)))))
    (should (equal (beemacs-render-dashboard-rows subs)
                   '(("beemacs" ["beemacs" "idle" "2" "0" "blue" "yes" "1"])
                     ("beehive" ["beehive" "idle" "0" "1" "" "no" "0"]))))))

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

(ert-deftest beemacs-test-api-skills-path ()
  "`beemacs-api-skills' hits the hive-wide skills.json endpoint (no submodule)."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"hygiene\":{},\"dances\":[],\"cache\":{}}"))))
      (let ((result (beemacs-api-skills)))
        (should (string-suffix-p "/skills.json" seen-url))
        (should (equal (alist-get 'dances result) []))))))

(ert-deftest beemacs-test-api-dashboard-path ()
  "`beemacs-api-dashboard' hits the hive-wide dashboard.json endpoint."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}"))))
      (let ((result (beemacs-api-dashboard)))
        (should (string-suffix-p "/dashboard.json" seen-url))
        (should (equal (length (alist-get 'subs result)) 1))))))

(ert-deftest beemacs-test-api-dashboard-submodule-finds-match ()
  "`beemacs-api-dashboard-submodule' filters the `subs' vector by Name."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"subs\":[{\"Name\":\"alpha\",\"State\":\"idle\"},"
                          "{\"Name\":\"beemacs\",\"State\":\"busy\"}]}"))))
    (let ((result (beemacs-api-dashboard-submodule "beemacs")))
      (should (equal (alist-get 'State result) "busy")))))

(ert-deftest beemacs-test-api-dashboard-submodule-no-match ()
  "`beemacs-api-dashboard-submodule' returns nil when NAME is absent."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"subs\":[{\"Name\":\"alpha\"}]}")))
    (should (null (beemacs-api-dashboard-submodule "beemacs")))))

(ert-deftest beemacs-test-api-plan-path ()
  "`beemacs-api-plan' hits the plan.json endpoint for the given submodule."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"plan\":{\"Items\":[]}}"))))
      (let ((result (beemacs-api-plan "beemacs")))
        (should (string-suffix-p "/submodule/beemacs/plan.json" seen-url))
        (should (equal (alist-get 'name result) "beemacs"))))))

(ert-deftest beemacs-test-api-roi-path ()
  "`beemacs-api-roi' hits the roi.json endpoint for the given submodule."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"name\":\"beemacs\",\"body\":\"# ROI\",\"remote_url\":\"u\"}"))))
      (let ((result (beemacs-api-roi "beemacs")))
        (should (string-suffix-p "/submodule/beemacs/roi.json" seen-url))
        (should (equal (alist-get 'body result) "# ROI"))))))

(ert-deftest beemacs-test-api-secrets-path ()
  "`beemacs-api-secrets' hits the hive-wide secrets.json endpoint."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"global\":[],\"submodules\":[]}"))))
      (beemacs-api-secrets)
      (should (string-suffix-p "/secrets.json" seen-url)))))

(ert-deftest beemacs-test-api-secrets-for-filters-by-name ()
  "`beemacs-api-secrets-for' returns just NAME's `keys' vector."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"global\":[],\"submodules\":["
                          "{\"name\":\"alpha\",\"keys\":[\"A\"]},"
                          "{\"name\":\"beemacs\",\"keys\":[\"B\",\"C\"]}]}"))))
    (should (equal (append (beemacs-api-secrets-for "beemacs") nil) '("B" "C")))))

(ert-deftest beemacs-test-api-secrets-for-no-match ()
  "`beemacs-api-secrets-for' returns nil when NAME is absent."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"global\":[],\"submodules\":[]}")))
    (should (null (beemacs-api-secrets-for "beemacs")))))

(ert-deftest beemacs-test-render-plan-rows ()
  "`beemacs-render-plan-rows' builds one tabulated row per task."
  (let ((items [((ID . "foo") (Status . "TODO") (Weight . 4) (Deps . ["bar"])
                 (Session . "") (Active . :json-false) (Stale . :json-false))
                ((ID . "bar") (Status . "DONE") (Weight . 2) (Deps . [])
                 (Session . "sess-1") (Active . t) (Stale . :json-false))]))
    (should (equal (beemacs-render-plan-rows items)
                   '(("foo" ["foo" "TODO" "4" "bar" ""])
                     ("bar" ["bar" "DONE" "2" "" "active sess-1"]))))))

(ert-deftest beemacs-test-render-claim-state-stale ()
  "`beemacs-render--claim-state' labels a past-TTL claim \"stale <session>\"."
  (should (equal (beemacs-render--claim-state
                   '((Session . "sess-2") (Active . :json-false) (Stale . t)))
                 "stale sess-2"))
  (should (equal (beemacs-render--claim-state
                   '((Session . "") (Active . :json-false) (Stale . :json-false)))
                 "")))

(ert-deftest beemacs-test-submodule-view-refresh-shows-summary ()
  "`beemacs-submodule-view' populates its buffer with summary + nav lines."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\","
                          "\"Stamp\":\"abc123\",\"Pending\":2,\"Human\":0,"
                          "\"Env\":\"blue\",\"Working\":false,\"Bees\":1}]}"))))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (with-current-buffer "*beemacs-submodule: beemacs*"
          (should (derived-mode-p 'beemacs-submodule-view-mode))
          (should (equal beemacs-submodule-view--name "beemacs"))
          (let ((text (buffer-string)))
            (should (string-match-p "State: idle" text))
            (should (string-match-p "ROI stamp: abc123" text))
            (should (string-match-p "Pending: 2" text))
            (should (string-match-p "\\[p\\] Plan" text))
            (should (string-match-p "\\[o\\] ROI" text))
            (should (string-match-p "\\[d\\] Docs" text))))
      (when (get-buffer "*beemacs-submodule: beemacs*")
        (kill-buffer "*beemacs-submodule: beemacs*")))))

(ert-deftest beemacs-test-submodule-view-refresh-no-summary ()
  "A submodule absent from the dashboard payload still renders navigation."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"subs\":[]}")))
    (beemacs-submodule-view "ghost")
    (unwind-protect
        (with-current-buffer "*beemacs-submodule: ghost*"
          (let ((text (buffer-string)))
            (should (string-match-p "no dashboard summary available" text))
            (should (string-match-p "\\[S\\] Secrets" text))))
      (when (get-buffer "*beemacs-submodule: ghost*")
        (kill-buffer "*beemacs-submodule: ghost*")))))

(ert-deftest beemacs-test-dashboard-populates-rows ()
  "`beemacs-dashboard' fetches dashboard.json and lists each submodule."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil
                       (concat "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\","
                               "\"Stamp\":\"abc\",\"Pending\":2,\"Human\":0,"
                               "\"Env\":\"blue\",\"Working\":false,\"Bees\":1}]}")))))
      (beemacs-dashboard)
      (unwind-protect
          (with-current-buffer "*beemacs-dashboard*"
            (should (string-suffix-p "/dashboard.json" seen-url))
            (should (derived-mode-p 'beemacs-dashboard-mode))
            (should (equal tabulated-list-entries
                            '(("beemacs" ["beemacs" "idle" "2" "0" "blue" "no" "1"])))))
        (when (get-buffer "*beemacs-dashboard*")
          (kill-buffer "*beemacs-dashboard*"))))))

(ert-deftest beemacs-test-dashboard-refresh-refetches ()
  "`beemacs-dashboard-refresh' re-fetches and redisplays entries."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"subs\":[]}")))
    (beemacs-dashboard))
  (unwind-protect
      (with-current-buffer "*beemacs-dashboard*"
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 (concat "{\"subs\":[{\"Name\":\"beehive\",\"State\":\"busy\","
                                "\"Stamp\":\"z\",\"Pending\":1,\"Human\":1,"
                                "\"Env\":\"green\",\"Working\":true,\"Bees\":3}]}"))))
          (beemacs-dashboard-refresh))
        (should (equal tabulated-list-entries
                        '(("beehive" ["beehive" "busy" "1" "1" "green" "yes" "3"])))))
    (when (get-buffer "*beemacs-dashboard*")
      (kill-buffer "*beemacs-dashboard*"))))

(ert-deftest beemacs-test-dashboard-open-at-point-drills-into-submodule-view ()
  "RET in `beemacs-dashboard-mode' opens `beemacs-submodule-view' for the row."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\","
                          "\"Stamp\":\"abc\",\"Pending\":0,\"Human\":0,"
                          "\"Env\":\"\",\"Working\":false,\"Bees\":0}]}"))))
    (beemacs-dashboard)
    (unwind-protect
        (progn
          (with-current-buffer "*beemacs-dashboard*"
            (goto-char (point-min))
            (beemacs-dashboard-open-at-point))
          (should (get-buffer "*beemacs-submodule: beemacs*")))
      (when (get-buffer "*beemacs-dashboard*") (kill-buffer "*beemacs-dashboard*"))
      (when (get-buffer "*beemacs-submodule: beemacs*")
        (kill-buffer "*beemacs-submodule: beemacs*")))))

(ert-deftest beemacs-test-submodule-view-plan-opens-plan-buffer ()
  "RET on [p] fetches plan.json and renders it as a navigable plan buffer."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beemacs-transport--call)
                     (beemacs-test--mock-call
                      200 (concat "{\"name\":\"beemacs\",\"plan\":{\"Items\":"
                                  "[{\"ID\":\"foo\",\"Status\":\"TODO\","
                                  "\"Weight\":4,\"Deps\":[]}]}}"))))
            (with-current-buffer "*beemacs-submodule: beemacs*"
              (beemacs-submodule-view-plan)))
          (with-current-buffer "*beemacs-plan: beemacs*"
            (should (derived-mode-p 'beemacs-plan-mode))
            (should (equal beemacs-plan--submodule "beemacs"))
            (should (equal tabulated-list-entries
                            '(("foo" ["foo" "TODO" "4" "" ""]))))))
      (dolist (b '("*beemacs-submodule: beemacs*" "*beemacs-plan: beemacs*"))
        (when (get-buffer b) (kill-buffer b))))))

(ert-deftest beemacs-test-plan-view-populates-rows ()
  "`beemacs-plan-view' fetches plan.json and lists each task."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil
                       (concat "{\"name\":\"beemacs\",\"plan\":{\"Items\":"
                               "[{\"ID\":\"foo\",\"Status\":\"TODO\","
                               "\"Weight\":4,\"Deps\":[\"bar\"],"
                               "\"Session\":\"\",\"Active\":false,"
                               "\"Stale\":false}]}}")))))
      (beemacs-plan-view "beemacs")
      (unwind-protect
          (with-current-buffer "*beemacs-plan: beemacs*"
            (should (string-suffix-p "/submodule/beemacs/plan.json" seen-url))
            (should (derived-mode-p 'beemacs-plan-mode))
            (should (equal tabulated-list-entries
                            '(("foo" ["foo" "TODO" "4" "bar" ""])))))
        (when (get-buffer "*beemacs-plan: beemacs*")
          (kill-buffer "*beemacs-plan: beemacs*"))))))

(ert-deftest beemacs-test-plan-refresh-refetches ()
  "`beemacs-plan-refresh' re-fetches and redisplays entries."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"name\":\"beemacs\",\"plan\":{\"Items\":[]}}")))
    (beemacs-plan-view "beemacs"))
  (unwind-protect
      (with-current-buffer "*beemacs-plan: beemacs*"
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 (concat "{\"name\":\"beemacs\",\"plan\":{\"Items\":"
                                "[{\"ID\":\"baz\",\"Status\":\"DONE\","
                                "\"Weight\":1,\"Deps\":[]}]}}"))))
          (beemacs-plan-refresh))
        (should (equal tabulated-list-entries
                        '(("baz" ["baz" "DONE" "1" "" ""])))))
    (when (get-buffer "*beemacs-plan: beemacs*")
      (kill-buffer "*beemacs-plan: beemacs*"))))

(ert-deftest beemacs-test-plan-open-at-point-opens-doc ()
  "RET in `beemacs-plan-mode' opens the row's linked change doc."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"name\":\"beemacs\",\"plan\":{\"Items\":"
                          "[{\"ID\":\"foo\",\"Status\":\"NEEDS-REVIEW\","
                          "\"Weight\":4,\"Deps\":[],"
                          "\"DocHref\":\"/submodule/beemacs/doc/bee-foo-foo.md\","
                          "\"Running\":false}]}}"))))
    (beemacs-plan-view "beemacs"))
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call 200 "{\"body\":\"doc body text\"}")))
          (with-current-buffer "*beemacs-plan: beemacs*"
            (goto-char (point-min))
            (beemacs-plan-open-at-point)))
        (should (get-buffer "*beemacs-doc: beemacs/bee-foo-foo.md*"))
        (with-current-buffer "*beemacs-doc: beemacs/bee-foo-foo.md*"
          (should (string-match-p "doc body text" (buffer-string)))))
    (dolist (b '("*beemacs-plan: beemacs*" "*beemacs-doc: beemacs/bee-foo-foo.md*"))
      (when (get-buffer b) (kill-buffer b)))))

(ert-deftest beemacs-test-plan-open-at-point-reports-live-session ()
  "RET in `beemacs-plan-mode' messages the live session when Running."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"name\":\"beemacs\",\"plan\":{\"Items\":"
                          "[{\"ID\":\"foo\",\"Status\":\"TODO\","
                          "\"Weight\":4,\"Deps\":[],\"Running\":true,"
                          "\"SessionHref\":\"/submodule/beemacs/session/bee-foo\"}]}}"))))
    (beemacs-plan-view "beemacs"))
  (unwind-protect
      (let (seen-message)
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (setq seen-message (apply #'format fmt args)))))
          (with-current-buffer "*beemacs-plan: beemacs*"
            (goto-char (point-min))
            (beemacs-plan-open-at-point)))
        (should (string-match-p "/submodule/beemacs/session/bee-foo" seen-message)))
    (when (get-buffer "*beemacs-plan: beemacs*")
      (kill-buffer "*beemacs-plan: beemacs*"))))

(ert-deftest beemacs-test-submodule-view-roi-opens-roi-buffer ()
  "RET on [o] fetches roi.json and renders it read-only."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beemacs-transport--call)
                     (beemacs-test--mock-call
                      200 "{\"name\":\"beemacs\",\"body\":\"# ROI text\"}")))
            (with-current-buffer "*beemacs-submodule: beemacs*"
              (beemacs-submodule-view-roi)))
          (with-current-buffer "*beemacs-roi: beemacs*"
            (should (string-match-p "# ROI text" (buffer-string)))
            (should buffer-read-only)))
      (dolist (b '("*beemacs-submodule: beemacs*" "*beemacs-roi: beemacs*"))
        (when (get-buffer b) (kill-buffer b))))))

(ert-deftest beemacs-test-submodule-view-secrets-opens-secrets-buffer ()
  "RET on [S] fetches secrets.json filtered to this submodule's keys."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'beemacs-transport--call)
                     (beemacs-test--mock-call
                      200 (concat "{\"global\":[],\"submodules\":"
                                  "[{\"name\":\"beemacs\",\"keys\":[\"TOKEN\"]}]}"))))
            (with-current-buffer "*beemacs-submodule: beemacs*"
              (beemacs-submodule-view-secrets)))
          (with-current-buffer "*beemacs-secrets: beemacs*"
            (should (string-match-p "TOKEN" (buffer-string)))))
      (dolist (b '("*beemacs-submodule: beemacs*" "*beemacs-secrets: beemacs*"))
        (when (get-buffer b) (kill-buffer b))))))

(ert-deftest beemacs-test-submodule-view-sessions-reports-pending-gap ()
  "The sessions drill-in reports the pending JSON-API gap via `message'."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (with-current-buffer "*beemacs-submodule: beemacs*"
          (let (msg)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
              (beemacs-submodule-view-sessions))
            (should (string-match-p "sessions.json" msg))))
      (when (get-buffer "*beemacs-submodule: beemacs*")
        (kill-buffer "*beemacs-submodule: beemacs*")))))

(ert-deftest beemacs-test-render-skill-rows ()
  "The render layer builds tabulated-list rows from a skills.json-shaped payload."
  (let ((dances (vector '((Name . "modify-roi") (Title . "Modify an ROI")
                           (Summary . "edit intent") (Destructive . :json-false)
                           (ReportOnly . t))
                         '((Name . "cleanup") (Title . "Cleanup")
                           (Summary . "clear stale state") (Destructive . t)
                           (ReportOnly . :json-false)))))
    (should (equal (beemacs-render-skill-rows dances)
                   '(("modify-roi" ["modify-roi" "Modify an ROI" "edit intent"])
                     ("cleanup" ["cleanup" "Cleanup" "clear stale state"]))))))

(ert-deftest beemacs-test-transport-post-sets-method-and-body ()
  "`beemacs-transport-post' issues a POST with a JSON body and content type."
  (let (seen-method seen-data seen-headers)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (setq seen-method url-request-method
                       seen-data url-request-data
                       seen-headers url-request-extra-headers)
                 (list 200 nil "ok"))))
      (beemacs-transport-post "/api/editor" "{\"file\":\"a.el\"}")
      (should (equal seen-method "POST"))
      (should (equal seen-data (encode-coding-string "{\"file\":\"a.el\"}" 'utf-8)))
      (should (equal (alist-get "Content-Type" seen-headers nil nil #'equal)
                     "application/json")))))

(ert-deftest beemacs-test-api-json-post-well-formed ()
  "`beemacs-api-json-post' encodes PAYLOAD and parses a well-formed JSON reply."
  (let (seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (setq seen-data url-request-data)
                 (list 200 nil "{\"id\":\"e1\",\"file\":\"a.el\",\"state\":\"live\"}"))))
      (let ((result (beemacs-api-json-post "/api/editor" '((file . "a.el")))))
        (should (equal seen-data (encode-coding-string "{\"file\":\"a.el\"}" 'utf-8)))
        (should (equal (alist-get 'id result) "e1"))
        (should (equal (alist-get 'state result) "live"))))))

(ert-deftest beemacs-test-api-json-post-http-error-surfaces-detail ()
  "`beemacs-api-json-post' surfaces the JSON `error' field on a non-2xx reply."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 404 "{\"error\":\"no such session\"}")))
    (let ((err (should-error (beemacs-api-json-post "/api/editor/missing/chat"
                                                      '((message . "hi")))
                              :type 'beemacs-api-error)))
      (should (string-match-p "no such session" (error-message-string err))))))

(ert-deftest beemacs-test-editor-open-creates-buffer ()
  "`beemacs-editor-open' opens a session and creates its chat buffer."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"e1\",\"file\":\"a.el\",\"branch\":\"bee-edit-e1\",\"state\":\"live\"}")))
    (unwind-protect
        (let ((id (beemacs-editor-open "a.el")))
          (should (equal id "e1"))
          (let ((buf (beemacs-editor--find-buffer "e1")))
            (should buf)
            (with-current-buffer buf
              (should (derived-mode-p 'beemacs-editor-mode))
              (should (equal beemacs-editor--id "e1"))
              (should (equal beemacs-editor--file "a.el"))
              (should (equal beemacs-editor--branch "bee-edit-e1"))
              (should (equal beemacs-editor--state "live")))))
      (let ((buf (beemacs-editor--find-buffer "e1")))
        (when buf (kill-buffer buf))))))

(ert-deftest beemacs-test-editor-chat-appends-reply ()
  "`beemacs-editor-chat' sends a message and appends the agent's real reply."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"e2\",\"file\":\"a.el\",\"branch\":\"bee-edit-e2\",\"state\":\"live\"}")))
    (beemacs-editor-open "a.el"))
  (unwind-protect
      (cl-letf (((symbol-function 'beemacs-transport--call)
                 (beemacs-test--mock-call
                  200 "{\"reply\":\"done\",\"state\":\"live\",\"merged\":false}")))
        (let ((buf (beemacs-editor--find-buffer "e2")))
          (with-current-buffer buf
            (beemacs-editor-chat "make it better")
            (should (equal beemacs-editor--state "live"))
            (should (string-match-p "make it better" (buffer-string)))
            (should (string-match-p "done" (buffer-string))))))
    (let ((buf (beemacs-editor--find-buffer "e2")))
      (when buf (kill-buffer buf)))))

(ert-deftest beemacs-test-editor-diff-renders-diff-mode-buffer ()
  "`beemacs-editor-diff' fetches base/proposed and renders a `diff-mode' buffer."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"e3\",\"file\":\"a.el\",\"branch\":\"bee-edit-e3\",\"state\":\"live\"}")))
    (beemacs-editor-open "a.el"))
  (unwind-protect
      (cl-letf (((symbol-function 'beemacs-transport--call)
                 (beemacs-test--mock-call
                  200 "{\"base\":\"a\\nb\",\"proposed\":\"a\\nb\\nc\",\"state\":\"live\"}")))
        (let ((buf (beemacs-editor--find-buffer "e3")))
          (with-current-buffer buf
            (beemacs-editor-diff))))
    (let ((diff-buf (get-buffer "*beemacs-editor-diff: e3*")))
      (should diff-buf)
      (with-current-buffer diff-buf
        (should (derived-mode-p 'diff-mode))
        (should (string-match-p "\\+c" (buffer-string))))
      (when diff-buf (kill-buffer diff-buf))
      (let ((buf (beemacs-editor--find-buffer "e3")))
        (when buf (kill-buffer buf))))))

(ert-deftest beemacs-test-editor-merge-reports-state ()
  "`beemacs-editor-merge' posts confirm=false by default and reports state."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"e4\",\"file\":\"a.el\",\"branch\":\"bee-edit-e4\",\"state\":\"live\"}")))
    (beemacs-editor-open "a.el"))
  (unwind-protect
      (let (seen-data)
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (lambda (_url)
                     (setq seen-data url-request-data)
                     (list 200 nil "{\"state\":\"merged\"}"))))
          (let ((buf (beemacs-editor--find-buffer "e4")))
            (with-current-buffer buf
              (beemacs-editor-merge nil)
              (should (equal beemacs-editor--state "merged"))
              (should (equal seen-data
                             (encode-coding-string "{\"confirm\":false}" 'utf-8))))))
        (let ((buf (beemacs-editor--find-buffer "e4")))
          (when buf (kill-buffer buf))))))

(ert-deftest beemacs-test-editor-close-kills-buffer-locally ()
  "`beemacs-editor-close' kills the local chat buffer with no server call."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"id\":\"e5\",\"file\":\"a.el\",\"branch\":\"bee-edit-e5\",\"state\":\"live\"}")))
    (beemacs-editor-open "a.el"))
  (let ((buf (beemacs-editor--find-buffer "e5")))
    (should buf)
    (with-current-buffer buf
      (beemacs-editor-close))
    (should-not (buffer-live-p buf))))

;;; beemacs-pi tests

(defun beemacs-test--make-fake-pi-rpc (script)
  "Write an executable shell SCRIPT to a temp file, return its path.

The stub stands in for the real `pi' RPC binary: it is invoked with
`beemacs-pi-rpc-args' and should read/write newline-delimited JSON on
stdin/stdout like the real RPC mode.  SCRIPT is the shell body."
  (let ((path (make-temp-file "beemacs-fake-pi-rpc-")))
    (with-temp-file path
      (insert "#!/bin/sh\n")
      (insert script))
    (set-file-modes path #o755)
    path))

(defun beemacs-test--make-fake-pi-oneshot (stdout exit-code)
  "Write an executable shell stub printing STDOUT and exiting EXIT-CODE.

Stands in for `pi's one-shot JSON print mode invocation."
  (let ((path (make-temp-file "beemacs-fake-pi-oneshot-")))
    (with-temp-file path
      (insert "#!/bin/sh\n")
      (insert (format "printf '%%s' %s\n" (shell-quote-argument stdout)))
      (insert (format "exit %d\n" exit-code)))
    (set-file-modes path #o755)
    path))

(ert-deftest beemacs-test-pi-start-spawns-live-process ()
  "`beemacs-pi-start' spawns a running process for a well-behaved pi stub."
  (let* ((beemacs-pi-executable
          (beemacs-test--make-fake-pi-rpc "cat >/dev/null &\nwait\n"))
         (handle (beemacs-pi-start)))
    (unwind-protect
        (progn
          (should (beemacs-pi-alive-p handle))
          (should (beemacs-pi-health-check handle)))
      (beemacs-pi-stop handle))))

(ert-deftest beemacs-test-pi-send-receive-round-trip ()
  "`beemacs-pi-send' writes a JSON line the stub echoes, ON-MESSAGE sees it."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat\n"))
         (received nil)
         (handle (beemacs-pi-start (lambda (msg) (push msg received)))))
    (unwind-protect
        (progn
          (beemacs-pi-send handle '((type . "ping") (id . 1)))
          (with-timeout (2 nil)
            (while (null received)
              (accept-process-output (beemacs-pi-process-proc handle) 0.1)))
          (should received)
          (should (equal (alist-get 'type (car received)) "ping"))
          (should (equal (alist-get 'id (car received)) 1)))
      (beemacs-pi-stop handle))))

(ert-deftest beemacs-test-pi-stop-cleanly-shuts-down ()
  "`beemacs-pi-stop' terminates the child; `beemacs-pi-alive-p' then nil."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (handle (beemacs-pi-start)))
    (should (beemacs-pi-alive-p handle))
    (beemacs-pi-stop handle)
    (should-not (beemacs-pi-alive-p handle))))

(ert-deftest beemacs-test-pi-stop-noop-on-dead-process ()
  "`beemacs-pi-stop' is a safe no-op when the process already exited."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "exit 0\n"))
         (handle (beemacs-pi-start)))
    (with-timeout (2 nil)
      (while (beemacs-pi-alive-p handle)
        (accept-process-output nil 0.1)))
    (should-not (beemacs-pi-alive-p handle))
    (should-not (beemacs-pi-stop handle))))

(ert-deftest beemacs-test-pi-send-signals-when-not-running ()
  "`beemacs-pi-send' signals `beemacs-pi-error' if the process is not alive."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "exit 0\n"))
         (handle (beemacs-pi-start)))
    (with-timeout (2 nil)
      (while (beemacs-pi-alive-p handle)
        (accept-process-output nil 0.1)))
    (should-error (beemacs-pi-send handle '((type . "ping")))
                  :type 'beemacs-pi-error)))

(ert-deftest beemacs-test-pi-start-signals-on-missing-executable ()
  "`beemacs-pi-start' signals `beemacs-pi-error' when the executable is absent."
  (let ((beemacs-pi-executable "/nonexistent/beemacs-fake-pi-binary-does-not-exist"))
    (should-error (beemacs-pi-start) :type 'beemacs-pi-error)))

(ert-deftest beemacs-test-pi-oneshot-parses-json-on-success ()
  "`beemacs-pi-run-oneshot' returns parsed JSON when the stub exits 0."
  (let ((stub (beemacs-test--make-fake-pi-oneshot "{\"reply\":\"ok\"}" 0)))
    (let ((result (beemacs-pi-run-oneshot "hello" stub)))
      (should (equal (alist-get 'reply result) "ok")))))

(ert-deftest beemacs-test-pi-oneshot-signals-on-nonzero-exit ()
  "`beemacs-pi-run-oneshot' signals `beemacs-pi-error' on a non-zero exit."
  (let ((stub (beemacs-test--make-fake-pi-oneshot "boom" 1)))
    (should-error (beemacs-pi-run-oneshot "hello" stub) :type 'beemacs-pi-error)))

(ert-deftest beemacs-test-pi-oneshot-signals-on-malformed-json ()
  "`beemacs-pi-run-oneshot' signals `beemacs-pi-error' on malformed stdout."
  (let ((stub (beemacs-test--make-fake-pi-oneshot "not json at all {{{" 0)))
    (should-error (beemacs-pi-run-oneshot "hello" stub) :type 'beemacs-pi-error)))

(ert-deftest beemacs-test-pi-oneshot-signals-on-missing-executable ()
  "`beemacs-pi-run-oneshot' signals `beemacs-pi-error' for a missing binary."
  (should-error
   (beemacs-pi-run-oneshot "hello" "/nonexistent/beemacs-fake-pi-binary-does-not-exist")
   :type 'beemacs-pi-error))

;;; beemacs-pi-chat tests

(defun beemacs-test--make-fake-pi-rpc-events (&rest json-lines)
  "Write a stub `pi' RPC binary emitting JSON-LINES then blocking on stdin.

Each element of JSON-LINES is already a JSON-encoded string; the stub
prints each on its own line (simulating a canned RPC event sequence) and
then `cat's stdin to stay alive (and to let round-trip sends be observed)
until it is stopped."
  (beemacs-test--make-fake-pi-rpc
   (concat (mapconcat (lambda (line) (format "printf '%%s\\n' %s"
                                              (shell-quote-argument line)))
                       json-lines "\n")
           "\ncat >/dev/null\n")))

(defun beemacs-test--wait-for (predicate &optional timeout)
  "Busy-wait up to TIMEOUT (default 2) seconds until PREDICATE returns non-nil."
  (with-timeout ((or timeout 2) nil)
    (while (not (funcall predicate))
      (accept-process-output nil 0.05))))

(ert-deftest beemacs-test-pi-chat-renders-canned-event-sequence ()
  "`beemacs-pi-chat-open' renders a canned turn/token/tool-call/result stream."
  (let* ((beemacs-pi-executable
          (beemacs-test--make-fake-pi-rpc-events
           (json-encode '((type . "turn_start")))
           (json-encode '((type . "token") (text . "Hello, ")))
           (json-encode '((type . "token") (text . "world.")))
           (json-encode '((type . "tool_call") (id . "t1") (name . "grep")
                          (input . ((pattern . "foo")))))
           (json-encode '((type . "tool_result") (id . "t1") (output . "no matches")))
           (json-encode '((type . "turn_end")))))
         (buf (beemacs-pi-chat-open "test-session")))
    (unwind-protect
        (progn
          (with-current-buffer buf
            (beemacs-test--wait-for
             (lambda () (string-match-p "\\[tool-result\\]" (buffer-string))))
            (should (string-match-p "Hello, world\\." (buffer-string)))
            (should (string-match-p "\\[tool-call\\] grep" (buffer-string)))
            (should (string-match-p "\\[tool-result\\] grep -> no matches" (buffer-string)))
            (should-not beemacs-pi-chat--turn-active)))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-pi-chat-kill-buffer-stops-process ()
  "Killing a `beemacs-pi-chat-mode' buffer cleanly stops its `pi' process."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (buf (beemacs-pi-chat-open "test-teardown"))
         (handle (with-current-buffer buf beemacs-pi-chat--handle)))
    (should (beemacs-pi-alive-p handle))
    (let (kill-buffer-query-functions) (kill-buffer buf))
    (beemacs-test--wait-for (lambda () (not (beemacs-pi-alive-p handle))))
    (should-not (beemacs-pi-alive-p handle))))

(ert-deftest beemacs-test-pi-chat-send-starts-prompt-when-idle ()
  "`beemacs-pi-chat-send' sends a `prompt' message when no turn is active."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (buf (beemacs-pi-chat-open "test-prompt"))
         (sent nil))
    (unwind-protect
        (with-current-buffer buf
          (setq beemacs-pi-chat--turn-active nil)
          (cl-letf (((symbol-function 'beemacs-pi-send)
                     (lambda (_handle request) (push request sent))))
            (beemacs-pi-chat-send "do the thing"))
          (should (equal (alist-get 'type (car sent)) "prompt"))
          (should (equal (alist-get 'text (car sent)) "do the thing"))
          (should (string-match-p "> do the thing" (buffer-string))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-pi-chat-send-steers-when-turn-active ()
  "`beemacs-pi-chat-send' sends a `steer' message when a turn is in flight."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (buf (beemacs-pi-chat-open "test-steer"))
         (sent nil))
    (unwind-protect
        (with-current-buffer buf
          (setq beemacs-pi-chat--turn-active t)
          (cl-letf (((symbol-function 'beemacs-pi-send)
                     (lambda (_handle request) (push request sent))))
            (beemacs-pi-chat-send "actually do this instead"))
          (should (equal (alist-get 'type (car sent)) "steer"))
          (should (string-match-p "(steer) > actually do this instead" (buffer-string))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-pi-chat-abort-sends-abort-message ()
  "`beemacs-pi-chat-abort' sends `{\"type\":\"abort\"}' to the pi process."
  (let* ((beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (buf (beemacs-pi-chat-open "test-abort"))
         (sent nil))
    (unwind-protect
        (with-current-buffer buf
          (cl-letf (((symbol-function 'beemacs-pi-send)
                     (lambda (_handle request) (push request sent))))
            (beemacs-pi-chat-abort))
          (should (equal (alist-get 'type (car sent)) "abort"))
          (should (string-match-p "\\[abort requested\\]" (buffer-string))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

;;; beemacs-pi-sessions tests

(defun beemacs-test--make-fake-pi-session-list (sessions-json)
  "Write a stub `pi' RPC binary replying `session_list' with SESSIONS-JSON.

SESSIONS-JSON is an already JSON-encoded array string. The stub ignores
its stdin request line, replies once, and then blocks (`cat's stdin) so
teardown via `beemacs-pi-stop' can be observed like the real process."
  (beemacs-test--make-fake-pi-rpc
   (concat "read _line\n"
           (format "printf '%%s\\n' %s\n"
                   (shell-quote-argument
                    (json-encode `((type . "session_list")
                                   (sessions . ,sessions-json)))))
           "cat >/dev/null\n")))

(ert-deftest beemacs-test-pi-sessions-list-returns-records ()
  "`beemacs-pi-sessions-list' round-trips a `list_sessions'/`session_list' exchange."
  (let* ((sessions (vector '((id . "s1") (label . "root") (parent . :null) (updated . "2026-01-01"))
                            '((id . "s2") (label . "child") (parent . "s1") (updated . "2026-01-02"))))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-session-list sessions))
         (records (beemacs-pi-sessions-list)))
    (should (= (length records) 2))
    (should (equal (beemacs-pi-sessions--session-id (nth 0 records)) "s1"))
    (should (equal (beemacs-pi-sessions--session-id (nth 1 records)) "s2"))))

(ert-deftest beemacs-test-pi-sessions-list-signals-on-timeout ()
  "`beemacs-pi-sessions-list' signals `beemacs-pi-sessions-error' if pi never replies."
  (let* ((beemacs-pi-sessions-list-timeout 0.2)
         (beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n")))
    (should-error (beemacs-pi-sessions-list) :type 'beemacs-pi-sessions-error)))

(ert-deftest beemacs-test-pi-sessions-build-tree-orders-depth-first ()
  "`beemacs-pi-sessions--build-tree' walks parent-before-child, MRU siblings first."
  (let* ((records (list '((id . "root1") (label . "r1") (parent . :null) (updated . "2026-01-01"))
                        '((id . "c1") (label . "c1") (parent . "root1") (updated . "2026-01-03"))
                        '((id . "c2") (label . "c2") (parent . "root1") (updated . "2026-01-05"))
                        '((id . "root2") (label . "r2") (parent . nil) (updated . "2026-01-02"))))
         (tree (beemacs-pi-sessions--build-tree records))
         (ids (mapcar (lambda (dr) (beemacs-pi-sessions--session-id (cdr dr))) tree))
         (depths (mapcar #'car tree)))
    ;; root2 is more recently updated than root1, so it sorts first among roots.
    (should (equal ids '("root2" "root1" "c2" "c1")))
    (should (equal depths '(0 0 1 1)))))

(ert-deftest beemacs-test-pi-sessions-build-tree-orphan-parent-becomes-root ()
  "A record whose `parent' id is absent from RECORDS is treated as a root."
  (let* ((records (list '((id . "s1") (label . "s1") (parent . "missing-parent") (updated . "2026-01-01"))))
         (tree (beemacs-pi-sessions--build-tree records)))
    (should (equal (mapcar #'car tree) '(0)))))

(ert-deftest beemacs-test-pi-sessions-record-visit-persists-mru ()
  "`beemacs-pi-sessions-record-visit' persists a most-recent-first, deduped, capped MRU."
  (let ((beemacs-pi-sessions-persist-file (make-temp-file "beemacs-pi-sessions-mru-"))
        (beemacs-pi-sessions-mru-limit 2))
    (unwind-protect
        (progn
          (beemacs-pi-sessions-record-visit "s1")
          (beemacs-pi-sessions-record-visit "s2")
          (beemacs-pi-sessions-record-visit "s3")
          (should (equal (beemacs-pi-sessions-mru) '("s3" "s2")))
          (beemacs-pi-sessions-record-visit "s2")
          (should (equal (beemacs-pi-sessions-mru) '("s2" "s3"))))
      (delete-file beemacs-pi-sessions-persist-file))))

(ert-deftest beemacs-test-pi-sessions-resume-sends-resume-request-and-records-mru ()
  "`beemacs-pi-sessions-resume' opens a chat buffer, sends `resume', records MRU."
  (let* ((beemacs-pi-sessions-persist-file (make-temp-file "beemacs-pi-sessions-mru-"))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (record '((id . "sess-42") (label . "42") (parent . :null) (updated . "2026-01-01")))
         sent buf)
    (unwind-protect
        (with-current-buffer (get-buffer-create "*beemacs-pi-sessions-test*")
          (beemacs-pi-sessions-mode)
          (setq beemacs-pi-sessions--records (list record))
          (setq tabulated-list-entries #'beemacs-pi-sessions--entries)
          (tabulated-list-print t)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'beemacs-pi-send)
                     (lambda (_handle request) (push request sent))))
            (setq buf (beemacs-pi-sessions-resume)))
          (should (equal (alist-get 'type (car sent)) "resume"))
          (should (equal (alist-get 'id (car sent)) "sess-42"))
          (should (equal (beemacs-pi-sessions-mru) '("sess-42"))))
      (delete-file beemacs-pi-sessions-persist-file)
      (let (kill-buffer-query-functions)
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*beemacs-pi-sessions-test*")
          (kill-buffer "*beemacs-pi-sessions-test*"))))))

(ert-deftest beemacs-test-pi-sessions-branch-sends-branch-request ()
  "`beemacs-pi-sessions-branch' sends a `branch' request with a `from' id."
  (let* ((beemacs-pi-sessions-persist-file (make-temp-file "beemacs-pi-sessions-mru-"))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (record '((id . "sess-7") (label . "7") (parent . :null) (updated . "2026-01-01")))
         sent buf)
    (unwind-protect
        (with-current-buffer (get-buffer-create "*beemacs-pi-sessions-test-branch*")
          (beemacs-pi-sessions-mode)
          (setq beemacs-pi-sessions--records (list record))
          (setq tabulated-list-entries #'beemacs-pi-sessions--entries)
          (tabulated-list-print t)
          (goto-char (point-min))
          (cl-letf (((symbol-function 'beemacs-pi-send)
                     (lambda (_handle request) (push request sent))))
            (setq buf (beemacs-pi-sessions-branch)))
          (should (equal (alist-get 'type (car sent)) "branch"))
          (should (equal (alist-get 'from (car sent)) "sess-7")))
      (delete-file beemacs-pi-sessions-persist-file)
      (let (kill-buffer-query-functions)
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (get-buffer "*beemacs-pi-sessions-test-branch*")
          (kill-buffer "*beemacs-pi-sessions-test-branch*"))))))

;;; beemacs-pi-model tests

(defun beemacs-test--make-fake-pi-model-list (providers-json)
  "Write a stub `pi' RPC binary replying `model_list' with PROVIDERS-JSON.

PROVIDERS-JSON is an already JSON-encoded array string. The stub ignores
its stdin request line, replies once, and then blocks (`cat's stdin) so
teardown via `beemacs-pi-stop' can be observed like the real process."
  (beemacs-test--make-fake-pi-rpc
   (concat "read _line\n"
           (format "printf '%%s\\n' %s\n"
                   (shell-quote-argument
                    (json-encode `((type . "model_list")
                                   (providers . ,providers-json)))))
           "cat >/dev/null\n")))

(ert-deftest beemacs-test-pi-model-list-returns-records ()
  "`beemacs-pi-model-list' round-trips a `list_models'/`model_list' exchange."
  (let* ((providers (vector '((provider . "anthropic") (models . ["claude-opus-4" "claude-sonnet-4"]))
                             '((provider . "openai") (models . ["gpt-5"]))))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-model-list providers))
         (records (beemacs-pi-model-list)))
    (should (= (length records) 2))
    (should (equal (beemacs-pi-model--provider-name (nth 0 records)) "anthropic"))
    (should (equal (beemacs-pi-model--provider-models (nth 0 records))
                   '("claude-opus-4" "claude-sonnet-4")))
    (should (equal (beemacs-pi-model--provider-name (nth 1 records)) "openai"))
    (should (equal (beemacs-pi-model--provider-models (nth 1 records)) '("gpt-5")))))

(ert-deftest beemacs-test-pi-model-list-signals-on-timeout ()
  "`beemacs-pi-model-list' signals `beemacs-pi-model-error' if pi never replies."
  (let* ((beemacs-pi-model-list-timeout 0.2)
         (beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n")))
    (should-error (beemacs-pi-model-list) :type 'beemacs-pi-model-error)))

(ert-deftest beemacs-test-pi-model-candidates-flattens-provider-model-pairs ()
  "`beemacs-pi-model--candidates' flattens providers/models to \"PROVIDER/MODEL\" pairs."
  (let* ((providers (list '((provider . "anthropic") (models . ("claude-opus-4" "claude-sonnet-4")))
                          '((provider . "openai") (models . ("gpt-5")))))
         (candidates (beemacs-pi-model--candidates providers)))
    (should (equal (mapcar #'car candidates)
                   '("anthropic/claude-opus-4" "anthropic/claude-sonnet-4" "openai/gpt-5")))
    (should (equal (cdr (assoc "openai/gpt-5" candidates)) '("openai" . "gpt-5")))))

(ert-deftest beemacs-test-pi-model-set-and-get-default-persists ()
  "`beemacs-pi-model-set-default'/`beemacs-pi-model-default' round-trip via the persist file."
  (let ((beemacs-pi-model-persist-file (make-temp-file "beemacs-pi-model-default-")))
    (unwind-protect
        (progn
          (should-not (beemacs-pi-model-default))
          (beemacs-pi-model-set-default "anthropic" "claude-sonnet-4")
          (should (equal (beemacs-pi-model-default) '("anthropic" . "claude-sonnet-4"))))
      (delete-file beemacs-pi-model-persist-file))))

(ert-deftest beemacs-test-pi-model-current-prefers-session-override ()
  "`beemacs-pi-model-current' prefers a buffer-local override over the persisted default."
  (let ((beemacs-pi-model-persist-file (make-temp-file "beemacs-pi-model-default-")))
    (unwind-protect
        (with-temp-buffer
          (beemacs-pi-model-set-default "anthropic" "claude-sonnet-4")
          (should (equal (beemacs-pi-model-current) '("anthropic" . "claude-sonnet-4")))
          (setq beemacs-pi-model--session-override '("openai" . "gpt-5"))
          (should (equal (beemacs-pi-model-current) '("openai" . "gpt-5"))))
      (delete-file beemacs-pi-model-persist-file))))

(ert-deftest beemacs-test-pi-model-select-persists-default-outside-chat-buffer ()
  "`beemacs-pi-model-select' persists the choice as the install default by default."
  (let* ((beemacs-pi-model-persist-file (make-temp-file "beemacs-pi-model-default-"))
         (providers (vector '((provider . "anthropic") (models . ["claude-opus-4"]))))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-model-list providers)))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "anthropic/claude-opus-4")))
            (beemacs-pi-model-select))
          (should (equal (beemacs-pi-model-default) '("anthropic" . "claude-opus-4")))
          (should-not beemacs-pi-model--session-override))
      (delete-file beemacs-pi-model-persist-file))))

(ert-deftest beemacs-test-pi-model-select-session-only-does-not-persist ()
  "`beemacs-pi-model-select' with SESSION-ONLY sets a buffer-local override, no persist."
  (let* ((beemacs-pi-model-persist-file (make-temp-file "beemacs-pi-model-default-"))
         (providers (vector '((provider . "openai") (models . ["gpt-5"]))))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-model-list providers)))
    (unwind-protect
        (with-temp-buffer
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) "openai/gpt-5")))
            (beemacs-pi-model-select t))
          (should (equal beemacs-pi-model--session-override '("openai" . "gpt-5")))
          (should-not (beemacs-pi-model-default)))
      (delete-file beemacs-pi-model-persist-file))))

(ert-deftest beemacs-test-pi-model-select-in-chat-buffer-sets-session-override ()
  "`beemacs-pi-model-select' invoked in a `beemacs-pi-chat-mode' buffer overrides only that session."
  (let* ((beemacs-pi-model-persist-file (make-temp-file "beemacs-pi-model-default-"))
         (providers (vector '((provider . "anthropic") (models . ["claude-sonnet-4"]))))
         (model-executable (beemacs-test--make-fake-pi-model-list providers))
         (beemacs-pi-executable (beemacs-test--make-fake-pi-rpc "cat >/dev/null\n"))
         (buf (beemacs-pi-chat-open "test-model-select")))
    (unwind-protect
        (with-current-buffer buf
          (let ((beemacs-pi-executable model-executable))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "anthropic/claude-sonnet-4")))
              (beemacs-pi-model-select)))
          (should (equal beemacs-pi-model--session-override '("anthropic" . "claude-sonnet-4")))
          (should-not (beemacs-pi-model-default)))
      (delete-file beemacs-pi-model-persist-file)
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

;; --- beemacs-streaming (SSE) tests ---

(defun beemacs-test--sse-conn (&optional callback)
  "Return a fresh `beemacs-sse-connection' with HEADERS-DONE-P already set.

Skips HTTP-header stripping so tests can feed body-only SSE byte chunks
directly, per the framing tests below.  CALLBACK defaults to a lambda
that pushes payloads onto a list bound in the caller's `let'."
  (beemacs-sse-connection--create
   :callback (or callback #'ignore)
   :headers-done-p t))

(ert-deftest beemacs-test-sse-feed-single-chunk-single-event ()
  "A single chunk containing one complete SSE frame fires the callback once."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "data: hello\n\n")
    (should (equal received '("hello")))
    (should (equal (beemacs-sse-connection-pending conn) ""))))

(ert-deftest beemacs-test-sse-feed-multiple-events-one-chunk ()
  "Two frames delivered in one chunk both fire the callback, in order."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "data: one\n\ndata: two\n\n")
    (should (equal (nreverse received) '("one" "two")))))

(ert-deftest beemacs-test-sse-feed-split-data-line-across-chunks ()
  "A `data:' frame whose payload is split mid-line across two chunks is
reassembled into a single correct callback invocation, not two, and not
corrupted or dropped."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "data: hel")
    (should (null received))
    (beemacs-sse--feed conn "lo world\n\n")
    (should (equal received '("hello world")))))

(ert-deftest beemacs-test-sse-feed-split-across-blank-line-terminator ()
  "A split that lands exactly between the data line and its terminating
blank line still dispatches exactly once, with the frame boundary
correctly detected only once the blank line itself has arrived."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "data: payload\n")
    (should (null received))
    (beemacs-sse--feed conn "\n")
    (should (equal received '("payload")))))

(ert-deftest beemacs-test-sse-feed-split-many-tiny-chunks ()
  "A frame delivered byte-by-byte across many tiny chunks still reassembles
into one correct callback invocation."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received))))
         (bytes (append (string-to-list "data: chunked\n\n") nil)))
    (dolist (b bytes)
      (beemacs-sse--feed conn (char-to-string b)))
    (should (equal received '("chunked")))))

(ert-deftest beemacs-test-sse-feed-multiline-data-joined-with-newline ()
  "Multiple `data:' lines within one frame are joined with a newline,
matching the SSE multi-line-data convention."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "data: line one\ndata: line two\n\n")
    (should (equal received '("line one\nline two")))))

(ert-deftest beemacs-test-sse-feed-ignores-event-and-comment-lines ()
  "`event:', `id:', and comment (\":\"-prefixed) lines do not themselves
trigger a callback and do not corrupt the surrounding data payload."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received)))))
    (beemacs-sse--feed conn ": keep-alive\nevent: message\nid: 42\ndata: real\n\n")
    (should (equal received '("real")))))

(ert-deftest beemacs-test-sse-feed-strips-http-headers-before-body ()
  "`beemacs-sse--feed' skips the raw HTTP response header block (even when
split across chunks) before parsing SSE frames out of the body."
  (let* ((received nil)
         (conn (beemacs-sse-connection--create
                :callback (lambda (p) (push p received)))))
    (beemacs-sse--feed conn "HTTP/1.1 200 OK\r\nContent-Type: text/event-stream\r\n")
    (should (null received))
    (should-not (beemacs-sse-connection-headers-done-p conn))
    (beemacs-sse--feed conn "\r\ndata: after-headers\n\n")
    (should (beemacs-sse-connection-headers-done-p conn))
    (should (equal received '("after-headers")))))

(ert-deftest beemacs-test-sse-filter-noop-after-abort ()
  "A filter built for an aborted connection drops chunks without invoking
CALLBACK, so a race between process teardown and an in-flight chunk
cannot fire a callback on a torn-down connection."
  (let* ((received nil)
         (conn (beemacs-test--sse-conn (lambda (p) (push p received))))
         (filter (beemacs-sse--filter conn)))
    (setf (beemacs-sse-connection-aborted-p conn) t)
    (funcall filter 'fake-proc "data: nope\n\n")
    (should (null received))))

(ert-deftest beemacs-test-sse-connect-installs-filter-and-abort-cleans-up ()
  "`beemacs-sse-connect' returns a connection whose process filter feeds
`beemacs-sse--feed', and `beemacs-sse-abort' leaves no live process or
buffer behind."
  (let* ((received nil)
         (fake-buffer (generate-new-buffer " *beemacs-sse-test*"))
         (fake-proc (make-pipe-process :name "beemacs-sse-test"
                                        :buffer fake-buffer
                                        :noquery t
                                        :filter #'ignore)))
    (unwind-protect
        (cl-letf (((symbol-function 'url-retrieve)
                   (lambda (_url _callback &rest _)
                     fake-buffer)))
          (let ((conn (beemacs-sse-connect
                       "/events" (lambda (p) (push p received)))))
            (should (beemacs-sse-connection-proc conn))
            (should (process-filter (beemacs-sse-connection-proc conn)))
            (funcall (process-filter (beemacs-sse-connection-proc conn))
                     fake-proc "HTTP/1.1 200 OK\r\n\r\ndata: live\n\n")
            (should (equal received '("live")))
            (beemacs-sse-abort conn)
            (should-not (process-live-p fake-proc))
            (should-not (buffer-live-p fake-buffer))))
      (when (process-live-p fake-proc) (delete-process fake-proc))
      (when (buffer-live-p fake-buffer) (kill-buffer fake-buffer)))))

(ert-deftest beemacs-test-sse-connect-signals-when-url-retrieve-fails ()
  "`beemacs-sse-connect' signals `beemacs-sse-error' when the underlying
request cannot even be started (`url-retrieve' returns nil)."
  (cl-letf (((symbol-function 'url-retrieve) (lambda (&rest _) nil)))
    (should-error (beemacs-sse-connect "/events" #'ignore)
                  :type 'beemacs-sse-error)))

(defconst beemacs-test--env-panel-html
  "<div id=\"env-panel\">
<h1>Environments</h1>
<p>active: <b>blue</b></p>
<form method=\"post\" action=\"/env/deploy\">
  <select name=\"target\"><option>blue</option><option>green</option></select>
  <button>deploy</button>
</form>
</div>"
  "A fixture mirroring beehived's `env_panel.html' with active=blue.")

(defconst beemacs-test--env-panel-html-green
  "<div id=\"env-panel\">
<h1>Environments</h1>
<p>active: <b>green</b></p>
<form method=\"post\" action=\"/env/deploy\">
  <select name=\"target\"><option>blue</option><option>green</option></select>
  <button>deploy</button>
</form>
</div>"
  "A fixture mirroring beehived's `env_panel.html' with active=green.")

(ert-deftest beemacs-test-env-parse-extracts-active-and-envs ()
  "`beemacs-env--parse' extracts the active env and the full envs list."
  (let ((state (beemacs-env--parse beemacs-test--env-panel-html)))
    (should (equal (alist-get 'active state) "blue"))
    (should (equal (alist-get 'envs state) '("blue" "green")))))

(ert-deftest beemacs-test-env-parse-signals-on-unrecognized-response ()
  "`beemacs-env--parse' signals rather than guessing on an unrecognized body."
  (should-error (beemacs-env--parse "<html>nothing here</html>")
                :type 'user-error))

(ert-deftest beemacs-test-env-state-performs-real-get ()
  "`beemacs-env-state' issues `GET /env' and returns the parsed real state."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 beemacs-test--env-panel-html
                                       '(("content-type" . "text/html")))))
    (let ((state (beemacs-env-state)))
      (should (equal (alist-get 'active state) "blue"))
      (should (equal (alist-get 'envs state) '("blue" "green"))))))

(ert-deftest beemacs-test-env-view-reports-real-state ()
  "`beemacs-env-view' returns the real parsed server state."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 beemacs-test--env-panel-html
                                       '(("content-type" . "text/html")))))
    (let ((state (beemacs-env-view)))
      (should (equal (alist-get 'active state) "blue")))))

(ert-deftest beemacs-test-env-deploy-sends-target-as-query-param ()
  "`beemacs-env-deploy' POSTs to `/env/deploy' with TARGET in the URL query."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url)
                 (list 200 '(("content-type" . "text/html"))
                       beemacs-test--env-panel-html-green))))
      (beemacs-env-deploy "green")
      (should (string-match-p "/env/deploy\\?target=green\\'" seen-url)))))

(ert-deftest beemacs-test-env-deploy-reports-real-post-deploy-active ()
  "`beemacs-env-deploy' reports the ACTUAL post-deploy active env."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 beemacs-test--env-panel-html-green
                                       '(("content-type" . "text/html")))))
    (let ((state (beemacs-env-deploy "green")))
      (should (equal (alist-get 'active state) "green")))))

(ert-deftest beemacs-test-env-deploy-signals-when-deploy-did-not-take-effect ()
  "`beemacs-env-deploy' signals a `user-error' if the reported active env
does not match the requested TARGET, rather than reporting a false success."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 beemacs-test--env-panel-html
                                       '(("content-type" . "text/html")))))
    (should-error (beemacs-env-deploy "green") :type 'user-error)))

(ert-deftest beemacs-test-env-deploy-signals-on-transport-failure ()
  "`beemacs-env-deploy' propagates a real `beemacs-http-error' on failure."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 500 "internal server error")))
    (should-error (beemacs-env-deploy "green") :type 'beemacs-http-error)))

(ert-deftest beemacs-test-instruction-update-reports-real-response-body ()
  "`beemacs-instruction-update' POSTs `/instruction/update' and returns the
server's real response body."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"status\":\"ok\"}")))
    (should (equal (beemacs-instruction-update) "{\"status\":\"ok\"}"))))

(ert-deftest beemacs-test-instruction-update-signals-real-error ()
  "`beemacs-instruction-update' surfaces the server's real error (e.g. a
404 for a not-yet-wired route) rather than a fabricated success."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 404 "404 page not found")))
    (should-error (beemacs-instruction-update) :type 'beemacs-http-error)))

(provide 'beemacs-tests)

;;; beemacs-tests.el ends here
