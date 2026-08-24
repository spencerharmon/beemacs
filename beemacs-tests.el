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
(require 'beemacs-persistence)
(require 'beemacs-pi)
(require 'beemacs-pi-chat)
(require 'beemacs-pi-sessions)
(require 'beemacs-pi-model)
(require 'beemacs-streaming)
(require 'beemacs-session)
(require 'beemacs-human)
(require 'beemacs-stats)
(require 'beemacs-transient)

(ert-deftest beemacs-test-version-defined ()
  "Smoke test: `beemacs-version' is defined and looks like a version string."
  (should (stringp beemacs-version))
  (should (string-match-p "\\`[0-9]+\\.[0-9]+\\.[0-9]+\\'" beemacs-version)))

(ert-deftest beemacs-test-persistence-read-write-file-round-trips ()
  "`beemacs-persistence-write-file'/`beemacs-persistence-read-file' round-trip."
  (let ((file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (should-not (beemacs-persistence-read-file file))
          (beemacs-persistence-write-file file '(("a" . 1) ("b" . 2)))
          (should (equal (beemacs-persistence-read-file file)
                          '(("a" . 1) ("b" . 2)))))
      (delete-file file))))

(ert-deftest beemacs-test-persistence-read-file-predicate-rejects-mismatch ()
  "`beemacs-persistence-read-file' returns nil when PREDICATE rejects the value."
  (let ((file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (beemacs-persistence-write-file file "not-a-list")
          (should-not (beemacs-persistence-read-file file #'listp))
          (should (equal (beemacs-persistence-read-file file #'stringp) "not-a-list")))
      (delete-file file))))

(ert-deftest beemacs-test-persistence-endpoint-round-trips ()
  "`beemacs-persistence-set-endpoint'/`beemacs-persistence-endpoint' round-trip
and apply the override to `beemacs-endpoint' immediately."
  (let ((beemacs-persistence-file (make-temp-file "beemacs-persistence-"))
        (beemacs-endpoint beemacs-endpoint))
    (unwind-protect
        (progn
          (should-not (beemacs-persistence-endpoint))
          (beemacs-persistence-set-endpoint "http://example.com:9090")
          (should (equal (beemacs-persistence-endpoint) "http://example.com:9090"))
          (should (equal beemacs-endpoint "http://example.com:9090")))
      (delete-file beemacs-persistence-file))))

(ert-deftest beemacs-test-persistence-pi-executable-round-trips ()
  "`beemacs-persistence-set-pi-executable'/`beemacs-persistence-pi-executable' round-trip."
  (let ((beemacs-persistence-file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (should-not (beemacs-persistence-pi-executable))
          (beemacs-persistence-set-pi-executable "/usr/local/bin/pi")
          (should (equal (beemacs-persistence-pi-executable) "/usr/local/bin/pi")))
      (delete-file beemacs-persistence-file))))

(ert-deftest beemacs-test-persistence-pi-default-model-round-trips ()
  "`beemacs-persistence-set-pi-default-model'/`-pi-default-model' round-trip."
  (let ((beemacs-persistence-file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (should-not (beemacs-persistence-pi-default-model))
          (beemacs-persistence-set-pi-default-model '("anthropic" . "claude-opus-4"))
          (should (equal (beemacs-persistence-pi-default-model)
                          '("anthropic" . "claude-opus-4"))))
      (delete-file beemacs-persistence-file))))

(ert-deftest beemacs-test-persistence-mru-records-most-recent-first-deduped-capped ()
  "`beemacs-persistence-record-mru' moves ids to front, dedupes, and caps."
  (let ((beemacs-persistence-file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (should (equal (beemacs-persistence-mru 'submodule) nil))
          (beemacs-persistence-record-mru 'submodule "sm1" 2)
          (beemacs-persistence-record-mru 'submodule "sm2" 2)
          (beemacs-persistence-record-mru 'submodule "sm3" 2)
          (should (equal (beemacs-persistence-mru 'submodule) '("sm3" "sm2")))
          (beemacs-persistence-record-mru 'submodule "sm2" 2)
          (should (equal (beemacs-persistence-mru 'submodule) '("sm2" "sm3"))))
      (delete-file beemacs-persistence-file))))

(ert-deftest beemacs-test-persistence-mru-kinds-are-independent ()
  "Distinct MRU KINDs (e.g. `submodule' vs `session') persist independently."
  (let ((beemacs-persistence-file (make-temp-file "beemacs-persistence-")))
    (unwind-protect
        (progn
          (beemacs-persistence-record-mru 'submodule "sm1")
          (beemacs-persistence-record-mru 'session "sess-1")
          (should (equal (beemacs-persistence-mru 'submodule) '("sm1")))
          (should (equal (beemacs-persistence-mru 'session) '("sess-1"))))
      (delete-file beemacs-persistence-file))))

(ert-deftest beemacs-test-persistence-header-line-installed-for-beemacs-major-mode ()
  "A buffer in a `beemacs-*' major mode gets the version header line installed."
  (with-temp-buffer
    (let ((major-mode 'beemacs-stats-mode))
      (beemacs-persistence--maybe-install-header-line)
      (should (equal header-line-format
                     (format " beemacs %s" beemacs-version))))))

(ert-deftest beemacs-test-persistence-header-line-not-installed-for-other-modes ()
  "A buffer in a non-beemacs major mode is left alone."
  (with-temp-buffer
    (let ((major-mode 'fundamental-mode)
          (header-line-format nil))
      (beemacs-persistence--maybe-install-header-line)
      (should-not header-line-format))))

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

(ert-deftest beemacs-test-api-dance-plan-path-and-body ()
  "`beemacs-api-dance-plan' POSTs to /api/dances/{name}/plan with no confirm
field, and returns the decoded identity/plan payload."
  (let (seen-url seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url seen-data url-request-data)
                 (list 200 nil
                       (concat "{\"name\":\"gc\",\"title\":\"GC\","
                               "\"destructive\":false,\"reportOnly\":false,"
                               "\"plan\":{\"empty\":true,\"diffs\":[]}}")))))
      (let ((result (beemacs-api-dance-plan "gc")))
        (should (string-suffix-p "/api/dances/gc/plan" seen-url))
        (should (equal seen-data (encode-coding-string "" 'utf-8)))
        (should (equal (alist-get 'name result) "gc"))
        (should (eq (alist-get 'empty (alist-get 'plan result)) t))))))

(ert-deftest beemacs-test-api-dance-apply-omits-confirm-by-default ()
  "`beemacs-api-dance-apply' without CONFIRM POSTs no `confirm' form field."
  (let (seen-url seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url seen-data url-request-data)
                 (list 200 nil
                       (concat "{\"confirmRequired\":true,"
                               "\"error\":\"dance is destructive and requires "
                               "explicit confirmation\"}")))))
      (let ((result (beemacs-api-dance-apply "gc")))
        (should (string-suffix-p "/api/dances/gc/apply" seen-url))
        (should (equal seen-data (encode-coding-string "" 'utf-8)))
        (should (eq (alist-get 'confirmRequired result) t))))))

(ert-deftest beemacs-test-api-dance-apply-sends-confirm-true ()
  "`beemacs-api-dance-apply' with a non-nil CONFIRM POSTs confirm=true."
  (let (seen-url seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url seen-data url-request-data)
                 (list 200 nil
                       "{\"name\":\"gc\",\"result\":{\"ok\":true},\"plan\":{\"empty\":true,\"diffs\":[]}}"))))
      (let ((result (beemacs-api-dance-apply "gc" t)))
        (should (string-suffix-p "/api/dances/gc/apply" seen-url))
        (should (equal seen-data (encode-coding-string "confirm=true" 'utf-8)))
        (should (equal (alist-get 'name result) "gc"))))))

(ert-deftest beemacs-test-api-dance-plan-unknown-signals-api-error ()
  "`beemacs-api-dance-plan' on beehived's 404 unknown-dance response signals
`beemacs-api-error' carrying the server's own detail message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url)
               (list 404 nil "{\"error\":\"unknown dance\"}"))))
    (let ((err (should-error (beemacs-api-dance-plan "nope") :type 'beemacs-api-error)))
      (should (string-match-p "unknown dance" (cadr err))))))

(ert-deftest beemacs-test-dance-plan-opens-buffer-with-diff ()
  "`beemacs-dance-plan' pops a `beemacs-dance-plan-mode' buffer rendering the
dance's identity fields and a unified diff for each changed file."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url)
               (list 200 nil
                     (concat "{\"name\":\"repair-plan\",\"title\":\"Repair plan\","
                             "\"destructive\":true,\"reportOnly\":false,"
                             "\"plan\":{\"empty\":false,\"diffs\":"
                             "[{\"path\":\"PLAN.md\",\"before\":\"a\\n\",\"after\":\"b\\n\"}]}}")))))
    (unwind-protect
        (progn
          (beemacs-dance-plan "repair-plan")
          (with-current-buffer "*beemacs-dance-plan: repair-plan*"
            (should (derived-mode-p 'beemacs-dance-plan-mode))
            (should (equal beemacs-dance-plan--name "repair-plan"))
            (should buffer-read-only)
            (should (string-match-p "Name: repair-plan" (buffer-string)))
            (should (string-match-p "Destructive: yes" (buffer-string)))
            (should (string-match-p "\\+b" (buffer-string)))))
      (when (get-buffer "*beemacs-dance-plan: repair-plan*")
        (kill-buffer "*beemacs-dance-plan: repair-plan*")))))

(ert-deftest beemacs-test-dance-plan-empty-plan-renders-no-changes ()
  "`beemacs-dance-plan' renders \"(no changes)\" for an empty plan."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url)
               (list 200 nil
                     (concat "{\"name\":\"gc\",\"title\":\"GC\","
                             "\"destructive\":false,\"reportOnly\":false,"
                             "\"plan\":{\"empty\":true,\"diffs\":[]}}")))))
    (unwind-protect
        (progn
          (beemacs-dance-plan "gc")
          (with-current-buffer "*beemacs-dance-plan: gc*"
            (should (string-match-p "(no changes)" (buffer-string)))))
      (when (get-buffer "*beemacs-dance-plan: gc*")
        (kill-buffer "*beemacs-dance-plan: gc*")))))

(ert-deftest beemacs-test-dance-apply-reports-applied-result ()
  "`beemacs-dance-apply' on a non-destructive dance applies immediately (no
confirmation prompt) and reports the server's real result via `message'."
  (let (messages)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (list 200 nil
                       "{\"name\":\"gc\",\"result\":{\"removed\":3},\"plan\":{\"empty\":true,\"diffs\":[]}}")))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (beemacs-dance-apply "gc")
      (should (cl-some (lambda (m) (string-match-p "gc applied" m)) messages)))))

(ert-deftest beemacs-test-dance-apply-confirm-required-declined-does-not-mutate ()
  "`beemacs-dance-apply' on a destructive dance, when the user declines the
`yes-or-no-p' confirmation, never issues a second (mutating) apply call and
reports it was not applied."
  (let ((call-count 0) messages)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (cl-incf call-count)
                 (list 200 nil
                       "{\"confirmRequired\":true,\"error\":\"dance is destructive and requires explicit confirmation\"}")))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) nil))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (beemacs-dance-apply "repair-plan")
      (should (= call-count 1))
      (should (cl-some (lambda (m) (string-match-p "NOT applied" m)) messages)))))

(ert-deftest beemacs-test-dance-apply-confirm-required-accepted-reapplies-with-confirm ()
  "`beemacs-dance-apply' on a destructive dance, when the user accepts the
`yes-or-no-p' confirmation, re-issues the apply call with confirm=true and
reports the resulting applied outcome."
  (let (seen-datas messages)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (push url-request-data seen-datas)
                 (if (equal url-request-data (encode-coding-string "confirm=true" 'utf-8))
                     (list 200 nil
                           "{\"name\":\"repair-plan\",\"result\":{\"repaired\":true},\"plan\":{\"empty\":true,\"diffs\":[]}}")
                   (list 200 nil
                         "{\"confirmRequired\":true,\"error\":\"dance is destructive and requires explicit confirmation\"}"))))
              ((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
      (beemacs-dance-apply "repair-plan")
      (should (= (length seen-datas) 2))
      (should (cl-some (lambda (m) (string-match-p "repair-plan applied" m)) messages)))))

(ert-deftest beemacs-test-skills-plan-at-point-uses-row-name ()
  "`beemacs-skills-plan-at-point' reads the dance name from the current
tabulated-list row and delegates to `beemacs-dance-plan'."
  (let ((entries (beemacs-render-skill-rows
                   (vector '((Name . "gc") (Title . "GC") (Summary . "s")
                             (Destructive . :json-false) (ReportOnly . :json-false)))))
        seen-name)
    (cl-letf (((symbol-function 'beemacs-dance-plan)
               (lambda (name) (setq seen-name name))))
      (with-temp-buffer
        (beemacs-skills-mode)
        (setq tabulated-list-entries entries)
        (tabulated-list-print t)
        (goto-char (point-min))
        (beemacs-skills-plan-at-point)
        (should (equal seen-name "gc"))))))

(ert-deftest beemacs-test-api-stats-path ()
  "`beemacs-api-stats' hits the hive-wide stats.json endpoint (no submodule)."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil "{\"subs\":[],\"total\":{\"Name\":\"total\"}}"))))
      (let ((result (beemacs-api-stats)))
        (should (string-suffix-p "/stats.json" seen-url))
        (should (equal (alist-get 'subs result) []))
        (should (equal (alist-get 'Name (alist-get 'total result)) "total"))))))

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

(ert-deftest beemacs-test-api-secrets-set-submodule-scoped ()
  "`beemacs-api-secrets-set' POSTs key/value/submodule to `/secrets.json'."
  (let (seen-url seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url seen-data url-request-data)
                 (list 200 nil "{\"global\":[],\"submodules\":[]}"))))
      (beemacs-api-secrets-set "TOKEN" "s3cr3t" "beemacs")
      (should (string-suffix-p "/secrets.json" seen-url))
      (let ((decoded (json-parse-string (decode-coding-string seen-data 'utf-8)
                                         :object-type 'alist)))
        (should (equal (alist-get 'key decoded) "TOKEN"))
        (should (equal (alist-get 'value decoded) "s3cr3t"))
        (should (equal (alist-get 'submodule decoded) "beemacs"))))))

(ert-deftest beemacs-test-api-secrets-set-global-omits-submodule ()
  "`beemacs-api-secrets-set' omits `submodule' from the payload when nil."
  (let (seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (setq seen-data url-request-data)
                 (list 200 nil "{\"global\":[],\"submodules\":[]}"))))
      (beemacs-api-secrets-set "TOKEN" "s3cr3t")
      (let ((decoded (json-parse-string (decode-coding-string seen-data 'utf-8)
                                         :object-type 'alist)))
        (should (equal (alist-get 'key decoded) "TOKEN"))
        (should (equal (alist-get 'value decoded) "s3cr3t"))
        (should (null (alist-get 'submodule decoded)))))))

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
  "RET on [o] fetches roi.json and renders it as editable `beemacs-roi-edit-mode'
content (never read-only -- `C-c C-c' is the sanctioned publish path)."
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
            (should (derived-mode-p 'beemacs-roi-edit-mode))
            (should (equal beemacs-roi-edit--name "beemacs"))
            (should-not buffer-read-only)))
      (dolist (b '("*beemacs-submodule: beemacs*" "*beemacs-roi: beemacs*"))
        (when (get-buffer b) (kill-buffer b))))))

(ert-deftest beemacs-test-api-roi-set-posts-body-and-returns-body ()
  "`beemacs-api-roi-set' POSTs BODY as the `body' form field to
`/roi/{name}' and returns the raw response body on a 2xx."
  (let (seen-url seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url seen-data url-request-data)
                 (list 200 nil "<div>roi editor</div>"))))
      (should (equal (beemacs-api-roi-set "beemacs" "# new intent\n")
                     "<div>roi editor</div>"))
      (should (string-suffix-p "/roi/beemacs" seen-url))
      (should (equal (decode-coding-string seen-data 'utf-8)
                     "body=%23%20new%20intent%0A")))))

(ert-deftest beemacs-test-api-roi-set-surfaces-true-error-text ()
  "`beemacs-api-roi-set' surfaces the server's real `http.Error' body on
failure, never a synthesized generic message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 500 "permission denied")))
    (let ((err (should-error (beemacs-api-roi-set "beemacs" "x")
                              :type 'beemacs-api-error)))
      (should (string-match-p "permission denied" (cadr err))))))

(ert-deftest beemacs-test-roi-edit-publish-reports-true-success ()
  "`beemacs-roi-edit-publish' (`C-c C-c') sends the whole buffer to
`beemacs-api-roi-set' and echoes a message only after the backend
actually accepted the publish."
  (let (msg seen-name seen-body)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*beemacs-roi: beemacs*")
            (beemacs-roi-edit-mode)
            (setq beemacs-roi-edit--name "beemacs")
            (insert "# edited intent\n"))
          (cl-letf (((symbol-function 'beemacs-api-roi-set)
                     (lambda (name body)
                       (setq seen-name name seen-body body)
                       "<div>ok</div>"))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
            (with-current-buffer "*beemacs-roi: beemacs*"
              (beemacs-roi-edit-publish)))
          (should (equal seen-name "beemacs"))
          (should (equal seen-body "# edited intent\n"))
          (should (string-match-p "published" msg)))
      (when (get-buffer "*beemacs-roi: beemacs*")
        (kill-buffer "*beemacs-roi: beemacs*")))))

(ert-deftest beemacs-test-roi-edit-publish-reports-true-failure ()
  "`beemacs-roi-edit-publish' echoes the TRUE backend error text on
failure, never a generic \"done\" message."
  (let (msg)
    (unwind-protect
        (progn
          (with-current-buffer (get-buffer-create "*beemacs-roi: beemacs*")
            (beemacs-roi-edit-mode)
            (setq beemacs-roi-edit--name "beemacs")
            (insert "# bad intent\n"))
          (cl-letf (((symbol-function 'beemacs-api-roi-set)
                     (lambda (_name _body)
                       (signal 'beemacs-api-error '("permission denied (/roi/beemacs)"))))
                    ((symbol-function 'message)
                     (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
            (with-current-buffer "*beemacs-roi: beemacs*"
              (beemacs-roi-edit-publish)))
          (should (string-match-p "FAILED" msg))
          (should (string-match-p "permission denied" msg)))
      (when (get-buffer "*beemacs-roi: beemacs*")
        (kill-buffer "*beemacs-roi: beemacs*")))))

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

(ert-deftest beemacs-test-secrets-view-set-key-writes-and-refreshes ()
  "`beemacs-secrets-view-set-key' POSTs the entered key/value scoped to this
buffer's submodule, then re-renders the listing from the response."
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 "{\"global\":[],\"submodules\":[{\"name\":\"beemacs\",\"keys\":[\"OLD\"]}]}")))
          (beemacs-secrets-view "beemacs"))
        (let (seen-data)
          (cl-letf (((symbol-function 'beemacs-transport--call)
                     (lambda (_url)
                       (when (equal url-request-method "POST")
                         (setq seen-data url-request-data))
                       (list 200 nil
                             "{\"global\":[],\"submodules\":[{\"name\":\"beemacs\",\"keys\":[\"OLD\",\"NEW\"]}]}")))
                    ((symbol-function 'read-string) (lambda (&rest _) "NEW"))
                    ((symbol-function 'read-passwd) (lambda (&rest _) "s3cr3t")))
            (with-current-buffer "*beemacs-secrets: beemacs*"
              (beemacs-secrets-view-set-key)))
          (let ((decoded (json-parse-string (decode-coding-string seen-data 'utf-8)
                                             :object-type 'alist)))
            (should (equal (alist-get 'key decoded) "NEW"))
            (should (equal (alist-get 'value decoded) "s3cr3t"))
            (should (equal (alist-get 'submodule decoded) "beemacs"))))
        (with-current-buffer "*beemacs-secrets: beemacs*"
          (should (string-match-p "NEW" (buffer-string)))))
    (when (get-buffer "*beemacs-secrets: beemacs*")
      (kill-buffer "*beemacs-secrets: beemacs*"))))

(ert-deftest beemacs-test-submodule-view-sessions-reports-pending-gap ()
  "The sessions drill-in reports the pending sessions.json listing gap via
`message' when the user gives no branch name (an empty `read-string')."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (with-current-buffer "*beemacs-submodule: beemacs*"
          (let (msg)
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args) (setq msg (apply #'format fmt args))))
                      ((symbol-function 'read-string) (lambda (&rest _) "")))
              (beemacs-submodule-view-sessions))
            (should (string-match-p "sessions.json" msg))))
      (when (get-buffer "*beemacs-submodule: beemacs*")
        (kill-buffer "*beemacs-submodule: beemacs*")))))

(ert-deftest beemacs-test-submodule-view-sessions-opens-branch-when-given ()
  "The sessions drill-in opens `beemacs-session-view' when the user supplies
a branch name."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 "{\"subs\":[{\"Name\":\"beemacs\",\"State\":\"idle\"}]}")))
    (beemacs-submodule-view "beemacs")
    (unwind-protect
        (with-current-buffer "*beemacs-submodule: beemacs*"
          (let (opened-name opened-branch)
            (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "bee-foo"))
                      ((symbol-function 'beemacs-session-view)
                       (lambda (name branch)
                         (setq opened-name name opened-branch branch))))
              (beemacs-submodule-view-sessions))
            (should (equal opened-name "beemacs"))
            (should (equal opened-branch "bee-foo"))))
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

(ert-deftest beemacs-test-render-stat-rows ()
  "The render layer builds tabulated-list rows from a stats.json-shaped payload."
  (let ((subs (vector '((Name . "beemacs") (DeliveredTasks . 5) (Honeybees . 10)
                        (ActiveNow . 1) (Stranded . 0) (DeliveredPerBeePct . 50.0))
                       '((Name . "beehive") (DeliveredTasks . 2) (Honeybees . 8)
                         (ActiveNow . 0) (Stranded . 1) (DeliveredPerBeePct . 25.0)))))
    (should (equal (beemacs-render-stat-rows subs)
                   '(("beemacs" ["beemacs" "5" "10" "1" "0" "50.0%"])
                     ("beehive" ["beehive" "2" "8" "0" "1" "25.0%"]))))))

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

;; --- beemacs-session (live session-transcript buffer) tests ---

(defconst beemacs-test--session-pane-html-1
  "<div id=\"session-pane\"><div id=\"session-transcript\"><section>turn one</section></div></div>"
  "First transcript-pane frame used by the session-view tests below.")

(defconst beemacs-test--session-pane-html-2
  "<div id=\"session-pane\"><div id=\"session-transcript\"><section>turn one</section><section>turn two</section></div></div>"
  "Second (grown) transcript-pane frame used by the session-view tests below.")

(defun beemacs-test--session-view-open (name branch)
  "Open a `beemacs-session-view' buffer for NAME/BRANCH with `url-retrieve'
and `beemacs-sse-connect' mocked out, returning (BUFFER . FILTER) where
FILTER is the process filter installed on the connection -- tests feed it
raw SSE bytes directly to simulate server frames, exactly like
`beemacs-test-sse-connect-installs-filter-and-abort-cleans-up' does for the
lower-level primitive."
  (let* ((fake-buffer (generate-new-buffer " *beemacs-session-test*"))
         (fake-proc (make-pipe-process :name "beemacs-session-test"
                                        :buffer fake-buffer
                                        :noquery t
                                        :filter #'ignore)))
    (cl-letf (((symbol-function 'url-retrieve)
               (lambda (_url _callback &rest _) fake-buffer)))
      (let ((buf (beemacs-session-view name branch)))
        (cons buf (process-filter (beemacs-sse-connection-proc
                                    (with-current-buffer buf
                                      beemacs-session-view--conn))))))))

(ert-deftest beemacs-test-session-view-opens-and-renders-first-frame ()
  "`beemacs-session-view' opens a buffer named for NAME/BRANCH and renders
the first transcript-pane HTML frame as readable (shr-rendered) text."
  (let* ((opened (beemacs-test--session-view-open "beemacs" "bee-foo"))
         (buf (car opened))
         (filter (cdr opened)))
    (unwind-protect
        (progn
          (should (equal (buffer-name buf) "*beemacs-session: beemacs/bee-foo*"))
          (funcall filter 'fake-proc
                   (concat "HTTP/1.1 200 OK\r\n\r\ndata: "
                           beemacs-test--session-pane-html-1 "\n\n"))
          (with-current-buffer buf
            (should (string-match-p "turn one" (buffer-string)))
            (should-not (string-match-p "<div" (buffer-string)))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-session-view-auto-scrolls-when-point-at-end ()
  "When point sits at the end of the buffer, a new (grown) transcript frame
leaves point at the new end too -- the auto-scroll case."
  (let* ((opened (beemacs-test--session-view-open "beemacs" "bee-foo"))
         (buf (car opened))
         (filter (cdr opened)))
    (unwind-protect
        (progn
          (funcall filter 'fake-proc
                   (concat "HTTP/1.1 200 OK\r\n\r\ndata: "
                           beemacs-test--session-pane-html-1 "\n\n"))
          (with-current-buffer buf (goto-char (point-max)))
          (funcall filter 'fake-proc
                   (concat "data: " beemacs-test--session-pane-html-2 "\n\n"))
          (with-current-buffer buf
            (should (string-match-p "turn two" (buffer-string)))
            (should (= (point) (point-max)))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-session-view-preserves-position-when-point-moved-back ()
  "When the user has moved point back off the end, a new transcript frame
does not yank point back to the bottom -- it keeps the same distance from
the (new) end that point had from the (old) end."
  (let* ((opened (beemacs-test--session-view-open "beemacs" "bee-foo"))
         (buf (car opened))
         (filter (cdr opened)))
    (unwind-protect
        (progn
          (funcall filter 'fake-proc
                   (concat "HTTP/1.1 200 OK\r\n\r\ndata: "
                           beemacs-test--session-pane-html-1 "\n\n"))
          (with-current-buffer buf (goto-char (point-min)))
          (funcall filter 'fake-proc
                   (concat "data: " beemacs-test--session-pane-html-2 "\n\n"))
          (with-current-buffer buf
            (should (string-match-p "turn two" (buffer-string)))
            (should (= (point) (point-min)))))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-session-view-end-frame-aborts-connection ()
  "The server's \"end\" frame (an always-empty `data:' payload) aborts the
SSE connection and clears the buffer-local handle, leaking no process."
  (let* ((opened (beemacs-test--session-view-open "beemacs" "bee-foo"))
         (buf (car opened))
         (filter (cdr opened)))
    (unwind-protect
        (progn
          (funcall filter 'fake-proc
                   (concat "HTTP/1.1 200 OK\r\n\r\ndata: "
                           beemacs-test--session-pane-html-1 "\n\n"))
          (with-current-buffer buf
            (should beemacs-session-view--conn))
          (funcall filter 'fake-proc "event: end\ndata: \n\n")
          (with-current-buffer buf
            (should-not beemacs-session-view--conn)))
      (let (kill-buffer-query-functions) (kill-buffer buf)))))

(ert-deftest beemacs-test-session-view-kill-buffer-aborts-sse ()
  "Killing a `beemacs-session-view-mode' buffer aborts its SSE connection
(the underlying process is no longer live), never leaking it."
  (let* ((opened (beemacs-test--session-view-open "beemacs" "bee-foo"))
         (buf (car opened))
         (proc (with-current-buffer (car opened)
                 (beemacs-sse-connection-proc beemacs-session-view--conn))))
    (let (kill-buffer-query-functions) (kill-buffer buf))
    (should-not (process-live-p proc))))

(ert-deftest beemacs-test-stats-view-populates-rows ()
  "`beemacs-stats-view' fetches stats.json and lists each submodule."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil
                       (concat "{\"subs\":[{\"Name\":\"beemacs\",\"DeliveredTasks\":5,"
                               "\"Honeybees\":10,\"ActiveNow\":1,\"Stranded\":0,"
                               "\"DeliveredPerBeePct\":50.0}],"
                               "\"total\":{\"Name\":\"total\",\"DeliveredTasks\":5}}")))))
      (beemacs-stats-view)
      (unwind-protect
          (with-current-buffer "*beemacs-stats*"
            (should (string-suffix-p "/stats.json" seen-url))
            (should (derived-mode-p 'beemacs-stats-mode))
            (should (equal tabulated-list-entries
                            '(("beemacs" ["beemacs" "5" "10" "1" "0" "50.0%"]))))
            (should (equal (alist-get 'Name beemacs-stats--total) "total")))
        (when (get-buffer "*beemacs-stats*")
          (kill-buffer "*beemacs-stats*"))))))

(ert-deftest beemacs-test-stats-view-refresh-refetches ()
  "`beemacs-stats-refresh' re-fetches and redisplays entries."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"subs\":[],\"total\":{}}")))
    (beemacs-stats-view))
  (unwind-protect
      (with-current-buffer "*beemacs-stats*"
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 (concat "{\"subs\":[{\"Name\":\"beehive\",\"DeliveredTasks\":3,"
                                "\"Honeybees\":4,\"ActiveNow\":1,\"Stranded\":2,"
                                "\"DeliveredPerBeePct\":75.0}],\"total\":{}}"))))
          (beemacs-stats-refresh))
        (should (equal tabulated-list-entries
                        '(("beehive" ["beehive" "3" "4" "1" "2" "75.0%"])))))
    (when (get-buffer "*beemacs-stats*")
      (kill-buffer "*beemacs-stats*"))))

(ert-deftest beemacs-test-stats-view-open-at-point-shows-model-breakdown ()
  "RET in `beemacs-stats-mode' opens a read-only per-model breakdown buffer."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"subs\":[{\"Name\":\"beemacs\",\"DeliveredTasks\":5,"
                          "\"Honeybees\":10,\"ActiveNow\":1,\"Stranded\":0,"
                          "\"DeliveredPerBeePct\":50.0,"
                          "\"Models\":[{\"Model\":\"opus\",\"DeliveredTasks\":5,"
                          "\"Honeybees\":10,\"DeliveredPerBeePct\":50.0}]}],"
                          "\"total\":{}}"))))
    (beemacs-stats-view)
    (unwind-protect
        (progn
          (with-current-buffer "*beemacs-stats*"
            (goto-char (point-min))
            (beemacs-stats-open-at-point))
          (with-current-buffer "*beemacs-stats: beemacs*"
            (should (string-match-p "Name: beemacs" (buffer-string)))
            (should (string-match-p "opus" (buffer-string)))
            (should buffer-read-only)))
      (when (get-buffer "*beemacs-stats*") (kill-buffer "*beemacs-stats*"))
      (when (get-buffer "*beemacs-stats: beemacs*")
        (kill-buffer "*beemacs-stats: beemacs*")))))
(ert-deftest beemacs-test-transport-post-form-sets-method-and-body ()
  "`beemacs-transport-post-form' issues a POST with a URL-encoded form body
and the `application/x-www-form-urlencoded' content type."
  (let (seen-method seen-data seen-headers)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (setq seen-method url-request-method
                       seen-data url-request-data
                       seen-headers url-request-extra-headers)
                 (list 200 nil "ok"))))
      (beemacs-transport-post-form "/merge" '(("name" . "beemacs") ("branch" . "bee-foo")))
      (should (equal seen-method "POST"))
      (should (equal seen-data (encode-coding-string "name=beemacs&branch=bee-foo" 'utf-8)))
      (should (equal (alist-get "Content-Type" seen-headers nil nil #'equal)
                     "application/x-www-form-urlencoded")))))

(ert-deftest beemacs-test-transport-post-form-encodes-special-characters ()
  "`beemacs-transport-post-form' URL-encodes field values needing escaping."
  (let (seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url)
                 (setq seen-data url-request-data)
                 (list 200 nil "ok"))))
      (beemacs-transport-post-form "/merge" '(("name" . "a b") ("branch" . "bee/foo")))
      (should (equal seen-data
                     (encode-coding-string "name=a%20b&branch=bee%2Ffoo" 'utf-8))))))

(ert-deftest beemacs-test-api-merge-success-returns-body ()
  "`beemacs-api-merge' returns the raw response body on a 2xx `POST /merge'."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "<div>merge panel</div>")))
    (should (equal (beemacs-api-merge "beemacs" "bee-foo")
                   "<div>merge panel</div>"))))

(ert-deftest beemacs-test-api-merge-conflict-surfaces-true-error-text ()
  "`beemacs-api-merge' surfaces the real plain-text `http.Error' body (e.g.
\"merge conflict\") on a 409, never a synthesized generic message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 409 "merge conflict")))
    (let ((err (should-error (beemacs-api-merge "beemacs" "bee-foo")
                              :type 'beemacs-api-error)))
      (should (string-match-p "merge conflict" (cadr err))))))

(ert-deftest beemacs-test-api-merge-server-error-surfaces-git-error-text ()
  "A 500 `POST /merge' failure surfaces the actual wrapped git error text."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 500 "exit status 128: fatal: no such branch")))
    (let ((err (should-error (beemacs-api-merge "beemacs" "bee-foo")
                              :type 'beemacs-api-error)))
      (should (string-match-p "fatal: no such branch" (cadr err))))))

(ert-deftest beemacs-test-api-merge-connection-failure-signals ()
  "A connection failure during `beemacs-api-merge' still signals
`beemacs-api-error' (never assumed success)."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (should-error (beemacs-api-merge "beemacs" "bee-foo") :type 'beemacs-api-error)))

(ert-deftest beemacs-test-merge-command-reports-true-success ()
  "`beemacs-merge' echoes a message only after `beemacs-api-merge' actually
succeeds (no assumed-success message on failure)."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-merge) (lambda (_n _b) "ok"))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-merge "beemacs" "bee-foo")
      (should (string-match-p "merged into" msg)))))

(ert-deftest beemacs-test-merge-command-reports-true-failure ()
  "`beemacs-merge' echoes the TRUE backend error text on failure, never a
generic \"done\" message."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-merge)
               (lambda (_n _b) (signal 'beemacs-api-error '("merge conflict (/merge)"))))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-merge "beemacs" "bee-foo")
      (should (string-match-p "FAILED" msg))
      (should (string-match-p "merge conflict" msg)))))

;;; Fleet management tests (submodule add/link/set-remote)

(ert-deftest beemacs-test-api-submodule-add-encodes-all-fields ()
  "`beemacs-api-submodule-add' POSTs `url', `name', and `branch' as
form-urlencoded fields to `/submodule/add' when all three are given."
  (let (seen-path seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-path url
                       seen-data url-request-data)
                 (list 200 nil "ok"))))
      (beemacs-api-submodule-add "https://example.com/x.git" "x" "main")
      (should (string-match-p "/submodule/add\\'" seen-path))
      (should (equal seen-data
                     (encode-coding-string
                      "url=https%3A%2F%2Fexample.com%2Fx.git&name=x&branch=main"
                      'utf-8))))))

(ert-deftest beemacs-test-api-submodule-add-omits-blank-optional-fields ()
  "`beemacs-api-submodule-add' omits `name'/`branch' entirely when not given,
mirroring beehived's derive-from-URL/no-branch defaults."
  (let (seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (_url) (setq seen-data url-request-data) (list 200 nil "ok"))))
      (beemacs-api-submodule-add "https://example.com/x.git")
      (should (equal seen-data
                     (encode-coding-string "url=https%3A%2F%2Fexample.com%2Fx.git" 'utf-8))))))

(ert-deftest beemacs-test-api-submodule-add-success-returns-body ()
  "`beemacs-api-submodule-add' returns the raw response body on a 2xx
`POST /submodule/add'."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "<div>dashboard</div>")))
    (should (equal (beemacs-api-submodule-add "https://example.com/x.git")
                   "<div>dashboard</div>"))))

(ert-deftest beemacs-test-api-submodule-add-conflict-surfaces-true-error-text ()
  "`beemacs-api-submodule-add' surfaces the real plain-text `http.Error'
body on a 409 (name already exists), never a synthesized message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 409 "submodule already exists")))
    (let ((err (should-error (beemacs-api-submodule-add "https://example.com/x.git" "x")
                              :type 'beemacs-api-error)))
      (should (string-match-p "submodule already exists" (cadr err))))))

(ert-deftest beemacs-test-api-submodule-add-connection-failure-signals ()
  "A connection failure during `beemacs-api-submodule-add' still signals
`beemacs-api-error' (never assumed success)."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (should-error (beemacs-api-submodule-add "https://example.com/x.git")
                  :type 'beemacs-api-error)))

(ert-deftest beemacs-test-api-submodule-link-encodes-from-to ()
  "`beemacs-api-submodule-link' POSTs `from'/`to' as form-urlencoded
fields to `/submodule/link'."
  (let (seen-path seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-path url
                       seen-data url-request-data)
                 (list 200 nil "ok"))))
      (beemacs-api-submodule-link "alpha" "beta")
      (should (string-match-p "/submodule/link\\'" seen-path))
      (should (equal seen-data (encode-coding-string "from=alpha&to=beta" 'utf-8))))))

(ert-deftest beemacs-test-api-submodule-link-success-returns-body ()
  "`beemacs-api-submodule-link' returns the raw response body on a 2xx
`POST /submodule/link'."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "<div>links</div>")))
    (should (equal (beemacs-api-submodule-link "alpha" "beta") "<div>links</div>"))))

(ert-deftest beemacs-test-api-submodule-link-cycle-surfaces-true-error-text ()
  "`beemacs-api-submodule-link' surfaces the real plain-text `http.Error'
body on a 409 (would form a wait-cycle), never a synthesized message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 409 "dependency would form a cycle")))
    (let ((err (should-error (beemacs-api-submodule-link "alpha" "beta")
                              :type 'beemacs-api-error)))
      (should (string-match-p "cycle" (cadr err))))))

(ert-deftest beemacs-test-api-submodule-link-connection-failure-signals ()
  "A connection failure during `beemacs-api-submodule-link' still signals
`beemacs-api-error' (never assumed success)."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (should-error (beemacs-api-submodule-link "alpha" "beta") :type 'beemacs-api-error)))

(ert-deftest beemacs-test-api-submodule-set-remote-encodes-url-to-name-path ()
  "`beemacs-api-submodule-set-remote' POSTs `url' to
`/submodule/{name}/remote', with NAME in the path, not the form body."
  (let (seen-path seen-data)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-path url
                       seen-data url-request-data)
                 (list 200 nil "ok"))))
      (beemacs-api-submodule-set-remote "beemacs" "https://example.com/new.git")
      (should (string-match-p "/submodule/beemacs/remote\\'" seen-path))
      (should (equal seen-data
                     (encode-coding-string "url=https%3A%2F%2Fexample.com%2Fnew.git" 'utf-8))))))

(ert-deftest beemacs-test-api-submodule-set-remote-success-returns-body ()
  "`beemacs-api-submodule-set-remote' returns the raw response body on a
2xx `POST /submodule/{name}/remote'."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "<div>roi editor</div>")))
    (should (equal (beemacs-api-submodule-set-remote "beemacs" "https://example.com/new.git")
                   "<div>roi editor</div>"))))

(ert-deftest beemacs-test-api-submodule-set-remote-not-found-surfaces-true-error-text ()
  "`beemacs-api-submodule-set-remote' surfaces the real plain-text
`http.Error' body on a 404 (unknown submodule), never a synthesized
message."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 404 "no such submodule")))
    (let ((err (should-error
                (beemacs-api-submodule-set-remote "nope" "https://example.com/new.git")
                :type 'beemacs-api-error)))
      (should (string-match-p "no such submodule" (cadr err))))))

(ert-deftest beemacs-test-api-submodule-set-remote-connection-failure-signals ()
  "A connection failure during `beemacs-api-submodule-set-remote' still
signals `beemacs-api-error' (never assumed success)."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (lambda (_url) (signal 'beemacs-http-error (list "connection refused")))))
    (should-error (beemacs-api-submodule-set-remote "beemacs" "https://example.com/new.git")
                  :type 'beemacs-api-error)))

(ert-deftest beemacs-test-submodule-add-command-reports-true-success ()
  "`beemacs-submodule-add' echoes a message only after
`beemacs-api-submodule-add' actually succeeds (no assumed-success
message on failure)."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-add)
               (lambda (_url _name _branch) "ok"))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-add "https://example.com/x.git" "x" nil)
      (should (string-match-p "registered" msg)))))

(ert-deftest beemacs-test-submodule-add-command-reports-true-failure ()
  "`beemacs-submodule-add' echoes the TRUE backend error text on
failure, never a generic \"done\" message."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-add)
               (lambda (_url _name _branch)
                 (signal 'beemacs-api-error '("submodule already exists (/submodule/add)"))))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-add "https://example.com/x.git" "x" nil)
      (should (string-match-p "FAILED" msg))
      (should (string-match-p "already exists" msg)))))

(ert-deftest beemacs-test-submodule-link-command-reports-true-success ()
  "`beemacs-submodule-link' echoes a message only after
`beemacs-api-submodule-link' actually succeeds."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-link)
               (lambda (_from _to) "ok"))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-link "alpha" "beta")
      (should (string-match-p "registered" msg)))))

(ert-deftest beemacs-test-submodule-link-command-reports-true-failure ()
  "`beemacs-submodule-link' echoes the TRUE backend error text on
failure, never a generic \"done\" message."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-link)
               (lambda (_from _to)
                 (signal 'beemacs-api-error '("dependency would form a cycle (/submodule/link)"))))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-link "alpha" "beta")
      (should (string-match-p "FAILED" msg))
      (should (string-match-p "cycle" msg)))))

(ert-deftest beemacs-test-submodule-set-remote-command-reports-true-success ()
  "`beemacs-submodule-set-remote' echoes a message only after
`beemacs-api-submodule-set-remote' actually succeeds."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-set-remote)
               (lambda (_name _url) "ok"))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-set-remote "beemacs" "https://example.com/new.git")
      (should (string-match-p "remote set" msg)))))

(ert-deftest beemacs-test-submodule-set-remote-command-reports-true-failure ()
  "`beemacs-submodule-set-remote' echoes the TRUE backend error text on
failure, never a generic \"done\" message."
  (let (msg)
    (cl-letf (((symbol-function 'beemacs-api-submodule-set-remote)
               (lambda (_name _url)
                 (signal 'beemacs-api-error '("no such submodule (/submodule/nope/remote)"))))
              ((symbol-function 'message) (lambda (fmt &rest args)
                                             (setq msg (apply #'format fmt args)))))
      (beemacs-submodule-set-remote "nope" "https://example.com/new.git")
      (should (string-match-p "FAILED" msg))
      (should (string-match-p "no such submodule" msg)))))

;;; beemacs-human tests

(ert-deftest beemacs-test-render-human-rows ()
  "The render layer builds tabulated-list rows from a human.json-shaped payload."
  (let ((tasks (vector '((sub . "jellyfin") (id . "t1") (desc . "need a secret")
                         (deps . ["a" "b"]) (reason . "need X")
                         (category . "secret"))
                       '((sub . "beemacs") (id . "t2") (desc . "arch call")
                         (deps . []) (reason . "pick a wire format")
                         (category . "architecture")))))
    (should (equal (beemacs-render-human-rows tasks)
                   `((("jellyfin" . "t1")
                      ["jellyfin" "t1" "need a secret" "secret" "need X"])
                     (("beemacs" . "t2")
                      ["beemacs" "t2" "arch call" "architecture" "pick a wire format"]))))))

(ert-deftest beemacs-test-human-list-populates-rows ()
  "`beemacs-human-list' fetches human.json and lists every NEEDS-HUMAN task."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url) (setq seen-url url)
                 (list 200 nil
                       (concat "{\"tasks\":[{\"sub\":\"jellyfin\",\"id\":\"t1\","
                               "\"desc\":\"need a secret\",\"deps\":[],"
                               "\"reason\":\"need X\",\"category\":\"secret\"}]}")))))
      (beemacs-human-list)
      (unwind-protect
          (with-current-buffer "*beemacs-human*"
            (should (string-suffix-p "/human.json" seen-url))
            (should (derived-mode-p 'beemacs-human-list-mode))
            (should (equal tabulated-list-entries
                            '((("jellyfin" . "t1")
                               ["jellyfin" "t1" "need a secret" "secret" "need X"])))))
        (when (get-buffer "*beemacs-human*")
          (kill-buffer "*beemacs-human*"))))))

(ert-deftest beemacs-test-human-list-refresh-refetches ()
  "`beemacs-human-list-refresh' re-fetches and redisplays entries."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call 200 "{\"tasks\":[]}")))
    (beemacs-human-list))
  (unwind-protect
      (with-current-buffer "*beemacs-human*"
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 (concat "{\"tasks\":[{\"sub\":\"beehive\",\"id\":\"t9\","
                                "\"desc\":\"d\",\"deps\":[],\"reason\":\"r\","
                                "\"category\":\"contradiction\"}]}"))))
          (beemacs-human-list-refresh))
        (should (equal tabulated-list-entries
                        '((("beehive" . "t9")
                           ["beehive" "t9" "d" "contradiction" "r"])))))
    (when (get-buffer "*beemacs-human*")
      (kill-buffer "*beemacs-human*"))))

(ert-deftest beemacs-test-human-list-open-at-point-opens-resolve-view ()
  "RET in `beemacs-human-list-mode' opens the resolution workspace for the
task at point."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--mock-call
              200 (concat "{\"tasks\":[{\"sub\":\"jellyfin\",\"id\":\"t1\","
                          "\"desc\":\"d\",\"deps\":[],\"reason\":\"r\","
                          "\"category\":\"secret\"}]}"))))
    (beemacs-human-list))
  (unwind-protect
      (progn
        (with-current-buffer "*beemacs-human*"
          (goto-char (point-min))
          (let (opened-sub opened-id)
            (cl-letf (((symbol-function 'beemacs-human-resolve-view)
                       (lambda (sub id) (setq opened-sub sub opened-id id))))
              (beemacs-human-list-open-at-point))
            (should (equal opened-sub "jellyfin"))
            (should (equal opened-id "t1")))))
    (when (get-buffer "*beemacs-human*")
      (kill-buffer "*beemacs-human*"))))

(defun beemacs-test--human-mock-call (responses)
  "Return a mock `beemacs-transport--call' dispatching on URL SUFFIX.

RESPONSES is an alist of (SUFFIX . BODY); the first entry whose SUFFIX
the requested URL ends with supplies the 200 JSON BODY returned. Errors
if no entry matches, so a test never silently mis-mocks an unexpected
call."
  (lambda (url)
    (let ((hit (cl-find-if (lambda (r) (string-suffix-p (car r) url)) responses)))
      (unless hit
        (error "beemacs-test--human-mock-call: no mock for %s" url))
      (list 200 nil (cdr hit)))))

(ert-deftest beemacs-test-human-resolve-view-opens-and-renders-context ()
  "`beemacs-human-resolve-view' fetches task context + opens a session and
renders both the static context and the live transcript."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"need a secret\",\"body\":\"full body\",\"deps\":[],\"reason\":\"need X\",\"category\":\"secret\",\"has_session\":false}")
                ("/session" .
                 "{\"sid\":\"h1\",\"Log\":[],\"Stat\":\"\",\"HasChange\":false,\"Busy\":false,\"Published\":false}")))))
    (unwind-protect
        (progn
          (beemacs-human-resolve-view "jellyfin" "t1")
          (with-current-buffer "*beemacs-human: jellyfin/t1*"
            (should (derived-mode-p 'beemacs-human-resolve-mode))
            (should (equal beemacs-human-resolve--sub "jellyfin"))
            (should (equal beemacs-human-resolve--id "t1"))
            (should (equal beemacs-human-resolve--sid "h1"))
            (should (string-match-p "need a secret" (buffer-string)))
            (should (string-match-p "full body" (buffer-string)))
            (should (string-match-p "no messages yet" (buffer-string)))))
      (when (get-buffer "*beemacs-human: jellyfin/t1*")
        (kill-buffer "*beemacs-human: jellyfin/t1*")))))

(ert-deftest beemacs-test-human-resolve-message-appends-transcript ()
  "`beemacs-human-resolve-message' sends a message and renders the resolver's
real reply in the transcript."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h2\",\"Log\":[],\"Stat\":\"\",\"HasChange\":false,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (cl-letf (((symbol-function 'beemacs-transport--call)
                 (beemacs-test--mock-call
                  200 (concat "{\"Log\":[{\"role\":\"user\",\"text\":\"please help\"},"
                              "{\"role\":\"agent\",\"text\":\"working on it\"}],"
                              "\"Stat\":\"1 file changed\",\"HasChange\":true,"
                              "\"Busy\":false,\"Published\":false}"))))
        (with-current-buffer "*beemacs-human: jellyfin/t1*"
          (beemacs-human-resolve-message "please help")
          (should (string-match-p "please help" (buffer-string)))
          (should (string-match-p "working on it" (buffer-string)))
          (should (string-match-p "1 file changed" (buffer-string)))
          (should (string-match-p "has-change" (buffer-string)))))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

(ert-deftest beemacs-test-human-resolve-publish-reports-published-state ()
  "`beemacs-human-resolve-publish' publishes and reflects the returned
Published state in the transcript."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h3\",\"Log\":[],\"Stat\":\"\",\"HasChange\":true,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (cl-letf (((symbol-function 'beemacs-transport--call)
                 (beemacs-test--mock-call
                  200 "{\"Log\":[],\"Stat\":\"\",\"HasChange\":true,\"Busy\":false,\"Published\":true}")))
        (with-current-buffer "*beemacs-human: jellyfin/t1*"
          (beemacs-human-resolve-publish)
          (should (string-match-p "published" (buffer-string)))))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

(ert-deftest beemacs-test-human-resolve-publish-surfaces-embedded-error ()
  "`beemacs-human-resolve-publish' surfaces the panel's embedded `error' key
via `message' when the server reports a publish failure."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h4\",\"Log\":[],\"Stat\":\"\",\"HasChange\":true,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (let (msg)
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--mock-call
                    200 "{\"error\":\"publish conflict\",\"Log\":[],\"HasChange\":true}"))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
          (with-current-buffer "*beemacs-human: jellyfin/t1*"
            (beemacs-human-resolve-publish)))
        (should (string-match-p "publish conflict" msg)))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

(ert-deftest beemacs-test-human-resolve-discard-adopts-new-session ()
  "`beemacs-human-resolve-discard' adopts the fresh session id the server
returns for a still-blocked task."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h5\",\"Log\":[],\"Stat\":\"\",\"HasChange\":true,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (progn
        (cl-letf (((symbol-function 'beemacs-transport--call)
                   (beemacs-test--human-mock-call
                    '(("/discard/h5" . "{\"sid\":\"h6\"}")
                      ("/panel/h6" .
                       "{\"Log\":[],\"Stat\":\"\",\"HasChange\":false,\"Busy\":false,\"Published\":false}")))))
          (with-current-buffer "*beemacs-human: jellyfin/t1*"
            (beemacs-human-resolve-discard)
            (should (equal beemacs-human-resolve--sid "h6")))))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

(ert-deftest beemacs-test-human-resolve-apply-confirms-and-calls-resolve ()
  "`beemacs-human-resolve-apply' confirms via `yes-or-no-p' then calls the
sanctioned resolve endpoint, never a direct PLAN.md write."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h7\",\"Log\":[],\"Stat\":\"\",\"HasChange\":false,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (let (called-sub called-id)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                  ((symbol-function 'beemacs-api-human-resolve)
                   (lambda (sub id) (setq called-sub sub called-id id))))
          (with-current-buffer "*beemacs-human: jellyfin/t1*"
            (beemacs-human-resolve-apply)))
        (should (equal called-sub "jellyfin"))
        (should (equal called-id "t1")))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

(ert-deftest beemacs-test-human-resolve-apply-declines-without-confirmation ()
  "`beemacs-human-resolve-apply' never calls the resolve endpoint when the
user declines confirmation."
  (cl-letf (((symbol-function 'beemacs-transport--call)
             (beemacs-test--human-mock-call
              '(("/human.json/jellyfin/t1" .
                 "{\"sub\":\"jellyfin\",\"id\":\"t1\",\"desc\":\"d\",\"body\":\"\",\"deps\":[],\"reason\":\"r\",\"category\":\"secret\"}")
                ("/session" .
                 "{\"sid\":\"h8\",\"Log\":[],\"Stat\":\"\",\"HasChange\":false,\"Busy\":false,\"Published\":false}")))))
    (beemacs-human-resolve-view "jellyfin" "t1"))
  (unwind-protect
      (let (called)
        (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) nil))
                  ((symbol-function 'beemacs-api-human-resolve)
                   (lambda (&rest _) (setq called t))))
          (with-current-buffer "*beemacs-human: jellyfin/t1*"
            (beemacs-human-resolve-apply)))
        (should-not called))
    (when (get-buffer "*beemacs-human: jellyfin/t1*")
      (kill-buffer "*beemacs-human: jellyfin/t1*"))))

;;; beemacs-transient / shared keymap tests

(ert-deftest beemacs-test-menu-is-a-transient-prefix ()
  "`beemacs-menu' is defined as a real `transient' prefix command, not a
placeholder."
  (should (fboundp 'beemacs-menu))
  (should (get 'beemacs-menu 'transient--prefix)))

(ert-deftest beemacs-test-transient-menu-degrades-gracefully-without-transient ()
  "When `transient' is absent or too old to provide
`transient-define-prefix', loading `beemacs-transient' in a FRESH Emacs
process must NOT error, `beemacs-transient--available-p' must be nil, and
`beemacs-menu' must still be `fboundp' but signal a clear `user-error'
instead of a void-function/unbound-variable at call time.

Exercised out-of-process (a genuinely separate `emacs -Q --batch') against
a fake `transient.el' stub that shadows the real one by load-path order
and merely `(provide \\='transient)'s without defining
`transient-define-prefix' -- simulating the mismatched-version case,
since this test's own Emacs already has a working `transient' loaded and
cannot un-require it in-process."
  (let* ((this-dir (file-name-directory (locate-library "beemacs-transient")))
         (stub-dir (make-temp-file "beemacs-transient-stub-" t))
         (stub-file (expand-file-name "transient.el" stub-dir)))
    (unwind-protect
        (progn
          (with-temp-file stub-file
            (insert ";;; transient.el --- fake mismatched stub -*- lexical-binding: t; -*-\n"
                    "(provide 'transient)\n"))
          (let* ((form
                  `(progn
                     (require 'beemacs-transient)
                     (prin1
                      (list beemacs-transient--available-p
                            (fboundp 'beemacs-menu)
                            (condition-case err
                                (progn (beemacs-menu) 'no-error)
                              (user-error (list 'user-error (cadr err)))
                              (error (list 'other-error (error-message-string err))))))))
                 (result
                  (with-temp-buffer
                    (let ((status
                           (call-process
                            (concat invocation-directory invocation-name) nil t nil
                            "-Q" "--batch"
                            "-L" stub-dir "-L" this-dir
                            "--eval" (prin1-to-string form))))
                      (should (eq status 0))
                      (goto-char (point-min))
                      (read (current-buffer)))))
                 (available-p (nth 0 result))
                 (menu-bound-p (nth 1 result))
                 (call-outcome (nth 2 result)))
            (should-not available-p)
            (should menu-bound-p)
            (should (consp call-outcome))
            (should (eq (car call-outcome) 'user-error))
            (should (string-match-p "unavailable" (cadr call-outcome)))))
      (delete-directory stub-dir t))))

(ert-deftest beemacs-test-shared-dispatch-refresh-calls-mode-specific-command ()
  "`beemacs-shared-refresh' resolves to the current major mode's OWN
refresh command via the dispatch table -- it never reimplements it."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-dashboard*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-dashboard-mode)
          (let (called)
            (cl-letf (((symbol-function 'beemacs-dashboard-refresh)
                       (lambda () (interactive) (setq called t))))
              (beemacs-shared-refresh))
            (should called)))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-drill-in-calls-mode-specific-command ()
  "`beemacs-shared-drill-in' resolves to the current major mode's OWN
drill-in command."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-skills*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-skills-mode)
          (let (called)
            (cl-letf (((symbol-function 'beemacs-skills-open-at-point)
                       (lambda () (interactive) (setq called t))))
              (beemacs-shared-drill-in))
            (should called)))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-act-calls-mode-specific-command ()
  "`beemacs-shared-act' resolves to the current major mode's OWN act
command (e.g. dance-plan apply)."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-dance-plan*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-dance-plan-mode)
          (let (called)
            (cl-letf (((symbol-function 'beemacs-dance-plan-apply)
                       (lambda () (interactive) (setq called t))))
              (beemacs-shared-act))
            (should called)))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-stream-calls-mode-specific-command ()
  "`beemacs-shared-stream' resolves to `beemacs-submodule-view-sessions' in
`beemacs-submodule-view-mode'."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-submodule*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-submodule-view-mode)
          (let (called)
            (cl-letf (((symbol-function 'beemacs-submodule-view-sessions)
                       (lambda () (interactive) (setq called t))))
              (beemacs-shared-stream))
            (should called)))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-abort-calls-mode-specific-command ()
  "`beemacs-shared-abort' resolves to `beemacs-human-resolve-discard' in
`beemacs-human-resolve-mode'."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-human-resolve*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-human-resolve-mode)
          (let (called)
            (cl-letf (((symbol-function 'beemacs-human-resolve-discard)
                       (lambda () (interactive) (setq called t))))
              (beemacs-shared-abort))
            (should called)))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-signals-when-verb-unsupported ()
  "A verb absent from the current major mode's dispatch entry signals a
real `user-error', never a silent no-op."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-stats*")))
    (unwind-protect
        (with-current-buffer buf
          (beemacs-stats-mode)
          (should-error (beemacs-shared-act) :type 'user-error))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-dispatch-signals-for-unmapped-major-mode ()
  "A major mode with no dispatch-table entry at all also signals
`user-error' rather than silently doing nothing."
  (let ((buf (generate-new-buffer "*beemacs-shared-test-fundamental*")))
    (unwind-protect
        (with-current-buffer buf
          (fundamental-mode)
          (should-error (beemacs-shared-refresh) :type 'user-error))
      (kill-buffer buf))))

(ert-deftest beemacs-test-shared-mode-auto-enabled-in-beemacs-major-modes ()
  "`beemacs-shared-mode' is turned on automatically by every beemacs major
mode's own mode hook, so its keymap is present without any manual step."
  (dolist (mode '(beemacs-dashboard-mode
                  beemacs-docs-mode
                  beemacs-branches-mode
                  beemacs-plan-mode
                  beemacs-submodule-view-mode
                  beemacs-secrets-view-mode
                  beemacs-skills-mode
                  beemacs-dance-plan-mode
                  beemacs-human-list-mode
                  beemacs-stats-mode))
    (let ((buf (generate-new-buffer (format "*beemacs-shared-auto-%s*" mode))))
      (unwind-protect
          (with-current-buffer buf
            (funcall mode)
            (should beemacs-shared-mode))
        (kill-buffer buf)))))

(ert-deftest beemacs-test-shared-keymap-uses-c-c-prefix-only ()
  "Every binding in `beemacs-shared-mode-map' lives under the `C-c' prefix
so it layers over, and never shadows, a mode's own single-letter
bindings -- including an editable buffer like `beemacs-roi-edit-mode'
where a bare letter must still self-insert."
  (map-keymap
   (lambda (event _binding)
     (should (eq event ?\C-c)))
   beemacs-shared-mode-map))

;;; live-integration tests (explicit tag; skipped in the default batch run)

;; These two tests exercise beemacs against REAL external processes -- a
;; running `beehived' HTTP server and a real `pi' agent process -- rather
;; than the mocked transport/process fixtures used everywhere else in this
;; suite. Both are tagged `:live-integration' so they are selectable on
;; their own (`(ert-run-tests-batch-and-exit \\='(tag :live-integration))'),
;; and BOTH additionally self-skip via `ert-skip' unless their required
;; environment variable is set -- so the default, untagged
;; `ert-run-tests-batch-and-exit' batch run (this task's own `Check:', and
;; CI's default job) never spends real LLM tokens or requires a live
;; `beehived' just to pass. Run them explicitly with the env var(s) set:
;;
;;   BEEMACS_LIVE_BEEHIVED_URL=http://127.0.0.1:8080 \
;;     emacs -Q --batch -L . -l ert -l beemacs-tests.el \
;;       --eval "(ert-run-tests-batch-and-exit '(tag :live-integration))"
;;
;;   BEEMACS_LIVE_PI_BIN=pi \
;;     emacs -Q --batch -L . -l ert -l beemacs-tests.el \
;;       --eval "(ert-run-tests-batch-and-exit '(tag :live-pi))"

(ert-deftest beemacs-test-live-integration-beehived-dashboard ()
  :tags '(:live-integration)
  "Against a REAL running `beehived' (named by
`BEEMACS_LIVE_BEEHIVED_URL'), `beemacs-api-dashboard' round-trips a real
HTTP GET and returns a parsed dashboard alist with the expected shape.
Skipped (not failed) when the env var is unset, so the default batch run
never depends on a live server."
  (let ((url (getenv "BEEMACS_LIVE_BEEHIVED_URL")))
    (unless url
      (ert-skip "BEEMACS_LIVE_BEEHIVED_URL not set; skipping live beehived integration test"))
    (let* ((beemacs-endpoint url)
           (result (beemacs-api-dashboard)))
      (should (listp result))
      (should (assq 'subs result))
      (should (vectorp (alist-get 'subs result))))))

(ert-deftest beemacs-test-live-integration-pi-round-trip ()
  :tags '(:live-pi)
  "Against a REAL `pi' executable (named by `BEEMACS_LIVE_PI_BIN'), spawn
an RPC child via `beemacs-pi-start', send one request, and confirm a
reply arrives and the process shuts down cleanly via `beemacs-pi-stop'.
Skipped (not failed) when the env var is unset -- this is the one test in
the suite that spends real LLM tokens, so it never runs in the default
batch invocation."
  (let ((pi-bin (getenv "BEEMACS_LIVE_PI_BIN")))
    (unless pi-bin
      (ert-skip "BEEMACS_LIVE_PI_BIN not set; skipping live pi integration test"))
    (let* ((beemacs-pi-executable pi-bin)
           (replies nil)
           (handle (beemacs-pi-start (lambda (msg) (push msg replies)))))
      (unwind-protect
          (progn
            (should (beemacs-pi-alive-p handle))
            (beemacs-pi-send handle '((type . "ping")))
            (beemacs-test--wait-for (lambda () replies) 30)
            (should replies))
        (beemacs-pi-stop handle)
        (should-not (beemacs-pi-alive-p handle))))))

(provide 'beemacs-tests)

;;; beemacs-tests.el ends here
