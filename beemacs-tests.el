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

(defun beemacs-test--plan-fixture ()
  "A minimal `beemacs-api-plan'-shaped payload for plan-view tests.

Mirrors GET /submodule/{name}/plan.json's shape (internal/web.Plan/
PlanItem, JSON-decoded via `beemacs-api--parse-json': alists/vectors,
booleans as `t'/`:json-false')."
  '((name . "beemacs")
    (plan . ((ROIStamp . "abc123")
             (Items
              . [((ID . "beemacs-plan-view")
                  (Status . "TODO")
                  (Weight . 4)
                  (Deps . ["beemacs-api-contract" "beehive:beemacs-json-api"])
                  (Active . t)
                  (Stale . :json-false)
                  (Session . "beemacs-1")
                  (DocHref . "/submodule/beemacs/doc/foo.md")
                  (SessionHref . ""))
                 ((ID . "beemacs-transport")
                  (Status . "DONE")
                  (Weight . 3)
                  (Deps . [])
                  (Active . :json-false)
                  (Stale . :json-false)
                  (Session . "")
                  (DocHref . "")
                  (SessionHref . "/submodule/beemacs/session/xyz"))
                 ((ID . "beemacs-idle")
                  (Status . "TODO")
                  (Weight . 1)
                  (Deps . [])
                  (Active . :json-false)
                  (Stale . t)
                  (Session . "stale-owner")
                  (DocHref . "")
                  (SessionHref . ""))])))))

(ert-deftest beemacs-test-render-plan-item-claim-active ()
  "An active claim renders as \"active <session>\"."
  (let* ((data (beemacs-test--plan-fixture))
         (item (beemacs-render-plan-find-item data "beemacs-plan-view")))
    (should (equal (beemacs-render-plan-item-claim item) "active beemacs-1"))))

(ert-deftest beemacs-test-render-plan-item-claim-stale ()
  "A stale (TTL-expired) claim renders as \"stale <session>\"."
  (let* ((data (beemacs-test--plan-fixture))
         (item (beemacs-render-plan-find-item data "beemacs-idle")))
    (should (equal (beemacs-render-plan-item-claim item) "stale stale-owner"))))

(ert-deftest beemacs-test-render-plan-item-claim-unclaimed ()
  "An unclaimed task (neither active nor stale) renders as \"\"."
  (let* ((data (beemacs-test--plan-fixture))
         (item (beemacs-render-plan-find-item data "beemacs-transport")))
    (should (equal (beemacs-render-plan-item-claim item) ""))))

(ert-deftest beemacs-test-render-plan-item-deps ()
  "Deps render as a comma-joined string, empty string when there are none."
  (let* ((data (beemacs-test--plan-fixture)))
    (should (equal (beemacs-render-plan-item-deps
                    (beemacs-render-plan-find-item data "beemacs-plan-view"))
                   "beemacs-api-contract,beehive:beemacs-json-api"))
    (should (equal (beemacs-render-plan-item-deps
                    (beemacs-render-plan-find-item data "beemacs-transport"))
                   ""))))

(ert-deftest beemacs-test-render-plan-rows ()
  "`beemacs-render-plan-rows' shapes each item into a tabulated-list entry."
  (let* ((data (beemacs-test--plan-fixture))
         (rows (beemacs-render-plan-rows data)))
    (should (equal (length rows) 3))
    (should (equal (car (nth 0 rows)) "beemacs-plan-view"))
    (should (equal (cadr (nth 0 rows))
                    ["beemacs-plan-view" "TODO" "4"
                     "beemacs-api-contract,beehive:beemacs-json-api"
                     "active beemacs-1"]))
    (should (equal (cadr (nth 1 rows))
                    ["beemacs-transport" "DONE" "3" "" ""]))))

(ert-deftest beemacs-test-render-plan-find-item ()
  "`beemacs-render-plan-find-item' resolves an id to its full task alist,
or nil for an id absent from the plan."
  (let ((data (beemacs-test--plan-fixture)))
    (should (equal (alist-get 'Status (beemacs-render-plan-find-item
                                        data "beemacs-transport"))
                    "DONE"))
    (should (null (beemacs-render-plan-find-item data "no-such-task")))))

(ert-deftest beemacs-test-api-plan-builds-plan-json-path ()
  "`beemacs-api-plan' requests /submodule/{name}/plan.json and parses it."
  (let (seen-url)
    (cl-letf (((symbol-function 'beemacs-transport--call)
               (lambda (url)
                 (setq seen-url url)
                 (list 200 nil
                       "{\"name\":\"beemacs\",\"plan\":{\"ROIStamp\":\"abc\",\"Items\":[]}}"))))
      (let ((result (beemacs-api-plan "beemacs")))
        (should (string-suffix-p "/submodule/beemacs/plan.json" seen-url))
        (should (equal (alist-get 'name result) "beemacs"))
        (should (equal (alist-get 'ROIStamp (alist-get 'plan result)) "abc"))))))

(provide 'beemacs-tests)

;;; beemacs-tests.el ends here
