;;; beemacs-api.el --- Typed beehived API wrappers -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Typed request/response wrappers over `beemacs-transport.el'.  This module
;; knows the shape of the beehived HTTP API (submodules, plans, tasks, ROIs,
;; docs, sessions, human escalations, editor, dances, hygiene, secrets,
;; stats) and translates raw JSON payloads into elisp data structures used by
;; `beemacs-render.el' and interactive commands.

;;; Code:

(require 'json)
(require 'beemacs-transport)

(defgroup beemacs-api nil
  "API layer over the beehived HTTP transport."
  :group 'beemacs
  :prefix "beemacs-api-")

(defun beemacs-api--parse-json (body)
  "Parse JSON BODY string into an elisp structure using alists/vectors."
  (let ((json-object-type 'alist)
        (json-array-type 'vector)
        (json-key-type 'symbol))
    (json-read-from-string body)))

(defun beemacs-api-submodules ()
  "Return the list of submodules known to the connected beehived instance."
  (beemacs-api--parse-json (beemacs-transport-get "/submodules")))

(define-error 'beemacs-api-error "beehived API request failed")

(defun beemacs-api--error-detail (body)
  "Return the human-readable `error' field from JSON BODY, or nil.

BODY is the raw response string.  If BODY parses as JSON and contains an
`error' key (the convention every beehived JSON handler uses, per
`writeJSON' in internal/web/editor.go and jsonapi.go), return its string
value.  Any parse failure or missing key yields nil rather than signaling
-- this helper is purely best-effort enrichment of an already-failing
request, never a second source of failure."
  (when (stringp body)
    (condition-case nil
        (let ((parsed (beemacs-api--parse-json body)))
          (when (listp parsed)
            (let ((err (alist-get 'error parsed)))
              (when (stringp err) err))))
      (error nil))))

(defun beemacs-api-json-request (path &optional endpoint)
  "Perform a JSON GET for PATH against ENDPOINT, returning parsed JSON.

This is the shared JSON-parsing/error-surfacing helper for every
JSON-backed beehived endpoint (today's `/api/editor/*' surface, and every
future `*.json'/dashboard/plan/roi/docs/branches/commit/stats/skills view
once the beehive:beemacs-json-api endpoints are consumed from beemacs).

On a transport-level failure (non-2xx status or connection failure),
`beemacs-transport-get' signals `beemacs-http-error'; this function
inspects the error data for an HTTP response body, extracts its JSON
`error' field per beehived's `writeJSON' convention, and re-signals
`beemacs-api-error' with that human-readable detail instead of the raw
transport error -- so callers see \"no such session\" rather than a bare
\"non-2xx response 404\". If no JSON `error' field is present, the
original transport error message is preserved.

On a 2xx response whose body is not valid JSON (a malformed payload),
this signals `beemacs-api-error' with a parse-failure message rather
than propagating the raw `json-readtable-error'/`json-error'.

On success, returns the parsed JSON structure (alists/vectors/strings/
numbers per `beemacs-api--parse-json')."
  (condition-case err
      (let ((body (beemacs-transport-get path endpoint)))
        (condition-case parse-err
            (beemacs-api--parse-json body)
          (error
           (signal 'beemacs-api-error
                    (list (format "malformed JSON response for %s: %s"
                                  path (error-message-string parse-err)))))))
    (beemacs-http-error
     (let* ((data (cdr err))
            ;; `beemacs-http-error' data is either (message) for a
            ;; connection failure, or (message (status headers body)) for
            ;; a non-2xx HTTP response -- see beemacs-transport.el.
            (response (nth 1 data))
            (response-body (and (listp response) (nth 2 response)))
            (detail (beemacs-api--error-detail response-body)))
       (signal 'beemacs-api-error
                (list (if detail
                          (format "%s (%s)" detail path)
                        (format "%s" (car data)))))))))

(defun beemacs-api-plan (name &optional endpoint)
  "Return the parsed plan payload for submodule NAME.

Mirrors GET /submodule/{name}/plan.json (beehived's `planJSON' handler,
internal/web/jsonapi.go): an alist with a `name' key and a `plan' key
whose value carries `ROIStamp' and the `Items' vector of task alists
(ID/Status/Weight/Deps/claim state/DocHref/SessionHref, per
internal/web.PlanItem -- these structs carry no json tags, so keys
serialize verbatim as the Go field names). `beemacs-render-plan-rows'
projects this payload into `tabulated-list-entries'.

ENDPOINT optionally overrides `beemacs-endpoint' for this call only.
Signals `beemacs-api-error' on any transport or JSON failure."
  (beemacs-api-json-request (format "/submodule/%s/plan.json" name) endpoint))

(provide 'beemacs-api)

;;; beemacs-api.el ends here
