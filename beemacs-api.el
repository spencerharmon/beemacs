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
(require 'cl-lib)
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
    ;; `beemacs-http-error' data is either (message) for a connection
    ;; failure, or (message (status headers body)) for a non-2xx HTTP
    ;; response -- see beemacs-transport.el and
    ;; `beemacs-api--handle-http-error'.
    (beemacs-http-error (beemacs-api--handle-http-error err path))))

(defun beemacs-api--handle-http-error (err path)
  "Convert a caught `beemacs-http-error' ERR for PATH into `beemacs-api-error'.

Shared by `beemacs-api-json-request' and `beemacs-api-json-post': both
JSON-backed transport wrappers hit the same `writeJSON' failure
convention on the server (a non-2xx response, JSON body with an `error'
field, per `beemacs-api--error-detail'), so both convert a caught
`beemacs-http-error' identically. See `beemacs-api-json-request' for the
full documented behavior; this signals in place of returning."
  (let* ((data (cdr err))
         (response (nth 1 data))
         (response-body (and (listp response) (nth 2 response)))
         (detail (beemacs-api--error-detail response-body)))
    (signal 'beemacs-api-error
             (list (if detail
                       (format "%s (%s)" detail path)
                     (format "%s" (car data)))))))

(defun beemacs-api-json-post (path payload &optional endpoint)
  "Perform a JSON POST for PATH against ENDPOINT with PAYLOAD.
Return the parsed JSON response.

PAYLOAD is an elisp alist encoded with `json-encode' as the request body
-- the write-side counterpart to `beemacs-api-json-request', used by
every `/api/editor/*' write call (`POST /api/editor',
`POST /api/editor/{id}/chat', `POST /api/editor/{id}/merge'). Error
handling (malformed 2xx JSON, a non-2xx response with a JSON `error'
body, a non-2xx response with a non-JSON body, or a connection failure)
is identical to `beemacs-api-json-request' -- see its docstring for the
full behavior; both funnel through `beemacs-api--handle-http-error' so
the two never drift."
  (condition-case err
      (let ((body (beemacs-transport-post path (json-encode payload) endpoint)))
        (condition-case parse-err
            (beemacs-api--parse-json body)
          (error
           (signal 'beemacs-api-error
                    (list (format "malformed JSON response for %s: %s"
                                  path (error-message-string parse-err)))))))
    (beemacs-http-error (beemacs-api--handle-http-error err path))))

(defun beemacs-api-docs (name)
  "Return the docs/ file listing for submodule NAME.

Mirrors `GET /submodule/{name}/docs.json' (beehived's `docsJSON', wrapping
`docTree' -- the same recursive docs/ walk the HTML doc explorer shows).
The returned alist carries top-level keys `name' and `docs' (a vector of
per-entry alists with keys `Path', `Name', `Dir', `Href' -- `DocEntry' has
no json tags, so decoded keys are the exact capitalized Go field names)."
  (beemacs-api-json-request (format "/submodule/%s/docs.json" name)))

(defun beemacs-api-doc (name file)
  "Return one doc's raw content: submodule NAME, path FILE (docs/-relative).

Mirrors `GET /submodule/{name}/doc.json/{file...}' (beehived's `docJSON').
The returned alist carries `name', `file', and `body' (the raw file
content as a string) -- all lower-case, since `docJSON' builds its own
response map rather than marshaling a struct."
  (beemacs-api-json-request (format "/submodule/%s/doc.json/%s" name file)))

(defun beemacs-api--query-string (params)
  "Build a URL query string \"?k=v&...\" from PARAMS, an alist of (KEY . VALUE).

A VALUE of nil omits that pair. Returns \"\" when every value is nil."
  (let ((pairs (delq nil
                      (mapcar (lambda (kv)
                                (when (cdr kv)
                                  (format "%s=%s" (car kv)
                                          (url-hexify-string (format "%s" (cdr kv))))))
                              params))))
    (if pairs (concat "?" (mapconcat #'identity pairs "&")) "")))

(defun beemacs-api-branches (name &optional offset limit)
  "Return a page of submodule NAME's commit history.

Mirrors `GET /submodule/{name}/branches.json' (beehived's `branchesJSON',
wrapping `commitGraph' plus the same DocHref/FlipSHA/FlipHref
delivery-traceability enrichment the HTML branch view applies -- just
flat, without HTML-only date sectioning). OFFSET/LIMIT are optional
pagination params passed through as query parameters when non-nil (the
server defaults to offset 0, limit 50, capped at 200 -- see `pageParams').
The returned alist carries `name', `commits' (a vector of per-commit
alists with keys `SHA', `Refs', `Subject', `Author', `Date', `DocTask',
`DocPath', `DocHref', `FlipSHA', `FlipHref' -- `Commit' has no json tags,
so decoded keys are the exact capitalized Go field names), `offset',
`limit', and `has_next'."
  (beemacs-api-json-request
   (format "/submodule/%s/branches.json%s" name
           (beemacs-api--query-string `(("offset" . ,offset) ("limit" . ,limit))))))

(defun beemacs-api-commit (name sha)
  "Return one hive commit's PLAN.md before/after content.

Mirrors `GET /submodule/{name}/commit.json/{sha}' (beehived's
`commitJSON'), scoped to submodule NAME's PLAN.md, at commit SHA. The
returned alist carries `name', `sha', `author', `date', `subject',
`plan_before', and `plan_after' -- all lower-case, since `commitJSON'
builds its own response map rather than marshaling a struct."
  (beemacs-api-json-request (format "/submodule/%s/commit.json/%s" name sha)))

(defun beemacs-api-dashboard ()
  "Return the hive-wide swarm dashboard: per-submodule cards plus widgets.

Mirrors `GET /dashboard.json' (beehived's `dashboardJSON'), the same data
the HTML dashboard renders. The returned alist carries top-level keys
`subs' (a vector of per-submodule `subView' alists with keys `Name',
`State', `Stamp', `Pending', `Human', `Env', `Working', `Bees' -- `subView'
has no json tags, so decoded keys are the exact capitalized Go field
names), `hygiene', `bootstrap', `root_files', `root_files_drift',
`skills_drift', and `instruction_drift'. Hive-wide -- takes no submodule
NAME, unlike `beemacs-api-plan'/`beemacs-api-roi' below."
  (beemacs-api-json-request "/dashboard.json"))

(defun beemacs-api-dashboard-submodule (name)
  "Return NAME's `subView' alist from `beemacs-api-dashboard', or nil.

A thin convenience filter over the hive-wide dashboard payload so callers
needing just one submodule's summary card (state/stamp/pending/human/env/
working/bees) don't re-implement the `subs' vector scan themselves."
  (let ((subs (append (alist-get 'subs (beemacs-api-dashboard)) nil)))
    (cl-find-if (lambda (s) (equal (alist-get 'Name s) name)) subs)))

(defun beemacs-api-plan (name)
  "Return submodule NAME's live task list.

Mirrors `GET /submodule/{name}/plan.json' (beehived's `planJSON', wrapping
`planViewData' -- the same claim/running state and doc links the plan
page and the runner's own selection use). The returned alist carries
`name' and `plan' (an alist with keys `ROIStamp' and `Items', each item
carrying the capitalized `PlanItem' struct fields -- `ID', `Status',
`Desc', `Body', `Deps', `DepStates', `Weight', `Session', `Heartbeat',
`NotBefore', `Active', `Stale', `Doc', `DocHref', `HumanReason',
`Category', `Running', `SessionHref' -- no json tags, so decoded keys are
the exact capitalized Go field names)."
  (beemacs-api-json-request (format "/submodule/%s/plan.json" name)))

(defun beemacs-api-roi (name)
  "Return submodule NAME's raw ROI.md content and tracked remote url.

Mirrors `GET /submodule/{name}/roi.json' (beehived's `roiJSON'). The
returned alist carries `name', `body' (the raw ROI.md text), and
`remote_url' -- all lower-case, since `roiJSON' builds its own response
map rather than marshaling a struct."
  (beemacs-api-json-request (format "/submodule/%s/roi.json" name)))

(defun beemacs-api-secrets ()
  "Return the hive-wide secrets key-name listing (global plus per-submodule).

Mirrors `GET /secrets.json' (beehived's `secretsJSON'). Like the HTML
secrets panels, this NEVER returns values, only key names. The returned
alist carries `global' (a vector of key-name strings for the active
repo's root SECRETS.yaml.gpg) and `submodules' (a vector of alists with
keys `name' and `keys', one per tracked submodule) -- all lower-case,
since `secretsJSON' builds its own response map rather than marshaling a
struct. Hive-wide -- takes no submodule NAME; use
`beemacs-api-secrets-for' to filter to one submodule's keys."
  (beemacs-api-json-request "/secrets.json"))

(defun beemacs-api-secrets-for (name)
  "Return NAME's `keys' vector from `beemacs-api-secrets', or nil.

A thin convenience filter over the hive-wide secrets payload's
`submodules' vector, mirroring `beemacs-api-dashboard-submodule''s filter
pattern over `beemacs-api-dashboard''s `subs' vector."
  (let* ((data (beemacs-api-secrets))
         (subs (append (alist-get 'submodules data) nil))
         (entry (cl-find-if (lambda (s) (equal (alist-get 'name s) name)) subs)))
    (and entry (alist-get 'keys entry))))

(defun beemacs-api-secrets-set (key value &optional submodule)
  "Set secret KEY to VALUE, in SUBMODULE's own file or globally if nil.

Mirrors `POST /secrets.json' (beehived's `secretsWriteJSON', the JSON
write-side counterpart to `secretsPost'/`submoduleSecretsPost'). Like
those HTML write handlers, this only ever WRITES a value -- it is never
returned by any read path. When SUBMODULE is nil or empty, the key is
written to the active repo's own global `SECRETS.yaml.gpg'; otherwise it
is written to that submodule's own secrets file. Returns the same
hive-wide key-name listing `beemacs-api-secrets' returns (the server
re-renders `secretsJSON' after the write), so callers can refresh a
displayed listing from the response instead of issuing a second GET."
  (beemacs-api-json-post
   "/secrets.json"
   `((key . ,key)
     (value . ,value)
     ,@(when (and submodule (not (string-empty-p submodule)))
         `((submodule . ,submodule))))))

(defun beemacs-api-skills ()
  "Return the hive-wide skills/dances registry, hygiene scan, and cache widget.

Mirrors `GET /skills.json' (beehived's `skillsJSON'), the same data the
combined hygiene+dances page renders (`/skills' pre-rename redirects to
`/hygiene'). The returned alist carries top-level keys `hygiene' (the
cruft-scan result), `dances' (a vector of per-skill alists with keys
`Name', `Title', `Summary', `Destructive', `ReportOnly' -- `dancePanel'
has no json tags, so decoded keys are the exact capitalized Go field
names), and `cache' (the view-cache widget). This is a hive-wide
endpoint -- unlike the submodule-scoped `beemacs-api-docs'/
`beemacs-api-branches'/`beemacs-api-commit', it takes no submodule NAME."
  (beemacs-api-json-request "/skills.json"))

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
