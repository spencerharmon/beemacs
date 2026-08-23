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

(defun beemacs-api-json-post-form (path fields &optional endpoint)
  "Perform a JSON-returning form POST for PATH against ENDPOINT with FIELDS.
Return the parsed JSON response.

FIELDS is an alist of (KEY . VALUE) strings encoded as
`application/x-www-form-urlencoded' via `beemacs-transport-post-form' --
the write-side counterpart to `beemacs-api-json-post' for a handler that
reads its input via Go's `r.FormValue' (e.g. the dances JSON apply route,
which reads `confirm' exactly like its HTML sibling) rather than a JSON
request body. The RESPONSE is still expected to be JSON, unlike
`beemacs-transport-post-form''s own HTML/htmx callers. Error handling
(malformed 2xx JSON, a non-2xx response with a JSON `error' body, a
non-2xx response with a non-JSON body, or a connection failure) is
identical to `beemacs-api-json-request'/`beemacs-api-json-post' -- see
`beemacs-api-json-request''s docstring for the full behavior; all three
funnel through `beemacs-api--handle-http-error' so none of them drift."
  (condition-case err
      (let ((body (beemacs-transport-post-form path fields endpoint)))
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

(defun beemacs-api-roi-set (name body &optional endpoint)
  "Publish submodule NAME's ROI.md as BODY via `POST /roi/{name}'.

Mirrors beehived's `roiPost' handler (`internal/web/web.go'): the
sanctioned ROI publish path used by the web UI's ROI editor -- it writes
BODY to the submodule's `ROI.md' on the server and immediately
`publishMain's the commit (never a direct checkout write from this
client). This is a plain HTML/htmx form-POST route (see
`docs/api-contract.md''s HTML-vs-JSON split), not one of the `*.json'/
`/api/editor/*' JSON surfaces -- so, like `beemacs-api-merge', the
SUCCESS return value is the raw HTML response body text (the re-rendered
`roi_editor.html' fragment), not parsed JSON; callers must not scrape it
as structured data, only confirm a 2xx was actually received.

Returns the raw response body string on success (2xx). On failure,
signals `beemacs-api-error' carrying the server's TRUE result text (a
plain-text `http.Error' body from a failed ROI.md write or publish),
via `beemacs-api--handle-form-http-error' -- never an assumed success or
a synthesized generic failure. ENDPOINT optionally overrides
`beemacs-endpoint' for this call only."
  (condition-case err
      (beemacs-transport-post-form
       (format "/roi/%s" name) `(("body" . ,body)) endpoint)
    (beemacs-http-error
     (beemacs-api--handle-form-http-error err (format "/roi/%s" name)))))

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

(defun beemacs-api-dance-plan (name)
  "Return named dance NAME's identity plus its deterministic dry-run plan.

Mirrors `POST /api/dances/{name}/plan' (beehived's `apiDancePlan', the
JSON mirror of the HTML `/dances/{name}/plan' route -- see
`beemacs-json-dances-api'). Both call the SAME `danceRegistry.plan' the
HTML handler uses, so this can never drift from what the web UI's dance
panel shows; only the response encoding differs. Mutates nothing. The
returned alist carries `name', `title', `destructive', `reportOnly', and
`plan' (an alist with keys `empty' and `diffs', each diff an alist with
keys `path'/`before'/`after' -- `dancePlanJSON'/`danceDiffJSON' have
`json' tags, so decoded keys are lower-case). An unknown NAME signals
`beemacs-api-error' with beehived's own \"unknown dance\" detail (see
`beemacs-api--handle-http-error')."
  (beemacs-api-json-post-form (format "/api/dances/%s/plan" name) nil))

(defun beemacs-api-dance-apply (name &optional confirm)
  "Apply named dance NAME, returning its result plus a fresh plan.

Mirrors `POST /api/dances/{name}/apply' (beehived's `apiDanceApply'),
passing CONFIRM as the same `confirm' form value the HTML route reads
via `r.FormValue' -- omit/nil for the normal first attempt, non-nil to
proceed past a destructive dance's confirmation gate. Reuses the SAME
`danceRegistry.apply' the HTML handler calls, so plan/apply can never
drift between the two surfaces.

An unknown NAME or a report-only dance signals `beemacs-api-error' (404/
400 respectively, per beehived's invocation guards). A destructive dance
invoked WITHOUT CONFIRM performs NO mutation: it returns 200 with
`confirmRequired' non-nil (`t' after JSON decoding) alongside a fresh
`plan' key -- inspect this BEFORE assuming an apply mutated anything;
never treat a 200 response alone as proof of a completed apply. On an
actual apply, the returned alist carries `name', `result' (the dance's
own beehived-defined result payload), and `plan' (a fresh dry-run,
`empty' once the apply's proposed changes have been made)."
  (beemacs-api-json-post-form
   (format "/api/dances/%s/apply" name)
   (when confirm '(("confirm" . "true")))))

(defun beemacs-api-stats ()
  "Return the hive-wide honeybee-performance stats (`GET /stats.json').

Mirrors beehived's `statsJSON', the same figures the web `/stats' page
renders, sourced from the JSON contract rather than a direct Prometheus
query or scraped HTML. The returned alist carries top-level key `subs'
(a vector of per-submodule alists with the capitalized `subStat' fields
`Name'/`DeliveredTasks'/`Honeybees'/`ActiveNow'/`Stranded'/
`DeliveredPerBeePct'/`Models'/`Deliveries' -- `subStat' has no json
tags, so decoded keys are the exact capitalized Go field names) and
`total' (the same shape, aggregated across every submodule). This is a
hive-wide endpoint -- like `beemacs-api-skills' and
`beemacs-api-dashboard', it takes no submodule NAME."
  (beemacs-api-json-request "/stats.json"))

(defun beemacs-api--handle-form-http-error (err path)
  "Convert a caught `beemacs-http-error' ERR for form-POST PATH into
`beemacs-api-error', preferring the server's TRUE response body over a
synthesized \"non-2xx response NNN\" message.

The plain HTML/htmx maintenance routes this backs (`/merge' and friends)
fail via Go's `http.Error', which writes a PLAIN-TEXT body (\"merge
conflict\", a wrapped git error, ...), not the `writeJSON'
`{\"error\":...}' convention `beemacs-api--handle-http-error' expects. So
this first tries the JSON convention (some of these routes may still use
it), then falls back to the raw response body text (trimmed) when it is
non-empty plain text, and only falls back further to the generic
transport message when the body itself is empty -- ensuring the caller
always surfaces the actual backend result, never an assumed failure
reason."
  (let* ((data (cdr err))
         (response (nth 1 data))
         (response-body (and (listp response) (nth 2 response)))
         (detail (or (beemacs-api--error-detail response-body)
                     (and (stringp response-body)
                          (not (string-empty-p (string-trim response-body)))
                          (string-trim response-body)))))
    (signal 'beemacs-api-error
             (list (if detail
                       (format "%s (%s)" detail path)
                     (format "%s" (car data)))))))

(defun beemacs-api-merge (name branch &optional endpoint)
  "Merge BRANCH into submodule NAME's tracked branch via `POST /merge'.

Mirrors beehived's `mergePost' handler (`internal/web/web.go'): the
swarm-maintenance form-POST route that runs the real `git merge' against
the submodule's checkout, commits the hive-side PLAN.md on success, and
re-renders the merge panel -- or fails loudly with the true backend
result (plain-text \"merge conflict\" on a real git conflict, HTTP 409;
any other git failure, HTTP 500 with the git error text). Companion
`GET /merge' (`mergeGet') renders the same panel read-only and is
reachable via `beemacs-transport-get' directly since it takes no
parameters.

This is a plain HTML/htmx route (see `docs/api-contract.md''s
HTML-vs-JSON split), not one of the `*.json'/`/api/editor/*' JSON
surfaces -- so, unlike every other `beemacs-api-*' function in this
file, the SUCCESS return value is the raw HTML response body text (the
re-rendered merge panel fragment), not parsed JSON. Callers must not
scrape it as structured data (per the contract's non-goals); it exists
so a caller can confirm/display that a 2xx response was actually
received, never merely assume one.

Returns the raw response body string on success (2xx). On failure,
signals `beemacs-api-error' carrying the server's TRUE result text (the
plain-text git/merge error, extracted via
`beemacs-api--handle-form-http-error') -- this command never reports an
assumed-success or assumed-failure message; every outcome, success or
error, reflects what beehived's `mergePost' actually returned.
ENDPOINT optionally overrides `beemacs-endpoint' for this call only."
  (condition-case err
      (beemacs-transport-post-form
       "/merge" `(("name" . ,name) ("branch" . ,branch)) endpoint)
    (beemacs-http-error (beemacs-api--handle-form-http-error err "/merge"))))

;;; Fleet management: add a submodule, link two submodules, change a
;;; tracked remote

(defun beemacs-api-submodule-add (url &optional name branch endpoint)
  "Register a new submodule tracking URL via `POST /submodule/add'.

Mirrors beehived's `submoduleAdd' handler (`internal/web/web.go'): the
swarm-fleet-management form-POST route that clones URL as a new tracked
submodule, optionally named NAME (derived from URL when omitted; must be
a single safe path segment, no `/'/`\\', not `.'/`..') and optionally
following tracked BRANCH, then commits/publishes the registration.

This is a plain HTML/htmx form-POST route (see `docs/api-contract.md''s
HTML-vs-JSON split), not one of the `*.json'/`/api/editor/*' JSON
surfaces -- so, like `beemacs-api-merge', the SUCCESS return value is
whatever raw response body text beehived returns (a 303 redirect to `/'
is followed transparently by `url-retrieve-synchronously', so the body
is typically the re-rendered dashboard HTML), not parsed JSON; callers
must not scrape it as structured data, only confirm a 2xx was actually
received.

Returns the raw response body string on success (2xx). On failure --
missing URL (`ErrURLRequired'), an invalid NAME (`ErrInvalidName'), a
NAME already in use (`ErrExists', HTTP 409), or any other git/publish
failure -- signals `beemacs-api-error' carrying the server's TRUE
plain-text result via `beemacs-api--handle-form-http-error', never an
assumed success or a synthesized generic failure. ENDPOINT optionally
overrides `beemacs-endpoint' for this call only."
  (condition-case err
      (beemacs-transport-post-form
       "/submodule/add"
       `(("url" . ,url)
         ,@(when (and name (not (string-empty-p name))) `(("name" . ,name)))
         ,@(when (and branch (not (string-empty-p branch))) `(("branch" . ,branch))))
       endpoint)
    (beemacs-http-error (beemacs-api--handle-form-http-error err "/submodule/add"))))

(defun beemacs-api-submodule-link (from to &optional endpoint)
  "Register a dependency edge FROM -> TO via `POST /submodule/link'.

Mirrors beehived's `submoduleLink' handler (`internal/web/web.go'): the
swarm-fleet-management form-POST route that records, in
`SUBMODULE-LINKS.yaml', that submodule FROM may depend on (reference
tasks owned by) submodule TO, cycle-checked before it is written.

This is a plain HTML/htmx form-POST route (see `docs/api-contract.md''s
HTML-vs-JSON split), not one of the `*.json'/`/api/editor/*' JSON
surfaces -- so, like `beemacs-api-merge', the SUCCESS return value is
the raw response body text beehived returns, not parsed JSON; callers
must not scrape it as structured data, only confirm a 2xx was actually
received.

Returns the raw response body string on success (2xx). On failure --
an empty FROM/TO (`ErrInvalidDep'), a link that would form a wait-cycle
or self-dependency (`ErrCycle', HTTP 409), or any other failure --
signals `beemacs-api-error' carrying the server's TRUE plain-text result
via `beemacs-api--handle-form-http-error', never an assumed success or a
synthesized generic failure. ENDPOINT optionally overrides
`beemacs-endpoint' for this call only."
  (condition-case err
      (beemacs-transport-post-form
       "/submodule/link" `(("from" . ,from) ("to" . ,to)) endpoint)
    (beemacs-http-error (beemacs-api--handle-form-http-error err "/submodule/link"))))

(defun beemacs-api-submodule-set-remote (name url &optional endpoint)
  "Change tracked submodule NAME's remote to URL via
`POST /submodule/{name}/remote'.

Mirrors beehived's `submoduleRemote' handler (`internal/web/web.go'):
the swarm-fleet-management form-POST route that rewrites
`.gitmodules''s `submodule.<path>.url' to URL and syncs the checkout's
`origin' remote (`submod.SetRemoteURL', `git submodule sync'
underneath).

This is a plain HTML/htmx form-POST route (see `docs/api-contract.md''s
HTML-vs-JSON split), not one of the `*.json'/`/api/editor/*' JSON
surfaces -- so, like `beemacs-api-merge', the SUCCESS return value is
the raw response body text beehived returns (a 303 redirect to
`/roi/{name}' is followed transparently, so the body is typically the
re-rendered ROI editor HTML for NAME), not parsed JSON; callers must not
scrape it as structured data, only confirm a 2xx was actually received.

Returns the raw response body string on success (2xx). On failure --
a missing URL (`ErrURLRequired'), an invalid NAME (`ErrInvalidName'), an
unknown submodule checkout (`ErrNotExist', HTTP 404), or any other
failure -- signals `beemacs-api-error' carrying the server's TRUE
plain-text result via `beemacs-api--handle-form-http-error', never an
assumed success or a synthesized generic failure. ENDPOINT optionally
overrides `beemacs-endpoint' for this call only."
  (let ((path (format "/submodule/%s/remote" name)))
    (condition-case err
        (beemacs-transport-post-form path `(("url" . ,url)) endpoint)
      (beemacs-http-error (beemacs-api--handle-form-http-error err path)))))

(defun beemacs-api-human ()
  "Return the hive-wide NEEDS-HUMAN task listing across every tracked submodule.

Mirrors `GET /human.json' (beehived's `humanJSON', internal/web/jsonapi.go),
the same scan the HTML `/human' list page renders. The returned alist
carries a single `tasks' key: a vector of alists with keys `sub', `id',
`desc', `body', `deps', `reason' (the blocker's `HumanReason'), and
`category' -- all lower-case, since `humanJSON' builds its own response
map rather than marshaling a struct. Hive-wide -- takes no submodule
name, like `beemacs-api-skills'/`beemacs-api-stats'."
  (beemacs-api-json-request "/human.json"))

(defun beemacs-api-human-task (sub id)
  "Return one NEEDS-HUMAN task's context for submodule SUB's task ID.

Mirrors `GET /human.json/{sub}/{id}' (beehived's `humanTaskJSON'), the
same lookup `humanResolvePage' uses. The returned alist carries `sub',
`id', `desc', `body', `deps', `reason', `category', and `has_session'
(whether a resolution agent session already exists for this task). A
task that is unknown or no longer `NEEDS-HUMAN' 404s, surfaced as
`beemacs-api-error' via `beemacs-api-json-request''s normal handling."
  (beemacs-api-json-request (format "/human.json/%s/%s" sub id)))

(defun beemacs-api-human-session (sub id)
  "Open (or return the existing) resolution session for SUB's task ID.

Mirrors `POST /api/human/{sub}/{id}/session' (beehived's
`apiHumanSession'), which opens the same AI resolution-agent session the
HTML `/human/{sub}/{id}' resolve page uses. Returns the panel-shaped
alist (`sid'/`SessID', `Sub', `TaskID', `Log', `Stat', `Diffs',
`HasChange', `Busy', `Published', `Error') -- see `beemacs-api-human-panel'
for the field meanings."
  (beemacs-api-json-post (format "/api/human/%s/%s/session" sub id) '()))

(defun beemacs-api-human-panel (sub id sid)
  "Poll session SID's live resolution panel for SUB's task ID.

Mirrors `GET /api/human/{sub}/{id}/panel/{sid}' (beehived's
`apiHumanPanel'), the JSON mirror of `humanResolvePanel'. The returned
alist carries `SessID', `Sub', `TaskID', `Log' (a vector of
`{role,text,at}' turns), `Stat' (a diffstat string), `Diffs' (a vector of
per-file diff boxes), `HasChange', `Busy' (a turn is in flight), and
`Published' (the branch has landed on main via a completed agent turn)."
  (beemacs-api-json-request (format "/api/human/%s/%s/panel/%s" sub id sid)))

(defun beemacs-api-human-message (sub id sid message)
  "Send MESSAGE to session SID resolving SUB's task ID, run one agent turn.

Mirrors `POST /api/human/{sub}/{id}/message/{sid}' (beehived's
`apiHumanMessage'), the JSON mirror of `humanResolveMessage': runs the
resolution agent's turn synchronously with `{\"message\": MESSAGE}' as the
body and returns the refreshed panel (same shape as
`beemacs-api-human-panel')."
  (beemacs-api-json-post
   (format "/api/human/%s/%s/message/%s" sub id sid)
   `((message . ,message))))

(defun beemacs-api-human-publish (sub id sid)
  "Publish session SID's committed changes for SUB's task ID to main.

Mirrors `POST /api/human/{sub}/{id}/publish/{sid}' (beehived's
`apiHumanPublish'), the JSON mirror of `humanResolvePublish': lands the
resolution agent's accumulated branch changes on the hive main. Returns
the refreshed panel, carrying an `error' key instead of signaling on a
publish failure (the server itself never returns non-2xx here -- see
`apiHumanPublish') -- callers should check the returned alist's `error'
key in addition to the normal `beemacs-api-error' failure path."
  (beemacs-api-json-post (format "/api/human/%s/%s/publish/%s" sub id sid) '()))

(defun beemacs-api-human-discard (sub id sid)
  "Discard session SID's unpublished work for SUB's task ID.

Mirrors `POST /api/human/{sub}/{id}/discard/{sid}' (beehived's
`apiHumanDiscard'), the JSON mirror of `humanResolveDiscard': tears down
the resolution agent's worktree/branch by session id and, when the task
is still `NEEDS-HUMAN', opens a fresh session so the operator can restart
cleanly. Returns `{\"sid\": ...}' -- the new session id, or nil/`:null'
when the task is no longer blocked."
  (beemacs-api-json-post (format "/api/human/%s/%s/discard/%s" sub id sid) '()))

(defun beemacs-api-human-resolve (sub id)
  "Flip SUB's NEEDS-HUMAN task ID back to TODO via the sanctioned backend flow.

Mirrors `POST /api/human/{sub}/{id}/resolve' (beehived's
`apiHumanResolve'), the JSON mirror of `humanResolveApply': the
deterministic `plan.Task.Resolve' + `publishMain' flow (never a direct
`PLAN.md'/`ROI.md' edit from this client) -- the same status flip + main
publish the HTML \"Mark resolved\" button drives. Rejects (via
`beemacs-api-error', a non-2xx `writeJSON' response) a task that is no
longer `NEEDS-HUMAN' (already resolved, or never blocked), so a
double-submit or a stale view can never reset an in-flight task's
status/claim."
  (beemacs-api-json-post (format "/api/human/%s/%s/resolve" sub id) '()))

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
