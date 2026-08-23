# beemacs ↔ beehived API contract

This document is the machine-consumable contract beemacs codes against: what
HTTP surface `beehived` exposes, which parts are JSON (safe for an Emacs
client to parse and act on), which parts are server-rendered HTML/htmx (not a
stable data contract), and the shared client-side conventions every
JSON-backed beemacs command uses.

## Audit: beehived's HTTP surface today

`beehived` (submodule `beehive`, `internal/web/`) serves two very different
kinds of routes:

1. **Server-rendered HTML/htmx views** — the large majority of routes
   (`GET /`, `GET /submodule/{name}`, `GET /submodule/{name}/plan`,
   `GET /submodule/{name}/roi`, `GET /submodule/{name}/docs`,
   `GET /submodule/{name}/session/{branch}`, `/human/...`, `/dances/...`,
   `/secrets/...`, etc., `internal/web/web.go`). These return HTML fragments
   designed for htmx to splice into a browser DOM. **They are not a JSON
   contract** — their shape is presentation markup, not data, and beemacs
   must never scrape them.
2. **JSON endpoints** — two families:
   - The **`/api/editor/*`** surface (`internal/web/editor.go`): the
     agentic-editor read/write API — `POST /api/editor` (open),
     `GET /api/editor/{id}` (poll state), `POST /api/editor/{id}/chat`,
     `POST /api/editor/{id}/merge`, `GET /api/editor/{id}/diff`. Every
     handler here uses the shared `writeJSON(w, status, body)` helper and,
     on failure, always returns `{"error": "<message>"}` alongside the
     non-2xx status — this is the load-bearing convention
     `beemacs-api-json-request` below relies on.
   - The **read-only `*.json` mirror surface** added by
     `beehive:beemacs-json-api` (`internal/web/jsonapi.go`), landed
     additively alongside the existing HTML views: `GET /dashboard.json`,
     `GET /stats.json`, `GET /skills.json`,
     `GET /submodule/{name}/plan.json`, `GET /submodule/{name}/roi.json`,
     `GET /submodule/{name}/docs.json`,
     `GET /submodule/{name}/doc/{doc}.json` (exact path per `docJSON`),
     `GET /submodule/{name}/branches.json`,
     `GET /submodule/{name}/commit/{sha}.json` (per `commitJSON`), plus the
     streaming exception below. None of these replace their HTML sibling
     route; each is purely additive and mirrors the same underlying
     view-data function (e.g. `planJSON` calls the same `planViewData` the
     HTML `plan` handler renders).
   - **Streaming exception**: `GET /submodule/{name}/session/{branch}/stream`
     is `text/event-stream` SSE, not a single JSON document — a live session
     transcript feed. It is JSON-*framed* (each SSE `data:` line is a JSON
     event) but must be consumed incrementally, not via
     `beemacs-api-json-request`.

### Gap this task closes: filed, not re-litigated here

At the time this contract was written, the `*.json` mirror surface and the
SSE stream endpoint did **not yet exist** — beehived's JSON surface was only
`/api/editor/*`. That gap was filed as the real cross-submodule task
`beehive:beemacs-json-api` (`submodules/beehive/PLAN.md`, linked via
`SUBMODULE-LINKS.yaml`) rather than invented as a local placeholder
dependency. That task has since landed
(`submodules/beehive/docs/bee-beemacs-json-api-beemacs-json-api.md`,
commit `22728e3ca24f608a2682b373cbb485acf3771425`) and is reflected in the
audit above. Every beemacs read view (`beemacs-dashboard`,
`beemacs-submodule-view`, `beemacs-plan-view`, `beemacs-roi-view`,
`beemacs-docs-commits-branches`, `beemacs-stats-view`, `beemacs-skills-view`)
depends on `beemacs-api-contract` (this task) *and*
`beehive:beemacs-json-api` and consumes the JSON surface documented above.

## Client-side contract: `beemacs-api-json-request`

`beemacs-api.el` provides `beemacs-api-json-request` (PATH &optional
ENDPOINT) as the **single shared entry point** every JSON-backed beemacs
command uses to call any of the endpoints above (today: `/api/editor/*`;
going forward: every `*.json` view and any future JSON write path). It
layers on `beemacs-transport-get`:

- **Success** — a 2xx response whose body is valid JSON returns the parsed
  structure (`json-object-type 'alist`, `json-array-type 'vector`,
  `json-key-type 'symbol`, matching `beemacs-api--parse-json`).
- **Malformed JSON body on a 2xx response** — signals `beemacs-api-error`
  with a parse-failure message, never propagating a raw
  `json-readtable-error`/`json-error` to the caller.
- **Non-2xx response with a JSON `{"error": "..."}"` body** — the
  `writeJSON` convention every `/api/editor/*` and `*.json` handler follows
  on failure. `beemacs-api-json-request` extracts that `error` string and
  signals `beemacs-api-error` with it, so a caller sees the server's actual
  message (`"no such session"`, `"message required"`, …) instead of a bare
  `"non-2xx response 404"`.
- **Non-2xx response with a non-JSON body, or a connection failure** —
  falls back to the underlying `beemacs-http-error` message, still
  re-signaled as `beemacs-api-error` so callers only ever need to handle
  one error type.

`beemacs-api-error` (via `define-error`) is the one condition every
JSON-backed beemacs command should catch; `beemacs-http-error` remains the
lower-level transport condition for non-JSON transport users.

## Non-goals

- HTML/htmx routes are explicitly out of contract — beemacs never parses
  rendered markup as data.
- Write-path semantics beyond `/api/editor/*` (e.g. a future `POST` to a
  `*.json` route) are not yet part of beehived's surface and are not
  claimed here; they are scoped to whichever future task adds them.
