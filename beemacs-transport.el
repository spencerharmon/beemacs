;;; beemacs-transport.el --- HTTP transport for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Low-level HTTP transport to a running `beehived' instance.  This module
;; owns the connection details (base endpoint, request construction,
;; response parsing) and exposes a small, synchronous API consumed by
;; `beemacs-api.el'.  No other module should build an HTTP request or call
;; `url.el' directly.
;;
;; `beemacs-endpoint' is the only baked-in default and is intentionally a
;; loopback address -- every real deployment configures its own base URL via
;; `customize' or a per-call override; no real hostname is ever hard-coded
;; here.
;;
;; `beemacs-request' returns a structured `(status headers body)' list on
;; any HTTP response (2xx or otherwise), and signals `beemacs-http-error' --
;; never a silently-swallowed failure -- on a non-2xx status or a connection
;; failure (DNS/refused/timeout).  Callers that only want a successful body
;; can use `beemacs-transport-get'/`beemacs-transport-request', which raise
;; the same error on failure.

;;; Code:

(require 'url)
(require 'cl-lib)

(defgroup beemacs-transport nil
  "Transport layer for talking to a beehived server."
  :group 'beemacs
  :prefix "beemacs-")

(define-obsolete-variable-alias 'beemacs-server-url 'beemacs-endpoint "0.2.0"
  "Renamed for clarity; `beemacs-endpoint' is the per-install base URL.")

(defcustom beemacs-endpoint "http://127.0.0.1:8080"
  "Base URL of the beehived HTTP server this beemacs install talks to.

This is a per-install setting -- point it at your own beehived instance.
No real/production host is ever baked in as the shipped default; the
default is a loopback address suitable only for local development."
  :group 'beemacs-transport
  :type 'string)

(define-error 'beemacs-http-error "beehived HTTP request failed")

(defun beemacs-transport--url (path &optional endpoint)
  "Build a full request URL for PATH against ENDPOINT or `beemacs-endpoint'."
  (concat (string-remove-suffix "/" (or endpoint beemacs-endpoint))
          "/"
          (string-remove-prefix "/" path)))

(defun beemacs-transport--parse-status-line (line)
  "Parse an HTTP status LINE such as \"HTTP/1.1 200 OK\" into an integer."
  (if (and line (string-match "\\` *HTTP/[0-9.]+ +\\([0-9]+\\)" line))
      (string-to-number (match-string 1 line))
    (error "beemacs-transport: malformed HTTP status line: %S" line)))

(defun beemacs-transport--parse-headers ()
  "Parse RFC-2822-style headers from point to the blank line, return an alist.

Point must be at the start of the header block; point is left just after
the blank line that terminates the headers, at the start of the body."
  (let (headers)
    (while (and (not (eobp))
                (not (looking-at-p "\r?\n")))
      (when (looking-at "^\\([^:\n]+\\):[ \t]*\\(.*\\)$")
        (push (cons (downcase (match-string 1)) (match-string 2)) headers))
      (forward-line 1))
    ;; Skip the blank line separating headers from body, if present.
    (when (looking-at-p "\r?\n")
      (forward-line 1))
    (nreverse headers)))

(defun beemacs-transport--call (url)
  "Perform a synchronous GET against URL, returning `(status headers body)'.

Signals `beemacs-http-error' if the connection itself fails (DNS failure,
connection refused, timeout, etc.) -- it does NOT signal on a non-2xx HTTP
response, which is instead returned as data for the caller to inspect."
  (let (buffer)
    (unwind-protect
        (condition-case err
            (progn
              (setq buffer (url-retrieve-synchronously url t t 30))
              (unless buffer
                (signal 'beemacs-http-error
                        (list (format "connection failed for %s" url))))
              (with-current-buffer buffer
                (goto-char (point-min))
                (let* ((status-line (buffer-substring
                                      (point) (line-end-position)))
                       (status (beemacs-transport--parse-status-line
                                status-line)))
                  (forward-line 1)
                  (let* ((headers (beemacs-transport--parse-headers))
                         (body (buffer-substring (point) (point-max))))
                    (list status headers body)))))
          (beemacs-http-error (signal (car err) (cdr err)))
          (error
           (signal 'beemacs-http-error
                    (list (format "connection failed for %s: %s"
                                  url (error-message-string err))))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun beemacs-request (path &optional endpoint)
  "Perform an HTTP GET for PATH against ENDPOINT (or `beemacs-endpoint').

Returns a structured list `(status headers body)' on any HTTP response.
Signals `beemacs-http-error' on a non-2xx status or on a connection
failure -- never swallows a failure silently."
  (let* ((url (beemacs-transport--url path endpoint))
         (result (beemacs-transport--call url))
         (status (nth 0 result)))
    (if (and (>= status 200) (< status 300))
        result
      (signal 'beemacs-http-error
              (list (format "non-2xx response %d for %s" status url)
                    result)))))

(defun beemacs-transport-request (path &optional endpoint)
  "Perform an HTTP GET for PATH against ENDPOINT, returning the body string.

Signals `beemacs-http-error' on a non-2xx status or connection failure."
  (nth 2 (beemacs-request path endpoint)))

(defun beemacs-transport-get (path &optional endpoint)
  "Perform an HTTP GET against beehived PATH, returning the body string.

ENDPOINT optionally overrides `beemacs-endpoint' for this call only.
Signals `beemacs-http-error' on a non-2xx status or connection failure."
  (beemacs-transport-request path endpoint))

(defun beemacs-transport--form-encode (fields)
  "Encode FIELDS, an alist of (KEY . VALUE) strings, as `application/
x-www-form-urlencoded' body text, the same encoding Go's
`http.Request.FormValue' decodes on the server side."
  (mapconcat (lambda (kv)
               (format "%s=%s"
                       (url-hexify-string (format "%s" (car kv)))
                       (url-hexify-string (format "%s" (cdr kv)))))
             fields "&"))

(defun beemacs-transport-post-form (path fields &optional endpoint)
  "POST FIELDS (an alist of form-field name/value strings) to PATH.

Sets the request method to POST, a `Content-Type:
application/x-www-form-urlencoded' header, and a URL-encoded body built
from FIELDS -- the counterpart to `beemacs-transport-post' for
beehived's plain HTML/htmx form-POST routes (`POST /merge',
`POST /roi/{name}', `POST /secrets', ...) that read their input via Go's
`r.FormValue', not a JSON body. Returns the response body string on
success (a rendered HTML fragment for these routes, not JSON).
ENDPOINT optionally overrides `beemacs-endpoint' for this call only.
Signals `beemacs-http-error' on a non-2xx status or connection failure,
carrying the full `(status headers body)' response data so a caller can
recover the server's true (often plain-text, `http.Error'-produced)
failure message -- never assume success or synthesize a status."
  (let ((url-request-method "POST")
        (url-request-extra-headers
         '(("Content-Type" . "application/x-www-form-urlencoded")))
        (url-request-data
         (encode-coding-string (beemacs-transport--form-encode fields) 'utf-8)))
    (beemacs-transport-request path endpoint)))

(defun beemacs-transport-post (path json-string &optional endpoint)
  "POST JSON-STRING to beehived PATH, returning the response body string.

Sets the request method to POST, the body to the UTF-8 encoding of
JSON-STRING, and a `Content-Type: application/json' header, then
delegates to `beemacs-transport-request' (and, underneath it,
`beemacs-transport--call') exactly as `beemacs-transport-get' does --
these dynamic `url-request-*' bindings are how `url.el' distinguishes a
POST from its default GET, so no separate low-level call path is needed.
ENDPOINT optionally overrides `beemacs-endpoint' for this call only.
Signals `beemacs-http-error' on a non-2xx status or connection failure."
  (let ((url-request-method "POST")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url-request-data (encode-coding-string json-string 'utf-8)))
    (beemacs-transport-request path endpoint)))

(provide 'beemacs-transport)

;;; beemacs-transport.el ends here
