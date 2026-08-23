;;; beemacs-env.el --- Blue/green env view + deploy for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Client for beehived's blue/green environment panel
;; (`internal/web/env.go' + `internal/web/web.go's `envGet'/`envDeploy',
;; templates/env_panel.html):
;;
;;   GET  /env         -- render the current Active env + available Envs
;;   POST /env/deploy   -- switch the active env to `target', commit, and
;;                          return the refreshed panel
;;
;; Both routes are HTML-only (there is no JSON `env.json' surface today), so
;; this module parses the small, stable `env_panel.html' fragment with two
;; anchored regexes rather than pulling in an HTML parser -- `active:
;; <b>...</b>' and the `<select name="target">...</select>' option list.
;; Every command below reports the REAL parsed post-call state (or the real
;; transport/HTTP error) -- never a guessed/assumed success.
;;
;; `beemacs-instruction-update' mirrors the CLI's `beehive instruction
;; update' (refreshing a hive's managed AGENTS.md/HONEYBEE.md/skills files
;; from the binary's embedded defaults) against a `POST /instruction/update'
;; route. That route does not exist on the server yet -- see
;; `submodules/beehive/docs/tasks/instruction-update-endpoint.md' (filed
;; alongside this task) for the prerequisite that adds it. Until it ships,
;; this command's real, honest behavior is to surface the server's real
;; 404/error rather than fabricate success.

;;; Code:

(require 'beemacs-transport)

(defgroup beemacs-env nil
  "Blue/green environment view and deploy client for beemacs."
  :group 'beemacs
  :prefix "beemacs-env-")

(defun beemacs-env--parse (html)
  "Parse the `env_panel.html' fragment HTML into an alist.

Returns `((active . STRING) (envs . (STRING ...)))'. Signals `user-error'
if the expected `active: <b>...</b>' marker is not present -- an env panel
response beemacs cannot understand is not silently treated as \"blue\"."
  (unless (string-match "active:[ \t\n]*<b>\\([^<]*\\)</b>" html)
    (user-error "beemacs-env: could not find active env in server response"))
  (let ((active (match-string 1 html))
        (envs nil))
    (when (string-match "<select name=\"target\">\\(.*?\\)</select>" html)
      (let ((opts (match-string 1 html))
            (start 0))
        (while (string-match "<option>\\([^<]*\\)</option>" opts start)
          (push (match-string 1 opts) envs)
          (setq start (match-end 0)))
        (setq envs (nreverse envs))))
    (list (cons 'active active) (cons 'envs envs))))

(defun beemacs-env-state ()
  "Fetch and parse the current blue/green env state from beehived.

Performs `GET /env' and returns the `beemacs-env--parse' alist. Signals
`beemacs-http-error' on a transport/HTTP failure -- never returns a
guessed state."
  (beemacs-env--parse (beemacs-transport-get "/env")))

;;;###autoload
(defun beemacs-env-view ()
  "Display beehived's current active environment and available targets."
  (interactive)
  (let* ((state (beemacs-env-state))
         (active (alist-get 'active state))
         (envs (alist-get 'envs state)))
    (message "beemacs-env: active=%s available=%s"
             active (mapconcat #'identity envs ", "))
    state))

;;;###autoload
(defun beemacs-env-deploy (target)
  "Switch beehived's active environment to TARGET and report the real result.

Performs `POST /env/deploy' with TARGET passed as a URL query parameter
(the server's `envDeploy' reads it via `r.FormValue', which -- per
`net/http''s `ParseForm' -- always includes the raw URL query regardless
of the POST body's content type, so no form-encoded body is needed).
Parses the server's refreshed panel and reports the environment that is
ACTUALLY active afterward, not merely that TARGET was requested. Signals
`beemacs-http-error' on a transport/HTTP failure and `user-error' if the
deploy call succeeded but the reported active env does not match TARGET
-- both are the real backend result, not an assumption."
  (interactive
   (list (completing-read "Deploy environment: "
                           (alist-get 'envs (beemacs-env-state)) nil t)))
  (let* ((path (format "/env/deploy?target=%s" (url-hexify-string target)))
         (state (beemacs-env--parse (beemacs-transport-post path "{}")))
         (active (alist-get 'active state)))
    (if (equal active target)
        (progn
          (message "beemacs-env: deployed %s (active=%s)" target active)
          state)
      (user-error "beemacs-env: deploy to %s reported active=%s (deploy did not take effect)"
                  target active))))

;;;###autoload
(defun beemacs-instruction-update ()
  "Trigger a managed-instruction refresh (the `beehive instruction update' equivalent).

Performs `POST /instruction/update' and reports the server's real JSON
response (or its real transport/HTTP error) via `message'. There is
today no such route wired into beehived's `internal/web' server -- see
`submodules/beehive/docs/tasks/instruction-update-endpoint.md' -- so
until that prerequisite ships, this command's honest, real result is
whatever the server actually returns for the missing route (typically a
404), never a fabricated success."
  (interactive)
  (let ((body (beemacs-transport-post "/instruction/update" "{}")))
    (message "beemacs-instruction-update: %s" body)
    body))

(provide 'beemacs-env)

;;; beemacs-env.el ends here
