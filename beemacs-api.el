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

(provide 'beemacs-api)

;;; beemacs-api.el ends here
