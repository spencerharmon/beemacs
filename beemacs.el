;;; beemacs.el --- Emacs front-end for beehive -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;; Author: beehive swarm
;; Maintainer: beehive swarm
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: tools, convenience
;; URL: https://github.com/spencerharmon/beemacs

;; This file is not part of GNU Emacs.

;;; Commentary:

;; beemacs is an Emacs interface to a running `beehived' HTTP server — the
;; coordination layer for an autonomous self-improvement swarm (see the
;; `beehive' project).  It aims to be feature-complete with the beehive web
;; UI: browsing submodules, plans/tasks, ROIs, docs, live session
;; transcripts, human (NEEDS-HUMAN) escalation resolution, the agentic
;; editor, dances, hygiene, secrets, and swarm stats — all from Emacs.
;;
;; This file is the package entry point.  It requires the per-concern
;; modules and defines package-wide constants used across them.  Concrete
;; functionality lives in:
;;   - beemacs-transport.el — HTTP transport to a beehived instance
;;   - beemacs-api.el       — typed request/response wrappers over transport
;;   - beemacs-render.el    — buffer rendering / display logic
;;
;; Load the package with `use-package' + `straight.el':
;;
;;   (use-package beemacs
;;     :straight (beemacs :type git :host github
;;                         :repo "spencerharmon/beemacs")
;;     :custom (beemacs-server-url "http://127.0.0.1:8080"))

;;; Code:

(require 'beemacs-transport)
(require 'beemacs-api)
(require 'beemacs-render)

(defconst beemacs-version "0.1.0"
  "Current version of beemacs.")

(defgroup beemacs nil
  "Emacs front-end for beehive."
  :group 'tools
  :prefix "beemacs-")

(provide 'beemacs)

;;; beemacs.el ends here
