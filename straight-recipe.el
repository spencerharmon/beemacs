;;; -*- lexical-binding: t; -*-
;; straight.el recipe for beemacs.
;;
;; Add to your straight.el / use-package config:
;;
;;   (use-package beemacs
;;     :straight (beemacs :type git :host github
;;                         :repo "spencerharmon/beemacs"
;;                         :files ("*.el"))
;;     :custom (beemacs-endpoint "http://127.0.0.1:8080"))
;;
;; This file is consumed by `straight.el' recipe repositories / local
;; overrides; it is not loaded at runtime.
(:name beemacs
 :type git
 :host github
 :repo "spencerharmon/beemacs"
 :files ("*.el"))
