;;; beemacs-transport.el --- HTTP transport for beemacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 beemacs contributors

;;; Commentary:

;; Low-level HTTP transport to a running `beehived' instance.  This module
;; owns the connection details (base URL, request construction, response
;; parsing) and exposes a small, synchronous/callback-based API consumed by
;; `beemacs-api.el'.  No other module should build an HTTP request directly.

;;; Code:

(require 'url)
(require 'json)

(defgroup beemacs-transport nil
  "Transport layer for talking to a beehived server."
  :group 'beemacs
  :prefix "beemacs-transport-")

(defcustom beemacs-server-url "http://127.0.0.1:8080"
  "Base URL of the beehived HTTP server."
  :group 'beemacs-transport
  :type 'string)

(defun beemacs-transport--url (path)
  "Build a full request URL for PATH against `beemacs-server-url'."
  (concat (string-remove-suffix "/" beemacs-server-url)
          "/"
          (string-remove-prefix "/" path)))

(defun beemacs-transport-get (path &optional callback)
  "Perform an HTTP GET against beehived PATH.

If CALLBACK is nil, block and return the response body as a string.
If CALLBACK is non-nil, it is called asynchronously with the response
body as a string once the request completes."
  (let ((url (beemacs-transport--url path)))
    (if callback
        (url-retrieve
         url
         (lambda (_status)
           (goto-char (point-min))
           (re-search-forward "\n\n" nil t)
           (funcall callback (buffer-substring (point) (point-max)))))
      (with-current-buffer (url-retrieve-synchronously url)
        (goto-char (point-min))
        (re-search-forward "\n\n" nil t)
        (prog1 (buffer-substring (point) (point-max))
          (kill-buffer))))))

(provide 'beemacs-transport)

;;; beemacs-transport.el ends here
