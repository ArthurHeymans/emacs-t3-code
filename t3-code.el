;;; t3-code.el --- Emacs client for T3 Code  -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (transient "0.3.0"))
;; Keywords: tools, processes

;;; Commentary:

;; Entry points for an Emacs renderer of T3 Code state.  This initial release
;; establishes the versioned bridge protocol, shared connection lifecycle, fake
;; integration harness, shell dashboard, live thread viewer, and normalized
;; mutation commands.

;;; Code:

(require 'url-parse)
(require 't3-code-core)
(require 't3-code-thread)
(require 't3-code-dashboard)

(defcustom t3-code-default-endpoint "http://127.0.0.1:3773"
  "Default T3 server HTTP endpoint."
  :type 'string
  :group 't3-code)

(defun t3-code (&optional endpoint)
  "Open the T3 dashboard for ENDPOINT."
  (interactive)
  (let* ((endpoint (or endpoint t3-code-default-endpoint))
         (environment (t3-code-get-environment endpoint endpoint default-directory)))
    (t3-code-connect environment)
    (t3-code-dashboard environment)))

(defun t3-code-connect-prompt (&optional bearer)
  "Prompt for a server URL and token, then open its dashboard.
By default TOKEN is a one-time pairing token.  With prefix argument
BEARER, treat it as an existing bearer access token instead.  The token is
passed only to the newly started bridge process; it is not saved in Emacs's
environment or the T3 environment state.  Reconnecting requires a new token."
  (interactive "P")
  (let* ((endpoint (string-trim (read-string "T3 server URL: "
                                             t3-code-default-endpoint)))
         (url (url-generic-parse-url endpoint)))
    (unless (and (member (url-type url) '("http" "https"))
                 (url-host url)
                 (not (url-user url)) (not (url-password url)))
      (user-error "Enter an HTTP(S) URL without embedded credentials"))
    (let* ((token (read-passwd (if bearer "T3 bearer token: "
                                 "T3 pairing token (C-u for bearer): ")))
           (environment (t3-code-get-environment endpoint endpoint default-directory)))
      (when (string-empty-p token) (user-error "Token cannot be empty"))
      ;; Restart also re-subscribes existing dashboard and thread views.
      (t3-code-restart environment (cons (if bearer 'bearer 'pairing) token))
      (t3-code-dashboard environment))))

(defun t3-code-demo ()
  "Open the dashboard against the repository's deterministic fake bridge."
  (interactive)
  (let* ((root (file-name-directory (or load-file-name
                                        (locate-library "t3-code")
                                        buffer-file-name)))
         (t3-code-bridge-command
          (list "node" (expand-file-name "bridge/fake-t3e.mjs" root))))
    (t3-code "fake://demo")))

(provide 't3-code)
;;; t3-code.el ends here
