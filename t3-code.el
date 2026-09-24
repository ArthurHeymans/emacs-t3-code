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

(defcustom t3-code-token 'ask
  "Token to give the bridge when connecting.
`ask' prompts securely each time a bridge is started; a string uses that
value.  Nil inherits credentials from Emacs's process environment instead.
A configured string can be persisted by Customize, so treat it as a secret."
  :type '(choice (const :tag "Ask on connect" ask)
                 (string :tag "Token (stored in Emacs configuration)")
                 (const :tag "Use process environment" nil))
  :group 't3-code)

(defcustom t3-code-token-type 'pairing
  "Type of `t3-code-token': one-time pairing or reusable bearer token."
  :type '(choice (const :tag "Pairing" pairing)
                 (const :tag "Bearer" bearer))
  :group 't3-code)

(defun t3-code--configured-credential ()
  "Resolve the configured token for a new bridge process."
  (when t3-code-token
    (let ((token (if (eq t3-code-token 'ask)
                     (read-passwd (if (eq t3-code-token-type 'bearer)
                                      "T3 bearer token: " "T3 pairing token: "))
                   t3-code-token)))
      (unless (and (stringp token) (not (string-empty-p token)))
        (user-error "T3 token cannot be empty"))
      (cons t3-code-token-type token))))

(defun t3-code--reconnect (environment)
  "Restart ENVIRONMENT with the configured authentication setting."
  (t3-code-restart environment (t3-code--configured-credential)))

(defun t3-code (&optional endpoint)
  "Open the T3 dashboard for ENDPOINT.
Ask for a token on connect unless `t3-code-token' specifies one or uses the
environment.  Reopening an already-connected dashboard does not prompt."
  (interactive)
  (let* ((endpoint (or endpoint t3-code-default-endpoint))
         (environment (t3-code-get-environment endpoint endpoint default-directory)))
    (unless (process-live-p (t3-code-environment-process environment))
      (t3-code--reconnect environment))
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
         (t3-code-token nil)
         (t3-code-bridge-command
          (list "node" (expand-file-name "bridge/fake-t3e.mjs" root))))
    (t3-code "fake://demo")))

(provide 't3-code)
;;; t3-code.el ends here
