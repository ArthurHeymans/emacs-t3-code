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
