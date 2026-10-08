;;; t3-code-integration-test.el --- Fake bridge integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-core)
(require 't3-code)

(defconst t3-code-test--root
  (file-name-directory (directory-file-name
                        (file-name-directory (or load-file-name buffer-file-name)))))

(defun t3-code-test--wait-until (predicate &optional timeout)
  "Wait until PREDICATE succeeds, for at most TIMEOUT seconds."
  (let ((deadline (+ (float-time) (or timeout 5))))
    (while (and (not (funcall predicate)) (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun t3-code-test--fake-command ()
  "Return the fake bridge command used by integration tests."
  (list "node" (expand-file-name "bridge/fake-t3e.mjs" t3-code-test--root)))

(defun t3-code-test--environment (name)
  "Create a local fake environment named NAME."
  (t3-code-environment-create
   :id (format "fake-%s" name) :endpoint (format "fake://%s" name)
   :directory t3-code-test--root))

(ert-deftest t3-code-test-fake-bridge-registers-project-and-updates-shell ()
  (skip-unless (executable-find "node"))
  (let* ((t3-code-bridge-command (t3-code-test--fake-command))
         (environment (t3-code-test--environment "register"))
         result failure)
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (t3-code-shell-ensure environment)
          (should (t3-code-test--wait-until
                   (lambda () (t3-code-environment-shell environment))))
          (should (t3-code-capability-p environment :projectRegistration))
          (t3-code-request
           environment "project.create"
           '(:commandId "register" :projectId "majutsu" :title "majutsu"
             :workspaceRoot "/home/me/src/majutsu")
           (lambda (value error) (setq result value failure error)))
          (should (t3-code-test--wait-until (lambda () (or result failure))))
          (should-not failure)
          (should (equal (plist-get (plist-get result :project) :root)
                         "/home/me/src/majutsu"))
          (should (t3-code-test--wait-until
                   (lambda () (t3-code-shell-find-project environment "majutsu"))))
          (should (equal (plist-get (t3-code-shell-project-for-directory
                                    environment "/home/me/src/majutsu/test/"
                                    "/home/me/src/majutsu/") :id)
                         "majutsu")))
      (t3-code-disconnect environment)
      (when-let* ((buffer (get-buffer (format " *t3:%s:stderr*"
                                             (t3-code-environment-id environment)))))
        (kill-buffer buffer)))))

(ert-deftest t3-code-test-fake-bridge-handshake-request-and-subscription ()
  (skip-unless (executable-find "node"))
  (let* ((t3-code-bridge-command (t3-code-test--fake-command))
         (environment (t3-code-test--environment "test"))
         request-result request-error mutation-result shell-snapshot thread-snapshot
         synchronized event-seen
         shell-reference thread-reference)
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (should (t3-code-test--wait-until
                   (lambda () (eq (t3-code-environment-state environment) 'ready))))
          (should (equal (t3-code-environment-bridge-version environment)
                         "0.1.0-fake"))
          (t3-code-request
           environment "server.getConfig" nil
           (lambda (result error)
             (setq request-result result request-error error)))
          (should (t3-code-test--wait-until (lambda () request-result)))
          (should-not request-error)
          (should (equal (plist-get request-result :serverVersion) "fake"))
          (t3-code-request
           environment "thread.runtimeMode.set"
           '(:threadId "thread-doh" :commandId "command-test"
             :runtimeMode "full-access")
           (lambda (result error)
             (should-not error)
             (setq mutation-result result)))
          (should (t3-code-test--wait-until (lambda () mutation-result)))
          (should (eq (plist-get mutation-result :accepted) t))
          (should (equal (plist-get mutation-result :operation)
                         "thread.runtimeMode.set"))
          (setq shell-reference
                (t3-code-subscribe
                 environment "shell" nil
                 (lambda (message)
                   (pcase (plist-get message :kind)
                     ("snapshot" (setq shell-snapshot (plist-get message :payload)))
                     ("synchronized" (setq synchronized t))
                     ("event" (setq event-seen t))))))
          (should (t3-code-test--wait-until
                   (lambda () (and shell-snapshot synchronized event-seen))))
          (should (equal (plist-get (car (plist-get shell-snapshot :projects)) :name)
                         "arthur/resolved-rs"))
          (dolist (thread (plist-get (car (plist-get shell-snapshot :projects)) :threads))
            (when (eq (plist-get thread :settled) t)
              (should-not (member (plist-get thread :status)
                                  '("running" "waiting-approval")))))
          (should (= (t3-code-subscription-sequence
                      (t3-code-subscription-reference-subscription shell-reference))
                     11))
          (setq thread-reference
                (t3-code-subscribe
                 environment "thread" "thread-doh"
                 (lambda (message)
                   (when (equal (plist-get message :kind) "snapshot")
                     (setq thread-snapshot (plist-get message :payload))))))
          (should (t3-code-test--wait-until (lambda () thread-snapshot)))
          (should (equal (plist-get (plist-get thread-snapshot :thread) :title)
                         "add doh fallback resolver"))
          (should (equal (mapcar (lambda (item) (plist-get item :type))
                                 (plist-get thread-snapshot :items))
                         '("user_message" "reasoning" "command_execution" "file_change"
                           "assistant_message" "approval_request" "user_input_request")))
          (should (equal (plist-get (t3-code-request-sync environment "provider.commands"
                                                          '(:instanceId "codex"))
                                    :skills)
                         '((:name "deploy" :description "Ship a release"
                            :userInvocationOnly :false))))
          (should (equal (mapcar (lambda (item) (plist-get item :id))
                                 (plist-get (t3-code-request-sync
                                             environment "thread.history"
                                             '(:threadId "thread-doh"
                                               :beforeItemId "item-user-1"))
                                            :items))
                         '("item-old-user" "item-old-answer"))))
      (when shell-reference (t3-code-unsubscribe environment shell-reference))
      (when thread-reference (t3-code-unsubscribe environment thread-reference))
      (t3-code-disconnect environment)
      (t3-code-test--wait-until
       (lambda () (null (t3-code-environment-process environment))) 1))))

(ert-deftest t3-code-test-restart-increments-generation-and-resumes-subscription ()
  (skip-unless (executable-find "node"))
  (let* ((t3-code-bridge-command (t3-code-test--fake-command))
         (environment (t3-code-test--environment "restart"))
         snapshot-sequences reference)
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (setq reference
                (t3-code-subscribe
                 environment "shell" nil
                 (lambda (message)
                   (when (equal (plist-get message :kind) "snapshot")
                     (push (plist-get message :sequence) snapshot-sequences)))))
          ;; The fake sends an event after its snapshot; resume the processed
          ;; event sequence, not whichever pipe chunk arrived first.
          (should (t3-code-test--wait-until
                   (lambda () (equal (t3-code-subscription-sequence
                                      (t3-code-subscription-reference-subscription reference))
                                     11))))
          (should (= (t3-code-environment-generation environment) 1))
          (t3-code-restart environment)
          (should (t3-code-test--wait-until
                   (lambda () (equal (t3-code-subscription-sequence
                                      (t3-code-subscription-reference-subscription reference))
                                     13))))
          (should (equal (nreverse snapshot-sequences) '(10 12)))
          (should (= (t3-code-subscription-sequence
                      (t3-code-subscription-reference-subscription reference))
                     13))
          (should (= (t3-code-environment-generation environment) 2))
          (should (eq (t3-code-environment-state environment) 'ready)))
      (when reference (t3-code-unsubscribe environment reference))
      (t3-code-disconnect environment))))

(ert-deftest t3-code-test-cancel-removes-pending-request ()
  (skip-unless (executable-find "node"))
  (let* ((t3-code-bridge-command (t3-code-test--fake-command))
         (environment (t3-code-test--environment "cancel")))
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (should (t3-code-test--wait-until
                   (lambda () (eq (t3-code-environment-state environment) 'ready))))
          (let ((id (t3-code-request environment "never" nil #'ignore)))
            (should (= (hash-table-count (t3-code-environment-pending environment)) 1))
            (t3-code-cancel-request environment id)
            (should (= (hash-table-count
                        (t3-code-environment-pending environment)) 0))))
      (t3-code-disconnect environment))))

(ert-deftest t3-code-test-protocol-mismatch-terminates-bridge ()
  (skip-unless (executable-find "node"))
  (let* ((process-environment (cons "T3E_FAKE_PROTOCOL_VERSION=2" process-environment))
         (t3-code-bridge-command (t3-code-test--fake-command))
         (environment (t3-code-test--environment "skew")))
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (should (t3-code-test--wait-until
                   (lambda () (eq (t3-code-environment-state environment)
                                  'disconnected))))
          (should (seq-some (lambda (text)
                              (string-match-p "Unsupported t3e protocol" text))
                            (t3-code-environment-diagnostics environment))))
      (t3-code-disconnect environment))))

(ert-deftest t3-code-test-exit-drops-pre-ready-request-queue ()
  (skip-unless (executable-find "node"))
  (let* ((t3-code-bridge-command '("node" "-e" "process.exit(7)"))
         (environment (t3-code-test--environment "exit"))
         callback-error)
    (unwind-protect
        (progn
          (t3-code-connect environment)
          (t3-code-request environment "server.getConfig" nil
                           (lambda (_result error) (setq callback-error error)))
          (should (t3-code-test--wait-until
                   (lambda () (eq (t3-code-environment-state environment)
                                  'disconnected))))
          (should callback-error)
          (should-not (t3-code-environment-outbound-queue environment))
          (should (= (hash-table-count
                      (t3-code-environment-pending environment)) 0)))
      (t3-code-disconnect environment))))

(ert-deftest t3-code-test-adjacent-t3code-bridge-speaks-protocol-v1 ()
  "Exercise the real checkout's bridge, not only the fake protocol fixture."
  (let* ((checkout (getenv "T3CODE_DIR"))
         (entry (and checkout (expand-file-name "apps/server/src/bin.ts" checkout))))
    (skip-unless (and (executable-find "node") entry (file-exists-p entry)))
    (let* ((process-environment (append '("T3_CLIENT_ACCESS_TOKEN="
                                          "T3_CLIENT_PAIRING_TOKEN=")
                                        process-environment))
           (t3-code-bridge-command (list "node" entry "client" "--stdio"))
           (environment (t3-code-environment-create
                         :id "checkout-smoke" :endpoint "http://127.0.0.1:1"
                         :directory checkout)))
      (unwind-protect
          (progn
            (t3-code-connect environment)
            (should (t3-code-test--wait-until
                     (lambda () (eq (t3-code-environment-state environment)
                                    'disconnected)) 15))
            (should (equal (plist-get (t3-code-environment-fatal-error environment)
                                      :code)
                           "environment-unreachable")))
        (t3-code-disconnect environment)))))

(ert-deftest t3-code-test-connect-uses-configured-token-or-asks ()
  (let ((t3-code--environments (make-hash-table :test #'equal))
        (t3-code-token 'ask)
        (t3-code-token-type 'pairing)
        (prompts 0)
        credentials)
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest _) (cl-incf prompts) "asked-token"))
              ((symbol-function 't3-code-restart)
               (lambda (_environment &optional credential)
                 (push credential credentials)))
              ((symbol-function 't3-code-dashboard) #'ignore))
      (t3-code--environment "http://127.0.0.1:3773")
      (let ((t3-code-token "configured-token")
            (t3-code-token-type 'bearer))
        (t3-code--environment "http://127.0.0.1:3773"))
      (let ((t3-code-token nil))
        (t3-code--environment "http://127.0.0.1:3773"))
      (should (equal (nreverse credentials)
                     '((pairing . "asked-token")
                       (bearer . "configured-token") nil)))
      (should (= prompts 1)))))

(ert-deftest t3-code-test-connect-reuses-live-bridge-without-prompt ()
  (let* ((t3-code--environments (make-hash-table :test #'equal))
         (t3-code-token 'ask)
         (environment (t3-code-get-environment "live" "live")))
    (setf (t3-code-environment-process environment) 'existing)
    (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process 'existing)))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (ert-fail "Should not prompt for live bridge")))
              ((symbol-function 't3-code-dashboard) #'ignore))
      (t3-code--environment "live"))))

(ert-deftest t3-code-test-connect-prompt-scopes-credentials-to-bridge ()
  (let ((t3-code--environments (make-hash-table :test #'equal))
        (process-environment (append '("T3_CLIENT_ACCESS_TOKEN=ambient-access"
                                       "T3_CLIENT_PAIRING_TOKEN=ambient-pairing")
                                     process-environment))
        observed)
    (cl-letf (((symbol-function 'read-string)
               (lambda (&rest _) "https://localhost:3773"))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) "fresh-secret"))
              ((symbol-function 't3-code-restart)
               (lambda (environment credential)
                 (push (list (t3-code-environment-endpoint environment)
                             credential
                             (getenv "T3_CLIENT_ACCESS_TOKEN")
                             (getenv "T3_CLIENT_PAIRING_TOKEN"))
                       observed)))
              ((symbol-function 't3-code-dashboard) #'ignore))
      (t3-code-connect-prompt)
      (t3-code-connect-prompt t))
    (should (equal (nreverse observed)
                   '(("https://localhost:3773" (pairing . "fresh-secret")
                      "ambient-access" "ambient-pairing")
                     ("https://localhost:3773" (bearer . "fresh-secret")
                      "ambient-access" "ambient-pairing"))))
    (should (equal (getenv "T3_CLIENT_ACCESS_TOKEN") "ambient-access"))
    (should (equal (getenv "T3_CLIENT_PAIRING_TOKEN") "ambient-pairing"))))

(ert-deftest t3-code-test-connect-credential-only-at-process-spawn ()
  (let ((process-environment (append '("T3_CLIENT_ACCESS_TOKEN=ambient-access"
                                       "T3_CLIENT_PAIRING_TOKEN=ambient-pairing")
                                     process-environment))
        (environment (t3-code-test--environment "credential"))
        observed)
    (dolist (credential '((pairing . "temporary") (bearer . "temporary")))
      (cl-letf (((symbol-function 'make-process)
                 (lambda (&rest _)
                   (push (list (getenv "T3_CLIENT_ACCESS_TOKEN")
                               (getenv "T3_CLIENT_PAIRING_TOKEN")) observed)
                   (error "Stop before starting the test process"))))
        (should-error (t3-code-connect environment credential)))
      (should (equal (getenv "T3_CLIENT_ACCESS_TOKEN") "ambient-access"))
      (should (equal (getenv "T3_CLIENT_PAIRING_TOKEN") "ambient-pairing")))
    (should (equal (nreverse observed)
                   '(("" "temporary") ("temporary" ""))))))

(ert-deftest t3-code-test-connect-prompt-rejects-credentialed-url ()
  (cl-letf (((symbol-function 'read-string)
             (lambda (&rest _) "https://user:secret@localhost:3773"))
            ((symbol-function 'read-passwd)
             (lambda (&rest _) (ert-fail "Must reject URL before prompting for token"))))
    (should-error (t3-code-connect-prompt) :type 'user-error)))

(ert-deftest t3-code-test-auth-source-credential-is-saved-after-acceptance ()
  (let* ((netrc (make-temp-file "t3-authinfo"))
         (auth-sources (list netrc))
         (auth-source-save-behavior t)
         (t3-code--pending-credential-saves (make-hash-table :test #'equal))
         (t3-code-token 'auth-source)
         (environment (t3-code-environment-create
                       :id "auth" :endpoint "http://127.0.0.1:3773"))
         (prompts 0))
    (unwind-protect
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _) (cl-incf prompts) "issued-bearer")))
          (auth-source-forget-all-cached)
          (should (equal (t3-code--configured-credential environment)
                         '(bearer . "issued-bearer")))
          ;; Nothing is written until the server accepts the token.
          (should (string-empty-p (with-temp-buffer (insert-file-contents netrc)
                                                    (buffer-string))))
          (setf (t3-code-environment-state environment) 'ready)
          (t3-code--credential-state-changed environment)
          (let ((saved (with-temp-buffer (insert-file-contents netrc) (buffer-string))))
            (dolist (field '("machine 127.0.0.1" "port 3773" "login t3-code"
                             "password issued-bearer"))
              (should (string-match-p (regexp-quote field) saved))))
          ;; The next connection reuses it without asking.
          (auth-source-forget-all-cached)
          (should (equal (t3-code--configured-credential environment)
                         '(bearer . "issued-bearer")))
          (should (= prompts 1)))
      (auth-source-forget-all-cached)
      (delete-file netrc))))

(ert-deftest t3-code-test-auth-source-credential-rejected-is-not-saved ()
  (let* ((netrc (make-temp-file "t3-authinfo"))
         (auth-sources (list netrc))
         (auth-source-save-behavior t)
         (t3-code--pending-credential-saves (make-hash-table :test #'equal))
         (t3-code-token 'auth-source)
         (environment (t3-code-environment-create
                       :id "auth-bad" :endpoint "http://127.0.0.1:3773")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) "wrong"))
                  ((symbol-function 'message) #'ignore))
          (auth-source-forget-all-cached)
          (t3-code--configured-credential environment)
          (setf (t3-code-environment-state environment) 'disconnected
                (t3-code-environment-fatal-error environment)
                '(:code "authentication-failed"))
          (t3-code--credential-state-changed environment)
          (should (string-empty-p (with-temp-buffer (insert-file-contents netrc)
                                                    (buffer-string))))
          (should (= (hash-table-count t3-code--pending-credential-saves) 0)))
      (auth-source-forget-all-cached)
      (delete-file netrc))))

(ert-deftest t3-code-test-auth-source-loopback-names-are-interchangeable ()
  (let* ((netrc (make-temp-file "t3-authinfo" nil nil
                                "machine 127.0.0.1 port 13773 login t3-code password stored\n"))
         (auth-sources (list netrc))
         (t3-code--pending-credential-saves (make-hash-table :test #'equal))
         (t3-code-token 'auth-source)
         (environment (t3-code-environment-create
                       :id "loopback" :endpoint "http://localhost:13773")))
    (unwind-protect
        (cl-letf (((symbol-function 'read-passwd)
                   (lambda (&rest _) (ert-fail "Must use the stored 127.0.0.1 entry"))))
          (auth-source-forget-all-cached)
          (should (equal (t3-code--configured-credential environment) '(bearer . "stored"))))
      (auth-source-forget-all-cached)
      (delete-file netrc))))

(provide 't3-code-integration-test)
;;; t3-code-integration-test.el ends here
