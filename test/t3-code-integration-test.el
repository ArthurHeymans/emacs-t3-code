;;; t3-code-integration-test.el --- Fake bridge integration tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-core)

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
                         '("user_message" "reasoning" "command_execution"
                           "assistant_message" "approval_request"))))
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
          (should (t3-code-test--wait-until
                   (lambda () (= (length snapshot-sequences) 1))))
          (should (= (t3-code-environment-generation environment) 1))
          (t3-code-restart environment)
          (should (t3-code-test--wait-until
                   (lambda () (= (length snapshot-sequences) 2))))
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

(provide 't3-code-integration-test)
;;; t3-code-integration-test.el ends here
