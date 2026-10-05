;;; t3-code-core-test.el --- Tests for t3-code-core  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-core)

(ert-deftest t3-code-test-accumulates-split-lines ()
  (let* ((first (t3-code--accumulate-line-chunks nil "{\"kind\":\"rea"))
         (second (t3-code--accumulate-line-chunks
                  (cdr first) "dy\"}\n{\"kind\":\"event\"}\npartial")))
    (should-not (car first))
    (should (equal (car second)
                   '("{\"kind\":\"ready\"}" "{\"kind\":\"event\"}")))
    (should (equal (t3-code--line-chunks-string (cdr second)) "partial"))))

(ert-deftest t3-code-test-json-round-trip ()
  (let* ((object '(:kind "request" :id "request-1"
                   :input (:enabled t :disabled :false :missing nil)))
         (parsed (t3-code--json-parse (string-trim-right
                                      (t3-code--json-line object)))))
    (should (equal (plist-get parsed :kind) "request"))
    (should (eq (plist-get (plist-get parsed :input) :enabled) t))
    (should (eq (plist-get (plist-get parsed :input) :disabled) :false))
    (should-not (plist-get (plist-get parsed :input) :missing))))

(ert-deftest t3-code-test-ready-validates-generation-independent-handshake ()
  (let ((environment (t3-code-environment-create :id "local")))
    (t3-code--handle-ready
     environment
     '(:kind "ready" :protocolVersion 1 :environmentId "local"
       :bridgeVersion "1" :pinnedT3Version "abc" :serverVersion "2"
       :capabilities (:shell t)))
    (should (eq (t3-code-environment-state environment) 'ready))
    (should (equal (t3-code-environment-pinned-t3-version environment) "abc"))))

(ert-deftest t3-code-test-ready-rejects-protocol-skew ()
  (let ((environment (t3-code-environment-create :id "local")))
    (should-error
     (t3-code--handle-ready
      environment '(:kind "ready" :protocolVersion 2 :environmentId "local")))))

(ert-deftest t3-code-test-request-correlation ()
  (let ((environment (t3-code-environment-create :id "local"))
        received)
    (puthash "request-1" (lambda (result error)
                           (setq received (list result error)))
             (t3-code-environment-pending environment))
    (t3-code--handle-response
     environment '(:kind "response" :id "request-1" :result (:ok t)))
    (should (equal received '((:ok t) nil)))
    (should (= (hash-table-count (t3-code-environment-pending environment)) 0))))

(ert-deftest t3-code-test-subscription-ignores-duplicates-and-stale-generations ()
  (let* ((environment (t3-code-environment-create :id "local" :generation 2))
         (seen nil)
         (subscription
          (t3-code-subscription-create
           :id "shell:null" :kind "shell" :sequence 10)))
    (puthash "listener" (lambda (message) (push message seen))
             (t3-code-subscription-callbacks subscription))
    (puthash "shell:null" subscription
             (t3-code-environment-subscriptions environment))
    (t3-code--handle-subscription-message
     environment '(:kind "event" :subscriptionId "shell:null"
                    :generation 1 :sequence 11 :payload (:old t)))
    (t3-code--handle-subscription-message
     environment '(:kind "event" :subscriptionId "shell:null"
                    :generation 2 :sequence 10 :payload (:duplicate t)))
    (t3-code--handle-subscription-message
     environment '(:kind "event" :subscriptionId "shell:null"
                    :generation 2 :sequence 11 :payload (:new t)))
    (should (= (length seen) 1))
    (should (= (t3-code-subscription-sequence subscription) 11))))

(ert-deftest t3-code-test-synchronized-same-sequence-is-delivered ()
  (let* ((environment (t3-code-environment-create
                       :id "local" :generation 2 :state 'repairing))
         (seen nil)
         (subscription
          (t3-code-subscription-create
           :id "shell:null" :kind "shell" :sequence 10)))
    (puthash "listener" (lambda (message) (push (plist-get message :kind) seen))
             (t3-code-subscription-callbacks subscription))
    (puthash "shell:null" subscription
             (t3-code-environment-subscriptions environment))
    (t3-code--handle-subscription-message
     environment '(:kind "synchronized" :subscriptionId "shell:null"
                    :generation 2 :sequence 10))
    (should (equal seen '("synchronized")))
    (should (t3-code-subscription-synchronized subscription))
    (should (eq (t3-code-environment-state environment) 'ready))))

(ert-deftest t3-code-test-sequence-gap-requests-authoritative-snapshot ()
  (let* ((environment (t3-code-environment-create
                       :id "local" :generation 2 :state 'ready))
         (sent nil)
         (subscription
          (t3-code-subscription-create
           :id "shell:null" :kind "shell" :sequence 10)))
    (puthash "shell:null" subscription
             (t3-code-environment-subscriptions environment))
    (cl-letf (((symbol-function 't3-code--send-now)
               (lambda (_environment record) (push record sent))))
      (t3-code--handle-subscription-message
       environment '(:kind "event" :subscriptionId "shell:null"
                      :generation 2 :sequence 12)))
    (should (eq (t3-code-environment-state environment) 'repairing))
    (should-not (t3-code-subscription-sequence subscription))
    (should (equal (mapcar (lambda (record) (plist-get record :kind))
                           (nreverse sent))
                   '("unsubscribe" "subscribe")))
    (should-not (plist-member (car sent) :resumeSequence))))

(ert-deftest t3-code-test-refresh-resnapshots-without-reconnecting ()
  (let* ((environment (t3-code-environment-create
                       :id "local" :generation 1 :state 'ready))
         (subscription (t3-code-subscription-create
                        :id "shell:null" :kind "shell" :sequence 41))
         (reference (t3-code-subscription-reference-create
                     :subscription subscription :token "listener"))
         sent received)
    (puthash "shell:null" subscription
             (t3-code-environment-subscriptions environment))
    (puthash "listener" (lambda (message) (push (plist-get message :sequence) received))
             (t3-code-subscription-callbacks subscription))
    (cl-letf (((symbol-function 't3-code--send-now)
               (lambda (_environment record) (push record sent))))
      (t3-code-refresh-subscription environment reference)
      (should (equal (mapcar (lambda (record) (plist-get record :kind))
                             (nreverse sent)) '("unsubscribe" "subscribe")))
      (should-not (plist-member (car sent) :resumeSequence))
      ;; Ignore a late event from the old stream while waiting for its replacement.
      (t3-code--handle-subscription-message
       environment '(:kind "event" :subscriptionId "shell:null"
                     :generation 1 :sequence 42))
      (should-not received)
      (t3-code--handle-subscription-message
       environment '(:kind "snapshot" :subscriptionId "shell:null"
                     :generation 1 :sequence 1 :payload (:projects ())))
      (t3-code--handle-subscription-message
       environment '(:kind "event" :subscriptionId "shell:null"
                     :generation 1 :sequence 2 :payload (:projects ()))))
    (should (equal (nreverse received) '(1 2)))
    (should (= (t3-code-subscription-sequence subscription) 2))
    (should (eq (t3-code-environment-state environment) 'ready))))

(ert-deftest t3-code-test-reference-counted-subscriptions ()
  (let* ((environment (t3-code-environment-create :id "local" :state 'connecting))
         (first (t3-code-subscribe environment "shell" nil #'ignore))
         (second (t3-code-subscribe environment "shell" nil #'ignore))
         (subscription (t3-code-subscription-reference-subscription first)))
    (should (eq subscription
                (t3-code-subscription-reference-subscription second)))
    (should (= (hash-table-count
                (t3-code-subscription-callbacks subscription)) 2))
    (t3-code-unsubscribe environment first)
    (should (= (hash-table-count
                (t3-code-subscription-callbacks subscription)) 1))
    (should (= (hash-table-count
                (t3-code-environment-subscriptions environment)) 1))
    (t3-code-unsubscribe environment second)
    (should (= (hash-table-count
                (t3-code-environment-subscriptions environment)) 0))))

(ert-deftest t3-code-test-stale-process-output-cannot-replace-state ()
  (let* ((environment (t3-code-environment-create
                       :id "local" :state 'connecting :generation 2))
         (old (make-process :name "t3-old" :command '("cat") :noquery t))
         (current (make-process :name "t3-current" :command '("cat") :noquery t)))
    (unwind-protect
        (progn
          (process-put old 't3-code-environment environment)
          (process-put current 't3-code-environment environment)
          (setf (t3-code-environment-process environment) current)
          (t3-code--process-filter
           old
           "{\"kind\":\"ready\",\"protocolVersion\":1,\"environmentId\":\"local\",\"bridgeVersion\":\"OLD\"}\n")
          (should (eq (t3-code-environment-state environment) 'connecting))
          (should-not (t3-code-environment-bridge-version environment)))
      (delete-process old)
      (delete-process current))))

(ert-deftest t3-code-test-oversized-output-terminates-process ()
  (let* ((t3-code-max-line-bytes 10)
         (environment (t3-code-environment-create :id "local"))
         (process (make-process :name "t3-oversized" :command '("cat") :noquery t)))
    (process-put process 't3-code-environment environment)
    (setf (t3-code-environment-process environment) process)
    (t3-code--process-filter process "{\"kind\":\"far-too-long\"}\n")
    (should-not (process-live-p process))
    (should (string-match-p "oversized bridge line"
                            (car (t3-code-environment-diagnostics environment))))))

(ert-deftest t3-code-test-malformed-output-is-diagnosed ()
  (let* ((environment (t3-code-environment-create :id "local"))
         (process (make-process :name "t3-malformed" :command '("cat") :noquery t)))
    (unwind-protect
        (progn
          (process-put process 't3-code-environment environment)
          (setf (t3-code-environment-process environment) process)
          (t3-code--process-filter process "not-json\n")
          (should (string-match-p "Malformed bridge JSON"
                                  (car (t3-code-environment-diagnostics environment)))))
      (delete-process process))))

(ert-deftest t3-code-test-refresh-watchdog-resubscribes-until-snapshot ()
  (let* ((environment (t3-code-environment-create
                       :id "local" :generation 1 :state 'ready :process 'bridge))
         (subscription (t3-code-subscription-create
                        :id "shell:null" :kind "shell" :sequence 41))
         (reference (t3-code-subscription-reference-create
                     :subscription subscription :token "listener"))
         sent timers)
    (puthash "shell:null" subscription (t3-code-environment-subscriptions environment))
    (cl-letf (((symbol-function 't3-code--send-now)
               (lambda (_environment record) (push (plist-get record :kind) sent)))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat function) (push function timers)))
              ((symbol-function 'process-live-p) (lambda (process) (eq process 'bridge))))
      (t3-code-refresh-subscription environment reference)
      (should (equal sent '("subscribe" "unsubscribe")))
      ;; The bridge lost the snapshot: the watchdog asks again.
      (funcall (pop timers))
      (should (equal (length sent) 4))
      ;; While the bridge reconnects it only keeps watching.
      (setf (t3-code-environment-state environment) 'retrying)
      (funcall (pop timers))
      (should (equal (length sent) 4))
      (setf (t3-code-environment-state environment) 'ready)
      (t3-code--handle-subscription-message
       environment '(:kind "snapshot" :subscriptionId "shell:null"
                     :generation 1 :sequence 1 :payload (:projects ())))
      (funcall (pop timers))
      (should (equal (length sent) 4))
      (should-not timers))))

(ert-deftest t3-code-test-core-maps-server-paths-to-emacs-files ()
  (let ((local (t3-code-environment-create :endpoint "http://127.0.0.1:3773"))
        (remote (t3-code-environment-create :endpoint "https://devbox.example.com:3773"))
        (forwarded (t3-code-environment-create :endpoint "http://localhost:4000"))
        (t3-code-file-prefixes '(("http://localhost:4000" . "/ssh:devbox:")))
        (t3-code-tramp-default-method "sshx"))
    (should-not (t3-code-file-prefix local))
    (should (equal (t3-code-local-file local "/src/app") "/src/app"))
    (should (equal (t3-code-server-path local "/src/app/") "/src/app/"))
    (should-not (t3-code-server-path local "/ssh:devbox:/src/app/"))
    ;; A remote host uses the default method until a TRAMP buffer reaches it.
    (should (equal (t3-code-local-file remote "/src/app") "/sshx:devbox.example.com:/src/app"))
    (t3-code-note-directory remote "/ssh:me@devbox:/src/app/")
    (should (equal (t3-code-local-file remote "/src/app") "/ssh:me@devbox:/src/app"))
    (should (equal (t3-code-server-path remote "/ssh:me@devbox:/src/app/x") "/src/app/x"))
    (should-not (t3-code-server-path remote "/src/app/x"))
    (should-not (t3-code-server-path remote "/ssh:elsewhere:/src/app/x"))
    ;; A forwarded port is configured explicitly.
    (should (equal (t3-code-local-file forwarded "/src/app") "/ssh:devbox:/src/app"))
    (t3-code-note-directory forwarded "/ssh:me@devbox:/src/")
    (should (equal (t3-code-local-file forwarded "/src/app") "/ssh:devbox:/src/app"))))

(provide 't3-code-core-test)
;;; t3-code-core-test.el ends here
