;;; t3-code-fleet-test.el --- Shared environment tests -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code)

(defmacro t3-code-test--with-catalog (&rest body)
  "Run BODY with a live simulated primary bridge and captured outbound records."
  (declare (indent 0) (debug t))
  `(let* ((t3-code--environments (make-hash-table :test #'equal))
          (root (t3-code-environment-create :id "local" :endpoint "http://localhost:3773"
                                           :directory temporary-file-directory
                                           :state 'ready :process 'bridge))
          sent)
     (puthash "local" root t3-code--environments)
     (cl-letf (((symbol-function 'process-live-p) (lambda (process) (eq process 'bridge)))
               ((symbol-function 'process-send-string)
                (lambda (_process text) (push (t3-code--json-parse text) sent))))
       ,@body)))

(defun t3-code-test--catalog-message (&rest records)
  "Return an authoritative snapshot containing RECORDS."
  (list :kind "snapshot" :payload (list :environments records)))

(ert-deftest t3-code-test-fleet-discovery-attaches-without-editor-credentials ()
  (t3-code-test--with-catalog
    (cl-letf (((symbol-function 't3-code--configured-credential)
               (lambda (&rest _) (ert-fail "Discovery requested a remote token"))))
      (t3-code-fleet--receive
       root (t3-code-test--catalog-message
             '(:id "remote" :label "Devbox" :endpoint "https://devbox:3773")))
      (let* ((remote (car (t3-code-environment-environments root)))
             (attach (car sent)))
        (should (equal (t3-code-environment-label remote) "Devbox"))
        (should (eq (t3-code-environment-parent remote) root))
        (should (equal (plist-get attach :operation) "environment.attach"))
        (should (equal (plist-get attach :input)
                       '(:environmentId "remote" :clientEnvironmentId "local::remote" :generation 1)))
        ;; Reconciliation is idempotent; an identical snapshot does not reattach.
        (t3-code-fleet--receive
         root (t3-code-test--catalog-message
               '(:id "remote" :label "Renamed" :endpoint "https://devbox:3773")))
        (should (= (length sent) 1))
        (should (equal (t3-code-environment-label remote) "Renamed"))
        ;; Disabling a remote is authoritative and disconnects only that child.
        (t3-code-fleet--receive
         root (t3-code-test--catalog-message
               '(:id "remote" :label "Devbox" :endpoint "https://devbox:3773" :enabled :false)))
        (should-not (t3-code-environment-environments root))
        (should-not (t3-code-environment-process remote))
        (should (equal (plist-get (car sent) :operation) "environment.detach"))
        (should (eq (t3-code-environment-process root) 'bridge))
        ;; Restoring an identity reuses open views' environment/subscriptions.
        (t3-code-fleet--receive
         root (t3-code-test--catalog-message
               '(:id "remote" :label "Devbox" :endpoint "https://devbox:3773")))
        (should (eq (car (t3-code-environment-environments root)) remote))
        (should (= (t3-code-environment-generation remote) 2))))))

(ert-deftest t3-code-test-fleet-routes-protocol-and-isolates-child-failure ()
  (t3-code-test--with-catalog
    (t3-code-fleet--receive
     root (t3-code-test--catalog-message '(:id "remote" :endpoint "https://devbox:3773")))
    (let ((remote (car (t3-code-environment-environments root))))
      (t3-code--dispatch
       root '(:kind "environment.message" :environmentId "local::remote" :generation 1
              :message (:kind "ready" :protocolVersion 1 :environmentId "local::remote"
                        :bridgeVersion "test" :pinnedT3Version "test" :serverVersion "test"
                        :capabilities (:shell t :threads t))))
      (should (eq (t3-code-environment-state remote) 'ready))
      (should (equal (plist-get (car sent) :kind) "environment.send"))
      (should (equal (plist-get (car sent) :environmentId) "local::remote"))
      (should (equal (plist-get (plist-get (car sent) :message) :stream) "shell"))
      ;; A late error from the previous generation must not close the new one.
      (t3-code--dispatch
       root '(:kind "environment.message" :environmentId "local::remote" :generation 0
              :message (:kind "fatal" :code "old" :message "Previous connection")))
      (should (eq (t3-code-environment-state remote) 'ready))
      (t3-code--dispatch
       root '(:kind "environment.message" :environmentId "local::remote" :generation 1
              :message (:kind "fatal" :code "remote-exit" :message "Remote disconnected")))
      (should (eq (t3-code-environment-state remote) 'disconnected))
      (should (eq (t3-code-environment-state root) 'ready))
      (should (eq (t3-code-environment-process root) 'bridge)))))

(ert-deftest t3-code-test-fleet-reconnect-resumes-remote-subscriptions ()
  (t3-code-test--with-catalog
    (let ((snapshot (t3-code-test--catalog-message
                     '(:id "remote" :endpoint "https://devbox:3773"))))
      (t3-code-fleet--receive root snapshot)
      (let ((remote (car (t3-code-environment-environments root))))
        (setf (t3-code-environment-process root) nil)
        (t3-code--set-state root 'disconnected)
        (should-not (t3-code-environment-process remote))
        (setf (t3-code-environment-process root) 'bridge)
        (t3-code--set-state root 'ready)
        (t3-code-fleet--receive root snapshot)
        (should (= (t3-code-environment-generation remote) 2))
        (should (seq-some (lambda (record)
                           (equal (plist-get record :operation) "environment.attach")) sent))
        (should (= (hash-table-count (t3-code-environment-subscriptions remote)) 1))))))

(ert-deftest t3-code-test-fleet-same-thread-id-has-distinct-rows-and-actions ()
  (t3-code-test--with-catalog
    (let* ((remote (t3-code-environment-create :id "remote" :endpoint "https://devbox:3773"
                                              :parent root :label "Devbox" :state 'ready))
           (projects '((:id "project" :name "App" :root "/srv/app"
                        :threads ((:id "same-thread" :title "Fix" :path "/srv/app"
                                   :status "idle" :settled :false)))))
           opened dispatched)
      (puthash "remote" remote t3-code--environments)
      (setf (t3-code-environment-environments root) (list remote)
            (t3-code-environment-shell root) (list :projects projects)
            (t3-code-environment-shell remote) (list :projects projects))
      (with-temp-buffer
        (t3-code-fleet-mode)
        (setq t3-code-fleet--root root)
        (t3-code-fleet--refresh)
        (should (assoc '("local" "same-thread") tabulated-list-entries))
        (should (assoc '("remote" "same-thread") tabulated-list-entries))
        (should (t3-code-dashboard--goto-id '("remote" "same-thread")))
        (cl-letf (((symbol-function 't3-code-thread-open)
                   (lambda (environment thread) (setq opened (list environment thread))))
                  ((symbol-function 't3-code-shell-dispatch)
                   (lambda (environment command &rest _) (setq dispatched (list environment command)))))
          (t3-code-fleet-open)
          (should (eq (car opened) remote))
          (t3-code-fleet-pin)
          (should (eq (car dispatched) remote))
          (should (equal (plist-get (cadr dispatched) :threadId) "same-thread"))
          (should (equal (tabulated-list-get-id) '("remote" "same-thread"))))))))

(ert-deftest t3-code-test-fleet-opening-a-chat-keeps-the-chat-intact ()
  (t3-code-test--with-catalog
    (save-window-excursion
      (let* ((t3-code--thread-buffers (make-hash-table :test #'equal))
             (t3-code-tramp-default-method "ssh")
             (t3-code-input-window-display 'hidden)
             (remote (t3-code-environment-create
                      :id "remote" :endpoint "https://devbox:3773" :parent root
                      :process 'bridge :state 'ready :capabilities '(:threads t :shell t)))
             (thread '(:id "open-thread" :title "Open remote" :path "/srv/app" :status "idle"))
             (fleet (generate-new-buffer " *fleet-open-test*")))
        (setf (t3-code-environment-environments root) (list remote)
              (t3-code-environment-shell remote)
              (list :projects (list (list :id "project" :name "App" :threads (list thread)))))
        (unwind-protect
            (progn
              (switch-to-buffer fleet)
              (t3-code-fleet-mode)
              (setq t3-code-fleet--root root)
              (t3-code-fleet--refresh)
              (should (t3-code-dashboard--goto-id '("remote" "open-thread")))
              (t3-code-fleet-open)
              ;; Opening changes the current buffer; the refresh belongs to
              ;; the ledger, not the newly selected conversation.
              (should (derived-mode-p 't3-code-thread-mode))
              (should (equal default-directory "/ssh:devbox:/srv/app/"))
              (should-not tabulated-list-entries)
              (with-current-buffer fleet
                (should (assoc '("remote" "open-thread") tabulated-list-entries))))
          (when-let* ((buffer (t3-code-thread-buffer remote "open-thread")))
            (kill-buffer buffer))
          (when (buffer-live-p fleet) (kill-buffer fleet)))))))

(ert-deftest t3-code-test-fleet-switcher-disambiguates-identical-labels ()
  (t3-code-test--with-catalog
    (let ((first (t3-code-environment-create :id "first" :label "Devbox" :parent root))
          (second (t3-code-environment-create :id "second" :label "Devbox" :parent root))
          (projects '((:name "App" :threads ((:id "same" :title "Fix" :status "idle")))))
          opened)
      (setf (t3-code-environment-environments root) (list first second)
            (t3-code-environment-shell first) (list :projects projects)
            (t3-code-environment-shell second) (list :projects projects))
      (cl-letf (((symbol-function 't3-code--current-environment) (lambda () root))
                ((symbol-function 'completing-read)
                 (lambda (_prompt table &rest _)
                   (let ((names (funcall table "" nil t)))
                     (should (= (length (delete-dups (copy-sequence names))) 2))
                     (cadr names))))
                ((symbol-function 't3-code-thread-open)
                 (lambda (environment &rest _) (setq opened environment))))
        (t3-code-switch-thread)
        (should (eq opened second))))))

(ert-deftest t3-code-test-fleet-starts-first-thread-on-environment-heading ()
  (t3-code-test--with-catalog
    (let ((remote (t3-code-environment-create :id "remote" :endpoint "https://devbox:3773"
                                              :parent root :state 'ready))
          selected)
      (setf (t3-code-environment-environments root) (list remote)
            (t3-code-environment-shell remote)
            '(:projects ((:id "project" :name "Empty" :root "/srv/empty" :threads nil))))
      (with-temp-buffer
        (t3-code-fleet-mode)
        (setq t3-code-fleet--root root)
        (t3-code-fleet--refresh)
        (should (t3-code-dashboard--goto-id '("remote" :environment)))
        (cl-letf (((symbol-function 't3-code-new-thread)
                   (lambda (&rest _) (setq selected (t3-code--current-environment)))))
          (t3-code-fleet-new-thread t)
          (should (eq selected remote)))))))

(ert-deftest t3-code-test-fleet-cold-tramp-entry-waits-for-catalog ()
  (t3-code-test--with-catalog
    (let (tick selected)
      (setf (t3-code-environment-capabilities root) '(:environments t))
      (cl-letf (((symbol-function 'run-at-time)
                 (lambda (_delay _repeat function) (setq tick function) 'timer))
                ((symbol-function 'cancel-timer) #'ignore)
                ((symbol-function 't3-code--when-shell)
                 (lambda (environment callback &rest _) (funcall callback t))))
        (t3-code--when-directory root "/ssh:me@devbox:/srv/app/"
                                 (lambda (environment _ready) (setq selected environment)))
        (funcall tick)
        (should-not selected)
        (t3-code-fleet--receive
         root (t3-code-test--catalog-message
               '(:id "remote" :endpoint "https://devbox:3773")))
        (funcall tick)
        (should (eq selected (car (t3-code-environment-environments root))))
        (should (equal (t3-code-file-prefix selected) "/ssh:me@devbox:"))))))

(ert-deftest t3-code-test-fleet-tramp-reentry-reattaches-failed-child ()
  (t3-code-test--with-catalog
    (let ((endpoint (t3-code-environment-endpoint root))
          (default-directory "/ssh:me@devbox:/srv/app/"))
      (puthash endpoint root t3-code--environments)
      (t3-code-fleet--receive
       root (t3-code-test--catalog-message
             '(:id "remote" :endpoint "https://devbox:3773")))
      (let ((remote (car (t3-code-environment-environments root))))
        (setf (t3-code-environment-process remote) nil)
        (t3-code--set-state remote 'disconnected)
        (cl-letf (((symbol-function 't3-code--configured-credential)
                   (lambda (&rest _) (ert-fail "Child reconnect asked for a token"))))
          (should (eq (t3-code--environment endpoint) remote)))
        (should (= (t3-code-environment-generation remote) 2))))))

(provide 't3-code-fleet-test)
;;; t3-code-fleet-test.el ends here
