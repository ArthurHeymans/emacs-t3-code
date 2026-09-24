;;; t3-code-dashboard-test.el --- Dashboard tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-dashboard)

(ert-deftest t3-code-test-dashboard-builds-stable-thread-rows ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "thread-1" :title "First" :status "running"
                        :provider "pi" :model "model" :worktree "root"
                        :additions 3 :deletions 1)
                       (:id "thread-2" :title "Second" :status "idle")))))
    (t3-code-dashboard--refresh)
    (goto-char (point-min))
    (should (t3-code-dashboard--goto-id "thread-2"))
    (should (equal (tabulated-list-get-id) "thread-2"))
    (should (string-match-p "Second" (buffer-string)))
    (should (= (nth 1 (aref tabulated-list-format 0)) 36))
    (should (eq (lookup-key t3-code-dashboard-mode-map (kbd "TAB"))
                #'t3-code-dashboard-toggle-at-point))
    (let ((running (aref (cadr (car tabulated-list-entries)) 1)))
      (should (equal (substring-no-properties running) "● WORK"))
      (should (eq (get-text-property 0 'face running)
                  't3-code-dashboard-running-face)))))

(ert-deftest t3-code-test-dashboard-ret-opens-thread-viewer ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--environment 'environment
          t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "thread-1" :title "First" :status "idle")))))
    (t3-code-dashboard--refresh)
    (should (t3-code-dashboard--goto-id "thread-1"))
    (let (opened)
      (cl-letf (((symbol-function 't3-code-thread-open)
                 (lambda (environment thread)
                   (setq opened (list environment (plist-get thread :id))))))
        (t3-code-dashboard-open-thread))
      (should (equal opened '(environment "thread-1"))))))

(ert-deftest t3-code-test-dashboard-refresh-preserves-row ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "thread-1" :title "First" :status "running")
                       (:id "thread-2" :title "Second" :status "idle")))))
    (t3-code-dashboard--refresh)
    (should (t3-code-dashboard--goto-id "thread-2"))
    (setq t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "thread-2" :title "Second updated" :status "running")
                       (:id "thread-3" :title "Third" :status "idle")))))
    (t3-code-dashboard--refresh)
    (should (equal (tabulated-list-get-id) "thread-2"))))

(ert-deftest t3-code-test-dashboard-empty-replacement-clears-rows ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "thread-1" :title "First" :status "running")))))
    (t3-code-dashboard--refresh)
    (t3-code-dashboard--on-shell-message
     '(:kind "event" :payload (:projects ())))
    (should-not tabulated-list-entries)))

(ert-deftest t3-code-test-dashboard-warns-when-shell-is-truncated ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--environment
          (t3-code-environment-create :id "test" :endpoint "fake://test"))
    (t3-code-dashboard--on-shell-message
     '(:kind "snapshot" :payload
       (:truncated t
        :projects ((:id "project" :name "owner/repo"
                    :threads ((:id "thread" :title "Visible" :status "idle")))))))
    (should (equal (mapcar #'car tabulated-list-entries)
                   (list t3-code-dashboard--truncated-heading-id "thread")))
    (should (string-match-p "View truncated by bridge limits" (buffer-string)))
    (should (string-match-p "truncated" (t3-code-dashboard--header-line)))))

(ert-deftest t3-code-test-dashboard-shows-settled-threads-by-default ()
  (let ((t3-code-dashboard-collapse-settled nil))
    (with-temp-buffer
      (t3-code-dashboard-mode)
      (setq t3-code-dashboard--projects
            '((:id "project" :name "owner/repo"
               :threads ((:id "active" :title "Active" :status "idle" :settled :false)
                         (:id "settled" :title "Settled" :status "idle" :settled t)))))
      (t3-code-dashboard--refresh)
      (should (equal (mapcar #'car tabulated-list-entries)
                     (list "active" t3-code-dashboard--settled-heading-id "settled")))
      (should (t3-code-dashboard--goto-id t3-code-dashboard--settled-heading-id))
      (should (string-match-p "▾ Settled (1)" (buffer-string)))
      (t3-code-dashboard-toggle-at-point)
      (should (equal (mapcar #'car tabulated-list-entries)
                     (list "active" t3-code-dashboard--settled-heading-id)))
      (should (string-match-p "▸ Settled (1)" (buffer-string)))
      (should-not (t3-code-dashboard--goto-id "settled")))))

(ert-deftest t3-code-test-dashboard-groups-agents-below-parent-collapsed ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--projects
          '((:id "project" :name "owner/repo"
             :threads ((:id "parent" :title "Parent" :status "running"
                        :parentThreadId nil :relationshipToParent nil)
                       (:id "agent" :title "Agent" :status "idle"
                        :parentThreadId "parent" :relationshipToParent "subagent")
                       (:id "other" :title "Other" :status "idle"
                        :parentThreadId nil :relationshipToParent nil)))))
    (t3-code-dashboard--refresh)
    (let ((heading (t3-code-dashboard--agents-heading-id "parent")))
      (should (equal (mapcar #'car tabulated-list-entries)
                     (list "parent" heading "other")))
      (should (t3-code-dashboard--goto-id heading))
      (t3-code-dashboard-toggle-at-point)
      (should (equal (mapcar #'car tabulated-list-entries)
                     (list "parent" heading "agent" "other")))
      (should (equal (substring-no-properties
                      (aref (cadr (nth 2 tabulated-list-entries)) 3))
                     " ↳ Agent")))))

(ert-deftest t3-code-test-dashboard-reopen-does-not-leak-reference ()
  (let* ((environment (t3-code-environment-create
                       :id "dashboard-reopen" :state 'connecting))
         buffer subscription)
    (unwind-protect
        (save-window-excursion
          (setq buffer (t3-code-dashboard environment))
          (t3-code-dashboard environment)
          (setq subscription
                (t3-code-subscription-reference-subscription
                 (buffer-local-value 't3-code-dashboard--subscription buffer)))
          (should (= (hash-table-count
                      (t3-code-subscription-callbacks subscription)) 1))
          (kill-buffer buffer)
          (setq buffer nil)
          (should (= (hash-table-count
                      (t3-code-environment-subscriptions environment)) 0)))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(ert-deftest t3-code-test-dashboard-shows-bridge-fatal-reason ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (let ((environment (t3-code-environment-create
                        :id "test" :endpoint "http://127.0.0.1:3773"
                        :state 'disconnected
                        :fatal-error '(:code "authentication-required"
                                       :message "Set T3_CLIENT_PAIRING_TOKEN"))))
      (setq t3-code-dashboard--environment environment)
      (should (string-match-p "Set T3_CLIENT_PAIRING_TOKEN"
                              (t3-code-dashboard--header-line)))
      (t3-code--dispatch environment
                         '(:kind "fatal" :code "protocol-mismatch"
                           :message "Update the bridge"))
      (should (equal (plist-get (t3-code-environment-fatal-error environment) :code)
                     "protocol-mismatch"))
      (should (string-match-p "Update the bridge"
                              (t3-code-dashboard--header-line))))))

(ert-deftest t3-code-test-dashboard-reconnect-uses-token-setting ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (let ((t3-code-token "configured-token")
          (t3-code-token-type 'bearer)
          (t3-code-dashboard--environment
           (t3-code-environment-create :id "test" :endpoint "http://localhost:3773"))
          credential)
      (cl-letf (((symbol-function 't3-code-restart)
                 (lambda (_environment &optional supplied)
                   (setq credential supplied))))
        (t3-code-dashboard-reconnect t))
      (should (equal credential '(bearer . "configured-token"))))))

(ert-deftest t3-code-test-dashboard-refresh-does-not-restart-or-ask ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (let* ((environment (t3-code-environment-create :id "test" :state 'ready))
           (reference (t3-code-subscription-reference-create :token "listener"))
           (t3-code-dashboard--environment environment)
           (t3-code-dashboard--subscription reference)
           refreshed)
      (cl-letf (((symbol-function 't3-code-refresh-subscription)
                 (lambda (env ref) (setq refreshed (list env ref))))
                ((symbol-function 't3-code--reconnect)
                 (lambda (&rest _) (ert-fail "Refresh restarted the bridge")))
                ((symbol-function 'read-passwd)
                 (lambda (&rest _) (ert-fail "Refresh requested another token"))))
        (t3-code-dashboard-reconnect))
      (should (equal refreshed (list environment reference))))))

(ert-deftest t3-code-test-dashboard-plain-refresh-disconnected-never-asks ()
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--environment
          (t3-code-environment-create :id "test" :state 'disconnected))
    (cl-letf (((symbol-function 't3-code--reconnect)
               (lambda (&rest _) (ert-fail "Refresh restarted the bridge")))
              ((symbol-function 'read-passwd)
               (lambda (&rest _) (ert-fail "Refresh requested a token"))))
      (should-error (t3-code-dashboard-reconnect) :type 'user-error))))

(provide 't3-code-dashboard-test)
;;; t3-code-dashboard-test.el ends here
