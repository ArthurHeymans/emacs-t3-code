;;; t3-code-thread-test.el --- Thread viewer tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-thread)

(ert-deftest t3-code-test-thread-renders-and-toggles-bounded-details ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--thread-id "thread-1"
          t3-code-thread--summary '(:title "Example" :status "running")
          t3-code-thread--payload
          '(:thread (:id "thread-1" :title "Example" :status "running"
                     :provider "codex" :model "gpt-5.3" :worktree "root")
            :items ((:id "user" :type "user_message" :status "completed"
                     :label "You" :text "Please fix it." :detail nil :streaming :false)
                    (:id "thinking" :type "reasoning" :status "completed"
                     :label "Thinking" :title "Approach" :text "Inspect first."
                     :detail nil :streaming :false)
                    (:id "command" :type "command_execution" :status "completed"
                     :label "Command" :title "Tests" :text "make check"
                     :detail "23 tests passed" :streaming :false))
            :truncated :false))
    (plist-put t3-code-thread--payload :truncated t)
    (t3-code-thread--refresh)
    (goto-char (+ (point-min) 10))
    (let ((position (point)))
      (t3-code-thread--refresh)
      (should (= (point) position)))
    (should (string-match-p "YOU" (buffer-string)))
    (should (string-match-p "Please fix it" (buffer-string)))
    (should-not (string-match-p "Inspect first" (buffer-string)))
    (should-not (string-match-p "23 tests passed" (buffer-string)))
    (should (t3-code-thread--goto-item "command"))
    (t3-code-thread-toggle-details)
    (should (string-match-p "23 tests passed" (buffer-string)))
    (should (equal (t3-code-thread--item-at-point) "command"))
    (t3-code-thread-toggle-details)
    (should-not (string-match-p "23 tests passed" (buffer-string)))))

(ert-deftest t3-code-test-thread-actions-send-normalized-mutations ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--environment
          (t3-code-environment-create
           :id "test" :state 'ready :capabilities '(:mutations t))
          t3-code-thread--thread-id "thread-1"
          t3-code-thread--payload
          '(:thread (:runtimeMode "full-access" :interactionMode "default")
            :items ((:id "approval" :type "approval_request" :status "waiting"
                     :actionId "request-1"))))
    (let (requests)
      (cl-letf (((symbol-function 't3-code-request)
                 (lambda (_environment operation input callback)
                   (push (list operation input) requests)
                   (funcall callback '(:sequence 1) nil))))
        (t3-code-thread-settle)
        (t3-code-thread-approve)
        (t3-code-thread-respond-approval "cancel"))
      (should (equal (mapcar #'car (reverse requests))
                     '("thread.settled.set" "thread.approval.respond"
                       "thread.approval.respond")))
      (should (eq (plist-get (cadr (nth 2 requests)) :settled) t))
      (should (equal (plist-get (cadr (car requests)) :decision) "cancel")))
    (let ((input (t3-code-thread--send-input "thread-1" "hello" "auto")))
      (should-not (plist-member input :runtimeMode))
      (should-not (plist-member input :interactionMode)))
    (should (eq (lookup-key t3-code-thread-mode-map (kbd "a"))
                #'t3-code-thread-actions))
    (should (eq (lookup-key t3-code-compose-mode-map (kbd "C-c C-c"))
                #'t3-code-compose-send))
    (let ((suffix (transient-get-suffix 't3-code-thread-actions "q")))
      (should (eq (plist-get (cdr suffix) :command)
                  #'t3-code-thread-compose-queue)))))

(ert-deftest t3-code-test-thread-shows-truncation-and-errors ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload '(:thread nil :items nil :truncated t
                                     :error "safe failure"))
    (t3-code-thread--refresh)
    (should (string-match-p "Could not load thread" (buffer-string)))
    (should (string-match-p "safe failure" (buffer-string)))))

(provide 't3-code-thread-test)
;;; t3-code-thread-test.el ends here
