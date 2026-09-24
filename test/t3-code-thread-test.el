;;; t3-code-thread-test.el --- Thread viewer tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-thread)

(defun t3-code-test--visible-text ()
  "Extract only the displayed transcript text."
  (apply #'string (cl-loop for position from (point-min) below (point-max)
                          unless (invisible-p position) collect (char-after position))))

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
    (should (string-match-p "You" (t3-code-test--visible-text)))
    (should (string-match-p "Please fix it" (buffer-string)))
    (should-not (string-match-p "Inspect first" (t3-code-test--visible-text)))
    (should-not (string-match-p "23 tests passed" (t3-code-test--visible-text)))
    (should (t3-code-thread--goto-item "command"))
    (t3-code-thread-toggle-details)
    (should (string-match-p "23 tests passed" (t3-code-test--visible-text)))
    (should (equal (t3-code-thread--item-at-point) "command"))
    (t3-code-thread-toggle-details)
    (should-not (string-match-p "23 tests passed" (t3-code-test--visible-text)))))

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

(ert-deftest t3-code-test-thread-retains-last-good-snapshot-on-stream-error ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (let ((good '(:thread (:id "thread-1" :title "Example")
                  :items ((:id "answer" :type "assistant_message"
                           :label "Assistant" :text "Still here"))))
          (failure '(:thread nil :items nil :error "SocketCloseError: 1005")))
      (t3-code-thread--receive (list :kind "snapshot" :payload failure))
      (should (string-match-p "Could not load thread" (buffer-string)))
      (let ((tick (buffer-chars-modified-tick)))
        (t3-code-thread--receive (list :kind "snapshot" :payload failure))
        (should (= tick (buffer-chars-modified-tick))))
      (t3-code-thread--receive (list :kind "snapshot" :payload good))
      (should (string-match-p "Still here" (buffer-string)))
      (should (string-match-p "retrying" (buffer-string)))
      (t3-code-thread--receive (list :kind "snapshot" :payload failure))
      (should (equal (plist-get t3-code-thread--payload :items)
                     (plist-get good :items)))
      (let ((tick (buffer-chars-modified-tick)))
        (t3-code-thread--receive (list :kind "snapshot" :payload failure))
        (should (= tick (buffer-chars-modified-tick))))
      (t3-code-thread--receive '(:kind "synchronized"))
      (should-not (string-match-p "retrying" (buffer-string)))
      (should (string-match-p "Still here" (buffer-string))))))

(defun t3-code-test--section-payload ()
  "Return a fresh grouped thread fixture."
  (copy-tree
   '(:thread (:id "thread-1" :title "Folds")
     :items ((:id "user" :runId "run-1" :runStatus "completed" :runOrdinal 1
              :type "user_message" :label "You" :text "Fix the folds")
             (:id "tool" :runId "run-1" :type "command_execution" :label "Command"
              :text "make test" :detail "test output" :status "completed")
             (:id "answer" :runId "run-1" :type "assistant_message" :label "Assistant"
              :text "Folding now works." :status "completed")
             (:id "approval" :runId "run-1" :type "approval_request" :label "Approval"
              :text "Allow integration tests?" :actionId "request-1" :status "waiting")))))

(ert-deftest t3-code-test-thread-nested-folds-survive-streaming-and-outline ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload (t3-code-test--section-payload))
    (t3-code-thread--refresh)
    (should (string-match-p "Folding now works" (t3-code-test--visible-text)))
    (should-not (string-match-p "test output" (t3-code-test--visible-text)))
    (goto-char (car (gethash "work:run-1" t3-code-thread--positions)))
    (t3-code-thread-toggle-details)
    (should (string-match-p "Command" (t3-code-test--visible-text)))
    (t3-code-thread--goto-item "tool")
    (t3-code-thread-toggle-details)
    (should (string-match-p "test output" (t3-code-test--visible-text)))
    (t3-code-thread-toggle-details)
    (setf (plist-get (nth 1 (plist-get t3-code-thread--payload :items)) :detail) "late output")
    (t3-code-thread--refresh)
    (should-not (string-match-p "late output" (t3-code-test--visible-text)))
    (cl-letf (((symbol-function 'message) #'ignore)) (t3-code-thread-cycle-view))
    (should-not (string-match-p "Folding now works" (t3-code-test--visible-text)))
    (should (string-match-p "! Approval" (t3-code-test--visible-text)))
    (goto-char (car (gethash "attention:approval" t3-code-thread--positions)))
    (t3-code-thread-inspect)
    (should (equal (t3-code-thread--item-at-point) "approval"))
    (should-not (invisible-p (point)))
    (should (string-match-p "Folding now works" (t3-code-test--visible-text)))))

(ert-deftest t3-code-test-thread-patches-one-body-and-preserves-reading-anchors ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (t3-code-thread-mode)
      (setq t3-code-thread--payload (t3-code-test--section-payload))
      (t3-code-thread--refresh)
      (let* ((answer (car (gethash "item:answer:body" t3-code-thread--positions)))
             (marker (copy-marker (+ answer 8)))
             (other (split-window-right)))
        (set-window-buffer other (current-buffer))
        (goto-char (+ answer 8))
        (set-window-start other answer t)
        (set-window-point other (+ answer 3))
        (setf (plist-get (nth 1 (plist-get t3-code-thread--payload :items)) :detail)
              (make-string 300 ?x))
        (t3-code-thread--refresh)
        (let ((updated (car (gethash "item:answer:body" t3-code-thread--positions))))
          (should (= (marker-position marker) (+ updated 8)))
          (should (= (point) (+ updated 8)))
          (should (= (window-point other) (+ updated 3)))
          (should (= (window-start other) updated)))
        (set-marker marker nil)))))

(ert-deftest t3-code-test-thread-follow-is-per-window-and-disarms-on-reading ()
  (save-window-excursion
    (with-temp-buffer
      (switch-to-buffer (current-buffer))
      (t3-code-thread-mode)
      (setq t3-code-thread--payload (t3-code-test--section-payload))
      (t3-code-thread--refresh)
      (goto-char (point-min))
      (let ((following (split-window-right)))
        (set-window-buffer following (current-buffer))
        (set-window-point following (point-max))
        (set-window-parameter following 't3-code-follow t)
        (setf (plist-get (nth 3 (plist-get t3-code-thread--payload :items)) :text)
              "Longer approval prompt received while reading")
        (t3-code-thread--refresh)
        (should (= (window-point following) (point-max)))
        (should (= (point) (point-min)))
        (set-window-point following (point-min))
        (t3-code-thread--refresh)
        (should-not (window-parameter following 't3-code-follow))
        (should (= (window-point following) (point-min)))))))

(ert-deftest t3-code-test-thread-isearch-can-reveal-nested-hidden-output ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload (t3-code-test--section-payload))
    (t3-code-thread--refresh)
    (let* ((position (car (gethash "item:tool:body" t3-code-thread--positions)))
           (hidden (seq-filter (lambda (overlay) (overlay-get overlay 'invisible))
                               (overlays-at position))))
      (should (invisible-p position))
      (dolist (overlay hidden)
        (funcall (overlay-get overlay 'isearch-open-invisible-temporary) overlay nil))
      (should-not (invisible-p position))
      (dolist (overlay hidden)
        (funcall (overlay-get overlay 'isearch-open-invisible-temporary) overlay t))
      (should (invisible-p position)))))

(ert-deftest t3-code-test-thread-copy-visible-and-deferred-search-updates ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload (t3-code-test--section-payload))
    (t3-code-thread--refresh)
    (let ((kill-ring nil))
      (t3-code-thread-copy (point-min) (point-max))
      (should-not (string-match-p "test output" (car kill-ring)))
      (t3-code-thread-copy (point-min) (point-max) t)
      (should (string-match-p "test output" (car kill-ring))))
    (let ((isearch-mode t)
          (before (buffer-string)))
      (setf (plist-get (nth 1 (plist-get t3-code-thread--payload :items)) :detail) "search-time update")
      (t3-code-thread--refresh)
      (should (equal (buffer-string) before)))
    (run-hooks 'isearch-mode-end-hook)
    (should (string-match-p "search-time update" (buffer-string)))
    (should-not (string-match-p "search-time update" (t3-code-test--visible-text)))))

(ert-deftest t3-code-test-thread-does-not-guess-unkeyed-turns ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload
          '(:items ((:id "one" :type "user_message" :label "You" :text "Hi")
                    (:id "two" :type "assistant_message" :label "Assistant" :text "Hello"))))
    (t3-code-thread--refresh)
    (should-not (string-match-p "Turn" (buffer-string)))
    (should (string-match-p "Hello" (t3-code-test--visible-text)))))

(ert-deftest t3-code-test-composer-reuses-draft-and-preserves-inflight-edits ()
  (save-window-excursion
    (let ((origin (generate-new-buffer " *t3-test-origin*")) composer callback)
      (unwind-protect
          (progn
            (switch-to-buffer origin)
            (t3-code-thread-mode)
            (setq t3-code-thread--environment
                  (t3-code-environment-create :id "draft-test" :state 'ready :capabilities '(:mutations t))
                  t3-code-thread--thread-id "thread-1")
            (setq composer (t3-code-thread-compose))
            (insert "Original draft")
            (with-current-buffer origin
              (should (eq composer (t3-code-thread-compose))))
            (with-current-buffer composer
              (should (equal (buffer-string) "Original draft"))
              (cl-letf (((symbol-function 't3-code-request)
                         (lambda (_environment _operation _input cb) (setq callback cb)))
                        ((symbol-function 'message) #'ignore))
                (t3-code-compose-send)
                (should-error (t3-code-compose-send) :type 'user-error)
                (insert " plus new edits")
                (funcall callback '(:accepted t) nil)
                (should (equal (buffer-string) "Original draft plus new edits"))
                (should (equal (car t3-code-compose--history) "Original draft"))
                (should-not t3-code-compose--sending)
                (t3-code-compose-send)
                (funcall callback nil '(:message "disconnected"))
                (should (equal (buffer-string) "Original draft plus new edits"))
                (should-not t3-code-compose--sending)
                (t3-code-compose-send)
                (funcall callback '(:accepted t) nil)
                (should (buffer-live-p composer))
                (should (string-empty-p (buffer-string))))))
        (when (buffer-live-p composer) (kill-buffer composer))
        (kill-buffer origin)))))

(provide 't3-code-thread-test)
;;; t3-code-thread-test.el ends here
