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
                     :detail "line 1\nline 2\nline 3\nline 4\nline 5\n23 tests passed"
                     :streaming :false))
            :truncated :false))
    (t3-code-thread--refresh)
    (goto-char (+ (point-min) 10))
    (let ((position (point)))
      (t3-code-thread--refresh)
      (should (= (point) position)))
    (should (string-match-p "^You$" (t3-code-test--visible-text)))
    (should (string-match-p "Please fix it" (t3-code-test--visible-text)))
    ;; Reasoning folds to its heading by default, as in T3 Code.
    (should (string-match-p "▸ Thinking" (t3-code-test--visible-text)))
    (should-not (string-match-p "Inspect first" (t3-code-test--visible-text)))
    (should (t3-code-thread--goto-item "thinking"))
    (t3-code-thread-toggle-details)
    (should (string-match-p "Inspect first" (t3-code-test--visible-text)))
    ;; Tool output collapses to a preview with a count of the hidden rest.
    (should (string-match-p "\\$ make check" (t3-code-test--visible-text)))
    (should (string-match-p "line 4" (t3-code-test--visible-text)))
    (should (string-match-p "2 more lines" (t3-code-test--visible-text)))
    (should-not (string-match-p "23 tests passed" (t3-code-test--visible-text)))
    (should (t3-code-thread--goto-item "command"))
    (t3-code-thread-toggle-details)
    (should (string-match-p "23 tests passed" (t3-code-test--visible-text)))
    (should-not (string-match-p "more lines" (t3-code-test--visible-text)))
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
    (should (eq (lookup-key t3-code-thread-mode-map (kbd "?"))
                #'t3-code-thread-actions))
    (should (eq (lookup-key t3-code-compose-mode-map (kbd "C-c C-k"))
                #'t3-code-compose-abort))
    (should (eq (lookup-key t3-code-compose-mode-map (kbd "C-c C-c"))
                #'t3-code-compose-send))
    (let ((suffix (transient-get-suffix 't3-code-thread-actions "q")))
      (should (eq (plist-get (cdr suffix) :command)
                  #'t3-code-thread-compose-queue)))))

(ert-deftest t3-code-test-composer-menu-targets-new-draft-or-existing-chat ()
  (save-window-excursion
    (with-temp-buffer
      (t3-code-thread-mode)
      (let ((chat (current-buffer)) menu-buffer menu)
        (with-temp-buffer
          (t3-code-compose-mode)
          (switch-to-buffer (current-buffer))
          (insert "Unsent draft")
          (setq t3-code-compose--launch '(:projectId "project-1"))
          (cl-letf (((symbol-function 'transient-setup)
                     (lambda (prefix &rest _)
                       (setq menu prefix menu-buffer (current-buffer)))))
            (call-interactively
             (lookup-key t3-code-compose-mode-map (kbd "C-c C-p")))
            (should (eq menu 't3-code-compose-launch-actions))
            (should (eq menu-buffer (current-buffer)))
            (should (eq (window-buffer (selected-window)) (current-buffer)))
            (should (equal (buffer-string) "Unsent draft"))
            (setq t3-code-compose--launch nil
                  t3-code-compose--origin-buffer chat)
            (t3-code-compose-actions)
            (should (eq menu 't3-code-thread-actions))
            (should (eq menu-buffer chat))
            (should (eq (window-buffer (selected-window)) chat))))))))

(ert-deftest t3-code-test-thread-select-model-preserves-draft-and-sends-options ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--environment
          (t3-code-environment-create
           :id "test" :state 'ready :capabilities '(:mutations t :modelSelection t))
          t3-code-thread--thread-id "thread-1"
          t3-code-thread--payload
          '(:thread (:id "thread-1" :title "Example"
                     :modelSelection (:instanceId "codex" :model "old"))))
    (let ((origin (current-buffer))
          (catalog '(:providers ((:instanceId "work" :name "Work" :available t
                                  :models ((:slug "new" :name "New"
                                            :options ((:id "effort" :label "Effort"
                                                       :type "select"
                                                       :choices ((:id "high" :isDefault t)
                                                                 (:id "low"))))))))))
          (answers '("Work · New [work]" "low")) request)
      (with-temp-buffer
        (t3-code-compose-mode)
        (setq t3-code-compose--origin-buffer origin)
        (insert "unsent draft")
        (cl-letf (((symbol-function 't3-code-request)
                   (lambda (_environment operation _input callback)
                     (should (equal operation "model.catalog"))
                     (funcall callback catalog nil)))
                  ((symbol-function 'run-at-time)
                   (lambda (_delay _repeat callback &rest args) (apply callback args)))
                  ((symbol-function 'completing-read)
                   (lambda (&rest _args) (pop answers)))
                  ((symbol-function 't3-code-thread--request)
                   (lambda (operation input &rest _args)
                     (setq request (list operation input)))))
          (t3-code-compose-select-model))
        (should (equal (buffer-string) "unsent draft")))
      (should (equal (car request) "thread.modelSelection.set"))
      (should (equal (plist-get (plist-get (cadr request) :modelSelection) :instanceId)
                     "work"))
      (should (equal (plist-get (plist-get (cadr request) :modelSelection) :options)
                     [(:id "effort" :value "low")])))))

(ert-deftest t3-code-test-thread-model-picker-explains-unsupported-change ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload
          '(:thread (:modelSelection (:instanceId "work" :model "old")
                     :hasStartedSession t)))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "Work · New [work]")))
      (should-error
       (t3-code-thread--select-model
        '(:providers ((:instanceId "work" :name "Work" :available t
                       :requiresNewThreadForModelChange t
                       :models ((:slug "new" :name "New"))))))
       :type 'user-error))))

(ert-deftest t3-code-test-thread-header-distinguishes-running-model ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload
          '(:thread (:title "Example" :status "running" :provider "work"
                     :model "next" :activeRunProvider "work" :activeRunModel "old")))
    (should (string-match-p "work/next.*running: work/old"
                            (t3-code-thread--header-line)))))

(ert-deftest t3-code-test-thread-model-selection-rejects-unsupported-bridge ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--environment
          (t3-code-environment-create :id "old" :state 'ready :capabilities '(:mutations t)))
    (should-error (t3-code-thread-select-model) :type 'user-error)))

(ert-deftest t3-code-test-thread-shows-truncation-and-errors ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload '(:thread nil :items nil :truncated t
                                     :error "safe failure"))
    (t3-code-thread--refresh)
    (should (string-match-p "Could not load thread" (buffer-string)))
    (should (string-match-p "safe failure" (buffer-string)))))

(ert-deftest t3-code-test-thread-coalesces-renders-of-streamed-updates ()
  (save-window-excursion
    (let ((buffer (generate-new-buffer " *t3-coalesce*"))
          (t3-code-render-interval 60)
          (payload (lambda (text) (list :thread '(:id "t")
                                        :items (list (list :id "a" :type "assistant_message"
                                                           :text text))))))
      (unwind-protect
          (with-current-buffer buffer
            (t3-code-thread-mode)
            ;; Hidden: nothing renders until the chat is displayed.
            (t3-code-thread--receive (list :kind "event" :payload (funcall payload "one")))
            (should-not (string-match-p "one" (buffer-string)))
            (switch-to-buffer buffer)
            (run-hook-with-args 'window-buffer-change-functions (selected-window))
            (should (string-match-p "one" (buffer-string)))
            ;; Visible: the first update renders, later ones wait for the timer.
            (t3-code-thread--receive (list :kind "event" :payload (funcall payload "two")))
            (should (string-match-p "two" (buffer-string)))
            (t3-code-thread--receive (list :kind "event" :payload (funcall payload "three")))
            (should-not (string-match-p "three" (buffer-string)))
            (should (timerp t3-code-thread--render-timer))
            (timer-event-handler t3-code-thread--render-timer)
            (should (string-match-p "three" (buffer-string))))
        (with-current-buffer buffer
          (when (timerp t3-code-thread--render-timer)
            (cancel-timer t3-code-thread--render-timer)))
        (kill-buffer buffer)))))

(ert-deftest t3-code-test-thread-retains-last-good-snapshot-on-stream-error ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (let ((t3-code-render-interval nil)
          (good '(:thread (:id "thread-1" :title "Example")
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
              :text "make test" :detail "head 1\nhead 2\nhead 3\nhead 4\nhead 5\ntest output"
              :status "completed")
             (:id "answer" :runId "run-1" :type "assistant_message" :label "Assistant"
              :text "Folding now works." :status "completed")
             (:id "approval" :runId "run-1" :type "approval_request" :label "Approval"
              :text "Allow integration tests?" :actionId "request-1" :status "waiting")))))

(ert-deftest t3-code-test-thread-nested-folds-survive-streaming-and-outline ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload (t3-code-test--section-payload))
    (t3-code-thread--refresh)
    (should (string-match-p "You · turn 1" (t3-code-test--visible-text)))
    (should (string-match-p "Fix the folds" (t3-code-test--visible-text)))
    (should (string-match-p "^Assistant$" (t3-code-test--visible-text)))
    (should (string-match-p "Folding now works" (t3-code-test--visible-text)))
    ;; A settled turn folds its work, keeping the answer and the request.
    (should (string-match-p "▸ Worked · 1 tool call" (t3-code-test--visible-text)))
    (should-not (string-match-p "make test" (t3-code-test--visible-text)))
    (goto-char (car (gethash "work:run-1" t3-code-thread--positions)))
    (t3-code-thread-toggle-details)
    (should (string-match-p "make test" (t3-code-test--visible-text)))
    (should-not (string-match-p "test output" (t3-code-test--visible-text)))
    (t3-code-thread--goto-item "tool")
    (t3-code-thread-toggle-details)
    (should (string-match-p "test output" (t3-code-test--visible-text)))
    (t3-code-thread-toggle-details)
    (setf (plist-get (nth 1 (plist-get t3-code-thread--payload :items)) :detail)
          "head 1\nhead 2\nhead 3\nhead 4\nhead 5\nlate output")
    (t3-code-thread--refresh)
    (should-not (string-match-p "late output" (t3-code-test--visible-text)))
    ;; Folding the assistant section from inside an unfoldable answer.
    (goto-char (car (gethash "item:answer:body" t3-code-thread--positions)))
    (t3-code-thread-toggle-details)
    (should-not (string-match-p "Folding now works" (t3-code-test--visible-text)))
    (should (string-match-p "You · turn 1" (t3-code-test--visible-text)))
    (t3-code-thread-toggle-details)
    (should (string-match-p "Folding now works" (t3-code-test--visible-text)))
    (cl-letf (((symbol-function 'message) #'ignore)) (t3-code-thread-cycle-view))
    (should-not (string-match-p "Folding now works" (t3-code-test--visible-text)))
    (should (string-match-p "You · turn 1 …" (t3-code-test--visible-text)))
    (should (string-match-p "! Approval" (t3-code-test--visible-text)))
    (goto-char (car (gethash "attention:approval" t3-code-thread--positions)))
    (t3-code-thread-inspect)
    (should (equal (t3-code-thread--item-at-point) "approval"))
    (should-not (invisible-p (point)))
    (should (string-match-p "Folding now works" (t3-code-test--visible-text)))))

(ert-deftest t3-code-test-thread-groups-work-of-a-turn-in-progress ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload
          '(:thread (:id "t" :status "running" :activeRunId "r")
            :items ((:id "u" :runId "r" :runStatus "running" :type "user_message" :text "Go")
                    (:id "c1" :runId "r" :type "command_execution" :text "make one")
                    (:id "t1" :runId "r" :type "reasoning" :text "Hmm")
                    (:id "m" :runId "r" :type "assistant_message" :text "Found it.")
                    (:id "e" :runId "r" :type "file_change" :path "a.el" :status "failed")
                    (:id "c2" :runId "r" :type "command_execution" :text "make two")
                    (:id "t2" :runId "r" :type "reasoning" :text "**Checking** the result"
                     :streaming t))))
    (t3-code-thread--refresh)
    (let ((text (t3-code-test--visible-text)))
      (should (string-match-p "▸ Worked · 1 tool call · 1 thought$" text))
      (should (string-match-p "Found it\\." text))
      ;; A failed tool call folds too, marking its group.
      (should-not (string-match-p "a\\.el" text))
      (should (string-match-p
               "▸ Working · 2 tool calls · 1 thought · 1 failed · Thinking: Checking the result"
               text))
      (should-not (string-match-p "make" text)))))

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
    (let* ((position (car (gethash "item:tool:rest" t3-code-thread--positions)))
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
      (setf (plist-get (nth 1 (plist-get t3-code-thread--payload :items)) :detail)
            "head 1\nhead 2\nhead 3\nhead 4\nhead 5\nsearch-time update")
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
    (should-not (string-match-p "turn" (buffer-string)))
    (should (string-match-p "^You$" (t3-code-test--visible-text)))
    (should (string-match-p "^Assistant$" (t3-code-test--visible-text)))
    (should (string-match-p "Hello" (t3-code-test--visible-text)))
    (should (equal (mapcar #'car (t3-code-thread--imenu)) '("You · Hi")))))

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

(defmacro t3-code-test--with-chat (payload &rest body)
  "Run BODY in a ready chat buffer showing PAYLOAD, capturing requests.
Requests are collected in `requests' as (OPERATION INPUT), newest first."
  (declare (indent 1))
  `(with-temp-buffer
     (t3-code-thread-mode)
     (setq t3-code-thread--environment
           (t3-code-environment-create
            :id "test" :state 'ready
            :capabilities '(:mutations t :modelSelection t :threadLifecycle t))
           t3-code-thread--thread-id "thread-1"
           t3-code-thread--payload ,payload)
     (t3-code-thread--refresh)
     (let (requests)
       (cl-letf (((symbol-function 't3-code-request)
                  (lambda (_environment operation input callback)
                    (push (list operation input) requests)
                    (funcall callback '(:sequence 1) nil)))
                 ((symbol-function 'message) #'ignore))
         ,@body))))

(ert-deftest t3-code-test-thread-answers-input-with-option-values ()
  (t3-code-test--with-chat
      (copy-tree
       '(:thread (:id "thread-1")
         :items ((:id "ask" :type "user_input_request" :status "waiting" :actionId "input-1"
                  :label "Input needed" :text "Scope: Which?"
                  :questions ((:id "q1" :header "Scope" :question "Which?"
                               :allowCustomAnswer :false
                               :options ((:label "UDP" :description "fast" :value "udp")
                                         (:label "DoH" :description "fallback" :value "doh ")))
                              (:id "q2" :header "Extras" :question "Also?" :multiSelect t
                               :options ((:label "Tests" :description "" :value "tests")
                                         (:label "Docs" :description "" :value "docs"))))))))
    (should (string-match-p "RET to answer" (buffer-string)))
    ;; Multi-select collects one choice per prompt; commas stay intact.
    (let ((replies (list "DoH" "Tests" "custom, with comma" "")))
     (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) (pop replies))))
      (t3-code-thread--goto-item "ask")
      (t3-code-thread-ret)))
    (pcase-let ((`(,operation ,input) (car requests)))
      (should (equal operation "thread.command"))
      (let* ((command (plist-get input :command))
             (answers (plist-get command :answers)))
        (should (equal (plist-get command :type) "runtime-request.respond"))
        (should (equal (plist-get command :requestId) "input-1"))
        (should (equal (gethash "q1" answers) "doh "))
        (should (equal (gethash "q2" answers) ["tests" "custom, with comma"]))
        (should (string-match-p "\"q1\":\"doh \"" (json-serialize input)))))))

(ert-deftest t3-code-test-thread-cycles-reasoning-effort ()
  (let ((catalog '(:providers ((:instanceId "codex"
                                :models ((:slug "gpt" :options
                                          ((:id "reasoningEffort" :type "select"
                                            :choices ((:id "low") (:id "medium" :isDefault t)
                                                      (:id "high"))))))))))
        (selection '(:instanceId "codex" :model "gpt" :options [(:id "other" :value "x")]))
        (cycle (lambda (catalog selection)
                 (t3-code-thread--with-effort catalog selection #'t3-code-thread--next-effort))))
    (let ((next (funcall cycle catalog selection)))
      (should (equal (t3-code-thread--effort next) "high"))
      (should (member '(:id "other" :value "x") (append (plist-get next :options) nil)))
      (should (equal (t3-code-thread--effort (funcall cycle catalog next)) "low")))
    (should-error (funcall cycle '(:providers nil) selection) :type 'user-error)))

(ert-deftest t3-code-test-compose-queues-while-busy-and-starts-when-idle ()
  (let ((chat (generate-new-buffer " *t3-chat*")))
    (unwind-protect
        (with-temp-buffer
          (t3-code-compose-mode)
          (setq t3-code-compose--origin-buffer chat)
          (with-current-buffer chat
            (t3-code-thread-mode)
            (setq t3-code-thread--payload '(:thread (:status "idle"))))
          (should (equal (t3-code-compose--default-mode) "auto"))
          (with-current-buffer chat
            (setq t3-code-thread--payload '(:thread (:status "running" :activeRunId "run"))))
          (should (equal (t3-code-compose--default-mode) "queue"))
          (setq t3-code-compose--dispatch-mode "steer")
          (should (equal (t3-code-compose--default-mode) "steer")))
      (kill-buffer chat))))

(ert-deftest t3-code-test-thread-loads-older-history-before-first-item ()
  (t3-code-test--with-chat
      (copy-tree '(:thread (:id "thread-1") :hasOlderHistory t
                   :items ((:id "new" :type "assistant_message" :text "Newest"))))
    (should (string-match-p "Older history available" (buffer-string)))
    (cl-letf (((symbol-function 't3-code-request)
               (lambda (_environment operation input callback)
                 (push (list operation input) requests)
                 (funcall callback
                          '(:items ((:id "old" :type "assistant_message" :text "Oldest"))
                            :hasMore :false)
                          nil))))
      (goto-char (car (gethash "history" t3-code-thread--positions)))
      (t3-code-thread-ret))
    (should (equal (car requests)
                   '("thread.history" (:threadId "thread-1" :beforeItemId "new"))))
    (should-not (string-match-p "Older history" (buffer-string)))
    (should (< (string-search "Oldest" (buffer-string))
               (string-search "Newest" (buffer-string))))))

(ert-deftest t3-code-test-thread-forks-at-turn-at-point ()
  (t3-code-test--with-chat (t3-code-test--section-payload)
    (t3-code-thread--goto-item "answer")
    (cl-letf (((symbol-function 't3-code-thread-open) #'ignore))
      (t3-code-thread-fork))
    (pcase-let ((`(,operation ,input) (car requests)))
      (should (equal operation "thread.fork"))
      (should (equal (plist-get input :runId) "run-1"))
      (should (plist-get input :targetThreadId)))
    (cl-letf (((symbol-function 't3-code-thread-open) #'ignore))
      (t3-code-thread-fork t))
    (should (plist-member (cadr (car requests)) :runId))
    (should-not (plist-get (cadr (car requests)) :runId))))

(ert-deftest t3-code-test-thread-web-links-are-clickable ()
  (dolist (mode '(nil gfm-mode))
    (when (or (null mode) (require 'markdown-mode nil t))
      (let ((t3-code-markdown-mode mode))
        (with-temp-buffer
          (t3-code-thread-mode)
          (setq t3-code-thread--payload
                (copy-tree
                 '(:thread (:id "t")
                   :items ((:id "a" :type "assistant_message"
                            :text "[docs](<https://example.com/docs> \"Docs\")\n[https://label.example](https://target.example)\nSee (https://example.com/foo(bar)).")))))
          (t3-code-thread--refresh)
          (let (opened)
            (cl-letf (((symbol-function 'browse-url)
                       (lambda (url &rest _) (push url opened))))
              (dolist (entry '(("docs" . "https://example.com/docs")
                               ("https://label.example" . "https://target.example")
                               ("https://example.com/foo" . "https://example.com/foo(bar)")))
                (goto-char (point-min))
                (search-forward (car entry))
                (backward-char)
                (should (equal (get-text-property (point) 't3-code-url) (cdr entry)))
                (should (eq (get-text-property (point) 'mouse-face) 'highlight))
                (should (equal (get-text-property (point) 'help-echo) (cdr entry)))
                (should (get-text-property (point) 'follow-link))
                (should (eq (lookup-key (get-text-property (point) 'keymap) [mouse-1])
                            #'t3-code-markdown-browse-url))
                (t3-code-thread-ret)
                (should (equal (car opened) (cdr entry))))
              ;; Exercise the actual mouse command at its event's point.
              (save-window-excursion
                (switch-to-buffer (current-buffer))
                (goto-char (point-min))
                (search-forward "docs")
                (let ((event (list 'mouse-1
                                   (list (selected-window) (1- (point)) '(0 . 0) 0))))
                  (t3-code-markdown-browse-url event))
                (should (equal (car opened) "https://example.com/docs"))))
            ;; Streaming replacements must drop old targets and leave new ones usable.
            (setf (plist-get (car (plist-get t3-code-thread--payload :items)) :text)
                  "Now https://new.example/")
            (t3-code-thread--refresh)
            (goto-char (point-min))
            (search-forward "https://new.example/")
            (should (equal (get-text-property (1- (point)) 't3-code-url)
                           "https://new.example/"))
            (should-not (string-search "example.com/docs" (buffer-string)))))))))

(ert-deftest t3-code-test-compose-path-completion-matches-pi ()
  (let ((root (make-temp-file "t3-complete" t)))
    (unwind-protect
        (progn
          (make-directory (expand-file-name "sub" root))
          (with-temp-file (expand-file-name "file.txt" root) (insert "test"))
          (with-temp-buffer
            (t3-code-compose-mode)
            (setq default-directory (file-name-as-directory root))
            (should (equal completion-at-point-functions
                           '(t3-code-compose-completion-at-point)))
            (dolist (path (list "./" "../" "~/" (concat root "/")))
              (cl-letf (((symbol-function 'file-name-all-completions)
                         (lambda (base _directory)
                           (should (equal base ""))
                           '("./" "../" "sub/" "file.txt"))))
                (erase-buffer)
                (insert "Read " path)
                (let* ((result (t3-code-compose-completion-at-point))
                       (annotation (plist-get (nthcdr 3 result) :annotation-function)))
                  (should (equal (nth 2 result)
                                 (list (concat path "sub/") (concat path "file.txt"))))
                  (should (equal (funcall annotation (concat path "sub/")) " (dir)"))
                  (should (equal (funcall annotation (concat path "file.txt")) " (file)")))))
            ;; Actual TAB completion inside quotes preserves the surrounding draft.
            (erase-buffer)
            (insert "Read \"./fi\" please")
            (goto-char (point-min))
            (search-forward "./fi")
            (t3-code-compose-complete)
            (should (equal (buffer-string) "Read \"./file.txt\" please"))
            ;; A directory completion retains ./ and can be continued.
            (erase-buffer)
            (insert "./su")
            (t3-code-compose-complete)
            (should (equal (buffer-string) "./sub/"))
            (dolist (text '("prose" "src/file" "/tmp/" "Read ./missing/"
                            "Read /ssh:other:/tmp/"))
              (erase-buffer)
              (insert text)
              (should-not (t3-code-compose-completion-at-point)))))
      (delete-directory root t))))

(ert-deftest t3-code-test-compose-path-completion-follows-remote-worktree ()
  (with-temp-buffer
    (t3-code-compose-mode)
    (setq default-directory "/ssh:bastion|sudo:root@t3-host:/src/project/")
    (dolist (entry '(("./lo" . "/ssh:bastion|sudo:root@t3-host:/src/project/")
                     ("../lo" . "/ssh:bastion|sudo:root@t3-host:/src/")
                     ("/var/lo" . "/ssh:bastion|sudo:root@t3-host:/var/")
                     ("~/lo" . "/ssh:bastion|sudo:root@t3-host:~/")))
      (erase-buffer)
      (insert "Read " (car entry))
      (cl-letf (((symbol-function 'file-directory-p)
                 (lambda (directory) (should (equal directory (cdr entry))) t))
                ((symbol-function 'file-name-all-completions)
                 (lambda (base directory)
                   (should (equal base "lo"))
                   (should (equal directory (cdr entry)))
                   '("log/" "local"))))
        (let ((result (t3-code-compose-completion-at-point))
              (dir (file-name-directory (car entry))))
          (should (equal (nth 2 result) (list (concat dir "log/") (concat dir "local")))))))))

(ert-deftest t3-code-test-thread-parses-and-visits-file-locations ()
  (should (equal (t3-code-thread--parse-location "`src/app.el:12:3`,")
                 '("src/app.el" 12 3)))
  (should (equal (t3-code-thread--parse-location "src/app.el#L12-L20")
                 '("src/app.el" 12 nil)))
  (should (equal (t3-code-thread--parse-location "README.org") '("README.org" nil nil)))
  (let* ((root (make-temp-file "t3-root" t))
         (file (expand-file-name "notes.txt" root)))
    (unwind-protect
        (progn
          (with-temp-file file (insert "one\ntwo\nthree\n"))
          (should (equal (t3-code-thread--resolve-file "notes.txt:2" root) (list file 2 nil)))
          (should-not (t3-code-thread--resolve-file "missing.txt" root))
          (should-not (t3-code-thread--resolve-file "https://example.com" root)))
      (delete-directory root t)))
  (should (equal (t3-code-thread--substitute-file "wc -l * | sort" "/tmp/a b")
                 (concat "wc -l " (shell-quote-argument "/tmp/a b") " | sort")))
  (should (equal (t3-code-thread--substitute-file "head -n2" "/tmp/x") "head -n2 /tmp/x")))

(ert-deftest t3-code-test-thread-reports-activity-phase-changes ()
  (let (calls)
    (let ((t3-code-activity-phase-functions
           (list (lambda (_chat _input old new) (push (list old new) calls)))))
      (with-temp-buffer
        (t3-code-thread-mode)
        (setq t3-code-thread--payload
              '(:thread (:status "running" :activeRunId "r")
                :items ((:id "r" :type "reasoning" :streaming t :text "hm"))))
        (t3-code-thread--refresh)
        (setq t3-code-thread--payload '(:thread (:status "idle") :items nil))
        (t3-code-thread--refresh)))
    (should (equal (nreverse calls) '(("idle" "thinking") ("thinking" "idle"))))))

(ert-deftest t3-code-test-launch-input-creates-thread-with-first-message ()
  (save-window-excursion
    (let* ((environment (t3-code-environment-create
                         :id "launch" :state 'ready
                         :capabilities '(:mutations t :threadLifecycle t)))
           (launch (list :projectId "project-1" :projectName "owner/repo"
                         ;; As parsed from a payload: JSON arrays arrive as lists.
                         :modelSelection '(:instanceId "codex" :model "gpt"
                                           :options ((:id "reasoningEffort" :value "high")))
                         :runtimeMode "full-access" :interactionMode "default"
                         :workspaceStrategy '(:type "root")))
           (input (t3-code-compose-open-launch environment launch))
           request opened)
      (unwind-protect
          (with-current-buffer input
            (insert "Build the resolver")
            (should (string-match-p "New thread.*owner/repo.*project root"
                                    (t3-code-compose--header-line)))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt &rest _)
                         (if (string-prefix-p "Runtime" prompt)
                             "approval-required"
                           "plan"))))
              (call-interactively
               (plist-get (cdr (transient-get-suffix 't3-code-compose-launch-actions "p"))
                          :command))
              (call-interactively
               (plist-get (cdr (transient-get-suffix 't3-code-compose-launch-actions "P"))
                          :command)))
            (should (equal (buffer-string) "Build the resolver"))
            (should (string-match-p "approval-required.*plan"
                                    (t3-code-compose--header-line)))
            (cl-letf (((symbol-function 't3-code-request)
                       (lambda (_environment operation input callback)
                         (setq request (list operation input))
                         (should (string-match-p "\"options\":\\[{\"id\""
                                                 (json-serialize input)))
                         (funcall callback '(:threadId "thread-new") nil)))
                      ((symbol-function 't3-code-thread-open)
                       (lambda (_environment thread)
                         (setq opened thread)
                         (generate-new-buffer " *opened*"))))
              (t3-code-compose-send))
            (should (equal (car request) "thread.create"))
            (should (equal (plist-get (cadr request) :text) "Build the resolver"))
            (should (equal (plist-get (cadr request) :projectId) "project-1"))
            (should (equal (plist-get (cadr request) :runtimeMode) "approval-required"))
            (should (equal (plist-get (cadr request) :interactionMode) "plan"))
            (should (equal (plist-get (cadr request) :workspaceStrategy) '(:type "root")))
            (should-not (plist-member (cadr request) :projectName))
            (should (equal (plist-get opened :id) "thread-new"))
            (should-not (buffer-live-p input)))
        (when (buffer-live-p input) (kill-buffer input))))))

(ert-deftest t3-code-test-thread-keeps-items-displaced-after-loading-history ()
  (with-temp-buffer
    (t3-code-thread-mode)
    (setq t3-code-thread--payload
          '(:thread (:id "t") :items ((:id "edge" :type "assistant_message" :text "Edge")
                                      (:id "new" :type "assistant_message" :text "New")))
          t3-code-thread--older-items
          '((:id "old" :type "assistant_message" :text "Old")))
    (t3-code-thread--receive
     '(:kind "event" :payload (:thread (:id "t")
                               :items ((:id "new" :type "assistant_message" :text "New")
                                       (:id "newer" :type "assistant_message" :text "Newer")))))
    (should (equal (mapcar (lambda (item) (plist-get item :id)) (t3-code-thread--items))
                   '("old" "edge" "new" "newer")))))

(ert-deftest t3-code-test-thread-refuses-to-edit-truncated-queued-message ()
  (t3-code-test--with-chat
      '(:thread (:id "thread-1")
        :queued ((:runId "run-2" :position 1 :held :false :text "preview" :truncated t)))
    (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "1. preview"))
              ((symbol-function 'read-multiple-choice) (lambda (&rest _) '(?e "edit"))))
      (should-error (t3-code-thread-manage-queue) :type 'user-error))
    (should-not requests)))

(ert-deftest t3-code-test-thread-model-options-send-real-booleans ()
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) "no")))
    (let ((options (t3-code-thread--model-options
                    '((:id "fast" :label "Fast" :type "boolean")) nil)))
      (should (equal options '((:id "fast" :value :false))))
      (should (string-match-p "\"value\":false"
                              (json-serialize (car options) :false-object :false))))))

(ert-deftest t3-code-test-thread-effort-ignores-boolean-options ()
  (should-not (t3-code-thread--effort '(:options [(:id "thinking" :value t)])))
  (should (equal (t3-code-thread--effort '(:options ((:id "thinking" :value :false)
                                                     (:id "effort" :value "high"))))
                 "high")))

(ert-deftest t3-code-test-thread-ignores-remote-file-references ()
  (let ((root (make-temp-file "t3-root" t)))
    (unwind-protect
        ;; A stand-in remote handler: name syntax is fine, any access fails.
        (let ((file-name-handler-alist
               (list (cons "\\`/[^/|:]+:"
                           (lambda (operation &rest _)
                             (if (eq operation 'file-remote-p) t
                               (ert-fail (format "Touched a remote file: %s" operation))))))))
          (should-not (t3-code-thread--resolve-file "/ssh:host:/etc/passwd" root))
          ;; Nor against a remote server, whose prefix the agent cannot change.
          (should-not (t3-code-thread--resolve-file "/ssh:other:/etc/passwd" "/srv/"
                                                    "/ssh:host:")))
      (delete-directory root t))))

(ert-deftest t3-code-test-thread-follows-remote-worktree ()
  (let ((file-name-handler-alist
         ;; A stand-in remote handler where every remote file exists.
         (list (cons "\\`/ssh:host:"
                     (lambda (operation &rest args)
                       (pcase operation
                         ('file-remote-p (and (string-match "\\`/ssh:host:" (car args))
                                              (pcase (cadr args)
                                                ('host "host") ('localname (substring (car args) 10))
                                                (_ "/ssh:host:"))))
                         ('file-exists-p t)
                         ((or 'expand-file-name 'file-name-as-directory
                              'file-name-directory 'file-name-nondirectory)
                          (let ((file-name-handler-alist nil)) (apply operation args)))
                         (_ (ert-fail (format "Touched a remote file: %s" operation))))))))
        (environment (t3-code-environment-create :id "remote"
                                                 :endpoint "https://host:3773")))
    (with-temp-buffer
      (t3-code-thread-mode)
      (setq t3-code-thread--environment environment
            t3-code-thread--payload '(:thread (:worktreePath "/srv/app")))
      (setf (t3-code-environment-remote-prefix environment) "/ssh:host:")
      (t3-code-thread--follow-directory)
      (should (equal default-directory "/ssh:host:/srv/app/"))
      (should (equal (t3-code-thread--resolve-file "src/a.el:3" "/srv/app" "/ssh:host:")
                     '("/ssh:host:/srv/app/src/a.el" 3 nil))))))


(ert-deftest t3-code-test-thread-names-buffers-after-worktree-and-title ()
  (save-window-excursion
    (let ((environment (t3-code-environment-create :id "names" :state 'ready
                                                   :endpoint "http://localhost:3773"))
          chat)
      (cl-letf (((symbol-function 't3-code-subscribe) (lambda (&rest _) nil))
                ((symbol-function 't3-code-shell-mark-visited) #'ignore))
        (unwind-protect
            (progn
              (setq chat (t3-code-thread-open environment
                                              '(:id "t1" :title "Fix the\nfolds"
                                                :path "/src/emacs-t3-code")))
              (should (equal (buffer-name chat) "*t3 emacs-t3-code: Fix the folds*"))
              (with-current-buffer chat
                (should (equal (buffer-name t3-code-thread--composer)
                               " *t3 emacs-t3-code: Fix the folds · input*"))
                (t3-code-thread--receive
                 '(:kind "snapshot" :payload (:thread (:title "Renamed"
                                                       :worktreePath "/src/wt")))))
              (should (equal (buffer-name chat) "*t3 wt: Renamed*"))
              (should (eq (t3-code-thread-buffer environment "t1") chat)))
          (when (buffer-live-p chat)
            (with-current-buffer chat
              (when (buffer-live-p t3-code-thread--composer)
                (kill-buffer t3-code-thread--composer)))
            (kill-buffer chat)))))))

(ert-deftest t3-code-test-thread-switch-from-input-replaces-chat-and-input ()
  (save-window-excursion
    (delete-other-windows)
    (let ((environment (t3-code-environment-create :id "layout" :state 'ready))
          (t3-code-input-window-display 'always)
          buffers)
      (cl-letf (((symbol-function 't3-code-subscribe) (lambda (&rest _) nil))
                ((symbol-function 't3-code-shell-mark-visited) #'ignore))
        (unwind-protect
            (progn
              (push (t3-code-thread-open environment '(:id "one")) buffers)
              (should (with-current-buffer (window-buffer) (derived-mode-p 't3-code-compose-mode)))
              (push (t3-code-thread-open environment '(:id "two")) buffers)
              (should (= (length (window-list)) 2))
              (should (eq (window-buffer (frame-first-window))
                          (t3-code-thread-buffer environment "two")))
              (should (with-current-buffer (window-buffer) (derived-mode-p 't3-code-compose-mode)))
              (should (> (window-height (frame-first-window)) (window-height))))
          (dolist (buffer buffers)
            (with-current-buffer buffer
              (when (buffer-live-p t3-code-thread--composer)
                (kill-buffer t3-code-thread--composer)))
            (kill-buffer buffer)))))))

(ert-deftest t3-code-test-markdown-code-fences-cannot-enable-arbitrary-modes ()
  (should (t3-code-markdown--safe-mode-p 'emacs-lisp-mode))
  (should-not (t3-code-markdown--safe-mode-p 'global-hl-line-mode))
  (should-not (t3-code-markdown--safe-mode-p 'server-mode))
  (skip-unless (require 'markdown-mode nil t))
  (let ((t3-code-markdown-mode 'gfm-mode)
        (t3-code-markdown--cache (make-hash-table :test #'equal))
        (enabled nil)
        (hook-ran nil))
    (cl-letf (((symbol-function 't3-test-probe-mode) (lambda (&rest _) (setq enabled t)))
              (emacs-lisp-mode-hook (list (lambda () (setq hook-ran t)))))
      (t3-code-markdown-fontify "```t3-test-probe\nx\n```\n\n```emacs-lisp\n(car x)\n```\n"
                                'markdown))
    (should-not enabled)
    (should-not hook-ran)))

(provide 't3-code-thread-test)
;;; t3-code-thread-test.el ends here
