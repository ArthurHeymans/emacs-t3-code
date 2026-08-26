;;; t3-code-thread.el --- Read-only T3 thread viewer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders the normalized thread projection supplied by the version-matched
;; bridge.  Raw T3 contracts and provider payloads never reach this buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 't3-code-core)

(defvar-local t3-code-thread--environment nil)
(defvar-local t3-code-thread--thread-id nil)
(defvar-local t3-code-thread--summary nil)
(defvar-local t3-code-thread--subscription nil)
(defvar-local t3-code-thread--payload nil)
(defvar-local t3-code-thread--expanded-details nil)

(defcustom t3-code-thread-default-runtime-mode "full-access"
  "Runtime mode applied when sending a message."
  :type '(choice (const "approval-required")
                 (const "auto-accept-edits")
                 (const "auto")
                 (const "full-access"))
  :group 't3-code)

(defcustom t3-code-thread-default-interaction-mode "default"
  "Interaction mode applied when sending a message."
  :type '(choice (const "default") (const "plan"))
  :group 't3-code)

(defvar-local t3-code-compose--environment nil)
(defvar-local t3-code-compose--thread-id nil)
(defvar-local t3-code-compose--dispatch-mode nil)
(defvar-local t3-code-compose--origin-buffer nil)

(defface t3-code-thread-user-face
  '((((class color) (background dark)) :foreground "#b6a0ff" :weight bold)
    (((class color) (background light)) :foreground "#6d28d9" :weight bold)
    (t :inherit font-lock-keyword-face))
  "Face for user messages."
  :group 't3-code)

(defface t3-code-thread-assistant-face
  '((((class color) (background dark)) :foreground "#79a8ff" :weight bold)
    (((class color) (background light)) :foreground "#1d4ed8" :weight bold)
    (t :inherit font-lock-function-name-face))
  "Face for assistant messages."
  :group 't3-code)

(defface t3-code-thread-action-face
  '((((class color) (background dark)) :foreground "#00d3d0" :weight bold)
    (((class color) (background light)) :foreground "#0f766e" :weight bold)
    (t :inherit font-lock-type-face))
  "Face for tool and action entries."
  :group 't3-code)

(defface t3-code-thread-plan-face
  '((((class color) (background dark)) :foreground "#f78fe7" :weight bold)
    (((class color) (background light)) :foreground "#a21caf" :weight bold)
    (t :inherit font-lock-constant-face))
  "Face for plans and todos."
  :group 't3-code)

(defun t3-code-thread--status-face (status)
  "Return an appropriate face for normalized STATUS."
  (pcase status
    ("running" 't3-code-thread-assistant-face)
    ("waiting-approval" 'warning)
    ("failed" 'error)
    (_ 'shadow)))

(defun t3-code-thread--header-line ()
  "Render thread identity and live status."
  (let* ((thread (plist-get t3-code-thread--payload :thread))
         (summary t3-code-thread--summary)
         (title (or (plist-get thread :title) (plist-get summary :title)
                    t3-code-thread--thread-id "Thread"))
         (status (or (plist-get thread :status) (plist-get summary :status) "loading"))
         (provider (or (plist-get thread :provider) (plist-get summary :provider) ""))
         (model (or (plist-get thread :model) (plist-get summary :model) "")))
    (concat " " (propertize title 'face 'bold)
            "  [" (propertize status 'face (t3-code-thread--status-face status)) "]"
            (if (string-empty-p provider) "" (concat "  " provider))
            (if (string-empty-p model) "" (concat "/" model)))))

(defun t3-code-thread--item-face (type)
  "Return the heading face for normalized item TYPE."
  (pcase type
    ("user_message" 't3-code-thread-user-face)
    ((or "assistant_message" "reasoning") 't3-code-thread-assistant-face)
    ((or "proposed_plan" "todo_list") 't3-code-thread-plan-face)
    ("error" 'error)
    ((or "approval_request" "user_input_request") 'warning)
    (_ 't3-code-thread-action-face)))

(defun t3-code-thread--detail-text (item)
  "Return collapsible detail text for ITEM."
  (let ((type (plist-get item :type))
        (text (plist-get item :text))
        (detail (plist-get item :detail)))
    (if (equal type "reasoning")
        (string-join (delq nil (list text detail)) "\n")
      detail)))

(defun t3-code-thread--body-text (item)
  "Return primary body text for ITEM."
  (unless (equal (plist-get item :type) "reasoning")
    (plist-get item :text)))

(defun t3-code-thread--insert-text (text &optional face)
  "Insert TEXT as a paragraph, optionally using FACE."
  (when (and (stringp text) (not (string-empty-p text)))
    (let ((start (point)))
      (insert text)
      (unless (bolp) (insert "\n"))
      (when face (add-text-properties start (point) (list 'face face))))))

(defun t3-code-thread--insert-item (item)
  "Insert one normalized timeline ITEM."
  (let* ((start (point))
         (id (plist-get item :id))
         (type (plist-get item :type))
         (status (plist-get item :status))
         (label (or (plist-get item :label) type "Activity"))
         (title (plist-get item :title))
         (streaming (eq (plist-get item :streaming) t))
         (detail (t3-code-thread--detail-text item))
         (expanded (and detail (gethash id t3-code-thread--expanded-details))))
    (insert (propertize (upcase label) 'face (t3-code-thread--item-face type)))
    (when title (insert (propertize (format "  %s" title) 'face 'bold)))
    (when streaming (insert (propertize "  ● streaming" 'face 't3-code-thread-assistant-face)))
    (when (member status '("failed" "waiting" "running"))
      (insert (propertize (format "  [%s]" status)
                          'face (if (equal status "failed") 'error 'shadow))))
    (insert "\n")
    (t3-code-thread--insert-text (t3-code-thread--body-text item)
                                 (when (member type '("command_execution" "file_change"))
                                   'fixed-pitch))
    (when detail
      (insert (propertize (if expanded "▾ Details (TAB)\n" "▸ Details (TAB)\n")
                          'face 'font-lock-comment-face))
      (when expanded
        (t3-code-thread--insert-text detail 'fixed-pitch)))
    (insert "\n")
    (add-text-properties start (point)
                         (list 't3-code-thread-item-id id
                               't3-code-thread-has-detail (and detail t)
                               'rear-nonsticky t))))

(defun t3-code-thread--item-at-point ()
  "Return the normalized item ID at point."
  (or (get-char-property (point) 't3-code-thread-item-id)
      (and (> (point) (point-min))
           (get-char-property (1- (point)) 't3-code-thread-item-id))))

(defun t3-code-thread--goto-item (id)
  "Move point to timeline item ID, returning non-nil when found."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (< (point) (point-max)))
      (if (equal (get-char-property (point) 't3-code-thread-item-id) id)
          (setq found t)
        (forward-line 1)))
    found))

(defun t3-code-thread--refresh ()
  "Render the current normalized thread payload."
  (when (derived-mode-p 't3-code-thread-mode)
    (let ((item-id (t3-code-thread--item-at-point))
          (fallback-point (point))
          (line-offset (- (point) (line-beginning-position)))
          (window-starts
           (mapcar (lambda (window) (cons window (window-start window)))
                   (get-buffer-window-list (current-buffer) nil t)))
          (inhibit-read-only t))
      (erase-buffer)
      (cond
       ((null t3-code-thread--payload)
        (if (and t3-code-thread--environment
                 (eq (t3-code-environment-state t3-code-thread--environment)
                     'disconnected))
            (insert (propertize
                     "Thread unavailable: environment disconnected. Press g to reconnect.\n"
                     'face 'warning))
          (insert (propertize "Loading thread…\n" 'face 'shadow))))
       ((plist-get t3-code-thread--payload :error)
        (insert (propertize "Could not load thread\n\n" 'face 'error)
                (plist-get t3-code-thread--payload :error) "\n"))
       ((plist-get t3-code-thread--payload :deleted)
        (insert (propertize "This thread was deleted.\n" 'face 'warning)))
       (t
        (when (eq (plist-get t3-code-thread--payload :truncated) t)
          (insert (propertize "Showing the most recent bounded timeline.\n\n"
                              'face 'font-lock-comment-face)))
        (dolist (item (plist-get t3-code-thread--payload :items))
          (t3-code-thread--insert-item item))
        (when (null (plist-get t3-code-thread--payload :items))
          (insert (propertize "No timeline items yet.\n" 'face 'shadow)))))
      (goto-char (point-min))
      (if (and item-id (t3-code-thread--goto-item item-id))
          (move-to-column line-offset)
        (goto-char (min fallback-point (point-max))))
      (dolist (entry window-starts)
        (when (window-live-p (car entry))
          (set-window-start (car entry)
                            (min (cdr entry) (point-max)) t))))))

(defun t3-code-thread-toggle-details ()
  "Expand or collapse details for the timeline item at point."
  (interactive)
  (let ((id (t3-code-thread--item-at-point)))
    (unless id (user-error "No T3 timeline item at point"))
    (unless (get-char-property (point) 't3-code-thread-has-detail)
      (user-error "This timeline item has no details"))
    (if (gethash id t3-code-thread--expanded-details)
        (remhash id t3-code-thread--expanded-details)
      (puthash id t t3-code-thread--expanded-details))
    (t3-code-thread--refresh)))

(defun t3-code-thread--id (kind)
  "Return a stable-enough semantic identifier for KIND."
  (format "emacs-%s-%s"
          kind
          (secure-hash 'sha256
                       (format "%s:%s:%s:%s"
                               (emacs-pid) (float-time) (random) t3-code-thread--thread-id))))

(defun t3-code-thread--request (operation input &optional success-message callback)
  "Send mutation OPERATION with INPUT and report completion.
SUCCESS-MESSAGE is displayed after success; CALLBACK receives the result."
  (let ((environment t3-code-thread--environment)
        (buffer (current-buffer)))
    (unless (eq (t3-code-environment-state environment) 'ready)
      (user-error "T3 environment is not ready"))
    (unless (eq (plist-get (t3-code-environment-capabilities environment) :mutations) t)
      (user-error "Connected bridge does not advertise mutations"))
    (t3-code-request
     environment operation input
     (lambda (result error)
       (if error
           (message "T3 mutation failed: %s"
                    (or (plist-get error :message) error))
         (when success-message (message "%s" success-message))
         (when callback (funcall callback result))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (force-mode-line-update t))))))))

(defun t3-code-thread--current-runtime-mode ()
  "Return the current or configured runtime mode."
  (or (plist-get (plist-get t3-code-thread--payload :thread) :runtimeMode)
      t3-code-thread-default-runtime-mode))

(defun t3-code-thread--current-interaction-mode ()
  "Return the current or configured interaction mode."
  (or (plist-get (plist-get t3-code-thread--payload :thread) :interactionMode)
      t3-code-thread-default-interaction-mode))

(defun t3-code-thread--send-input (thread-id text dispatch-mode)
  "Build a normalized send input for THREAD-ID, TEXT, and DISPATCH-MODE."
  (list :threadId thread-id
        :commandId (t3-code-thread--id "command")
        :messageId (t3-code-thread--id "message")
        :text text
        :dispatchMode dispatch-mode))

(defun t3-code-compose-send ()
  "Send the composed message and close the composer after acceptance."
  (interactive)
  (let ((text (string-trim (buffer-substring-no-properties (point-min) (point-max)))))
    (when (string-empty-p text) (user-error "Message is empty"))
    (let ((composer (current-buffer))
          (origin t3-code-compose--origin-buffer)
          (thread-id t3-code-compose--thread-id)
          (dispatch-mode t3-code-compose--dispatch-mode))
      (unless (buffer-live-p origin) (user-error "Thread viewer was closed"))
      (with-current-buffer origin
        (t3-code-thread--request
         "thread.send"
         (t3-code-thread--send-input thread-id text dispatch-mode)
         (format "T3 message dispatched (%s)" dispatch-mode)
         (lambda (_result)
           (when (buffer-live-p composer) (kill-buffer composer))
           (when (buffer-live-p origin) (pop-to-buffer origin))))))))

(defun t3-code-compose-cancel ()
  "Cancel composition without sending."
  (interactive)
  (kill-buffer (current-buffer)))

(defvar-keymap t3-code-compose-mode-map
  :parent text-mode-map
  "C-c C-c" #'t3-code-compose-send
  "C-c C-k" #'t3-code-compose-cancel)

;; `defvar-keymap' does not replace an existing map when this file is reloaded.
(keymap-set t3-code-compose-mode-map "C-c C-c" #'t3-code-compose-send)
(keymap-set t3-code-compose-mode-map "C-c C-k" #'t3-code-compose-cancel)

(define-derived-mode t3-code-compose-mode text-mode "T3-Compose"
  "Major mode for composing a T3 thread message."
  (setq header-line-format
        " T3 message — C-c C-c send, C-c C-k cancel")
  (visual-line-mode 1))

(defun t3-code-thread-compose (&optional dispatch-mode)
  "Compose a multiline message using DISPATCH-MODE."
  (interactive)
  (let* ((dispatch-mode (or dispatch-mode "auto"))
         (origin (current-buffer))
         (buffer (generate-new-buffer
                  (format "*t3-compose:%s*" t3-code-thread--thread-id))))
    (with-current-buffer buffer
      (t3-code-compose-mode)
      (setq t3-code-compose--environment
            (buffer-local-value 't3-code-thread--environment origin)
            t3-code-compose--thread-id
            (buffer-local-value 't3-code-thread--thread-id origin)
            t3-code-compose--dispatch-mode dispatch-mode
            t3-code-compose--origin-buffer origin))
    (pop-to-buffer buffer)))

(defun t3-code-thread-compose-auto ()
  "Compose and automatically steer, queue, or start as supported."
  (interactive)
  (t3-code-thread-compose "auto"))

(defun t3-code-thread-compose-queue ()
  "Compose a message queued after active work."
  (interactive)
  (t3-code-thread-compose "queue"))

(defun t3-code-thread-compose-steer ()
  "Compose a message that steers active work."
  (interactive)
  (t3-code-thread-compose "steer"))

(defun t3-code-thread-compose-restart ()
  "Compose a message that interrupts and restarts active work."
  (interactive)
  (t3-code-thread-compose "restart"))

(defun t3-code-thread-interrupt ()
  "Interrupt the current active run."
  (interactive)
  (when (yes-or-no-p "Interrupt the active T3 run? ")
    (t3-code-thread--request
     "thread.interrupt"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "interrupt"))
     "T3 interrupt requested")))

(defun t3-code-thread--item-by-id (id)
  "Return normalized timeline item ID."
  (seq-find (lambda (item) (equal (plist-get item :id) id))
            (plist-get t3-code-thread--payload :items)))

(defun t3-code-thread--approval-item ()
  "Return the approval item at point or the latest actionable approval."
  (let ((at-point (t3-code-thread--item-by-id (t3-code-thread--item-at-point))))
    (if (and at-point (equal (plist-get at-point :type) "approval_request")
             (plist-get at-point :actionId))
        at-point
      (seq-find (lambda (item)
                  (and (equal (plist-get item :type) "approval_request")
                       (plist-get item :actionId)
                       (member (plist-get item :status) '("pending" "waiting"))))
                (reverse (plist-get t3-code-thread--payload :items))))))

(defun t3-code-thread-respond-approval (decision)
  "Respond to the current approval with DECISION."
  (let ((item (t3-code-thread--approval-item)))
    (unless item (user-error "No pending approval in this thread"))
    (t3-code-thread--request
     "thread.approval.respond"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "approval")
           :requestId (plist-get item :actionId)
           :decision decision)
     (format "T3 approval response sent: %s" decision))))

(defun t3-code-thread-approve ()
  "Approve the current request once."
  (interactive)
  (t3-code-thread-respond-approval "accept"))

(defun t3-code-thread-approve-session ()
  "Approve the current request for this provider session."
  (interactive)
  (t3-code-thread-respond-approval "acceptForSession"))

(defun t3-code-thread-decline ()
  "Decline the current approval request."
  (interactive)
  (t3-code-thread-respond-approval "decline"))

(defun t3-code-thread-cancel-approval ()
  "Cancel the turn associated with the current approval request."
  (interactive)
  (when (yes-or-no-p "Cancel the turn awaiting approval? ")
    (t3-code-thread-respond-approval "cancel")))

(defun t3-code-thread-set-runtime-mode ()
  "Choose and set the thread runtime mode."
  (interactive)
  (let ((mode (completing-read
               "Runtime mode: "
               '("full-access" "auto" "auto-accept-edits" "approval-required")
               nil t nil nil (t3-code-thread--current-runtime-mode))))
    (t3-code-thread--request
     "thread.runtimeMode.set"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "runtime-mode")
           :runtimeMode mode)
     (format "T3 runtime mode: %s" mode)
     (lambda (_result)
       (when-let* ((thread (plist-get t3-code-thread--payload :thread)))
         (plist-put thread :runtimeMode mode))))))

(defun t3-code-thread-set-interaction-mode ()
  "Choose and set default or plan interaction mode."
  (interactive)
  (let ((mode (completing-read
               "Interaction mode: " '("default" "plan") nil t nil nil
               (t3-code-thread--current-interaction-mode))))
    (t3-code-thread--request
     "thread.interactionMode.set"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "interaction-mode")
           :interactionMode mode)
     (format "T3 interaction mode: %s" mode)
     (lambda (_result)
       (when-let* ((thread (plist-get t3-code-thread--payload :thread)))
         (plist-put thread :interactionMode mode))))))

(defun t3-code-thread-settle ()
  "Move this thread to the settled section."
  (interactive)
  (t3-code-thread--request
   "thread.settled.set"
   (list :threadId t3-code-thread--thread-id
         :commandId (t3-code-thread--id "settle") :settled t)
   "T3 thread settled"))

(defun t3-code-thread-reactivate ()
  "Reactivate this thread."
  (interactive)
  (t3-code-thread--request
   "thread.settled.set"
   (list :threadId t3-code-thread--thread-id
         :commandId (t3-code-thread--id "reactivate") :settled :false)
   "T3 thread reactivated"))

(defun t3-code-thread-snooze ()
  "Snooze this thread for a selected duration."
  (interactive)
  (let* ((choice (completing-read "Snooze for: " '("1 hour" "1 day" "1 week") nil t))
         (seconds (pcase choice ("1 hour" 3600) ("1 day" 86400) (_ (* 7 86400))))
         (until (format-time-string "%Y-%m-%dT%H:%M:%S.000Z"
                                    (time-add nil seconds) t)))
    (t3-code-thread--request
     "thread.snooze.set"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "snooze")
           :snoozedUntil until)
     (format "T3 thread snoozed until %s" until))))

(defun t3-code-thread-unsnooze ()
  "Clear the thread snooze."
  (interactive)
  (t3-code-thread--request
   "thread.snooze.set"
   (list :threadId t3-code-thread--thread-id
         :commandId (t3-code-thread--id "unsnooze")
         :snoozedUntil nil)
   "T3 thread snooze cleared"))

(defun t3-code-thread--worktree-path ()
  "Return the locally meaningful worktree path."
  (or (plist-get (plist-get t3-code-thread--payload :thread) :worktreePath)
      (plist-get t3-code-thread--summary :path)))

(defun t3-code-thread-open-worktree ()
  "Open the thread worktree in Dired."
  (interactive)
  (let ((path (t3-code-thread--worktree-path)))
    (unless (and (stringp path) (file-directory-p path))
      (user-error "Thread worktree is not locally accessible: %s" path))
    (dired path)))

(defun t3-code-thread-open-status ()
  "Open a version-control status/diff view for the worktree."
  (interactive)
  (let ((path (t3-code-thread--worktree-path)))
    (unless (and (stringp path) (file-directory-p path))
      (user-error "Thread worktree is not locally accessible: %s" path))
    (if (fboundp 'magit-status)
        (magit-status path)
      (vc-dir path))))

(defun t3-code-thread-copy-thread-id ()
  "Copy the current thread ID."
  (interactive)
  (kill-new t3-code-thread--thread-id)
  (message "Copied T3 thread ID"))

(defun t3-code-thread-copy-run-id ()
  "Copy the active run ID."
  (interactive)
  (let ((id (plist-get (plist-get t3-code-thread--payload :thread) :activeRunId)))
    (unless id (user-error "No active run"))
    (kill-new id)
    (message "Copied T3 run ID")))

(transient-define-prefix t3-code-thread-actions ()
  "Actions for the current T3 thread."
  [["Message"
    ("m" "Send / auto" t3-code-thread-compose-auto)
    ("q" "Queue" t3-code-thread-compose-queue)
    ("e" "Steer" t3-code-thread-compose-steer)
    ("R" "Restart with message" t3-code-thread-compose-restart)]
   ["Run and approval"
    ("i" "Interrupt" t3-code-thread-interrupt)
    ("y" "Approve once" t3-code-thread-approve)
    ("Y" "Approve session" t3-code-thread-approve-session)
    ("n" "Decline" t3-code-thread-decline)
    ("x" "Cancel turn" t3-code-thread-cancel-approval)]
   ["Modes"
    ("r" "Runtime mode" t3-code-thread-set-runtime-mode)
    ("p" "Interaction mode" t3-code-thread-set-interaction-mode)]
   ["Lifecycle"
    ("s" "Settle" t3-code-thread-settle)
    ("u" "Reactivate" t3-code-thread-reactivate)
    ("z" "Snooze" t3-code-thread-snooze)
    ("Z" "Unsnooze" t3-code-thread-unsnooze)]
   ["Open / copy"
    ("w" "Worktree" t3-code-thread-open-worktree)
    ("d" "VC status / diff" t3-code-thread-open-status)
    ("c" "Copy thread ID" t3-code-thread-copy-thread-id)
    ("C" "Copy run ID" t3-code-thread-copy-run-id)]])

(defun t3-code-thread-reconnect ()
  "Reconnect the shared environment used by this thread."
  (interactive)
  (t3-code-restart t3-code-thread--environment))

(defun t3-code-thread-quit ()
  "Kill the current thread viewer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun t3-code-thread--cleanup ()
  "Release this buffer's thread subscription."
  (when (and t3-code-thread--environment t3-code-thread--subscription)
    (t3-code-unsubscribe t3-code-thread--environment t3-code-thread--subscription)
    (setq t3-code-thread--subscription nil)))

(defvar-keymap t3-code-thread-mode-map
  :parent special-mode-map
  "TAB" #'t3-code-thread-toggle-details
  "<tab>" #'t3-code-thread-toggle-details
  "RET" #'t3-code-thread-toggle-details
  "a" #'t3-code-thread-actions
  "." #'t3-code-thread-actions
  "m" #'t3-code-thread-compose-auto
  "i" #'t3-code-thread-interrupt
  "g" #'t3-code-thread-reconnect
  "q" #'t3-code-thread-quit)

;; Keep live thread buffers useful after evaluating a newer package version:
;; `defvar-keymap' preserves the old map object across reloads.
(keymap-set t3-code-thread-mode-map "TAB" #'t3-code-thread-toggle-details)
(keymap-set t3-code-thread-mode-map "<tab>" #'t3-code-thread-toggle-details)
(keymap-set t3-code-thread-mode-map "RET" #'t3-code-thread-toggle-details)
(keymap-set t3-code-thread-mode-map "a" #'t3-code-thread-actions)
(keymap-set t3-code-thread-mode-map "." #'t3-code-thread-actions)
(keymap-set t3-code-thread-mode-map "m" #'t3-code-thread-compose-auto)
(keymap-set t3-code-thread-mode-map "i" #'t3-code-thread-interrupt)
(keymap-set t3-code-thread-mode-map "g" #'t3-code-thread-reconnect)
(keymap-set t3-code-thread-mode-map "q" #'t3-code-thread-quit)

(define-derived-mode t3-code-thread-mode special-mode "T3-Thread"
  "Major mode for a live, read-only T3 thread timeline."
  (setq header-line-format '(:eval (t3-code-thread--header-line))
        t3-code-thread--expanded-details (make-hash-table :test #'equal))
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'t3-code-thread--cleanup nil t))

(defun t3-code-thread-open (environment thread)
  "Open normalized THREAD from ENVIRONMENT in a live viewer."
  (let* ((thread-id (plist-get thread :id))
         (buffer (get-buffer-create
                  (format "*t3:%s/%s*" (t3-code-environment-id environment) thread-id))))
    (with-current-buffer buffer
      (unless (derived-mode-p 't3-code-thread-mode)
        (t3-code-thread-mode))
      (setq t3-code-thread--environment environment
            t3-code-thread--thread-id thread-id
            t3-code-thread--summary thread)
      (unless t3-code-thread--subscription
        (setq t3-code-thread--subscription
              (t3-code-subscribe
               environment "thread" thread-id
               (lambda (message)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (when (member (plist-get message :kind) '("snapshot" "event"))
                       (setq t3-code-thread--payload (plist-get message :payload))
                       (t3-code-thread--refresh))))))))
      (t3-code-thread--refresh))
    (pop-to-buffer buffer)
    buffer))

(provide 't3-code-thread)
;;; t3-code-thread.el ends here
