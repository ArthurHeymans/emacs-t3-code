;;; t3-code-thread.el --- Read-only T3 thread viewer  -*- lexical-binding: t; -*-

;;; Commentary:

;; Renders the normalized thread projection supplied by the version-matched
;; bridge.  Raw T3 contracts and provider payloads never reach this buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'transient)
(require 't3-code-core)
(require 't3-code-render)

(defvar-local t3-code-thread--environment nil)
(defvar-local t3-code-thread--thread-id nil)
(defvar-local t3-code-thread--summary nil)
(defvar-local t3-code-thread--subscription nil)
(defvar-local t3-code-thread--payload nil)
(defvar-local t3-code-thread--composer nil)

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
(defvar-local t3-code-compose--sending nil)
(defvar-local t3-code-compose--history nil)
(defvar-local t3-code-compose--history-index nil)
(defvar-local t3-code-compose--history-draft nil)

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
            (if (string-empty-p model) "" (concat "/" model))
            (format "  · %s · %d unseen updates" t3-code-thread--view t3-code-thread--unseen)
            (when (and t3-code-thread--environment
                       (not (eq (t3-code-environment-state t3-code-thread--environment) 'ready)))
              "  · disconnected / synchronizing"))))

(defun t3-code-thread--item-face (type)
  "Return the heading face for normalized item TYPE."
  (pcase type
    ("user_message" 't3-code-thread-user-face)
    ((or "assistant_message" "reasoning") 't3-code-thread-assistant-face)
    ((or "proposed_plan" "todo_list") 't3-code-thread-plan-face)
    ("error" 'error)
    ((or "approval_request" "user_input_request") 'warning)
    (_ 't3-code-thread-action-face)))

(defun t3-code-thread--id (kind)
  "Return a stable-enough semantic identifier for KIND."
  (format "emacs-%s-%s"
          kind
          (secure-hash 'sha256
                       (format "%s:%s:%s:%s"
                               (emacs-pid) (float-time) (random) t3-code-thread--thread-id))))

(defun t3-code-thread--request (operation input &optional success-message callback error-callback)
  "Send mutation OPERATION with INPUT and report completion.
SUCCESS-MESSAGE is displayed after success; CALLBACK receives the result.
ERROR-CALLBACK receives failures without discarding caller state."
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
           (progn
             (when error-callback (funcall error-callback error))
             (message "T3 mutation failed: %s"
                      (or (plist-get error :message) error)))
         (when success-message (message "%s" success-message))
         (when callback
           (if (buffer-live-p buffer)
               (with-current-buffer buffer (funcall callback result))
             (funcall callback result)))
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

(defun t3-code-compose-send (&optional mode)
  "Send the draft using MODE or the displayed dispatch mode.
Keep the input buffer and preserve edits made while acceptance is pending."
  (interactive)
  (when t3-code-compose--sending (user-error "A send is already pending"))
  (let* ((composer (current-buffer))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (tick (buffer-chars-modified-tick))
         (origin t3-code-compose--origin-buffer)
         (thread-id t3-code-compose--thread-id)
         (dispatch-mode (or mode t3-code-compose--dispatch-mode "auto")))
    (when (string-empty-p (string-trim text)) (user-error "Message is empty"))
    (unless (buffer-live-p origin) (user-error "Reopen the thread before sending this draft"))
    (when (and (equal dispatch-mode "restart")
               (not (yes-or-no-p "Interrupt and restart with this message? ")))
      (user-error "Restart cancelled"))
    (setq t3-code-compose--sending t)
    (condition-case error
        (with-current-buffer origin
          (t3-code-thread--request
           "thread.send" (t3-code-thread--send-input thread-id text dispatch-mode)
           (format "T3 message dispatched (%s)" dispatch-mode)
           (lambda (_result)
             (when (buffer-live-p composer)
               (with-current-buffer composer
                 (setq t3-code-compose--sending nil
                       t3-code-compose--history-index nil
                       t3-code-compose--history
                       (seq-take (cons text (delete text t3-code-compose--history)) 100))
                 ;; A revision check is intentionally conservative: edits and
                 ;; undo during the request must never be erased by its reply.
                 (if (= tick (buffer-chars-modified-tick))
                     (progn (erase-buffer) (set-buffer-modified-p nil))
                   (message "Sent; newer draft edits retained"))
                 (force-mode-line-update))))
           (lambda (_error)
             (when (buffer-live-p composer)
               (with-current-buffer composer
                 (setq t3-code-compose--sending nil)
                 (force-mode-line-update))))))
      (error (setq t3-code-compose--sending nil)
             (signal (car error) (cdr error))))))

(defun t3-code-compose-steer ()
  "Send the current draft as a steering message."
  (interactive)
  (t3-code-compose-send "steer"))

(defun t3-code-compose-queue ()
  "Send the current draft as queued follow-up work."
  (interactive)
  (t3-code-compose-send "queue"))

(defun t3-code-compose-history-previous (&optional next)
  "Recall an older prompt, or a newer one when NEXT is non-nil."
  (interactive)
  (unless t3-code-compose--history (user-error "No sent prompt history"))
  (unless t3-code-compose--history-index
    (setq t3-code-compose--history-draft (buffer-string)))
  (let ((index (+ (or t3-code-compose--history-index -1) (if next -1 1))))
    (when (>= index (length t3-code-compose--history)) (user-error "Oldest prompt"))
    (setq t3-code-compose--history-index (and (>= index 0) index))
    (erase-buffer)
    (insert (if t3-code-compose--history-index
                (nth index t3-code-compose--history)
              (or t3-code-compose--history-draft "")))))

(defun t3-code-compose-history-next ()
  "Recall a newer prompt or restore the draft."
  (interactive)
  (t3-code-compose-history-previous t))

(defun t3-code-compose-cancel ()
  "Hide composition without sending or discarding the draft."
  (interactive)
  (quit-window))

(defvar-keymap t3-code-compose-mode-map
  :parent text-mode-map
  "C-c C-c" #'t3-code-compose-send
  "C-c C-k" #'t3-code-compose-cancel
  "C-c C-s" #'t3-code-compose-steer
  "C-c C-q" #'t3-code-compose-queue
  "M-p" #'t3-code-compose-history-previous
  "M-n" #'t3-code-compose-history-next)

;; `defvar-keymap' does not replace an existing map when this file is reloaded.
(keymap-set t3-code-compose-mode-map "C-c C-c" #'t3-code-compose-send)
(keymap-set t3-code-compose-mode-map "C-c C-k" #'t3-code-compose-cancel)
(keymap-set t3-code-compose-mode-map "C-c C-s" #'t3-code-compose-steer)
(keymap-set t3-code-compose-mode-map "C-c C-q" #'t3-code-compose-queue)
(keymap-set t3-code-compose-mode-map "M-p" #'t3-code-compose-history-previous)
(keymap-set t3-code-compose-mode-map "M-n" #'t3-code-compose-history-next)

(defun t3-code-compose--header-line ()
  "Display the draft target and current thread policy."
  (let ((origin t3-code-compose--origin-buffer))
    (concat
     " T3 → "
     (if (buffer-live-p origin)
         (with-current-buffer origin
           (concat (or (plist-get (plist-get t3-code-thread--payload :thread) :title)
                       (plist-get t3-code-thread--summary :title)
                       t3-code-thread--thread-id)
                   " · " (t3-code-thread--current-runtime-mode)))
       "thread closed (draft retained)")
     " · " (or t3-code-compose--dispatch-mode "auto")
     (if t3-code-compose--sending " · sending" "")
     " · C-c C-c send · C-c C-k hide")))

(define-derived-mode t3-code-compose-mode text-mode "T3-Compose"
  "Major mode for composing a T3 thread message."
  (setq header-line-format '(:eval (t3-code-compose--header-line)))
  (visual-line-mode 1))

(defun t3-code-thread-compose (&optional dispatch-mode)
  "Compose a multiline message using DISPATCH-MODE."
  (interactive)
  (let* ((dispatch-mode (or dispatch-mode "auto"))
         (origin (current-buffer))
         (buffer (get-buffer-create
                  (format "*t3-input:%s/%s*"
                          (t3-code-environment-id t3-code-thread--environment)
                          t3-code-thread--thread-id))))
    (setq t3-code-thread--composer buffer)
    (with-current-buffer buffer
      (unless (derived-mode-p 't3-code-compose-mode) (t3-code-compose-mode))
      (setq t3-code-compose--environment
            (buffer-local-value 't3-code-thread--environment origin)
            t3-code-compose--thread-id
            (buffer-local-value 't3-code-thread--thread-id origin)
            t3-code-compose--dispatch-mode dispatch-mode
            t3-code-compose--origin-buffer origin))
    (pop-to-buffer buffer
                   '((display-buffer-reuse-window display-buffer-below-selected)
                     (window-height . 0.25)))
    buffer))

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
            (append (plist-get t3-code-thread--payload :items)
                    (plist-get t3-code-thread--payload :attention))))

(defun t3-code-thread--approval-item ()
  "Return the approval item at point or the latest actionable approval."
  (let ((at-point (t3-code-thread--item-by-id (t3-code-thread--item-at-point))))
    (if (and at-point (equal (plist-get at-point :type) "approval_request")
             (plist-get at-point :actionId)
             (member (plist-get at-point :status) '("pending" "waiting")))
        at-point
      (seq-find (lambda (item)
                  (and (equal (plist-get item :type) "approval_request")
                       (plist-get item :actionId)
                       (member (plist-get item :status) '("pending" "waiting"))))
                (reverse (append (plist-get t3-code-thread--payload :items)
                                 (plist-get t3-code-thread--payload :attention)))))))

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

(declare-function t3-code--reconnect "t3-code" (environment))

(defun t3-code-thread-reconnect (&optional restart)
  "Refresh this thread without restarting the shared bridge.
With prefix argument RESTART, start a new bridge (which may require a new
token).  Plain refresh never asks for a token, even after a disconnect."
  (interactive "P")
  (let ((environment t3-code-thread--environment))
    (cond
     ((and (not restart) (eq (t3-code-environment-state environment) 'ready))
      (t3-code-refresh-subscription environment t3-code-thread--subscription))
     ((not restart)
      (user-error "T3 bridge is not ready; use C-u g to reconnect"))
     (t
      (require 't3-code)
      (t3-code--reconnect environment)))))

(defun t3-code-thread-quit ()
  "Hide this frame's thread and input windows without stopping work."
  (interactive)
  (let ((viewer (current-buffer)))
    (when (buffer-live-p t3-code-thread--composer)
      (dolist (window (get-buffer-window-list t3-code-thread--composer nil))
        (quit-window nil window)))
    (dolist (window (get-buffer-window-list viewer nil))
      (quit-window nil window))))

(defun t3-code-thread--cleanup ()
  "Release this buffer's thread subscription."
  (when (and t3-code-thread--environment t3-code-thread--subscription)
    (t3-code-unsubscribe t3-code-thread--environment t3-code-thread--subscription)
    (setq t3-code-thread--subscription nil)))

(defvar-keymap t3-code-thread-mode-map
  :parent special-mode-map
  "TAB" #'t3-code-thread-toggle-details
  "<tab>" #'t3-code-thread-toggle-details
  "RET" #'t3-code-thread-inspect
  "<backtab>" #'t3-code-thread-cycle-view
  "n" #'t3-code-thread-next-turn
  "p" #'t3-code-thread-previous-turn
  "G" #'t3-code-thread-jump-to-latest
  "M-w" #'t3-code-thread-copy
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
(keymap-set t3-code-thread-mode-map "RET" #'t3-code-thread-inspect)
(keymap-set t3-code-thread-mode-map "a" #'t3-code-thread-actions)
(keymap-set t3-code-thread-mode-map "." #'t3-code-thread-actions)
(keymap-set t3-code-thread-mode-map "m" #'t3-code-thread-compose-auto)
(keymap-set t3-code-thread-mode-map "i" #'t3-code-thread-interrupt)
(keymap-set t3-code-thread-mode-map "g" #'t3-code-thread-reconnect)
(keymap-set t3-code-thread-mode-map "q" #'t3-code-thread-quit)
(keymap-set t3-code-thread-mode-map "<backtab>" #'t3-code-thread-cycle-view)
(keymap-set t3-code-thread-mode-map "n" #'t3-code-thread-next-turn)
(keymap-set t3-code-thread-mode-map "p" #'t3-code-thread-previous-turn)
(keymap-set t3-code-thread-mode-map "G" #'t3-code-thread-jump-to-latest)
(keymap-set t3-code-thread-mode-map "M-w" #'t3-code-thread-copy)

(define-derived-mode t3-code-thread-mode special-mode "T3-Thread"
  "Major mode for a live, read-only T3 thread timeline."
  (setq header-line-format '(:eval (t3-code-thread--header-line)))
  (t3-code-thread-render-setup)
  (visual-line-mode 1)
  (add-hook 'kill-buffer-hook #'t3-code-thread--cleanup nil t))

(defun t3-code-thread--receive (message)
  "Apply a thread subscription MESSAGE without discarding the last good view.
A retrying bridge may emit an error snapshot between successful snapshots;
only synchronization proves that its stream has recovered."
  (let* ((kind (plist-get message :kind))
         (payload (plist-get message :payload))
         (error-text (and payload (plist-get payload :error))))
    (cond
     (error-text
      (setq t3-code-thread--stream-error error-text)
      (unless (plist-get t3-code-thread--payload :thread)
        (setq t3-code-thread--payload payload)))
     ((member kind '("snapshot" "event"))
      (setq t3-code-thread--payload payload))
     ((equal kind "synchronized")
      (setq t3-code-thread--stream-error nil)))
    (when (or (not (equal t3-code-thread--payload t3-code-thread--rendered-payload))
              (not (equal t3-code-thread--stream-error t3-code-thread--rendered-error)))
      (t3-code-thread--refresh))))

(defun t3-code-thread-open (environment thread)
  "Open normalized THREAD from ENVIRONMENT in a live viewer."
  (let* ((thread-id (plist-get thread :id))
         (name (format "*t3:%s/%s*" (t3-code-environment-id environment) thread-id))
         (fresh (not (get-buffer name)))
         (buffer (get-buffer-create name)))
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
                     (t3-code-thread--receive message)))))))
      (t3-code-thread--refresh))
    (pop-to-buffer buffer)
    (with-current-buffer buffer
      (when fresh (t3-code-thread-jump-to-latest))
      (t3-code-thread-compose))
    buffer))

(provide 't3-code-thread)
;;; t3-code-thread.el ends here
