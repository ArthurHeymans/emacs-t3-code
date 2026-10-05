;;; t3-code-thread.el --- T3 chat and input buffers  -*- lexical-binding: t; -*-

;;; Commentary:

;; A chat buffer renders the normalized thread projection supplied by the
;; version-matched bridge; an ordinary input buffer below it composes the next
;; message.  Raw T3 contracts and provider payloads never reach these buffers.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'thingatpt)
(require 'transient)
(require 't3-code-core)
(require 't3-code-shell)
(require 't3-code-render)

(defvar-local t3-code-thread--environment nil)
(defvar-local t3-code-thread--thread-id nil)
(defvar-local t3-code-thread--summary nil)
(defvar-local t3-code-thread--subscription nil)
(defvar-local t3-code-thread--payload nil)
(defvar-local t3-code-thread--composer nil)
(defvar-local t3-code-thread--phase "idle")

(defcustom t3-code-thread-default-runtime-mode "full-access"
  "Runtime mode shown for threads without projected policy data."
  :type '(choice (const "approval-required")
                 (const "auto-accept-edits")
                 (const "auto")
                 (const "full-access"))
  :group 't3-code)

(defcustom t3-code-thread-default-interaction-mode "default"
  "Interaction mode shown for threads without projected policy data."
  :type '(choice (const "default") (const "plan"))
  :group 't3-code)

(defcustom t3-code-input-window-height 0.25
  "Maximum height of the input window, as a fraction of the frame or in lines.
The window grows with the draft up to this height."
  :type 'number
  :group 't3-code)

(defcustom t3-code-input-window-min-height 3
  "Minimum height of the input window in lines, excluding its header."
  :type 'integer
  :group 't3-code)

(defcustom t3-code-input-window-display 'always
  "When the input window is shown.
`always' shows it with the chat, `on-demand' hides it after each send,
and `hidden' only opens it when composing (\\`m', \\`i' or \\`a')."
  :type '(choice (const always) (const on-demand) (const hidden))
  :group 't3-code)

(defcustom t3-code-visit-file-other-window t
  "Whether \\`RET' on a file reference visits it in another window.
A prefix argument inverts this for one visit."
  :type 'boolean
  :group 't3-code)

(defcustom t3-code-context-warning-threshold 70
  "Context usage percentage shown with a warning face."
  :type 'integer
  :group 't3-code)

(defcustom t3-code-context-error-threshold 85
  "Context usage percentage shown with an error face."
  :type 'integer
  :group 't3-code)

(defvar t3-code-activity-phase-functions nil
  "Abnormal hook run when a thread's activity phase changes.
Each function receives (CHAT-BUFFER INPUT-BUFFER OLD-PHASE NEW-PHASE), where
phases are \"idle\", \"thinking\", \"replying\", \"running\" or \"waiting\".
INPUT-BUFFER may be nil.  Handlers should be idempotent.")

(defvar-local t3-code-compose--environment nil)
(defvar-local t3-code-compose--thread-id nil)
(defvar-local t3-code-compose--dispatch-mode nil)
(defvar-local t3-code-compose--origin-buffer nil)
(defvar-local t3-code-compose--sending nil)
(defvar-local t3-code-compose--history nil)
(defvar-local t3-code-compose--history-index nil)
(defvar-local t3-code-compose--history-draft nil)
(defvar-local t3-code-compose--launch nil
  "Launch parameters when this input starts a new thread instead.")

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

(defface t3-code-model-face
  '((t :inherit font-lock-type-face))
  "Face for the model name in header lines."
  :group 't3-code)

(defface t3-code-activity-phase-face
  '((t :inherit shadow))
  "Face for the activity phase in header lines."
  :group 't3-code)

(declare-function t3-code-new-thread "t3-code" (&optional arg))
(declare-function t3-code-resume "t3-code" ())
(declare-function t3-code-switch-thread "t3-code" ())
(declare-function t3-code-ledger "t3-code" (&optional environment))
(declare-function t3-code-search-threads "t3-code" (query))
(declare-function t3-code--reconnect "t3-code" (environment))

;;;; Thread state

(defun t3-code-thread--thread ()
  "Return the projected thread plist of the current chat buffer."
  (plist-get t3-code-thread--payload :thread))

(defun t3-code-thread--status-face (status)
  "Return an appropriate face for normalized STATUS."
  (pcase status
    ("running" 't3-code-thread-assistant-face)
    ("waiting-approval" 'warning)
    ("failed" 'error)
    (_ 'shadow)))

(defun t3-code-thread--busy-p ()
  "Whether the current thread has an active run."
  (let ((thread (t3-code-thread--thread)))
    (or (plist-get thread :activeRunId)
        (member (plist-get thread :status) '("running" "waiting-approval")))))

(defun t3-code-thread-activity-phase (payload)
  "Return the activity phase described by thread PAYLOAD."
  (let* ((thread (plist-get payload :thread))
         (streaming (seq-find (lambda (item) (eq (plist-get item :streaming) t))
                              (reverse (plist-get payload :items)))))
    (cond ((null thread) "idle")
          ((equal (plist-get thread :status) "waiting-approval") "waiting")
          ((and streaming (equal (plist-get streaming :type) "reasoning")) "thinking")
          (streaming "replying")
          ((or (plist-get thread :activeRunId)
               (equal (plist-get thread :status) "running"))
           "running")
          (t "idle"))))

(defun t3-code-thread--update-phase ()
  "Run `t3-code-activity-phase-functions' when the phase changed."
  (let ((phase (t3-code-thread-activity-phase t3-code-thread--payload))
        (old t3-code-thread--phase))
    (unless (equal phase old)
      (setq t3-code-thread--phase phase)
      (run-hook-with-args 't3-code-activity-phase-functions
                          (current-buffer)
                          (and (buffer-live-p t3-code-thread--composer) t3-code-thread--composer)
                          old phase))))

(defun t3-code-thread--context-percent ()
  "Return the latest context usage percentage, or nil."
  (when-let* ((usage (plist-get (t3-code-thread--thread) :tokenUsage))
              (max (plist-get usage :maxTokens))
              ((numberp max))
              ((> max 0)))
    (round (* 100.0 (/ (float (plist-get usage :usedTokens)) max)))))

(defun t3-code-thread--context-text ()
  "Return the propertized context usage indicator, or nil."
  (when-let* ((percent (t3-code-thread--context-percent)))
    (propertize (format "ctx %d%%" percent)
                'face (cond ((>= percent t3-code-context-error-threshold) 'error)
                            ((>= percent t3-code-context-warning-threshold) 'warning)
                            (t 'shadow)))))

(defconst t3-code-thread--effort-regexp "effort\\|reasoning\\|thinking"
  "Model option IDs treated as a reasoning-effort level.")

(defun t3-code-thread--effort (selection)
  "Return the reasoning effort value in model SELECTION, or nil."
  (when-let* ((option (seq-find (lambda (option)
                                  (and (stringp (plist-get option :value))
                                       (string-match-p t3-code-thread--effort-regexp
                                                       (format "%s" (plist-get option :id)))))
                                (append (plist-get selection :options) nil))))
    (plist-get option :value)))

(defun t3-code-thread--indicator ()
  "Return the global activity indicator for this buffer's environment."
  (t3-code-shell-indicator (or t3-code-thread--environment t3-code-compose--environment)))

(defun t3-code-thread--escape-header (text)
  "Escape TEXT for `header-line-format', where %-constructs are special."
  (replace-regexp-in-string "%" "%%" text t t))

(defun t3-code-thread--header-line ()
  "Render thread identity and live status."
  (let* ((thread (t3-code-thread--thread))
         (summary t3-code-thread--summary)
         (title (or (plist-get thread :title) (plist-get summary :title)
                    t3-code-thread--thread-id "Thread"))
         (status (or (plist-get thread :status) (plist-get summary :status) "loading"))
         (provider (or (plist-get thread :provider) (plist-get summary :provider) ""))
         (model (or (plist-get thread :model) (plist-get summary :model) ""))
         (running-model (plist-get thread :activeRunModel))
         (running-provider (plist-get thread :activeRunProvider))
         (branch (or (plist-get thread :branch) (plist-get summary :branch))))
    (string-join
     (delq nil
           (list
            (concat " " (propertize title 'face 'bold)
                    "  [" (propertize status 'face (t3-code-thread--status-face status)) "]")
            (unless (string-empty-p provider)
              (propertize (concat provider (if (string-empty-p model) "" (concat "/" model)))
                          'face 't3-code-model-face))
            (when (and running-model
                       (or (not (equal running-model model))
                           (not (equal running-provider provider))))
              (format "running: %s/%s" (or running-provider provider) running-model))
            (when branch (propertize branch 'face 'shadow))
            (unless (equal t3-code-thread--phase "idle")
              (propertize t3-code-thread--phase 'face 't3-code-activity-phase-face))
            (t3-code-thread--context-text)
            (when (> t3-code-thread--unseen 0)
              (format "%d unseen updates" t3-code-thread--unseen))
            (when (and t3-code-thread--environment
                       (not (eq (t3-code-environment-state t3-code-thread--environment) 'ready)))
              (propertize "disconnected / synchronizing" 'face 'warning))
            (t3-code-thread--indicator)))
     " · ")))

(defun t3-code-thread--id (kind)
  "Return a fresh semantic identifier for KIND."
  (t3-code-new-id (format "emacs-%s-" kind)))

(defun t3-code-thread--request (operation input &optional success-message callback error-callback)
  "Send mutation OPERATION with INPUT and report completion.
SUCCESS-MESSAGE is displayed after success; CALLBACK receives the result.
ERROR-CALLBACK receives failures without discarding caller state."
  (let ((environment (or t3-code-thread--environment t3-code-compose--environment))
        (buffer (current-buffer)))
    (unless (eq (t3-code-environment-state environment) 'ready)
      (user-error "T3 environment is not ready"))
    (unless (t3-code-capability-p environment :mutations)
      (user-error "Connected bridge does not advertise mutations"))
    (t3-code-request
     environment operation input
     (lambda (result error)
       (if error
           (progn
             (when error-callback (funcall error-callback error))
             (message "T3 %s failed: %s" operation
                      (or (plist-get error :message) error)))
         (when success-message (message "%s" success-message))
         (when callback
           (if (buffer-live-p buffer)
               (with-current-buffer buffer (funcall callback result))
             (funcall callback result)))
         (when (buffer-live-p buffer)
           (with-current-buffer buffer (force-mode-line-update t))))))))

(defun t3-code-thread--command (command &optional success-message)
  "Dispatch allowlisted thread COMMAND for this thread, reporting SUCCESS-MESSAGE."
  (unless (t3-code-capability-p t3-code-thread--environment :threadLifecycle)
    (user-error "Connected bridge does not support this thread command"))
  (t3-code-thread--request
   "thread.command"
   (list :command (append command (list :threadId t3-code-thread--thread-id
                                        :commandId (t3-code-thread--id "command"))))
   success-message))

(defun t3-code-thread--current-runtime-mode ()
  "Return the current or configured runtime mode."
  (or (plist-get (t3-code-thread--thread) :runtimeMode)
      t3-code-thread-default-runtime-mode))

(defun t3-code-thread--current-interaction-mode ()
  "Return the current or configured interaction mode."
  (or (plist-get (t3-code-thread--thread) :interactionMode)
      t3-code-thread-default-interaction-mode))

(defun t3-code-thread--send-input (thread-id text dispatch-mode)
  "Build a normalized send input for THREAD-ID, TEXT, and DISPATCH-MODE."
  (list :threadId thread-id
        :commandId (t3-code-thread--id "command")
        :messageId (t3-code-thread--id "message")
        :text text
        :dispatchMode dispatch-mode))

;;;; Input buffer

(defun t3-code-compose--chat-buffer ()
  "Return the live chat buffer this input belongs to, or nil."
  (and (buffer-live-p t3-code-compose--origin-buffer) t3-code-compose--origin-buffer))

(defun t3-code-compose--default-mode ()
  "Return the dispatch mode for \\[t3-code-compose-send]: queue while busy."
  (or t3-code-compose--dispatch-mode
      (if (when-let* ((chat (t3-code-compose--chat-buffer)))
            (with-current-buffer chat (t3-code-thread--busy-p)))
          "queue"
        "auto")))

(defun t3-code-compose--sent (text tick)
  "Record sent TEXT, clearing the draft unless it changed since TICK."
  (setq t3-code-compose--sending nil
        t3-code-compose--dispatch-mode nil
        t3-code-compose--history-index nil
        t3-code-compose--history
        (seq-take (cons text (delete text t3-code-compose--history)) 100))
  ;; A revision check is intentionally conservative: edits and undo during
  ;; the request must never be erased by its reply.
  (if (= tick (buffer-chars-modified-tick))
      (progn (erase-buffer) (set-buffer-modified-p nil))
    (message "Sent; newer draft edits retained"))
  (force-mode-line-update))

(defun t3-code-compose--launch-thread (text)
  "Start the new thread described by `t3-code-compose--launch' with TEXT."
  (let* ((composer (current-buffer))
         (environment t3-code-compose--environment)
         (tick (buffer-chars-modified-tick))
         (launch t3-code-compose--launch))
    (setq t3-code-compose--sending t)
    (condition-case error
        (t3-code-request
         environment "thread.create"
         (list :commandId (t3-code-thread--id "launch") :text text
               :projectId (plist-get launch :projectId)
               :modelSelection (t3-code-thread--wire-selection
                                (plist-get launch :modelSelection))
               :runtimeMode (plist-get launch :runtimeMode)
               :interactionMode (plist-get launch :interactionMode)
               :workspaceStrategy (plist-get launch :workspaceStrategy))
         (t3-code-compose--launch-callback composer environment text tick))
      (error (setq t3-code-compose--sending nil)
             (signal (car error) (cdr error))))))

(defun t3-code-compose--launch-callback (composer environment text tick)
  "Return the response handler for launching TEXT from COMPOSER.
ENVIRONMENT hosts the new thread; TICK is the draft revision that was sent."
  (lambda (result error)
    (when (buffer-live-p composer)
      (with-current-buffer composer (setq t3-code-compose--sending nil)))
    (if error
        (message "T3 new thread failed: %s" (or (plist-get error :message) error))
      (let ((history (and (buffer-live-p composer)
                          (with-current-buffer composer
                            (t3-code-compose--sent text tick)
                            t3-code-compose--history))))
        ;; Open the thread from the launch input's window, so the new chat
        ;; and input take over the windows of the chat/input pair.
        (when-let* ((window (and (buffer-live-p composer) (get-buffer-window composer))))
          (select-window window))
        (let ((chat (t3-code-thread-open environment
                                         (list :id (plist-get result :threadId)
                                               :title (t3-code-thread--preview text 60)))))
          (with-current-buffer chat
            (when (buffer-live-p t3-code-thread--composer)
              (with-current-buffer t3-code-thread--composer
                (setq t3-code-compose--history history)))))
        ;; The draft became the first message; drop it unless edited since.
        (when (and (buffer-live-p composer) (= 0 (buffer-size composer)))
          (kill-buffer composer))))))

(defun t3-code-thread--wire-selection (selection)
  "Return model SELECTION ready for JSON, with its options as an array.
Parsed payloads represent JSON arrays as lists, which would serialize as
objects."
  (if (plist-member selection :options)
      (plist-put (copy-sequence selection) :options
                 (vconcat (plist-get selection :options)))
    selection))

(defun t3-code-compose-send (&optional mode)
  "Send the draft using MODE, or queue it while the thread is busy.
Keep the input buffer and preserve edits made while acceptance is pending."
  (interactive)
  (when t3-code-compose--sending (user-error "A send is already pending"))
  (let* ((composer (current-buffer))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (tick (buffer-chars-modified-tick))
         (origin t3-code-compose--origin-buffer)
         (thread-id t3-code-compose--thread-id)
         (dispatch-mode (or mode (t3-code-compose--default-mode))))
    (when (string-empty-p (string-trim text)) (user-error "Message is empty"))
    (if t3-code-compose--launch
        (t3-code-compose--launch-thread text)
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
                   (t3-code-compose--sent text tick)
                   (when (eq t3-code-input-window-display 'on-demand)
                     (dolist (window (get-buffer-window-list composer nil t))
                       (quit-window nil window))))))
             (lambda (_error)
               (when (buffer-live-p composer)
                 (with-current-buffer composer
                   (setq t3-code-compose--sending nil)
                   (force-mode-line-update))))))
        (error (setq t3-code-compose--sending nil)
               (signal (car error) (cdr error)))))))

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

(defun t3-code-compose-history-search ()
  "Replace the draft with a sent prompt chosen with completion."
  (interactive)
  (unless t3-code-compose--history (user-error "No sent prompt history"))
  (let ((prompt (completing-read "Prompt history: " t3-code-compose--history nil t)))
    (setq t3-code-compose--history-index nil
          t3-code-compose--history-draft (buffer-string))
    (erase-buffer)
    (insert prompt)))

(defun t3-code-compose-cancel ()
  "Hide the input window without sending or discarding the draft."
  (interactive)
  (quit-window))

(defmacro t3-code-compose--in-chat (&rest body)
  "Evaluate BODY in this input's chat buffer."
  (declare (indent 0) (debug t))
  `(let ((chat (t3-code-compose--chat-buffer)))
     (unless chat (user-error "This input is not attached to an open thread"))
     (with-current-buffer chat ,@body)))

(defun t3-code-compose-abort ()
  "Interrupt the thread's current response."
  (interactive)
  (t3-code-compose--in-chat (t3-code-thread-interrupt)))

(defun t3-code-compose-select-model ()
  "Select the next model without losing the current message draft."
  (interactive)
  (if t3-code-compose--launch
      (t3-code-compose--select-launch-model)
    (t3-code-compose--in-chat (t3-code-thread-select-model))))

(defun t3-code-compose-cycle-effort ()
  "Cycle the reasoning effort of the next turn, or of the thread to create."
  (interactive)
  (if (not t3-code-compose--launch)
      (t3-code-compose--in-chat (t3-code-thread-cycle-effort))
    (let ((buffer (current-buffer)))
      (t3-code-thread--with-catalog
       t3-code-compose--environment
       (lambda (catalog)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (setq t3-code-compose--launch
                   (plist-put t3-code-compose--launch :modelSelection
                              (t3-code-thread--with-effort
                               catalog (plist-get t3-code-compose--launch :modelSelection)
                               #'t3-code-thread--next-effort)))
             (force-mode-line-update))))))))

(defun t3-code-compose-manage-queue ()
  "Manage this thread's queued messages, as in pi."
  (interactive)
  (t3-code-compose--in-chat (t3-code-thread-manage-queue)))

(defun t3-code-compose-actions ()
  "Open the thread menu from the input buffer.
The menu acts on the chat, so its window is selected first."
  (interactive)
  (let ((chat (t3-code-compose--chat-buffer)))
    (unless chat (user-error "This input is not attached to an open thread"))
    (select-window (or (get-buffer-window chat) (display-buffer chat)))
    (call-interactively #'t3-code-thread-actions)))

(defun t3-code-compose-copy-last ()
  "Copy the last assistant message of this thread."
  (interactive)
  (t3-code-compose--in-chat (t3-code-thread-copy-last-message)))

(defun t3-code-compose--chat-window ()
  "Return a window showing this input's chat buffer."
  (when-let* ((chat (t3-code-compose--chat-buffer)))
    (get-buffer-window chat)))

(defun t3-code-compose-scroll-chat-up ()
  "Scroll the linked chat window forward."
  (interactive)
  (if-let* ((window (t3-code-compose--chat-window)))
      (with-selected-window window
        (condition-case nil (scroll-up-command) (end-of-buffer nil)))
    (user-error "The chat is not visible")))

(defun t3-code-compose-scroll-chat-down ()
  "Scroll the linked chat window backward."
  (interactive)
  (if-let* ((window (t3-code-compose--chat-window)))
      (with-selected-window window
        (condition-case nil (scroll-down-command) (beginning-of-buffer nil)))
    (user-error "The chat is not visible")))

;;;; Completion in the input buffer

(defun t3-code-compose--context ()
  "Return (ENVIRONMENT INSTANCE-ID DIRECTORY) for completion."
  (if t3-code-compose--launch
      (list t3-code-compose--environment
            (plist-get (plist-get t3-code-compose--launch :modelSelection) :instanceId)
            (plist-get t3-code-compose--launch :directory))
    (when-let* ((chat (t3-code-compose--chat-buffer)))
      (with-current-buffer chat
        (let ((thread (t3-code-thread--thread)))
          (list t3-code-thread--environment
                (or (plist-get (plist-get thread :modelSelection) :instanceId)
                    (plist-get thread :provider))
                (t3-code-thread--worktree-path)))))))

(defun t3-code-compose--provider-commands (environment instance)
  "Return cached slash commands and skills of INSTANCE in ENVIRONMENT."
  (when (and instance (t3-code-capability-p environment :composerCompletion))
    (let ((key (list "provider.commands" instance))
          (cache (t3-code-environment-cache environment)))
      (or (gethash key cache)
          (puthash key (t3-code-request-sync environment "provider.commands"
                                             (list :instanceId instance))
                   cache)))))

(defun t3-code-compose--annotated (name description)
  "Return completion candidate NAME annotated with DESCRIPTION."
  (propertize name 't3-code-annotation description))

(defun t3-code-compose--annotate (candidate)
  "Return the annotation of completion CANDIDATE."
  (when-let* ((description (get-text-property 0 't3-code-annotation candidate))
              ((not (string-empty-p description))))
    (concat "  " (propertize description 'face 'completions-annotations))))

(defun t3-code-compose--file-entries (environment directory query)
  "Search project entries below DIRECTORY in ENVIRONMENT for QUERY."
  (when (and directory (t3-code-capability-p environment :composerCompletion))
    (mapcar (lambda (entry) (plist-get entry :path))
            (plist-get (t3-code-request-sync environment "project.searchEntries"
                                             (list :cwd directory :query query :limit 50))
                       :entries))))

(defun t3-code-compose-completion-at-point ()
  "Complete /commands, $skills, @files and ./paths in the input buffer."
  (let* ((end (point))
         (start (save-excursion (skip-chars-backward "^ \t\n") (point)))
         (token (buffer-substring-no-properties start end))
         (context (t3-code-compose--context)))
    (cond
     ((and (string-prefix-p "/" token) (= start (point-min)) context)
      (when-let* ((commands (apply #'t3-code-compose--provider-commands (seq-take context 2))))
        (list (1+ start) end
              (mapcar (lambda (command)
                        (t3-code-compose--annotated (plist-get command :name)
                                                    (plist-get command :description)))
                      (plist-get commands :slashCommands))
              :annotation-function #'t3-code-compose--annotate)))
     ((and (string-prefix-p "$" token) context)
      (when-let* ((commands (apply #'t3-code-compose--provider-commands (seq-take context 2))))
        (list (1+ start) end
              (mapcar (lambda (skill)
                        (t3-code-compose--annotated (plist-get skill :name)
                                                    (plist-get skill :description)))
                      (plist-get commands :skills))
              :annotation-function #'t3-code-compose--annotate)))
     ((and (string-prefix-p "@" token) context)
      (let* ((cache nil)
             (table (completion-table-dynamic
                     (lambda (query)
                       (unless (equal (car cache) query)
                         (setq cache (cons query (t3-code-compose--file-entries
                                                  (nth 0 context) (nth 2 context) query))))
                       (cdr cache)))))
        (list (1+ start) end
              ;; The server matches anywhere in the path, so complete by substring.
              (lambda (string predicate action)
                (if (eq action 'metadata)
                    '(metadata (category . t3-code-file))
                  (funcall table string predicate action))))))
     ;; Paths come from the user's draft, but a remote file name would still
     ;; open a TRAMP connection just for completion.
     ((and (string-match-p "\\`\\(?:\\.\\.?/\\|~/\\|/.\\)" token)
           (not (file-remote-p token)))
      (let ((default-directory (or (t3-code-thread--local-directory (and context (nth 2 context)))
                                   default-directory)))
        (list start end #'completion-file-name-table))))))

(add-to-list 'completion-category-defaults '(t3-code-file (styles substring basic)))

(defun t3-code-compose-complete ()
  "Complete a command, skill, file or path at point, or indent."
  (interactive)
  (unless (completion-at-point)
    (indent-for-tab-command)))

;;;; Input header

(defvar-keymap t3-code-compose--model-map
  "<header-line> <mouse-1>" #'t3-code-compose-select-model)

(defvar-keymap t3-code-compose--effort-map
  "<header-line> <mouse-1>" #'t3-code-compose-cycle-effort
  "<header-line> <mouse-2>" #'t3-code-compose-cycle-effort)

(defun t3-code-compose--model-segment (selection fallback)
  "Return SELECTION's clickable model and reasoning effort, as in pi.
FALLBACK names the model when SELECTION is nil."
  (concat (propertize (if selection
                          (format "%s/%s" (plist-get selection :instanceId)
                                  (plist-get selection :model))
                        fallback)
                      'face 't3-code-model-face
                      'mouse-face 'highlight
                      'help-echo "mouse-1: select model"
                      'local-map t3-code-compose--model-map)
          (when-let* ((effort (and selection (t3-code-thread--effort-default selection))))
            (concat " • "
                    (propertize effort
                                'mouse-face 'highlight
                                'help-echo "mouse-1: cycle reasoning effort"
                                'local-map t3-code-compose--effort-map)))))

(defun t3-code-compose--launch-header ()
  "Describe the thread this input will create."
  (let ((launch t3-code-compose--launch))
    (string-join
     (delq nil
           (list (concat " " (propertize "New thread" 'face 'bold))
                 (plist-get launch :projectName)
                 (let ((strategy (plist-get launch :workspaceStrategy)))
                   (pcase (plist-get strategy :type)
                     ("worktree" (format "new worktree from %s" (plist-get strategy :baseRef)))
                     ("existing_worktree" (abbreviate-file-name
                                           (plist-get strategy :worktreePath)))
                     (_ "project root")))
                 (t3-code-compose--model-segment (plist-get launch :modelSelection) "model")
                 (if t3-code-compose--sending "starting…" "C-c C-c start")))
     " · ")))

(defun t3-code-compose--header-line ()
  "Display the model, activity and policy of the thread this input targets."
  (if t3-code-compose--launch
      (t3-code-compose--launch-header)
    (let ((origin t3-code-compose--origin-buffer)
          (sending t3-code-compose--sending)
          (preselected t3-code-compose--dispatch-mode))
      (if (not (buffer-live-p origin))
          " T3 · thread closed (draft retained)"
        (with-current-buffer origin
          (let* ((thread (t3-code-thread--thread))
                 (selection (plist-get thread :modelSelection))
                 (queued (length (plist-get t3-code-thread--payload :queued))))
            (string-join
             (delq nil
                   (list
                    (concat " " (t3-code-compose--model-segment
                                 selection (or (plist-get thread :model) "model")))
                    (propertize t3-code-thread--phase 'face 't3-code-activity-phase-face)
                    (t3-code-thread--context-text)
                    (propertize (truncate-string-to-width
                                 (or (plist-get thread :title)
                                     (plist-get t3-code-thread--summary :title)
                                     t3-code-thread--thread-id "")
                                 40 nil nil "…")
                                'face 'bold)
                    (t3-code-thread--current-runtime-mode)
                    (when (equal (t3-code-thread--current-interaction-mode) "plan") "plan")
                    (when (> queued 0) (format "%d queued" queued))
                    (when sending "sending")
                    (when preselected
                      (propertize (format "C-c C-c will %s" preselected) 'face 'warning))
                    (t3-code-thread--indicator)))
             " · ")))))))

;;;; Input mode

(defvar-keymap t3-code-compose-mode-map
  :parent text-mode-map
  "TAB" #'t3-code-compose-complete
  "C-c C-c" #'t3-code-compose-send
  "C-c C-s" #'t3-code-compose-steer
  "C-c C-q" #'t3-code-compose-manage-queue
  "C-c C-k" #'t3-code-compose-abort
  "C-c C-p" #'t3-code-compose-actions
  "C-c C-m" #'t3-code-compose-select-model
  "C-c C-t" #'t3-code-compose-cycle-effort
  "C-c C-y" #'t3-code-compose-copy-last
  "C-c C-n" #'t3-code-new-thread
  "C-c C-r" #'t3-code-resume
  "C-c C-b" #'t3-code-ledger
  "C-c C-j" #'t3-code-switch-thread
  "M-p" #'t3-code-compose-history-previous
  "M-n" #'t3-code-compose-history-next
  "C-<up>" #'t3-code-compose-history-previous
  "C-<down>" #'t3-code-compose-history-next
  "C-r" #'t3-code-compose-history-search
  "M-<next>" #'t3-code-compose-scroll-chat-up
  "M-<prior>" #'t3-code-compose-scroll-chat-down)

(define-derived-mode t3-code-compose-mode text-mode "T3-Input"
  "Major mode for composing a T3 thread message.
\\{t3-code-compose-mode-map}"
  (setq header-line-format
        '(:eval (t3-code-thread--escape-header (t3-code-compose--header-line))))
  ;; Replace text-mode's spelling completion; global functions still run.
  (setq-local completion-at-point-functions
              (list #'t3-code-compose-completion-at-point t))
  (add-hook 'after-change-functions #'t3-code-compose--refit nil t)
  (visual-line-mode 1))

(defun t3-code-compose--fit-window (window)
  "Fit input WINDOW to its draft within the configured height bounds."
  (let ((max (if (integerp t3-code-input-window-height)
                 t3-code-input-window-height
               (floor (* t3-code-input-window-height (frame-height (window-frame window))))))
        ;; Fitting heights include the header and mode lines.
        (chrome (- (window-total-height window) (window-body-height window))))
    (fit-window-to-buffer window (max (+ max chrome) 1)
                          (+ t3-code-input-window-min-height chrome))))

(defun t3-code-compose--refit (&rest _)
  "Refit the windows showing this input after its draft changed."
  (dolist (window (get-buffer-window-list nil nil t))
    (t3-code-compose--fit-window window)))

(defun t3-code-compose--prefetch-catalog (environment)
  "Fetch ENVIRONMENT's model catalog in the background so headers show defaults."
  (when (and environment
             (t3-code-capability-p environment :modelSelection)
             (eq (t3-code-environment-state environment) 'ready))
    (let ((cache (t3-code-environment-cache environment)))
      (unless (or (gethash "model.catalog" cache) (gethash "model.catalog/loading" cache))
        (puthash "model.catalog/loading" t cache)
        (t3-code-request environment "model.catalog" nil
                         (lambda (result error)
                           (remhash "model.catalog/loading" cache)
                           (unless error
                             (puthash "model.catalog" result cache)
                             (force-mode-line-update t))))))))

(defun t3-code-compose--input-window-p (window)
  "Whether WINDOW shows a T3 input buffer."
  (and (window-live-p window)
       (with-current-buffer (window-buffer window) (derived-mode-p 't3-code-compose-mode))))

(defun t3-code-compose--display (buffer)
  "Display input BUFFER below the selected chat window and select it.
An input window already there, or selected, is reused rather than split."
  (let ((window (or (get-buffer-window buffer)
                    (and (t3-code-compose--input-window-p (selected-window))
                         (selected-window))
                    (and (t3-code-compose--input-window-p (window-in-direction 'below))
                         (window-in-direction 'below)))))
    (t3-code-compose--prefetch-catalog
     (buffer-local-value 't3-code-compose--environment buffer))
    (if window
        (progn (set-window-buffer window buffer)
               (select-window window)
               (t3-code-compose--fit-window window))
      (pop-to-buffer buffer
                     '((display-buffer-reuse-window display-buffer-below-selected)
                       (window-height . t3-code-compose--fit-window))))))

(defun t3-code-thread-compose (&optional dispatch-mode)
  "Focus this thread's input, preselecting DISPATCH-MODE for the next send."
  (interactive)
  (let* ((origin (current-buffer))
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
            t3-code-compose--dispatch-mode
            (unless (equal dispatch-mode "auto") dispatch-mode)
            t3-code-compose--origin-buffer origin))
    (t3-code-compose--display buffer)
    buffer))

(defun t3-code-thread-compose-auto ()
  "Focus the input; sending queues while the thread is busy."
  (interactive)
  (t3-code-thread-compose))

(defun t3-code-thread-compose-end ()
  "Focus the input with point at the end of the draft."
  (interactive)
  (with-current-buffer (t3-code-thread-compose) (goto-char (point-max))))

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

(defun t3-code-compose-open-launch (environment launch)
  "Open an input that creates a thread in ENVIRONMENT from LAUNCH parameters."
  (let ((buffer (get-buffer-create
                 (format "*t3-input:%s/new:%s*" (t3-code-environment-id environment)
                         (plist-get launch :projectName)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 't3-code-compose-mode) (t3-code-compose-mode))
      (setq t3-code-compose--environment environment
            t3-code-compose--launch launch
            t3-code-compose--origin-buffer nil
            t3-code-compose--thread-id nil))
    (t3-code-compose--display buffer)
    buffer))

(defun t3-code-compose--select-launch-model ()
  "Choose the model of the thread this input will create."
  (let ((buffer (current-buffer)))
    (t3-code-thread--with-catalog
     t3-code-compose--environment
     (lambda (catalog)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((selection (t3-code-thread--read-model-selection
                             catalog (plist-get t3-code-compose--launch :modelSelection))))
             (setq t3-code-compose--launch
                   (plist-put t3-code-compose--launch :modelSelection selection))
             (force-mode-line-update))))))))

;;;; Thread operations

(defun t3-code-thread-interrupt ()
  "Interrupt the current active run."
  (interactive)
  (t3-code-thread--request
   "thread.interrupt"
   (list :threadId t3-code-thread--thread-id
         :commandId (t3-code-thread--id "interrupt"))
   "T3 interrupt requested"))

(defun t3-code-thread--item-by-id (id)
  "Return normalized timeline item ID."
  (seq-find (lambda (item) (equal (plist-get item :id) id))
            (append (plist-get t3-code-thread--payload :attention)
                    (t3-code-thread--items))))

(defun t3-code-thread--pending-item (type)
  "Return the pending request of TYPE at point or the latest one."
  (let ((at-point (t3-code-thread--item-by-id (t3-code-thread--item-at-point)))
        (match (lambda (item)
                 (and (equal (plist-get item :type) type)
                      (plist-get item :actionId)
                      (t3-code-thread--pending-p item)))))
    (if (and at-point (funcall match at-point))
        at-point
      (seq-find match (reverse (append (t3-code-thread--items)
                                       (plist-get t3-code-thread--payload :attention)))))))

(defun t3-code-thread--approval-item ()
  "Return the approval item at point or the latest actionable approval."
  (t3-code-thread--pending-item "approval_request"))

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

(defun t3-code-thread--read-answer (question)
  "Ask QUESTION in the minibuffer and return its answer value.
Multi-select questions collect one choice per prompt, so option labels and
custom answers may contain commas.  Empty answers are refused, as the
server treats them as unanswered."
  (let* ((options (plist-get question :options))
         (labels (mapcar (lambda (option) (plist-get option :label)) options))
         (custom (not (eq (plist-get question :allowCustomAnswer) :false)))
         (prompt (format "%s — %s: " (plist-get question :header)
                         (t3-code-thread--preview (plist-get question :question) 120)))
         (value (lambda (choice)
                  (if-let* ((option (seq-find (lambda (option)
                                                (equal (plist-get option :label) choice))
                                              options)))
                      (plist-get option :value)
                    choice)))
         (completion-extra-properties
          (list :annotation-function
                (lambda (choice)
                  (when-let* ((option (seq-find (lambda (option)
                                                  (equal (plist-get option :label) choice))
                                                options)))
                    (concat "  " (plist-get option :description))))))
         (read-one (lambda (prompt choices)
                     (if options
                         (completing-read prompt choices nil (not custom))
                       (read-string prompt)))))
    (if (eq (plist-get question :multiSelect) t)
        (let (chosen choice)
          (while (not (string-empty-p
                       (setq choice (funcall read-one
                                             (if chosen
                                                 (format "%s[%s] another (empty to finish): "
                                                         prompt (string-join (reverse chosen) ", "))
                                               prompt)
                                             (seq-difference labels chosen)))))
            (push choice chosen))
          (unless chosen (user-error "Choose at least one answer"))
          (vconcat (mapcar value (nreverse chosen))))
      (let ((choice (funcall read-one prompt labels)))
        (when (string-empty-p (string-trim choice)) (user-error "An answer is required"))
        (funcall value choice)))))

(defun t3-code-thread-answer-input ()
  "Answer the pending user-input request at point or the latest one."
  (interactive)
  (let ((item (t3-code-thread--pending-item "user_input_request")))
    (unless item (user-error "No pending input request in this thread"))
    (unless (plist-get item :questions)
      (user-error "The bridge did not supply structured questions for this request"))
    (let ((answers (make-hash-table :test #'equal)))
      (dolist (question (plist-get item :questions))
        (puthash (plist-get question :id) (t3-code-thread--read-answer question) answers))
      (t3-code-thread--command
       (list :type "runtime-request.respond" :requestId (plist-get item :actionId)
             :answers answers)
       "T3 answer sent"))))

(defun t3-code-thread-dismiss-input ()
  "Dismiss the pending user-input request without answering."
  (interactive)
  (let ((item (t3-code-thread--pending-item "user_input_request")))
    (unless item (user-error "No pending input request in this thread"))
    (t3-code-thread--command
     (list :type "thread.user-input.dismiss" :requestId (plist-get item :actionId))
     "T3 input request dismissed")))

;;;; Models

(defun t3-code-thread--with-catalog (environment callback)
  "Call CALLBACK with ENVIRONMENT's model catalog, fetching it once."
  (unless (t3-code-capability-p environment :modelSelection)
    (user-error "Connected bridge does not support model selection"))
  (let ((cache (t3-code-environment-cache environment)))
    (if-let* ((catalog (gethash "model.catalog" cache)))
        (funcall callback catalog)
      (t3-code-request
       environment "model.catalog" nil
       (lambda (result error)
         (if error
             (message "T3 model catalog failed: %s" (or (plist-get error :message) error))
           (puthash "model.catalog" result cache)
           ;; Leave the process filter before prompting in the minibuffer.
           (run-at-time 0 nil callback result)))))))

(defun t3-code-thread--model-options (descriptors current)
  "Prompt for DESCRIPTORS, preserving CURRENT option values as defaults."
  (delq nil
        (mapcar
         (lambda (descriptor)
           (let* ((id (plist-get descriptor :id))
                  (saved (seq-find (lambda (option) (equal id (plist-get option :id))) current))
                  (value (plist-get saved :value))
                  (label (plist-get descriptor :label)))
             (pcase (plist-get descriptor :type)
               ("boolean"
                (let ((answer (completing-read
                               (format "%s: " label) '("default" "yes" "no") nil t nil nil
                               (if saved (if (eq value t) "yes" "no") "default"))))
                  (unless (equal answer "default")
                    ;; nil would serialize as JSON null, which is not a boolean.
                    (list :id id :value (if (equal answer "yes") t :false)))))
               ("select"
                (let* ((choices (plist-get descriptor :choices))
                       (default (or (and (stringp value) value)
                                    (plist-get (seq-find (lambda (choice)
                                                           (eq (plist-get choice :isDefault) t))
                                                         choices) :id)))
                       (answer (completing-read
                                (format "%s: " label)
                                (append '("default") (mapcar (lambda (choice)
                                                                  (plist-get choice :id)) choices))
                                nil t nil nil (or default "default"))))
                  (unless (equal answer "default")
                    (list :id id :value answer)))))))
         descriptors)))

(defun t3-code-thread--read-provider-model (catalog)
  "Read a provider and model from CATALOG; return (PROVIDER . MODEL)."
  (let* ((choices (cl-loop for provider in (plist-get catalog :providers)
                           append (cl-loop for model in (plist-get provider :models)
                                           collect (cons
                                                    (format "%s · %s [%s]%s"
                                                            (plist-get provider :name)
                                                            (plist-get model :name)
                                                            (plist-get provider :instanceId)
                                                            (if (eq (plist-get provider :available) t)
                                                                ""
                                                              " (unavailable)"))
                                                    (cons provider model)))))
         (picked (if choices
                     (assoc (completing-read "Provider / model: " choices nil t) choices)
                   (user-error "No available models in the server catalog")))
         (provider (cadr picked)))
    (unless (eq (plist-get provider :available) t)
      (user-error "Provider unavailable: %s"
                  (or (plist-get provider :reason) (plist-get provider :name))))
    (cdr picked)))

(defun t3-code-thread--read-model-selection (catalog current)
  "Read a model selection from CATALOG, defaulting options from CURRENT."
  (pcase-let* ((`(,provider . ,model) (t3-code-thread--read-provider-model catalog))
               (instance (plist-get provider :instanceId))
               (slug (plist-get model :slug))
               (same-model (and (equal instance (plist-get current :instanceId))
                                (equal slug (plist-get current :model))))
               (descriptors (plist-get model :options))
               (options (if descriptors
                            (t3-code-thread--model-options
                             descriptors (when same-model (plist-get current :options)))
                          (when same-model (plist-get current :options)))))
    (append (list :instanceId instance :model slug)
            (when options (list :options (vconcat options))))))

(defun t3-code-thread--set-model-selection (selection)
  "Persist model SELECTION for this thread's subsequent turns."
  (t3-code-thread--request
   "thread.modelSelection.set"
   (list :threadId t3-code-thread--thread-id
         :commandId (t3-code-thread--id "model")
         :modelSelection (t3-code-thread--wire-selection selection))
   (format "T3 next model: %s/%s%s" (plist-get selection :instanceId)
           (plist-get selection :model)
           (if-let* ((effort (t3-code-thread--effort selection))) (concat " · " effort) ""))
   (lambda (_result)
     (when t3-code-thread--subscription
       (t3-code-refresh-subscription t3-code-thread--environment
                                     t3-code-thread--subscription)))))

(defun t3-code-thread--select-model (catalog)
  "Select a model from normalized CATALOG and persist it on this thread."
  (let* ((thread (t3-code-thread--thread))
         (current (plist-get thread :modelSelection))
         (choice (t3-code-thread--read-provider-model catalog))
         (provider (car choice))
         (instance (plist-get provider :instanceId))
         (slug (plist-get (cdr choice) :slug)))
    (when (and (eq (plist-get thread :hasStartedSession) t)
               (equal instance (plist-get current :instanceId))
               (not (equal slug (plist-get current :model)))
               (eq (plist-get provider :requiresNewThreadForModelChange) t))
      (user-error "This provider needs a new thread to change models"))
    (let* ((same-model (and (equal instance (plist-get current :instanceId))
                            (equal slug (plist-get current :model))))
           (descriptors (plist-get (cdr choice) :options))
           (options (if descriptors
                        (t3-code-thread--model-options
                         descriptors (when same-model (plist-get current :options)))
                      (when same-model (plist-get current :options))))
           (selection (append (list :instanceId instance :model slug)
                              (when options (list :options (vconcat options))))))
      (unless (equal (t3-code-thread--wire-selection selection)
                     (t3-code-thread--wire-selection current))
        (t3-code-thread--set-model-selection selection)))))

(defun t3-code-thread--model-selection-supported-p ()
  "Whether this thread's bridge advertises model selection."
  (t3-code-capability-p t3-code-thread--environment :modelSelection))

(defun t3-code-thread-select-model ()
  "Choose the provider instance, model and options for subsequent turns."
  (interactive)
  (unless (t3-code-thread--model-selection-supported-p)
    (user-error "Connected bridge does not support model selection"))
  (unless (t3-code-thread--thread)
    (user-error "Thread details have not loaded"))
  (let ((buffer (current-buffer)))
    (t3-code-thread--with-catalog
     t3-code-thread--environment
     (lambda (catalog)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer (t3-code-thread--select-model catalog)))))))

(defun t3-code-thread--effort-descriptor (catalog selection)
  "Return the reasoning effort option descriptor of SELECTION's model in CATALOG."
  (let* ((provider (seq-find (lambda (provider)
                               (equal (plist-get provider :instanceId)
                                      (plist-get selection :instanceId)))
                             (plist-get catalog :providers)))
         (model (seq-find (lambda (model) (equal (plist-get model :slug)
                                                 (plist-get selection :model)))
                          (plist-get provider :models))))
    (seq-find (lambda (option)
                (and (equal (plist-get option :type) "select")
                     (string-match-p t3-code-thread--effort-regexp
                                     (plist-get option :id))))
              (plist-get model :options))))

(defun t3-code-thread--effort-default (selection)
  "Return the effort SELECTION uses, consulting the cached catalog.
Without an explicit option the model's default choice applies; nil when
neither is known."
  (or (t3-code-thread--effort selection)
      (when-let* ((environment (or t3-code-thread--environment t3-code-compose--environment))
                  (catalog (gethash "model.catalog" (t3-code-environment-cache environment)))
                  (descriptor (t3-code-thread--effort-descriptor catalog selection)))
        (plist-get (seq-find (lambda (choice) (eq (plist-get choice :isDefault) t))
                             (plist-get descriptor :choices))
                   :id))))

(defun t3-code-thread--with-effort (catalog selection pick)
  "Return SELECTION with the reasoning effort chosen by PICK from CATALOG.
PICK receives the model's levels and the current one and returns a level."
  (let* ((descriptor (or (t3-code-thread--effort-descriptor catalog selection)
                         (user-error "This model has no reasoning effort setting")))
         (id (plist-get descriptor :id))
         (levels (mapcar (lambda (choice) (plist-get choice :id))
                         (plist-get descriptor :choices)))
         (current (or (t3-code-thread--effort selection)
                      (plist-get (seq-find (lambda (choice) (eq (plist-get choice :isDefault) t))
                                           (plist-get descriptor :choices))
                                 :id))))
    (plist-put (copy-sequence selection) :options
               (vconcat (cons (list :id id :value (funcall pick levels current))
                              (seq-remove (lambda (option) (equal (plist-get option :id) id))
                                          (append (plist-get selection :options) nil)))))))

(defun t3-code-thread--next-effort (levels current)
  "Return the level after CURRENT in LEVELS, wrapping around."
  (or (cadr (member current levels)) (car levels)))

(defun t3-code-thread--read-effort (levels current)
  "Read one of LEVELS in the minibuffer, mentioning CURRENT."
  (completing-read (format "Reasoning effort (current: %s): " current) levels nil t))

(defun t3-code-thread--change-effort (pick)
  "Persist the reasoning effort chosen by PICK for subsequent turns.
See `t3-code-thread--with-effort' for PICK."
  (let ((buffer (current-buffer))
        (selection (plist-get (t3-code-thread--thread) :modelSelection)))
    (unless selection (user-error "Thread details have not loaded"))
    (t3-code-thread--with-catalog
     t3-code-thread--environment
     (lambda (catalog)
       (when (buffer-live-p buffer)
         (with-current-buffer buffer
           (let ((next (t3-code-thread--with-effort catalog selection pick)))
             (unless (equal next selection)
               (t3-code-thread--set-model-selection next)))))))))

(defun t3-code-thread-cycle-effort ()
  "Cycle the reasoning effort used for subsequent turns."
  (interactive)
  (t3-code-thread--change-effort #'t3-code-thread--next-effort))

(defun t3-code-thread-select-effort ()
  "Choose the reasoning effort used for subsequent turns from the minibuffer."
  (interactive)
  (t3-code-thread--change-effort #'t3-code-thread--read-effort))

;;;; Modes and lifecycle

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
       (when-let* ((thread (t3-code-thread--thread)))
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
       (when-let* ((thread (t3-code-thread--thread)))
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

(defun t3-code-thread-rename (&optional regenerate)
  "Rename this thread; with prefix argument REGENERATE, let the server title it."
  (interactive "P")
  (if regenerate
      (t3-code-thread--command (list :type "thread.metadata.update" :regenerateTitle t)
                               "T3 title regeneration requested")
    (let ((title (string-trim (read-string "Thread title: "
                                           (plist-get (t3-code-thread--thread) :title)))))
      (when (string-empty-p title) (user-error "Title cannot be empty"))
      (t3-code-thread--command (list :type "thread.metadata.update" :title title)
                               "T3 thread renamed"))))

(defun t3-code-thread--shell-thread ()
  "Return this thread's shell summary, refreshed from the shared store."
  (or (cdr (t3-code-shell-find-thread t3-code-thread--environment t3-code-thread--thread-id))
      t3-code-thread--summary))

(defun t3-code-thread-toggle-pin ()
  "Pin or unpin this thread."
  (interactive)
  (if (eq (plist-get (t3-code-thread--shell-thread) :pinned) t)
      (t3-code-thread--command '(:type "thread.unpin") "T3 thread unpinned")
    (t3-code-thread--command '(:type "thread.pin") "T3 thread pinned")))

(defun t3-code-thread-archive ()
  "Archive this thread and hide its windows."
  (interactive)
  (t3-code-thread--command '(:type "thread.archive") "T3 thread archived")
  (t3-code-thread-quit))

(defun t3-code-thread-unarchive ()
  "Restore this thread from the archive."
  (interactive)
  (t3-code-thread--command '(:type "thread.unarchive") "T3 thread restored"))

(defun t3-code-thread-delete ()
  "Delete this thread permanently."
  (interactive)
  (when (yes-or-no-p "Delete this T3 thread permanently? ")
    (t3-code-thread--command '(:type "thread.delete") "T3 thread deleted")))

(defun t3-code-thread-fork (&optional latest)
  "Fork the conversation at the turn at point into a new thread.
With prefix argument LATEST, or outside any turn, fork from the latest
stable point."
  (interactive "P")
  (unless (t3-code-capability-p t3-code-thread--environment :threadLifecycle)
    (user-error "Connected bridge does not support forking"))
  (let* ((run-id (unless latest (t3-code-thread-run-at-point)))
         (target (t3-code-new-id))
         (environment t3-code-thread--environment)
         (title (or (plist-get (t3-code-thread--thread) :title) "thread")))
    (t3-code-thread--request
     "thread.fork"
     (list :threadId t3-code-thread--thread-id
           :commandId (t3-code-thread--id "fork")
           :targetThreadId target
           :runId run-id)
     (if run-id "T3 forked at turn" "T3 forked from latest")
     (lambda (result)
       (t3-code-thread-open environment
                            (list :id (or (plist-get result :threadId) target)
                                  :title (concat "Fork of " title)))))))

;;;; Queued messages

(defun t3-code-thread--queued ()
  "Return this thread's queued messages."
  (plist-get t3-code-thread--payload :queued))

(defun t3-code-thread-manage-queue ()
  "Edit, cancel, reorder, steer with, or resume queued messages."
  (interactive)
  (let* ((queued (t3-code-thread--queued))
         (choices (seq-map-indexed
                   (lambda (entry index)
                     (cons (format "%d. %s%s" (1+ index)
                                   (t3-code-thread--preview (plist-get entry :text) 70)
                                   (if (eq (plist-get entry :held) t) "  [held]" ""))
                           index))
                   queued)))
    (unless queued (user-error "No queued messages"))
    (let* ((index (cdr (assoc (completing-read "Queued message: " choices nil t) choices)))
           (entry (nth index queued))
           (run-id (plist-get entry :runId))
           (action (read-multiple-choice
                    "Queued message"
                    (append '((?e "edit") (?c "cancel"))
                            (when (plist-get (t3-code-thread--thread) :activeRunId)
                              '((?s "steer now")))
                            (when (> index 0) '((?u "move up")))
                            (when (< index (1- (length queued))) '((?d "move down")))
                            (when (seq-some (lambda (entry) (eq (plist-get entry :held) t))
                                            queued)
                              '((?r "resume queue")))))))
      (pcase (car action)
        (?e (when (eq (plist-get entry :truncated) t)
              ;; Saving would replace the full message with this preview.
              (user-error "This queued message is too long to edit here; cancel and resend it"))
            (let ((text (read-string-from-buffer "Edit queued message" (plist-get entry :text))))
              (t3-code-thread--command (list :type "queued-run.edit" :runId run-id :text text)
                                       "T3 queued message edited")))
        (?c (t3-code-thread--command (list :type "queued-run.cancel" :runId run-id)
                                     "T3 queued message cancelled"))
        (?s (t3-code-thread--command
             (list :type "queued-message.promote-to-steer" :queuedRunId run-id
                   :targetRunId (plist-get (t3-code-thread--thread) :activeRunId))
             "T3 queued message sent as steering"))
        (?u (t3-code-thread--command
             (list :type "queued-run.reorder" :runId run-id
                   :beforeRunId (plist-get (nth (1- index) queued) :runId))
             "T3 queued message moved up"))
        (?d (t3-code-thread--command
             (list :type "queued-run.reorder" :runId run-id
                   :beforeRunId (plist-get (nth (+ index 2) queued) :runId))
             "T3 queued message moved down"))
        (?r (t3-code-thread--command '(:type "queue.resume") "T3 queue resumed"))))))

;;;; History

(defun t3-code-thread-load-older ()
  "Load the page of history before the oldest loaded item."
  (interactive)
  (unless (eq (plist-get t3-code-thread--payload :hasOlderHistory) t)
    (user-error "All history is loaded"))
  (unless (t3-code-capability-p t3-code-thread--environment :threadLifecycle)
    (user-error "Connected bridge cannot load older history"))
  (when (eq t3-code-thread--older-state 'exhausted) (user-error "All history is loaded"))
  (unless (eq t3-code-thread--older-state 'loading)
    (let ((buffer (current-buffer))
          (first (plist-get (car (t3-code-thread--items)) :id)))
      (setq t3-code-thread--older-state 'loading)
      (t3-code-thread--refresh)
      (t3-code-request
       t3-code-thread--environment "thread.history"
       (list :threadId t3-code-thread--thread-id :beforeItemId first)
       (lambda (result error)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (if error
                 (progn (setq t3-code-thread--older-state nil)
                        (message "T3 history failed: %s" (or (plist-get error :message) error)))
               (setq t3-code-thread--older-items (append (plist-get result :items)
                                                         t3-code-thread--older-items)
                     t3-code-thread--older-state (unless (eq (plist-get result :hasMore) t)
                                                   'exhausted)))
             (t3-code-thread--refresh))))))))

;;;; Files and links

(defun t3-code-thread--worktree-path ()
  "Return the locally meaningful worktree path."
  (or (plist-get (t3-code-thread--thread) :worktreePath)
      (plist-get (t3-code-thread--shell-thread) :path)))

(defun t3-code-thread--parse-location (text)
  "Split TEXT such as path:12:3 or path#L12 into (PATH LINE COLUMN)."
  (let ((text (string-trim text "[[`'\"(<]+" "[]`'\")>,;.!?]+")))
    (cond
     ((string-match "\\`\\(.+?\\)#L\\([0-9]+\\)\\(?:-L?[0-9]+\\)?\\'" text)
      (list (match-string 1 text) (string-to-number (match-string 2 text)) nil))
     ((string-match "\\`\\(.+?\\):\\([0-9]+\\)\\(?::\\([0-9]+\\)\\)?:?\\'" text)
      (list (match-string 1 text) (string-to-number (match-string 2 text))
            (and (match-string 3 text) (string-to-number (match-string 3 text)))))
     (t (list text nil nil)))))

(defun t3-code-thread--local-directory (directory)
  "Return DIRECTORY as a directory name when it is local and exists."
  (and (stringp directory) (not (file-remote-p directory))
       (file-directory-p directory)
       (file-name-as-directory directory)))

(defun t3-code-thread--resolve-file (candidate root)
  "Resolve CANDIDATE against ROOT; return (FILE LINE COLUMN) when it exists.
Remote names are ignored: agent text must not open TRAMP connections."
  (pcase-let ((`(,path ,line ,column) (t3-code-thread--parse-location candidate)))
    (unless (or (string-empty-p path) (string-match-p "\\`[a-z]+://" path)
                (file-remote-p path))
      (let ((file (expand-file-name path (or (t3-code-thread--local-directory root)
                                             default-directory))))
        (when (and (not (file-remote-p file)) (file-exists-p file))
          (list file line column))))))

(defun t3-code-thread--markdown-link-at-point ()
  "Return the target of a Markdown link around point, or nil."
  (save-excursion
    (let ((position (point)) target)
      (goto-char (line-beginning-position))
      (while (and (not target)
                  (re-search-forward "\\[[^]\n]*\\](\\([^)\n]+\\))" (line-end-position) t))
        (when (and (<= (match-beginning 0) position) (< position (match-end 0)))
          (setq target (match-string-no-properties 1))))
      target)))

(defun t3-code-thread--file-target-at-point ()
  "Return (FILE LINE COLUMN) for the file reference at point, or nil."
  (let* ((item (t3-code-thread--item-by-id (t3-code-thread--item-at-point)))
         (row (t3-code-thread--row-at-point))
         (root (t3-code-thread--worktree-path)))
    (seq-some (lambda (candidate) (and candidate (t3-code-thread--resolve-file candidate root)))
              (list (and (plist-get row :header) (plist-get item :path))
                    (t3-code-thread--markdown-link-at-point)
                    ;; Bare words such as "test" are prose, not references.
                    (let ((word (thing-at-point 'filename t)))
                      (and word (string-match-p "[/.]" word) word))))))

(defun t3-code-thread--visit-file (target &optional invert)
  "Visit TARGET (FILE LINE COLUMN); INVERT `t3-code-visit-file-other-window'."
  (pcase-let ((`(,file ,line ,column) target))
    (if (xor t3-code-visit-file-other-window invert)
        (find-file-other-window file)
      (find-file file))
    (when line
      (unless (and (<= (point-min) (point)) (= (point-min) 1)) (widen))
      (goto-char (point-min))
      (forward-line (1- line))
      (when column (move-to-column (max 0 (1- column)))))))

(defun t3-code-thread-visit-at-point (&optional invert)
  "Visit the URL or file reference at point; return non-nil when one exists.
With prefix argument INVERT, invert `t3-code-visit-file-other-window'."
  (interactive "P")
  (let ((link (t3-code-thread--markdown-link-at-point)))
    (cond
     ((and link (string-match-p "\\`https?://" link)) (browse-url link) t)
     ((thing-at-point 'url t) (browse-url (thing-at-point 'url t)) t)
     ((when-let* ((target (t3-code-thread--file-target-at-point)))
        (t3-code-thread--visit-file target invert)
        t)))))

(defun t3-code-thread-ret (&optional invert)
  "Act on point: inspect attention, load history, answer, visit, or fold.
With prefix argument INVERT, invert where files are visited."
  (interactive "P")
  (let ((row (t3-code-thread--row-at-point))
        (item (t3-code-thread--item-by-id (t3-code-thread--item-at-point))))
    (cond
     ((get-text-property (point) 't3-code-attention) (t3-code-thread-inspect))
     ((plist-get row :history) (t3-code-thread-load-older))
     ((and item (equal (plist-get item :type) "user_input_request")
           (t3-code-thread--pending-p item))
      (t3-code-thread-answer-input))
     ((t3-code-thread-visit-at-point invert))
     (t (t3-code-thread-toggle-details)))))

(defun t3-code-thread--substitute-file (command file)
  "Place quoted FILE into shell COMMAND at an isolated `*', or append it."
  (let ((quoted (shell-quote-argument file)))
    (if (string-match-p "\\(?:\\`\\|[ \t]\\)\\*\\(?:[ \t]\\|\\'\\)" command)
        (replace-regexp-in-string "\\(\\`\\|[ \t]\\)\\*\\([ \t]\\|\\'\\)"
                                  (concat "\\1" (replace-regexp-in-string "\\\\" "\\\\\\\\" quoted)
                                          "\\2")
                                  command t)
      (concat command " " quoted))))

(defun t3-code-thread-shell-command-on-file (command)
  "Run shell COMMAND on the file at point, like `dired-do-shell-command'.
An isolated `*' marks where the file goes; otherwise it is appended.  A
trailing `&' runs the command asynchronously."
  (interactive
   (let ((target (or (t3-code-thread--file-target-at-point)
                     (user-error "No file reference at point"))))
     (list (read-shell-command (format "! on %s: " (file-name-nondirectory (car target)))))))
  (let* ((file (car (t3-code-thread--file-target-at-point)))
         (async (string-match-p "[ \t]&[ \t]*\\'" command))
         (command (t3-code-thread--substitute-file
                   (string-trim-right (replace-regexp-in-string "[ \t]&[ \t]*\\'" "" command))
                   file))
         (default-directory (or (t3-code-thread--local-directory
                                 (t3-code-thread--worktree-path))
                                default-directory)))
    (if async (async-shell-command command) (shell-command command))))

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
  (let ((id (plist-get (t3-code-thread--thread) :activeRunId)))
    (unless id (user-error "No active run"))
    (kill-new id)
    (message "Copied T3 run ID")))

(defun t3-code-thread-copy-last-message ()
  "Copy the text of the last assistant message."
  (interactive)
  (let ((item (seq-find (lambda (item) (equal (plist-get item :type) "assistant_message"))
                        (reverse (t3-code-thread--items)))))
    (unless item (user-error "No assistant message loaded"))
    (kill-new (plist-get item :text))
    (message "Copied last assistant message")))

;;;; Menu

(defun t3-code-thread--lifecycle-p ()
  "Whether the bridge supports thread lifecycle commands."
  (t3-code-capability-p t3-code-thread--environment :threadLifecycle))

(transient-define-prefix t3-code-thread-actions ()
  "Actions for the current T3 thread."
  [["Threads"
    ("n" "new" t3-code-new-thread)
    ("r" "resume" t3-code-resume)
    ("j" "switch" t3-code-switch-thread)
    ("b" "ledger" t3-code-ledger)
    ("/" "search" t3-code-search-threads :if t3-code-thread--lifecycle-p)
    ("f" "fork at point" t3-code-thread-fork :if t3-code-thread--lifecycle-p)]
   ["Message"
    ("m" "compose" t3-code-thread-compose-auto)
    ("e" "steer" t3-code-thread-compose-steer)
    ("q" "queue" t3-code-thread-compose-queue)
    ("Q" "queued messages" t3-code-thread-manage-queue :if t3-code-thread--lifecycle-p)
    ("R" "restart with message" t3-code-thread-compose-restart)
    ("k" "abort" t3-code-thread-interrupt)]
   ["Requests"
    ("y" "approve once" t3-code-thread-approve)
    ("Y" "approve session" t3-code-thread-approve-session)
    ("x" "decline" t3-code-thread-decline)
    ("X" "cancel turn" t3-code-thread-cancel-approval)
    ("a" "answer input" t3-code-thread-answer-input :if t3-code-thread--lifecycle-p)
    ("A" "dismiss input" t3-code-thread-dismiss-input :if t3-code-thread--lifecycle-p)]
   ["Modes"
    ("M" "provider / model" t3-code-thread-select-model
     :if t3-code-thread--model-selection-supported-p)
    ("t" t3-code-thread-select-effort
     :description (lambda ()
                    (format "reasoning effort: %s"
                            (or (t3-code-thread--effort-default
                                 (plist-get (t3-code-thread--thread) :modelSelection))
                                "default")))
     :if t3-code-thread--model-selection-supported-p)
    ("p" "plan / default" t3-code-thread-set-interaction-mode)
    ("P" "runtime mode" t3-code-thread-set-runtime-mode)]]
  [["Lifecycle"
    ("N" "rename" t3-code-thread-rename :if t3-code-thread--lifecycle-p)
    ("+" "pin / unpin" t3-code-thread-toggle-pin :if t3-code-thread--lifecycle-p)
    ("s" "settle" t3-code-thread-settle)
    ("u" "reactivate" t3-code-thread-reactivate)
    ("z" "snooze" t3-code-thread-snooze)
    ("Z" "wake" t3-code-thread-unsnooze)]
   ["Archive"
    ("v" "archive" t3-code-thread-archive :if t3-code-thread--lifecycle-p)
    ("V" "unarchive" t3-code-thread-unarchive :if t3-code-thread--lifecycle-p)
    ("D" "delete" t3-code-thread-delete :if t3-code-thread--lifecycle-p)]
   ["Open / copy"
    ("o" "older history" t3-code-thread-load-older :if t3-code-thread--lifecycle-p)
    ("w" "worktree" t3-code-thread-open-worktree)
    ("d" "VC status / diff" t3-code-thread-open-status)
    ("l" "copy last message" t3-code-thread-copy-last-message)
    ("c" "copy thread ID" t3-code-thread-copy-thread-id)
    ("C" "copy run ID" t3-code-thread-copy-run-id)]])

;;;; Chat buffer

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
  "RET" #'t3-code-thread-ret
  "<backtab>" #'t3-code-thread-cycle-view
  "n" #'t3-code-thread-next-turn
  "p" #'t3-code-thread-previous-turn
  "f" #'t3-code-thread-fork
  "o" #'t3-code-thread-load-older
  "!" #'t3-code-thread-shell-command-on-file
  "G" #'t3-code-thread-jump-to-latest
  "M-w" #'t3-code-thread-copy
  "?" #'t3-code-thread-actions
  "." #'t3-code-thread-actions
  "m" #'t3-code-thread-compose-auto
  "i" #'t3-code-thread-compose-auto
  "a" #'t3-code-thread-compose-end
  "g" #'t3-code-thread-reconnect
  "q" #'t3-code-thread-quit
  "C-c C-p" #'t3-code-thread-actions
  "C-c C-k" #'t3-code-thread-interrupt
  "C-c C-m" #'t3-code-thread-select-model
  "C-c C-t" #'t3-code-thread-cycle-effort
  "C-c C-y" #'t3-code-thread-copy-last-message
  "C-c C-n" #'t3-code-new-thread
  "C-c C-r" #'t3-code-resume
  "C-c C-b" #'t3-code-ledger
  "C-c C-j" #'t3-code-switch-thread)

(define-derived-mode t3-code-thread-mode special-mode "T3-Chat"
  "Major mode for a live T3 conversation.
\\{t3-code-thread-mode-map}"
  (setq header-line-format
        '(:eval (t3-code-thread--escape-header (t3-code-thread--header-line))))
  (t3-code-thread-render-setup)
  (visual-line-mode 1)
  (add-hook 't3-code-thread-refresh-hook #'t3-code-thread--update-phase nil t)
  (add-hook 'kill-buffer-hook #'t3-code-thread--cleanup nil t))

(defun t3-code-thread--retain-displaced (old new)
  "Keep items of live window OLD that are missing from NEW as loaded history.
Once older pages are loaded, items sliding out of the live window would
otherwise fall between those pages and the window, where paging backwards
cannot reach them."
  (when t3-code-thread--older-items
    (let ((known (make-hash-table :test #'equal)))
      (dolist (item (append new t3-code-thread--older-items))
        (puthash (plist-get item :id) t known))
      (setq t3-code-thread--older-items
            (append t3-code-thread--older-items
                    (seq-remove (lambda (item) (gethash (plist-get item :id) known))
                                old))))))

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
      (t3-code-thread--retain-displaced (plist-get t3-code-thread--payload :items)
                                        (plist-get payload :items))
      (setq t3-code-thread--payload payload))
     ((equal kind "synchronized")
      (setq t3-code-thread--stream-error nil)))
    (when (or (not (equal t3-code-thread--payload t3-code-thread--rendered-payload))
              (not (equal t3-code-thread--stream-error t3-code-thread--rendered-error)))
      (t3-code-thread--refresh))))

(defvar t3-code-thread--display-action
  '((display-buffer-reuse-window display-buffer-same-window))
  "Chats replace the selected window, like visiting a file.
`display-buffer-alist' entries still take precedence.")

(defun t3-code-thread--select-chat-window ()
  "From a T3 input window, select the chat window above it.
A new chat then replaces the old chat, and its input the old input,
instead of the chat landing in the small input window."
  ;; Check the window, not the current buffer: callbacks run elsewhere.
  (when (t3-code-compose--input-window-p (selected-window))
    (when-let* ((above (window-in-direction 'above))
                ((with-current-buffer (window-buffer above)
                   (derived-mode-p 't3-code-thread-mode))))
      (select-window above))))

(defun t3-code-thread-show (chat &optional focus-input)
  "Display CHAT with its input window; select the input when FOCUS-INPUT."
  (t3-code-thread--select-chat-window)
  (pop-to-buffer chat t3-code-thread--display-action)
  (with-current-buffer chat
    (when (or focus-input (not (eq t3-code-input-window-display 'hidden)))
      (let ((input (save-selected-window (t3-code-thread-compose))))
        (when focus-input (select-window (get-buffer-window input)))))))

(defun t3-code-thread-open (environment thread)
  "Open normalized THREAD from ENVIRONMENT in a live chat with its input."
  (let* ((thread-id (plist-get thread :id))
         (name (t3-code-thread-buffer-name environment thread-id))
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
    (t3-code-thread--select-chat-window)
    (pop-to-buffer buffer t3-code-thread--display-action)
    (with-current-buffer buffer
      (when fresh (t3-code-thread-jump-to-latest))
      ;; Opening always records a visit: unread tracking needs a watermark.
      (t3-code-shell-mark-visited environment thread-id t)
      (unless (eq t3-code-input-window-display 'hidden)
        (t3-code-thread-compose)))
    buffer))

(provide 't3-code-thread)
;;; t3-code-thread.el ends here
