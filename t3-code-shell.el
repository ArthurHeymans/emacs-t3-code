;;; t3-code-shell.el --- Shared shell state and notifications  -*- lexical-binding: t; -*-

;;; Commentary:

;; One shell subscription per environment keeps the normalized project/thread
;; list available to every view: the ledger, the thread switcher, project-first
;; entry, the global activity indicator, and state-change notifications.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 't3-code-core)

(defcustom t3-code-notify 'message
  "How to announce threads that finish, fail or need attention.
`message' uses the echo area, `desktop' also sends a desktop notification,
and nil stays silent.  Threads visible in a window are never announced."
  :type '(choice (const :tag "Echo area" message)
                 (const :tag "Echo area and desktop" desktop)
                 (const :tag "Silent" nil))
  :group 't3-code)

(defvar t3-code-shell-update-functions nil
  "Abnormal hook run with (ENVIRONMENT OLD NEW) shell payloads after a change.")

(defun t3-code-thread-buffer-name (environment thread-id)
  "Return the transcript buffer name for THREAD-ID in ENVIRONMENT."
  (format "*t3:%s/%s*" (t3-code-environment-id environment) thread-id))

(defun t3-code-shell--receive (environment message)
  "Store a shell MESSAGE for ENVIRONMENT and notify observers."
  (when (member (plist-get message :kind) '("snapshot" "event"))
    (let ((payload (plist-get message :payload))
          (old (t3-code-environment-shell environment)))
      (when (plist-member payload :projects)
        (setf (t3-code-environment-shell environment) payload)
        (unless (equal old payload)
          (run-hook-with-args 't3-code-shell-update-functions environment old payload))))))

(defun t3-code-shell-ensure (environment)
  "Keep ENVIRONMENT's shell projection subscribed and cached."
  (unless (t3-code-environment-shell-reference environment)
    (setf (t3-code-environment-shell-reference environment)
          (t3-code-subscribe environment "shell" nil
                             (lambda (message)
                               (t3-code-shell--receive environment message)))))
  environment)

(defun t3-code-shell-entries (environment)
  "Return (PROJECT . THREAD) pairs from ENVIRONMENT's cached shell."
  (mapcan (lambda (project)
            (mapcar (lambda (thread) (cons project thread))
                    (plist-get project :threads)))
          (plist-get (t3-code-environment-shell environment) :projects)))

(defun t3-code-shell-find-thread (environment thread-id)
  "Return (PROJECT . THREAD) for THREAD-ID in ENVIRONMENT, if known."
  (seq-find (lambda (entry) (equal (plist-get (cdr entry) :id) thread-id))
            (t3-code-shell-entries environment)))

(defun t3-code-shell-find-project (environment project-id)
  "Return the normalized project PROJECT-ID in ENVIRONMENT."
  (seq-find (lambda (project) (equal (plist-get project :id) project-id))
            (plist-get (t3-code-environment-shell environment) :projects)))

(defun t3-code-shell--path-within-p (directory root)
  "Whether DIRECTORY is ROOT or below it."
  (and (stringp root) (not (string-empty-p root))
       (string-prefix-p (file-name-as-directory (expand-file-name root))
                        (file-name-as-directory (expand-file-name directory)))))

(defun t3-code-shell-project-for-directory (environment directory)
  "Return the project of ENVIRONMENT containing DIRECTORY.
A thread worktree inside or outside the project root also identifies its
project.  The most specific match wins."
  (car (car (seq-sort-by
             (lambda (match) (- (length (cdr match))))
             #'<
             (mapcan (lambda (project)
                       (seq-keep (lambda (root)
                                   (when (t3-code-shell--path-within-p directory root)
                                     (cons project root)))
                                 (cons (plist-get project :root)
                                       (mapcar (lambda (thread) (plist-get thread :path))
                                               (plist-get project :threads)))))
                     (plist-get (t3-code-environment-shell environment) :projects))))))

(defun t3-code-shell-working-p (thread)
  "Whether THREAD is running or awaiting a response."
  (member (plist-get thread :status) '("running" "waiting-approval")))

(defun t3-code-shell-counts (environment)
  "Return (WORKING . UNSEEN) thread counts for ENVIRONMENT."
  (let ((threads (mapcar #'cdr (t3-code-shell-entries environment))))
    (cons (seq-count #'t3-code-shell-working-p threads)
          (seq-count (lambda (thread) (eq (plist-get thread :unread) t)) threads))))

(declare-function t3-code-ledger "t3-code" (&optional environment))
(declare-function notifications-notify "notifications" (&rest params))

(defvar-keymap t3-code-shell-indicator-map
  "<header-line> <mouse-1>" #'t3-code-ledger
  "<mode-line> <mouse-1>" #'t3-code-ledger)

(defun t3-code-shell-indicator (environment)
  "Return a clickable summary of working and newly finished ENVIRONMENT threads."
  (when (and environment (t3-code-environment-shell environment))
    (pcase-let ((`(,working . ,unseen) (t3-code-shell-counts environment)))
      (when (or (> working 0) (> unseen 0))
        (propertize (concat (if (> working 0) (format "⚙%d" working) "")
                            (if (and (> working 0) (> unseen 0)) " " "")
                            (if (> unseen 0) (format "✓%d" unseen) ""))
                    'face 'shadow
                    'mouse-face 'highlight
                    'help-echo "mouse-1: open the T3 ledger"
                    'local-map t3-code-shell-indicator-map)))))

(defun t3-code-shell--visible-p (environment thread-id)
  "Whether THREAD-ID of ENVIRONMENT is displayed in a visible window."
  (when-let* ((buffer (get-buffer (t3-code-thread-buffer-name environment thread-id))))
    (get-buffer-window buffer 'visible)))

(defun t3-code-shell--transition (old-thread new-thread)
  "Describe a noteworthy change from OLD-THREAD to NEW-THREAD, or nil."
  (let ((before (plist-get old-thread :status))
        (after (plist-get new-thread :status)))
    (unless (equal before after)
      (pcase after
        ("waiting-approval" "needs your attention")
        ("failed" "failed")
        ("idle" (when (equal before "running") "finished"))))))

(defun t3-code-shell-notify (text)
  "Announce TEXT according to `t3-code-notify'."
  (when t3-code-notify
    (message "T3: %s" text)
    (when (and (eq t3-code-notify 'desktop)
               (require 'notifications nil t))
      (ignore-errors (notifications-notify :title "T3 Code" :body text)))))

(defun t3-code-shell--announce-changes (environment old new)
  "Announce threads in NEW whose status changed noteworthily since OLD.
ENVIRONMENT identifies visible thread buffers.  The first snapshot is not
announced."
  (when old
    (let ((before (make-hash-table :test #'equal)))
      (dolist (project (plist-get old :projects))
        (dolist (thread (plist-get project :threads))
          (puthash (plist-get thread :id) thread before)))
      (dolist (project (plist-get new :projects))
        (dolist (thread (plist-get project :threads))
          (when-let* ((previous (gethash (plist-get thread :id) before))
                      (change (t3-code-shell--transition previous thread))
                      ((not (t3-code-shell--visible-p environment (plist-get thread :id)))))
            (t3-code-shell-notify
             (format "“%s” %s" (or (plist-get thread :title) (plist-get thread :id))
                     change))))))))

(add-hook 't3-code-shell-update-functions #'t3-code-shell--announce-changes)

(defun t3-code-shell-dispatch (environment command &optional callback)
  "Dispatch allowlisted thread COMMAND plist in ENVIRONMENT.
COMMAND gains a fresh :commandId.  CALLBACK receives (RESULT ERROR)."
  (unless (t3-code-capability-p environment :threadLifecycle)
    (user-error "Connected bridge does not support thread commands"))
  (t3-code-request environment "thread.command"
                   (list :command (append command
                                          (list :commandId (t3-code-new-id "emacs-"))))
                   (or callback
                       (lambda (_result error)
                         (when error
                           (message "T3 %s failed: %s" (plist-get command :type)
                                    (or (plist-get error :message) error)))))))

(defun t3-code-shell-mark-visited (environment thread-id)
  "Record that THREAD-ID in ENVIRONMENT has been seen, when it is unread."
  (when-let* ((entry (t3-code-shell-find-thread environment thread-id))
              ((eq (plist-get (cdr entry) :unread) t))
              ((t3-code-capability-p environment :threadLifecycle)))
    ;; Clear locally so repeated refreshes do not resend before the shell updates.
    (plist-put (cdr entry) :unread :false)
    (t3-code-shell-dispatch
     environment
     (list :type "thread.visit" :threadId thread-id
           :visitedAt (format-time-string "%Y-%m-%dT%H:%M:%S.%3NZ" nil t)))))

(defun t3-code-shell--visit-visible (environment _old new)
  "Mark ENVIRONMENT threads visible in a window as seen after shell update NEW."
  (dolist (project (plist-get new :projects))
    (dolist (thread (plist-get project :threads))
      (when (and (eq (plist-get thread :unread) t)
                 (t3-code-shell--visible-p environment (plist-get thread :id)))
        (t3-code-shell-mark-visited environment (plist-get thread :id))))))

(add-hook 't3-code-shell-update-functions #'t3-code-shell--visit-visible)

(provide 't3-code-shell)
;;; t3-code-shell.el ends here
