;;; t3-code.el --- Emacs client for T3 Code  -*- lexical-binding: t; -*-

;; Version: 0.2.0
;; Package-Requires: ((emacs "29.1") (transient "0.3.0"))
;; Keywords: tools, processes

;;; Commentary:

;; Entry points for an Emacs client of T3 Code.  `t3-code' opens the current
;; project's conversation (a chat buffer above an input buffer); the ledger,
;; switcher, resume and search commands reach every other thread.

;;; Code:

(require 'url-parse)
(require 't3-code-core)
(require 't3-code-shell)
(require 't3-code-thread)
(require 't3-code-dashboard)

(defcustom t3-code-default-endpoint "http://127.0.0.1:3773"
  "Default T3 server HTTP endpoint."
  :type 'string
  :group 't3-code)

(defcustom t3-code-token 'ask
  "Token to give the bridge when connecting.
`ask' prompts securely each time a bridge is started; a string uses that
value.  Nil inherits credentials from Emacs's process environment instead.
A configured string can be persisted by Customize, so treat it as a secret."
  :type '(choice (const :tag "Ask on connect" ask)
                 (string :tag "Token (stored in Emacs configuration)")
                 (const :tag "Use process environment" nil))
  :group 't3-code)

(defcustom t3-code-token-type 'pairing
  "Type of `t3-code-token': one-time pairing or reusable bearer token."
  :type '(choice (const :tag "Pairing" pairing)
                 (const :tag "Bearer" bearer))
  :group 't3-code)

(defcustom t3-code-new-thread-workspace 'ask
  "Where \\[t3-code-new-thread] starts a thread.
`ask' prompts, `root' uses the project checkout and `worktree' creates a
new worktree from the current branch."
  :type '(choice (const ask) (const root) (const worktree))
  :group 't3-code)

(defun t3-code--configured-credential ()
  "Resolve the configured token for a new bridge process."
  (when t3-code-token
    (let ((token (if (eq t3-code-token 'ask)
                     (read-passwd (if (eq t3-code-token-type 'bearer)
                                      "T3 bearer token: " "T3 pairing token: "))
                   t3-code-token)))
      (unless (and (stringp token) (not (string-empty-p token)))
        (user-error "T3 token cannot be empty"))
      (cons t3-code-token-type token))))

(defun t3-code--reconnect (environment)
  "Restart ENVIRONMENT with the configured authentication setting."
  (t3-code-restart environment (t3-code--configured-credential)))

(defun t3-code--environment (&optional endpoint)
  "Return the connected environment for ENDPOINT, connecting if needed."
  (let* ((endpoint (or endpoint t3-code-default-endpoint))
         (environment (t3-code-get-environment endpoint endpoint default-directory)))
    (unless (process-live-p (t3-code-environment-process environment))
      (t3-code--reconnect environment))
    (t3-code-shell-ensure environment)))

(defun t3-code--current-environment ()
  "Return the environment of the current T3 buffer, or the default one."
  (or t3-code-thread--environment
      t3-code-compose--environment
      (bound-and-true-p t3-code-dashboard--environment)
      (t3-code--environment)))

(defun t3-code--when-shell (environment callback &optional timeout)
  "Call CALLBACK once ENVIRONMENT's shell has loaded, or nil after TIMEOUT."
  (if (t3-code-environment-shell environment)
      (funcall callback t)
    (let ((deadline (+ (float-time) (or timeout 15))) timer)
      (setq timer
            (run-at-time
             0.1 0.1
             (lambda ()
               (let ((ready (t3-code-environment-shell environment)))
                 (when (or ready (> (float-time) deadline)
                           (eq (t3-code-environment-state environment) 'disconnected))
                   (cancel-timer timer)
                   (funcall callback ready)))))))))

(defun t3-code--rank (thread)
  "Sort key for THREAD: working, then unseen, then most recently updated."
  (list (if (t3-code-shell-working-p thread) 0 1)
        (if (eq (plist-get thread :unread) t) 0 1)
        (plist-get thread :updatedAt)))

(defun t3-code--rank< (a b)
  "Whether thread A ranks before thread B."
  (let ((ka (t3-code--rank a)) (kb (t3-code--rank b)))
    (or (< (nth 0 ka) (nth 0 kb))
        (and (= (nth 0 ka) (nth 0 kb))
             (or (< (nth 1 ka) (nth 1 kb))
                 (and (= (nth 1 ka) (nth 1 kb))
                      (string> (or (nth 2 ka) "") (or (nth 2 kb) ""))))))))

(defun t3-code--project-thread (project directory)
  "Return the best active top-level thread of PROJECT for DIRECTORY."
  (let* ((threads (seq-filter (lambda (thread)
                                (and (not (eq (plist-get thread :settled) t))
                                     (not (equal (plist-get thread :relationshipToParent)
                                                 "subagent"))))
                              (plist-get project :threads)))
         ;; A thread whose worktree contains DIRECTORY is the one being worked on.
         (local (seq-filter (lambda (thread)
                              (and (not (equal (plist-get thread :path)
                                               (plist-get project :root)))
                                   (t3-code-shell--path-within-p directory
                                                                 (plist-get thread :path))))
                            threads)))
    (car (sort (copy-sequence (or local threads)) #'t3-code--rank<))))

(defun t3-code--session-buffers ()
  "Return the chat and input buffers related to the current buffer."
  (cond ((derived-mode-p 't3-code-thread-mode)
         (list (current-buffer) t3-code-thread--composer))
        ((and (derived-mode-p 't3-code-compose-mode) t3-code-compose--origin-buffer)
         (list t3-code-compose--origin-buffer (current-buffer)))))

;;;###autoload
(defun t3-code (&optional ledger)
  "Open the T3 conversation for the current project.
From a T3 buffer, restore missing chat/input windows and focus the input.
Elsewhere, open the most relevant active thread of the project containing
`default-directory', or offer to start one.  Outside known projects, or
with prefix argument LEDGER, open the ledger of all threads instead.

Ask for a token on connect unless `t3-code-token' specifies one or uses
the environment.  Reopening an already-connected environment does not
prompt."
  (interactive "P")
  (let ((session (t3-code--session-buffers)))
    (if (and session (buffer-live-p (car session)) (not ledger))
        (t3-code-thread-show (car session) t)
      (let ((environment (t3-code--environment))
            (directory default-directory))
        (if ledger
            (t3-code-dashboard environment)
          (t3-code--when-shell
           environment
           (lambda (ready)
             (let* ((project (and ready (t3-code-shell-project-for-directory
                                         environment directory)))
                    (thread (and project (t3-code--project-thread project directory))))
               (cond
                (thread (t3-code-thread-open environment thread))
                ((and project (y-or-n-p (format "No active thread in %s; start one? "
                                                (plist-get project :name))))
                 (let ((default-directory directory)) (t3-code-new-thread)))
                (t (t3-code-dashboard environment)))))))))))

;;;###autoload
(defun t3-code-ledger (&optional environment)
  "Open the ledger of all threads in ENVIRONMENT."
  (interactive)
  (t3-code-dashboard (or environment (t3-code--current-environment))))

(defun t3-code-toggle ()
  "Hide the current T3 chat and input windows, or show them again."
  (interactive)
  (if-let* ((session (t3-code--session-buffers)))
      (with-current-buffer (car session) (t3-code-thread-quit))
    (t3-code)))

(defun t3-code--thread-candidates (environment entries)
  "Return completion candidates for (PROJECT . THREAD) ENTRIES in ENVIRONMENT."
  (mapcar (lambda (entry)
            (let ((thread (cdr entry)))
              (cons (propertize (format "%s  %s" (or (plist-get thread :title) "")
                                        (propertize (substring (plist-get thread :id) 0
                                                               (min 6 (length (plist-get thread :id))))
                                                    'face 'shadow))
                                't3-code-group (plist-get (car entry) :name)
                                't3-code-thread thread
                                't3-code-environment environment)
                    thread)))
          entries))

(defun t3-code--read-thread (prompt candidates)
  "Read a thread with PROMPT from CANDIDATES, grouped by project."
  (unless candidates (user-error "No matching threads"))
  (let* ((annotate (lambda (candidate)
                     (when-let* ((thread (get-text-property 0 't3-code-thread candidate)))
                       (concat "  "
                               (t3-code-dashboard--status (plist-get thread :status))
                               (if (eq (plist-get thread :unread) t) " •" "")
                               (propertize (format "  %s/%s" (or (plist-get thread :provider) "")
                                                   (or (plist-get thread :model) ""))
                                           'face 'shadow)))))
         (group (lambda (candidate transform)
                  (if transform candidate
                    (or (get-text-property 0 't3-code-group candidate) "Threads"))))
         (table (lambda (string predicate action)
                  (if (eq action 'metadata)
                      `(metadata (category . t3-code-thread)
                                 (display-sort-function . identity)
                                 (annotation-function . ,annotate)
                                 (group-function . ,group))
                    (complete-with-action action candidates string predicate)))))
    (cdr (assoc (completing-read prompt table nil t) candidates))))

(defun t3-code-switch-thread ()
  "Switch to any active thread, grouped by project and ranked by activity."
  (interactive)
  (let* ((environment (t3-code--current-environment))
         (entries (seq-filter (lambda (entry) (not (eq (plist-get (cdr entry) :settled) t)))
                              (t3-code-shell-entries environment)))
         (entries (sort entries (lambda (a b) (t3-code--rank< (cdr a) (cdr b))))))
    (t3-code-thread-open environment
                         (t3-code--read-thread "Switch to thread: "
                                               (t3-code--thread-candidates environment entries)))))

(defun t3-code--current-project (environment)
  "Return the project of the current buffer in ENVIRONMENT, if known."
  (or (when-let* ((thread (and (derived-mode-p 't3-code-thread-mode)
                               (t3-code-thread--thread)))
                  (project-id (plist-get thread :projectId)))
        (t3-code-shell-find-project environment project-id))
      (when-let* ((thread-id (or t3-code-thread--thread-id
                                 (and (buffer-live-p t3-code-compose--origin-buffer)
                                      (buffer-local-value 't3-code-thread--thread-id
                                                          t3-code-compose--origin-buffer)))))
        (car (t3-code-shell-find-thread environment thread-id)))
      (t3-code-shell-project-for-directory environment default-directory)))

(defun t3-code-resume ()
  "Resume a settled or archived thread of the current project.
Outside a known project, offer settled threads of every project."
  (interactive)
  (let* ((environment (t3-code--current-environment))
         (project (t3-code--current-project environment))
         (in-scope (lambda (project-id)
                     (or (null project) (equal project-id (plist-get project :id)))))
         (settled (seq-filter (lambda (entry)
                                (and (eq (plist-get (cdr entry) :settled) t)
                                     (funcall in-scope (plist-get (car entry) :id))))
                              (t3-code-shell-entries environment)))
         (archived (when (t3-code-capability-p environment :threadLifecycle)
                     (seq-keep
                      (lambda (thread)
                        (when (funcall in-scope (plist-get thread :projectId))
                          (cons (list :id (plist-get thread :projectId)
                                      :name (concat (plist-get thread :projectName)
                                                    " · archived"))
                                (append thread (list :status "archived")))))
                      (plist-get (t3-code-request-sync environment "threads.archived" nil 10)
                                 :threads))))
         (entries (sort (append settled archived)
                        (lambda (a b) (string> (or (plist-get (cdr a) :updatedAt) "")
                                               (or (plist-get (cdr b) :updatedAt) "")))))
         (thread (t3-code--read-thread "Resume thread: "
                                       (t3-code--thread-candidates environment entries))))
    (if (not (equal (plist-get thread :status) "archived"))
        (t3-code-thread-open environment thread)
      ;; Archived threads are outside the shell; restore before opening so
      ;; lifecycle commands such as pin and settle apply again.
      (t3-code-shell-dispatch
       environment (list :type "thread.unarchive" :threadId (plist-get thread :id))
       (lambda (_result error)
         (if error
             (message "T3 unarchive failed: %s" (or (plist-get error :message) error))
           (t3-code-thread-open environment
                                (plist-put (copy-sequence thread) :status "idle"))))))))

(defun t3-code-search-threads (query)
  "Search thread titles and messages for QUERY and open a match."
  (interactive (list (read-string "Search T3 threads: ")))
  (let* ((environment (t3-code--current-environment))
         (_ (unless (t3-code-capability-p environment :threadLifecycle)
              (user-error "Connected bridge does not support thread search")))
         (_ (when (< (length (string-trim query)) 2)
              (user-error "Search for at least two characters")))
         (matches (plist-get (t3-code-request-sync environment "thread.search"
                                                   (list :query (string-trim query)) 10)
                             :matches))
         (candidates
          (mapcar (lambda (match)
                    (let* ((entry (t3-code-shell-find-thread environment
                                                             (plist-get match :threadId)))
                           (thread (or (cdr entry) (list :id (plist-get match :threadId)))))
                      (cons (propertize (format "%s — %s"
                                                (or (plist-get thread :title)
                                                    (plist-get match :threadId))
                                                (plist-get match :snippet))
                                        't3-code-group
                                        (or (plist-get (car entry) :name) "Other")
                                        't3-code-thread thread)
                            thread)))
                  matches)))
    (t3-code-thread-open environment (t3-code--read-thread "Open match: " candidates))))

(defun t3-code--read-workspace (project branch)
  "Read a workspace strategy for a thread in PROJECT based on BRANCH."
  (pcase (if (eq t3-code-new-thread-workspace 'ask)
             (car (read-multiple-choice
                   "Workspace" '((?r "root" "the project checkout")
                                 (?w "worktree" "a new worktree")
                                 (?e "existing" "an existing worktree"))))
           (if (eq t3-code-new-thread-workspace 'worktree) ?w ?r))
    (?w (let ((base (read-string (format "Base ref (default %s): " (or branch "HEAD"))
                                 nil nil (or branch "HEAD")))
              (name (string-trim (read-string "New branch (empty for automatic): "))))
          (append (list :type "worktree" :baseRef base)
                  (unless (string-empty-p name) (list :branch name)))))
    (?e (let* ((paths (delete-dups
                       (seq-keep (lambda (thread)
                                   (let ((path (plist-get thread :path)))
                                     (unless (equal path (plist-get project :root)) path)))
                                 (plist-get project :threads))))
               (path (completing-read "Worktree: " paths)))
          (list :type "existing_worktree" :worktreePath (expand-file-name path))))
    (_ (list :type "root"))))

(defun t3-code-new-thread (&optional choose)
  "Start a new thread in the current project.
The input buffer opens first; \\[t3-code-compose-send] starts the thread
with its first message.  With prefix argument CHOOSE, pick the project and
model instead of inheriting them from the current thread."
  (interactive "P")
  (let* ((environment (t3-code--current-environment))
         (_ (unless (t3-code-capability-p environment :threadLifecycle)
              (user-error "Connected bridge cannot start threads")))
         (source (cond ((derived-mode-p 't3-code-thread-mode) (t3-code-thread--thread))
                       ((buffer-live-p t3-code-compose--origin-buffer)
                        (with-current-buffer t3-code-compose--origin-buffer
                          (t3-code-thread--thread)))))
         (projects (plist-get (t3-code-environment-shell environment) :projects))
         (project (or (and (not choose) (t3-code--current-project environment))
                      (let ((names (mapcar (lambda (project)
                                             (cons (plist-get project :name) project))
                                           projects)))
                        (unless names (user-error "No T3 projects are known yet"))
                        (cdr (assoc (completing-read "Project: " names nil t) names)))))
         (siblings (sort (copy-sequence (plist-get project :threads)) #'t3-code--rank<))
         (template (or source (car siblings)))
         (workspace (t3-code--read-workspace project (plist-get template :branch)))
         (launch (list :projectId (plist-get project :id)
                       :projectName (plist-get project :name)
                       :directory (or (plist-get workspace :worktreePath)
                                      (plist-get project :root))
                       :runtimeMode (or (plist-get source :runtimeMode)
                                        t3-code-thread-default-runtime-mode)
                       :interactionMode (or (plist-get source :interactionMode)
                                            t3-code-thread-default-interaction-mode)
                       :workspaceStrategy workspace))
         (selection (and (not choose)
                         (or (plist-get source :modelSelection)
                             (when (plist-get template :provider)
                               (list :instanceId (plist-get template :provider)
                                     :model (plist-get template :model)))))))
    (if selection
        (t3-code-compose-open-launch environment (append launch (list :modelSelection selection)))
      (t3-code-thread--with-catalog
       environment
       (lambda (catalog)
         (t3-code-compose-open-launch
          environment
          (append launch (list :modelSelection
                               (t3-code-thread--read-model-selection catalog nil)))))))))

(defun t3-code-connect-prompt (&optional bearer)
  "Prompt for a server URL and token, then open its ledger.
By default TOKEN is a one-time pairing token.  With prefix argument
BEARER, treat it as an existing bearer access token instead.  The token is
passed only to the newly started bridge process; it is not saved in Emacs's
environment or the T3 environment state.  Reconnecting requires a new token."
  (interactive "P")
  (let* ((endpoint (string-trim (read-string "T3 server URL: "
                                             t3-code-default-endpoint)))
         (url (url-generic-parse-url endpoint)))
    (unless (and (member (url-type url) '("http" "https"))
                 (url-host url)
                 (not (url-user url)) (not (url-password url)))
      (user-error "Enter an HTTP(S) URL without embedded credentials"))
    (let* ((token (read-passwd (if bearer "T3 bearer token: "
                                 "T3 pairing token (C-u for bearer): ")))
           (environment (t3-code-get-environment endpoint endpoint default-directory)))
      (when (string-empty-p token) (user-error "Token cannot be empty"))
      ;; Restart also re-subscribes existing ledger and thread views.
      (t3-code-restart environment (cons (if bearer 'bearer 'pairing) token))
      (t3-code-shell-ensure environment)
      (t3-code-dashboard environment))))

(defun t3-code-demo ()
  "Open the ledger against the repository's deterministic fake bridge."
  (interactive)
  (let* ((root (file-name-directory (or load-file-name
                                        (locate-library "t3-code")
                                        buffer-file-name)))
         (t3-code-token nil)
         (t3-code-bridge-command
          (list "node" (expand-file-name "bridge/fake-t3e.mjs" root))))
    (t3-code-dashboard (t3-code--environment "fake://demo"))))

(provide 't3-code)
;;; t3-code.el ends here
