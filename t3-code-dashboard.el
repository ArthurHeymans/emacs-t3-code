;;; t3-code-dashboard.el --- T3 fleet dashboard  -*- lexical-binding: t; -*-

;;; Commentary:

;; A deliberately small read-only dashboard.  Its data is a normalized shell
;; projection supplied by t3e, never raw T3 server state.

;;; Code:

(require 'seq)
(require 'tabulated-list)
(require 't3-code-core)
(require 't3-code-thread)

(defcustom t3-code-dashboard-collapse-settled t
  "Whether new dashboards initially hide settled threads."
  :type 'boolean
  :group 't3-code)

(defface t3-code-dashboard-running-face
  '((((class color) (background dark)) :foreground "#79a8ff" :weight bold)
    (((class color) (background light)) :foreground "#1d4ed8" :weight bold)
    (t :inherit font-lock-function-name-face))
  "Face for actively working threads."
  :group 't3-code)

(defface t3-code-dashboard-approval-face
  '((((class color) (background dark)) :foreground "#d0bc00" :weight bold)
    (((class color) (background light)) :foreground "#a16207" :weight bold)
    (t :inherit warning))
  "Face for threads awaiting approval."
  :group 't3-code)

(defface t3-code-dashboard-failed-face
  '((((class color) (background dark)) :foreground "#ff8059" :weight bold)
    (((class color) (background light)) :foreground "#b91c1c" :weight bold)
    (t :inherit error))
  "Face for failed threads."
  :group 't3-code)

(defface t3-code-dashboard-settled-face
  '((t :inherit shadow))
  "Face for settled lifecycle labels."
  :group 't3-code)

(defface t3-code-dashboard-project-face
  '((((class color) (background dark)) :foreground "#b6a0ff" :weight bold)
    (((class color) (background light)) :foreground "#6d28d9" :weight bold)
    (t :inherit font-lock-keyword-face))
  "Face for project names."
  :group 't3-code)

(defface t3-code-dashboard-provider-face
  '((((class color) (background dark)) :foreground "#f78fe7")
    (((class color) (background light)) :foreground "#a21caf")
    (t :inherit font-lock-type-face))
  "Face for provider names."
  :group 't3-code)

(defface t3-code-dashboard-model-face
  '((((class color) (background dark)) :foreground "#00d3d0")
    (((class color) (background light)) :foreground "#0f766e")
    (t :inherit font-lock-constant-face))
  "Face for model names."
  :group 't3-code)

(defface t3-code-dashboard-worktree-face
  '((((class color) (background dark)) :foreground "#ef8b50")
    (((class color) (background light)) :foreground "#c2410c")
    (t :inherit font-lock-variable-name-face))
  "Face for worktree names."
  :group 't3-code)

(defvar-local t3-code-dashboard--environment nil)
(defvar-local t3-code-dashboard--subscription nil)
(defvar-local t3-code-dashboard--projects nil)
(defvar-local t3-code-dashboard--shell-truncated nil)
(defvar-local t3-code-dashboard--settled-collapsed nil)
(defvar-local t3-code-dashboard--expanded-agents nil)

(defconst t3-code-dashboard--settled-heading-id 't3-code-settled-heading)
(defconst t3-code-dashboard--truncated-heading-id 't3-code-truncated-heading)
(defconst t3-code-dashboard--agents-heading-tag 't3-code-agents-heading)

(defun t3-code-dashboard--header-line ()
  "Render the current dashboard connection state."
  (when t3-code-dashboard--environment
    (let* ((state (t3-code-environment-state t3-code-dashboard--environment))
           (state-face (pcase state
                         ('ready 'success)
                         ((or 'connecting 'resuming 'snapshot) 't3-code-dashboard-running-face)
                         ((or 'repairing 'retrying 'authenticating) 'warning)
                         (_ 'error))))
      (concat " "
              (propertize "T3" 'face 'bold)
              " "
              (propertize (t3-code-environment-id t3-code-dashboard--environment)
                          'face 't3-code-dashboard-project-face)
              "  "
              (propertize (t3-code-environment-endpoint t3-code-dashboard--environment)
                          'face 'font-lock-comment-face)
              "  [" (propertize (symbol-name state) 'face state-face) "]"
              "  settled:"
              (propertize (if t3-code-dashboard--settled-collapsed "hidden" "shown")
                          'face 't3-code-dashboard-settled-face)
              (if t3-code-dashboard--shell-truncated
                  (propertize "  ⚠ truncated" 'face 'warning)
                "")
              (when (eq state 'disconnected)
                (when-let* ((fatal (t3-code-environment-fatal-error
                                    t3-code-dashboard--environment))
                            (reason (plist-get fatal :message)))
                  (concat "  · "
                          (propertize
                           (truncate-string-to-width
                            (replace-regexp-in-string "[[:cntrl:]]+" " " reason)
                            160 nil nil "…")
                           'face 'error))))))))

(defun t3-code-dashboard--status (status)
  "Return a colored status-dot display for thread STATUS."
  (pcase status
    ("running" (propertize "● WORK" 'face 't3-code-dashboard-running-face))
    ("waiting-approval" (propertize "● APPR" 'face 't3-code-dashboard-approval-face))
    ("failed" (propertize "● FAIL" 'face 't3-code-dashboard-failed-face))
    ("idle" (propertize "○ idle" 'face 'shadow))
    (_ (propertize (or status "?") 'face 'shadow))))

(defun t3-code-dashboard--thread-entry (project thread &optional depth)
  "Create a tabulated entry for THREAD in PROJECT at nesting DEPTH."
  (let* ((thread-id (plist-get thread :id))
         (depth (or depth 0))
         (prefix (if (> depth 0) (concat (make-string depth ?\s) "↳ ") "")))
    (list thread-id
          (vector
           (if (> depth 0)
               ""
             (propertize (or (plist-get project :name) "")
                         'face 't3-code-dashboard-project-face))
           (t3-code-dashboard--status (plist-get thread :status))
           (if (eq (plist-get thread :settled) t)
               (propertize "settled" 'face 't3-code-dashboard-settled-face)
             (propertize "active" 'face 'success))
           (propertize (concat prefix (or (plist-get thread :title) thread-id ""))
                       'face (if (> depth 0) 'font-lock-doc-face 'default))
           (propertize (or (plist-get thread :provider) "")
                       'face 't3-code-dashboard-provider-face)
           (propertize (or (plist-get thread :model) "")
                       'face 't3-code-dashboard-model-face)
           (propertize (or (plist-get thread :worktree) "root")
                       'face 't3-code-dashboard-worktree-face)
           (concat
            (propertize (format "+%s" (or (plist-get thread :additions) 0))
                        'face 'success)
            "/"
            (propertize (format "-%s" (or (plist-get thread :deletions) 0))
                        'face 'error))))))

(defun t3-code-dashboard--agents-heading-id (parent-id)
  "Return the stable agents-menu ID below PARENT-ID."
  (list t3-code-dashboard--agents-heading-tag parent-id))

(defun t3-code-dashboard--agents-heading-id-p (id)
  "Return non-nil when ID identifies an agents menu."
  (and (consp id) (eq (car id) t3-code-dashboard--agents-heading-tag)))

(defun t3-code-dashboard--agents-heading-entry (parent-id count depth)
  "Create the agents menu below PARENT-ID for COUNT children at DEPTH."
  (let ((expanded (gethash parent-id t3-code-dashboard--expanded-agents)))
    (list (t3-code-dashboard--agents-heading-id parent-id)
          (vector
           "" "" ""
           (propertize
            (format "%s%s Agents (%d)"
                    (make-string (1+ depth) ?\s)
                    (if expanded "▾" "▸") count)
            'face 'font-lock-comment-face
            'help-echo "RET: collapse/expand delegated agents")
           "" "" "" ""))))

(defun t3-code-dashboard--render-thread-tree (project thread children depth)
  "Render THREAD and its CHILDREN map under PROJECT at DEPTH."
  (let* ((thread-id (plist-get thread :id))
         (direct (gethash thread-id children))
         (expanded (gethash thread-id t3-code-dashboard--expanded-agents)))
    (append
     (list (t3-code-dashboard--thread-entry project thread depth))
     (when direct
       (cons
        (t3-code-dashboard--agents-heading-entry thread-id (length direct) depth)
        (when expanded
          (mapcan (lambda (child)
                    (t3-code-dashboard--render-thread-tree
                     project child children (1+ depth)))
                  direct)))))))

(defun t3-code-dashboard--truncated-heading-entry ()
  "Create a warning row for a bounded shell projection."
  (list t3-code-dashboard--truncated-heading-id
        (vector
         (propertize "⚠ View truncated by bridge limits" 'face 'warning
                     'help-echo "Some projects or threads are omitted")
         "" "" "" "" "" "" "")))

(defun t3-code-dashboard--settled-heading-entry (count)
  "Create the collapsible settled-section heading for COUNT rows."
  (list t3-code-dashboard--settled-heading-id
        (vector
         (propertize (format "%s Settled (%d)"
                             (if t3-code-dashboard--settled-collapsed "▸" "▾")
                             count)
                     'face 'font-lock-keyword-face
                     'help-echo "RET or s: collapse/expand settled threads")
         "" "" "" "" "" "" "")))

(defun t3-code-dashboard--entries ()
  "Build root threads, hidden agent trees, and the bottom settled section."
  (let ((children (make-hash-table :test #'equal)) active settled)
    (dolist (project t3-code-dashboard--projects)
      (dolist (thread (plist-get project :threads))
        (when (and (equal (plist-get thread :relationshipToParent) "subagent")
                   (plist-get thread :parentThreadId))
          (let ((parent-id (plist-get thread :parentThreadId)))
            (puthash parent-id
                     (append (gethash parent-id children) (list thread))
                     children)))))
    (dolist (project t3-code-dashboard--projects)
      (dolist (thread (plist-get project :threads))
        (unless (equal (plist-get thread :relationshipToParent) "subagent")
          (push (cons project thread)
                (if (eq (plist-get thread :settled) t) settled active)))))
    (setq active (nreverse active)
          settled (nreverse settled))
    (append
     (when t3-code-dashboard--shell-truncated
       (list (t3-code-dashboard--truncated-heading-entry)))
     (mapcan (lambda (row)
               (t3-code-dashboard--render-thread-tree
                (car row) (cdr row) children 0))
             active)
     (when settled
       (cons (t3-code-dashboard--settled-heading-entry (length settled))
             (unless t3-code-dashboard--settled-collapsed
               (mapcan (lambda (row)
                         (t3-code-dashboard--render-thread-tree
                          (car row) (cdr row) children 0))
                       settled)))))))

(defun t3-code-dashboard--goto-id (id)
  "Move point to the row whose stable domain ID equals ID."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (< (point) (point-max)))
      (when (equal (get-text-property (point) 'tabulated-list-id) id)
        (setq found t))
      (unless found (forward-line 1)))
    found))

(defun t3-code-dashboard--refresh ()
  "Refresh the table while preserving row and window position."
  (when (derived-mode-p 't3-code-dashboard-mode)
    (let ((row-id (tabulated-list-get-id))
          (column (current-column))
          (window-starts
           (mapcar (lambda (window) (cons window (window-start window)))
                   (get-buffer-window-list (current-buffer) nil t))))
      (setq tabulated-list-entries (t3-code-dashboard--entries))
      (tabulated-list-print t)
      (when (and row-id (t3-code-dashboard--goto-id row-id))
        (beginning-of-line)
        (move-to-column column))
      (dolist (entry window-starts)
        (when (window-live-p (car entry))
          (set-window-start (car entry) (cdr entry) t))))))

(defun t3-code-dashboard--on-shell-message (message)
  "Apply normalized shell MESSAGE to the current dashboard.
The M1 bridge projection uses replacement shell payloads for both snapshots and
coalesced events, keeping raw T3 reducer schemas out of Elisp."
  (when (member (plist-get message :kind) '("snapshot" "event"))
    (let ((payload (plist-get message :payload)))
      (when (plist-member payload :projects)
        (setq t3-code-dashboard--projects (plist-get payload :projects)
              t3-code-dashboard--shell-truncated
              (eq (plist-get payload :truncated) t))
        (t3-code-dashboard--refresh)))))

(defun t3-code-dashboard--find-thread (thread-id)
  "Return the normalized thread summary for THREAD-ID."
  (seq-some (lambda (project)
              (seq-find (lambda (thread)
                          (equal (plist-get thread :id) thread-id))
                        (plist-get project :threads)))
            t3-code-dashboard--projects))

(defun t3-code-dashboard--toggle-id (id)
  "Toggle menu ID and return non-nil when ID names a menu."
  (cond
   ((eq id t3-code-dashboard--settled-heading-id)
    (t3-code-dashboard-toggle-settled)
    t)
   ((t3-code-dashboard--agents-heading-id-p id)
    (t3-code-dashboard-toggle-agents (cadr id))
    t)))

(defun t3-code-dashboard-open-thread ()
  "Open the thread or toggle the section at point."
  (interactive)
  (if-let* ((id (tabulated-list-get-id)))
      (unless (t3-code-dashboard--toggle-id id)
        (if (eq id t3-code-dashboard--truncated-heading-id)
            (user-error "Some projects or threads are omitted by bridge limits")
          (if-let* ((thread (t3-code-dashboard--find-thread id)))
              (t3-code-thread-open t3-code-dashboard--environment thread)
            (user-error "Thread is no longer present: %s" id))))
    (user-error "No T3 thread at point")))

(defun t3-code-dashboard-toggle-at-point ()
  "Collapse or expand the menu at point."
  (interactive)
  (unless (t3-code-dashboard--toggle-id (tabulated-list-get-id))
    (user-error "No collapsible T3 menu at point")))

(defun t3-code-dashboard-toggle-agents (parent-id)
  "Toggle delegated agents below PARENT-ID."
  (if (gethash parent-id t3-code-dashboard--expanded-agents)
      (remhash parent-id t3-code-dashboard--expanded-agents)
    (puthash parent-id t t3-code-dashboard--expanded-agents))
  (t3-code-dashboard--refresh))

(defun t3-code-dashboard-toggle-settled ()
  "Toggle visibility of settled threads in the current dashboard."
  (interactive)
  (setq t3-code-dashboard--settled-collapsed
        (not t3-code-dashboard--settled-collapsed))
  (t3-code-dashboard--refresh)
  (force-mode-line-update t))

(declare-function t3-code--reconnect "t3-code" (environment))

(defun t3-code-dashboard-reconnect ()
  "Restart the current dashboard's environment bridge."
  (interactive)
  (require 't3-code)
  (t3-code--reconnect t3-code-dashboard--environment))

(defun t3-code-dashboard-quit ()
  "Kill the dashboard, releasing only its shell subscription."
  (interactive)
  (kill-buffer (current-buffer)))

(defun t3-code-dashboard--cleanup ()
  "Release this buffer's environment subscription."
  (when (and t3-code-dashboard--environment t3-code-dashboard--subscription)
    (t3-code-unsubscribe t3-code-dashboard--environment
                         t3-code-dashboard--subscription)
    (setq t3-code-dashboard--subscription nil)))

(defvar-keymap t3-code-dashboard-mode-map
  :parent tabulated-list-mode-map
  "RET" #'t3-code-dashboard-open-thread
  "TAB" #'t3-code-dashboard-toggle-at-point
  "<tab>" #'t3-code-dashboard-toggle-at-point
  "g" #'t3-code-dashboard-reconnect
  "s" #'t3-code-dashboard-toggle-settled
  "q" #'t3-code-dashboard-quit)

;; Keep reloads useful while iterating in a live dashboard buffer: `defvar-keymap'
;; preserves an existing map, so explicitly install newly added bindings too.
(keymap-set t3-code-dashboard-mode-map "TAB" #'t3-code-dashboard-toggle-at-point)
(keymap-set t3-code-dashboard-mode-map "<tab>" #'t3-code-dashboard-toggle-at-point)

(defun t3-code-dashboard--configure-columns ()
  "Configure dashboard columns and initialize their header."
  (setq tabulated-list-format
        [("Project" 36 nil)
         ("State" 8 nil)
         ("Life" 8 nil)
         ("Thread" 36 nil)
         ("Provider" 10 nil)
         ("Model" 18 nil)
         ("Worktree" 16 nil)
         ("Diff" 12 nil)]
        tabulated-list-padding 2
        tabulated-list-sort-key nil)
  (tabulated-list-init-header))

(define-derived-mode t3-code-dashboard-mode tabulated-list-mode "T3-Dashboard"
  "Major mode for the read-only T3 fleet dashboard."
  (setq header-line-format '(:eval (t3-code-dashboard--header-line)))
  (t3-code-dashboard--configure-columns)
  (setq t3-code-dashboard--settled-collapsed
        t3-code-dashboard-collapse-settled
        t3-code-dashboard--expanded-agents (make-hash-table :test #'equal))
  (add-hook 'kill-buffer-hook #'t3-code-dashboard--cleanup nil t))

(defun t3-code-dashboard (environment)
  "Display a dashboard for shared ENVIRONMENT."
  (let ((buffer (get-buffer-create (format "*t3:%s*" (t3-code-environment-id environment)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 't3-code-dashboard-mode)
        (t3-code-dashboard-mode))
      (setq t3-code-dashboard--environment environment)
      (unless t3-code-dashboard--subscription
        (setq t3-code-dashboard--subscription
              (t3-code-subscribe environment "shell" nil
                                 (lambda (message)
                                   (when (buffer-live-p buffer)
                                     (with-current-buffer buffer
                                       (t3-code-dashboard--on-shell-message message)))))))
      (t3-code-dashboard--refresh))
    (pop-to-buffer buffer)
    buffer))

(provide 't3-code-dashboard)
;;; t3-code-dashboard.el ends here
