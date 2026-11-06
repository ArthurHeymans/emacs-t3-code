;;; t3-code-fleet.el --- Shared remote environments -*- lexical-binding: t; -*-

;;; Commentary:
;; The primary bridge owns remote credentials.  Emacs receives only metadata
;; and normalized protocol records, and reuses the ordinary per-server views.

;;; Code:

(require 't3-code-dashboard)

(declare-function t3-code-dashboard--display "t3-code-dashboard" (buffer))
(declare-function t3-code-dashboard--configure-columns "t3-code-dashboard" (&optional width))

(defvar-local t3-code-fleet--root nil)
(defvar-local t3-code-fleet--agent-menus nil)
(defvar-local t3-code-fleet--section-states nil
  "Per-environment Working, Settled and Snoozed fold state.")

(defun t3-code-fleet-root (environment)
  "Return the catalog connection owning ENVIRONMENT."
  (or (t3-code-environment-parent environment) environment))

(defun t3-code-fleet-members (environment)
  "Return the primary and discovered connections owning ENVIRONMENT."
  (let ((root (t3-code-fleet-root environment)))
    (cons root (t3-code-environment-environments root))))

(defun t3-code-fleet-loaded-p (root)
  "Whether ROOT has received its first authoritative environment catalog."
  (gethash 'catalog-loaded (t3-code-environment-cache root)))

(defun t3-code-fleet--receive (root message)
  "Reconcile ROOT's authoritative redacted environment MESSAGE."
  (when (and (member (plist-get message :kind) '("snapshot" "event"))
             (plist-member (plist-get message :payload) :environments))
    (let ((records (plist-get (plist-get message :payload) :environments)) members)
      (unless (and (listp records) (<= (length records) 100))
        (error "Invalid or oversized environment catalog"))
      (dolist (record records)
        (let* ((id (plist-get record :id))
               (endpoint (plist-get record :endpoint))
               (url (and (stringp endpoint) (url-generic-parse-url endpoint))))
          (when (and (stringp id) (not (string-empty-p id)) url
                     (member (url-type url) '("http" "https"))
                     (url-host url) (not (url-user url)) (not (url-password url))
                     (not (eq (plist-get record :enabled) :false)))
            (let* ((client-id (concat (t3-code-environment-id root) "::" id))
                   (environment (t3-code-get-environment
                                 client-id endpoint (t3-code-environment-directory root))))
              (unless (equal endpoint (t3-code-environment-endpoint environment))
                (t3-code-disconnect environment)
                (setf (t3-code-environment-endpoint environment) endpoint
                      (t3-code-environment-remote-prefix environment) nil
                      (t3-code-environment-shell environment) nil))
              (setf (t3-code-environment-parent environment) root
                    (t3-code-environment-catalog-id environment) id
                    (t3-code-environment-label environment)
                    (or (plist-get record :label) endpoint))
              (unless (memq environment members) (push environment members))
              (when (and (eq (t3-code-environment-state root) 'ready)
                         (not (process-live-p (t3-code-environment-process environment))))
                (if (t3-code-environment-shell-reference environment)
                    (t3-code-restart environment)
                  (t3-code-connect environment))
                (t3-code-shell-ensure environment))))))
      (dolist (old (t3-code-environment-environments root))
        (unless (memq old members)
          (t3-code-disconnect old)
          (setf (t3-code-environment-shell old) nil)))
      (setf (t3-code-environment-environments root) (nreverse members))
      (puthash 'catalog-loaded t (t3-code-environment-cache root))
      (t3-code-fleet--refresh-views root))))

(defun t3-code-fleet--state-changed (environment)
  "Maintain discovered connections and redraw views when ENVIRONMENT changes."
  (unless (t3-code-environment-parent environment)
    (pcase (t3-code-environment-state environment)
      ('ready
       (when (and (plist-get (t3-code-environment-capabilities environment) :serverEnvironmentId)
                  (not (gethash 'label-requested (t3-code-environment-cache environment))))
         (puthash 'label-requested t (t3-code-environment-cache environment))
         (t3-code-request
          environment "server.getConfig" nil
          (lambda (result error)
            (unless error
              (setf (t3-code-environment-label environment)
                    (plist-get (plist-get result :environment) :label))
              (t3-code-fleet--refresh-views environment)))))
       (when (and (t3-code-capability-p environment :environments)
                  (not (t3-code-environment-environments-reference environment)))
         (setf (t3-code-environment-environments-reference environment)
               (t3-code-subscribe environment "environments" nil
                                  (lambda (message)
                                    (t3-code-fleet--receive environment message))))))
      ('disconnected
       (remhash 'catalog-loaded (t3-code-environment-cache environment))
       (dolist (child (t3-code-environment-environments environment))
         (setf (t3-code-environment-process child) nil
               (t3-code-environment-outbound-queue child) nil)
         (t3-code--fail-pending child '(:code "catalog-disconnected"
                                       :message "Catalog bridge disconnected"))
         (t3-code--set-state child 'disconnected)))))
  (t3-code-fleet--refresh-views (t3-code-fleet-root environment)))

(defun t3-code-fleet--refresh-views (root &rest _)
  "Refresh live fleet views of ROOT without scanning thread buffers."
  (when-let* ((buffer (get-buffer (format "*t3 fleet: %s*" (t3-code-environment-id root)))))
    (with-current-buffer buffer
      (when (derived-mode-p 't3-code-fleet-mode)
        (t3-code-fleet--refresh)))))

(defun t3-code-fleet--shell-changed (environment &rest _)
  "Redraw the fleet when ENVIRONMENT's shell changes."
  (t3-code-fleet--refresh-views (t3-code-fleet-root environment)))

(add-hook 't3-code-environment-state-hook #'t3-code-fleet--state-changed)
(add-hook 't3-code-shell-update-functions #'t3-code-fleet--shell-changed)

(defun t3-code-fleet--menu-state (environment)
  "Return ENVIRONMENT's independent agent-fold state for this fleet."
  (or (gethash (t3-code-environment-id environment) t3-code-fleet--agent-menus)
      (puthash (t3-code-environment-id environment) (make-hash-table :test #'equal)
               t3-code-fleet--agent-menus)))

(defun t3-code-fleet--section-state (environment)
  "Return ENVIRONMENT's independent section-fold plist for this fleet."
  (unless t3-code-fleet--section-states
    (setq t3-code-fleet--section-states (make-hash-table :test #'equal)))
  (or (gethash (t3-code-environment-id environment) t3-code-fleet--section-states)
      (puthash (t3-code-environment-id environment)
               (list :working t3-code-dashboard-collapse-working
                     :settled t3-code-dashboard-collapse-settled
                     :snoozed t)
               t3-code-fleet--section-states)))

(defun t3-code-fleet--store-section-state (environment state)
  "Record ENVIRONMENT's section-fold STATE for this fleet."
  (unless t3-code-fleet--section-states
    (setq t3-code-fleet--section-states (make-hash-table :test #'equal)))
  (puthash (t3-code-environment-id environment) state t3-code-fleet--section-states))

(defun t3-code-fleet--call-in-environment (environment function)
  "Call FUNCTION with ENVIRONMENT's shell and folds as the dashboard context.
Section folds are bound per environment, so toggling one environment's Working,
Settled or Snoozed heading never changes another environment's view.  Any fold
FUNCTION changes are written back to ENVIRONMENT alone."
  (let* ((shell (t3-code-environment-shell environment))
         (state (t3-code-fleet--section-state environment))
         (t3-code-dashboard--environment environment)
         (t3-code-dashboard--projects (plist-get shell :projects))
         (t3-code-dashboard--expanded-agents (t3-code-fleet--menu-state environment))
         (t3-code-dashboard--shell-truncated (eq (plist-get shell :truncated) t))
         (t3-code-dashboard--omitted-settled-count (plist-get shell :omittedSettledCount))
         (t3-code-dashboard--omitted-other-count (plist-get shell :omittedOtherCount))
         (t3-code-dashboard--omitted-project-count (plist-get shell :omittedProjectCount))
         (t3-code-dashboard--working-collapsed (plist-get state :working))
         (t3-code-dashboard--settled-collapsed (plist-get state :settled))
         (t3-code-dashboard--snoozed-collapsed (plist-get state :snoozed)))
    (unwind-protect
        (funcall function)
      (t3-code-fleet--store-section-state
       environment
       (list :working t3-code-dashboard--working-collapsed
             :settled t3-code-dashboard--settled-collapsed
             :snoozed t3-code-dashboard--snoozed-collapsed)))))

(defun t3-code-fleet--entries ()
  "Render environment headings and reuse each ordinary dashboard's rows."
  (mapcan
   (lambda (environment)
     (let* ((id (t3-code-environment-id environment))
            (state (t3-code-environment-state environment))
            (fatal (t3-code-environment-fatal-error environment)))
       (cons
        (list (list id :environment)
              (vector
               (propertize (t3-code-environment-display-name environment) 'face 'bold)
               (symbol-name state) ""
               (propertize (t3-code-environment-endpoint environment) 'face 'shadow)
               "" "" ""
               (propertize (or (plist-get fatal :message) "") 'face 'error)
               ""))
        (t3-code-fleet--call-in-environment
         environment
         (lambda ()
           (mapcar (lambda (row) (cons (list id (car row)) (cdr row)))
                   (t3-code-dashboard--entries)))))))
   (t3-code-fleet-members t3-code-fleet--root)))

(defun t3-code-fleet--refresh ()
  "Refresh the fleet while preserving the environment-qualified row at point."
  (let ((id (tabulated-list-get-id)) (column (current-column)))
    (t3-code-dashboard--configure-columns)
    (setq tabulated-list-entries (t3-code-fleet--entries))
    (tabulated-list-print t)
    (when (and id (t3-code-dashboard--goto-id id))
      (move-to-column column))))

(defun t3-code-fleet-environment-at-point ()
  "Return the row's environment, or the primary connection on blank space."
  (or (when-let* ((id (car-safe (tabulated-list-get-id))))
        (seq-find (lambda (environment) (equal id (t3-code-environment-id environment)))
                  (t3-code-fleet-members t3-code-fleet--root)))
      t3-code-fleet--root))

(defun t3-code-fleet--action (command)
  "Invoke dashboard COMMAND against the row's actual environment."
  (let ((buffer (current-buffer))
        (id (cadr (tabulated-list-get-id)))
        (environment (t3-code-fleet-environment-at-point)))
    (when (eq id :environment)
      (if (eq command #'t3-code-dashboard-new-thread)
          (setq id nil)
        (user-error "Choose a thread below this environment")))
    (t3-code-fleet--call-in-environment
     environment
     (lambda ()
       (let ((t3-code-dashboard--row-id-function (lambda () id)))
         (cl-letf (((symbol-function 't3-code-dashboard--refresh) #'ignore))
           (call-interactively command)))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer (t3-code-fleet--refresh)))))

(defun t3-code-fleet-open ()
  "Open the environment-qualified thread at point."
  (interactive)
  (t3-code-fleet--action #'t3-code-dashboard-open-thread))

(defun t3-code-fleet-toggle ()
  "Toggle a folded section in its owning environment."
  (interactive)
  (t3-code-fleet--action #'t3-code-dashboard-toggle-at-point))

(defun t3-code-fleet--toggle-section (command)
  "Toggle a section of the environment at point with dashboard COMMAND."
  (t3-code-fleet--call-in-environment
   (t3-code-fleet-environment-at-point)
   (lambda ()
     (cl-letf (((symbol-function 't3-code-dashboard--refresh) #'ignore))
       (funcall command))))
  (t3-code-fleet--refresh))

(defun t3-code-fleet-toggle-working ()
  "Toggle the Working section of the environment at point."
  (interactive)
  (t3-code-fleet--toggle-section #'t3-code-dashboard-toggle-working))

(defun t3-code-fleet-toggle-settled ()
  "Toggle the Settled section of the environment at point."
  (interactive)
  (t3-code-fleet--toggle-section #'t3-code-dashboard-toggle-settled))

(defun t3-code-fleet-toggle-snoozed ()
  "Toggle the Snoozed section of the environment at point."
  (interactive)
  (t3-code-fleet--toggle-section #'t3-code-dashboard-toggle-snoozed))

(defun t3-code-fleet-pin ()
  "Toggle pinning on the thread at point."
  (interactive)
  (t3-code-fleet--action #'t3-code-dashboard-toggle-pin))

(defun t3-code-fleet-archive ()
  "Archive the thread at point in its owning environment."
  (interactive)
  (t3-code-fleet--action #'t3-code-dashboard-archive))

(defun t3-code-fleet-new-thread (&optional arg)
  "Start a thread in the row's remote project; ARG selects other settings."
  (interactive "P")
  (let ((current-prefix-arg arg))
    (t3-code-fleet--action #'t3-code-dashboard-new-thread)))

(declare-function t3-code--reconnect "t3-code" (environment))

(defun t3-code-fleet-reconnect (&optional restart)
  "Refresh every fleet connection, or RESTART the catalog bridge with C-u."
  (interactive "P")
  (if restart
      (t3-code--reconnect t3-code-fleet--root)
    (dolist (environment (t3-code-fleet-members t3-code-fleet--root))
      (when (eq (t3-code-environment-state environment) 'ready)
        (when-let* ((reference (t3-code-environment-shell-reference environment)))
          (t3-code-refresh-subscription environment reference))))
    (when-let* ((reference (t3-code-environment-environments-reference t3-code-fleet--root)))
      (t3-code-refresh-subscription t3-code-fleet--root reference))))

(defvar-keymap t3-code-fleet-mode-map
  :parent t3-code-dashboard-mode-map
  "RET" #'t3-code-fleet-open
  "TAB" #'t3-code-fleet-toggle
  "<tab>" #'t3-code-fleet-toggle
  "+" #'t3-code-fleet-pin
  "v" #'t3-code-fleet-archive
  "N" #'t3-code-fleet-new-thread
  "C-c C-n" #'t3-code-fleet-new-thread
  "g" #'t3-code-fleet-reconnect
  "w" #'t3-code-fleet-toggle-working
  "s" #'t3-code-fleet-toggle-settled
  "z" #'t3-code-fleet-toggle-snoozed)

;; Override the inherited dashboard command when reloading an existing map too.
(keymap-set t3-code-fleet-mode-map "w" #'t3-code-fleet-toggle-working)

(defun t3-code-fleet--header-line ()
  "Describe the fleet and explain unavailable shared remote discovery."
  (concat " T3 environments · " (t3-code-environment-display-name t3-code-fleet--root)
          "  · RET open · TAB fold · I details · C-h m keys"
          (unless (t3-code-capability-p t3-code-fleet--root :environments)
            (propertize " · Shared remotes unavailable (local admin connection required)"
                        'face 'warning))))

(define-derived-mode t3-code-fleet-mode t3-code-dashboard-mode "T3-Fleet"
  "Ledger of threads grouped by their owning local or remote environment."
  (setq t3-code-fleet--agent-menus (make-hash-table :test #'equal)
        t3-code-fleet--section-states (make-hash-table :test #'equal))
  ;; Fleet listeners belong to the shared store, not to this view.
  (remove-hook 'kill-buffer-hook #'t3-code-dashboard--cleanup t)
  (setq header-line-format '(:eval (t3-code-fleet--header-line))))

(defun t3-code-fleet (environment)
  "Display local and shared remote threads belonging to ENVIRONMENT."
  (let* ((root (t3-code-fleet-root environment))
         (buffer (get-buffer-create (format "*t3 fleet: %s*" (t3-code-environment-id root)))))
    (with-current-buffer buffer
      (unless (derived-mode-p 't3-code-fleet-mode) (t3-code-fleet-mode))
      (setq t3-code-fleet--root root
            t3-code-dashboard--environment root)
      (t3-code-fleet--state-changed root)
      (t3-code-fleet--refresh))
    (t3-code-dashboard--display buffer)
    buffer))

(provide 't3-code-fleet)
;;; t3-code-fleet.el ends here
