;;; t3-code-entry-test.el --- Project-first entry tests -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code)

(ert-deftest t3-code-test-project-root-finds-nested-jj-without-backend ()
  (let ((root (make-temp-file "t3-entry" t)))
    (unwind-protect
        (let ((nested (expand-file-name "src/majutsu/" root)))
          (make-directory (expand-file-name ".git" root))
          (make-directory (expand-file-name ".jj" nested) t)
          (make-directory (expand-file-name "test" nested))
          (cl-letf (((symbol-function 'project-current)
                     (lambda (&rest _) (cons 'transient root))))
            (should (equal (t3-code--project-root (expand-file-name "test/" nested))
                           nested))))
      (delete-directory root t))))

(ert-deftest t3-code-test-project-root-expands-abbreviations-before-ranking ()
  (cl-letf (((symbol-function 'project-current)
             (lambda (&rest _) (cons 'transient (expand-file-name "~/"))))
            ((symbol-function 'locate-dominating-file)
             (lambda (_dir marker) (and (equal marker ".jj") "~/src/jj/"))))
    (should (equal (t3-code--project-root (expand-file-name "~/src/jj/"))
                   (expand-file-name "~/src/jj/")))))

(ert-deftest t3-code-test-project-boundary-keeps-registered-worktrees ()
  (let ((environment (t3-code-environment-create :id "entry")))
    (setf (t3-code-environment-shell environment)
          '(:projects ((:id "home" :root "/home/me" :threads nil)
                       (:id "app" :root "/home/me/src/app"
                        :threads ((:id "wt" :path "/home/me/worktrees/app"))))))
    (should-not (t3-code-shell-project-for-directory
                 environment "/home/me/src/majutsu/test/" "/home/me/src/majutsu/"))
    (should (equal (plist-get (t3-code-shell-project-for-directory
                              environment "/home/me/worktrees/app/src/"
                              "/home/me/worktrees/app/") :id)
                   "app"))
    (should (equal (plist-get (t3-code-shell-project-for-directory
                              environment "/home/me/src/app/lib/"
                              "/home/me/src/app/") :id)
                   "app"))))

;; Keep the shell real; only replace connection/UI boundaries.  Delaying the
;; callback simulates entering from one buffer and receiving data in another.
(defmacro t3-code-test--with-entry (&rest body)
  (declare (indent 0) (debug t))
  `(let ((environment (t3-code-environment-create
                       :id "entry" :state 'ready
                       :capabilities '(:projectRegistration t :threadLifecycle t)))
         shell-callback request-callback request prompt opened ledger)
     (setf (t3-code-environment-shell environment)
           (copy-tree '(:projects ((:id "home" :name "Home" :root "/home/me"
                                   :threads ((:id "home-thread" :path "/home/me")))))))
     (cl-letf (((symbol-function 't3-code--environment) (lambda (&rest _) environment))
               ((symbol-function 't3-code--project-root)
                (lambda (_) "/home/me/src/majutsu/"))
               ((symbol-function 't3-code--when-shell)
                (lambda (_env callback &rest _) (setq shell-callback callback)))
               ((symbol-function 'y-or-n-p)
                (lambda (question) (setq prompt question) t))
               ((symbol-function 't3-code-request)
                (lambda (_env operation input callback)
                  (setq request (list operation input) request-callback callback)))
               ((symbol-function 't3-code-fleet) (lambda (_env) (setq ledger t)))
               ((symbol-function 't3-code-thread-open)
                (lambda (_env thread) (setq opened thread)))
               ((symbol-function 't3-code-new-thread)
                (lambda (_choose project env)
                  (should (eq env environment))
                  (setq opened (list :project project :directory default-directory)))))
       (with-temp-buffer
         (setq default-directory "/home/me/src/majutsu/test/")
         (t3-code))
       ,@body)))

(ert-deftest t3-code-test-entry-registers-invoking-project-before-shell-update ()
  (t3-code-test--with-entry
    (with-temp-buffer
      (setq default-directory "/home/me/")
      (funcall shell-callback t))
    (should (equal prompt "Register /home/me/src/majutsu/ as a T3 project? "))
    (should (equal (car request) "project.create"))
    (let* ((input (cadr request))
           (project (list :id (plist-get input :projectId) :name "majutsu"
                          :root "/home/me/src/majutsu" :threads nil)))
      (should (stringp (plist-get input :commandId)))
      (should (stringp (plist-get input :projectId)))
      (should (equal (plist-get input :workspaceRoot) "/home/me/src/majutsu"))
      (should (equal (plist-get input :title) "majutsu"))
      (should-not opened)
      (funcall request-callback (list :project project) nil)
      (should (equal (plist-get opened :project) project))
      (should (equal (plist-get opened :directory) "/home/me/src/majutsu/"))
      (should-not ledger))))

(ert-deftest t3-code-test-entry-declining-registration-opens-ledger ()
  (t3-code-test--with-entry
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_) nil)))
      (funcall shell-callback t))
    (should ledger)
    (should-not request)
    (should-not opened)))

(ert-deftest t3-code-test-entry-registration-error-does-not-open-parent-thread ()
  (t3-code-test--with-entry
    (funcall shell-callback t)
    (funcall request-callback nil '(:message "Permission denied"))
    (should ledger)
    (should-not opened)))

(ert-deftest t3-code-test-entry-old-bridge-and-shell-timeout-open-ledger ()
  (t3-code-test--with-entry
    (setf (t3-code-environment-capabilities environment) '(:threadLifecycle t))
    (funcall shell-callback t)
    (should ledger)
    (should-not prompt)
    (should-not request)
    (setq ledger nil)
    (funcall shell-callback nil)
    (should ledger)
    (should-not opened)))

(ert-deftest t3-code-test-entry-registered-project-opens-its-thread ()
  (t3-code-test--with-entry
    (push '(:id "majutsu" :name "majutsu" :root "/home/me/src/majutsu"
            :threads ((:id "majutsu-thread" :path "/home/me/src/majutsu")))
          (plist-get (t3-code-environment-shell environment) :projects))
    (funcall shell-callback t)
    (should (equal (plist-get opened :id) "majutsu-thread"))
    (should-not request)
    (should-not prompt)
    (should-not ledger)))

(ert-deftest t3-code-test-entry-registration-maps-remote-root-to-server-path ()
  (t3-code-test--with-entry
    (setf (t3-code-environment-endpoint environment) "http://host:3773"
          (t3-code-environment-remote-prefix environment) "/ssh:me@host:")
    (cl-letf (((symbol-function 't3-code--project-root)
               (lambda (_) "/ssh:me@host:/srv/majutsu/")))
      (with-temp-buffer
        (setq default-directory "/ssh:me@host:/srv/majutsu/test/")
        (t3-code)))
    (funcall shell-callback t)
    (should (equal (plist-get (cadr request) :workspaceRoot) "/srv/majutsu"))
    (funcall request-callback
             '(:project (:id "remote" :name "majutsu" :root "/srv/majutsu")) nil)
    (should (equal (plist-get opened :directory) "/ssh:me@host:/srv/majutsu/"))))

(ert-deftest t3-code-test-entry-registers-on-server-selected-after-discovery ()
  (t3-code-test--with-entry
    (let ((remote (t3-code-environment-create
                   :id "remote" :endpoint "http://host:3773" :parent environment
                   :remote-prefix "/ssh:me@host:"
                   :state 'ready :shell '(:projects nil)
                   :capabilities '(:projectRegistration t :threadLifecycle t)))
          directory-callback)
      (cl-letf (((symbol-function 't3-code--project-root)
                 (lambda (_) "/ssh:me@host:/srv/majutsu/"))
                ((symbol-function 't3-code--when-directory)
                 (lambda (env directory callback)
                   (should (eq env environment))
                   (should (equal directory "/ssh:me@host:/srv/majutsu/test/"))
                   (setq directory-callback callback)))
                ((symbol-function 't3-code-request)
                 (lambda (env operation input callback)
                   (should (eq env remote))
                   (setq request (list operation input) request-callback callback)))
                ((symbol-function 't3-code-new-thread)
                 (lambda (_choose project env)
                   (should (eq env remote))
                   (setq opened (list :project project :directory default-directory)))))
        (with-temp-buffer
          (setq default-directory "/ssh:me@host:/srv/majutsu/test/")
          (t3-code))
        (should-not request)
        (funcall directory-callback remote t)
        (should (equal (plist-get (cadr request) :workspaceRoot) "/srv/majutsu"))
        (funcall request-callback
                 '(:project (:id "remote-project" :name "majutsu" :root "/srv/majutsu")) nil)
        (should (equal (plist-get opened :directory) "/ssh:me@host:/srv/majutsu/"))))))

(ert-deftest t3-code-test-entry-prefix-opens-ledger-without-registering ()
  (t3-code-test--with-entry
    (setq shell-callback nil)
    (with-temp-buffer (t3-code t))
    (should ledger)
    (should-not shell-callback)
    (should-not request)))

(ert-deftest t3-code-test-new-thread-uses-registration-response-without-shell ()
  (with-temp-buffer
    (let* ((root (make-temp-file "t3-new-project" t))
           (environment (t3-code-environment-create
                         :id "new" :capabilities '(:threadLifecycle t)))
           (project (list :id "new-project" :name "majutsu" :root root))
           (t3-code-new-thread-workspace 'root)
           launch)
      (unwind-protect
          (cl-letf (((symbol-function 't3-code-thread--with-catalog)
                     (lambda (_env callback) (funcall callback nil)))
                    ((symbol-function 't3-code-thread--read-model-selection)
                     (lambda (&rest _) '(:instanceId "pi" :model "test")))
                    ((symbol-function 't3-code-compose-open-launch)
                     (lambda (_env value) (setq launch value))))
            (t3-code-new-thread nil project environment)
            (should (equal (plist-get launch :projectId) "new-project"))
            (should (equal (plist-get launch :directory) root))
            (should (equal (plist-get launch :workspaceStrategy) '(:type "root"))))
        (delete-directory root t)))))

(provide 't3-code-entry-test)
;;; t3-code-entry-test.el ends here
