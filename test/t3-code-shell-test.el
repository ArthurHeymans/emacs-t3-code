;;; t3-code-shell-test.el --- Shared shell state tests  -*- lexical-binding: t; -*-

(require 'ert)
(require 't3-code-shell)

(defun t3-code-test--shell (statuses)
  "Return a shell payload with one thread per (ID STATUS) in STATUSES."
  (list :projects
        (list (list :id "p" :name "owner/repo" :root "/work/repo"
                    :threads (mapcar (lambda (entry)
                                       (list :id (car entry) :title (car entry)
                                             :status (cadr entry)
                                             :path (or (nth 2 entry) "/work/repo")
                                             :unread (nth 3 entry)))
                                     statuses)))))

(ert-deftest t3-code-test-shell-announces-noteworthy-transitions-only ()
  (let ((environment (t3-code-environment-create :id "shell"))
        (t3-code-notify 'message)
        (t3-code-shell-update-functions (list #'t3-code-shell--announce-changes))
        messages)
    (cl-letf (((symbol-function 'message)
               (lambda (format &rest args) (push (apply #'format format args) messages))))
      (t3-code-shell--receive environment
                              (list :kind "snapshot"
                                    :payload (t3-code-test--shell '(("a" "running")
                                                                    ("b" "running")
                                                                    ("c" "idle")))))
      (should-not messages)
      (t3-code-shell--receive environment
                              (list :kind "event"
                                    :payload (t3-code-test--shell '(("a" "idle")
                                                                    ("b" "waiting-approval")
                                                                    ("c" "idle"))))))
    (should (equal (sort messages #'string<)
                   '("T3: “a” finished" "T3: “b” needs your attention")))))

(ert-deftest t3-code-test-shell-finds-project-counts-and-indicator ()
  (let ((environment (t3-code-environment-create :id "shell")))
    (setf (t3-code-environment-shell environment)
          (t3-code-test--shell '(("a" "running" "/work/repo")
                                 ("b" "idle" "/work/wt/feature" t)
                                 ("c" "idle" "/work/repo" :false))))
    (should (equal (plist-get (t3-code-shell-project-for-directory environment "/work/repo/src/")
                              :id)
                   "p"))
    (should (equal (plist-get (t3-code-shell-project-for-directory environment "/work/wt/feature")
                              :id)
                   "p"))
    (should-not (t3-code-shell-project-for-directory environment "/elsewhere"))
    (should (equal (t3-code-shell-counts environment) '(1 . 1)))
    (should (equal (substring-no-properties (t3-code-shell-indicator environment)) "1 active · 1 done"))
    (should (equal (plist-get (cdr (t3-code-shell-find-thread environment "b")) :path)
                   "/work/wt/feature"))))

(ert-deftest t3-code-test-shell-marks-unread-thread-visited-once ()
  (let ((environment (t3-code-environment-create
                      :id "shell" :state 'ready :capabilities '(:threadLifecycle t)))
        requests)
    (setf (t3-code-environment-shell environment)
          (t3-code-test--shell '(("b" "idle" "/work/repo" t))))
    (cl-letf (((symbol-function 't3-code-request)
               (lambda (_environment operation input _callback)
                 (push (list operation input) requests))))
      (t3-code-shell-mark-visited environment "b")
      (t3-code-shell-mark-visited environment "b"))
    (should (= (length requests) 1))
    (should (equal (plist-get (plist-get (cadr (car requests)) :command) :type)
                   "thread.visit"))))

(ert-deftest t3-code-test-shell-opening-records-first-visit-from-server-time ()
  (let ((environment (t3-code-environment-create
                      :id "shell" :state 'ready :capabilities '(:threadLifecycle t)))
        requests)
    (setf (t3-code-environment-shell environment)
          (t3-code-test--shell '(("b" "idle" "/work/repo" :false))))
    ;; A server clock ahead of ours must win, or the thread stays unread.
    (plist-put (cdr (t3-code-shell-find-thread environment "b"))
               :updatedAt "2999-01-01T00:00:00.000Z")
    (cl-letf (((symbol-function 't3-code-request)
               (lambda (_environment _operation input _callback)
                 (push (plist-get input :command) requests))))
      (t3-code-shell-mark-visited environment "b")
      (should-not requests)
      (t3-code-shell-mark-visited environment "b" t)
      (t3-code-shell-mark-visited environment "b" t))
    (should (= (length requests) 1))
    (should (equal (plist-get (car requests) :visitedAt) "2999-01-01T00:00:00.000Z"))))

(provide 't3-code-shell-test)
;;; t3-code-shell-test.el ends here
