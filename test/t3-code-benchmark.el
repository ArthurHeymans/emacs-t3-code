;;; t3-code-benchmark.el --- Isolated allocation benchmarks -*- lexical-binding: t; -*-

;; Run after `make compile':
;;   emacs -Q --batch -L . -l test/t3-code-benchmark.el
;; These synthetic workloads exercise bounded shell projections and unusually
;; fragmented input, without connecting to a server or a running Emacs session.
;; Output: (elapsed-seconds GC-count GC-seconds), then cumulative allocation
;; deltas from `memory-use-counts' (conses, floats, vector cells, symbols,
;; string characters, miscellaneous objects, intervals).

(require 'benchmark)
(require 't3-code-dashboard)

(defun t3-code-benchmark--measure (label repetitions function)
  "Report time and allocation counts for REPETITIONS of FUNCTION under LABEL."
  (funcall function)
  (garbage-collect)
  (let* ((before (memory-use-counts))
         (stats (benchmark-run repetitions (funcall function))))
    (princ (format "%s: %S allocations=%S\n" label stats
                   (cl-mapcar #'- (memory-use-counts) before)))))

(let* ((threads (cl-loop for i below 2000 collect
                         (list :id (format "t%d" i) :title "Thread" :status "running")))
       (environment
        (t3-code-environment-create
         :id "bench" :shell (list :projects (list (list :id "p" :threads threads))))))
  (t3-code-benchmark--measure
   "shell-find-first" 300 (lambda () (t3-code-shell-find-thread environment "t0")))
  (t3-code-benchmark--measure
   "shell-counts" 300 (lambda () (t3-code-shell-counts environment)))
  (with-temp-buffer
    (t3-code-dashboard-mode)
    (setq t3-code-dashboard--projects
          (list (list :id "p" :threads
                      (cons '(:id "parent" :status "idle")
                            (cl-loop for i below 1999 collect
                                     (list :id (format "child%d" i) :status "idle"
                                           :parentThreadId "parent"
                                           :relationshipToParent "subagent"))))))
    (t3-code-benchmark--measure "dashboard-agents" 10 #'t3-code-dashboard--entries)))

(t3-code-benchmark--measure
 "fragmented-line" 3
 (lambda ()
   (let* ((process (make-pipe-process :name "t3-bench-pipe" :noquery t))
          (environment (t3-code-environment-create :id "fragments" :process process)))
     (unwind-protect
         (progn
           (process-put process 't3-code-environment environment)
           (dotimes (_ 4000) (t3-code--process-filter process "x")))
       (delete-process process)))))

;;; t3-code-benchmark.el ends here
