;;; t3-code-core.el --- T3 bridge lifecycle and protocol  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Arthur Heymans
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools, processes

;;; Commentary:

;; Shared environment connections and the versioned NDJSON protocol used by
;; t3-code.el.  Raw T3 and Effect RPC schemas deliberately do not cross this
;; boundary.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup t3-code nil
  "An Emacs client for T3 Code."
  :group 'tools)

(defcustom t3-code-bridge-command '("t3" "client" "--stdio")
  "Command used to start the version-matched T3 bridge.
The command is an argv list.  Tests and `t3-code-demo' bind this to the fake
bridge included in the repository."
  :type '(repeat string))

(defcustom t3-code-max-line-bytes (* 1024 1024)
  "Maximum accepted bridge protocol line size in bytes."
  :type 'integer)

(defcustom t3-code-stderr-max-chars 4000
  "Maximum number of bridge stderr characters retained in an exit report."
  :type 'integer)

(defcustom t3-code-refresh-timeout 20
  "Seconds a refreshing subscription waits for its snapshot before asking again.
While refreshing, events are ignored, so a snapshot lost by the bridge would
otherwise freeze the view."
  :type 'number)

(defcustom t3-code-max-queued-records 1000
  "Maximum records queued while a bridge is not ready."
  :type 'integer)

(defconst t3-code-protocol-version 1)
(defconst t3-code--connection-phases
  '("connecting" "authenticating" "snapshot" "resuming" "ready"
    "repairing" "retrying"))

(cl-defstruct (t3-code-environment
               (:constructor t3-code-environment-create))
  id endpoint directory process (generation 0) (state 'disconnected)
  capabilities server-version bridge-version pinned-t3-version
  (pending (make-hash-table :test #'equal))
  (subscriptions (make-hash-table :test #'equal))
  (next-id 0) outbound-queue diagnostics exit-error fatal-error
  ;; Latest normalized shell projection, kept by `t3-code-shell'.
  shell shell-reference
  ;; Request results that rarely change (model catalog, provider commands).
  (cache (make-hash-table :test #'equal)))

(cl-defstruct (t3-code-subscription
               (:constructor t3-code-subscription-create))
  id kind identity (callbacks (make-hash-table :test #'equal))
  sequence generation synchronized)

(cl-defstruct (t3-code-subscription-reference
               (:constructor t3-code-subscription-reference-create))
  subscription token)

(defvar t3-code--environments (make-hash-table :test #'equal))
(defvar t3-code-environment-state-hook nil
  "Hook run with one argument, an environment whose state changed.")

(defun t3-code--json-parse (line)
  "Parse JSON LINE into a plist, or return nil for malformed input."
  (condition-case nil
      (json-parse-string line :object-type 'plist :array-type 'list
                         :null-object nil :false-object :false)
    (json-error nil)))

(defun t3-code--json-line (object)
  "Encode OBJECT as one newline-terminated JSON protocol record."
  (concat (json-serialize object :null-object nil :false-object :false) "\n"))

(defun t3-code--line-chunks-string (chunks)
  "Materialize reverse-ordered line CHUNKS."
  (if chunks (apply #'concat (nreverse (copy-sequence chunks))) ""))

(defun t3-code--accumulate-line-chunks (chunks chunk)
  "Add CHUNK to unfinished line CHUNKS.
Return (LINES . REMAINDER-CHUNKS), avoiding repeated concatenation of long
partial lines."
  (let ((lines nil) (start 0) newline)
    (while (setq newline (string-search "\n" chunk start))
      (let ((line (substring chunk start newline)))
        (when chunks
          (push line chunks)
          (setq line (t3-code--line-chunks-string chunks)
                chunks nil))
        (unless (string-empty-p line)
          (push line lines)))
      (setq start (1+ newline)))
    (when (< start (length chunk))
      (push (substring chunk start) chunks))
    (cons (nreverse lines) chunks)))

(defun t3-code--diagnose (environment format-string &rest args)
  "Append a bounded diagnostic to ENVIRONMENT using FORMAT-STRING and ARGS."
  (let* ((text (apply #'format format-string args))
         (diagnostics (cons text (t3-code-environment-diagnostics environment))))
    (setf (t3-code-environment-diagnostics environment)
          (seq-take diagnostics 100))))

(defun t3-code--set-state (environment state)
  "Set ENVIRONMENT to STATE and notify observers."
  (setf (t3-code-environment-state environment) state)
  (run-hook-with-args 't3-code-environment-state-hook environment))

(defun t3-code-get-environment (id endpoint &optional directory)
  "Return the canonical environment for ID and ENDPOINT.
DIRECTORY controls where the bridge starts.  M0 accepts local directories only."
  (or (gethash id t3-code--environments)
      (let ((environment
             (t3-code-environment-create
              :id id :endpoint endpoint :directory (or directory default-directory))))
        (puthash id environment t3-code--environments)
        environment)))

(defun t3-code--next-id (environment prefix)
  "Return a stable next ID in ENVIRONMENT with PREFIX."
  (format "%s-%d" prefix (cl-incf (t3-code-environment-next-id environment))))

(defun t3-code--bounded-enqueue (queue value)
  "Append VALUE to QUEUE or signal when the bounded queue is full."
  (when (>= (length queue) t3-code-max-queued-records)
    (error "T3 bridge outbound queue is full"))
  (append queue (list value)))

(defun t3-code--send-now (environment object)
  "Send OBJECT immediately to ENVIRONMENT's live process."
  (let ((process (t3-code-environment-process environment)))
    (unless (process-live-p process)
      (error "T3 bridge is not running"))
    (process-send-string process (t3-code--json-line object))))

(defun t3-code--send (environment object)
  "Send OBJECT when ENVIRONMENT is ready, preserving FIFO order."
  (if (eq (t3-code-environment-state environment) 'ready)
      (t3-code--send-now environment object)
    (setf (t3-code-environment-outbound-queue environment)
          (t3-code--bounded-enqueue
           (t3-code-environment-outbound-queue environment) object))))

(defun t3-code--flush-outbound (environment)
  "Send queued records for ENVIRONMENT."
  (let ((queue (t3-code-environment-outbound-queue environment)))
    (setf (t3-code-environment-outbound-queue environment) nil)
    (dolist (object queue)
      (t3-code--send-now environment object))))

(defun t3-code--stderr-excerpt (process)
  "Return a bounded stderr excerpt for PROCESS."
  (when-let* ((buffer (process-get process 't3-code-stderr-buffer))
              ((buffer-live-p buffer))
              (text (string-trim-right
                     (with-current-buffer buffer
                       (buffer-substring-no-properties (point-min) (point-max)))))
              ((not (string-empty-p text))))
    (if (<= (length text) t3-code-stderr-max-chars)
        text
      (let* ((head (/ t3-code-stderr-max-chars 2))
             (tail (- t3-code-stderr-max-chars head)))
        (concat (substring text 0 head)
                "\n… [stderr truncated] …\n"
                (substring text (- (length text) tail)))))))

(defun t3-code--stderr-filter (process output)
  "Retain bounded head and tail diagnostic OUTPUT for stderr PROCESS."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((inhibit-read-only t)
            (limit (* 2 t3-code-stderr-max-chars)))
        (goto-char (point-max))
        (insert output)
        (when (> (buffer-size) limit)
          (delete-region (+ (point-min) t3-code-stderr-max-chars)
                         (- (point-max) t3-code-stderr-max-chars)))))))

(defun t3-code--cleanup-stderr (process)
  "Dispose of PROCESS's private stderr buffer."
  (when-let* ((buffer (process-get process 't3-code-stderr-buffer)))
    (process-put process 't3-code-stderr-buffer nil)
    (when (buffer-live-p buffer)
      (when-let* ((stderr-process (get-buffer-process buffer)))
        (set-process-query-on-exit-flag stderr-process nil)
        (delete-process stderr-process))
      (kill-buffer buffer))))

(defun t3-code--fail-pending (environment error-object)
  "Complete all pending ENVIRONMENT requests with ERROR-OBJECT."
  (let ((pending (t3-code-environment-pending environment)) callbacks)
    (maphash (lambda (_id callback) (push callback callbacks)) pending)
    (clrhash pending)
    (dolist (callback callbacks)
      (condition-case error
          (funcall callback nil error-object)
        (error
         (t3-code--diagnose environment "Exit callback failed: %s"
                            (error-message-string error)))))))

(defun t3-code--process-sentinel (process event)
  "Handle PROCESS state change EVENT."
  (unless (process-live-p process)
    (let* ((environment (process-get process 't3-code-environment))
           (stderr (t3-code--stderr-excerpt process))
           (error-object (list :code "bridge-exit"
                               :message (format "Bridge exited: %s"
                                                (string-trim event))
                               :exitCode (process-exit-status process)
                               :stderr stderr)))
      (when (eq process (t3-code-environment-process environment))
        (setf (t3-code-environment-process environment) nil
              (t3-code-environment-outbound-queue environment) nil
              (t3-code-environment-exit-error environment) error-object)
        (t3-code--fail-pending environment error-object)
        (t3-code--set-state environment 'disconnected))
      (t3-code--cleanup-stderr process))))

(defun t3-code--process-filter (process output)
  "Decode NDJSON OUTPUT from bridge PROCESS."
  (let* ((environment (process-get process 't3-code-environment))
         (chunks (process-get process 't3-code-partial-output-chunks))
         (result (t3-code--accumulate-line-chunks chunks output)))
    (if (not (eq process (t3-code-environment-process environment)))
        (t3-code--diagnose environment "Ignored output from stale bridge process")
      (process-put process 't3-code-partial-output-chunks (cdr result))
      (when (> (string-bytes (t3-code--line-chunks-string (cdr result)))
               t3-code-max-line-bytes)
        (t3-code--diagnose environment "Rejected oversized partial bridge line")
        (delete-process process))
      (dolist (line (car result))
        (cond
         ((> (string-bytes line) t3-code-max-line-bytes)
          (t3-code--diagnose environment "Rejected oversized bridge line (%d bytes)"
                             (string-bytes line))
          (delete-process process))
         (t
          (if-let* ((message (t3-code--json-parse line)))
              (condition-case error
                  (t3-code--dispatch environment message)
                (error
                 (t3-code--diagnose environment "Dispatch error: %s"
                                    (error-message-string error))
                 (when (equal (plist-get message :kind) "ready")
                   (delete-process process))))
            (t3-code--diagnose environment "Malformed bridge JSON: %.200s" line))))))))

(defun t3-code--dispatch (environment message)
  "Dispatch one normalized bridge MESSAGE for ENVIRONMENT."
  (pcase (plist-get message :kind)
    ("ready" (t3-code--handle-ready environment message))
    ("response" (t3-code--handle-response environment message))
    ((or "snapshot" "event" "synchronized")
     (t3-code--handle-subscription-message environment message))
    ("state"
     (let ((phase (plist-get message :phase)))
       (if (member phase t3-code--connection-phases)
           (progn
             (when (plist-get message :message)
               (t3-code--diagnose environment "Bridge state %s: %s%s"
                                  phase (plist-get message :message)
                                  (if-let* ((retry (plist-get message :retryAt)))
                                      (format " (retry at %s)" retry)
                                    "")))
             (t3-code--set-state environment (intern phase)))
         (t3-code--diagnose environment "Unknown bridge state phase: %S" phase))))
    ("fatal"
     (setf (t3-code-environment-fatal-error environment)
           (list :code (plist-get message :code)
                 :message (plist-get message :message)))
     (t3-code--diagnose environment "Bridge fatal%s: %s%s"
                        (if-let* ((code (plist-get message :code)))
                            (format " [%s]" code) "")
                        (or (plist-get message :message) "unknown error")
                        (if-let* ((stderr (plist-get message :stderrExcerpt)))
                            (format "\n%s" (substring stderr 0
                                                       (min (length stderr)
                                                            t3-code-stderr-max-chars)))
                          ""))
     (when-let* ((process (t3-code-environment-process environment)))
       (delete-process process)))
    (_ (t3-code--diagnose environment "Unknown bridge message kind: %S"
                           (plist-get message :kind)))))

(defun t3-code--stream-capability-key (stream)
  "Return the advertised capability key for normalized STREAM."
  (pcase stream
    ("shell" :shell)
    ("thread" :threads)
    (_ (intern (concat ":" stream)))))

(defun t3-code--handle-ready (environment message)
  "Validate ready MESSAGE and activate ENVIRONMENT."
  (unless (= (or (plist-get message :protocolVersion) -1)
             t3-code-protocol-version)
    (error "Unsupported t3e protocol version: %S"
           (plist-get message :protocolVersion)))
  (unless (equal (plist-get message :environmentId)
                 (t3-code-environment-id environment))
    (error "Bridge environment mismatch"))
  (dolist (field '(:bridgeVersion :pinnedT3Version :serverVersion))
    (unless (stringp (plist-get message field))
      (error "Bridge ready message lacks required %s" field)))
  (unless (plist-member message :capabilities)
    (error "Bridge ready message lacks required capabilities"))
  (setf (t3-code-environment-bridge-version environment)
        (plist-get message :bridgeVersion)
        (t3-code-environment-pinned-t3-version environment)
        (plist-get message :pinnedT3Version)
        (t3-code-environment-server-version environment)
        (plist-get message :serverVersion)
        (t3-code-environment-capabilities environment)
        (plist-get message :capabilities))
  (let ((capabilities (t3-code-environment-capabilities environment)))
    (setf (t3-code-environment-outbound-queue environment)
          (seq-filter
           (lambda (record)
             (let* ((stream (and (equal (plist-get record :kind) "subscribe")
                                 (plist-get record :stream)))
                    (unsupported
                     (and stream
                          (not (eq (plist-get capabilities
                                              (t3-code--stream-capability-key stream))
                                   t)))))
               (when unsupported
                 (t3-code--diagnose environment
                                    "Server does not advertise %s capability" stream))
               (not unsupported)))
           (t3-code-environment-outbound-queue environment))))
  (t3-code--set-state environment 'ready)
  (t3-code--flush-outbound environment))

(defun t3-code--handle-response (environment message)
  "Correlate response MESSAGE in ENVIRONMENT."
  (let* ((id (plist-get message :id))
         (pending (t3-code-environment-pending environment))
         (callback (and id (gethash id pending))))
    (if callback
        (progn
          (remhash id pending)
          (funcall callback (plist-get message :result)
                   (plist-get message :error)))
      (t3-code--diagnose environment "Response for unknown request: %S" id))))

(defun t3-code--subscription-record (subscription &optional without-resume)
  "Build a subscribe record for SUBSCRIPTION.
Omit resume state when WITHOUT-RESUME is non-nil."
  (append (list :kind "subscribe"
                :subscriptionId (t3-code-subscription-id subscription)
                :stream (t3-code-subscription-kind subscription)
                :identity (t3-code-subscription-identity subscription))
          (when (and (not without-resume)
                     (t3-code-subscription-sequence subscription))
            (list :resumeSequence (t3-code-subscription-sequence subscription)))))

(defun t3-code--resubscribe (environment subscription)
  "Ask ENVIRONMENT's bridge for a fresh snapshot of SUBSCRIPTION.
Events are ignored until it arrives; a watchdog asks again if it does not."
  (setf (t3-code-subscription-sequence subscription) nil
        (t3-code-subscription-synchronized subscription) 'refreshing)
  (t3-code--send-now environment
                     (list :kind "unsubscribe"
                           :subscriptionId (t3-code-subscription-id subscription)))
  (t3-code--send-now environment (t3-code--subscription-record subscription t))
  (t3-code--watch-refresh environment subscription
                          (t3-code-environment-process environment)))

(defun t3-code--watch-refresh (environment subscription process)
  "Resubscribe SUBSCRIPTION if it is still refreshing after a timeout.
PROCESS is the bridge the request went to; a restarted bridge resubscribes
on its own, and a released subscription needs nothing."
  (run-at-time
   t3-code-refresh-timeout nil
   (lambda ()
     (when (and (eq (t3-code-subscription-synchronized subscription) 'refreshing)
                (eq subscription (gethash (t3-code-subscription-id subscription)
                                          (t3-code-environment-subscriptions environment)))
                (eq process (t3-code-environment-process environment))
                (process-live-p process))
       (if (eq (t3-code-environment-state environment) 'ready)
           (progn
             (t3-code--diagnose environment "No snapshot for %s after %ss; resubscribing"
                                (t3-code-subscription-id subscription)
                                t3-code-refresh-timeout)
             (t3-code--resubscribe environment subscription))
         ;; The bridge is reconnecting; it resubscribes when ready.
         (t3-code--watch-refresh environment subscription process))))))

(defun t3-code--repair-subscription (environment subscription)
  "Request an authoritative replacement snapshot for SUBSCRIPTION."
  (t3-code--set-state environment 'repairing)
  (t3-code--resubscribe environment subscription))

(defun t3-code--handle-subscription-message (environment message)
  "Apply normalized subscription MESSAGE in ENVIRONMENT."
  (let* ((id (plist-get message :subscriptionId))
         (subscription (gethash id (t3-code-environment-subscriptions environment)))
         (generation (plist-get message :generation))
         (sequence (plist-get message :sequence))
         (kind (plist-get message :kind)))
    (cond
     ((null subscription)
      (t3-code--diagnose environment "Message for unknown subscription: %S" id))
     ((not (= (or generation -1) (t3-code-environment-generation environment)))
      (t3-code--diagnose environment "Ignored stale generation %S for %s" generation id))
     ((and (eq (t3-code-subscription-synchronized subscription) 'refreshing)
           (not (equal kind "snapshot")))
      nil)
     ((and (equal kind "event") sequence
           (t3-code-subscription-sequence subscription)
           (<= sequence (t3-code-subscription-sequence subscription)))
      nil)
     ((and (equal kind "event") sequence
           (t3-code-subscription-sequence subscription)
           (> sequence (1+ (t3-code-subscription-sequence subscription))))
      (t3-code--diagnose environment "Sequence gap for %s: expected %d, got %d"
                         id (1+ (t3-code-subscription-sequence subscription)) sequence)
      (t3-code--repair-subscription environment subscription))
     (t
      (when (equal kind "snapshot")
        (setf (t3-code-subscription-synchronized subscription) nil))
      (when sequence
        (setf (t3-code-subscription-sequence subscription)
              (if (equal kind "snapshot") sequence
                (max sequence (or (t3-code-subscription-sequence subscription) sequence)))))
      (setf (t3-code-subscription-generation subscription) generation)
      (when (equal kind "synchronized")
        (setf (t3-code-subscription-synchronized subscription) t)
        (when (eq (t3-code-environment-state environment) 'repairing)
          (t3-code--set-state environment 'ready)))
      (let (callbacks)
        (maphash (lambda (_token callback) (push callback callbacks))
                 (t3-code-subscription-callbacks subscription))
        (dolist (callback callbacks)
          (condition-case error
              (funcall callback message)
            (error
             (t3-code--diagnose environment "Subscription callback failed: %s"
                                (error-message-string error))))))))))

(defun t3-code-connect (environment &optional credential)
  "Start ENVIRONMENT's bridge and initiate the protocol handshake.
CREDENTIAL, when non-nil, is (TYPE . TOKEN), where TYPE is `pairing' or
`bearer'.  Only the bridge process inherits it; no token is retained."
  (unless (process-live-p (t3-code-environment-process environment))
    (unless (and (listp t3-code-bridge-command) t3-code-bridge-command)
      (user-error "`t3-code-bridge-command' is not configured"))
    (let* ((default-directory (t3-code-environment-directory environment))
           (_ (when (file-remote-p default-directory)
                (user-error "SSH/TRAMP environments are not implemented yet")))
           (command t3-code-bridge-command)
           (stderr-buffer (generate-new-buffer " *t3-code-stderr*"))
           process)
      (condition-case error
          (progn
            (setq process
                  (let ((process-environment
                         (if credential
                             (append (if (eq (car credential) 'bearer)
                                         (list (concat "T3_CLIENT_ACCESS_TOKEN="
                                                       (cdr credential))
                                               "T3_CLIENT_PAIRING_TOKEN=")
                                       (list "T3_CLIENT_ACCESS_TOKEN="
                                             (concat "T3_CLIENT_PAIRING_TOKEN="
                                                     (cdr credential))))
                                     process-environment)
                           process-environment)))
                    (make-process :name (format "t3e:%s" (t3-code-environment-id environment))
                                  :command command :connection-type 'pipe :noquery t
                                  :file-handler t :stderr stderr-buffer
                                  :filter #'t3-code--process-filter
                                  :sentinel #'t3-code--process-sentinel)))
            (process-put process 't3-code-environment environment)
            (process-put process 't3-code-stderr-buffer stderr-buffer)
            (when-let* ((stderr-process (get-buffer-process stderr-buffer)))
              (set-process-query-on-exit-flag stderr-process nil)
              (set-process-filter stderr-process #'t3-code--stderr-filter))
            ;; Catalogs and provider commands may differ after a restart.
            (clrhash (t3-code-environment-cache environment))
            (setf (t3-code-environment-process environment) process
                  (t3-code-environment-generation environment)
                  (1+ (t3-code-environment-generation environment))
                  (t3-code-environment-exit-error environment) nil
                  (t3-code-environment-fatal-error environment) nil)
            (t3-code--set-state environment 'connecting)
            (t3-code--send-now
             environment
             (list :kind "hello" :protocolVersion t3-code-protocol-version
                   :client (list :name "t3-code.el" :version "0.1.0")
                   :environment
                   (list :id (t3-code-environment-id environment)
                         :endpoint (t3-code-environment-endpoint environment)
                         :generation (t3-code-environment-generation environment)))))
        (error
         (when (buffer-live-p stderr-buffer) (kill-buffer stderr-buffer))
         (signal (car error) (cdr error))))))
  environment)

(defun t3-code-disconnect (environment)
  "Explicitly disconnect ENVIRONMENT and discard queued outbound work."
  (setf (t3-code-environment-outbound-queue environment) nil)
  (when-let* ((process (t3-code-environment-process environment)))
    (delete-process process))
  environment)

(defun t3-code-restart (environment &optional credential)
  "Restart ENVIRONMENT and resume its subscriptions by sequence.
Pass CREDENTIAL only to the new bridge process, as in `t3-code-connect'."
  (t3-code-disconnect environment)
  (t3-code-connect environment credential)
  (maphash (lambda (_id subscription)
             (t3-code--send environment
                            (t3-code--subscription-record subscription)))
           (t3-code-environment-subscriptions environment))
  environment)

(defun t3-code-request (environment operation input callback)
  "Call semantic OPERATION with INPUT in ENVIRONMENT.
CALLBACK receives (RESULT ERROR).  Mutations are not retried by this layer."
  (let ((id (t3-code--next-id environment "request")))
    (puthash id callback (t3-code-environment-pending environment))
    (t3-code--send environment
                   (list :kind "request" :id id :operation operation :input input))
    id))

(defun t3-code-capability-p (environment capability)
  "Whether ENVIRONMENT's bridge advertises CAPABILITY (a keyword)."
  (and environment
       (eq (plist-get (t3-code-environment-capabilities environment) capability) t)))

(defun t3-code-request-sync (environment operation input &optional timeout)
  "Call OPERATION with INPUT in ENVIRONMENT and wait up to TIMEOUT seconds.
Return the result, or signal a `user-error' on failure or timeout.  Only for
short reads such as completion candidates; mutations stay asynchronous."
  (let* ((done nil) result failure
         (id (t3-code-request environment operation input
                              (lambda (value error)
                                (setq result value failure error done t))))
         (deadline (+ (float-time) (or timeout 3))))
    (while (and (not done) (< (float-time) deadline))
      (accept-process-output nil 0.02))
    (unless done
      (t3-code-cancel-request environment id)
      (user-error "T3 %s timed out" operation))
    (when failure
      (user-error "T3 %s failed: %s" operation (or (plist-get failure :message) failure)))
    result))

(defun t3-code-new-id (&optional prefix)
  "Return a fresh random identifier, optionally starting with PREFIX.
Used for client-supplied command, message and thread IDs."
  (let ((hex (secure-hash 'sha256 (format "%s:%s:%s" (emacs-pid) (float-time) (random)))))
    (concat (or prefix "")
            (format "%s-%s-4%s-%s-%s"
                    (substring hex 0 8) (substring hex 8 12) (substring hex 13 16)
                    (substring hex 16 20) (substring hex 20 32)))))

(defun t3-code-cancel-request (environment id)
  "Cancel pending request ID in ENVIRONMENT."
  (when (remhash id (t3-code-environment-pending environment))
    (t3-code--send environment (list :kind "cancel" :id id))))

(defun t3-code-subscribe (environment kind identity callback)
  "Reference a KIND subscription for IDENTITY in ENVIRONMENT.
CALLBACK receives snapshot, event, and synchronized protocol records.  Return a
view-specific reference suitable for `t3-code-unsubscribe'."
  (when (and (eq (t3-code-environment-state environment) 'ready)
             (not (eq (plist-get (t3-code-environment-capabilities environment)
                                 (t3-code--stream-capability-key kind))
                      t)))
    (user-error "Connected server does not advertise %s capability" kind))
  (let* ((key (format "%s:%s" kind (json-serialize identity)))
         (subscriptions (t3-code-environment-subscriptions environment))
         (subscription (gethash key subscriptions))
         (new-subscription-p (null subscription))
         (token (t3-code--next-id environment "listener")))
    (unless subscription
      (setq subscription
            (t3-code-subscription-create :id key :kind kind :identity identity))
      (puthash key subscription subscriptions))
    (puthash token callback (t3-code-subscription-callbacks subscription))
    (when new-subscription-p
      (t3-code--send environment (t3-code--subscription-record subscription)))
    (t3-code-subscription-reference-create
     :subscription subscription :token token)))

(defun t3-code-refresh-subscription (environment reference)
  "Reload REFERENCE from an authoritative snapshot.
Keep ENVIRONMENT's bridge connection alive."
  (unless (eq (t3-code-environment-state environment) 'ready)
    (user-error "T3 environment is not ready"))
  (let* ((subscription (t3-code-subscription-reference-subscription reference))
         (id (t3-code-subscription-id subscription)))
    (unless (eq subscription (gethash id (t3-code-environment-subscriptions environment)))
      (user-error "T3 subscription is no longer active"))
    (t3-code--resubscribe environment subscription)))

(defun t3-code-unsubscribe (environment reference)
  "Release view-specific subscription REFERENCE from ENVIRONMENT."
  (let* ((subscription (t3-code-subscription-reference-subscription reference))
         (callbacks (t3-code-subscription-callbacks subscription)))
    (remhash (t3-code-subscription-reference-token reference) callbacks)
    (when (= (hash-table-count callbacks) 0)
      (remhash (t3-code-subscription-id subscription)
               (t3-code-environment-subscriptions environment))
      (t3-code--send environment
                     (list :kind "unsubscribe"
                           :subscriptionId (t3-code-subscription-id subscription))))))

(provide 't3-code-core)
;;; t3-code-core.el ends here
