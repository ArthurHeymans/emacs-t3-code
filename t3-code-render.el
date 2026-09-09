;;; t3-code-render.el --- Sectioned T3 transcript  -*- lexical-binding: t; -*-

;;; Commentary:
;; A bounded transcript built from normalized items.  Stable keyed rows let
;; streaming updates change one body without replacing the reader's history.
;; Visibility is local UI state, never an instruction to the server.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'imenu)
(require 't3-code-core)

(defvar-local t3-code-thread--payload nil)
(defvar-local t3-code-thread--environment nil)
(defvar-local t3-code-thread--visibility nil)
(defvar-local t3-code-thread--rows nil)
(defvar-local t3-code-thread--positions nil)
(defvar-local t3-code-thread--fold-overlays nil)
(defvar-local t3-code-thread--view 'conversation)
(defvar-local t3-code-thread--rendered-payload nil)
(defvar-local t3-code-thread--unseen 0)

(declare-function t3-code-thread--item-face "t3-code-thread" (type))

(defun t3-code-thread--preview (text)
  "Return a bounded single-line preview of TEXT."
  (truncate-string-to-width
   (replace-regexp-in-string "[\n\r\t ]+" " " (string-trim (or text "")))
   80 nil nil "…"))

(defun t3-code-thread--open-p (key default)
  "Return visibility of KEY, falling back to DEFAULT for unseen sections."
  (gethash key t3-code-thread--visibility default))

(defun t3-code-thread--attention-p (item)
  "Whether ITEM deserves attention outside any folded turn."
  (or (equal (plist-get item :status) "failed")
      (equal (plist-get item :type) "error")
      (and (member (plist-get item :type) '("approval_request" "user_input_request"))
           (member (plist-get item :status) '("pending" "waiting")))))

(defun t3-code-thread--message-p (item)
  "Whether ITEM belongs in conversation rather than the work group."
  (and (not (equal (plist-get item :presentation) "work"))
       (member (plist-get item :type)
               '("user_message" "assistant_message" "proposed_plan"
                 "approval_request" "user_input_request" "error"))))

(defun t3-code-thread--item-node (item)
  "Make a section node for normalized ITEM."
  (let* ((type (plist-get item :type))
         (label (or (plist-get item :label) type "Activity"))
         (status (plist-get item :status))
         (text (plist-get item :text))
         (detail (plist-get item :detail))
         (messagep (t3-code-thread--message-p item)))
    (list :key (concat "item:" (plist-get item :id))
          :item-id (plist-get item :id)
          :face (t3-code-thread--item-face type)
          :heading (concat label
                           (if messagep ""
                             (concat " · " (t3-code-thread--preview
                                            (or (plist-get item :title)
                                                (unless (equal type "reasoning") text)
                                                label))))
                           (if (or (eq (plist-get item :streaming) t)
                                   (member status '("running" "waiting" "pending" "failed")))
                               (format "  [%s]" (if (eq (plist-get item :streaming) t)
                                                       "streaming" status))
                             ""))
          :open messagep
          :body (string-join (seq-filter (lambda (s) (and (stringp s) (not (string-empty-p s))))
                                        (list text detail)) "\n\n"))))

(defun t3-code-thread--run-node (run-id items)
  "Group ITEMS belonging to RUN-ID without inferring run boundaries."
  (let* ((user (seq-find (lambda (item) (equal (plist-get item :type) "user_message")) items))
         (status (seq-some (lambda (item) (plist-get item :runStatus)) items))
         (ordinal (seq-some (lambda (item) (plist-get item :runOrdinal)) items))
         (work (seq-remove #'t3-code-thread--message-p items))
         (work-node (when work
                      (list :key (concat "work:" run-id) :open nil
                            :heading (format "Work · %d activities%s" (length work)
                                             (if status (concat " · " status) ""))
                            :face 'shadow
                            :children (mapcar #'t3-code-thread--item-node work))))
         (work-inserted nil))
    (list :key (concat "run:" run-id) :run t
          :heading (format "Turn%s · %s%s"
                           (if ordinal (format " %s" ordinal) "")
                           (if user (t3-code-thread--preview (plist-get user :text))
                             "Earlier prompt not in loaded history")
                           (if status (concat "  [" status "]") ""))
          :face 'font-lock-keyword-face
          :open (eq t3-code-thread--view 'conversation)
          :children (delq nil
                          (mapcar (lambda (item)
                                    (if (t3-code-thread--message-p item)
                                        (t3-code-thread--item-node item)
                                      (unless work-inserted
                                        (setq work-inserted t)
                                        work-node)))
                                  items)))))

(defun t3-code-thread--nodes ()
  "Build sections from the current bounded projection."
  (let ((groups (make-hash-table :test #'equal)) order
        (items (plist-get t3-code-thread--payload :items)))
    (dolist (item items)
      (let ((key (if-let* ((run (plist-get item :runId)))
                     (cons 'run run)
                   (cons 'item (plist-get item :id)))))
        (unless (gethash key groups) (push key order))
        (push item (gethash key groups))))
    (mapcar (lambda (key)
              (let ((members (nreverse (gethash key groups))))
                (if (eq (car key) 'run)
                    (t3-code-thread--run-node (cdr key) members)
                  (t3-code-thread--item-node (car members)))))
            (nreverse order))))

(defun t3-code-thread--node-rows (node &optional depth)
  "Flatten NODE into stable text rows, indented by DEPTH."
  (let* ((depth (or depth 0))
         (key (plist-get node :key))
         (children (plist-get node :children))
         (body (plist-get node :body))
         (foldable (or children (and body (not (string-empty-p body))))))
    (append
     (list (list :key key :node node :header t
                 :text (propertize
                        (concat (make-string (* 2 depth) ?\s)
                                (if foldable
                                    (if (t3-code-thread--open-p key (plist-get node :open)) "▾ " "▸ ")
                                  "  ")
                                (plist-get node :heading) "\n")
                        'face (plist-get node :face))))
     (when (and body (not (string-empty-p body)))
       (list (list :key (concat key ":body") :owner key
                   :item-id (plist-get node :item-id)
                   :text (concat body "\n\n"))))
     (mapcan (lambda (child) (t3-code-thread--node-rows child (1+ depth))) children)
     (list (list :key (concat key ":end") :end key :text "")))))

(defun t3-code-thread--attention-items ()
  "Return attention items, including any supplied outside the history window."
  (seq-uniq (append (plist-get t3-code-thread--payload :attention)
                    (seq-filter #'t3-code-thread--attention-p
                                (plist-get t3-code-thread--payload :items)))
            (lambda (a b) (equal (plist-get a :id) (plist-get b :id)))))

(defun t3-code-thread--render-rows ()
  "Build visible headings and bounded body rows for the current payload."
  (let* ((payload t3-code-thread--payload)
         (notice (cond
                  ((null payload)
                   (if (and t3-code-thread--environment
                            (eq (t3-code-environment-state t3-code-thread--environment) 'disconnected))
                       "Thread unavailable: disconnected. Press g to reconnect.\n"
                     "Loading thread…\n"))
                  ((plist-get payload :error) (concat "Could not load thread\n" (plist-get payload :error) "\n"))
                  ((eq (plist-get payload :deleted) t) "This thread was deleted.\n")
                  ((eq (plist-get payload :truncated) t)
                   "History incomplete: showing the recent bounded timeline.\n")))
         (attention (t3-code-thread--attention-items))
         (pending (plist-get payload :pendingRequestCount)))
    (append
     (when notice (list (list :key "notice" :text (propertize notice 'face 'shadow))))
     (when (and pending (> pending 0))
       (list (list :key "pending" :text (propertize
                                        (format "Needs attention · %d pending requests\n" pending)
                                        'face 'warning))))
     (mapcar (lambda (item)
               (list :key (concat "attention:" (plist-get item :id))
                     :item-id (plist-get item :id)
                     :text (propertize
                            (format "! %s · %s  (RET inspect)\n"
                                    (or (plist-get item :label) "Needs attention")
                                    (t3-code-thread--preview (or (plist-get item :text)
                                                               (plist-get item :title))))
                            'face 'warning 't3-code-attention (plist-get item :id))))
             attention)
     (mapcan #'t3-code-thread--node-rows (t3-code-thread--nodes))
     ;; Requests older than the history window still have an inspectable body.
     (mapcan #'t3-code-thread--node-rows
             (mapcar #'t3-code-thread--item-node
                     (seq-remove (lambda (item)
                                   (seq-find (lambda (other) (equal (plist-get item :id)
                                                                  (plist-get other :id)))
                                             (plist-get payload :items)))
                                 attention)))
     (when (and payload (null (plist-get payload :items)) (not notice))
       (list (list :key "empty" :text "No timeline items yet.\n"))))))

(defun t3-code-thread--anchor (position)
  "Describe POSITION relative to a stable row."
  (let* ((position (min position (point-max)))
         (key (get-text-property position 't3-code-row-key))
         (bounds (and key (gethash key t3-code-thread--positions))))
    (list key (if bounds (- position (car bounds)) 0) position)))

(defun t3-code-thread--restore-anchor (anchor)
  "Resolve ANCHOR against current row positions."
  (let ((bounds (gethash (car anchor) t3-code-thread--positions)))
    (if bounds (min (+ (car bounds) (cadr anchor)) (max (car bounds) (1- (cdr bounds))))
      (min (or (nth 2 anchor) (point-min)) (point-max)))))

(defun t3-code-thread--sync-rows (rows)
  "Reconcile keyed ROWS, leaving unchanged text and its markers in place."
  (let ((old t3-code-thread--rows)
        (positions (make-hash-table :test #'equal)))
    (goto-char (point-min))
    (dolist (row rows)
      (let* ((key (plist-get row :key))
             (match (seq-position old key (lambda (candidate wanted)
                                           (equal (plist-get candidate :key) wanted)))))
        (when match
          (dotimes (_ match)
            (delete-region (point) (+ (point) (length (plist-get (pop old) :text))))))
        (let* ((start (point))
               (text (plist-get row :text))
               (item-id (or (plist-get row :item-id)
                            (plist-get (plist-get row :node) :item-id))))
          (if (and old (equal key (plist-get (car old) :key)))
              (let ((previous (pop old)))
                (if (equal-including-properties text (plist-get previous :text))
                    (forward-char (length text))
                  (delete-region start (+ start (length (plist-get previous :text))))
                  (insert text)))
            (insert text))
          (add-text-properties start (point)
                               (list 't3-code-row-key key
                                     't3-code-thread-item-id item-id
                                     'rear-nonsticky t))
          (puthash key (cons start (point)) positions))))
    (delete-region (point) (point-max))
    (setq t3-code-thread--rows rows
          t3-code-thread--positions positions)))

(defun t3-code-thread--isearch-open (overlay)
  "Permanently open the section represented by OVERLAY after a search match."
  (puthash (overlay-get overlay 't3-code-section) t t3-code-thread--visibility)
  (overlay-put overlay 'invisible nil))

(defun t3-code-thread--isearch-temporary (overlay hide)
  "Temporarily reveal OVERLAY during isearch, or restore it when HIDE is non-nil."
  (overlay-put overlay 'invisible (and hide 't3-code-fold)))

(defun t3-code-thread--install-folds (rows)
  "Install visibility overlays over bodies in ROWS."
  (dolist (row rows)
    (when-let* ((node (plist-get row :node))
                (key (plist-get node :key))
                (heading (gethash key t3-code-thread--positions))
                (end (gethash (concat key ":end") t3-code-thread--positions)))
      (when (< (cdr heading) (car end))
        (let ((overlay (make-overlay (cdr heading) (car end))))
          (overlay-put overlay 't3-code-section key)
          (overlay-put overlay 'invisible
                       (unless (t3-code-thread--open-p key (plist-get node :open)) 't3-code-fold))
          (overlay-put overlay 'isearch-open-invisible #'t3-code-thread--isearch-open)
          (overlay-put overlay 'isearch-open-invisible-temporary #'t3-code-thread--isearch-temporary)
          (push overlay t3-code-thread--fold-overlays))))))

(defun t3-code-thread--refresh ()
  "Update the transcript while preserving semantic point and window anchors."
  (when (and (derived-mode-p 't3-code-thread-mode)
             (not (bound-and-true-p isearch-mode)))
    (unless t3-code-thread--visibility (t3-code-thread-render-setup))
    (let* ((anchor (t3-code-thread--anchor (point)))
           (windows (mapcar (lambda (window)
                              (list window
                                    (t3-code-thread--anchor (window-start window))
                                    (t3-code-thread--anchor (window-point window))
                                    ;; Follow is window-local and disarms as soon as the reader moves.
                                    (and (window-parameter window 't3-code-follow)
                                         (>= (window-point window) (1- (point-max))))))
                            (get-buffer-window-list (current-buffer) nil t)))
           (rows (t3-code-thread--render-rows))
           (changed (not (equal t3-code-thread--rendered-payload t3-code-thread--payload)))
           (inhibit-read-only t))
      (dolist (overlay t3-code-thread--fold-overlays) (delete-overlay overlay))
      (setq t3-code-thread--fold-overlays nil)
      (t3-code-thread--sync-rows rows)
      (t3-code-thread--install-folds rows)
      (goto-char (t3-code-thread--restore-anchor anchor))
      (dolist (entry windows)
        (let ((window (car entry)))
          (if (nth 3 entry)
              (progn
                (set-window-point window (point-max))
                (with-selected-window window (goto-char (point-max)) (recenter -1)))
            (set-window-parameter window 't3-code-follow nil)
            (set-window-point window (t3-code-thread--restore-anchor (nth 2 entry)))
            (set-window-start window (t3-code-thread--restore-anchor (nth 1 entry)) t))))
      (when changed
        (setq t3-code-thread--rendered-payload (copy-tree t3-code-thread--payload)
              t3-code-thread--unseen (if (seq-some (lambda (entry) (nth 3 entry)) windows)
                                         0 (1+ t3-code-thread--unseen))))
      (setq imenu--index-alist nil)
      (set-buffer-modified-p nil)
      (force-mode-line-update))))

(defun t3-code-thread--item-at-point ()
  "Return the normalized item ID at point."
  (get-char-property (point) 't3-code-thread-item-id))

(defun t3-code-thread--goto-item (id)
  "Move to item ID; return non-nil when it is loaded."
  (when-let* ((bounds (gethash (concat "item:" id) t3-code-thread--positions)))
    (goto-char (car bounds))
    t))

(defun t3-code-thread--section-at-point ()
  "Find the closest enclosing section key at point."
  (let* ((key (get-text-property (point) 't3-code-row-key))
         (row (seq-find (lambda (row) (equal key (plist-get row :key))) t3-code-thread--rows)))
    (or (and (plist-get row :header) key) (plist-get row :owner))))

(defun t3-code-thread-toggle-details ()
  "Toggle the item, work group or whole turn at point."
  (interactive)
  (let* ((key (t3-code-thread--section-at-point))
         (row (seq-find (lambda (row) (equal key (plist-get row :key))) t3-code-thread--rows))
         (node (plist-get row :node)))
    (unless node (user-error "No section at point"))
    (goto-char (car (gethash key t3-code-thread--positions)))
    (puthash key (not (t3-code-thread--open-p key (plist-get node :open))) t3-code-thread--visibility)
    (t3-code-thread--refresh)))

(defun t3-code-thread-inspect ()
  "Inspect an attention target, or toggle the section at point."
  (interactive)
  (if-let* ((id (get-text-property (point) 't3-code-attention)))
      (when (t3-code-thread--goto-item id)
        (let ((target (point)))
          (dolist (overlay t3-code-thread--fold-overlays)
            (when (and (<= (overlay-start overlay) target) (< target (overlay-end overlay)))
              (puthash (overlay-get overlay 't3-code-section) t t3-code-thread--visibility)))
          (puthash (concat "item:" id) t t3-code-thread--visibility)
          (t3-code-thread--refresh)))
    (t3-code-thread-toggle-details)))

(defun t3-code-thread-cycle-view ()
  "Switch explicitly between conversation and whole-turn outline views."
  (interactive)
  (setq t3-code-thread--view (if (eq t3-code-thread--view 'conversation) 'outline 'conversation))
  (dolist (row t3-code-thread--rows)
    (let ((node (plist-get row :node)))
      (when (or (plist-get node :run)
                ;; Old bridges can still provide an item outline, not fake turns.
                (and (not (seq-some (lambda (item) (plist-get item :runId))
                                    (plist-get t3-code-thread--payload :items)))
                     (plist-get node :item-id)))
        (puthash (plist-get node :key)
                 (and (eq t3-code-thread--view 'conversation)
                      (or (plist-get node :run) (plist-get node :open)))
                 t3-code-thread--visibility))))
  (t3-code-thread--refresh)
  (message "T3 view: %s" t3-code-thread--view))

(defun t3-code-thread-next-turn (&optional backward)
  "Move to the next turn heading, or previous if BACKWARD is non-nil."
  (interactive)
  (let* ((positions (mapcar #'cdr (t3-code-thread--imenu)))
         (target (if backward
                     (seq-find (lambda (position) (< position (point))) (reverse positions))
                   (seq-find (lambda (position) (> position (point))) positions))))
    (unless target (user-error "No %s loaded turn" (if backward "previous" "next")))
    (goto-char target)))

(defun t3-code-thread-previous-turn ()
  "Move to the previous loaded turn."
  (interactive)
  (t3-code-thread-next-turn t))

(defun t3-code-thread--imenu ()
  "Return an index of loaded turns, or user messages on legacy bridges."
  (delq nil
        (mapcar (lambda (row)
                  (let ((node (plist-get row :node)))
                    (when (or (plist-get node :run)
                              (and (not (seq-some (lambda (item) (plist-get item :runId))
                                                  (plist-get t3-code-thread--payload :items)))
                                   (plist-get node :item-id)
                                   (string-prefix-p "You" (plist-get node :heading))))
                      (cons (plist-get node :heading)
                            (car (gethash (plist-get row :key) t3-code-thread--positions))))))
                t3-code-thread--rows)))

(defun t3-code-thread-jump-to-latest ()
  "Resume following new output in the selected thread window."
  (interactive)
  (goto-char (point-max))
  (when (eq (window-buffer) (current-buffer))
    (set-window-parameter nil 't3-code-follow t)
    (recenter -1))
  (setq t3-code-thread--unseen 0)
  (force-mode-line-update))

(defun t3-code-thread-copy (begin end &optional raw)
  "Copy visible text between BEGIN and END; with RAW include folded text."
  (interactive "r\nP")
  (kill-new
   (if raw (buffer-substring-no-properties begin end)
     (let ((position begin) parts)
       (while (< position end)
         (let ((next (next-char-property-change position end)))
           (unless (invisible-p position)
             (push (buffer-substring-no-properties position next) parts))
           (setq position next)))
       (apply #'concat (nreverse parts)))))
  (deactivate-mark))

(defun t3-code-thread-render-setup ()
  "Initialize section rendering state in the current buffer."
  (setq t3-code-thread--visibility (make-hash-table :test #'equal)
        t3-code-thread--positions (make-hash-table :test #'equal)
        t3-code-thread--rows nil
        t3-code-thread--fold-overlays nil
        imenu-create-index-function #'t3-code-thread--imenu
        buffer-undo-list t)
  (add-to-invisibility-spec 't3-code-fold)
  ;; Do not replace overlays owned by an active isearch. Its end hook applies
  ;; the newest payload and refreshes any disclosure headings it opened.
  (add-hook 'isearch-mode-end-hook #'t3-code-thread--refresh nil t))

(provide 't3-code-render)
;;; t3-code-render.el ends here
