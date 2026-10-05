;;; t3-code-render.el --- Conversation transcript  -*- lexical-binding: t; -*-

;;; Commentary:
;; A bounded transcript built from normalized items, laid out like a chat:
;; each turn starts with a "You" heading, followed by the assistant's answer
;; with its tool calls inline.  Tool output shows a short preview and expands
;; with TAB.  Stable keyed rows let streaming updates change one body without
;; replacing the reader's history.  Visibility is local UI state, never an
;; instruction to the server.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'imenu)
(require 't3-code-core)
(require 't3-code-markdown)

(defcustom t3-code-tool-preview-lines 4
  "Lines of tool output shown while a tool block is collapsed."
  :type 'integer
  :group 't3-code)

(defcustom t3-code-thinking-display 'hidden
  "Initial display of reasoning.
`visible' expands it, `preview' shows its first lines and `hidden' only
its heading, as T3 Code does."
  :type '(choice (const :tag "Expanded" visible)
                 (const :tag "Collapsed preview" preview)
                 (const :tag "Heading only" hidden))
  :group 't3-code)

(defcustom t3-code-fold-work t
  "Whether tool calls and reasoning fold into collapsed groups, as in T3 Code.
A settled turn folds all its activity behind one \"Worked\" heading,
leaving the final answer, plans and failures visible.  A turn still in
progress folds each stretch of consecutive activity between its messages,
and its latest stretch names what is happening now."
  :type 'boolean
  :group 't3-code)

(defface t3-code-turn-heading-face
  '((t :inherit (bold font-lock-keyword-face) :height 1.1))
  "Face for \"You\" turn headings."
  :group 't3-code)

(defface t3-code-assistant-heading-face
  '((t :inherit (bold font-lock-function-name-face)))
  "Face for \"Assistant\" headings."
  :group 't3-code)

(defface t3-code-tool-name-face
  '((t :inherit font-lock-function-name-face :weight bold :slant italic))
  "Face for tool names in tool block headings."
  :group 't3-code)

(defface t3-code-tool-command-face
  '((t :inherit font-lock-function-name-face :slant italic))
  "Face for tool arguments in tool block headings."
  :group 't3-code)

(defface t3-code-tool-output-face
  '((t :inherit shadow))
  "Face for command and search output."
  :group 't3-code)

(defface t3-code-thinking-face
  '((t :inherit shadow :slant italic))
  "Face for reasoning headings."
  :group 't3-code)

(defface t3-code-collapsed-indicator-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for collapsed content indicators."
  :group 't3-code)

(defvar-local t3-code-thread--payload nil)
(defvar-local t3-code-thread--environment nil)
(defvar-local t3-code-thread--visibility nil)
(defvar-local t3-code-thread--rows nil)
(defvar-local t3-code-thread--positions nil)
(defvar-local t3-code-thread--fold-overlays nil)
(defvar-local t3-code-thread--view 'conversation)
(defvar-local t3-code-thread--rendered-payload nil)
(defvar-local t3-code-thread--stream-error nil)
(defvar-local t3-code-thread--rendered-error nil)
(defvar-local t3-code-thread--unseen 0)
(defvar-local t3-code-thread--older-items nil
  "Chronological items loaded from history before the live window.")
(defvar-local t3-code-thread--older-state nil
  "History paging state: nil, `loading', or `exhausted'.")
(defvar-local t3-code-thread--rendered-older nil)

(defun t3-code-thread--preview (text &optional width)
  "Return a bounded single-line preview of TEXT, at most WIDTH characters."
  (truncate-string-to-width
   (replace-regexp-in-string "[\n\r\t ]+" " " (string-trim (or text "")))
   (or width 80) nil nil "…"))

(defun t3-code-thread--open-p (key default)
  "Return visibility of KEY, falling back to DEFAULT for unseen sections."
  (gethash key t3-code-thread--visibility default))

(defun t3-code-thread--pending-p (item)
  "Whether ITEM is a request still waiting for a response."
  (and (member (plist-get item :type) '("approval_request" "user_input_request"))
       (member (plist-get item :status) '("pending" "waiting"))))

(defun t3-code-thread--attention-p (item)
  "Whether ITEM deserves attention outside any folded turn."
  (or (equal (plist-get item :status) "failed")
      (equal (plist-get item :type) "error")
      (t3-code-thread--pending-p item)))

(defun t3-code-thread--items ()
  "Return loaded history followed by the live window, without duplicates."
  (let* ((live (plist-get t3-code-thread--payload :items))
         (ids (make-hash-table :test #'equal)))
    (dolist (item live) (puthash (plist-get item :id) t ids))
    (append (seq-remove (lambda (item) (gethash (plist-get item :id) ids))
                        t3-code-thread--older-items)
            live)))

(defun t3-code-thread--status-suffix (item)
  "Return a heading suffix describing ITEM's live state."
  (let ((status (plist-get item :status)))
    (cond ((eq (plist-get item :streaming) t) "  [streaming]")
          ((member status '("running" "pending" "waiting" "failed"))
           (format "  [%s]" status))
          (t ""))))

(defun t3-code-thread--tool-heading (item)
  "Return the propertized heading for tool ITEM, imitating a shell transcript."
  (let* ((type (plist-get item :type))
         (text (plist-get item :text))
         (title (plist-get item :title))
         (verb-and-argument
          (pcase type
            ("command_execution" (cons "$" (t3-code-thread--preview text 120)))
            ("file_change" (cons "edit" (or (plist-get item :path) text)))
            ("file_search" (cons "search" (t3-code-thread--preview (or text title) 100)))
            ("web_search" (cons "web" (t3-code-thread--preview (or text title) 100)))
            ("subagent" (cons "agent" (t3-code-thread--preview (or title text) 100)))
            (_ (let ((label (or (plist-get item :label) type "Activity"))
                     (argument (t3-code-thread--preview (or title text) 100)))
                 ;; Labels such as "Tool · read" already name their title.
                 (cons label (unless (string-suffix-p argument label) argument)))))))
    (concat (propertize (car verb-and-argument) 'face 't3-code-tool-name-face)
            (if (string-empty-p (or (cdr verb-and-argument) ""))
                ""
              (propertize (concat " " (cdr verb-and-argument))
                          'face 't3-code-tool-command-face))
            (propertize (t3-code-thread--status-suffix item)
                        'face (if (equal (plist-get item :status) "failed") 'error 'shadow)))))

(defun t3-code-thread--tool-body (item)
  "Return the fontified expandable body of tool ITEM."
  (let* ((type (plist-get item :type))
         (text (plist-get item :text))
         (detail (plist-get item :detail))
         (body (string-join
                (seq-filter (lambda (s) (and (stringp s) (not (string-empty-p s))))
                            (pcase type
                              ;; The heading already shows the command or path.
                              ((or "command_execution" "file_change"
                                   "file_search" "web_search")
                               (list detail))
                              (_ (list (unless (equal text (plist-get item :title)) text)
                                       detail))))
                "\n\n")))
    (pcase type
      ("file_change" (t3-code-markdown-fontify body 'diff))
      ((or "command_execution" "file_search" "web_search")
       (propertize body 'face 't3-code-tool-output-face))
      (_ (t3-code-markdown-fontify body 'markdown)))))

(defun t3-code-thread--item-node (item)
  "Make a section node for normalized ITEM inside a turn."
  (let* ((type (plist-get item :type))
         (id (plist-get item :id))
         (text (plist-get item :text))
         (detail (plist-get item :detail))
         (markdown (lambda (&rest parts)
                     (t3-code-markdown-fontify
                      (string-join (seq-filter (lambda (s) (and (stringp s)
                                                                (not (string-empty-p s))))
                                               parts)
                                   "\n\n")
                      'markdown)))
         (base (list :key (concat "item:" id) :item-id id)))
    (pcase type
      ("assistant_message"
       (append base (list :open t :body (funcall markdown text))))
      ("user_message"
       (append base (list :heading "You" :setext t :face 't3-code-turn-heading-face
                          :open t :body (funcall markdown text))))
      ("reasoning"
       (append base (list :heading (concat "Thinking" (t3-code-thread--status-suffix item))
                          :face 't3-code-thinking-face
                          :preview (eq t3-code-thinking-display 'preview)
                          :open (eq t3-code-thinking-display 'visible)
                          :body (funcall markdown text detail))))
      ((or "proposed_plan" "todo_list")
       (append base (list :heading (or (plist-get item :label) "Plan")
                          :face 't3-code-thread-plan-face :open t
                          :body (funcall markdown text detail))))
      ((or "approval_request" "user_input_request")
       (append base (list :heading (concat (or (plist-get item :label) "Needs attention")
                                           (t3-code-thread--status-suffix item)
                                           (when (and (equal type "user_input_request")
                                                      (t3-code-thread--pending-p item))
                                             "  (RET to answer)"))
                          :face 'warning :open t
                          :body (funcall markdown
                                         (or (t3-code-thread--questions-text item) text)))))
      ("error"
       (append base (list :heading "Error" :face 'error :open t
                          :body (funcall markdown text detail))))
      (_
       (append base (list :heading (t3-code-thread--tool-heading item)
                          :preview t :open nil
                          :body (t3-code-thread--tool-body item)))))))

(defun t3-code-thread--questions-text (item)
  "Describe the structured questions of input request ITEM, if any."
  (when-let* ((questions (plist-get item :questions)))
    (mapconcat (lambda (question)
                 (concat "**" (plist-get question :header) "** "
                         (plist-get question :question)
                         (mapconcat (lambda (option)
                                      (format "\n- %s — %s" (plist-get option :label)
                                              (plist-get option :description)))
                                    (plist-get question :options) "")))
               questions "\n\n")))

(defun t3-code-thread--run-node (run-id items)
  "Group ITEMS belonging to RUN-ID into a turn without inferring boundaries."
  (let* ((user (seq-find (lambda (item) (equal (plist-get item :type) "user_message")) items))
         (status (seq-some (lambda (item) (plist-get item :runStatus)) items))
         (ordinal (seq-some (lambda (item) (plist-get item :runOrdinal)) items))
         (rest (remq user items))
         (prompt (and user (plist-get user :text))))
    (list :key (concat "run:" run-id) :run t :run-id run-id
          :item-id (and user (plist-get user :id))
          :label (format "Turn%s · %s" (if ordinal (format " %s" ordinal) "")
                         (if prompt (t3-code-thread--preview prompt 60) "earlier prompt"))
          :heading (concat "You"
                           (if ordinal (format " · turn %s" ordinal) "")
                           (unless user " · earlier prompt not loaded")
                           (if (member status '(nil "completed")) ""
                             (format "  [%s]" status)))
          :setext t :face 't3-code-turn-heading-face
          :open (eq t3-code-thread--view 'conversation)
          :body (and prompt (t3-code-markdown-fontify prompt 'markdown))
          :children
          (when rest
            (list (list :key (concat "assistant:" run-id)
                        :heading "Assistant" :setext t
                        :face 't3-code-assistant-heading-face :open t
                        :children
                        (let ((active (or (equal run-id (plist-get (plist-get t3-code-thread--payload
                                                                              :thread)
                                                                   :activeRunId))
                                          (seq-some (lambda (item)
                                                      (eq (plist-get item :streaming) t))
                                                    rest))))
                          (cond ((not t3-code-fold-work)
                                 (mapcar #'t3-code-thread--item-node rest))
                                ((and (member status '(nil "completed")) (not active))
                                 (t3-code-thread--settled-children run-id rest))
                                (t (t3-code-thread--grouped-children rest active))))))))))

(defun t3-code-thread--work-summary (verb items)
  "Describe work ITEMS after VERB, e.g. \"Worked · 5 tool calls · 2 thoughts\"."
  (let* ((count (lambda (type) (seq-count (lambda (item) (equal (plist-get item :type) type))
                                          items)))
         (thoughts (funcall count "reasoning"))
         (messages (funcall count "assistant_message"))
         (tools (- (length items) thoughts messages))
         (failed (seq-count (lambda (item) (equal (plist-get item :status) "failed")) items))
         (part (lambda (n singular plural)
                 (when (> n 0) (format "%d %s" n (if (= n 1) singular plural))))))
    (string-join (delq nil (list verb
                                 (funcall part tools "tool call" "tool calls")
                                 (funcall part thoughts "thought" "thoughts")
                                 (funcall part messages "message" "messages")
                                 (funcall part failed "failed" "failed")))
                 " · ")))

(defun t3-code-thread--work-node (key heading items)
  "Return a closed section KEY titled HEADING holding work ITEMS."
  (list :key key :heading heading
        :face 't3-code-collapsed-indicator-face :open nil
        :children (mapcar #'t3-code-thread--item-node items)))

(defun t3-code-thread--unfoldable-p (item)
  "Whether ITEM stays visible in folded turns: an error or a pending request.
A failed tool call folds like any other, marking its group, as in T3 Code."
  (or (equal (plist-get item :type) "error")
      (t3-code-thread--pending-p item)))

(defun t3-code-thread--work-p (item)
  "Whether ITEM is activity that folds away: a tool call, reasoning or the like."
  (not (or (member (plist-get item :type)
                   '("assistant_message" "user_message" "proposed_plan" "todo_list"))
           (t3-code-thread--unfoldable-p item))))

(defun t3-code-thread--latest-activity (item)
  "Describe work ITEM as the latest activity of a turn in progress."
  (if (equal (plist-get item :type) "reasoning")
      (concat "Thinking"
              (when-let* ((text (plist-get item :text))
                          ((not (string-blank-p text))))
                (concat ": " (t3-code-thread--preview
                              (replace-regexp-in-string "[*_`#]" "" text) 60))))
    (substring-no-properties (t3-code-thread--tool-heading item))))

(defun t3-code-thread--settled-children (run-id items)
  "Return nodes for the assistant ITEMS of settled RUN-ID.
Everything but the final answer, plans, errors and pending requests is
grouped under one closed \"Worked\" section, as in T3 Code."
  (let* ((terminal (seq-find (lambda (item) (equal (plist-get item :type) "assistant_message"))
                             (reverse items)))
         (visible-p (lambda (item)
                      (or (eq item terminal)
                          (t3-code-thread--unfoldable-p item)
                          (member (plist-get item :type) '("proposed_plan" "todo_list")))))
         (work (seq-remove visible-p items)))
    (if (null work)
        (mapcar #'t3-code-thread--item-node items)
      (cons (t3-code-thread--work-node (concat "work:" run-id)
                                       (t3-code-thread--work-summary "Worked" work)
                                       work)
            (mapcar #'t3-code-thread--item-node (seq-filter visible-p items))))))

(defun t3-code-thread--grouped-children (items active)
  "Return nodes for ITEMS of an unsettled turn, folding stretches of work.
Messages, plans, errors and pending requests stay visible between closed
groups of two or more consecutive work items.  When ACTIVE, the last
group names its latest activity."
  (let* ((segments
          ;; Split into maximal runs of work and single visible items.
          (nreverse
           (seq-reduce (lambda (segments item)
                         (if (and (t3-code-thread--work-p item)
                                  (eq (car (car segments)) 'work))
                             (cons (cons 'work (append (cdr (car segments)) (list item)))
                                   (cdr segments))
                           (cons (cons (if (t3-code-thread--work-p item) 'work 'item)
                                       (list item))
                                 segments)))
                       items nil)))
         (last (car (last segments))))
    (mapcan (lambda (segment)
              (let ((members (cdr segment)))
                (if (or (eq (car segment) 'item) (null (cdr members)))
                    (mapcar #'t3-code-thread--item-node members)
                  (let ((live (and active (eq segment last))))
                    (list (t3-code-thread--work-node
                           (concat "work:" (plist-get (car members) :id))
                           (concat (t3-code-thread--work-summary
                                    (if live "Working" "Worked") members)
                                   (when live
                                     (concat " · " (t3-code-thread--latest-activity
                                                    (car (last members))))))
                           members))))))
            segments)))

(defun t3-code-thread--nodes ()
  "Build sections from loaded history and the current bounded projection."
  (let ((groups (make-hash-table :test #'equal)) order)
    (dolist (item (t3-code-thread--items))
      (let ((key (if-let* ((run (plist-get item :runId)))
                     (cons 'run run)
                   (cons 'item (plist-get item :id)))))
        (unless (gethash key groups) (push key order))
        (push item (gethash key groups))))
    (mapcar (lambda (key)
              (let ((members (nreverse (gethash key groups))))
                (if (eq (car key) 'run)
                    (t3-code-thread--run-node (cdr key) members)
                  (let ((item (car members)))
                    (if (equal (plist-get item :type) "assistant_message")
                        ;; Without run metadata, label each answer explicitly.
                        (append (t3-code-thread--item-node item)
                                (list :heading "Assistant" :setext t
                                      :face 't3-code-assistant-heading-face))
                      (t3-code-thread--item-node item))))))
            (nreverse order))))

(defun t3-code-thread--heading-row-text (node depth open foldable)
  "Return heading text for NODE at DEPTH given OPEN and FOLDABLE state."
  (let ((heading (plist-get node :heading)))
    (cond
     ((null heading) "")
     ((plist-get node :setext)
      (let ((line (concat heading (if (and foldable (not open)) " …" ""))))
        (concat "\n" (propertize line 'face (plist-get node :face)) "\n"
                ;; Share the heading's face so the rule spans its height-scaled width.
                (propertize (make-string (max 3 (string-width line)) ?=)
                            'face (if-let* ((face (plist-get node :face)))
                                      (list 'shadow face)
                                    'shadow))
                "\n")))
     (t
      (concat (make-string (* 2 (max 0 (- depth 2))) ?\s)
              (if foldable (if open "▾ " "▸ ") "  ")
              (if (plist-get node :face)
                  (propertize heading 'face (plist-get node :face))
                heading)
              "\n")))))

(defun t3-code-thread--node-rows (node &optional depth)
  "Flatten NODE into stable text rows, indented by DEPTH."
  (let* ((depth (or depth 0))
         (key (plist-get node :key))
         (children (plist-get node :children))
         (body (let ((body (plist-get node :body)))
                 (and body (not (string-empty-p body)) body)))
         (lines (and body (plist-get node :preview) (split-string body "\n")))
         (preview-split (and lines (> (length lines) (1+ t3-code-tool-preview-lines))))
         (foldable (if (plist-get node :preview) preview-split (or children body)))
         (open (t3-code-thread--open-p key (plist-get node :open)))
         (body-row (lambda (suffix text)
                     (list :key (concat key suffix) :owner key
                           :item-id (plist-get node :item-id) :text text))))
    (append
     (list (list :key key :node node :header t :foldable foldable
                 :text (t3-code-thread--heading-row-text node depth open foldable)))
     (cond
      (preview-split
       (let ((rest (nthcdr t3-code-tool-preview-lines lines)))
         (list (funcall body-row ":body"
                        (concat (string-join (seq-take lines t3-code-tool-preview-lines) "\n")
                                "\n"))
               (funcall body-row ":rest" (concat (string-join rest "\n") "\n"))
               (funcall body-row ":more"
                        (if open ""
                          (propertize (format "  … %d more lines (TAB to expand)\n"
                                              (length rest))
                                      'face 't3-code-collapsed-indicator-face))))))
      (body (list (funcall body-row ":body" (concat body "\n")))))
     (mapcan (lambda (child) (t3-code-thread--node-rows child (1+ depth))) children)
     (list (list :key (concat key ":end") :end key
                 :text (if (or children (plist-get node :run)) "" "\n"))))))

(defun t3-code-thread--attention-items ()
  "Return attention items, including any supplied outside the history window."
  (seq-uniq (append (plist-get t3-code-thread--payload :attention)
                    (seq-filter #'t3-code-thread--attention-p (t3-code-thread--items)))
            (lambda (a b) (equal (plist-get a :id) (plist-get b :id)))))

(defun t3-code-thread--history-row ()
  "Return the row offering older history, when any exists."
  (let ((payload t3-code-thread--payload))
    (when (and (eq (plist-get payload :hasOlderHistory) t)
               (not (eq t3-code-thread--older-state 'exhausted)))
      (list (list :key "history" :history t
                  :text (propertize
                         (if (eq t3-code-thread--older-state 'loading)
                             "▲ Loading older history…\n"
                           "▲ Older history available (RET or o to load)\n")
                         'face 't3-code-collapsed-indicator-face))))))

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
                  (t3-code-thread--stream-error
                   (concat "Live updates interrupted (retrying): "
                           t3-code-thread--stream-error "\n"))
                  ((eq (plist-get payload :deleted) t) "This thread was deleted.\n")))
         (attention (t3-code-thread--attention-items))
         (pending (plist-get payload :pendingRequestCount))
         (items (t3-code-thread--items)))
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
     (t3-code-thread--history-row)
     (mapcan #'t3-code-thread--node-rows (t3-code-thread--nodes))
     ;; Requests older than the history window still have an inspectable body.
     (mapcan #'t3-code-thread--node-rows
             (mapcar #'t3-code-thread--item-node
                     (seq-remove (lambda (item)
                                   (seq-find (lambda (other) (equal (plist-get item :id)
                                                                  (plist-get other :id)))
                                             items))
                                 attention)))
     (when (and payload (null items) (not notice))
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

(defun t3-code-thread--fold-bounds (node)
  "Return the (START . END) region hidden when NODE is folded, or nil."
  (let ((key (plist-get node :key)))
    (if-let* ((rest (gethash (concat key ":rest") t3-code-thread--positions)))
        rest
      (when-let* ((heading (gethash key t3-code-thread--positions))
                  (end (gethash (concat key ":end") t3-code-thread--positions)))
        (cons (cdr heading) (car end))))))

(defun t3-code-thread--install-folds (rows)
  "Install visibility overlays over foldable sections in ROWS."
  (dolist (row rows)
    (when-let* (((plist-get row :foldable))
                (node (plist-get row :node))
                (key (plist-get node :key))
                (bounds (t3-code-thread--fold-bounds node)))
      (when (< (car bounds) (cdr bounds))
        (let ((overlay (make-overlay (car bounds) (cdr bounds))))
          (overlay-put overlay 't3-code-section key)
          (overlay-put overlay 'invisible
                       (unless (t3-code-thread--open-p key (plist-get node :open)) 't3-code-fold))
          (overlay-put overlay 'isearch-open-invisible #'t3-code-thread--isearch-open)
          (overlay-put overlay 'isearch-open-invisible-temporary #'t3-code-thread--isearch-temporary)
          (push overlay t3-code-thread--fold-overlays))))))

(defvar t3-code-thread-refresh-hook nil
  "Hook run in a transcript buffer after it has been re-rendered.")

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
      (setq t3-code-thread--rendered-error t3-code-thread--stream-error
            t3-code-thread--rendered-older t3-code-thread--older-items)
      (setq imenu--index-alist nil)
      (set-buffer-modified-p nil)
      (run-hooks 't3-code-thread-refresh-hook)
      (force-mode-line-update))))

(defun t3-code-thread--item-at-point ()
  "Return the normalized item ID at point."
  (get-char-property (point) 't3-code-thread-item-id))

(defun t3-code-thread--goto-item (id)
  "Move to item ID; return non-nil when it is loaded."
  (when-let* ((bounds (or (gethash (concat "item:" id) t3-code-thread--positions)
                          ;; A turn's prompt is rendered by the turn heading.
                          (seq-some (lambda (row)
                                      (when (equal (plist-get (plist-get row :node) :item-id) id)
                                        (gethash (plist-get row :key) t3-code-thread--positions)))
                                    t3-code-thread--rows))))
    (goto-char (car bounds))
    t))

(defun t3-code-thread--row-at-point ()
  "Return the rendered row at point."
  (let ((key (get-text-property (min (point) (max (point-min) (1- (point-max))))
                                't3-code-row-key)))
    (seq-find (lambda (row) (equal key (plist-get row :key))) t3-code-thread--rows)))

(defun t3-code-thread--section-at-point ()
  "Find the innermost foldable section enclosing point.
Rows are ordered parent before child, so the last match is innermost."
  (let ((position (point)) found)
    (dolist (candidate t3-code-thread--rows found)
      (when-let* (((plist-get candidate :foldable))
                  (key (plist-get candidate :key))
                  (start (car (gethash key t3-code-thread--positions)))
                  (end (car (gethash (concat key ":end") t3-code-thread--positions)))
                  ((<= start position))
                  ((< position end)))
        (setq found key)))))

(defun t3-code-thread-run-at-point ()
  "Return the run ID of the turn containing point, if any."
  (let ((position (point)) found)
    (dolist (row t3-code-thread--rows found)
      (when-let* ((node (plist-get row :node))
                  ((plist-get node :run))
                  (start (car (gethash (plist-get row :key) t3-code-thread--positions)))
                  (end (car (gethash (concat (plist-get row :key) ":end")
                                     t3-code-thread--positions)))
                  ((<= start position))
                  ((< position (max end (1+ start)))))
        (setq found (plist-get node :run-id))))))

(defun t3-code-thread-toggle-details ()
  "Toggle the tool block, thinking block or turn at point."
  (interactive)
  (let* ((key (t3-code-thread--section-at-point))
         (row (seq-find (lambda (row) (equal key (plist-get row :key))) t3-code-thread--rows))
         (node (plist-get row :node)))
    (unless node (user-error "No foldable section at point"))
    (goto-char (car (gethash key t3-code-thread--positions)))
    (puthash key (not (t3-code-thread--open-p key (plist-get node :open))) t3-code-thread--visibility)
    (t3-code-thread--refresh)))

(defun t3-code-thread-inspect ()
  "Reveal the item behind an attention entry at point."
  (interactive)
  (if-let* ((id (get-text-property (point) 't3-code-attention)))
      (when (t3-code-thread--goto-item id)
        (let ((target (point)))
          (dolist (overlay t3-code-thread--fold-overlays)
            (when (and (<= (overlay-start overlay) target) (< target (overlay-end overlay)))
              (puthash (overlay-get overlay 't3-code-section) t t3-code-thread--visibility)))
          (puthash (concat "item:" id) t t3-code-thread--visibility)
          (t3-code-thread--refresh)
          (t3-code-thread--goto-item id)))
    (t3-code-thread-toggle-details)))

(defun t3-code-thread-cycle-view ()
  "Switch explicitly between conversation and whole-turn outline views."
  (interactive)
  (setq t3-code-thread--view (if (eq t3-code-thread--view 'conversation) 'outline 'conversation))
  (dolist (row t3-code-thread--rows)
    (let ((node (plist-get row :node)))
      (when (plist-get node :run)
        (puthash (plist-get node :key) (eq t3-code-thread--view 'conversation)
                 t3-code-thread--visibility))))
  (t3-code-thread--refresh)
  (message "T3 view: %s" t3-code-thread--view))

(defun t3-code-thread-next-turn (&optional backward)
  "Move to the next user message, or previous if BACKWARD is non-nil."
  (interactive)
  (let* ((positions (mapcar #'cdr (t3-code-thread--imenu)))
         (here (line-beginning-position))
         (target (if backward
                     (seq-find (lambda (position) (< position here)) (reverse positions))
                   (seq-find (lambda (position) (> position (line-end-position))) positions))))
    (unless target (user-error "No %s loaded turn" (if backward "previous" "next")))
    (goto-char target)))

(defun t3-code-thread-previous-turn ()
  "Move to the previous loaded user message."
  (interactive)
  (t3-code-thread-next-turn t))

(defun t3-code-thread--imenu ()
  "Return an index of loaded turns and unkeyed user messages."
  (delq nil
        (mapcar (lambda (row)
                  (let ((node (plist-get row :node)))
                    (when (or (plist-get node :run)
                              (equal (plist-get node :heading) "You"))
                      (cons (or (plist-get node :label)
                                (concat "You · " (t3-code-thread--preview
                                                  (plist-get node :body) 60)))
                            ;; Skip the blank separator line before setext headings.
                            (1+ (car (gethash (plist-get row :key)
                                              t3-code-thread--positions)))))))
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
  ;; Markup only carries these symbols when `t3-code-markdown-hide-markup'
  ;; was on at fontification time, so the spec can stay unconditional.
  (dolist (markup t3-code-markdown-invisible-markup)
    (add-to-invisibility-spec markup))
  ;; Do not replace overlays owned by an active isearch. Its end hook applies
  ;; the newest payload and refreshes any disclosure headings it opened.
  (add-hook 'isearch-mode-end-hook #'t3-code-thread--refresh nil t))

(provide 't3-code-render)
;;; t3-code-render.el ends here
