;;; t3-code-markdown.el --- Fontify transcript text  -*- lexical-binding: t; -*-

;;; Commentary:

;; Message bodies are Markdown and file changes are unified diffs.  Rather than
;; running a major mode in the transcript buffer itself, each body is fontified
;; once in a hidden buffer and its faces are copied onto the inserted string.
;; This keeps the transcript's keyed row reconciliation unchanged.

;;; Code:

(require 'subr-x)

(defcustom t3-code-markdown-mode 'auto
  "Major mode used to fontify Markdown in transcripts.
`auto' prefers `gfm-mode' from the markdown-mode package, then the
built-in `markdown-ts-mode' when its grammar is installed.  A symbol names
a specific mode; nil shows plain text."
  :type '(choice (const :tag "Automatic" auto)
                 (const :tag "Plain text" nil)
                 (function :tag "Major mode"))
  :group 't3-code)

(defcustom t3-code-markdown-hide-markup t
  "Whether Markdown delimiters such as ** and ``` are hidden in transcripts.
\[universal-argument] \[t3-code-thread-copy] still copies the raw Markdown."
  :type 'boolean
  :group 't3-code)

(defconst t3-code-markdown-invisible-markup '(markdown-markup markdown-ts--markup)
  "Invisibility symbols the supported Markdown modes put on their markup.")

(defcustom t3-code-markdown-max-chars 60000
  "Bodies longer than this are shown without fontification."
  :type 'integer
  :group 't3-code)

(defvar markdown-fontify-code-blocks-natively)
(defvar markdown-hide-markup)
(defvar markdown-ts-hide-markup)
(defvar diff-font-lock-syntax)
(defvar diff-font-lock-prettify)

(defvar t3-code-markdown--cache (make-hash-table :test #'equal)
  "Fontified strings keyed by (MODE HIDE-MARKUP TEXT).")

(defun t3-code-markdown--resolve-mode (kind)
  "Return the major mode used to fontify text of KIND, or nil."
  (pcase kind
    ('diff 'diff-mode)
    ('markdown
     (if (eq t3-code-markdown-mode 'auto)
         (cond ((require 'markdown-mode nil t) 'gfm-mode)
               ((and (require 'markdown-ts-mode nil t)
                     (fboundp 'treesit-ready-p)
                     (treesit-ready-p 'markdown t))
                'markdown-ts-mode))
       t3-code-markdown-mode))))

(defun t3-code-markdown--buffer (mode)
  "Return a hidden buffer prepared for fontifying with MODE."
  (let ((buffer (get-buffer-create (format " *t3-fontify:%s*" mode))))
    (with-current-buffer buffer
      (unless (eq major-mode mode)
        ;; User mode hooks (spell checkers, LSP, ...) have no place here.
        (delay-mode-hooks (funcall mode))
        (setq-local markdown-fontify-code-blocks-natively t
                    diff-font-lock-syntax nil
                    diff-font-lock-prettify nil))
      (setq-local markdown-hide-markup t3-code-markdown-hide-markup
                  markdown-ts-hide-markup t3-code-markdown-hide-markup))
    buffer))

(defun t3-code-markdown--faces (begin end)
  "Return the text between BEGIN and END carrying only faces and hidden markup."
  (let ((result (buffer-substring-no-properties begin end))
        (position begin))
    (while (< position end)
      (let ((next (next-property-change position nil end))
            (face (or (get-text-property position 'face)
                      (get-text-property position 'font-lock-face)))
            (invisible (get-text-property position 'invisible)))
        (when face
          (put-text-property (- position begin) (- next begin) 'face face result))
        (when (memq invisible t3-code-markdown-invisible-markup)
          (put-text-property (- position begin) (- next begin) 'invisible invisible result))
        (setq position next)))
    result))

(defun t3-code-markdown--fontify-with (mode text)
  "Fontify TEXT using MODE and return the propertized copy."
  (with-current-buffer (t3-code-markdown--buffer mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert text)
      (font-lock-ensure)
      (t3-code-markdown--faces (point-min) (point-max)))))

(defun t3-code-markdown-fontify (text kind)
  "Return TEXT fontified as KIND, either `markdown' or `diff'.
Unavailable modes, oversized text and fontification errors yield TEXT."
  (let ((mode (and (stringp text)
                   (not (string-empty-p text))
                   (<= (length text) t3-code-markdown-max-chars)
                   (t3-code-markdown--resolve-mode kind))))
    (if (not (and mode (fboundp mode)))
        text
      (let ((key (list mode t3-code-markdown-hide-markup text)))
        (or (gethash key t3-code-markdown--cache)
            (let ((result (condition-case nil
                              (t3-code-markdown--fontify-with mode text)
                            (error text))))
              ;; Streaming bodies produce many one-off variants.
              (when (> (hash-table-count t3-code-markdown--cache) 2000)
                (clrhash t3-code-markdown--cache))
              (puthash key result t3-code-markdown--cache)))))))

(provide 't3-code-markdown)
;;; t3-code-markdown.el ends here
