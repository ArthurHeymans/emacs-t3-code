;;; t3-code-markdown.el --- Fontify transcript text  -*- lexical-binding: t; -*-

;;; Commentary:

;; Message bodies are Markdown and file changes are unified diffs.  Rather than
;; running a major mode in the transcript buffer itself, each body is fontified
;; once in a hidden buffer and its faces are copied onto the inserted string.
;; This keeps the transcript's keyed row reconciliation unchanged.

;;; Code:

(require 'cl-lib)
(require 'seq)
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

(defvar t3-code-markdown--auto-mode 'unknown
  "Markdown mode chosen by `auto', or nil when none is available.
Probing is cached: a failing `require' searches the whole `load-path', and
fontifying runs for every item of every streaming update.")

(defun t3-code-markdown--resolve-mode (kind)
  "Return the major mode used to fontify text of KIND, or nil."
  (pcase kind
    ('diff 'diff-mode)
    ('markdown
     (if (not (eq t3-code-markdown-mode 'auto))
         t3-code-markdown-mode
       (when (eq t3-code-markdown--auto-mode 'unknown)
         (setq t3-code-markdown--auto-mode
               (cond ((require 'markdown-mode nil t) 'gfm-mode)
                     ((and (require 'markdown-ts-mode nil t)
                           (fboundp 'treesit-ready-p)
                           (treesit-ready-p 'markdown t))
                      'markdown-ts-mode))))
       t3-code-markdown--auto-mode))))

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
        (when (and t3-code-markdown-hide-markup
                   (memq invisible t3-code-markdown-invisible-markup))
          (put-text-property (- position begin) (- next begin) 'invisible invisible result))
        (setq position next)))
    result))

(defun t3-code-markdown--safe-mode-p (mode)
  "Whether MODE is a major mode Emacs already uses for some file name.
Code fences name their language, and markdown-mode calls `LANG-mode' to
highlight them.  Transcript text comes from an agent, so a fence such as
```server must not enable an arbitrary function."
  (and mode (symbolp mode) (fboundp mode)
       (seq-some (lambda (entry)
                   (eq mode (if (consp (cdr entry)) (cadr entry) (cdr entry))))
                 auto-mode-alist)))

(declare-function markdown-get-lang-mode "markdown-mode" (lang))

(defun t3-code-markdown--fontify-with (mode text)
  "Fontify TEXT using MODE and return the propertized copy."
  (with-current-buffer (t3-code-markdown--buffer mode)
    (let ((inhibit-read-only t)
          (lang-mode (and (fboundp 'markdown-get-lang-mode)
                          (symbol-function 'markdown-get-lang-mode))))
      (erase-buffer)
      (insert text)
      (cl-letf (((symbol-function 'markdown-get-lang-mode)
                 (lambda (lang)
                   (let ((candidate (and lang-mode (funcall lang-mode lang))))
                     (and (t3-code-markdown--safe-mode-p candidate) candidate)))))
        ;; Language modes fontify code blocks in buffers of their own; their
        ;; hooks (LSP, linters, spell checkers) must not run there.  The
        ;; variable is buffer-local, so set its default for those buffers.
        (cl-letf (((default-value 'delay-mode-hooks) t))
          (font-lock-ensure)))
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
