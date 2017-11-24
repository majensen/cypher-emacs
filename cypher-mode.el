;; info nodes:
;; Search-based Fontification
;; Faces for Font Lock

;; for a cypher-mode - syntax elements
;; reserved words
;; functions
;; cli commands

;; shamelessly ripped off from sql-mode


;; for resetting
(makunbound 'cypher-kw)
(makunbound 'cypher-fn)
(makunbound 'cypher-cmd)
(makunbound 'cypher-font-lock-keywords)

(defvar cypher-buffer nil
  "Current cypher-shell buffer.")

;; words

(defvar cypher-kw
  '("add" "all" "and" "any" "as" "asc" "ascending" "assert" "by" "call"
    "case" "commit" "constraint" "contains" "create" "csv"
    "cypher" "delete" "desc" "descending" "detach" "distinct" "do" "drop"
    "else" "end" "ends" "explain" "extract" "false"
    "fieldterminator" "filter" "for" "foreach" "from" "headers" "in"
    "index" "is" "join" "limit" "load" "mandatory" "match" "merge" "node"
    "none" "not" "null" "of" "on" "optional" "or" "order" "periodic"
    "profile" "reduce" "rel" "relationship" "remove" "require" "return"
    "scalar" "scan" "set" "single" "skip" "start" "starts" "then" "true"
    "u" "union" "unique" "unwind" "using" "when" "where" "with" "xor"
    "yield" )
  "Cypher keywords")

(defvar cypher-fn
  '("abs" "acos" "allShortestPaths" "asin" "atan" "atan2" "ceil" "coalesce"
    "collect" "cos" "cot" "count" "degrees" "e" "exists" "exp" "floor"
    "haversin" "head" "id" "keys" "labels" "last" "left" "length" "log"
    "log10" "lower" "ltrim" "max" "min" "nodes" "percentileCont"
    "percentileDisc" "pi" "radians" "rand" "range" "relationships"
    "replace" "reverse" "right"	"round" "rtrim" "shortestPath" "sign"
    "sin" "size" "split" "sqrt" "stDev" "stDevP" "substring" "sum"
    "tail" "tan" "toInt" "toString" "trim" "type" "upper")
  "Cypher built-in functions")

(defvar cypher-cmd
  '("begin" "commit" "exit" "help" "history" "param" "params" "rollback")
  "Cypher-shell commands")

(defface font-lock-type-bold
  '((t :inherit (bold font-lock-type-face)))
  "Type face bolded")
(setq font-lock-type-bold 'font-lock-type-bold) ; this is weird that I have to do this


(let* 
;; matchers
;; matcher for labels
;; matcher for relationships <-- --> -- <-[]- -[]-> -[]-
;; matcher for nodes (<token>)
    ( (kw-re (regexp-opt cypher-kw))
      (fn-re (regexp-opt cypher-fn))
      (cmd-re (regexp-opt cypher-cmd))
      (var-re "\\b[0-9A-Za-z_]+\\b")
      )
  (defconst cypher-font-lock-keywords
    `(;; cypher commands first
      (,(concat "\\b:" cmd-re "\\b") . font-lock-constant-face)
      ;; then keywords
      ,(concat "\\b" kw-re "\\b")
      (,(concat "\\b" fn-re "\\b") . font-lock-function-name-face)
      ;; relationship patterns
      ( ")\\(<?-\\(:?\\[[^]]*\\]\\)?->?\\)("
	1  font-lock-type-face)
      ;; node patterns
      (,(concat "\\W\\((" var-re "\\)\\([^()]*\\)\\()\\)")
       (1  font-lock-type-bold)
       (3 font-lock-type-bold))
      ;; then labels
      ( ,(concat ":" var-re) . font-lock-constant-face)

      )))

;; Font Lock

(defun cypher-font-lock (keywords-only)
  "Configure font-lock.

The KEYWORDS-ONLY flag is passed to font-lock to specify whether
only keywords should be highlighted and syntactic highlighting
skipped."

    ;; Setup font-lock.  Force re-parsing of `font-lock-defaults'.
    (kill-local-variable 'font-lock-set-defaults)
    (set (make-local-variable 'font-lock-defaults)
         (list 'cypher-font-lock-keywords
               keywords-only t nil))

    ;; Force font lock to reinitialize if it is already on
    ;; Otherwise, we can wait until it can be started. 
   (when (and (fboundp 'font-lock-mode)
	       (boundp 'font-lock-mode)
	       font-lock-mode)
      (font-lock-mode-internal nil)
      (font-lock-mode-internal t))

    (add-hook 'font-lock-mode-hook
	      #'(lambda ()
                  ;; Provide defaults for new font-lock faces.
                  (defvar font-lock-builtin-face
                    (if (boundp 'font-lock-preprocessor-face)
                        font-lock-preprocessor-face
                      font-lock-keyword-face))
                  (defvar font-lock-doc-face font-lock-string-face))
	      nil t))



;; Var

(defvar cypher-prompt-regexp "^\\(neo4j> \\)"
  "Prompt used to initialize `comint-prompt-regexp'.

You can change `cypher-prompt-regexp' on `cypher-interactive-mode-hook'.")

(defvar cypher-prompt-length 7
  "Prompt used to set `left-margin' in `cypher-interactive-mode'.

You can change `cypher-prompt-length' on `cypher-interactive-mode-hook'.")

(defvar cypher-prompt-cont-regexp nil
  "Prompt pattern of statement continuation prompts.")

;; Hooks

(defcustom cypher-interactive-mode-hook '()
  "Hook for customizing `cypher-interactive-mode'."
  :type 'hook
  :group 'Cypher)

(defcustom cypher-mode-hook '()
  "Hook for customizing `cypher-mode'."
  :type 'hook
  :group 'Cypher)

;; Keymap for cypher-interactive-mode.

(defvar cypher-interactive-mode-map
  (let ((map (make-sparse-keymap)))
    (if (fboundp 'set-keymap-parent)
	(set-keymap-parent map comint-mode-map); Emacs
      (if (fboundp 'set-keymap-parents)
	  (set-keymap-parents map (list comint-mode-map)))); XEmacs
    (if (fboundp 'set-keymap-name)
	(set-keymap-name map 'cypher-interactive-mode-map)); XEmacs
    ;; (define-key map (kbd "C-j") 'sql-accumulate-and-indent)
    ;; (define-key map (kbd "C-c C-w") 'sql-copy-column)
    ;; (define-key map (kbd "O") 'sql-magic-go)
    ;; (define-key map (kbd "o") 'sql-magic-go)
    ;; (define-key map (kbd ";") 'sql-magic-semicolon)
    ;; (define-key map (kbd "C-c C-l a") 'sql-list-all)
    ;; (define-key map (kbd "C-c C-l t") 'sql-list-table)
    map)
  "Mode map used for `cypher-interactive-mode'.
Based on `comint-mode-map'.")

;; Keymap for cypher-mode.

(defvar cypher-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'cypher-send-paragraph)
    (define-key map (kbd "C-c C-r") 'cypher-send-region)
    (define-key map (kbd "C-c C-s") 'cypher-send-string)
    (define-key map (kbd "C-c C-b") 'cypher-send-buffer)
    (define-key map (kbd "C-c C-n") 'cypher-send-line-and-next)
    (define-key map (kbd "C-c C-i") 'cypher-product-interactive)
    (define-key map (kbd "C-c C-l a") 'cypher-list-all)
    (define-key map (kbd "C-c C-l t") 'cypher-list-table)
    (define-key map [remap beginning-of-defun] 'cypher-beginning-of-statement)
    (define-key map [remap end-of-defun] 'cypher-end-of-statement)
    map)
  "Mode map used for `cypher-mode'.")

;; Abbreviations
(define-abbrev-table 'cypher-mode-abbrev-table
  '(("ret" "return" nil nil t)
    ("ma" "match" nil nil t)
    ("cr" "create" nil nil t))
  "Abbrev table used in `cypher-mode' and `cypher-interactive-mode'.")

;; Syntax Table

;; want to highlight
;; --, <--, -->, -[.*]-, -[.*]->, <-[.*]-
;; :<rel_type>, :<label>
;; property conditions inside nodes like (a {prop:val})
;;

(defvar cypher-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; double quotes (") delimit strings
    (modify-syntax-entry ?\" "\"" table)
    ;; colon is a prefix
    (modify-syntax-entry ?: "'" table)
    ;; Make these all punctuation
    (mapc #'(lambda (c) (modify-syntax-entry c "." table))
          (string-to-list "!#$%&+,.;<=>?@\\|"))
    table)
  "Syntax table used in `cypher-mode' and `cypher-interactive-mode'.")

;; Cypher interactive mode
(put 'cypher-interactive-mode 'mode-class 'special)
(put 'cypher-interactive-mode 'custom-mode-group 'Cypher)
(defun cypher-interactive-mode ()
  "Major mode to use cypher-shell interactively."
  (delay-mode-hooks (comint-mode))
  (setq major-mode 'cypher-interactive-mode)
  (setq mode-name "iCypher")
  (use-local-map cypher-interactive-mode-map)
  (set-syntax-table cypher-mode-syntax-table)
  ;; start font lock
  (cypher-font-lock nil)
  (setq local-abbrev-table cypher-mode-abbrev-table)
  (setq abbrev-all-caps 1)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'cypher-stop)
  ;; buffer local variables here
  (set (make-local-variable 'cypher-prompt-regexp) cypher-prompt-regexp)
  (set (make-local-variable 'cypher-prompt-length) cypher-prompt-length)
  (make-local-variable 'cypher-preoutput-hold)
  ;; run hooks
  (run-mode-hooks 'cypher-interactive-mode-hooks)
  ;; set comint
  (setq cominit-prompt-regexp cypher-prompt-cont-regexp)
  (setq left-margin cypher-prompt-length)
  ;; input sender?
  )

(defun cypher-stop (process event)
  "Called after cypher-shell is killed."
  (if (not buffer-read-only)
      (insert (format "\nProcess %s %s\n" process event))
    (message "Process %s %s" process event)))

;;; Sending the region to the Cypher buffer.

(defun cypher-send-string (str)
  "Send the string STR to the Cypher-shell process."
  (interactive "sCypher text: ")

  (let ((comint-input-sender-no-newline nil)
        (s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
    (if (sql-buffer-live-p sql-buffer)
	(progn
	  ;; Ignore the hoping around...
	  (save-excursion
	    ;; Set product context
	    (with-current-buffer sql-buffer
	      ;; Send the string (trim the trailing whitespace)
	      (sql-input-sender (get-buffer-process sql-buffer) s)

	      ;; Send a command terminator if we must
	      (if sql-send-terminator
		  (sql-send-magic-terminator sql-buffer s sql-send-terminator))

	      (message "Sent string to buffer %s" sql-buffer)))

	  ;; Display the sql buffer
	  (if sql-pop-to-buffer-after-send-region
	      (pop-to-buffer sql-buffer)
	    (display-buffer sql-buffer)))

    ;; We don't have no stinkin' sql
    (user-error "No SQL process started"))))

(defun cypher-send-region (start end)
  "Send a region to the CYPHER process."
  (interactive "r")
  (cypher-send-string (buffer-substring-no-properties start end)))

(defun cypher-send-paragraph ()
  "Send the current paragraph to the CYPHER process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (cypher-send-region start end)))

(defun cypher-send-buffer ()
  "Send the buffer contents to the CYPHER process."
  (interactive)
  (cypher-send-region (point-min) (point-max)))

(defun cypher-send-line-and-next ()
  "Send the current line to the CYPHER process and go to the next line."
  (interactive)
  (cypher-send-region (line-beginning-position 1) (line-beginning-position 2))
  (beginning-of-line 2)
  (while (forward-comment 1)))  ; skip all comments and whitespace


