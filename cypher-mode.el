;; info nodes:
;; Search-based Fontification
;; Faces for Font Lock

;; for a cypher-mode - syntax elements
;; reserved words
;; functions
;; cli commands

;; shamelessly ripped off from sql-mode

(require 'thingatpt)

;; for resetting
(makunbound 'cypher-kw)
(makunbound 'cypher-fn)
(makunbound 'cypher-cmd)
(makunbound 'cypher-font-lock-keywords)
(makunbound 'cypher-prompt-regexp)

;; Custom vars

(defcustom cypher-pop-to-buffer-after-send-region nil
  "When non-nil, pop to the buffer SQL statements are sent to.

After a call to `cypher-send-string', `cypher-send-region',
`cypher-send-paragraph' or `cypher-send-buffer', the window is split
and the iCypher buffer is shown.  If this variable is not nil, that
buffer's window will be selected by calling `pop-to-buffer'.  If
this variable is nil, that buffer is shown using
`display-buffer'."
  :type 'boolean
  :group 'Cypher)

;; Var

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
  '(":begin" ":commit" ":exit" ":help" ":history" ":param" ":params" ":rollback" ":quit")
  "Cypher-shell commands")

(defvar cypher-buffer nil
  "Latest cypher-shell buffer.")

(defvar-local cypher-db-labels nil
  "Node labels in the db. 
Set in `cypher-shell'.")

(defvar cypher-kw-re (regexp-opt cypher-kw)
  "Cypher keyword regexp."
  )

(defvar cypher-fn-re (regexp-opt cypher-fn)
  "Cypher function regexp."
  )

(defvar cypher-cmd-re (regexp-opt cypher-cmd)
  "Cypher command regexp."
  )

(defvar cypher-var-re "\\b[0-9A-Za-z_]+\\b"
  "Cypher variable regexp"
  )

(defface font-lock-type-bold
  '((t :inherit (bold font-lock-type-face)))
  "Type face bolded")
(setq font-lock-type-bold 'font-lock-type-bold) ; this is weird that I have to do this


;; matchers
;; matcher for labels
;; matcher for relationships <-- --> -- <-[]- -[]-> -[]-
;; matcher for nodes (<token>)
(defconst cypher-font-lock-keywords
  `(;; cypher commands first
    (,(concat "\\b" cypher-cmd-re "\\b") . font-lock-constant-face)
    ;; then keywords
    ,(concat "\\b" cypher-kw-re "\\b")
    (,(concat "\\b" cypher-fn-re "\\b") . font-lock-function-name-face)
    ;; relationship patterns
    ( ")\\(<?-\\(:?\\[[^]]*\\]\\)?->?\\)("
      1  font-lock-type-face)
    ;; node patterns
    (,(concat "\\W\\((" cypher-var-re "\\)\\([^()]*\\)\\()\\)")
     (1  font-lock-type-bold)
     (3 font-lock-type-bold))
    ;; then labels
    ( ,(concat ":" cypher-var-re) . font-lock-constant-face)
    
    ))

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


;; Completion
(defun cypher-get-bol ()
  "Get position of beginning of command line."
  (let ((bocl (save-excursion (comint-bol) (point))))
    bocl))

(defun cypher-bolp ()
  "True if point at beginning of command line."
    (= (cypher-get-bol) (point)))

(defun cypher-completion-at-point ()
  "Function to provide cypher keyword completion.
Added to `comint-dynamic-complete-functions' hook"
  ;;          (START END COLLECTION . PROPS)
  (let (
	(bounds (bounds-of-thing-at-point 'word))
	(context (buffer-substring (cypher-get-bol) (point)))
	comp-list
	)
    ;; include a prefix colon
    (if bounds
	(let ( (c (char-before (car bounds))) )
	  (if (= c ?:)
	      (setcar bounds (1- (car bounds))))
	  )
      (if (= (char-before (point)) ?:)
	  (setq bounds (cons (1- (point)) (point)))
	(setq bounds (cons (point) (point)))
	))
    (setq comp-list (list (car bounds) (cdr bounds) 
			  (if (= (car bounds) (point-max))
			      ()
			    (if (= (length context) 0)
				cypher-kw
			      (if (string-match "^\\s-*:" context)
				  cypher-cmd
				(if (= (char-after (car bounds)) ?:)
				    cypher-db-labels
				  (if (and (char-after (cdr bounds))
					   (= (char-after (cdr bounds)) ?\())
				      cypher-fn
				    cypher-kw ))))) . nil ))
    comp-list
    ))


;; Var

;; note that preoutput, there are terminal escape codes coming from
;; cypher-shell that can screw up regexps in cypher-shell-output-filter

(defvar cypher-prompt-regexp "\\(^\\(?:\e.*?\\)*neo4j[>|#] \\(?:\e.*?\\)*\\)"
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
    (define-key map (kbd "RET") 'cypher-accumulate-or-send)
    (define-key map (kbd "TAB") 'completion-at-point)
    (define-key map (kbd "C-a") 'comint-bol-or-process-mark)
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
    ;; (define-key map (kbd "C-c C-i") 'cypher-product-interactive)
    (define-key map [remap beginning-of-defun] 'cypher-beginning-of-statement)
    (define-key map [remap end-of-defun] 'cypher-end-of-statement)
    map)
  "Mode map used for `cypher-mode'.")

;; Abbreviations

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

(put 'cypher-interactive-mode 'custom-mode-group 'Cypher)

(define-derived-mode cypher-interactive-mode comint-mode "iCypher"
  "Major mode to use cypher-shell interactively."
  :syntax-table cypher-mode-syntax-table
  :group 'Cypher
  :abbrev-table nil
  ;; start font lock
  (cypher-font-lock nil)
  (set-process-sentinel (get-buffer-process (current-buffer)) 'cypher-stop)
  ;; buffer local variables here
  (set (make-local-variable 'cypher-prompt-regexp) cypher-prompt-regexp)
  (set (make-local-variable 'cypher-prompt-length) cypher-prompt-length)
  (make-local-variable 'cypher-output-bufstr)
  ;; set comint
  (setq comint-prompt-regexp cypher-prompt-regexp)
  (setq left-margin cypher-prompt-length)
  ;; input sender?
  )

(defun cypher-stop (process event)
  "Called after cypher-shell is killed."
  (if (eq cypher-buffer (current-buffer))
      (setq cypher-buffer nil)) ;; should actually look for another buffer
  (if (not buffer-read-only)
      (insert (format "\nProcess %s %s\n" process event))
    (message "Process %s %s" process event)))

;;; Sending the region to the Cypher buffer.

(defun cypher-send-string (str)
  "Send the string STR to the Cypher-shell process."
  (interactive "sCypher statement: ")

  (let ((comint-input-sender-no-newline nil)
        (s (replace-regexp-in-string "[[:space:]\n\r]+\\'" "" str)))
    (if (cypher-buffer-live-p cypher-buffer)
	(progn
	  (save-excursion
	    (with-current-buffer cypher-buffer
	      ;; Send the string (trim the trailing whitespace)
	      (comint-simple-send cypher-buffer-process s)
	      (message "Sent string to buffer %s" sql-buffer)))

	  ;; Display the sql buffer
	  (if cypher-pop-to-buffer-after-send-region
	      (pop-to-buffer cypher-buffer)
	    (display-buffer cypher-buffer)))
    (user-error "No Cypher-shell process started"))))

(defun cypher-send-region (start end)
  "Send a region to the Cypher-shell process."
  (interactive "r")
  (cypher-send-string (buffer-substring-no-properties start end)))

(defun cypher-send-paragraph ()
  "Send the current paragraph to the Cypher-shell process."
  (interactive)
  (let ((start (save-excursion
		 (backward-paragraph)
		 (point)))
	(end (save-excursion
	       (forward-paragraph)
	       (point))))
    (cypher-send-region start end)))

(defun cypher-send-buffer ()
  "Send the buffer contents to the Cypher-shell process."
  (interactive)
  (cypher-send-region (point-min) (point-max)))

(defun cypher-send-line-and-next ()
  "Send the current line to the Cypher-shell process and go to the next line."
  (interactive)
  (cypher-send-region (line-beginning-position 1) (line-beginning-position 2))
  (beginning-of-line 2)
  (while (forward-comment 1)))  ; skip all comments and whitespace


