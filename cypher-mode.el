;; for a cypher-mode - syntax elements
;; reserved words
;; functions
;; cli commands

;; shamelessly ripped off from sql-mode

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
    ;; C-style comments /**/ (see elisp manual "Syntax Flags"))
    (modify-syntax-entry ?/ ". 14" table)
    (modify-syntax-entry ?* ". 23" table)
    ;; double-dash starts comments
    (modify-syntax-entry ?- ". 12b" table)
    ;; newline and formfeed end comments
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\f "> b" table)
    ;; single quotes (') delimit strings
    (modify-syntax-entry ?' "\"" table)
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "." table)
    ;; Make these all punctuation
    (mapc #'(lambda (c) (modify-syntax-entry c "." table))
          (string-to-list "!#$%&+,.:;<=>?@\\|"))
    table)
  "Syntax table used in `cypher-mode' and `cypher-interactive-mode'.")


(defvar cypher-mode-reserved-font-lock-keywords)
(setq cypher-mode-reserved-font-lock-keywords nil)
(defvar cypher-mode-function-font-lock-keywords)
(setq cypher-mode-function-font-lock-keywords nil)
(defvar cypher-mode-command-font-lock-keywords)
(setq cypher-mode-command-font-lock-keywords nil)

(defun cypher-font-lock-keywords-builder (face boundaries &rest keywords)
  "Generation of regexp matching any one of KEYWORDS."
  
  (let ((bdy (or boundaries '("\\b" . "\\b")))
	kwd)
    ;; pre-processing of keywords here
    ;; (dolist (k keywords) ... )
    (setq kwd keywords)
    ;; Create a properly formed font-lock-keywords item
    (cons (concat (car bdy)
		  (regexp-opt kwd t)
		  (cdr bdy))
	  face)))

(setq cypher-mode-reserved-font-lock-keywords
      (list
       ;; Cypher reserved words
       (cypher-font-lock-keywords-builder
	'font-lock-keyword-face nil
 ;;; keyword list
	"add" "all" "and" "any" "as" "asc" "ascending" "assert" "by" "call"
	"case" "commit" "constraint" "contains" "count" "create" "csv"
	"cypher" "delete" "desc" "descending" "detach" "distinct" "do" "drop"
	"else" "end" "ends" "exists" "explain" "extract" "false"
	"fieldterminator" "filter" "for" "foreach" "from" "headers" "in"
	"index" "is" "join" "limit" "load" "mandatory" "match" "merge" "node"
	"none" "not" "null" "of" "on" "optional" "or" "order" "periodic"
	"profile" "reduce" "rel" "relationship" "remove" "require" "return"
	"scalar" "scan" "set" "single" "skip" "start" "starts" "then" "true"
	"u" "union" "unique" "unwind" "using" "when" "where" "with" "xor"
	"yield" )))

(setq cypher-mode-function-font-lock-keywords
      (list
       ;; Cypher reserved words
       (cypher-font-lock-keywords-builder
	'font-lock-builtin-face nil
 ;;; function list
	"abs" "acos" "allShortestPaths" "asin" "atan" "atan2" "ceil" "coalesce" "collect" "cos" "cot"
	"count" "degrees" "e" "exists" "exp" "floor" "haversin" "head" "id" "keys" "labels" "last"
	"left" "length" "log" "log10" "lower" "ltrim" "max" "min" "nodes" "percentileCont"
	"percentileDisc" "pi" "radians" "rand" "range" "relationships" "replace" "reverse" "right"
	"round" "rtrim" "shortestPath" "sign" "sin" "size" "split" "sqrt" "stDev" "stDevP" "substring"
	"sum" "tail" "tan" "toInt" "toString" "trim" "type" "upper")))

(setq cypher-mode-command-font-lock-keywords
      (list
       ;; cypher-shell commands
       (cypher-font-lock-keywords-builder
	'font-lock-keyword-face nil
 ;;; command list
	":begin" ":commit" ":exit" ":help" ":history" ":param" ":params" ":rollback")))



(defun cypher-interactive-mode ()
  "Major mode for Neo4j cypher-shell CLI."
  (setq major-mode 'cypher-interactive-mode)
  (setq mode-name "Cypher")
  (use-local-map cypher-interactive-mode-map)
  (set-syntax-table cypher-mode-syntax-table)
  (setq local-abbrev-table cypher-mode-abbrev-table)
  (setq abbrev-all-caps 1)

  ;; buffer local status variables

  ;; run hooks
  (run-mode-hooks 'cypher-interactive-mode-hook)
  ;;
  (setq comint-prompt-regexp
	(if cypher-prompt-cont-regexp
            (concat "\\(" cypher-prompt-regexp
                    "\\|" cypher-prompt-cont-regexp "\\)")
          cypher-prompt-regexp))
  (setq left-margin cypher-prompt-length)
  
)
