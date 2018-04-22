;; cypher.el
;; Functions to run Neo4j shell programs in Emacs
;;

;; TODO: real help functions

(require 'thingatpt)
(require 'comint)
(require 'files)
(add-function :after (symbol-function 'comint-skip-prompt)
	      (lambda ()
		(if (and (bolp)
			 (looking-at (concat "\n+" comint-prompt-regexp)))
		    (goto-char (match-end 0))))
	      '((name . fix-comint-skip-prompt))
	      )


;; Custom variables

(defcustom cypher-mode-dir "/Users/jensenma/sandbox/cypher-emacs"
  "Where are cypher mode .el files?"
  :type '(file)
  :group 'Cypher)

(defcustom cypher-prog "cypher-shell"
  "Neo4j shell program name"
  :type '(string)
  :group 'Cypher)

(defcustom cypher-host-alist
  '((local
     :address "127.0.0.1"
     :url "localhost")
    )
  "List of cypher endpoints."
  :type '(alist)
  :group 'Cypher)

(defcustom neo4j-home "/usr/local/share/neo4j"
  "Neo4j home directory.
Will be used to set env $NEO4J_HOME"
  :type '(string)
  :group 'Cypher)
  

(defcustom cypher-proto-alist
  '((bolt
     :proto "bolt"
     :port "7687")
    (http
     :proto "http"
     :port "7474")
    (https
     :proto "https"
     :port "7473"))
  "Comm protocols and their ports"
  :type '(alist)
  :group 'Cypher)

(defcustom cypher-shell-hook '(cypher-get-db-labels)
  "Hook run at the end of `cypher-shell'
For customizing the shell setup."
  :type '(hook)
  :group 'Cypher)


(load (replace-regexp-in-string
       "/+" "/" (concat cypher-mode-dir "/" "cypher-mode.el")))

;; Variables

(defvar cypher-remove-cruft t
  "Remove stupid ASCII table borders and other cruft from output.
Set to nil for unfiltered output.")

(defvar cypher-statement-terminator ";"
  "Character that terminates cypher statements (semicolon)")

(defvar cypher-field-sep "\t"
  "Field separator for cypher-shell tabular output.
Only meaningful if `cypher-remove-cruft' is set.")

(defvar cypher-field-remove-quotes t
  "If t, remove double quotes surrounding field values in cypher-shell tabular output.
Only meaningful if `cypher-remove-cruft' is set.")

(defvar cypher-return-status-regexp ".*row.? available after [0-9]+ ms"
  "Regexp to identify cypher-shell return status line.")

(defvar cypher-error-status-regexp "(line[^,]+, column[^(]+(offset"
  "Regexp to identify cypher-shell error messages.")

(defvar cypher-valid-parm-name-regexp "^[a-zA-Z][a-zA-Z0-9]*$"
  "Regexp that matches a token that is a valid Cypher parameter name.")

(defvar cypher-number-regexp "^\\([+-]?[0-9]+\\)$\\|^[+-]?\\(\\(?:[0-9]+\\.[0-9]+\\)\\|\\([0-9]+\\.?\\)\\|\\(\\.?[0-9]+\\)\\)\\([eE][+-]?[0-9]+\\)$"
  "Regexp that matches things that look like numbers.")

(defvar cypher-quoted-regexp "^'.*'$\\|^\".*\"$"
  "Regexp that matches a quoted thing.")

(defvar cypher-pre-input-hook '(cypher-magic-shell-commands)
  "Hook which is run prior to sending or accumulating.
Run in `cypher-accumulate-or-send'
Default set to `cypher-magic-shell-commands', which allows one to leave out the
prefix ':' for most cypher-shell commands.")

(defvar-local cypher-buffer-process nil
  "The cypher-shell process for the buffer. Buffer-local.")

(defvar-local cypher-do-query-timeout 3
  "Timeout in seconds for `cypher-do-query' queries. Buffer-local.")


;; Functions

(defun cypher-get-host-interactive ()
  "Interactively choose host and protocol/port."
  (let* ( (hst (completing-read "Host: " cypher-host-alist))
	  (prt (completing-read "Protocol: " cypher-proto-alist nil nil "bolt") )
	  (h (assoc (intern hst) cypher-host-alist))
	  (p (assoc (intern prt) cypher-proto-alist))
	 )
    (list
     current-prefix-arg
     (or (plist-get (cdr h) :address)
	 (plist-get (cdr h) :url))
     (plist-get (cdr p) :proto)
     (plist-get (cdr p) :port)
    )))

(defvar cypher-output-bufstr ""
  "Cypher shell output buffer string.")

(defun cypher-shell-output-remove-internal-cruft (string)
  "Remove cruft from single cypher-shell output lines.
STRING should have newline stripped."
  (let ( (instr string) )
    (setq instr (replace-regexp-in-string
		 "\\s-+|$" ""
		 (replace-regexp-in-string "^|\\s-+" "" instr)))
    (setq instr (mapconcat
		 (function (lambda (x) x))
		 (split-string instr "\\s-+|\\s-+" t (if cypher-field-remove-quotes "\""))
		 cypher-field-sep))
    instr)
  )

(defun util-shift (l)
  "Shift a list.
Removes and returns the last elt of l."
  (if (not (listp l))
      (error "Arg must be a list"))
  (let* ( (r (nreverse l))
	  (ret (pop r)) )
    (setq l (nreverse r))
    ret))
	  

(defun cypher-shell-output-filter (string)
  "Filter cypher-shell output for buffer display.
Note that STRING will come in to this function having terminal escape sequences. Regexp beware."
  (setq cypher-output-bufstr (concat cypher-output-bufstr string))
  (let (
	(lines (split-string cypher-output-bufstr "[\n\r]"))
	(closed (string-match "[\n\r]$" cypher-output-bufstr)) ; ends with a CR
	(stracc "")
	line
	)
    (while (setq line (car lines))
      (if (> (length lines) 1)
	  (if cypher-remove-cruft
	      (if (string-match "\\+-+\\+" line)
		  nil
		(if (string-match cypher-return-status-regexp line)
		    (message line)
		  (setq stracc (concat stracc
				       (cypher-shell-output-remove-internal-cruft line)
				       "\n")))
		)
	    (setq stracc (concat stracc line)))
	;; last line...
	(if (string-match cypher-prompt-regexp line)
	    (progn
	      (setq stracc (concat stracc line))
	      (setq cypher-output-bufstr ""))
	  (if (not closed)
	      (setq cypher-output-bufstr line)
	    (setq cypher-output-bufstr ""))))
      (setq lines (cdr lines)))
    (setq stracc (replace-regexp-in-string "[\f]" "" stracc))
    ))


;; Interactive Functions
(defun cypher-shell (arg host protocol port)
  "Create a new cypher-shell window.
HOST url or ip
PROTOCOL bolt/http/https
PORT: appropriate Cypher-shell port
Note: cypher-shell will use cypher-get-host-interactive to more conveniently get these from custom vars.

Runs `cypher-shell-hook' before exit."
  (interactive (cypher-get-host-interactive))
  (if (not (getenv "NEO4J_HOME"))
      (setenv "NEO4J_HOME" neo4j-home))
  (cypher-comint host protocol port)
  (cypher-interactive-mode)
  (setq cypher-buffer-process (get-buffer-process (current-buffer)))
  ;;  (accept-process-output cypher-buffer-process cypher-do-query-timeout)
  (sit-for 1)
  (run-hooks 'cypher-shell-hook)
)

(defun cypher-buffer-live-p (buffer)
  "Return non-nil if the process associated with buffer is live.

BUFFER can be a buffer object or buffer name."
  (when buffer
    (setq buffer (get-buffer buffer))
    (and buffer
	 (buffer-live-p buffer)
	 (comint-check-proc buffer)
	 (with-current-buffer buffer
	   (derived-mode-p 'cypher-interactive-mode))
	 )))

(defun cypher-magic-shell-commands ()
  "Twiddles input line to make 'quit' do ':quit', etc.
Note this won't work for commands that need arguments (currently only 
:param)."
  (let (
	(line (thing-at-point 'line))
	(word (thing-at-point 'word))
	(word-bounds (bounds-of-thing-at-point 'word))
	)
    (if (string-match (concat cypher-prompt-regexp "\\s-*" word) line)
	(if (and (member (concat ":" word) cypher-cmd)
		 (not (eq word "param"))) ;; kludge - param needs arguments
	    (progn
	      (delete-region (car word-bounds) (cdr word-bounds))
	      (insert (concat ":" word)))))
    ))


(defun cypher-accumulate-or-send ()
  (interactive)
  ;; accumulate if no statement terminator (and not a cypher-shell cmd)
  (run-hooks 'cypher-pre-input-hook)
  (let ( (line (thing-at-point 'line)) )
    (if (or (not line)
	    (string-match (concat cypher-prompt-regexp "\\s-*" cypher-cmd-re ".*$") line)
	    (string-match (concat cypher-statement-terminator "\\s-*$") line)
	    (string-match (concat cypher-prompt-regexp "\\s-*$") line))
	(comint-send-input)
      (comint-accumulate)))
  )

(defun cypher-comint (host proto port)
    "Set up comint buffer for cypher-shell.
HOST url or ip
PROTO protocol (http https bolt)
PORT Cypher shell port"
    (let (( pgm (executable-find cypher-prog))
	  ( buf-name "Cypher"))
    (unless pgm
      (error "Can't find shell program %s" cypher-prog))
    (when (cypher-buffer-live-p (format "*%s*" buf-name))
      (let ((i 1))
	(while (cypher-buffer-live-p
		(format "*%s*"
			(setq buf-name (format "Cypher-%d" i))))
	  (setq i (1+ i)))))
    (setq cypher-buffer
	  (pop-to-buffer
	   (make-comint buf-name pgm "/dev/null" "-a"
			(concat (format "%s://" proto) host ":" port) )))
    (add-hook 'comint-preoutput-filter-functions 'cypher-shell-output-filter nil t)
    (add-hook 'comint-dynamic-complete-functions 'cypher-completion-at-point)
    (setq comint-use-prompt-regexp t)
    (setq comint-process-echoes t)
     ))

(defun cypher-do-query (qry &optional include-hdr)
  "Execute a cypher query directly and return response.
QRY is the query as string. The query is executed in the buffer
and the response is extracted and cleaned up. Response returned
as a string.
Pass t for INCLUDE-HDR to retrieve the output header line 
\(i.e., the column names\)"
  (if (not (boundp 'cypher-buffer-process))
      (user-error "Not a Cypher process buffer"))
  (if ( or (not cypher-buffer-process)
	   (not (process-live-p cypher-buffer-process)))
      (user-error "Buffer has no process"))
  (if (and (not (string-match ";\\s-*[\n]$" qry))
	   (not (string-match "^\\s-*:" qry)))
      (setq qry (concat qry ";\n"))
    (setq qry (concat qry "\n")))
  (let ( resp
	 (start (window-start) ) )
    (save-excursion
      (comint-goto-process-mark)
      (let ( (cur (point)) (aft (point-marker)))
	(set-marker-insertion-type aft t)
	(comint-send-string cypher-buffer-process qry)
	(accept-process-output cypher-buffer-process cypher-do-query-timeout)
	(setq resp (buffer-substring-no-properties cur aft))
	(delete-region cur aft)
	(set-marker aft nil)))
    ;; if many rows returned, the query can scroll original content off-screen,
    ;; and the deletion leaves point not visible in window. This
    ;; tries to take care of that:
    (set-window-start (selected-window) start)
    (setq resp (split-string resp "[\n\r]"))
    (if (and (not include-hdr)
	     (not (string-match "^\\s-*:" qry))) (setq resp (cdr resp)))
    (setq resp (mapconcat
		(function (lambda (x)
			    (if (string-match comint-prompt-regexp x) ""
			      (concat x "\n"))
			    ))
		resp ""))
    (if (string-match cypher-error-status-regexp resp)
	(user-error resp)
      resp)
    ))

(defun cypher-get-db-labels ()
  "Get the set of labels for the db and set `cypher-db-labels'.
Run in `cypher-shell-hook'"
  (condition-case nil
      (setq cypher-db-labels
	    (mapcar
	     (function (lambda (x) (concat ":" x)))
	     (remove ""
		     (split-string (cypher-do-query "call db.labels") "[\n|\r]"))))
    ;; or die quietly 
    (error nil))
  )
    
(defun cypher-param-query (qry parm-buffer-or-name)
  "Run a parameterized query over a list of values for each parameter.
QRY is the query as string, which may include parameters as
$<param>.  PARM-BUFFER-OR-NAME is a text buffer or buffer name
with the following format: Line 1: parameter names as <param1>
<param2> ..., tab-separated Following lines: parameter values to
be set according to Line 1.  QRY will be executed for each row of
parameter values in PARM-BUFFER, after setting the parameters.
Output is accumulated and returned as a list of \n-terminated
strings"
  (if (not (stringp qry))
      (user-error "QRY must be a string, not %s" (type-of qry)))
  (if (and (not (bufferp parm-buffer-or-name))
	   (not (get-buffer parm-buffer-or-name))
	   )
      (user-error "PARM-BUFFER must be a buffer or name , not %s" parm-buffer-or-name))
  (let ((parm-buffer (get-buffer parm-buffer-or-name))
	line parms
	(parm-cmds ())
	(result ()) )
    (save-current-buffer
      (set-buffer parm-buffer)
      (goto-char (point-min))
      (setq line (thing-at-point 'line))
      (setq parms (split-string line nil))
      (if (= (length parms) 0)
	  (user-error "No parameters in header line"))
      (mapcar (lambda (x) (if (not (string-match cypher-valid-parm-name-regexp x))
			      (user-error "'%s' is not a valid Cypher parameter name" x)))
	      parms)
      (forward-line)
      (while (not (eobp))
	(let* (
	       (ln (thing-at-point 'line))
	       (values (split-string ln "[\t]" nil "[\n]"))
	       (assign (mapcar (lambda (x) (cons x nil)) parms))
	      )
	  (if (= 0 (length values))
	      (forward-line) ;; skip
	    (dolist (x assign)
	       (let ( (v (pop values)) )
		 (if (not v)
		     (setq v "null")
		   (if (and (not (string-match cypher-number-regexp v))
			    (not (string-match cypher-quoted-regexp v)))
		       (setq v (concat "\"" v "\""))
		     t))
		 (setcdr x v)))
	    (push (mapcar
		   (lambda (x)
		     (format ":param %s %s" (car x) (cdr x)))
		   assign)
		  parm-cmds)
	    )
	  )
	(forward-line))
      )
    (setq parm-cmds (nreverse parm-cmds))
    ;; dolist instead?
    (dolist (cc parm-cmds)
      (dolist (c cc)
	(cypher-do-query c)
	(setq result (append result (split-string (cypher-do-query qry) "[\n]" t) ))
	))
    (comint-goto-process-mark)    
    result
    ))

