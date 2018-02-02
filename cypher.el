;; cypher.el
;; Functions to run Neo4j shell programs in Emacs
;;

;; TODO: cypher-remove-stupid-cruft
;; TODO: real help functions


(defvar cypher-mode-dir "/Users/jensenma/Sandbox/cypher-emacs"
  "Where are cypher mode .el files?")

(load (concat cypher-mode-dir "/" "cypher-mode.el"))
;; Custom variables

(defvar cypher-remove-cruft t
  "Remove stupid ASCII table borders and other cruft from output.")

(defvar cypher-field-sep "\t"
  "Field separator for cypher-shell tabular output.
Only meaningful if `cypher-remove-cruft' is set.")

(defvar cypher-field-remove-quotes t
  "If t, remove double quotes surrounding field values in cypher-shell tabular output.
Only meaningful if `cypher-remove-cruft' is set.")

(defcustom cypher-prog "cypher-shell"
  "Neo4j shell program name"
  :type 'string
  :group 'Cypher)

(defcustom cypher-host-alist
  '((local
     :address "127.0.0.1"
     :url "localhost")
    )
  "List of cypher endpoints."
  :type 'alist
  :group 'Cypher)

(defcustom neo4j-home "/usr/local/share/neo4j"
  "Neo4j home directory.
Will be used to set env $NEO4J_HOME"
  :type 'string
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
  :type 'alist
  :group 'Cypher)

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

(defun cypher-shell-output-remove-internal-cruft (string)
  "Remove cruft from single cypher-shell output lines."
  (let ( (instr string) )
    (setq instr (mapconcat
		 (function (lambda (x) x))
		 (split-string instr "\\s-+|\\s-+" t (if cypher-field-remove-quotes "\""))
		 cypher-field-sep))
    instr)
  )

(defun cypher-shell-output-filter (proc string)
  "Filter cypher-shell output for buffer display."
  ;;  (setq string (replace-regexp-in-string "\\+-*\\+$" "" string))
  (if cypher-remove-cruft
      (setq string (mapconcat 
		    'cypher-shell-output-remove-internal-cruft
		    (split-string string "\\+?--+\\+?$" t "[\n\r]") "\n"))
    )
  (comint-output-filter proc string)
  )

;; Interactive Functions
(defun cypher-shell (arg host protocol port)
  "Create a new cypher-shell window.
host: url or ip
protocol: bolt/http/https
port: appropriate port
Note: cypher-shell will use cypher-get-host-interactive to more conveniently get these from custom vars.
"
  (interactive (cypher-get-host-interactive))
  (if (not (getenv "NEO4J_HOME"))
      (setenv "NEO4J_HOME" neo4j-home))
  (cypher-comint protocol host port)
  (cypher-interactive-mode))

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
	  
(defun cypher-comint (proto host port)
    "Set up comint buffer for cypher-shell.

PROTO protocol (http https bolt)
HOST url or ip
PORT Cypher shell port"
    (let (( pgm (executable-find cypher-prog))
	  ( buf-name "Cypher")
	  ( the-proc nil ))
    (unless pgm
      (error "Can't find shell program %s" cypher-prog))
    (when (cypher-buffer-live-p (format "*%s*" buf-name))
      (let ((i 1))
	(while (cypher-buffer-live-p
		(format "*%s*"
			(setq buf-name (format "Cypher-%d" i))))
		(setq i (1+ i))))) 
    (setq the-proc (get-buffer-process 
			 (pop-to-buffer
			  (make-comint buf-name pgm "/dev/null" "-a"
				       (concat (format "%s://" proto) host ":" port) ))))
    (set-process-filter the-proc 'cypher-shell-output-filter)
    ))
