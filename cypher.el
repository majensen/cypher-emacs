;; cypher.el
;; Functions to run Neo4j shell programs in Emacs
;;

;; Custom variables

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
  (let ((proto (concat protocol "://"))
	(cypher_bufname (concat "cypher : " host )))
    (switch-to-buffer
     (make-comint cypher_bufname cypher-prog "/dev/null" "-a"
		  (concat proto host ":" port) ))
    (cypher-interactive-mode nil)
    ))
  
