(defcustom cypher-host-alist '(("local"."127.0.0.1"))
  "List of cypher endpoints
In form ( name . url-or-ip )"
  :type 'alist
  :group 'cypher)

(defcustom cypher-proto-alist '(("bolt".((proto . "bolt") (port . "7687")))
				("http".((proto . "http") (port . "7474")))
				("https".((proto . "https") (port . "7473"))))
  "Comm protocols and their ports"
  :type 'alist
  :group 'cypher)

(defun cypher-get-host-interactive ()
  "Interactively choose host and protocol/port."
  (let ( (hst (completing-read "Host: " cypher-host-alist))
	 (prt (completing-read "Protocol: " cypher-proto-alist nil nil "bolt") )
	 )
    (list
     current-prefix-arg
     (cdr (assoc hst cypher-host-alist))
     (alist-get 'proto (cdr (assoc prt cypher-proto-alist)))
     (alist-get 'port (cdr (assoc prt cypher-proto-alist))))))

(defun cypher-shell (arg host protocol port)
  "Create a new cypher-shell window."
  (interactive (cypher-get-host-interactive))
  (let (
	(proto (concat protocol "://"))
	(cypher_bufname "cypher-remote")
	(cypher_prog "cypher-shell"))
    (switch-to-buffer
     (make-comint cypher_bufname cypher_prog "/dev/null" "-a"
		  (concat proto host ":" port) ))
    ))
  
