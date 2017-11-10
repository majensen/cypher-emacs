
(defun cypher-shell (arg host)
  "Create a new cypher-shell window.
Enter host url at prompt. Assumes bolt:// protocol and port 7687."
  (interactive "P\nMhost: ")
  (let ((proto "bolt://")
    (port "7687")
    (cypher_bufname "cypher-remote")
    (cypher_prog "cypher-shell"))
    (switch-to-buffer
     (make-comint cypher_bufname cypher_prog "/dev/null" "-a"
		  (concat proto host ":" port) ))
    ))
  
