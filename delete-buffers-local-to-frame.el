;; TODO: When deleting the first frame, unintended frames like *scratch* is also deleted.
;; TODO: Don't delete minibuffers, they are probably deleted automatically.


(defvar server-clients)

(defun can/delete-buffers-local-to-frame (frame)
  "Delete buffers that were exclusively seen by frame."
  (when (>= (seq-length (frame-list)) 2)
    (let* (;; buffers seen by the frame
	   (frame-buffers (frame-parameter frame 'buffer-list))
	   
	   ;; buffers opened through the client arguments
	   ;; Client buffers are automatically killed and client is
	   ;; notified at frame deletion.
	   (client-buffers
	    (let ((proc (frame-parameter frame 'client)))
	      (if (and proc (memq proc server-clients))
		  (process-get proc 'buffers)
		nil)))
	   
	   ;; buffers seen exclusively by other frames
	   (other-frames-buffers
	    (cl-reduce #'append
		       (mapcar (lambda (f)
				 (frame-parameter f 'buffer-list))
			       (seq-filter (lambda (f) (not (eq f frame)))
					   (frame-list)))))
	   
	   ;; buffers that won't be deleted
	   (non-local-buffers (append client-buffers other-frames-buffers))
	   
	   ;; buffers that will be deleted
	   (buffers-to-be-deleted
	    (seq-filter (lambda (x) (not (memq x non-local-buffers)))
			frame-buffers)))

      (can/delete-buffers buffers-to-be-deleted))))

(defun can/delete-buffers (buffers-to-be-deleted)
  "Delete buffers in buffers-to-be-deleted. Save beforehand if
necessary. Prompt when saving."
  (save-some-buffers nil
		     (lambda ()
		       (and (buffer-file-name) ;; file visiting buffer
			    (memq (current-buffer) buffers-to-be-deleted))))
  
  (let ((killed-buffer-names
	 (seq-filter #'identity
		     (mapcar (lambda (b)
			       (let ((name (buffer-name b))
				     (res (kill-buffer b)))
				 (if res name nil)))
			     buffers-to-be-deleted))))
    
    (message "Killed buffers `%s'." killed-buffer-names)
    killed-buffer-names))

(add-hook 'delete-frame-functions #'can/delete-buffers-local-to-frame)
