(ql:quickload :cl-ppcre)

;; (defun extract-string-inside-bracket (str)
;;   (multiple-value-bind (match regs) (cl-ppcre::scan-to-strings "<([^>]*)>" str)
;;     (aref regs 0)))


(defun translate (str)
  (let ((var-list '()))
    (let ((sin (make-string-input-stream str))
	  (sout (make-string-output-stream)))
      (handler-case
	  (loop
	    (let* ((line (read-line sin))
		   (translated ""))
	      (if (cl-ppcre:scan "\\S" line)
		  (multiple-value-bind (match regs) (cl-ppcre::scan-to-strings "vector<([^>]*)>\\s+([^;]*)" line)
		    (let ((type (aref regs 0))
			  (var (aref regs 1)))
		      (format sout "~a~%" (concatenate 'string "VkDeviceSize " var "_size = sizeof(" type ");"))
		      (push var var-list)))
		  (format sout "~%"))))
	(end-of-file (o) (format t "~a" (get-output-stream-string sout)))))

    (terpri)
    (let ((i 0))
      (dolist (var var-list)
	(format t "~a~%" (concatenate 'string "createBuffer(" var "_size,  VK_BUFFER_USAGE_TRANSFER_SRC_BIT, VK_MEMORY_PROPERTY_HOST_VISIBLE_BIT | VK_MEMORY_PROPERTY_HOST_COHERENT_BIT, stagingBuffers[" (write-to-string i) "], stagingBuffersMemory[" (write-to-string i) "]);"))
	(incf i)
	))
    
    (terpri)
    (let ((i 0))
      (dolist (var var-list)
	(format t "~a~%" (concatenate 'string "sizeof(font_info." var "[" (write-to-string i) "]) * font_info." var ".size(),"))
	(incf i))
      )

    
    )

  
  
  
    )

