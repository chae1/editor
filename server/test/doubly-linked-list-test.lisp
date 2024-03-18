(in-package :my-list)

(defmacro test ()
  `(progn
     (defparameter *l* (create-multi-cursor-list!))
     (let ((cursor (create-cursor! *l* 0)))
       (push-cursor! *l* cursor)

       (format t "~a~%~%" *l*)
       (dotimes (m 5)
         (insert-data-after-cursor *l* cursor m))
       (format t "~a~%~%" *l*)

       (push-cursor! *l* (create-cursor! *l* 0))

       (format t "~a~%~%" *l*)
       (dotimes (m 5)
         (insert-data-after-cursor *l* cursor m))
       (format t "~a~%~%" *l*)
       (dotimes (m 3)
         (delete-data-before-cursor *l* cursor))
       (format t "~a~%~%" *l*)
       (move-cursor-to-next *l* cursor)
       (move-cursor-to-next *l* cursor)
       (move-cursor-to-next *l* cursor)
       (format t "~a~%~%" (is-cursor-last *l* cursor))

       (move-cursor-to-index *l* cursor 4)
       (format t "~a~%~%" *l*)

       (objlet* ((l-new-multi-cursor-list! (split-list-after-cursor *l* cursor)))
         (format t "~a~%~%" *l*)
         (format t "~s~%~%" l-new)
         (merge-list-to-prev *l* l-new)
         (format t "~a~%~%" *l*)
         (format t "~s~%~%" l-new)
         )

       (objlet* ((l-multi-cursor-list! (create-multi-cursor-list!))
		 (c-cursor! (create-cursor! l 0)))
	 (time
	  (dotimes (m 400000)
	    (if (eq 0 (mod m 100000))
		(print m))
            (insert-data-after-cursor l c m)))

	 (print (get-size l))

	 (dotimes (m 1)
	   (time
	    (progn
	      (move-cursor-to-index l c 10)
	      (print (get-data c))))
	   
	   (time
	    (progn
	      (move-cursor-to-index l c 300000)
	      (print (get-data c))
	      ))

	   (move-cursor-to-index l c 10)
	   (time
	    (dotimes (m 1000)
	      (move-cursor-to-next l c)
	      (print (get-data c))))
	   )
	 )
    
       )))


(defmacro test2 ()
  `(progn
     (defparameter *l* (create-multi-cursor-list!))
     (let ((cursor (create-cursor! *l* 0)))

       (let ((i 1))
	 (defun insert-num ()
	   (incf i)
	   (insert-data-after-cursor *l* cursor i)))

       (push-cursor! *l* cursor)

       (dotimes (m 5)
         (insert-num))

       (push-cursor! *l* (create-cursor! *l* 0))

       (dotimes (m 5)
         (insert-num))

       (dotimes (m 3)
         (delete-data-before-cursor *l* cursor))

       (move-cursor-to-next *l* cursor)
       (move-cursor-to-next *l* cursor)
       (move-cursor-to-next *l* cursor)

       (dotimes (m 5)
         (insert-num))

       (repeat 5 (move-cursor-to-prev *l* cursor))
       (print *l*)
       (terpri)
       
       (objlet* ((new-l (split-list-after-cursor *l* cursor)))
	 (print *l*)
	 (print new-l)
	 )
       nil
       )))
