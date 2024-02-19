(in-package :my-tree)

(defmacro test ()
  `(progn
     (defparameter *t* (create-multi-cursor-tree!))
     (objlet* ((cursor! (create-cursor! *t* 0))
               (cursor!-2 (create-cursor! *t* 0)))
       (push-cursor! *t* cursor!)
       (dotimes (m 5)
         (insert-data-after-cursor *t* cursor! m))
       (push-cursor! *t* cursor!-2)
       (dotimes (m 5)
         (insert-data-after-cursor *t* cursor! m))
       (dotimes (m 3)
         (delete-data-before-cursor *t* cursor!))
       (dotimes (m 3)
         (delete-data-before-cursor *t* cursor!-2))
       (repeat 4
         (move-cursor-to-prev *t* cursor!))
       (repeat 2
         (move-cursor-to-next *t* cursor!))
       (move-cursor-to-last *t* cursor!-2)
       (move-cursor-to-head *t* cursor!-2)
       (move-cursor-to-index *t* cursor!-2 3)
       (move-cursor-to-last *t* cursor!)
       (print *t*)
       (is-cursor-last *t* cursor!))

     (objlet* ((t-multi-cursor-tree! (create-multi-cursor-tree!))
	       (c-cursor! (create-cursor! t 0)))
       (time
	(dotimes (m 400000)
	  (if (eq 0 (mod m 100000))
	      (print m))
          (insert-data-after-cursor t c m)))

       (print (get-size t))

       (dotimes (m 1)
	 (time
	  (progn
	    (move-cursor-to-index t c 10)
	    (print (get-data c)))
	    )
	 (time
	  (progn
	    (move-cursor-to-index t c 300000)
	    (print (get-data c))))

	 (move-cursor-to-index t c 10)
	 (time
	  (dotimes (m 1000)
	    (move-cursor-to-next t c)
	    (print (get-data c)))))
       )
     )

  )
