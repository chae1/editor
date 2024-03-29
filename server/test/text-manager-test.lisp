(in-package :my-text)

(defmacro test ()
  `(progn
     (defparameter *tm* (create-text-manager!))
     (objlet* ((text-manager! *tm*))
       (load-text *tm* "/home/chaewon/Desktop/chae1/editor/server/src/aa-index-tree.lisp")
       (setf window-width 50)
       (setf window-height 5)
       (print-text *tm*)
       (format t "~a~%" (my-tree:get-size text))

       (init-cursors-manager! *tm* "chaewon")
       (push-primary-cursor *tm* "chaewon" 1 0)
       (push-primary-cursor *tm* "chaewon" 2 0)
       (copy-and-paste-primary-cursor *tm* "chaewon")
       (format t "~a~%" (gethash "chaewon" cursors-manager-table))

       (objlet* ((cursor! (get-primary-cursor *tm* "chaewon")))
         (move-cursor-right *tm* (get-primary-cursor *tm* "chaewon"))
         (move-cursor-left *tm* (get-primary-cursor *tm* "chaewon"))
         (move-cursor-up *tm* (get-primary-cursor *tm* "chaewon"))
         (move-cursor-down *tm* (get-primary-cursor *tm* "chaewon"))

         (dotimes (m 10)
           (insert-char-after-cursor *tm* cursor! #\*))

         (dotimes (m 5)
           (move-cursor-left *tm* cursor!))

         (print-text *tm*)
         (print-cursors-manager! *tm* "chaewon")

         (insert-new-line-after-cursor *tm* cursor!)
         (print-text *tm*)
         (print-cursors-manager! *tm* "chaewon")

         (dotimes (m 2)
           (move-cursor-right *tm* cursor!))
         (delete-char-before-cursor *tm* cursor!)
         (print-text *tm*)
         (print-cursors-manager! *tm* "chaewon")

         (move-cursor-left *tm* cursor!)
         (delete-new-line-before-cursor *tm* cursor!)
         (print-text *tm*)
         (print-cursors-manager! *tm* "chaewon")

         (reset-cursors-manager! *tm* "chaewon")
         (print-cursors-manager! *tm* "chaewon")
         )

       )))
