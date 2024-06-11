(in-package :my-text)

;; (defobj a!
;;   (a nil))

;; (defobjfun change-a (a!)
;;   (setf a 1))

;; (defobjfun print-text-stdout (text!)
;;   (my-tree:move-cursor-to-index line-tree text-iter-cursor 0)
;;   (until (my-tree:is-cursor-last line-tree text-iter-cursor)
;;     ;; skip head
;;     (my-tree:move-cursor-to-next line-tree text-iter-cursor)
;;     (objlet* ((line! (my-tree:get-data text-iter-cursor)))
;;       (my-list:move-cursor-to-index char-list line-iter-cursor 0)
;;       (until (my-list:is-cursor-last char-list line-iter-cursor)
;; 	(my-list:move-cursor-to-next char-list line-iter-cursor)
;; 	(objlet* ((char! (my-list:get-data line-iter-cursor)))
;; 	  (format t "~a" char))
;; 	)
;;       (format t "~%" )
;;       )
;;     )
;; )

;; (defparameter *user* nil)
;; (defparameter *text* nil)

;; (export 'test)
;; (defmacro test ()
;;   `(progn
;;      (format t "~a~%" (create-line!))
;;      (format t "~a~%" (make-cursor!))
;;      (format t "~a~%" (create-text!))

;;      (setf *user* (make-user! :font (gethash "UbuntuMono-R" *font-table*) :connect 1))
;;      (setf *text* (create-text!))

;;      (objlet* ((user! *user*)
;; 	       (user!2 (make-user! :font (gethash "UbuntuMono-R" *font-table*) :connect 2))
;; 	       (text! *text*))
;;        (load-text text! "/home/chaewon/Desktop/chae1/github/editor/server/src/multi-cursor-list.lisp")
;;        (link-user user! text!)

;;        (add-primary-cursor user! text! 1 0)
;;        (add-primary-cursor user! text! 2 0)
;;        (get-text user! text!)

;;        (objdolist (u-user! (hash-table-values user-table))
;; 	 (print u-cursor-list))

;;        (remove-except-primary-cursor user! text!)
;;        (print cursor-list)

;;        (link-user user!2 text!)
;;        (add-primary-cursor user!2 text! 2 0)
;;        (add-primary-cursor user!2 text! 3 0)
;;        (print cursor-list2)

;;        (let ((l '()))
;; 	 (objdolist (user! (hash-table-values user-table))
;; 	   (objdolist (cursor! cursor-list)
;; 	     (objlet* ((char! (my-list:get-data line-cursor)))
;; 	       (print line-cursor)
;; 	       (push line-cursor l))))


;; 	 (print (eq (my-list:get-multi-cursor-list (car l))
;; 		    (my-list:get-multi-cursor-list (cadr l))))
;; 	 (print (eq (my-list:get-multi-cursor-list (car l))
;; 		    (my-list:get-multi-cursor-list (caddr l))))

;; 	 (print (equal (car l) (cadr l)))
;; 	 (print (equal (car l) (caddr l)))
;; 	 )
;;        )))

;; 2024.06.01

;; varibles for test
(defparameter *text* (create-text!))
(defparameter *ubuntu-mono* (get-font "UbuntuMono-R"))
(defparameter *user* (make-user! :username "user123" :curr-font *ubuntu-mono*))

(objlet* ((user! *user*))
  (defparameter *a* (create-char! #\a)))
(objlet* ((user! *user*))
  (defparameter *space* (create-char! #\ )))

;; get-glyph-info
(objlet* ((char! *a*))
  glyph)

;; get-char-advance
(objlet* ((char! *a*))
  (get-char-advance))

;; get-space-width
(objlet* ((user! *user*)
	  (char! *space*))
  (get-space-advance))

;; get-current-line
(objlet* ((text! *text*))
  (get-current-line))

;; insert-line
(objlet* ((text! *text*))
  (insert-line (create-line!)))

(objlet* ((text! *text*)
	  (line! (get-current-line)))
  (insert-char *a*))

;; load-text
(objlet* ((text! *text*))
  (objlet* ((file-path (merge-pathnames "src/multi-cursor-list.lisp" (uiop/os:getcwd))))
    (load-text text! file-path)))

(objlet* ((user! *user*))
  (init-window-lines)
  (init-render-line-variables)
  user!)

(objlet* ((user! *user*))
  (get-x-in-vk-coord 10.0))

(objlet* ((user! *user*)
	  (char! *a*))
  (update-render-line-variables))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-x-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-y-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-width-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-height-in-vk-coord))

