(ql:quickload "alexandria")
(ql:quickload "lparallel")
(ql:quickload "ieee-floats")
(ql:quickload "cl-ppcre")

(defpackage new-my-text
  (:nicknames :new-my-text)
  (:use :common-lisp :my-utils :defobj :alexandria :lparallel :ieee-floats :cl-ppcre))

(in-package :new-my-text)

"
text : tree
  line : list ^lic-user0
    char1
    char2 ^cic-user0
    ..
  line : list
    char1
    char2
    ..

chaewon -> ((tc,            (tc,         ..
              (lc lc lc ..))  (lc lc ..))  ..)
"

(defobj char!
  (char nil)
  (type nil)
  (char-color #(0.0 0.0 0.0)))

(defobj line!
  (char-list nil :type my-list:multi-cursor-list!)
  (line-iter-cursor nil :type my-list:cursor!)
  (word-start-cursor nil :type my-list:cursor!)
  (word-end-cursor nil :type my-list:cursor!)
  (word-width 0.0 :type single-float))

(defmethod print-object ((l line!) out)
  (print-unreadable-object (l out :type t)
    (format out "~a" (line!-char-list l))))

(defmacro create-line! ()
  `(let ((char-list (my-list:create-multi-cursor-list!)))
     (make-line! :char-list char-list
                 :line-iter-cursor (my-list:get-default-cursor char-list)
                 :word-start-cursor (my-list:create-cursor! char-list 0)
                 :word-end-cursor (my-list:create-cursor! char-list 0))))

(defparameter *font-table* (make-hash-table :test 'equal))

(defobj font!
  (fontname nil :type (or null string))
  (em 0.0 :type float)
  (x-advance-table (make-hash-table) :type hash-table)
  (default-x-advance 0.0 :type float)
  (y-advance 0.0 :type float))

(defun get-font (font-name)
  (or (gethash font-name *font-table*)
      (setf (gethash font-name *font-table*) (make-font!))))

(defparameter *action-table* (make-hash-table :test 'equal))

(defun add-action (token func)
  (setf (gethash token *action-table*) func))

(add-action "glyph"
	    (lambda (fin)
	      (uiop:first-char (read-line fin))))

(add-action "advance-width"
	    (lambda (fin)
	      (read-from-string (read-line fin))))

(add-action "em"
	    (lambda (fin)
	      (read-from-string (read-line fin))))

(add-action "line-height"
	    (lambda (fin)
	      (read-from-string (read-line fin))))

(defobjfun parse-file (fpath)
  (let ((results '()))
    (with-open-file (fin fpath)
      (let ((line nil))
	(handler-case
	    (while (setf line (read-line fin))
	      (let ((token (first (split "\\s+" line))))
		(if-let (action (gethash token *action-table*))
		  (push (funcall action fin) results))))
 	  (end-of-file (o) nil))))
    (setf results (nreverse results))))

(defobjfun load-font (path)
  (when (pathname-directory path)
    (objlet* ((dir-name (car (last (pathname-directory path))))
	      (font! (get-font dir-name)))
      (setf fontname dir-name)
      (dolist (fpath (uiop:directory-files path))
	(if (equal (pathname-name fpath) "font-info")
	    (let ((results (parse-file fpath)))
	      (setf em (car results))
	      (setf y-advance (cadr results)))
	    (let* ((results (parse-file fpath))
		   (char (car results))
		   (x-advance (cadr results)))
	      (setf (gethash char x-advance-table) x-advance))))

      (setf default-x-advance (reduce #'min (hash-table-values x-advance-table)))
      (setf (gethash #\  x-advance-table) default-x-advance))))

(load-font "/home/chaewon/Desktop/chae1/github/editor/font/txt/UbuntuMono-R/")

(defobjfun get-font (fontname)
  (gethash fontname *font-table*))

(export 'cursor!)
(defobj cursor!
  (text-cursor nil :type (or null my-tree:cursor!))
  (line-cursor nil :type (or null my-list:cursor!))
  (default-pos-in-line 0 :type integer))

(defmethod print-object ((c cursor!) out)
  (print-unreadable-object (c out :type t)
    (format out "line-index : ~a, char-index : ~a"
            (if (cursor!-text-cursor c) (my-tree:index-of (cursor!-text-cursor c)))
            (if (cursor!-line-cursor c) (my-list:index-of (cursor!-line-cursor c))))))

(export 'make-user!)
(defobj user!
  (connect nil)
  (socket-stream-in nil)
  (state nil)
  (username nil)
  (osstream (make-string-output-stream) :type sb-impl::string-output-stream)
  (font nil)
  (font-size 10)
  (window-width 400.0 :type single-float)
  (window-height 600.0 :type single-float)
  (width-vacancy 0.0 :type single-float)
  (height-vacancy 0.0 :type single-float)
  (render-start-line-index 1 :type integer)
  (render-start-line-subindex 1 :type integer)
  (text nil :type (or null text!))
  (primary-cursor nil :type (or null cursor!))
  (cursor-list nil :type (or null cons))
  (cursor-color #(0.0 0.2 0.0)))

(declaim (inline get-x-advance))
(defobjfun get-x-advance (user! char!)
  (objlet* ((font! font)
	    (x-advance (gethash char x-advance-table)))
    (setf x-advance (if x-advance x-advance 0))
    (* (/ x-advance em) font-size)))

(declaim (inline get-default-x-advance))
(defobjfun get-default-x-advance (user!)
  (objlet* ((font! font))
    (* (/ default-x-advance em) font-size)))

(declaim (inline get-y-advance))
(defobjfun get-y-advance (user!)
  (objlet* ((font! font))
    (* (/ y-advance em) font-size)))

(defobj text!
  (file-path nil)
  (line-tree nil :type (or null my-tree:multi-cursor-tree!))
  (text-iter-cursor nil :type (or null my-tree:cursor!))
  (user-table (make-hash-table :test 'eq) :type hash-table))

(export 'create-text!)
(defobjfun create-text! ()
  (objlet* ((line-tree (my-tree:create-multi-cursor-tree!))
	    (text! (make-text! :line-tree line-tree
			       :text-iter-cursor (my-tree:get-default-cursor line-tree)))
	    (line! (create-line!)))
    (my-tree:insert-data-after-cursor line-tree text-iter-cursor line!)
    text!))

(defmethod print-object ((obj text!) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (text!-file-path obj))))

(defobjfun load-text (text! path)
  (with-open-file (fin path :if-does-not-exist nil)
    (if fin
        (progn
          (setf file-path path)
          (objlet* ((line! (create-line!)))
            (my-tree:insert-data-after-cursor line-tree text-iter-cursor line!)
	    (do ((char (read-char fin nil) (read-char fin nil)))
                ((eq char nil))
              (cond ((eq char #\Newline)
		     (progn
                       (setq line! (create-line!))
                       (my-tree:insert-data-after-cursor line-tree text-iter-cursor line!)))
                    (t (my-list:insert-data-after-cursor char-list line-iter-cursor (make-char! :char char)))))))
        (format t "file not exists.~%"))))

(defobjfun print-cursor (user! text! line!)
  (objdolist (user! (hash-table-values user-table))
    (objdolist (cursor! cursor-list)      
      (when (and (eq (my-tree:index-of text-iter-cursor)
		     (my-tree:index-of text-cursor))
		 (eq (my-list:index-of word-start-cursor)
		     (my-list:index-of line-cursor)))

	(format osstream "cursor ~a " username)
	
	(objlet* ((x-advance (get-default-x-advance user!))
		  (y-advance (get-y-advance user!)))   
	  (format osstream "~a ~a ~a ~a "
		  (+ -1 (* (/ (- window-width width-vacancy) window-width) 2))
		  (+ -1 (* (/ (- window-height height-vacancy) window-height) 2))
		  (* (/ x-advance window-width) 2)
		  (* (/ y-advance window-height) 2)))
	
	(format osstream "~a ~a ~a~%"
		(aref cursor-color 0)
		(aref cursor-color 1)
		(aref cursor-color 2))))))

(defobjfun print-new-line (user!)
  (setf width-vacancy window-width)
  (objlet* ((y-advance (get-y-advance user!)))
    ;; (format osstream "~%")
    (decf height-vacancy y-advance)))

(defobjfun print-char (user! text! line! char!)
  (objlet* ((x-advance (get-x-advance user! char!))
	    (y-advance (get-y-advance user!)))   
    (format osstream "key ~a ~a ~a ~a ~a"
	    char
	    (+ -1 (* (/ (- window-width width-vacancy) window-width) 2))
	    (+ -1 (* (/ (- window-height height-vacancy) window-height) 2))
	    (* (/ x-advance window-width) 2)
	    (* (/ y-advance window-height) 2))
   
    (format osstream "~%")
    
    (decf width-vacancy x-advance)
    (decf word-width x-advance)))

(defobjfun print-curr-word-in-line (user! text! line!)
  (until (my-list:same-indices? word-start-cursor word-end-cursor)
    (my-list:move-cursor-to-next char-list word-start-cursor)
    (objlet* ((char! (my-list:get-data word-start-cursor)))
      (print-char user! text! line! char!)
      (print-cursor user! text! line!))))

(defobjfun print-curr-word (user! text! line!)
  (cond ((>= width-vacancy word-width)
         (print-curr-word-in-line user! text! line!))

        ((< width-vacancy word-width)
	 (objlet* ((y-advance (get-y-advance user!)))
	   (cond ((>= window-width word-width)
                  (cond ((>= height-vacancy y-advance)
			 (print-new-line user!)
			 (print-curr-word-in-line user! text! line!))

			((< height-vacancy y-advance)
			 (return))))

		 ((< window-width word-width)
		  (until (or (my-list:same-indices? word-start-cursor word-end-cursor)
			     (< height-vacancy y-advance))
                    (my-list:move-cursor-to-next char-list word-start-cursor)
		    (objlet* ((char! (my-list:get-data word-start-cursor))
			      (x-advance (get-x-advance user! char!)))
		      (if (< width-vacancy x-advance)
			  (progn
			    (my-list:move-cursor-to-prev char-list word-start-cursor)
			    (print-new-line user!))
			  (progn
			    (print-char user! text! line! char!)
			    (print-cursor user! text! line!)))))))))))

(defobjfun get-text (user! text!)
  (setf width-vacancy window-width)
  (setf height-vacancy window-height)

  (my-tree:move-cursor-to-index line-tree text-iter-cursor (1- render-start-line-index)) 
  (until (or (my-tree:is-cursor-last line-tree text-iter-cursor)
	     (< height-vacancy (get-y-advance user!)))
    ;; skip head
    (my-tree:move-cursor-to-next line-tree text-iter-cursor)
    ;; (format t "height vacancy : ~a width vacancy : ~a~%~a~%" height-vacancy width-vacancy text-iter-cursor)

    (objlet* ((line! (my-tree:get-data text-iter-cursor)))      
      (my-list:move-cursor-to-head char-list word-start-cursor)
      (my-list:move-cursor-to-head char-list word-end-cursor)
      (setf word-width 0.0)

      (print-cursor user! text! line!)

      (until (my-list:is-cursor-last char-list word-end-cursor)
	(my-list:move-cursor-to-next char-list word-end-cursor)
	(objlet* ((char! (my-list:get-data word-end-cursor))
		  (x-advance (get-x-advance user! char!)))
	  
	  (incf word-width x-advance)

	  (cond ((eq char #\ )
                 ;; move cursor to the character before #\ 
                 (my-list:move-cursor-to-prev char-list word-end-cursor)
                 (decf word-width x-advance)
                 (print-curr-word user! text! line!)
                 ;; move cursor to  #\ 
		 (my-list:move-cursor-to-next char-list word-start-cursor)
                 (print-char user! text! line! char!)
		 (print-cursor user! text! line!)
                 (my-list:move-cursor-to-next char-list word-end-cursor))
		
                ((my-list:is-cursor-last char-list word-end-cursor)
                 (print-curr-word user! text! line!)))))
      
      (print-new-line user!)))

  (get-output-stream-string osstream))

"
text : tree
  line : list ^lic-user0
    char1
    char2 ^cic-user0
    ..
  line : list
    char1
    char2
    ..

chaewon -> ((tc,            (tc,         ..
              (lc lc lc ..))  (lc lc ..))  ..)
"

(defobjfun add-primary-cursor (user! text! line-index char-index)
  (let ((text-size (my-tree:get-size line-tree)))
    (if (or (> line-index text-size) (<= line-index 0))
        (format t "my-text:push-cursor! line-index out of bound.~%")
        (progn
          (my-tree:move-cursor-to-index line-tree text-iter-cursor line-index)
          (objlet* ((line! (my-tree:get-data text-iter-cursor))
                    (line-size (my-list:get-size char-list)))
            (if (or (> char-index line-size) (< char-index 0))
                (format t "my-text:push-cursor! char-index out of bound.~%")
                (objlet* ((cursor! (make-cursor!)))
                  (setf text-cursor (my-tree:create-cursor! line-tree line-index))
                  (setf line-cursor (my-list:create-cursor! char-list char-index))

                  (my-tree:push-cursor! line-tree text-cursor)
                  (my-list:push-cursor! char-list line-cursor)

		  (setf primary-cursor cursor!)
		  (push cursor! cursor-list))))))))

(defobjfun remove-except-primary-cursor (user! text!)
  (objdolist (cursor! cursor-list)
    (if (not-eq cursor! primary-cursor)
	(progn
	  (my-tree:remove-cursor! line-tree text-cursor)
	  (objlet* ((line! (my-tree:get-data text-cursor)))
            (my-list:remove-cursor! char-list line-cursor)))))
  (setf cursor-list (list primary-cursor)))

(defobjfun remove-cursors (user! text!)
  (objdolist (cursor! cursor-list)
    (objlet* ((line! (my-tree:get-data text-cursor)))
      (my-tree:remove-cursor! line-tree text-cursor)
      (my-list:remove-cursor! char-list line-cursor))))

(defobjfun link-user (user! text!)
  (setf (gethash connect user-table) user!)
  (add-primary-cursor user! text! 1 0)
  (setf text text!))

(defobjfun unlink-user (user! text!)
  (remhash connect user-table)
  (remove-cursors user! text!)
  (setf text nil))

(defobjfun remove-user (user! text!)
  (remhash connect user-table)
  (objdolist (cursor! cursor-list)
    (my-tree:remove-cursor! line-tree text-cursor)
    (objlet* ((line! (my-tree:get-data text-cursor)))
      (my-list:remove-cursor! char-list line-cursor)))
  (setf cursor-list nil))

(defobjfun copy-and-paste-primary-cursor (user! text!)
  (objlet* ((cursor! primary-cursor))
    (let ((line-index (my-tree:index-of text-cursor))
          (char-index (my-list:index-of line-cursor)))
      (add-primary-cursor user! text! line-index char-index))))

(defobjfun move-cursor-right (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (if (my-list:is-cursor-last char-list line-cursor)
        (if (my-tree:is-cursor-last line-tree text-cursor)
            nil
            (progn
	      (objlet* ((my-list:multi-cursor-list! char-list))
		(remove-el line-cursor cursors))
	      (my-tree:move-cursor-to-next line-tree text-cursor)
              (setq line! (my-tree:get-data text-cursor))
              (my-list:move-cursor-to-head char-list line-cursor)
	      (objlet* ((my-list:multi-cursor-list! char-list))
		(sorted-push line-cursor cursors #'my-list:cursor!-<=))))
        (progn
          (my-list:move-cursor-to-next char-list line-cursor)))
    (setf default-pos-in-line (my-list:index-of line-cursor)))
  cursor!)

(defobjfun move-cursor-left (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (if (= (my-list:index-of line-cursor) 0)
        (if (= (my-tree:index-of text-cursor) 1)
            nil
            (progn
	      (objlet* ((my-list:multi-cursor-list! char-list))
		(remove-el line-cursor cursors))
              (my-tree:move-cursor-to-prev line-tree text-cursor)
              (setq line! (my-tree:get-data text-cursor))
              (my-list:move-cursor-to-last char-list line-cursor)
	      (objlet* ((my-list:multi-cursor-list! char-list))
		(sorted-push line-cursor cursors #'my-list:cursor!-<=))))
        (progn
          (my-list:move-cursor-to-prev char-list line-cursor)))
    (setf default-pos-in-line (my-list:index-of line-cursor)))
  cursor!)

(defobjfun move-cursor-up (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (if (= (my-tree:index-of text-cursor) 1)
        nil
        (progn
	  (objlet* ((my-list:multi-cursor-list! char-list))
	    (remove-el line-cursor cursors))
          (my-tree:move-cursor-to-prev line-tree text-cursor)
          (setq line! (my-tree:get-data text-cursor))
          (my-list:move-cursor-to-head char-list line-cursor)
          (my-list:move-cursor-to-index char-list line-cursor (clamp default-pos-in-line 0 (my-list:get-size char-list)))
	  (objlet* ((my-list:multi-cursor-list! char-list))
	    (sorted-push line-cursor cursors #'my-list:cursor!-<=)))))
  cursor!)

(defobjfun move-cursor-down (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (if (my-tree:is-cursor-last line-tree text-cursor)
        nil
        (progn
	  (objlet* ((my-list:multi-cursor-list! char-list))
	    (remove-el line-cursor cursors))
          (my-tree:move-cursor-to-next line-tree text-cursor)
          (setq line! (my-tree:get-data text-cursor))
          (my-list:move-cursor-to-head char-list line-cursor)
          (my-list:move-cursor-to-index char-list line-cursor (clamp default-pos-in-line 0 (my-list:get-size char-list)))
	  (objlet* ((my-list:multi-cursor-list! char-list))
	    (sorted-push line-cursor cursors #'my-list:cursor!-<=)))))
  cursor!)

(defobjfun insert-char-after-cursor (text! cursor! char!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (my-list:insert-data-after-cursor char-list line-cursor char!)))

(defobjfun insert-new-line-after-cursor (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor))
            (line-to-insert (my-list:split-list-after-cursor char-list line-cursor))
            (new-line! (make-line! :char-list line-to-insert
                                   :line-iter-cursor (my-list:get-default-cursor line-to-insert)
                                   :word-start-cursor (my-list:create-cursor! line-to-insert 0)
                                   :word-end-cursor (my-list:create-cursor! line-to-insert 0))))
    (my-tree:insert-data-after-cursor line-tree text-cursor new-line!)))

(defobjfun delete-char-before-cursor (text! cursor!)
  (objlet* ((line! (my-tree:get-data text-cursor)))
    (my-list:delete-data-at-cursor char-list line-cursor)))

(defobjfun delete-line-before-cursor (text! cursor!)
  (if (>= (my-tree:index-of text-cursor) 2)
      (objlet* ((curr-line! (my-tree:get-data text-cursor)))
        (my-tree:delete-data-at-cursor line-tree text-cursor)
        (objlet* ((prev-line! (my-tree:get-data text-cursor)))
          (my-list:merge-list-to-prev prev-char-list curr-char-list)))))

(defobjfun insert-char (user! text! char)
  (objdolist (cursor! cursor-list)
    (insert-char-after-cursor text! cursor! (make-char! :char char))))

(defobjfun insert-new-line (user! text!)
  (objdolist (cursor! cursor-list)
    (insert-new-line-after-cursor text! cursor!)))

(defobjfun backspace (user! text!)
  (objdolist (cursor! cursor-list)
    (if (eq (my-list:index-of line-cursor) 0)
	(delete-line-before-cursor text! cursor!)
	(delete-char-before-cursor text! cursor!))))

(defobjfun move-right (user! text!)
  (objdolist (cursor! cursor-list)
    (move-cursor-right text! cursor!)))

(defobjfun move-left (user! text!)
  (objdolist (cursor! cursor-list)
    (move-cursor-left text! cursor!)))

(defobjfun move-up (user! text!)
  (objdolist (cursor! cursor-list)
    (move-cursor-up text! cursor!)))

(defobjfun move-down (user! text!)
  (objdolist (cursor! cursor-list)
    (move-cursor-down text! cursor!)))
