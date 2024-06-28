(ql:quickload "alexandria")
(ql:quickload "lparallel")
(ql:quickload "cl-ppcre")
;; (ql:quickload "ieee-floats")

(defpackage my-text
  (:nicknames :my-text)
  (:use :common-lisp :my-utils :defobj :alexandria :lparallel :cl-ppcre))

(in-package :my-text)

(export `(user! text! cursor! line! font! char! glyph! bounding-box!))
(declaim (special user! text! cursor! line! font! char! glyph! boundin1g-box!))

;; font

(defobj bounding-box!
  ;; normalized scale 0 ~ 1
  (x-min 0.0 :type single-float)
  (y-min 0.0 :type single-float)
  (x-max 0.0 :type single-float)
  (y-max 0.0 :type single-float))

(defobj glyph!
  ;; normalized scale 0 ~ 1
  ;; for space
  (advance-width 0.0 :type single-float)
  (advance-height 0.0 :type single-float)
  ;; for other characeters
  (bounding-box nil :type (or null bounding-box!)))

(defun create-glyph! ()
  (make-glyph! :bounding-box (make-bounding-box!)))

(defobj font!
  (fontname nil :type (or null string))
  (curr-glyph nil :type (or null glyph!))
  (glyph-table (make-hash-table) :type hash-table))

(defparameter *font-table* (make-hash-table :test 'equal))

(export `get-font)
(defun get-font (font-name)
  (or (gethash font-name *font-table*)
      (setf (gethash font-name *font-table*) (make-font!))))

(defparameter *func-table* (make-hash-table :test 'equal))

(defun add-parse-func (token func)
  (setf (gethash token *func-table*) func))

(defun get-parse-func (token)
  (gethash token *func-table*))

(add-parse-func "glyph"
		(objlambda (font! fin)
		  (let ((char (uiop:first-char (read-line fin))))
		    (setf curr-glyph (create-glyph!))
		    (setf (gethash char glyph-table) curr-glyph))))

(defun read-from-nth-str (n str-list)
  (read-from-string (nth n str-list)))

;; parsing glyph should be preceded

(add-parse-func "advance-width"
		(objlambda (font! fin)
		  (objlet* ((glyph! curr-glyph))
		    (setf advance-width (read-from-string (read-line fin))))))

(add-parse-func "advance-height"
		(objlambda (font! fin)
		  (objlet* ((glyph! curr-glyph))
		    (setf advance-height (read-from-string (read-line fin))))))

(add-parse-func "bounding-box"
		(objlambda (font! fin)
		  (objlet* ((glyph! curr-glyph)
			    (bounding-box! bounding-box))
		    (let ((str-list (split "\\s+" (read-line fin))))
		      (setf x-min (read-from-nth-str 0 str-list))
		      (setf y-min (read-from-nth-str 1 str-list))
		      (setf x-max (read-from-nth-str 2 str-list))
		      (setf y-max (read-from-nth-str 3 str-list))))))

(add-parse-func "em"
		(objlambda (font! fin)
		  (setf em (read-from-string (read-line fin)))))

(defmacro while-let ((var val) &body body)
  `(let ((,var nil))
    (while (setf ,var ,val)
      ,@body)))

(defmacro loop-line ((line fin file-path) &body body)
  `(with-open-file (,fin ,file-path :if-does-not-exist nil)
     (handler-case
	 (while-let (,line (read-line ,fin))
	   ,@body)
       (end-of-file (o) nil))))

(defun get-first-word-in-str (line)
  (first (split "\\s+" line)))

(defun parse-file (font! file-path)
  (loop-line (line fin file-path)
    (let* ((token (get-first-word-in-str line)))
      (if-let ((parse-func (get-parse-func token)))
	(funcall parse-func font! fin)))))

(defun load-font (path)
  (format t "load font path ~s~%" path)
  (if (pathname-directory path)
      (objlet* ((dirname (car (last (pathname-directory path))))
		(font! (get-font dirname)))
	(setf fontname dirname)
	(objdolist (file-path (uiop:directory-files path))
	  (parse-file font! file-path))
	(setf (gethash #\Tab glyph-table) (gethash #\  glyph-table)))
      (break (with-output-to-string (out)
	       (format out "Path ~s is not directory" path)))))

(defun get-ubuntumono-r-path ()
  (let* ((cwd (uiop/os:getcwd)))
    (merge-pathnames "../font/txt/UbuntuMono-R/" cwd)))

(defun load-ubuntumono-r ()
  (load-font (get-ubuntumono-r-path)))

(load-ubuntumono-r)

;; text

"
text : tree
  line : list ^tc-user0
    char1
    char2 ^lc-user0
    ..
  line : list
    char1
    char2
    ..

cursor-list of user!
  ((tc, (tc, (tc,
     lc), lc), lc), ...
"

(defobj char!
  (char nil :type (or null character))
  (char-color #(0.0 0.0 0.0) :type (simple-vector 3))
  (fontname nil :type (or null string))
  ;; normalized scale
  (glyph nil :type (or null glyph!))
  ;; normalized scale to pixel scale
  (font-size 10.0 :type single-float)
  ;; pixel scale
  (x-line-gap 0.0 :type single-float)
  (y-line-gap 0.0 :type single-float))

(defobj line!
  (char-list nil :type my-list:multi-cursor-list!)
  (line-iter-cursor nil :type my-list:cursor!)
 
  ;; line variables (pixel scale)
  (word-start-cursor nil :type my-list:cursor!)
  (word-end-cursor nil :type my-list:cursor!)
  (curr-word-width 0.0 :type single-float)
  (render-line-start-cursor nil :type my-list:cursor!)
  (render-line-end-cursor nil :type my-list:cursor!))

(defmethod print-object ((l line!) out)
  (print-unreadable-object (l out :type t)
    (format out "~a" (line!-char-list l))))

(defmacro create-line! ()
  `(let ((char-list (my-list:create-multi-cursor-list!)))
     (make-line! :char-list char-list
                 :line-iter-cursor (my-list:get-default-cursor char-list)
		 :word-start-cursor (my-list:create-cursor! char-list 0)
		 :word-end-cursor (my-list:create-cursor! char-list 0)
		 :render-line-start-cursor (my-list:create-cursor! char-list 0)
		 :render-line-end-cursor (my-list:create-cursor! char-list 0))))

(with-objs (line!)
  (defun init-line-variables ()
    (my-list:move-cursor-to-head char-list word-start-cursor)
    (my-list:move-cursor-to-head char-list word-end-cursor)
    (setf curr-word-width 0.0)
    (my-list:move-cursor-to-head char-list render-line-start-cursor)
    (my-list:move-cursor-to-head char-list render-line-end-cursor)))

(defobj text!
  (file-path nil)
  (line-tree nil :type (or null my-tree:multi-cursor-tree!))
  (text-iter-cursor nil :type (or null my-tree:cursor!))
  (user-list nil :type (or null cons)))

(defmethod print-object ((obj text!) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (text!-file-path obj))))
(export 'create-text!)

(defun create-text! ()
  (objlet* ((line-tree (my-tree:create-multi-cursor-tree!))
	    (text! (make-text! :line-tree line-tree
			       :text-iter-cursor (my-tree:get-default-cursor line-tree))))
    (insert-line-in-text (create-line!))
    text!))

(export 'cursor!)
(defobj cursor!
  (text-cursor nil :type (or null my-tree:cursor!))
  (line-cursor nil :type (or null my-list:cursor!))
  (default-pos-in-line 0 :type integer)
  (cursor-color #(1.0 0.0 0.0) :type (simple-vector 3)))

(defmethod print-object ((c cursor!) out)
  (print-unreadable-object (c out :type t)
    (format out "line-index : ~a, char-index : ~a"
            (if (cursor!-text-cursor c) (my-tree:index-of (cursor!-text-cursor c)))
            (if (cursor!-line-cursor c) (my-list:index-of (cursor!-line-cursor c))))))

(with-objs (char!)
  (defun get-char-advance ()
    (objlet* ((glyph! glyph)
              (char-bounding-box! bounding-box))
      (* (+ x-line-gap (- char-x-max char-x-min)) font-size))))

(with-objs (char!)
  (defun get-space-advance ()
    (objlet* ((glyph! glyph))
      (cond ((eq char #\ )
	     (* advance-width font-size))
	    ((eq char #\Tab)
	     (* (4 * advance-width) font-size))))))

(defmacro loop-char-in-file ((char fin file-path) &body body)
  `(with-open-file (,fin ,file-path :if-does-not-exist nil)
     (handler-case
	 (while-let (,char (read-char ,fin))
	   ,@body)
       (end-of-file (o) nil))))

(with-objs (text!)
  (defun insert-line-in-text (line!)
    (my-tree:insert-data-after-cursor line-tree text-iter-cursor line!)))

(with-objs (line!)
  (defun insert-char-in-line (char!)
    (my-list:insert-data-after-cursor char-list line-iter-cursor char!)))

(export '*default-font*)
(defparameter *default-font* (get-font "UbuntuMono-R"))
(defparameter *default-space-glyph*
  (objlet* ((font! *default-font*))
    (gethash #\  glyph-table)))

(export 'get-default-font)
(defun get-default-font ()
  *default-font*)

(export 'make-user!)
(defobj user!
  (username nil)
  (connect nil)
  (socket-stream-in nil)
  (key-input-state nil)
  (osstream (make-string-output-stream) :type sb-impl::string-output-stream)
  (text nil :type (or null text!))

  ;; char variables
  (curr-font (get-default-font) :type font!)
  ;; normalized scale to pixel scale
  (curr-font-size 10.0 :type single-float)

  ;; pixel scale
  (curr-x-line-gap 0.05 :type single-float)
  (curr-y-line-gap 0.1 :type single-float)
  (window-width 400.0 :type single-float)
  (window-height 600.0 :type single-float)

  ;; window variables (pixel scale)
  (x-line 0.0 :type single-float)
  (y-line 0.0 :type single-float)

  ;; render line height variables (pixel scale)
  (curr-render-line-base 0.0 :type single-float)
  (curr-render-line-height 0.0 :type single-float)
  
  (render-start-line-index 1 :type integer)
  (render-start-line-row-index 1 :type integer)

  (curr-render-line-row-index 1 :type integer)

  (print-started nil :type boolean)
  
  (primary-cursor nil :type (or null cursor!))
  (cursor-list nil :type (or null cons)))

(with-objs (user!)
  (defobjfun load-text (text! file-path-in)
    (setf file-path file-path-in)  
    (loop-char-in-file (char fin file-path-in)
      (if (eq char #\Newline)
          (insert-line-in-text (create-line!))
          (objlet* ((line! (my-tree:get-data text-iter-cursor)))
	    (insert-char-in-line (create-char! char)))))
    text!))

(with-objs (user!)
  (defun create-char! (char)
    (objlet* ((font! curr-font))
      (make-char! :char char
		  :fontname fontname
		  :glyph (gethash char glyph-table)
		  :font-size curr-font-size
		  :x-line-gap curr-x-line-gap
		  :y-line-gap curr-y-line-gap))))

(with-objs (user!)
  (defun init-print-started ()
    (if (= curr-render-line-row-index render-start-line-row-index)
	(setf print-started t)
	(setf print-started nil))))

(with-objs (user!)
  (defun update-print-started ()
    (if (= curr-render-line-row-index render-start-line-row-index)
	(setf print-started t))))

(with-objs (user!)
  (defun init-window-variables ()
    (setf x-line 0.0)
    (setf y-line 0.0)
    (objlet* ((text! text))
      (my-tree:move-cursor-to-index line-tree text-iter-cursor (1- render-start-line-index)))
    (setf curr-render-line-row-index 1)
    (init-print-started)))

(with-objs (user!)
  (defun init-render-line-height-variables ()
    (setf curr-render-line-base 0.0)
    (setf curr-render-line-height 0.0)))

(defmacro update-variable (var cand compare)
  (let ((cand-bind (symbol-append 'cand- (gensym))))
    `(let ((,cand-bind ,cand))
       (if (funcall ,compare ,cand-bind ,var)
	   (setf ,var ,cand-bind)))))

(with-objs (user! char!)
  (defun update-render-line-height-variables ()
    (objlet* ((glyph! glyph)
	      (char-bounding-box! bounding-box))
      (update-variable curr-render-line-base (* (+ y-line-gap char-y-max) font-size) #'>)
      (update-variable curr-render-line-height (+ curr-render-line-base (* (- char-y-min) font-size)) #'>))))

;; pixel scale to vk scale
(with-objs (user!)
  (defun get-x-in-vk-coord (x)
    (+ -1 (* (/ x window-width) 2))))

(with-objs (user!)
  (defun get-y-in-vk-coord (y)
    (+ -1 (* (/ y window-height) 2))))

(with-objs (user!)
  (defun get-width-in-vk-coord (width)
    (* (/ width window-width) 2)))

(with-objs (user!)
  (defun get-height-in-vk-coord (height)
    (* (/ height window-height) 2)))

(with-objs (user! char!)
  (defun get-char-x-in-vk-coord ()
    (get-x-in-vk-coord (+ x-line (* x-line-gap font-size)))))

(with-objs (user! char!)
  (defun get-char-y-in-vk-coord ()
    (objlet* ((glyph! glyph)
	      (char-bounding-box! bounding-box))
      (get-y-in-vk-coord (+ y-line curr-render-line-base (* (- char-y-min) font-size))))))

(with-objs (user! char!)
  (defun get-char-width-in-vk-coord ()
    (objlet* ((glyph! glyph)
	      (char-bounding-box! bounding-box))
      (get-width-in-vk-coord (* (- char-x-max char-x-min) font-size)))))

(with-objs (user! char!)
  (defun get-char-height-in-vk-coord ()
    (objlet* ((glyph! glyph)
	      (char-bounding-box! bounding-box))
      (get-height-in-vk-coord (* (- char-y-max char-y-min) font-size)))))

(with-objs (user!)
  (defun get-cursor-x-in-vk-coord ()
    (get-x-in-vk-coord x-line)))

(with-objs (user!)
  (defun get-cursor-y-in-vk-coord ()
    (get-y-in-vk-coord (+ y-line curr-render-line-height))))

(with-objs (user!)
  (defun get-cursor-width-in-vk-coord ()
    (get-width-in-vk-coord (objlet* ((font! curr-font)
				     (glyph! (gethash #\  glyph-table)))
			     (* (/ advance-width 5) curr-font-size)))))

(with-objs (user!)
  (defun get-cursor-height-in-vk-coord ()
    (get-height-in-vk-coord curr-render-line-height)))

(with-objs (user!)
  (defun get-space-x-in-vk-coord ()
    (get-x-in-vk-coord x-line)))

(with-objs (user!)
  (defun get-space-y-in-vk-coord ()
    (get-y-in-vk-coord (+ y-line curr-render-line-height))))

(with-objs (user! char!)
  (defun get-space-width-in-vk-coord ()
    (get-width-in-vk-coord (get-space-advance))))

(with-objs (user!)
  (defun get-space-height-in-vk-coord ()
    (get-height-in-vk-coord curr-render-line-height)))

(with-objs (text!)
  (defobjmacro loop-cursor ((user! cursor!) &body body)
    `(objdolist (,user! user-list)
       (objdolist (,cursor! ,cursor-list)
         ,@body))))

(with-objs (text! line! cursor!)
  (defun print-cursor-cond ()
    (and (eq (my-tree:index-of text-iter-cursor)
             (my-tree:index-of text-cursor))
         (eq (my-list:index-of render-line-start-cursor)
             (my-list:index-of line-cursor)))))

;; text-iter-cursor, word-start-cursor, and word-end-cursor in text! and line! will be iterated for text iteration

(with-objs (user! text! line! char!)
  (defun print-cursors ()
    (loop-cursor (cursor-user! cursor!)
      ;; (format t "~a ~a ~a ~a ~a~%" (my-tree:index-of text-iter-cursor) (my-tree:index-of text-cursor) (my-list:index-of render-line-start-cursor) (my-list:index-of line-cursor) (print-cursor-cond))
      
      (if (print-cursor-cond)
	  (format osstream "cursor ~a ~a ~a ~a ~a ~a ~a ~a~%"
		  cursor-username
		  (get-cursor-x-in-vk-coord)
		  (get-cursor-y-in-vk-coord)
		  (get-cursor-width-in-vk-coord)
		  (get-cursor-height-in-vk-coord)
		  (aref cursor-color 0)
		  (aref cursor-color 1)
		  (aref cursor-color 2))))))

(with-objs (user! char!)
  (defun print-char ()
    (format osstream "char ~a ~a ~a ~a ~a~%"
	    char
	    (get-char-x-in-vk-coord)
	    (get-char-y-in-vk-coord)
	    (get-char-width-in-vk-coord)
	    (get-char-height-in-vk-coord))))

(with-objs (user! char!)
  (defun print-space ()
    (format osstream "space ~a ~a ~a ~a~%"
	    (get-space-x-in-vk-coord)
	    (get-space-y-in-vk-coord)
	    (get-space-width-in-vk-coord)
	    (get-space-height-in-vk-coord))))

(defparameter *space-chars* '(#\  #\Tab))

(with-objs (char!)
  (defun is-space-char ()
    (member char *space-chars*)))

(define-condition y-line-exceeded-window-height (error)
  ())

(with-objs (line!)
  (defobjmacro loop-char-in-render-line ((char!) &body body)
    `(loop-until (my-list:same-indices? render-line-start-cursor render-line-end-cursor)
       (my-list:move-cursor-to-next char-list render-line-start-cursor)
       (objlet* ((,char! (my-list:get-data render-line-start-cursor)))
         ,@body))))

(with-objs (line!)
  (defobjmacro loop-char-in-render-line-preserve-cursors ((char!) &body body)
    (let ((start-i (symbol-append 'start-i (gensym)))
	  (end-i (symbol-append 'end-i (gensym))))
      `(let ((,start-i (my-list:index-of render-line-start-cursor))
	     (,end-i (my-list:index-of render-line-end-cursor)))
	 (loop-char-in-render-line (,char!)
	   ,@body)
	 (my-list:move-cursor-to-index char-list render-line-start-cursor ,start-i)
	 (my-list:move-cursor-to-index char-list render-line-end-cursor ,end-i)))))

(with-objs (user! text! line!)
  (defun print-curr-non-empty-render-line ()
    "Print characters between render-line-start-cursor and render-line-end-cursor."
    (setf x-line 0.0)
    (init-render-line-height-variables)

    (loop-char-in-render-line-preserve-cursors (char!)
      (update-render-line-height-variables))

    ;; (format t "print row ~a y-line ~a~%" curr-render-line-row-index y-line)
    ;; (format t "base ~a line-height ~a~%" curr-render-line-base curr-render-line-height)

    (print-cursors)
    (loop-char-in-render-line (char!)
      (if (eq char #\ )
	  (progn
	    ;; (print-space)
	    )
	  (print-char))

      (incf x-line (if (is-space-char)
		       (get-space-advance)
		       (get-char-advance)))
      (print-cursors))

    (setf x-line 0.0)
    (incf y-line curr-render-line-height)
    (init-render-line-height-variables)

    (if (>= y-line window-height)
	(error 'y-line-exceeded-window-height))))

(with-objs (user! text! line!)
  (defun print-and-prepare-new-render-line ()
    (if print-started
	(print-curr-non-empty-render-line))

    (incf curr-render-line-row-index)
    (if (not print-started)
	(update-print-started))))

(with-objs (user!)
  (defun print-and-prepare-new-line ()
    (if print-started
	(print-curr-non-empty-render-line))
    
    (setf curr-render-line-row-index 1)))

(with-objs (user! char!)
  (defun curr-char-overflows-render-line ()
    (> (+ x-line (get-char-advance)) window-width)))

(with-objs (line! char!)
  (defun increase-word-width ()
    (incf curr-word-width (get-char-advance))))

(with-objs (line! char!)
  (defun decrease-word-width ()
    (decf curr-word-width (get-char-advance))))

(with-objs (user! text! line! char!)
  (defun advance-char ()
    (if (curr-char-overflows-render-line)
        (print-and-prepare-new-render-line))
    (incf x-line (get-char-advance))
    (decrease-word-width)
    (my-list:move-cursor-to-next char-list render-line-end-cursor)))

(with-objs (user! text! line! char!)
  (defun advance-space ()
    (incf x-line (get-space-advance))
    (my-list:move-cursor-to-next char-list render-line-end-cursor)))

(with-objs (user! line!)
  (defun curr-word-overflows-render-line ()
    (> (+ x-line curr-word-width) window-width)))

(with-objs (user! line!)
  (defun curr-word-fits-in-new-render-line ()
    (<= curr-word-width window-width)))

(with-objs (line!)
  ;; loop word-start-cursor until it matches to word-end-cursor while binding char
  (defobjmacro loop-char-in-curr-word ((char!) &body body)
    `(loop-until (my-list:same-indices? word-start-cursor word-end-cursor)
       (my-list:move-cursor-to-next char-list word-start-cursor)       
       (objlet* ((,char! (my-list:get-data word-start-cursor)))
         ,@body))))

(with-objs (user! text! line!)
  (defun advance-word ()
    ;; (format t "advance-word~%")
    (if (and (curr-word-overflows-render-line)
             (curr-word-fits-in-new-render-line))
        (print-and-prepare-new-render-line))
    (loop-char-in-curr-word (char!)
      (advance-char))))

(with-objs (line!)
  (defobjmacro loop-char-in-line ((char!) &body body)
    `(progn
       (init-line-variables)
       (loop-until (my-list:is-cursor-last char-list word-end-cursor)
         (my-list:move-cursor-to-next char-list word-end-cursor)
         (objlet* ((,char! (my-list:get-data word-end-cursor)))
           ,@body)))))

(with-objs (user! text! line!)
  (defun print-line ()
    ;; (format t "(print-line) ~a~%" (my-list:get-size char-list))

    (if (my-list:is-empty char-list)
	(progn
	  (init-line-variables)
	  (setf x-line 0.0)
	  (setf curr-render-line-height (objlet* ((font! curr-font)
						  (glyph! (gethash #\  glyph-table)))
					  (* advance-height curr-font-size)))
	  (print-cursors)
	  (incf y-line curr-render-line-height)
	  (init-render-line-height-variables)

	  (if (>= y-line window-height)
	      (error 'y-line-exceeded-window-height))
	  
	  (return-from print-line)))

    (loop-char-in-line (char!)
      (if (is-space-char)
	  (progn
	    (my-list:move-cursor-to-prev char-list word-end-cursor)
            (advance-word)
            (my-list:move-cursor-to-next char-list word-start-cursor)
            (my-list:move-cursor-to-next char-list word-end-cursor)
            (advance-space))
	  (progn
	    (increase-word-width)))

      (if (my-list:is-cursor-last char-list word-end-cursor)
	  (progn
	    (advance-word)
	    (print-and-prepare-new-line))))))

(with-objs (text!)
  (defobjmacro loop-from-iter-line-in-text ((line!) &body body)
    `(loop-until (my-tree:is-cursor-last line-tree text-iter-cursor)
       (my-tree:move-cursor-to-next line-tree text-iter-cursor)
       (objlet* ((,line! (my-tree:get-data text-iter-cursor)))
	 ,@body))))

(export 'get-render-msg)
(with-objs (user!)
  (defun get-render-msg ()
    (handler-case
	(progn
	  (init-window-variables)
	  (init-render-line-height-variables)
	  (objlet* ((text! text))
	    (loop-from-iter-line-in-text (line!)
	      ;; (format t "(get-render-msg) line ~a~%" line!)
	      (print-line))
	    ;; (format t "(get-render-msg) text ~a~%" line-tree)
	    ))
      (y-line-exceeded-window-height (c)
	;; (format t "~s~%" c)
	))
    (get-output-stream-string osstream)))

(with-objs (text! line!)
  (defun create-cursor! (line-index char-index)
    (objlet* ((cursor! (make-cursor!)))
      (setf text-cursor (my-tree:create-cursor! line-tree line-index))
      (setf line-cursor (my-list:create-cursor! char-list char-index))
      cursor!)))

(with-objs (user! text!)
  (defun add-primary-cursor (line-index char-index)
    (if (and (> line-index 0)
	     (<= line-index (my-tree:get-size line-tree)))
	(my-tree:move-cursor-to-index line-tree text-iter-cursor line-index)
	(progn
	  (format t "(add-primary-cursor) line-index ~a should be greater than ~a and less or equal to ~a.~%" line-index 0 (my-tree:get-size line-tree))
	  (format t "max ~a~%" (my-tree:get-size line-tree))
	  (return-from add-primary-cursor)))

    (objlet* ((line! (my-tree:get-data text-iter-cursor)))
      (if (and (>= char-index 0)
	       (<= char-index (my-list:get-size char-list)))
	  (objlet* ((cursor! (create-cursor! line-index char-index)))
            (my-tree:push-cursor! line-tree text-cursor)
            (my-list:push-cursor! char-list line-cursor)
	    ;; update user
	    (setf primary-cursor cursor!)
	    (push cursor! cursor-list))
	  (progn
	    (format t "(add-primary-cursor) char-index ~a out of bound.~%" char-index)
	    (return-from add-primary-cursor))))))

(with-objs (user! text!)
  (defun copy-and-paste-primary-cursor ()
    (objlet* ((cursor! primary-cursor))
    (let ((line-index (my-tree:index-of text-cursor))
          (char-index (my-list:index-of line-cursor)))
      (add-primary-cursor line-index char-index)))))

(with-objs (text! line! cursor!)
  (defun remove-cursor-in-text ()
    (my-tree:remove-cursor! line-tree text-cursor)
    (my-list:remove-cursor! char-list line-cursor)))

(with-objs (user! text!)
  (defun remove-all-but-primary-cursor ()
    (objdolist (cursor! cursor-list)
      (if (not-eq cursor! primary-cursor)
          (objlet* ((line! (my-tree:get-data text-cursor)))
	    (remove-cursor-in-text))))
    (setf cursor-list (list primary-cursor))))

(with-objs (user! text!)
  (defun remove-cursors ()
    (objdolist (cursor! cursor-list)
      (objlet* ((line! (my-tree:get-data text-cursor)))
	(remove-cursor-in-text)))
    (setf cursor-list nil)))

;; export

(export 'link-user)
(with-objs (user! text!)
  (defun link-user ()
    (push user! user-list)
    (setf text text!)
    (add-primary-cursor 1 0)))

(export 'unlink-user)
(with-objs (user! text!)
  (defun unlink-user ()
    (remove-el user! user-list)
    (setf text nil)
    (remove-cursors)))

(with-objs (user! text! cursor!)
  (defun move-cursor-right ()
    (objlet* ((line! (my-tree:get-data text-cursor)))
      (if (my-list:is-cursor-last char-list line-cursor)
	  (if (my-tree:is-cursor-last line-tree text-cursor)
	      nil
	      (progn
		(my-list:remove-cursor! char-list line-cursor)
		(my-tree:move-cursor-to-next line-tree text-cursor)
		(setq line! (my-tree:get-data text-cursor))
		(my-list:move-cursor-to-head char-list line-cursor)
		(my-list:push-cursor! char-list line-cursor)))
	  (my-list:move-cursor-to-next char-list line-cursor)))
    (setf default-pos-in-line (my-list:index-of line-cursor))
    nil))

(export 'move-cursors-right)
(with-objs (user!)
  (defun move-cursors-right ()
    (objdolist (cursor! cursor-list)
      (move-cursor-right))))

(with-objs (user! text! cursor!)
  (defun move-cursor-left ()
    (objlet* ((line! (my-tree:get-data text-cursor)))
      (if (= (my-list:index-of line-cursor) 0)
          (if (= (my-tree:index-of text-cursor) 1)
              nil
              (progn		
		(my-list:remove-cursor! char-list line-cursor)
		(my-tree:move-cursor-to-prev line-tree text-cursor)
		(setq line! (my-tree:get-data text-cursor))
		(my-list:move-cursor-to-last char-list line-cursor)
		(my-list:push-cursor! char-list line-cursor)))
          (my-list:move-cursor-to-prev char-list line-cursor)))
    (setf default-pos-in-line (my-list:index-of line-cursor))
    nil))

(export 'move-cursors-left)
(with-objs (user!)
  (defun move-cursors-left ()
    (objdolist (cursor! cursor-list)
      (move-cursor-left))))

(with-objs (user! text! cursor!)
  (defun move-cursor-up ()
    (objlet* ((line! (my-tree:get-data text-cursor)))
      (if (= (my-tree:index-of text-cursor) 1)
          nil
          (progn
	    (my-list:remove-cursor! char-list line-cursor)
            (my-tree:move-cursor-to-prev line-tree text-cursor)
            (setq line! (my-tree:get-data text-cursor))
            (my-list:move-cursor-to-index char-list line-cursor (clamp default-pos-in-line 0 (my-list:get-size char-list)))
	    (my-list:push-cursor! char-list line-cursor))))
    nil))

(export 'move-cursors-up)
(with-objs (user!)
  (defun move-cursors-up ()
    (objdolist (cursor! cursor-list)
      (move-cursor-up))))

(with-objs (user! cursor!)
  (defun move-cursor-down ()
    (objlet* ((text! text)
	      (line! (my-tree:get-data text-cursor)))
      (if (my-tree:is-cursor-last line-tree text-cursor)
          nil
          (progn
	    (my-list:remove-cursor! char-list line-cursor)
            (my-tree:move-cursor-to-next line-tree text-cursor)
            (setq line! (my-tree:get-data text-cursor))
            (my-list:move-cursor-to-index char-list line-cursor (clamp default-pos-in-line 0 (my-list:get-size char-list)))
	    (my-list:push-cursor! char-list line-cursor))))
    nil))

(export 'move-cursors-down)
(with-objs (user!)
  (defun move-cursors-down ()
    (objdolist (cursor! cursor-list)
      (move-cursor-down))))

(with-objs (user! cursor!)
  (defun insert-char-after-cursor (char)
    (objlet* ((line! (my-tree:get-data text-cursor))
	      (char! (create-char! char)))
      (my-list:insert-data-after-cursor char-list line-cursor char!)
      (setf default-pos-in-line (my-list:index-of line-cursor)))))

(export 'insert-char-after-cursors)
(with-objs (user!)
  (defun insert-char-after-cursors (char)
    (objdolist (cursor! cursor-list)
      (insert-char-after-cursor char))))

(with-objs (user! cursor!)
  (defun insert-new-line-after-cursor ()
    (objlet* ((line! (my-tree:get-data text-cursor))
	      (split-char-list (my-list:split-list-after-cursor char-list line-cursor))
	      (new-line! (make-line! :char-list split-char-list
				     :line-iter-cursor (my-list:get-default-cursor split-char-list)
				     :word-start-cursor (my-list:create-cursor! split-char-list 0)
				     :word-end-cursor (my-list:create-cursor! split-char-list 0)
				     :render-line-start-cursor (my-list:create-cursor! split-char-list 0)
				     :render-line-end-cursor (my-list:create-cursor! split-char-list 0))))
      (objlet* ((text! text))
	(my-tree:insert-data-after-cursor line-tree text-cursor new-line!)
	(setf default-pos-in-line (my-list:index-of line-cursor))))))

(export 'insert-new-line-after-cursors)
(with-objs (user!)
  (defun insert-new-line-after-cursors ()
    (objdolist (cursor! cursor-list)
      (insert-new-line-after-cursor))))

(with-objs (user! cursor!)
  (defun delete-char-at-cursor ()
    (objlet* ((text! text)
	      (line! (my-tree:get-data text-cursor)))
      (if (eq (my-list:index-of line-cursor) 0)
	  (if (eq (my-tree:index-of text-cursor) 1)
	      nil
	      (progn
		(my-tree:delete-data-at-cursor line-tree text-cursor)
		(objlet* ((prev-line! (my-tree:get-data text-cursor)))
		  (my-list:merge-list-to-prev prev-char-list char-list))))
	  (my-list:delete-data-at-cursor char-list line-cursor)))))

(export 'delete-char-at-cursors)
(with-objs (user!)
  (defun delete-char-at-cursors ()
    (objdolist (cursor! cursor-list)
      (delete-char-at-cursor))))
