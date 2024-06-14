(ql:quickload "alexandria")
(ql:quickload "lparallel")
(ql:quickload "cl-ppcre")
;; (ql:quickload "ieee-floats")

(defpackage my-text
  (:nicknames :my-text)
  (:use :common-lisp :my-utils :defobj :alexandria :lparallel :cl-ppcre))

(in-package :my-text)

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
	(dolist (file-path (uiop:directory-files path))
	  (parse-file font! file-path)))
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
  (x-line-gap 1.0 :type single-float)
  (y-line-gap 1.0 :type single-float))

(defobj line!
  (char-list nil :type my-list:multi-cursor-list!)
  (line-iter-cursor nil :type my-list:cursor!)
  ;; line variables
  (word-start-cursor nil :type my-list:cursor!)
  (word-end-cursor nil :type my-list:cursor!)
  (curr-word-width 0.0 :type single-float))

(defmethod print-object ((l line!) out)
  (print-unreadable-object (l out :type t)
    (format out "~a" (line!-char-list l))))

(defmacro create-line! ()
  `(let ((char-list (my-list:create-multi-cursor-list!)))
     (make-line! :char-list char-list
                 :line-iter-cursor (my-list:get-default-cursor char-list)
                 :word-start-cursor (my-list:create-cursor! char-list 0)
                 :word-end-cursor (my-list:create-cursor! char-list 0))))

(defobj text!
  (file-path nil)
  (line-tree nil :type (or null my-tree:multi-cursor-tree!))
  (text-iter-cursor nil :type (or null my-tree:cursor!))

  (user-list nil :type (or null cons)))

(export 'create-text!)
(defobjfun create-text! ()
  (objlet* ((line-tree (my-tree:create-multi-cursor-tree!)))
    (make-text! :line-tree line-tree
		:text-iter-cursor (my-tree:get-default-cursor line-tree))))

(defmethod print-object ((obj text!) out)
  (print-unreadable-object (obj out :type t)
    (format out "~a" (text!-file-path obj))))

(export 'cursor!)
(defobj cursor!
  (text-cursor nil :type (or null my-tree:cursor!))
  (line-cursor nil :type (or null my-list:cursor!))
  (default-pos-in-line 0 :type integer)
  (cursor-color #(0.0 0.0 1.0) :type (simple-vector 3)))

(defmethod print-object ((c cursor!) out)
  (print-unreadable-object (c out :type t)
    (format out "line-index : ~a, char-index : ~a"
            (if (cursor!-text-cursor c) (my-tree:index-of (cursor!-text-cursor c)))
            (if (cursor!-line-cursor c) (my-list:index-of (cursor!-line-cursor c))))))

(declaim (special text! line! font! char! glyph! bounding-box!))

(with-objs (char!)
  (defun get-char-advance ()
    (objlet* ((glyph! glyph)
              (char-bounding-box! bounding-box))
      (+ x-line-gap (* (- char-x-max char-x-min) font-size)))))

(with-objs (char!)
  (defun get-space-advance ()
    (objlet* ((glyph! glyph))
      (* advance-width font-size))))

(with-objs (text!)
  (defun get-current-line ()
    (my-tree:get-data text-iter-cursor)))

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

(defobjfun load-text (text! file-path-in)
  (setf file-path file-path-in)
  (insert-line-in-text (create-line!))
  (loop-char-in-file (char fin file-path-in)
    (if (eq char #\Newline)
        (insert-line-in-text (create-line!))
        (objlet* ((line! (get-current-line)))
          (insert-char-in-line (make-char! :char char)))))
  text!)

;; get pos and length in vulkan x, y coordinate ranged from -1.0 to 1.0

(export 'make-user!)
(defobj user!
  (username nil)
  (connect nil)
  (socket-stream-in nil)
  (key-input-state nil)
  (osstream (make-string-output-stream) :type sb-impl::string-output-stream)
  (text nil :type (or null text!))

  ;; char variables
  (curr-font nil :type (or null font!))
  ;; normalized scale to pixel scale
  (curr-font-size 10.0 :type single-float)
  ;; pixel scale
  (curr-x-line-gap 1.0 :type single-float)
  (curr-y-line-gap 1.0 :type single-float)
  
  ;; pixel scale
  (window-width 400.0 :type single-float)
  (window-height 600.0 :type single-float)

  ;; window variables
  ;; pixel scale
  (x-line 0.0 :type single-float)
  (y-line 0.0 :type single-float)
  
  ;; render line height variables
  ;; pixel scale
  (curr-render-line-base 0.0 :type single-float)
  (curr-render-line-height 0.0 :type single-float)

  (curr-render-line-row-index 1 :type integer)
  
  (render-start-line-index 1 :type integer)
  (render-start-line-row-index 1 :type integer)

  (print-started nil :type boolean)
  
  (primary-cursor nil :type (or null cursor!))
  (cursor-list nil :type (or null cons)))

(declaim (special user!))

(with-objs (user!)
  (defun create-char! (char)
    (objlet* ((font! curr-font))
      (make-char! :char char
		  :fontname fontname
		  :glyph (gethash char glyph-table)
		  :font-size curr-font-size
		  :x-line-gap curr-x-line-gap
		  :y-line-gap curr-y-line-gap))))

;; pixel scale to vk scale
(with-objs (user!)
  (defun get-x-in-vk-coord (x)
    (+ -1 (* (/ x window-width) 2))))

;; pixel scale to vk scale
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
    (get-x-in-vk-coord (+ x-line x-line-gap))))

(with-objs (user! char!)
  (defun get-char-y-in-vk-coord ()
    (objlet* ((glyph! glyph)
	      (char-bounding-box! bounding-box))
      (get-y-in-vk-coord (+ y-line y-line-gap curr-render-line-base (- char-y-min))))))

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

(with-objs (text!)
  (defobjmacro loop-cursor ((user! cursor!) &body body)
    `(objdolist (,user! user-list)
       (objdolist (,cursor! ,cursor-list)
         ,@body))))

(with-objs (text! line! cursor!)
  (defun cursor-matches-text-iter-cursor ()
    (and (eq (my-tree:index-of text-iter-cursor)
             (my-tree:index-of text-cursor))
         (eq (my-list:index-of word-start-cursor)
             (my-list:index-of line-cursor)))))

;; text=iter-cursor, word-start-cursor, and word-end-cursor in text! and line! will be iterated for text iteration

(with-objs (user! text! line! char!)
  (defun print-cursors ()
    (loop-cursor (cursor-user! cursor!)
      (when (cursor-matches-text-iter-cursor)
	(format osstream "cursor ~a ~a ~a ~a ~a ~a ~a ~a~%"
		cursor-username
		(get-char-x-in-vk-coord)
		(get-char-y-in-vk-coord)
		(get-char-width-in-vk-coord)
		(get-char-height-in-vk-coord)
		(aref cursor-color 0)
		(aref cursor-color 1)
		(aref cursor-color 2))))))

(with-objs (user! char!)
  (defun print-char ()
    (when (not-eq char #\ )
      (format osstream "char ~a ~a ~a ~a ~a~%"
	      char
	      (get-char-x-in-vk-coord)
	      (get-char-y-in-vk-coord)
	      (get-char-width-in-vk-coord)
	      (get-char-height-in-vk-coord)))))

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
      (update-variable curr-render-line-base (* char-y-max font-size) #'>)
      (update-variable curr-render-line-height (+ y-line-gap curr-render-line-base (- char-y-min)) #'>))))

(define-condition y-line-exceeded-window-height (error)
  ())

(with-objs (user!)
  (defun insert-new-render-line ()
    (setf x-line 0.0)
    (incf curr-render-line-row-index)
    (if print-started
	(progn
	  (incf y-line curr-render-line-height)
	  (init-render-line-height-variables)
	  (if (>= y-line window-height)
	      (error 'y-line-exceeded-window-height))))))

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
        (insert-new-render-line))
    (incf x-line (get-char-advance))
    (decrease-word-width)
    (if print-started
	(progn
	  (print-char)
	  (print-cursors)
	  (update-render-line-height-variables)))))

(with-objs (user! text! line! char!)
  (defun advance-space ()
    (incf x-line (get-space-advance))
    (if print-started
	(print-cursors))))

(with-objs (line!)
  ;; loop word-start-cursor until it matches to word-end-cursor while binding char
  (defmacro loop-char-in-curr-word ((char!) &body body)
    `(loop-until (my-list:same-indices? word-start-cursor word-end-cursor)
       (my-list:move-cursor-to-next char-list word-start-cursor)
       (objlet* ((,char! (my-list:get-data word-start-cursor)))
         ,@body))))

(with-objs (user! line!)
  (defun curr-word-overflows-render-line ()
    (> (+ x-line curr-word-width) window-width)))

(with-objs (user! line!)
  (defun curr-word-fits-in-new-render-line ()
    (<= curr-word-width window-width)))

(with-objs (user! text! line!)
  (defun advance-word ()
    (if (and (curr-word-overflows-render-line)
             (curr-word-fits-in-new-render-line))
        (insert-new-render-line))
    (loop-char-in-curr-word (char!)
      (advance-char))))

(with-objs (line!)
  (defun init-line-variables ()
    (my-list:move-cursor-to-head char-list word-start-cursor)
    (my-list:move-cursor-to-head char-list word-end-cursor)
    (setf curr-word-width 0.0)))

(with-objs (line!)
  (defmacro loop-char-in-line ((char!) &body body)
    `(progn
       (init-line-variables)
       (loop-until (my-list:is-cursor-last char-list word-end-cursor)
         (my-list:move-cursor-to-next char-list word-end-cursor)
         (objlet* ((,char! (my-list:get-data word-end-cursor)))
           ,@body)))))

(defparameter *space-chars* '(#\ ))

(with-objs (char!)
  (defun is-space-char ()
    (member char *space-chars*)))

(with-objs (user! text! line!)
  (defun advance-line ()
    (loop-char-in-line (char!)
      (cond ((is-space-char)
             (my-list:move-cursor-to-prev char-list word-end-cursor)
             (advance-word)
             (my-list:move-cursor-to-next char-list word-start-cursor)
             (my-list:move-cursor-to-next char-list word-start-cursor)
             (advance-space))
	    ((my-list:is-cursor-last char-list word-end-cursor)
             (advance-word))
	    (t
             (increase-word-width))))))

(with-objs (user!)
  (defun init-window-variables ()
    (setf x-line 0.0)
    (setf y-line 0.0)
    (my-tree:move-cursor-to-index line-tree text-iter-cursor (1- render-start-line-index))
    (setf curr-render-line-row-index 1)
    (setf print-started nil)))

(with-objs (user!)
  (defun insert-new-line ()
    (setf x-line 0.0)
    (setf curr-render-line-row-index 1)
    (if print-started
	(progn
	  (incf y-line curr-render-line-height)
	  (init-render-line-height-variables)
	  (if (>= y-line window-height)
	      (error 'render-finished))))))

(with-objs (text!)
  (defmacro loop-from-iter-line-in-text ((line!) &body body)
    `(loop-until (my-tree:is-cursor-last line-tree text-iter-cursor)
       (my-tree:move-cursor-to-next line-tree text-iter-cursor)
       (objlet* ((,line! (my-tree:get-data text-iter-cursor)))
	 ,@body))))

(with-objs (user!)
  (defun print-text ()
    (handler-case
	(progn
	  (init-window-variables)
	  (init-render-line-height-variables)
	  (objlet* ((text! text))
	    (my-tree:move-cursor-to-index line-tree text-iter-cursor (1- render-start-line-index))
	    (loop-from-render-line-in-text (line!)
	      (advance-line)
	      (insert-new-line))))

      (y-line-exceeded-window-height (c)
	(format t "~s~%" c)))))

(with-objs (text! line!)
  (defun create-cursor! (line-index char-index)
    (objlet* ((cursor! (make-cursor!)))
      (setf text-cursor (my-tree:create-cursor! line-tree line-index))
      (setf line-cursor (my-list:create-cursor! char-list char-index)))))

(with-objs (user! text!)
  (defun add-primary-cursor (line-index char-index)
    (if (and (> line-index 0)
	     (<= line-index (my-tree:get-size line-tree)))
	(my-tree:move-cursor-to-index line-tree text-iter-cursor line-index)
	(progn
	  (format t "my-text:push-cursor! line-index out of bound.~%")
	  (return-from add-primary-cursor)))

    (objlet* ((line! (my-tree:get-data text-iter-cursor)))
      (if (and (> char-index 0)
	       (<= char-index (my-list:get-size char-list)))
	  (objlet* ((cursor! (create-cursor! line-index char-index)))
            (my-tree:push-cursor! line-tree text-cursor)
            (my-list:push-cursor! char-list line-cursor)
	    ;; update user
	    (setf primary-cursor cursor!)
	    (push cursor! cursor-list))
	  (progn
	    (format t "my-text:push-cursor! char-index out of bound.~%")
	    (return-from add-primary-cursor))))))

(declaim (special cursor!))

(with-objs (cursor!)
  (defun get-line ()
    (my-tree:get-data text-cursor)))

(with-objs (user! text!)
  (defun remove-all-but-primary-cursor ()
    (objdolist (cursor! cursor-list)
      (if (not-eq cursor! primary-cursor)
          (objlet* ((line! (get-line)))
            (my-tree:remove-cursor! line-tree text-cursor)
            (my-list:remove-cursor! char-list line-cursor)
            )))))





(with-objs (user! line!)
  ;; bind line-height before use
  (defmacro advance-new-line-in-window ()
    `(progn
       (decf height-vacancy line-height)
       (setf width-vacancy window-width)
       (incf render-line-row-count)))

  ;; bind line-height, char-width before use
  (defmacro advance-char-in-window ()
    `(progn
       (if (< width-vacancy char-width)
	   (advance-new-line-in-window))
       (decf width-vacancy char-width)))

  ;; move word-start-cursor first and handle corresponding char when looping character in a line
  (defmacro loop-char-in-curr-word ((char!) &body body)
    `(loop-until (my-list:same-indices? word-start-cursor word-end-cursor)
       (my-list:move-cursor-to-next char-list word-start-cursor)
       (objlet* ((,char! (my-list:get-data word-start-cursor)))
	 ,@body)))
  
  (defmacro curr-word-can-fit-in-new-line ()
    `(and (< width-vacancy curr-word-width) (<= curr-word-width window-width)))

  (defmacro advance-curr-word-in-window ()
    `(let ((line-height (get-line-height user! line!)))
       (if (curr-word-can-fit-in-new-line)
	   (advance-new-line-in-window))

       (loop-char-in-curr-word (char!)
	 (let ((char-width (get-char-width user! char!)))
	   (advance-char-in-window)
	   (decf curr-word-width char-width)))))

  (defmacro is-word-end-cursor-last-in-line ()
    `(my-list:is-cursor-last char-list word-end-cursor))
  
  (defmacro move-word-end-cursor-to-prev ()
    `(my-list:move-cursor-to-prev char-list word-end-cursor))

  (defmacro move-word-start-cursor-to-next ()
    `(my-list:move-cursor-to-next char-list word-start-cursor))

  (defmacro move-word-end-cursor-to-next ()
    `(my-list:move-cursor-to-next char-list word-end-cursor))

  (defmacro move-word-start-cursor-to-head ()
    `(my-list:move-cursor-to-head char-list word-start-cursor))

  (defmacro move-word-end-cursor-to-head ()
    `(my-list:move-cursor-to-head char-list word-end-cursor))

  (defmacro loop-char-in-line ((char!) &body body)
    `(progn
       (move-word-start-cursor-to-head)
       (move-word-end-cursor-to-head)
       (setf curr-word-width 0.0)
       
       (loop-until (is-word-end-cursor-last-in-line)
	 (move-word-end-cursor-to-next)
	 (objlet* ((,char! (my-list:get-data word-end-cursor)))
	   ,@body))))

  (defvar *space-chars* '(#\ ))

  (defobjfun space-char-p (char!)
    (member char *space-chars*))

  (defmacro advance-line-in-window ()
    `(loop-char-in-line (char!)
       (cond ((space-char-p char)
	      ;; move word-end-cursor to the prev character before space char temporarily
	      (move-word-end-cursor-to-prev)
	      (advance-curr-word-in-window)
              ;; move cursors to space char
	      (move-word-start-cursor-to-next)
	      (move-word-end-cursor-to-next)
	      (let ((char-width (get-char-width user! char!)))
		(advance-char-in-window)))

	     ((is-word-end-cursor-last-in-line)
	      (advance-curr-word-in-window))

	     (t
	      (incf curr-word-width char-width))))))

(defmacro with-return-form ((form) &body body)
  `(progn
     ,@body
     ,form))

(defobjfun get-render-line-row-size (user! line!)
  (with-return-form (render-line-row-count)
    (setf width-vacancy window-width)
    (setf height-vacancy window-height)
    (setf render-line-row-count 1)
    (advance-line-in-window)))

(with-objs (user! text! line!)
  (defmacro print-curr-word-in-window ()
    `(let ((line-height (get-line-height user! line!)))
       (if (curr-word-can-fit-in-new-line)
	   (advance-new-line-in-window))

       (loop-char-in-curr-word (char!)
	 (let ((char-width (get-char-width user! char!)))
	   (advance-char-in-window)
	   (decf curr-word-width char-width))))
    
    `(progn
       )))

(defobjfun print-curr-word-in-line (user! text! line!)
  (loop-char-in-curr-word (char!)    
    (let ((char-width (get-char-width user! char!)))
      (print-char user! char! flag)
      (print-cursors user! text! line! char!)
      (add-char-space-in-window user! char-width)
      (decf curr-word-width char-width))))

(defobjfun print-curr-word (user! text! line!)
  (cond ((>= width-vacancy curr-word-width)
         (print-curr-word-in-line user! text! line!))
	
        ((< width-vacancy curr-word-width)
	 (let ((line-height (get-line-height user!)))
	   (cond ((>= window-width curr-word-width)
                  (cond ((>= height-vacancy line-height)
			 (format-new-line-in-window user!)
			 (print-curr-word-in-line user! text! line!))

			((< height-vacancy line-height)
			 (return))))

		 ((< window-width curr-word-width)
		  (loop-char-in-curr-word-in-window (char! line!)
		    (let ((char-width (get-char-width user! char!)))
		      (cond ((< width-vacancy char-width)
			     (my-list:move-cursor-to-prev char-list word-start-cursor)
			     (format-new-line-in-window user!))

			    ((>= width-vacancy char-width)
			     (print-char user! text! line! char!)
			     (print-cursor user! text! line!)
			     (format-word-char-in-window user! char!)))))))))))

(defobjfun print-text (user! text!)
  (setf width-vacancy window-width)
  (setf height-vacancy window-height)

  (my-tree:move-cursor-to-index line-tree text-iter-cursor (1- render-start-line-index))

  (until (or (my-tree:is-cursor-last line-tree text-iter-cursor)
	     (< height-vacancy (get-line-height user!)))
    ;; skip head
	 (my-tree:move-cursor-to-next line-tree text-iter-cursor)
    (format t "height vacancy : ~a width vacancy : ~a~%~a~%" height-vacancy width-vacancy text-iter-cursor)

    (objlet* ((line! (my-tree:get-data text-iter-cursor)))      
      (my-list:move-cursor-to-head char-list word-start-cursor)
      (my-list:move-cursor-to-head char-list word-end-cursor)
      (setf curr-word-width 0.0)

      (print-cursor user! text! line!)

      (until (my-list:is-cursor-last char-list word-end-cursor)
	(my-list:move-cursor-to-next char-list word-end-cursor)
	(objlet* ((char! (my-list:get-data word-end-cursor))
		  (char-width (get-char-width user! char!)))
	  (incf curr-word-width char-width)
	  ;; (format t "curr-word-width ~a~%" curr-word-width)
	  (cond ((eq char #\ )
                 ;; move cursor to the character before #\ 
                 (my-list:move-cursor-to-prev char-list word-end-cursor)
                 (print-curr-word user! text! line!)
                 ;; move cursor to #\ 
		 (my-list:move-cursor-to-next char-list word-start-cursor)
                 (print-char user! text! line! char!)
		 (print-cursor user! text! line!)
                 (my-list:move-cursor-to-next char-list word-end-cursor))
		
                ((my-list:is-cursor-last char-list word-end-cursor)
                 (print-curr-word user! text! line!)))))
      
      (print-new-line user!)))

  (get-output-stream-string osstream))

(defobjfun curr-word-can-fit-in-new-line (uesr! line!)
  (and (< width-vacancy curr-word-width) (<= curr-word-width window-width)))

(with-objs (user! line!)
  (defobjmacro curr-word-can-fit-in-new-line ()
    `(and (< width-vacancy curr-word-width) (<= curr-word-width window-width))))

(defobjfun add-curr-word-in-window (user! line!)
  (let ((line-height (get-line-height user! line!)))
    (if (curr-word-can-fit-in-new-line)
	(add-new-line-in-window))

    (loop-char-in-curr-word (char! line!)
      (let ((char-width (get-char-width user! char!)))
	(add-char-space-in-window)
	(decf curr-word-width char-width)))))

(defobjmacro loop-char-in-line ((char! line!) &body body)
  `(progn
     (setf ,curr-word-width 0.0)
     (my-list:move-cursor-to-head ,char-list ,word-start-cursor)
     (my-list:move-cursor-to-head ,char-list ,word-end-cursor)
     (loop-until (my-list:is-cursor-last ,char-list ,word-end-cursor)
       (my-list:move-cursor-to-next char-list word-end-cursor)
       (objlet* ((,char! (my-list:get-data ,word-end-cursor)))
	 ,@body))))

(objlet* ((user!) (char!) (line!))
  (defobjmacro move-word-end-cursor-to-prev ()
    `(my-list:move-cursor-to-prev char-list word-end-cursor))

  (defobjmacro move-word-start-cursor-to-next ()
    `(my-list:move-cursor-to-next char-list word-start-cursor))

  (defobjmacro move-word-end-cursor-to-next ()
    `(my-list:move-cursor-to-next char-list word-end-cursor))

  (defobjmacro is-char-last ()
    `(my-list:is-cursor-last char-list word-end-cursor))

  )

(defobjfun add-line-in-window (user! line!)
  (loop-char-in-line (char! line!)
    (cond ((space-char-p char)
	   ;; move cursor to the character before space char 
	   (move-word-end-cursor-to-prev)
	   (add-curr-word-in-window user! line!)
           ;; move cursors to space char
	   (move-word-start-cursor-to-next)
	   (move-word-end-cursor-to-next)
	   (add-space-char-in-window user! char!))

	  ((is-char-last)
	   (add-curr-word-in-window user! line!))

	  (t
	   (incf curr-word-width char-width)))))

(defmacro return-val ((val &optional val-init) &body body)
  `(let ((,val ,val-init))
     ,@body
     ,val))

(defobjfun increase-render-start-line (user! amount)
  )

(defobjfun decrease-render-start-line (user! amount)
  )

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
  (push user! user-list)
  (add-primary-cursor user! text! 1 0)
  (setf text text!))

(defobjfun unlink-user (user! text!)
  (remove-el user! user-list)
  (remove-cursors user! text!)
  (setf text nil))

;; (defobjfun remove-user (user! text!)
;;   (remhash connect user-table)
;;   (objdolist (cursor! cursor-list)
;;     (my-tree:remove-cursor! line-tree text-cursor)
;;     (objlet* ((line! (my-tree:get-data text-cursor)))
;;       (my-list:remove-cursor! char-list line-cursor)))
;;   (setf cursor-list nil))

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

;; (defobjfun insert-char (user! text! char)
;;   (objdolist (cursor! cursor-list)
;;     (insert-char-after-cursor text! cursor! (make-char! :char char))))

;; (defobjfun insert-new-line (user! text!)
;;   (objdolist (cursor! cursor-list)
;;     (insert-new-line-after-cursor text! cursor!)))

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
