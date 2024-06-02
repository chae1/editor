(defvar zpb-path (merge-pathnames "../font/zpb-ttf/" (uiop/os:getcwd)))

(defun path-exists (path)
  (probe-file path))

(defun push-path-in-central-registry (path)
  (unless (member path asdf:*central-registry* :test #'equal)
    (push path asdf:*central-registry*)))

(if (path-exists zpb-path)
    (push-path-in-central-registry zpb-path))

(asdf:load-system "zpb-ttf")

(defpackage my-font
  (:use :common-lisp :my-utils :defobj :zpb-ttf))

(in-package :my-font)

(defparameter *debug-mode* t)

(defmacro compile-in-debug-mode (&body body)
  (if *debug-mode*
      `(progn
	 ,@body)
      nil))

(defvar *char->name* (make-hash-table :test #'equal))

(defun set-char-str-pair (pair)
  (setf (gethash (car pair) *char->name*) (cdr pair)))

(defparameter *small-chars* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(defparameter *big-chars* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defparameter *other-chars* `(#\` #\~ #\! #\@ #\# #\$ #\% #\^ #\& #\* #\( #\) #\- #\_ #\+ #\= #\{ #\} #\[ #\] #\| #\\ #\; #\: #\' #\" #\, #\. #\< #\> #\? #\/ #\  #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defparameter *chars* (append *small-chars* *big-chars* *other-chars*))

(defun char-to-str (c)
  (coerce `(,c) 'string))

(defparameter *small-char-str-pairs* (mapcar #'(lambda (c) (cons c (char-to-str c))) *small-chars*))

(defparameter *big-char-str-pairs* (mapcar #'(lambda (c) (cons c (char-to-str c))) *big-chars*))

(defparameter *other-char-str-pairs* '((#\` . "backquote") (#\~ . "tilde") (#\! . "exclamation_mark") (#\@ . "ampersat") (#\# . "sharp") (#\$ . "dollar") (#\% . "percent") (#\^ . "caret") (#\& . "ampersand") (#\* . "asterisk") (#\( . "open_parenthesis") (#\) . "close_parenthesis") (#\- . "hyphen") (#\_ . "underscore") (#\+ . "plus") (#\= . "equal") (#\{ . "open_brace") (#\} . "close_brace") (#\[ . "open_bracket") (#\] . "close_bracket") (#\| . "pipe") (#\\ . "backslash") (#\; . "semicolon") (#\: . "colon") (#\' . "single_quote") (#\" . "quote") (#\, . "comma") (#\. . "period") (#\< . "less_than") (#\> . "greater_than") (#\? . "question_mark") (#\/ . "slash") (#\  . "space") (#\0 . "zero") (#\1 . "one") (#\2 . "two") (#\3 . "three") (#\4 . "four") (#\5 . "five") (#\6 . "six") (#\7 . "seven") (#\8 . "eight") (#\9 . "nine")))

(defparameter *char-str-pairs* (append *small-char-str-pairs* *big-char-str-pairs* *other-char-str-pairs*))

(defun init-map-char->name ()
  (mapc #'set-char-str-pair *char-str-pairs*))

(init-map-char->name)

(defun print-hash-entry (key value)
  (format t "~S -> ~S~%" key value))

(defun print-char->name ()
  (maphash #'print-hash-entry *char->name*))

(defobj font-info!
  ;; slots
  (font-loader nil :type (or null zpb-ttf::font-loader))
  (font-path nil :type (or null pathname))
  (font-name nil :type (or null string))
  ;; variables
  (curr-glyph nil)
  (curr-points nil)
  ;; 0: start, 1: on curve visited before, 2: off curve visited before, 3: end
  (curr-point-state 0 :type integer))

(defun create-font-info! (ttf-path)
  (objlet* ((font-info! (make-font-info!)))
    (setf font-loader (open-font-loader ttf-path))
    (setf font-path ttf-path)
    (setf font-name (pathname-name ttf-path))
    (setf curr-point (make-point!))
    font-info!))

(declaim (special font-info!))

(with-objs (font-info!)
  (defobjmacro loop-points (() &body body)
    "iterate curr-points for curr-glyph"
    (let ((i (symbol-append 'i- (gensym))))
      `(let ((,i 0))
	 (loop-until (eq ,i (length (contours curr-glyph)))
	   (setf curr-points (explicit-contour-points (contour curr-glyph ,i)))
	   ,@body
	   (incf ,i))))))

(with-objs (font-info!)
  (defobjmacro loop-point ((i) &body body)
    "iterate i, i.e. point index, for curr-points"
    `(let ((,i 0))
       (loop-until (eq ,i (length curr-points))
	 ,@body
	 (incf ,i)))))

(with-objs (font-info!)
  (defobjmacro with-x-min-y-min ((x-min y-min) &body body)
    (let ((bbox (symbol-append 'bbox- (gensym))))
      `(let* ((,bbox (bounding-box curr-glyph))
	      (,x-min (aref ,bbox 0))
	      (,y-min (aref ,bbox 1)))
	 ,@body))))

(defobj point!
  (point)
  (x-out)
  (y-out))

(with-objs (font-info!)
  (defobjfun normalize-val (val)
    (let ((em (units/em font-loader)))
      (/ val em))))

(with-objs (font-info!)
  (defobjfun shift-point (point!)
    (with-x-min-y-min (x-min y-min)
      (decf x-out x-min)
      (decf y-out y-min)
      (setf x-out (normalize-val x-out))
      (setf y-out (normalize-val y-out)))))

(with-objs (font-info!)
  (defobjmacro with-point-at ((i point!) &body body)
    "bind point, point-x, point-y for i-th point of curr-points"    
    (objlet* ((new-point! (make-point!)))
      `(with-x-min-y-min (x-min y-min)
	 (objlet* ((,point! ,new-point!))
	   (setf ,point (aref curr-points ,i))
	   (setf ,x-out (x ,point))
	   (setf ,y-out (y ,point))
	   (shift-point ,point!)
	   ,@body)))))

;; 0: start, 1: on curve visited before, 2: off curve visited before, 3: end
(with-objs (font-info!)
  (defobjfun write-routine-at-i (file i)
    "write curve points for curves except the last one to file"    
    (with-point-at (i point!)
      (if (on-curve-p point)
	  (ccase curr-point-state
	    (0 (progn
		 (format file "~f ~f " x-out y-out)
		 (setf curr-point-state 1)))
	    (1 (with-point-at ((1- i) prev-point!)
		 (format file "~f ~f " (/ (+ x-out prev-x-out) 2) (/ (+ y-out prev-y-out) 2))
		 (format file "~f ~f~%" x-out y-out)
		 (format file "~f ~f " x-out y-out)
		 (setf curr-point-state 1)))
	    (2 (progn
		 (format file "~f ~f~%" x-out y-out)
		 (format file "~f ~f " x-out y-out)
		 (setf curr-point-state 1))))
	  (ccase curr-point-state
	    (1 (progn
		 (format file "~f ~f " x-out y-out)
		 (setf curr-point-state 2))))))))

;; 0: start, 1: on curve visited before, 2: off curve visited before, 3: end
(with-objs (font-info!)
  (defobjfun write-routine-at-0 (file)
    "write curve points for the last curve to file"
    (with-point-at (0 point!)
      (if (on-curve-p point)
	  (ccase curr-point-state
	    (1 (with-point-at ((1- (length curr-points)) prev-point!)
		 (format file "~f ~f " (/ (+ x-out prev-x-out) 2) (/ (+ y-out prev-y-out) 2))
		 (format file "~f ~f~%" x-out y-out)
		 (setf curr-point-state 3)))
	    (2 (progn
		 (format file "~f ~f~%" x-out y-out)
		 (setf curr-point-state 3))))
	  (cerror "first point is off curve" "")))))

(with-objs (font-info!)
  (defobjfun format-curves (file)
    "write curve points for glyph c to file"
    (progn
      (format file "~%curves p1x p1y p2x p2y p3x p3y~%")
      (when (> (length (contours curr-glyph)) 0)
	(loop-points ()
	  (setf curr-point-state 0)
	  (loop-point (i)
	    (write-routine-at-i file i))
	  (write-routine-at-0 file))))))

(with-objs (font-info!)
  (defobjfun format-bounding-box (file)
    (format file "~%bounding-box x-min y-min x-max y-max~%")
    (let ((bbox (bounding-box curr-glyph)))
      (mapc #'(lambda (i) (format file "~f " (normalize-val (aref bbox i)))) '(0 1 2 3)))
    (format file "~%")))

(with-objs (font-info!)
  (defobjfun generate-char-info-file (file-path c)
    "make file of file-path and write char info to file"
    (with-open-file (file file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (compile-in-debug-mode
	(format t "generate ~a~%" file-path))
      
      (format file "glyph~%~a~%" c)      
      (format-curves file)
      (format file "~%advance-width~%~f~%" (normalize-val (advance-width curr-glyph)))
      (format file "~%advance-height~%~f~%" (normalize-val (advance-height curr-glyph)))
      (format-bounding-box file)
      ;; (format file "~%render-box x-min y-min x-max y-max~%~f ~f ~f ~f~%" (aref mono-render-box 0) (aref mono-render-box 1) (aref mono-render-box 2) (aref mono-render-box 3))
      (format file "~%glyph-info-end~%"))))

;; font info file is created after all char info files are created
(with-objs (font-info!)
  (defobjfun generate-font-info-file (file-path)
    "make file of file-path and write char font info to file"
    (with-open-file (file file-path :direction :output :if-exists :supersede :if-does-not-exist :create)
      (print file-path)
      (format file "font~%~a~%" font-name)
      (format file "~%generated from ttf-path~%~f~%" font-path)
      ;; (format file "~%em~%~f~%" (units/em font-loader))
      (format file "~%font-info-end~%"))))

(with-objs (font-info!)
  (defun get-font-dir-path ()
    (concatenate 'string (namestring (merge-pathnames "../font/txt/" (uiop/os:getcwd))) font-name "/")))

(with-objs (font-info!)
  (defun get-font-file-path (c)
    (concatenate 'string (concatenate 'string (get-font-dir-path) (gethash c *char->name*)) ".txt")))

(with-objs (font-info!)
  (defun get-font-info-file-path ()
    (concatenate 'string (concatenate 'string (get-font-dir-path) "font-info.txt"))))

(defobjfun generate-font-info (font-info!)
  "make info files for font-info! object"
  (ensure-directories-exist (get-font-dir-path))
  (dolist (c *chars*)
    (setf curr-glyph (find-glyph c font-loader))
    (let ((path (get-font-file-path c)))
      (generate-char-info-file path c)))
  (let ((path (get-font-info-file-path)))
    (generate-font-info-file path)))

(let ((ubuntumono-r-path (merge-pathnames "../font/ttf/UbuntuMono-R.ttf" (uiop/os:getcwd))))
  (defparameter *ubuntumono-r* (create-font-info! ubuntumono-r-path)))

(defun generate-ubuntumono-r ()
  (generate-font-info *ubuntumono-r*))
