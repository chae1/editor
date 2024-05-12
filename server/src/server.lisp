(ql:quickload "cl-ppcre")
(ql:quickload "lparallel")
(ql:quickload "bt-semaphore")
(ql:quickload "serapeum")
(ql:quickload "alexandria")
(ql:quickload "sb-concurrency")

(defpackage text-server
  (:use :common-lisp :my-utils :defobj :my-text :lparallel :bt :alexandria))

(in-package :text-server)

;; -1 1
;; 0 width
;; 0 height
;; font-size 10
;; em 1000
;; x-advance 500
;; y-advance 900
;; x-max 500
;; y-max 890

;; -1 + 2*10/1000 * (n-1)
;; -1 + 2*10/

(defparameter *msg-handle-table* (make-hash-table :test 'equalp))

(defun get-msg-handle (msg)
  (gethash msg *msg-handle-table*))

(defobjmacro set-msg-handle (init-msg user! text! variables &body body)
  `(setf (gethash ,init-msg *msg-handle-table*)
	 (objlambda (,user! ,text!)
	   (let ,(mapcar (lambda (el) (list `,el `(read-line ,socket-stream-in)))
		  variables)
	     ,@body))))

(defun send-msg (connect msg)
  (sb-bsd-sockets:socket-send connect msg (length msg)))

(defun send-new-line (connect)
  (let ((newline-buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend #\Newline newline-buf)
    (send-msg connect newline-buf)))

(defobjfun send-text (user! text!)
  (send-msg connect (with-output-to-string (stream) (format stream "draw begin~%")))
  (let ((msg (print-text user! text!)))
    (print msg)
    (print (length msg))
    (send-msg connect msg))
  (send-msg connect (with-output-to-string (stream) (format stream "draw end~%"))))

(defobjfun send-max-size (user!)
  (objlet* ((font! font)
	    (x-advance (get-default-x-advance user!))
	    (y-advance (get-y-advance user!))
	    (max-cursor-num 100))
    (send-msg connect (with-output-to-string (stream) (format stream "max-obj-num ~a~%" (+ max-cursor-num (ceiling (* (/ window-width x-advance) (/ window-height y-advance)))))))))

(defparameter *user-info-table* (make-hash-table :test 'eq))

(set-msg-handle "login" user! text! (username-in width-in height-in font-size-in)
  (setf state 'normal)
  (setf username username-in)
  (setf font (get-font "UbuntuMono-R"))
  (setf font-size (read-from-string font-size-in))
  (setf window-width (float (read-from-string width-in)))
  (setf window-height (float (read-from-string height-in)))
  (setf cursor-color #(0.0 0.2 0.0))
  (link-user user! text!)

  (format t "user login~%")
  ;; (format t "width ~a height ~a received.~%" width height)
  (print user!)

  (send-max-size user!)
  (send-text user! text!)
  
  ;; (print (print-text user! text!))
  )

;; key

(defun map-alist-to-table (alist table)
  (mapc (lambda (pair) (setf (gethash (car pair) table) (cdr pair)))
	alist))

(defparameter *normal-state-draw-key-table* (make-hash-table :test 'eq))

(defparameter *numbers* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *small-chars* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defparameter *normal-state-number-alist* (let ((i 47))
			       (mapcar (lambda (c) (cons (incf i) c)) *numbers*)))
(defparameter *normal-state-char-alist* (let ((i 64))
				   (mapcar (lambda (c) (cons (incf i) c)) *small-chars*)))
(defparameter *normal-state-other-draw-key-alist* '((32 . #\ ) (39 . #\') (44 . #\,) (45 . #\-) (46 . #\.) (47 . #\/) (59 . #\;) (60 . #\=) (91 . #\[) (92 . #\\) (93 . #\]) (96 . #\`)))

(map-alist-to-table *normal-state-number-alist* *normal-state-draw-key-table*)
(map-alist-to-table *normal-state-char-alist* *normal-state-draw-key-table*)
(map-alist-to-table *normal-state-other-draw-key-alist* *normal-state-draw-key-table*)

(defparameter *extend-state-draw-key-table* (make-hash-table :test 'eq))

(defparameter *extended-numbers* '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\())
(defparameter *large-chars* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
(defparameter *extend-state-number-alist* (let ((i 47))
			       (mapcar (lambda (c) (cons (incf i) c)) *extended-numbers*)))
(defparameter *extend-state-char-alist* (let ((i 64))
				   (mapcar (lambda (c) (cons (incf i) c)) *large-chars*)))
(defparameter *extend-state-other-draw-key-alist* '((32 . #\ ) (39 . #\") (44 . #\<) (45 . #\_) (46 . #\>) (47 . #\?) (59 . #\:) (60 . #\+) (91 . #\{) (92 . #\|) (93 . #\}) (96 . #\~)))

(map-alist-to-table *extend-state-number-alist* *extend-state-draw-key-table*)
(map-alist-to-table *extend-state-char-alist* *extend-state-draw-key-table*)
(map-alist-to-table *extend-state-other-draw-key-alist* *extend-state-draw-key-table*)

(set-msg-handle "key-event" user! text! (key action)
  ;; (send-msg connect (with-output-to-string (stream) (format stream "key ~a action ~a received.~%" key action)))
  (let ((key (read-from-string key))
	(action (read-from-string action)))
    (case key
      ;; shift
      (340 (case action
	     ((0 1) (case state
		      ('normal (setf state 'extend))
		      ('extend (setf state 'normal))))))
      ;; control
      (341 (case action
	     (0 (setf state 'normal))
	     (1 (setf state 'control))))
      ;; capslock
      (280 (case action
	     (1 (case state
		  ('normal (setf state 'extend))
		  ('extend (setf state 'normal))))))
      ;; enter
      (257 (case action
	     (1 (insert-new-line user! text!))))
      ;; backspace
      (259 (case action
	     (1 (backspace user! text!))))
      ;; tab
      (258 (case action
	     (1 )))
      ;; right
      (262 (case action
	     (1 (move-right user! text!))))
      ;; left
      (263 (case action
	     (1 (move-left user! text!))))
      ;; up
      (263 (case action
	     (1 (move-up user! text!))))
      ;; down
      (264 (case action
	     (1 (move-down user! text!))))
      ;; draw
      (otherwise (case action
		   (1 (progn
			(case state
			  ('normal (if-let ((char (gethash key *normal-state-draw-key-table*)))
				     (insert-char user! text! char)))
			  ('extend (if-let ((char (gethash key *extend-state-draw-key-table*)))
				     (insert-char user! text! char))))
			
			)))))
    (case action
      (1 (progn
	   (objdolist (user! user-list)
	     (send-text user! text!))
	   ;; (format t "------------------------------------------------~%")
	   ;; (print line-tree)
	   ;; (terpri)
	   )))
    )
  
  ;; (print cursor-list)
  ;; (print state)
  
  (send-new-line connect))

(set-msg-handle "resize-window" user! text! (width-in height-in)
  (let ((width (float (read-from-string width-in)))
	(height (float (read-from-string height-in))))    
    (setf window-width width)
    (setf window-height height)
    (send-max-size user!)
    (send-text user! text!)))

(defmacro my-thread (form exit-form &rest cases)
  `(sb-thread:make-thread
    (lambda ()
      (unwind-protect
	   (handler-case
	       ,form
	     ,@cases)
	,exit-form))))

(defobjfun handle-msg (init-msg user! text!)
  (let ((func (get-msg-handle init-msg)))
    (if func
	(funcall (get-msg-handle init-msg) user! text!)
	(format t "handle for msg ~a not defined~%" init-msg))))

(defobjfun handle-connect (connect socket-stream-in text!)
  (objlet* ((user! (make-user! :connect connect :socket-stream-in socket-stream-in)))
    (my-thread
     (loop
       (let ((init-msg (read-line socket-stream-in)))
	 (handle-msg init-msg user! text!)))
     
     ;; exit form
     (progn
       (format t "~%")
       (format t "unlinking user and text~%")
       (unlink-user user! text!)
       (format t "(handle-connect) closing connect ~a~%" connect)
       (sb-bsd-sockets:socket-close connect)
       (format t "(handle-connect) exiting handle-connect thread for ~a~%" connect))
     ;; handler cases
     (end-of-file (o)
		  ;; (format t "~a~%" o)
		  )
     (sb-int:simple-stream-error (o)))))

(defvar *socket* nil)
(defvar *text* nil)

(defun run-server (&key (ip #(127 0 0 1)) (port 20741))
  (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (setf *text* (create-text!))
  (let ((out *standard-output*))
    (my-thread
     (progn
       (setf (sb-bsd-sockets:sockopt-reuse-address *socket*) t)
       (setf (sb-bsd-sockets:non-blocking-mode *socket*) t)
       (sb-bsd-sockets:socket-bind *socket* ip port)
       (sb-bsd-sockets:socket-listen *socket* 1)

       (format out "server start listening to ~a~%" *socket*)
       ;; wait for new connect
       (loop
	 (if-let ((connect (sb-bsd-sockets:socket-accept *socket*)))
	   (let ((socket-stream-in (sb-bsd-sockets:socket-make-stream connect :input t)))
	     (format t "new connect~%")
	     (handle-connect connect socket-stream-in *text*)))))
     ;; exit form
     (progn
       (format out "(run-text-server!) exiting server thread~%"))
     ;; handler cases
     (sb-bsd-sockets:bad-file-descriptor-error (o)
					       ;; (format t "(run-text-server!) ~a~%" o)
					       )
     (sb-bsd-sockets:address-in-use-error (o)
					  ;; (format t "(run-text-server!) ~a~%" o)
					  )
     )))

(defun close-server ()
  (format t "(stop-text-server!) closing server listeing to ~a~%" *socket*)
  (sb-bsd-sockets:socket-close *socket*))
