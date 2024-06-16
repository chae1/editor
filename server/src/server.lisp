(ql:quickload "cl-ppcre")
(ql:quickload "lparallel")
(ql:quickload "bt-semaphore")
(ql:quickload "serapeum")
(ql:quickload "alexandria")
(ql:quickload "sb-concurrency")

(defpackage text-server
  (:use :common-lisp :my-utils :defobj :my-text :lparallel :bt :alexandria))

(in-package :text-server)

(defmacro format-string (&rest rest)
  (let ((stream (symbol-append 'stream- (gensym))))
    `(with-output-to-string (,stream)
       (format ,stream ,@rest))))

(with-objs (user!)
  (defun send-msg (msg)
    (sb-bsd-sockets:socket-send connect msg (length msg))))

(with-objs (user!)
  (defun send-new-line ()
    (let ((newline-buf (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
    (vector-push-extend #\Newline newline-buf)
      (send-msg newline-buf))))

(with-objs (user!)
  (defun send-render-msg ()
    (send-msg (format-string "draw begin~%"))
    (send-msg (get-render-msg))
    (send-msg (format-string "draw end~%"))))

(defparameter *msg-handle-table* (make-hash-table :test 'equalp))

(defun get-msg-handle (msg)
  (gethash msg *msg-handle-table*))

(with-objs (user!)
  (defobjmacro set-msg-handle (init-msg variables &body body)
    `(setf (gethash ,init-msg *msg-handle-table*)
	   (lambda ()
	     (let ,(mapcar (lambda (el) (list el `(read-line socket-stream-in)))
		    variables)
	       ,@body)))))

(with-objs (user! text!)
  (set-msg-handle "login" (username-in width-in height-in font-size-in)
    (setf state 'normal)
    (setf username username-in)
    (setf font (get-font "UbuntuMono-R"))
    (setf font-size (read-from-string font-size-in))
    (setf window-width (float (read-from-string width-in)))
    (setf window-height (float (read-from-string height-in)))
    (setf cursor-color #(0.0 0.2 0.0))

    (link-user)
    (send-render-msg)

    (format t "user login~%")
    (format t "~a~%" user!)
    ;; (format t "width ~a height ~a received.~%" width height)
    ))

;; key input

(defun map-alist-to-table (alist table)
  (mapc (lambda (pair) (setf (gethash (car pair) table) (cdr pair)))
	alist))

(defparameter *normal-state-insert-key-table* (make-hash-table :test 'eq))

(defparameter *numbers* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *small-chars* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))

(defparameter *normal-state-number-alist*
  (let ((i 47))
    (mapcar (lambda (c) (cons (incf i) c)) *numbers*)))
(defparameter *normal-state-char-alist*
  (let ((i 64))
    (mapcar (lambda (c) (cons (incf i) c)) *small-chars*)))
(defparameter *normal-state-other-insert-key-alist*
  '((32 . #\ ) (39 . #\') (44 . #\,) (45 . #\-) (46 . #\.) (47 . #\/) (59 . #\;) (60 . #\=) (91 . #\[) (92 . #\\) (93 . #\]) (96 . #\`)))

(map-alist-to-table *normal-state-number-alist* *normal-state-insert-key-table*)
(map-alist-to-table *normal-state-char-alist* *normal-state-insert-key-table*)
(map-alist-to-table *normal-state-other-insert-key-alist* *normal-state-insert-key-table*)

(defparameter *extend-state-insert-key-table* (make-hash-table :test 'eq))

(defparameter *extended-numbers* '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\())
(defparameter *large-chars* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(defparameter *extend-state-number-alist*
  (let ((i 47))
    (mapcar (lambda (c) (cons (incf i) c)) *extended-numbers*)))
(defparameter *extend-state-char-alist*
  (let ((i 64))
    (mapcar (lambda (c) (cons (incf i) c)) *large-chars*)))
(defparameter *extend-state-other-insert-key-alist*
  '((32 . #\ ) (39 . #\") (44 . #\<) (45 . #\_) (46 . #\>) (47 . #\?) (59 . #\:) (60 . #\+) (91 . #\{) (92 . #\|) (93 . #\}) (96 . #\~)))

(map-alist-to-table *extend-state-number-alist* *extend-state-insert-key-table*)
(map-alist-to-table *extend-state-char-alist* *extend-state-insert-key-table*)
(map-alist-to-table *extend-state-other-insert-key-alist* *extend-state-insert-key-table*)

(with-objs (user!)
  (set-msg-handle "key-event" (key action)    
    ;; (send-msg connect (with-output-to-string (stream) (format stream "key ~a action ~a received.~%" key action)))
    (let ((key (read-from-string key))
	  (action (read-from-string action)))
      ;; 0: release, 1: push
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
	       (1 (insert-new-line-after-cursor))))

	;; backspace
	(259 (case action
	       (1 (delete-char-at-cursors))))

	;; tab
	(258 (case action
	       (1 )))

	;; right
	(262 (case action
	       (1 (move-cursors-right))))

	;; left
	(263 (case action
	       (1 (move-cursors-left))))

	;; up
	(263 (case action
	       (1 (move-cursors-up))))

	;; down
	(264 (case action
	       (1 (move-cursors-down))))

	;; otherwise insert character in text
	(otherwise (case action
		     (1 (progn
			  (case state
			    ('normal (if-let ((char (gethash key *normal-state-insert-key-table*)))
				       (insert-char-after-cursors char)))
			    ('extend (if-let ((char (gethash key *extend-state-insert-key-table*)))
				       (insert-char-after-cursors char)))))))))

      ;; send render message
      (case action
	(1 (objlet* ((text! text))
	     (objdolist (user! user-list)
	       (send-render-msg))
	     ;; (format t "------------------------------------------------~%")
	     ))))
    
    ;; (print cursor-list)
    ;; (print state)
    
    ))

(with-objs (user!)
  (set-msg-handle "resize-window" (width-in height-in)
    (let ((width (float (read-from-string width-in)))
	  (height (float (read-from-string height-in))))    
      (setf window-width width)
      (setf window-height height)
      (send-render-msg))))

(with-objs (user! text!)
  (defun handle-msg (init-msg)
    (if-let ((func (get-msg-handle init-msg)))
      (funcall func)
      (format t "handle for msg ~a not defined~%" init-msg))))

(defmacro thread-with-handler (form exit-form &rest cases)
  `(sb-thread:make-thread
    (lambda ()
      (unwind-protect
	   (handler-case
	       ,form
	     ,@cases)
	,exit-form))))

(with-objs (text!)
  (defun handle-connect (connect socket-stream-in)
    (objlet* ((user! (make-user! :connect connect :socket-stream-in socket-stream-in)))
      (thread-with-handler
       (loop
	 (let ((init-msg (read-line socket-stream-in)))
	   (handle-msg init-msg)))

       ;; exit form
       (progn
	 ;; (format t "~%")
	 (format t "unlinking user and text~%")
	 (unlink-user)
	 (format t "(handle-connect) closing connect ~a~%" connect)
	 (sb-bsd-sockets:socket-close connect)
	 (format t "(handle-connect) exiting handle-connect thread for ~a~%" connect))

       ;; handler cases
       (end-of-file (o)
		    (format t "~a~%" o)
		    )
       (sb-int:simple-stream-error (o)
				   (format t "~a~%" o)
				   )))))

(defvar *socket* nil)
(defvar *text* nil)

(defun run-server (&key (ip #(127 0 0 1)) (port 20741))
  (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (setf *text* (create-text!))
  (objlet* ((text! *text*)
	    (out *standard-output*))
    (thread-with-handler
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
	     (handle-connect connect socket-stream-in)))))
     ;; exit form
     (progn
       (format out "(run-text-server!) exiting server thread~%"))
     ;; handler cases
     (sb-bsd-sockets:bad-file-descriptor-error (o)
					       (format t "(run-text-server!) ~a~%" o)
					       )
     (sb-bsd-sockets:address-in-use-error (o)
					  (format t "(run-text-server!) ~a~%" o)
					  ))))

(defun close-server ()
  (format t "(stop-text-server!) closing server listeing to ~a~%" *socket*)
  (sb-bsd-sockets:socket-close *socket*))
