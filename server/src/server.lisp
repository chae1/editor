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
  (defun read-line-from-socket ()
    (read-line socket-stream-in)))

(with-objs (user!)
  (defobjmacro set-msg-handle (init-msg variables &body body)
    `(setf (gethash ,init-msg *msg-handle-table*)
	   (lambda ()
	     (let ,(mapcar (lambda (el) (list el `(read-line-from-socket)))
		    variables)
	       ,@body)))))

(with-objs (user! text!)
  (set-msg-handle "login" (username-in width-in height-in font-size-in)
    (setf state 'normal)
    (setf username username-in)
    (setf curr-font (get-font "UbuntuMono-R"))
    (setf curr-font-size (float (read-from-string font-size-in)))
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

(defmacro run-thread (&body body)
  `(sb-thread:make-thread
    (lambda ()
      ,@body)))

(defobj key-combi!
  (shift nil :type boolean)
  (control nil :type boolean)
  (alt nil :type boolean)
  (capslock nil :type boolean)
  (key nil :type (or null integer)))

(defparameter *curr-key-combi* (make-array 0 :fill-pointer t :adjustable t))
(defparameter *curr-key-combi-fn-table* (make-hash-table :test #'equalp))




(with-objs (user!)
  (set-msg-handle "key-event" (key action)    
    ;; (send-msg connect (with-output-to-string (stream) (format stream "key ~a action ~a received.~%" key action)))
    (let ((key (read-from-string key))
	  (action (read-from-string action)))
      (format t "key ~a, action ~a received.~%" key action)
      
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
	       (1 (insert-new-line-after-cursors))))

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
	(265 (case action
	       (1 (move-cursors-up))))

	;; down
	(264 (case action
	       (1 (move-cursors-down))))

	;; otherwise insert character in text
	(otherwise (case action
		     (1 (case state
			  ('normal (if-let ((char (gethash key *normal-state-insert-key-table*)))
				     (flet ((insert-char ()
					      (format t "insert char ~a~%" char)
					      (insert-char-after-cursors char)))
				       (insert-char)
				       ;; (sb-thread:make-thread
				       ;; 	(objlambda (user!)
				       ;; 	  (handler-case
				       ;; 	      (progn
				       ;; 		(insert-char)
				       ;; 		(sleep 0.5))
				       ;; 	    (insert-char-stop (o)))
				       ;; 	  )
					
				       ;; 	:arguments user!)
				       )))
			  
			  ('extend (if-let ((char (gethash key *extend-state-insert-key-table*)))
				     (progn
				       (format t "insert char ~a~%" char)
				       (insert-char-after-cursors char)))))
		      ))))

      ;; send render message
      (case action
	(1 (objlet* ((text! text))
	     (objdolist (user! user-list)
	       (send-render-msg))
	     ;; (format t "~a~%" line-tree)
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

(with-objs (text!)
  (defun handle-connect (connect socket-stream-in)
    (objlet* ((user! (make-user! :connect connect :socket-stream-in socket-stream-in)))
      (unwind-protect
	   (handler-case
	       (loop
		(let ((init-msg (read-line socket-stream-in)))
		  (handle-msg init-msg)))
	     (end-of-file (o)
	       (format t "~a~%" o)
	       )
	     (sb-int:simple-stream-error (o)
	       (format t "~a~%" o)
	       ))
	(progn
	  ;; (format t "~%")
	  (format t "(handle-connect) ~a exiting~%" username)
	  (format t "(handle-connect) closing connect ~a~%" connect)
	  (sb-bsd-sockets:socket-close connect)
	  (format t "(handle-connect) unlinking user and text~%")
	  (unlink-user)
	  (format t "(handle-connect) exiting handle-connect thread for ~a~%" connect))))))

(defparameter *socket* nil)
(defparameter *text* nil)

(defun run-server (&key (ip #(127 0 0 1)) (port 20741))
  (unwind-protect
       (handler-case
	   (progn
	     (setf *socket* (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
	     (setf (sb-bsd-sockets:non-blocking-mode *socket*) nil)
	     (setf (sb-bsd-sockets:sockopt-reuse-address *socket*) t)

	     (sb-bsd-sockets:socket-bind *socket* ip port)
	     (sb-bsd-sockets:socket-listen *socket* 1)
	     
	     (format t "(run-server) start listening to ~a~%" *socket*)

	     (setf *text* (create-text!))
	     
	     ;; wait for new connect
	     (objlet* ((text! *text*))
	       (loop
		 (if-let ((connect (sb-bsd-sockets:socket-accept *socket*)))
		   (let ((socket-stream-in (sb-bsd-sockets:socket-make-stream connect :input t)))
		     (format t "new connect~%")
		     (handle-connect connect socket-stream-in))))))
	 (sb-bsd-sockets:bad-file-descriptor-error (o)
	   (format t "(run-server) ~a~%" o)
	   )
	 (sb-bsd-sockets:address-in-use-error (o)
	   (format t "(run-server) ~a~%" o)
	   ))
    (progn
      (format t "(run-server) closing socket~%")
      (sb-bsd-sockets:socket-close *socket*)
      (format t "(run-server) exiting server thread~%")
      ))

  (run-thread
    ))

(defun close-socket ()
  (format t "(close-socket) closing socket~%")
  (sb-bsd-sockets:socket-close *socket*))

(defun print-all-users ()
  (objlet* ((text! *text*))
    (objdolist (user! user-list)
      (format t "~a~%" username))))
