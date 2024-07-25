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

(with-objs (user!)
  (defun broadcast-render-msg ()
    (objlet* ((text! text))
      (objdolist (user! user-list)
	(send-render-msg)))))

(defparameter *msg-handle-table* (make-hash-table :test 'equalp))

(defun get-msg-handle (msg)
  (gethash msg *msg-handle-table*))

(with-objs (user!)
  (defun read-line-from-socket ()
    (read-line socket-stream-in)))

(defparameter *msg-handle-lock* (sb-thread:make-mutex :name "lock for sequential msg handling in server"))

(with-objs (user!)
  (defobjmacro set-msg-handle (init-msg variables &body body)
    `(setf (gethash ,init-msg *msg-handle-table*)
	   (lambda ()
	     (sb-thread:with-mutex (*msg-handle-lock*)
	       (let ,(mapcar (lambda (el) (list el `(read-line-from-socket)))
		      variables)
		 ,@body))))))

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

;; define key table

(defparameter *key-table* (make-hash-table :test 'eq))

(defun add-to-key-table (alist)
  (dolist (pair alist)
    (let ((key (car pair))
	  (char (cdr pair)))
      (setf (gethash char *key-table*) key))))

(defun add-to-key-table-2 (keys chars)
  (mapc #'(lambda (key char)
	    (setf (gethash char *key-table*) key))
	keys chars))

(defun get-key (char)
  (gethash char *key-table*))


(defparameter *num-keys* (iota 10 :start 48))
(defparameter *normal-nums* '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
(defparameter *extended-nums* '(#\) #\! #\@ #\# #\$ #\% #\^ #\& #\* #\())

(add-to-key-table-2 *num-keys* *normal-nums*)
(add-to-key-table-2 *num-keys* *extended-nums*)

(defparameter *char-keys* (iota 26 :start 65))
(defparameter *normal-chars* '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
(defparameter *extended-chars* '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))

(add-to-key-table-2 *char-keys* *normal-chars*)
(add-to-key-table-2 *char-keys* *extended-chars*)

(defparameter *other-normal-chars-alist* '((32 . #\ ) (39 . #\') (44 . #\,) (45 . #\-) (46 . #\.) (47 . #\/) (59 . #\;) (61 . #\=) (91 . #\[) (92 . #\\) (93 . #\]) (96 . #\`)))
(defparameter *other-extended-chars-alist* '((32 . #\ ) (39 . #\") (44 . #\<) (45 . #\_) (46 . #\>) (47 . #\?) (59 . #\:) (61 . #\+) (91 . #\{) (92 . #\|) (93 . #\}) (96 . #\~)))

(add-to-key-table *other-normal-chars-alist*)
(add-to-key-table *other-extended-chars-alist*)

(defparameter *other-normal-chars* (mapcar #'(lambda (pair) (cdr pair)) *other-normal-chars-alist*))
(defparameter *other-extended-chars* (mapcar #'(lambda (pair) (cdr pair)) *other-extended-chars-alist*))


(defmacro with-return-val (binding-form &body body)
  `(objlet* (,binding-form)
     ,@body
     ,(car binding-form)))

;; fn-container! definition

(defobj fn-container!
  (entry-table (make-hash-table :test #'equalp) :type hash-table)  
  (curr-table nil :type (or null hash-table)))

(defun create-fn-container! ()
  (with-return-val (fn-container! (make-fn-container!))
    (setf curr-table entry-table)))

(with-objs (fn-container!)
  (defobjmacro get-next-elem (key-combi!)
    `(gethash ,key-combi! curr-table)))

(with-objs (fn-container!)
  (defobjfun set-next-elem (key-combi! elem)
    (setf (get-next-elem key-combi!) elem)))

(with-objs (fn-container!)
  (defobjfun get-next-table (key-combi!)
    (or (get-next-elem key-combi!)
	(setf (get-next-elem key-combi!)
	      (make-hash-table :test #'equalp)))))

(with-objs (fn-container!)
  (defobjfun advance-curr-table (key-combi!)
    (setf curr-table (get-next-table key-combi!))))

;; create fn-container!

(defparameter *key-combi-chain-fn-container* (create-fn-container!))

(objlet* ((fn-container! *key-combi-chain-fn-container*))
  (maphash #'(lambda (k v)
	       (format t "~a~%~a~%~%" k v))
	   entry-table))

;; define set-key-combi-fn

(defmacro case-str (token &body clauses)
  `(cond
     ,@(mapcar #'(lambda (clause)
		  `(,(if (eq (car clause) 'otherwise)
			 t
			 `(string-equal ,token ,(car clause)))
		    ,@(cdr clause))) clauses)))

(defmacro do-token ((token regex str) &body body)
  `(dolist (,token (cl-ppcre:split ,regex ,str))
     ,@body))

(defun create-key-combi! (key-combi-str)
  (with-return-val (key-combi! (my-text:make-key-combi!))
    (do-token (key-str "-" key-combi-str)
      (case-str key-str
	("control" (setf control t))
	("shift" (setf shift t))
	("alt" (setf alt t))
	("capslock" (setf capslock t))
	(otherwise (setf key (read-from-string key-str)))))))

(defmacro do-token-with-last-form ((token regex str) main-form last-form)
  (let ((token-list (symbol-append 'token-list (gensym))))
    `(do* ((,token-list (cl-ppcre:split ,regex ,str) (cdr ,token-list))
	   (,token (car ,token-list) (car ,token-list)))
	  ((eq (cdr ,token-list) nil)
	   ,last-form)
       ,main-form)))

(with-objs (fn-container!)
  (defun set-key-combi-fn (key-combi-chain-str fn)
    (do-token-with-last-form
	(key-combi-str "\\ " key-combi-chain-str)
	;; main-form
	(objlet* ((key-combi! (create-key-combi! key-combi-str)))
	  (advance-curr-table key-combi!))
	;; last-form
	(objlet* ((key-combi! (create-key-combi! key-combi-str)))
	  (set-next-elem key-combi! fn)
	  (setf curr-table entry-table)))))

;; set key combi chain functions

(defun set-key-combi-fns (fn key-str &rest attach-strs)
  (objlet* ((fn-container! *key-combi-chain-fn-container*))
    (dolist (attach-str attach-strs)
      (set-key-combi-fn (concatenate 'string attach-str key-str) fn))))

(defmacro set-insert-key-fns (chars &rest attach-strs)
  `(dolist (char ,chars)
     (set-key-combi-fns
      #'(lambda ()
	  (insert-char-after-cursors char)
	  (broadcast-render-msg))
      (write-to-string (get-key char)) ,@attach-strs)))

(progn
  (set-insert-key-fns *normal-chars* "" "shift-capslock-") ;; normal characters
  (set-insert-key-fns *extended-chars* "shift-" "capslock-") ;; extended characters
  (set-insert-key-fns *normal-nums* "" "capslock-") ;; normal numbers
  (set-insert-key-fns *extended-nums* "shift-" "shift-capslock-") ;; extended numbers
  (set-insert-key-fns *other-normal-chars* "" "capslock-") ;; other normal characters
  (set-insert-key-fns *other-extended-chars* "shift-" "shift-capslock-") ;; other extended characters
  )

(defmacro set-single-key-fns (fn key-str &rest attach-strs)
  `(set-key-combi-fns #'(lambda ()
			  (funcall ,fn)
			  (broadcast-render-msg))
		      ,key-str ,@attach-strs))

(progn
  (set-single-key-fns #'insert-new-line-after-cursors "257" "" "capslock-") ;; enter
  (set-single-key-fns #'delete-char-at-cursors "259" "" "capslock-") ;; backspace
  (set-single-key-fns #'move-cursors-right "262" "" "capslock-") ;; right
  (set-single-key-fns #'move-cursors-left "263" "" "capslock-")  ;; left
  (set-single-key-fns #'move-cursors-down "264" "" "capslock-") ;; down
  (set-single-key-fns #'move-cursors-up "265" "" "capslock-") ;; up
  )

;; define update-key-combi!

(defmacro setf-case (place case-form)
  `(case ,(car case-form)
     ,@(mapcar #'(lambda (clause)
		  (form (car clause) `(setf ,place ,(cadr clause))))
	(cdr case-form))))

(with-objs (user!)
  (defun update-key-combi! (key-in action)
    (objlet* ((key-combi! curr-key-combi))
      (case key-in
	(340 (setf-case shift (action (0 nil) (1 t))))
	(341 (setf-case control (action (0 nil) (1 t))))
	(342 (setf-case alt (action (0 nil) (1 t))))
	(280 (setf-case capslock (action (1 (not capslock)))))
	(otherwise (setf-case key (action (0 nil) (1 key-in))))))))

;; set key-event handle

(with-objs (user!)
  (set-msg-handle "key-event" (key action)
    (objlet* ((fn-container! *key-combi-chain-fn-container*))
      ;; (send-msg connect (with-output-to-string (stream) (format stream "key ~a action ~a received.~%" key action)))
      (let ((key (read-from-string key))
	    (action (read-from-string action)))
	(format t "key ~a, action ~a received.~%" key action)
	(update-key-combi! key action)
	
	(when (eq action 1)
	  (format t "~a~%" curr-key-combi)
	  (let ((next-elem (get-next-elem curr-key-combi)))
	    (cond ((hash-table-p next-elem)
		   ;; table
		   (setf curr-table next-elem))
		  ((functionp next-elem)
		   ;; function
		   (funcall next-elem)
		   (setf curr-table entry-table))
		  (t
		   ;; otherwise
		   (setf curr-table entry-table))))
	  
	  )
	
	)))
    )



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
    (sb-thread:make-thread
     (objlambda (text!)
       (objlet* ((user! (make-user! :connect connect :socket-stream-in socket-stream-in)))
	 (unwind-protect
	      (handler-case
		  (loop
		    (let ((init-msg (read-line socket-stream-in)))
		      (handle-msg init-msg)))
		(end-of-file (o) (format t "~a~%" o))
		(sb-int:simple-stream-error (o) (format t "~a~%" o)))

	   (progn
	     ;; (format t "~%")
	     (format t "(handle-connect) ~a exiting~%" username)
	     (format t "(handle-connect) closing connect ~a~%" connect)
	     (sb-bsd-sockets:socket-close connect)
	     (format t "(handle-connect) unlinking user and text~%")
	     (unlink-user)
	     (format t "(handle-connect) exiting handle-connect thread for ~a~%" connect)))))
     
     :arguments text!)))

(defparameter *socket* nil)
(defparameter *text* nil)

(export 'run-server)
(defun run-server (&key (ip #(127 0 0 1)) (port 100))
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
	       (if-let ((connect (sb-bsd-sockets:socket-accept *socket*)))
		 (let ((socket-stream-in (sb-bsd-sockets:socket-make-stream connect :input t)))
		   (format t "new connect~%")
		   (handle-connect connect socket-stream-in)))
	       
	       (loop
		 (if-let ((connect (sb-bsd-sockets:socket-accept *socket*)))
		  (let ((socket-stream-in (sb-bsd-sockets:socket-make-stream connect :input t)))
		    (format t "new connect~%")
		    (handle-connect connect socket-stream-in))))))
	 
	 (sb-bsd-sockets:bad-file-descriptor-error (o) (format t "(run-server) ~a~%" o))
	 (sb-bsd-sockets:address-in-use-error (o) (format t "(run-server) ~a~%" o))
	 (error (o) (format t "(run-server) ~a~%" o)))

    (progn
      (format t "(run-server) closing socket~%")
      (close-socket)
      (format t "(run-server) exiting server thread~%"))))

(defun close-socket ()
  (format t "(close-socket) closing socket~%")
  (sb-bsd-sockets:socket-close *socket*))

(defun print-all-users ()
  (objlet* ((text! *text*))
    (objdolist (user! user-list)
      (format t "~a~%" username))))
