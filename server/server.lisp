(ql:quickload "cl-ppcre")
(ql:quickload "lparallel")
(ql:quickload "bt-semaphore")
(ql:quickload "serapeum")
(ql:quickload "alexandria")

(defpackage text-server
  (:use :common-lisp :my-utils :defobj :my-text :lparallel :bt :alexandria))

(in-package :text-server)

(defobj text-server!
  (ip nil)
  (port nil)
  (socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
  (text-lock (make-lock) :type lock)
  (text-manager (create-text-manager!) :type text-manager!)
  (server nil :type (or null sb-thread:thread))
  (userinfos nil :type (or null cons)))

(let ((keys-size 10))
  (defobj state-keys!
    (state 'normal :type symbol)
    (keys (make-array keys-size :initial-element 0) :type array :read-only t))

  (defobj current-state!
    (state-keys (make-state-keys!) :type state-keys!)
    (temp-array (make-array keys-size :initial-element 0) :type array :read-only t)
    (size keys-size :type integer)
    (first-available-index 0 :type integer)))

(defobjfun one-key (current-state! key)
  (objlet* ((state-keys! state-keys))
    (do ((i 0 (1+ i)))
	((eq i size))
      (setf (aref keys i) 0))
    (setf (aref keys 0) key)
    (setf first-available-index 1)))

(defobjfun add-key (current-state! key)
  (objlet* ((state-keys! state-keys))
    (when (< first-available-index size)
      (setf (aref keys first-available-index) key)
      (incf first-available-index))))

(defobjfun remove-key (current-state! key)
  (objlet* ((state-keys! state-keys))  
    ;; copy
    (do ((i 0 (1+ i)))
	((eq i size))
      (setf (aref temp-array i) (aref keys i)))
    ;; copy
    (let ((j 0))
      (do* ((i 0 (1+ i)))
	   ((eq i size))
	(let ((curr-el (aref temp-array i)))
	  (when (not-eq curr-el key)
	    (setf (aref keys j) curr-el)
	    (incf j))))
      (setf first-available-index j)
      (do ((i j (1+ i)))
	  ((eq i size))
	(setf (aref keys i) 0)))))

(defobjfun init-key (current-state!)
  (objlet* ((state-keys! state-keys))
    (do ((i 0 (1+ i)))
	((eq i size))
      (setf (aref keys i) 0))
    (setf first-available-index 0)))

(defobj key-event!
  (key 0 :type integer)
  (action 0 :type integer))

(defobj userinfo!
  (username nil)
  (connect nil)
  (stream nil)
  (key-event (make-key-event!) :type key-event!)
  (current-state (make-current-state!) :type current-state!)
  (msg-handler nil :type (or null sb-thread:thread))
  )

(defmacro deftransition (name &body body)
  `(defobjfun ,name (userinfo!)
     (objlet* ((current-state! current-state)
	       (state-keys! state-keys)
	       (key-event! key-event))
       ,@body
       (format t "(transition) ~(~a~)~%" (quote ,name)))))

(let ((transition-table (make-hash-table :test 'equalp)))
  (defobjfun set-transition (key action transition)
    (objlet* ((key-event! (make-key-event! :key key :action action)))
      (setf (gethash key-event! transition-table) transition)))
  
  (defobjfun get-transition (key-event!)
    (gethash key-event! transition-table))

  (deftransition key-press
    (cond ((eq state 'normal) (one-key current-state! key))
	  ((eq state 'control) (add-key current-state! key))))

  (deftransition key-release
    (remove-key current-state! key))
  
  (deftransition ctrl-press
    (one-key current-state! key)
    (setf state 'control))

  (deftransition ctrl-release
    (init-key current-state!)
    (setf state 'normal))

  (set-transition 65 1 #'key-press)
  (set-transition 65 0 #'key-release)
  (set-transition 341 1 #'ctrl-press)
  (set-transition 341 0 #'ctrl-release)

  (defun get-transition-table ()
    transition-table))

(let ((char-map (make-hash-table)))
  ;; init
  ;; a-z
  (mapcar #'(lambda (key)
	      (setf (gethash key char-map) (code-char (+ key 32))))
	  (iota 26 :start 65))
  ;; 0-9
  (mapcar #'(lambda (key)
	      (setf (gethash key char-map) (code-char key)))
	  (iota 10 :start 48))

  (defun get-char (key)
    (gethash key char-map)))

(defmacro deffunc (name &body body)
  `(defobjfun ,name (text-server! userinfo!)
     (objlet* ((current-state! current-state)
	       (state-keys! state-keys)
	       (key-event! key-event))
       ,@body
       (format t "(func) ~(~a~)~%" (quote ,name)))))

(let ((function-table (make-hash-table :test 'equalp)))
  (defobjfun set-func (state keys func)
    (objlet* ((state-keys! (make-state-keys! :state state :keys keys)))
      (setf (gethash state-keys! function-table) func)))

  (defobjfun get-func (state-keys!)
    (gethash state-keys! function-table))

  (deffunc insert-key
    (let ((key-1 (aref keys 0)))
      (with-lock-held (text-lock)
	(insert-char text-manager username (get-char key))
	(print-text text-manager))))
  
  (set-func 'normal #(65 0 0 0 0 0 0 0 0 0) #'insert-key)
  
  )



(defobjfun handle-msg (text-server! userinfo! msg)
  (objlet* ((current-state! current-state))
    (case (char msg 0)
      (#\0 (progn
	     (setf username (read-line stream))
	     (format t "username : ~a~%" username)
	     (push userinfo! userinfos)
	     (init-cursors-manager! text-manager username)
	     (sb-bsd-sockets:socket-send connect "login succeeded
" nil)
	     (format t "~a~%" userinfo!)))

      (#\1 (progn
	     (objlet* ((key-event! key-event))
	       (setf key (parse-integer (read-line stream)))
	       (setf action (parse-integer (read-line stream)))
	       (format t "~%(~a) key : ~a, action : ~a~%" username key action))
	     (let ((transition (get-transition key-event)))
	       (when transition
		 (funcall transition userinfo!)))
	     
	     (let ((func (get-func state-keys)))
	       (when func
		 (funcall func text-server! userinfo!)))

	     (objlet* ((current-state! current-state)
		       (state-keys! state-keys))
	       (format t "(state) ~a~%(keys) ~a~%" state keys)))))))

(defobjfun run-msg-handler (text-server! userinfo!)
  (setf msg-handler
	(sb-thread:make-thread
	 (lambda ()
	   (unwind-protect
		(handler-case
		    (loop
		      (let ((msg (read-line stream)))
			(handle-msg text-server! userinfo! msg)))
		  (end-of-file (o) (format t "~a~%" o)))
	     (progn
	       (format t "(run-msg-handler) close connect~%")
	       (sb-bsd-sockets:socket-close connect)
	       ;; (remove-el userinfo! userinfos)
	       ;; (remove-cursors-manager! text-manager username)
	       (format t "(run-msg-handler) exiting msg-handler~%")))))))

(objlet* ((text-server! nil))
  (defobjfun run-text-server! (&optional (ip-in #(127 0 0 1)) (port-in 20741))
    (setq text-server! (make-text-server! :ip ip-in :port port-in))
    (init-text text-manager "temp path")
    (setf server
	  (sb-thread:make-thread
	   (lambda ()
	     (unwind-protect
		  (handler-case
		      (progn
			;; socket config
			(setf (sb-bsd-sockets:sockopt-reuse-address socket) nil)
			(setf (sb-bsd-sockets:non-blocking-mode socket) t)
			(sb-bsd-sockets:socket-bind socket ip port)
			(sb-bsd-sockets:socket-listen socket 1)
			;; wait for new connect
			(loop
			  (let ((connect (sb-bsd-sockets:socket-accept socket)))
			    (when connect
			      (format t "new connect~%")
			      (objlet* ((userinfo! (make-userinfo! :connect connect :stream (sb-bsd-sockets:socket-make-stream connect :input t :output t))))
				(run-msg-handler text-server! userinfo!))))))
		    
		    (sb-bsd-sockets:bad-file-descriptor-error (o) (format t "(run-text-server!) ~a~%" o))
		    (sb-bsd-sockets:address-in-use-error (o) (format t "(run-text-server!) ~a~%" o))))
	     (progn
	       (format t "(run-text-server!) exiting server~%"))))))

  (defun stop-text-server! ()
    (format t "(stop-text-server!) close socket~%")
    (sb-bsd-sockets:socket-close socket))

  (defun get-text-server! ()
    text-server!))
