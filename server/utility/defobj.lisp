(defpackage defobj
  (:use :common-lisp :my-utils))

(in-package :defobj)

;; defobj-info : package -> obj-info
;; obj-info : obj-name -> slot-names
(defvar *defobj-info* (make-hash-table :test 'equal))

(export 'print-*defobj-info*)
(defun print-*defobj-info* ()
  (format t "obj-name : slot-names~%")
  (maphash #'(lambda (k1 v1)
               (format t "~%~s~%" k1)
               (maphash #'(lambda (k2 v2)
                            (format t "~s : ~s~%" k2 v2)) v1)) *defobj-info*))

(defun get-obj-info (package)
    (or (gethash package *defobj-info*)
        (setf (gethash package *defobj-info*) (make-hash-table :test 'equal))))

(defun set-slot-names (obj slot-names)
  (let* ((obj-info (get-obj-info (symbol-package obj)))
         (obj-name (symbol-name obj)))
    (setf (gethash obj-name obj-info) slot-names)))

(defun get-slot-names (obj)
  (let* ((obj-info (get-obj-info obj))
         (obj-name (symbol-name obj)))
    (gethash obj-name obj-info)))

(export 'defobj)
(defmacro defobj (obj &body slot-forms)
  "(defobj line!
     (char-list nil :type my-list:multi-cursor-list!)
     (iter-cursor nil :type my-list:cursor!))
   ->
   (defstruct (line! (:conc-name nil))
     (line!-char-list ...)
     (line!-iter-cursor ...))"
  (let* ((conc-name (concatenate 'string (string obj) "-"))
	 (slot-names (mapcar #'(lambda (slot-form) (concatenate 'string conc-name (string (car slot-form)))) slot-forms)))
    (set-slot-names obj slot-names))
  `(progn
     (export (quote ,obj))
     (defstruct (,obj (:conc-name ,(symbol-append obj '-))) ,@slot-forms)))

(export 'get-bindings)
(defun get-bindings (obj)
  (let* ((obj-pkg (symbol-package obj))
         (obj-name (symbol-name obj))
         (obj-info (get-obj-info obj-pkg))
         (len (length obj-name))
         (binding-pairs '())
         (bound-objs '()))
    (block find-match
      (do ((width len (- width 1)))
          ((< width 1))
        (do* ((i 0 (+ i 1))
              (j (+ i width) (+ i width)))
             ((> j len))
          (let* ((test-str (subseq obj-name i j))
                 (matched-slot-names (gethash test-str obj-info)))
            (if matched-slot-names
                (let ((bound-obj obj)
                      (prefix (subseq obj-name 0 i))
                      (postfix (subseq obj-name j len)))
                  ;; push obj binding
                  (push (cons obj-name bound-obj) binding-pairs)
                  (push bound-obj bound-objs)
                  ;; push obj nickname (prefix or postfix) binding
                  (cond
		    ;; prefix
		    ((not (or (equal prefix "") (equal prefix "-")))
                         (let* ((last-i (- (length prefix) 1)) 
                                (last-char (aref prefix last-i))
                                (clean-prefix (if (eq last-char #\-)
                                                  (subseq prefix 0 last-i)
                                                  prefix)))
                           (if (symbolp (read-from-string-in-package clean-prefix obj-pkg))
                               (push (cons clean-prefix bound-obj) binding-pairs))))
		    ;; postfix
                    ((not (or (equal postfix "") (equal postfix "-"))) 
                     (let* ((first-char (aref postfix 0))
                            (clean-postfix (if (eq first-char #\-)
                                               (subseq postfix 1 (length postfix))
                                               postfix)))
                       (if (symbolp (read-from-string-in-package clean-postfix obj-pkg))
                           (push (cons clean-postfix bound-obj) binding-pairs)))))
                  ;; push prefix or postfix attached slot bindings
                  (dolist (matched-slot-name matched-slot-names)
                    (let* ((slot-name (concatenate 'string prefix (subseq matched-slot-name (1+ (- (length obj-name) (length prefix) (length postfix)))) postfix))
                           (bound-slot (list (read-from-string-in-package matched-slot-name obj-pkg) bound-obj)))
                      (push (cons slot-name bound-slot) binding-pairs)))
                  (return-from find-match)))))))
    (values binding-pairs bound-objs)))

(defparameter *binding-pairs* '())
(defparameter *bound-objs* '())
(defparameter *objss* '())

(defun add-obj-bindings (obj)
  (if (symbolp obj)
      (multiple-value-bind (new-pairs new-objs) (get-bindings obj)
        (dolist (pair (nreverse new-pairs))
          (push pair *binding-pairs*))
        (dolist (obj (nreverse new-objs))
          (push obj (car *objss*))
          (push obj *bound-objs*)
          ;; (format t "add ")
          ;; (print-*objss*)
          ))))

(defun remove-current-bindings ()
  (let ((objs (pop *objss*)))
    (while objs
      (let ((obj (pop objs)))
        (pop *bound-objs*)
        (while *binding-pairs*
          (let ((pair (pop *binding-pairs*)))
            (if (eq obj (cdr pair))
                (progn
                  (while (eq obj (cdar *binding-pairs*))
                    (pop *binding-pairs*))
                  (return))))))))
  ;; (format t "remove ")
  ;; (print-*objss*)
  )

(defmacro with-new-obj-bindings-slot (&body body)
  (if body
      (let ((last-index (1- (length body))))
        (setf (nth last-index body) (let ((var (gensym)))
                                      `(let ((,var ,(car (last body))))
                                         (remove-current-bindings)
                                         ,var)))
        `(progn
           (push nil *objss*)
           ;; (format t "new binding ")
           ;; (print-*objss*)
           ,@body))
      '()))

(defun print-*objss* ()
  (format t "*objss* : ~s~%" *objss*))

(defun modify-form (form)
  (cond ((listp form) (let ((first-el (car form)))
                        (if (and (symbolp first-el)
                                 (find-if #'(lambda (el) (string-equal first-el el))
                                          '(defobjfun
                                            objlet*
                                            objdo*
                                            objdolist
                                            defobjmacro
                                            objmacrolet
                                            objlambda)))

                            (macroexpand-1 form)
                            (mapcar #'modify-form form))))
        ((and (symbolp form) (not (keywordp form)))
         (dolist (pair *binding-pairs* form)
           (if (string-equal form (car pair))
               (return (cdr pair)))))
        (t form)))

(defun macro-modify-form (form)
  (cond ((listp form)
         (mapcar #'macro-modify-form form))
        ((and (symbolp form) (not (keywordp form)))
         (dolist (pair *binding-pairs* form)
           (if (string-equal form (car pair))
               (return (cdr pair)))))
        (t form)))

(export 'defobjfun)
(defmacro defobjfun (fun objs &body body)
  (with-new-obj-bindings-slot
    (dolist (obj objs)
      (add-obj-bindings obj))
    ;; (format t "~%defobjfun binding-pairs : ~%~s~%" *binding-pairs*)
    ;; (format t "~%defobjfun bound-objs : ~%~s~%" *bound-objs*)
    `(progn
       (export (quote ,fun))
       (defun ,fun ,(modify-form objs) ,@(modify-form body)))))

(export 'objlet*)
(defmacro objlet* (bindings &body body)
  (with-new-obj-bindings-slot
    (let ((new-bindings '()))
      (dolist (binding bindings)
        (let ((new-binding '()))
          (push (modify-form (cadr binding)) new-binding)
          (add-obj-bindings (car binding))
          (push (modify-form (car binding)) new-binding)
          (push new-binding new-bindings)))
      (setq new-bindings (nreverse new-bindings))
      ;; (format t "~%objlet* binding-pairs : ~%~s~%" *binding-pairs*)
      ;; (format t "~%objlet* bound-objs : ~%~s~%" *bound-objs*)
      `(let* ,new-bindings
         ,@(modify-form body)))))

(export 'objdo*)
(defmacro objdo* (bindings endlist &body body)
  (with-new-obj-bindings-slot
    (let ((new-bindings '()))
      (dolist (binding bindings)
        (add-obj-bindings (car binding))
        (push (modify-form binding) new-bindings))
      (setq new-bindings (nreverse new-bindings))
      ;; (format t "~%objdo* binding-pairs : ~%~s~%" *binding-pairs*)
      ;; (format t "~%objdo* bound-objs : ~%~s~%" *bound-objs*)
      `(do* ,new-bindings ,(modify-form endlist) ,@(modify-form body)))))

(export 'objdolist)
(defmacro objdolist ((obj list) &body body)
  (with-new-obj-bindings-slot
    (add-obj-bindings obj)
    ;; (format t "~%objdolist binding-pairs : ~%~s~%" *binding-pairs*)
    ;; (format t "~%objdolist bound-objs : ~%~s~%" *bound-objs*)
    `(dolist (,(modify-form obj) ,(modify-form list)) ,@(modify-form body))))

(defun bind-all-objs-in (lambda-list)
  (dolist (form lambda-list)
    (if (listp form)
        (bind-all-objs-in form)
        (if (symbolp form)
            (add-obj-bindings form)))))

;; not used
(defun get-let-bindings ()
  (let ((let-bindings '()))
    (dolist (pair *binding-pairs*)
      (let ((from (car pair))
            (to (cdr pair)))
            (push (form (read-from-string from)
                    (if (listp to)
                        (form 'form `(quote ,(car to)) (cadr to))
                        (form 'form )))
                    let-bindings)))
    let-bindings))

(export 'defobjmacro)
(defmacro defobjmacro (name lambda-list &body body)
  (with-new-obj-bindings-slot
    (bind-all-objs-in lambda-list)

    ;; (format t "~%defobjmacro binding-pairs : ~%~s~%" *binding-pairs*)
    ;; (format t "~%defobjmacro bound-objs : ~%~s~%" *bound-objs*)
    ;; (format t "~%defobjmacro objss : ~%~s~%" *objss*)
    ;; (format t "~%let-bindings : ~a~%" (get-let-bindings))
    `(progn
       (export (quote ,name))
       ,(form 'defmacro name lambda-list
	  (form 'let* (let ((let*-bindings '()))
			(do ((objs (car *objss*))
                             (binding-pairs *binding-pairs*))
                            ((not objs))
			  (let ((obj (car objs))
				(binding-pair (car binding-pairs)))
                            (if (eq obj (cdr binding-pair))
				(progn
				  (setq objs (cdr objs))
				  (setq binding-pairs (cdr binding-pairs)))
				(progn
				  (push (form (read-from-string (car binding-pair))
					  (form 'form `(quote ,(cadr binding-pair)) obj)) let*-bindings)
				  (setq binding-pairs (cdr binding-pairs))))))
			let*-bindings)
            (macro-modify-form (car body))))))
       )

(export 'objmacrolet)
(defmacro objmacrolet (definitions &body body)
  (form 'macrolet
    (mapcar (lambda (definition)
              (let ((name (car definition))
                    (lambda-list (cadr definition))
                    (body (cddr definition)))
                (with-new-obj-bindings-slot
                  (bind-all-objs-in lambda-list)
                  (form name lambda-list
                    (form 'let* (let ((let*-bindings '()))
                                  (do ((objs (car *objss*))
                                       (binding-pairs *binding-pairs*))
                                      ((not objs))
                                    (let ((obj (car objs))
                                          (binding-pair (car binding-pairs)))
                                      (if (eq obj (cdr binding-pair))
                                          (progn
                                            (setq objs (cdr objs))
                                            (setq binding-pairs (cdr binding-pairs)))
                                          (progn
                                            (push (form (read-from-string (car binding-pair))
                                                    (form 'form `(quote ,(cadr binding-pair)) obj)) let*-bindings)
                                            (setq binding-pairs (cdr binding-pairs))))))
                                  let*-bindings)
                      (modify-form (car body))))))) definitions)
    (modify-form (car body))))

(export 'objlambda)
(defmacro objlambda (objs &body body)
  (with-new-obj-bindings-slot
    (dolist (obj objs)
      (add-obj-bindings obj))
    ;; (format t "~%defobjfun binding-pairs : ~%~s~%" *binding-pairs*)
    ;; (format t "~%defobjfun bound-objs : ~%~s~%" *bound-objs*)
    `(lambda ,(modify-form objs) ,@(modify-form body))))


(export 'export-symbols)
(defmacro export-symbols (pkg &rest objs)
  (let ((pkg-curr *package*)
        (objs-n (mapcar #'(lambda (el) (eval el)) objs))
        (obj (gensym)))
    `(progn
       (in-package ,pkg)
       (dolist (,obj ',objs-n)
         (export (read-from-string (string ,obj))))
       (in-package ,(package-name pkg-curr)))))
