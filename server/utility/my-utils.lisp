(defpackage :my-utils
  (:use :common-lisp))

(in-package :my-utils)

(export 'symbol-append)
(defun symbol-append (&rest symbols)
  (intern (apply #'concatenate 'string
                 (mapcar #'symbol-name symbols))))

(export 'sym-name-eq)
(defmacro sym-name-eq (sym-a sym-b)
  `(string-equal (symbol-name ,sym-a) (symbol-name ,sym-b)))

(export 'not-eq)
(defmacro not-eq (a b)
  `(not (eq ,a ,b)))

(export 'while)
(defmacro while (end-cond &body body)
  `(do nil
    ((not ,end-cond) 1)
     ,@body))

(export 'repeat)
(defmacro repeat (times &body body)
  (let ((var (gensym)))
    `(dotimes (,var ,times nil) ,@body)))

(export 'form)
(defmacro form (&body body)
  (let ((form-name (gensym))
        (new-body '()))
    (dolist (el body)
      (push `(push ,el ,form-name) new-body))
    `(let ((,form-name '()))
       ,@(nreverse new-body)
       (nreverse ,form-name))))

(export 'read-from-string-in-package)
(defmacro read-from-string-in-package (str pkg)
  `(let ((curr-pkg *package*))
     (eval `(in-package ,(package-name ,pkg)))
     (let ((sym (read-from-string ,str)))
       (eval `(in-package ,(package-name curr-pkg)))
       sym)))

;; (export 'clamp)
;; (declaim (inline clamp))
;; (defun clamp (number min max)
;;   (if (< number min)
;;       min
;;       (if (> number max)
;;           max
;;           number)))

(export 'sorted-push)
(defmacro sorted-push (el container fn-<=)
  (let ((curr (gensym))
        (next (gensym))
        (el-curr (gensym))
        (el-next (gensym)))
    `(progn
       (if ,container
           (if (funcall ,fn-<= ,el (car ,container))
               (push ,el ,container)
               (do* ((,curr ,container (cdr ,curr))
                     (,next (cdr ,curr) (cdr ,curr)))
                    ((eq ,next nil) (push ,el (cdr ,curr)))
                 (let ((,el-curr (car ,curr))
                       (,el-next (car ,next)))
                   (if (funcall ,fn-<= ,el ,el-next)
                       (progn
                         (push ,el (cdr ,curr))
                         (return))))))
           (push ,el ,container))
       ,container)))

(export 'remove-el)
(defmacro remove-el (el container)
  (let ((curr (gensym))
        (next (gensym)))
    `(if ,container
         (if (eq ,el (car ,container))
             (setf ,container (cdr ,container))
             (do* ((,curr ,container (cdr ,curr))
                   (,next (cdr ,curr) (cdr ,curr)))
                  ((eq nil ,curr) ,container)
               (if (eq ,el (car ,next))
                   (progn
                     (setf (cdr ,curr) (cdr ,next))
                     (return ,container)))))
         nil)))
