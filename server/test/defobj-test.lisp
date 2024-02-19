(in-package :defobj)

(defobj obj!
  (slot nil))

(defparameter *o1* (make-obj!))
(defparameter *o2* (make-obj!))

(defobjfun f1 (obj!1 obj!2 i)
  (setf slot2 i)
  (setf slot1 slot2)
  (print slot1)
  (print slot2))

(defmacro m1 (obj!1 obj!2 i)
  (let* ((slot1 `(slot ,obj!1))
         (slot2 `(slot ,obj!2)))
    `(progn
       (setf ,slot2 ,i)
       (setf ,slot1 ,slot2)
       (print ,slot1)
       (print ,slot2))))

(defobjmacro m2 (obj!1 obj!2 i)
  `(progn
       (setf ,slot2 ,i)
       (setf ,slot1 ,slot2)
       (print ,slot1)
       (print ,slot2)))

(export 'm3)
(defmacro m3 ()
  `(print *package* abc))
