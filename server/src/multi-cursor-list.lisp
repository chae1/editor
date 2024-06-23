(defpackage my-list
  (:use :common-lisp :my-utils :defobj))

(in-package :my-list)

(export 'list-node!)
(defobj list-node!
  (data nil)
  (prev-node nil :type (or list-node! null))
  (next-node nil :type (or list-node! null)))

(defmethod print-object ((node list-node!) out)
  (print-unreadable-object (node out :type t)
    (format out "(data = ~s)" (list-node!-data node))))

(export 'multi-cursor-list!)
(export 'sorted-cursors)
(defobj multi-cursor-list!
  (size 0 :type integer)
  (head nil :type (or null list-node!))
  (tail nil :type (or null list-node!))
  (iter-cursor nil :type (or null cursor!))
  (sorted-cursors nil :type (or null cons)))

(export 'cursor!)
(defobj cursor!
  (curr-index nil :type (or integer null))
  (curr-node nil :type (or list-node! null)))

(export 'cursor!-<=)
(declaim (inline cursor!-<=))
(defobjfun cursor!-<= (cursor!-1 cursor!-2)
  (<= curr-index-1 curr-index-2))

(export 'create-cursor!)
(defobjfun create-cursor! (l-multi-cursor-list! index)
  (if (and (<= index l-size) (>= index 0))
      (make-cursor! :curr-index index :curr-node (get-node-by-index l index))
      (error "my-list:create-cursor! index out of bound.~%")))

(export 'create-multi-cursor-list!)
(defun create-multi-cursor-list! ()
  (objlet* ((multi-cursor-list! (make-multi-cursor-list!)))
    (setf head (make-list-node! :data "head"))
    (setf tail (make-list-node! :data "tail"))
    (setf (list-node!-next-node head) tail)
    (setf (list-node!-prev-node tail) head)

    (objlet* ((cursor! (make-cursor! :curr-index 0 :curr-node head)))
      (setf iter-cursor cursor!)
      (sorted-push cursor! sorted-cursors #'cursor!-<=))

    multi-cursor-list!))

(export 'do-cursor)
(defobjmacro do-cursor ((cursor! multi-cursor-list!) &body body)
  `(objdolist (,cursor! ,sorted-cursors)
     ,@body))

;; (maphash #'(lambda (,user-name sorted-cursors)
;;                 (objdolist (,cursor! sorted-cursors)
;;                   ,@body))
;;     (sorted-cursors-table ,multi-cursor-list!))

(defmethod print-object ((list multi-cursor-list!) out)
  (print-unreadable-object (list out :type t)
    (format out "(size = ~d, sorted-cursors at" (multi-cursor-list!-size list))
    (do-cursor (cursor! list)
      (format out " ~a" curr-index))
    (format out ")"))

  (objlet* ((multi-cursor-list! list))
    (objdo* ((list-node! head next-node))
	((eq list-node! nil))
      (format out "~%")
      (print-object list-node! out)

      (objdolist (cursor! sorted-cursors)
	(if (eq list-node! curr-node)
            (progn
              (format out " ^")
              (if (eq iter-cursor cursor!)
                  (format out "l-iter")
                  (format out "l-user"))))))))

(export 'get-node-by-index)
(defobjfun get-node-by-index (l-multi-cursor-list! index)
  (if (and (<= index l-size) (>= index 0))
      (objlet* ((ret-list-node! l-head))
        (repeat index
                (setf ret ret-next-node))
        ret)
      (error "Requested index is greater than size of list.")))

(export 'insert-data-after-cursor)
(defobjfun insert-data-after-cursor (l-multi-cursor-list! cursor! data)
  (let ((curr-next-node (list-node!-next-node curr-node))
        (new-node (make-list-node! :data data)))
    ;; insert new-node in the list
    (setf (list-node!-next-node curr-node) new-node)
    (setf (list-node!-prev-node new-node) curr-node)
    (setf (list-node!-next-node new-node) curr-next-node)
    (setf (list-node!-prev-node curr-next-node) new-node)
    ;; update list
    (incf l-size)
    ;; update all sorted-cursors greater than current cursor
    (do-cursor (iter-cursor! l)
      (if (> iter-curr-index curr-index)
          (incf iter-curr-index)))
    ;; update current node and index
    (setf curr-node new-node)
    (incf curr-index)
    l))

(export 'delete-data-at-cursor)
(defobjfun delete-data-at-cursor (l-multi-cursor-list! cursor!)
  (if (not-eq curr-node l-head)
      (let ((curr-prev-node (list-node!-prev-node curr-node))
            (curr-next-node (list-node!-next-node curr-node)))
        ;; delete curr node in the list
        (setf (list-node!-next-node curr-prev-node) curr-next-node)
        (setf (list-node!-prev-node curr-next-node) curr-prev-node)
        ;; update list
        (decf l-size)
        ;; update all sorted-cursors greater than or equal to current cursor
        (do-cursor (iter-cursor! l)
          (cond ((> iter-curr-index curr-index)
                 (decf iter-curr-index))
                ((= iter-curr-index curr-index)
                 (progn
                   (decf iter-curr-index)
                   (setf iter-curr-node curr-prev-node)))))))l)

(export 'move-cursor-to-prev)
(defobjfun move-cursor-to-prev (multi-cursor-list! cursor!)
  (if (not-eq curr-node head)
      (progn
        (setf curr-node (list-node!-prev-node curr-node))
        (decf curr-index)))
  cursor!)

(export 'move-cursor-to-next)
(defobjfun move-cursor-to-next (multi-cursor-list! cursor!)

  (if (not-eq (list-node!-next-node curr-node) tail)
      (progn
        (setf curr-node (list-node!-next-node curr-node))
        (incf curr-index)))
  cursor!)

(export 'move-cursor-to-head)
(defobjfun move-cursor-to-head (multi-cursor-list! cursor!)
  (setf curr-node head)
  (setf curr-index 0)
  cursor!)

(export 'move-cursor-to-last)
(defobjfun move-cursor-to-last (multi-cursor-list! cursor!)
  (setf curr-node (list-node!-prev-node tail))
  (setf curr-index size)
  cursor!)
 
(export 'move-cursor-to-index)
(defobjfun move-cursor-to-index (multi-cursor-list! cursor! index)
  (let ((new-index (cond ((< index 0) 0)
                         ((> index size) size)
                         (t index))))
    (move-cursor-to-head multi-cursor-list! cursor!)
    (repeat new-index
      (move-cursor-to-next multi-cursor-list! cursor!)))
  cursor!)

(export 'split-list-after-cursor)
(defobjfun split-list-after-cursor (multi-cursor-list! cursor!)
  (if (eq cursor! iter-cursor)
      (error "iter-cursor is used to split list"))

  (move-cursor-to-head multi-cursor-list! iter-cursor)
  
  (objlet* ((new-multi-cursor-list! (make-multi-cursor-list!))
            (curr-next (list-node!-next-node curr-node)))

    (setf new-head (make-list-node! :data "head"))
    (setf new-tail (make-list-node! :data "tail"))

    ;; change node relations
    (setf (list-node!-next-node new-head) curr-next)
    (setf (list-node!-prev-node curr-next) new-head)
    (setf (list-node!-next-node curr-node) new-tail)
    (setf (list-node!-prev-node new-tail) curr-node)

    ;; modify head tail
    (let ((temp-tail tail))
      (setf tail new-tail)
      (setf new-tail temp-tail))

    ;; modify size
    (setf new-size (- size curr-index))
    (setf size curr-index)

    (sort sorted-cursors #'cursor!-<=)

    ;; set sorted-cursors
    (do* ((curr-sorted-cursors sorted-cursors (cdr curr-sorted-cursors)))
         ((eq curr-sorted-cursors nil))
      (let* ((cursor (car curr-sorted-cursors)))
        (if (eq cursor cursor!)
            (progn
              ;; move curr-sorted-cursors further to the last with same index
              (while (and (cdr curr-sorted-cursors) (eq (index-of cursor) (index-of (cadr curr-sorted-cursors))))
                (setq curr-sorted-cursors (cdr curr-sorted-cursors)))

              ;; split sorted-cursors
              (setf new-sorted-cursors (cdr curr-sorted-cursors))
              (setf (cdr curr-sorted-cursors) nil)

              ;; modify curr-size of new sorted-cursors
              (objdolist (new-cursor! new-sorted-cursors)
                (decf new-curr-index size))

              ;; update cursor
              (remove-el cursor sorted-cursors)
              (move-cursor-to-head new-multi-cursor-list! cursor)
              (sorted-push cursor new-sorted-cursors #'cursor!-<=)
              (return)))))

    (objlet* ((cursor! (make-cursor! :curr-index 0 :curr-node new-head)))
      (setf new-iter-cursor cursor!)
      (sorted-push cursor! new-sorted-cursors #'cursor!-<=))

    new-multi-cursor-list!))

(export 'merge-list-to-prev)
(defobjfun merge-list-to-prev (multi-cursor-list!-1 multi-cursor-list!-2)
  ;; size
  (incf size-1 size-2)
  ;; node
  (let ((last-1 (list-node!-prev-node tail-1))
        (next-2 (list-node!-next-node head-2)))
    (setf (list-node!-next-node last-1) next-2)
    (setf (list-node!-prev-node next-2) last-1)
    (setf tail-1 tail-2))
  ;; sorted-cursors
  (remove-el iter-cursor-2 sorted-cursors-2)
  (mapc (objlambda (cursor!)
	  (if (eq curr-index 0)
	      (my-list:move-cursor-to-last multi-cursor-list!-1 cursor!)
	      (incf curr-index size-1)))
	sorted-cursors-2)
  ;; remove iter cursor in sorted-cursors 2
  (nconc sorted-cursors-1 sorted-cursors-2))

(export 'get-data)
(defobjmacro get-data (cursor!)
  `(list-node!-data ,curr-node))

(export 'is-cursor-last)
(declaim (inline is-cursor-last))
(defobjfun is-cursor-last (multi-cursor-list! cursor!)
  (eq size curr-index))

(export 'same-indices?)
(declaim (inline same-indices?))
(defobjfun same-indices? (cursor!-1 cursor!-2)
  (eq curr-index-1 curr-index-2))

(export 'index-of)
(declaim (inline index-of))
(defobjfun index-of (cursor!)
  curr-index)
;; (defobjmacro index-of (cursor!)
;;   `,curr-index)

(export 'is-empty)
(defobjmacro is-empty (multi-cursor-list!)
  `(eq ,size 0))

(export 'get-default-cursor)
(declaim (inline get-default-cursor))
(defobjfun get-default-cursor (multi-cursor-list!)
  iter-cursor)

(export 'push-cursor!)
(declaim (inline push-cursor!))
(defobjfun push-cursor! (l-multi-cursor-list! cursor!)
  (sorted-push cursor! l-sorted-cursors #'cursor!-<=))

(export 'remove-cursor!)
(declaim (inline remove-cursor!))
(defobjfun remove-cursor! (l-multi-cursor-list! cursor!)
  (remove-el cursor! l-sorted-cursors))

(export 'get-size)
(defobjmacro get-size (multi-cursor-list!)
  `,size)

(export 'list-multi-cursor-list!)
(export 'l-multi-cursor-list!)
(export 'cursor-cursor!)
(export 'c-cursor!)
(export 'lc-cursor!)
