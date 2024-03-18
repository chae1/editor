(defpackage my-text
  (:nicknames :my-text)
  (:use :common-lisp :my-utils :defobj))

(in-package :my-text)

"
text : list
  line-manager ^user0
    line : list
      c1
      c2 ^user0
      ..
  line-manager
    line : list
      c1
      c2
      ..

chaewon -> ((tc,            (tc,         ..
              (lc lc lc ..))  (lc lc ..))  ..)
"

;; line-manager!

(defobj line-manager!
  (line nil :type my-list:multi-cursor-list!)
  (line-iter-cursor nil :type my-list:cursor!)
  (word-start-cursor nil :type my-list:cursor!)
  (word-end-cursor nil :type my-list:cursor!))

(defmethod print-object ((lm line-manager!) out)
  (print-unreadable-object (lm out :type t)
    (format out "~a" (line-manager!-line lm))))

(defmacro create-line-manager! ()
  `(let ((line (my-list:create-multi-cursor-list!)))
     (make-line-manager! :line line
                         :line-iter-cursor (my-list:get-default-cursor line)
                         :word-start-cursor (my-list:create-cursor! line 0)
                         :word-end-cursor (my-list:create-cursor! line 0))))

(defobj text-manager!
  (file-path nil)
  (text nil :type my-tree:multi-cursor-tree!)
  (text-iter-cursor nil :type my-tree:cursor!)
  (render-start-line-index 1 :type integer)
  (render-start-line-subindex 1 :type integer)
  (window-width 20 :type integer)
  (window-height 10 :type integer)
  (cursors-manager-table (make-hash-table :test 'equal) :type hash-table))

(export 'create-text-manager!)
(defun create-text-manager! ()
  (objlet* ((text (my-tree:create-multi-cursor-tree!)))
    (make-text-manager! :text text
			:text-iter-cursor (my-tree:get-default-cursor text))))

(defmethod print-object ((text text-manager!) out)
  (print-unreadable-object (text out :type t)
    (format out "~a" (text-manager!-file-path text))))

;; text functions

(defobjfun init-text (text-manager! path)
  (objlet* ((line-manager! (create-line-manager!)))
    (my-tree:insert-data-after-cursor text text-iter-cursor line-manager!)))

(defobjfun load-text (text-manager! path)
  (with-open-file (fin path :if-does-not-exist nil)
    (if fin
        (progn
          (setf file-path path)
          (objlet* ((line-manager! (create-line-manager!)))
            (my-tree:insert-data-after-cursor text text-iter-cursor line-manager!)
            (do ((char (read-char fin nil) (read-char fin nil)))
                ((eq char nil))
              (cond ((eq char #\Newline) (progn
                                          (setq line-manager! (create-line-manager!))
                                          (my-tree:insert-data-after-cursor text text-iter-cursor line-manager!)))
                    (t (my-list:insert-data-after-cursor line line-iter-cursor char))))))
        (format t "file not exists.~%"))))

;; macros for print-text
;; following macros change height-vacancy, width-vacancy, word-width, and line-manager!
(objlet* ((text-manager!)
          (line-manager!))
  (defmacro print-char (char)
    `(progn
       (format t "~a" ,char)
       (decf width-vacancy)))

  (defmacro print-new-line ()
    `(progn
       (format t "~a" #\Newline)
       (setq width-vacancy window-width)
       (decf height-vacancy)))

  (defmacro print-curr-word-in-line ()
    `(progn
       (do nil
           ((my-list:same-indices? word-start-cursor word-end-cursor))
         (my-list:move-cursor-to-next line word-start-cursor)
         (decf word-width)
         (let ((c (my-list:get-data word-start-cursor)))
           (print-char c)))))

  (defmacro print-curr-word ()
    `(cond ((>= width-vacancy word-width)
            (print-curr-word-in-line))

           ((< width-vacancy word-width)
            (cond ((>= window-width word-width)
                   (cond ((> height-vacancy 0)
                          (print-new-line)
                          (print-curr-word-in-line))

                         ((<= height-vacancy 0)
                          (return))))

                  ((< window-width word-width)
                   (do nil
                       ((or (my-list:same-indices? word-start-cursor word-end-cursor)
                            (<= height-vacancy 0)))
                     (if (<= width-vacancy 0)
                         (print-new-line))
                     (my-list:move-cursor-to-next line word-start-cursor)
                     (decf word-width)
                     (let ((c (my-list:get-data word-start-cursor)))
                       (print-char c)))))))))

(export 'print-text)
"print-text : from width, height, line-cursor, and line-num, calculate and print text for rendering
   *print to the socket later"
(defobjfun print-text (text-manager!)
  (my-tree:move-cursor-to-index text text-iter-cursor (1- render-start-line-index))
  (objdo* ((line-manager!)
           (height-vacancy window-height)
           (width-vacancy window-width))
      ((or (<= height-vacancy 0) (my-tree:is-cursor-last text text-iter-cursor)))
    ;; skip head
    (my-tree:move-cursor-to-next text text-iter-cursor)
    (setq line-manager! (my-tree:get-data text-iter-cursor))

    ;; print current line
    (when (not (my-list:is-empty line))
      (my-list:move-cursor-to-head line word-start-cursor)
      (my-list:move-cursor-to-head line word-end-cursor)

      (do ((word-width 0))
          ((my-list:is-cursor-last line word-end-cursor))
        (my-list:move-cursor-to-next line word-end-cursor)
        (incf word-width)

        (let ((char (my-list:get-data word-end-cursor)))
          (cond ((eq char #\ )
                 (my-list:move-cursor-to-prev line word-end-cursor)
                 (decf word-width)
                 (print-curr-word)
                 (print-char #\ )
                 ;; skip #\
                 (my-list:move-cursor-to-next line word-start-cursor)
                 (my-list:move-cursor-to-next line word-end-cursor))

                ((eq char #\Newline)
                 ;; move before #\Newline
                 (my-list:move-cursor-to-prev line word-end-cursor)
                 (decf word-width)
                 (print-curr-word)
                 (print-char #\Newline)
                 ;; skip #\Newline
                 (my-list:move-cursor-to-next line word-start-cursor)
                 (my-list:move-cursor-to-next line word-end-cursor))

                ((my-list:is-cursor-last line word-end-cursor)
                 (print-curr-word))))))

    (print-new-line)))

(export 'cursor!)
(defobj cursor!
  (text-cursor nil :type (or null my-tree:cursor!))
  (line-cursor nil :type (or null my-list:cursor!))
  (updown-line-index 0 :type integer))

(defmethod print-object ((c cursor!) out)
  (print-unreadable-object (c out :type t)
    (format out "text-index : ~a, line-index : ~a"
            (my-tree:index-of (cursor!-text-cursor c))
            (my-list:index-of (cursor!-line-cursor c)))))

(defobj cursors-manager!
  (primary-cursor nil :type (or null cursor!))
  (cursors nil :type (or null cons)))

(defobjfun print-cursors-manager! (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (when cursors-manager!
      (objlet* ((cursor! primary-cursor))
        (format t "~% primary-cursor at (~a, ~a)" (my-tree:index-of text-cursor) (my-list:index-of line-cursor)))
      (format t ", cursors at")
      (objdolist (cursor! cursors)
        (format t " (~a, ~a)" (my-tree:index-of text-cursor) (my-list:index-of line-cursor)))
      (format t "~%"))))

(defobjfun push-primary-cursor (text-manager! username text-index line-index)
  (let ((text-size (my-tree:get-size text)))
    (if (or (> text-index text-size) (<= text-index 0))
        (error "my-text:push-cursor! text-index out of bound.~%")
        (progn
          (my-tree:move-cursor-to-index text text-iter-cursor text-index)
          (objlet* ((line-manager! (my-tree:get-data text-iter-cursor))
                    (line-size (my-list:get-size line)))
            (if (or (> line-index line-size) (< line-index 0))
                (error "my-text:push-cursor! line-index out of bound.~%")
                (objlet* ((cursor! (make-cursor!)))
                  (setf text-cursor (my-tree:create-cursor! text text-index))
                  (setf line-cursor (my-list:create-cursor! line line-index))

                  (my-tree:push-cursor! text text-cursor)
                  (my-list:push-cursor! line line-cursor)

                  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
                    (push cursor! cursors)
                    (setf primary-cursor cursor!)))))))))

(export 'init-cursors-manager!)
(defobjfun init-cursors-manager! (text-manager! username)
  (setf (gethash username cursors-manager-table) (make-cursors-manager!))
  (push-primary-cursor text-manager! username 1 0))

(defobjfun reset-cursors-manager! (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (objdolist (cursor! cursors)
      (if (not-eq cursor! primary-cursor)
          (objlet* ((line-manager! (my-tree:get-data text-cursor)))
            (my-tree:remove-cursor! text text-cursor)
            (my-list:remove-cursor! line line-cursor))))
    (setf cursors nil)
    (push primary-cursor cursors)
    cursors-manager!))

(export 'remove-cursors-manager!)
(defobjfun remove-cursors-manager! (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (objdolist (cursor! cursors)
      (objlet* ((line-manager! (my-tree:get-data text-cursor)))
        (my-tree:remove-cursor! text text-cursor)
        (my-list:remove-cursor! line line-cursor))))
  (remhash username cursors-manager-table))

(defobjfun copy-and-paste-primary-cursor (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table))
            (cursor! primary-cursor))
    (let ((text-index (my-tree:index-of text-cursor))
          (line-index (my-list:index-of line-cursor)))
      (push-primary-cursor text-manager! username text-index line-index))))

(defobjfun get-primary-cursor (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    primary-cursor))

(defobjfun move-cursor-right (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (if (my-list:is-cursor-last line line-cursor)
        (if (my-tree:is-cursor-last text text-cursor)
            nil
            (progn
              (my-tree:move-cursor-to-next text text-cursor)
              (setq line-manager! (my-tree:get-data text-cursor))
              (my-list:move-cursor-to-head line line-cursor)))
        (progn
          (my-list:move-cursor-to-next line line-cursor)))
    (setf updown-line-index (my-list:index-of line-cursor)))
  cursor!)

(defobjfun move-cursor-left (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (if (= (my-list:index-of line-cursor) 0)
        (if (= (my-tree:index-of text-cursor) 1)
            nil
            (progn
              (my-tree:move-cursor-to-prev text text-cursor)
              (setq line-manager! (my-tree:get-data text-cursor))
              (my-list:move-cursor-to-last line line-cursor)))
        (progn
          (my-list:move-cursor-to-prev line line-cursor)))
    (setf updown-line-index (my-list:index-of line-cursor)))
  cursor!)

(defobjfun move-cursor-up (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (if (= (my-tree:index-of text-cursor) 1)
        nil
        (progn
          (my-tree:move-cursor-to-prev text text-cursor)
          (setq line-manager! (my-tree:get-data text-cursor))
          (my-list:move-cursor-to-head line line-cursor)
          (my-list:move-cursor-to-index line line-cursor (clamp updown-line-index 0 (my-list:get-size line))))))
  cursor!)

(defobjfun move-cursor-down (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (if (my-tree:is-cursor-last text text-cursor)
        nil
        (progn
          (my-tree:move-cursor-to-next text text-cursor)
          (setq line-manager! (my-tree:get-data text-cursor))
          (my-list:move-cursor-to-head line line-cursor)
          (my-list:move-cursor-to-index line line-cursor (clamp updown-line-index 0 (my-list:get-size line))))))
  cursor!)

(defobjfun insert-char-after-cursor (text-manager! cursor! char)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (my-list:insert-data-after-cursor line line-cursor char)))

(defobjfun insert-new-line-after-cursor (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor))
            (line-to-insert (my-list:split-list-after-cursor line line-cursor))
            (new-line-manager! (make-line-manager! :line line-to-insert
                                                   :line-iter-cursor (my-list:get-default-cursor line-to-insert)
                                                   :word-start-cursor (my-list:create-cursor! line-to-insert 0)
                                                   :word-end-cursor (my-list:create-cursor! line-to-insert 0))))
    (my-tree:insert-data-after-cursor text text-cursor new-line-manager!)))

(defobjfun delete-char-before-cursor (text-manager! cursor!)
  (objlet* ((line-manager! (my-tree:get-data text-cursor)))
    (my-list:delete-data-before-cursor line line-cursor)))

(defobjfun delete-new-line-before-cursor (text-manager! cursor!)
  (if (>= (my-tree:index-of text-cursor) 2)
      (objlet* ((curr-line-manager! (my-tree:get-data text-cursor)))
        (my-tree:delete-data-before-cursor text text-cursor)
        (objlet* ((prev-line-manager! (my-tree:get-data text-cursor)))
          (my-list:merge-list-to-prev prev-line curr-line)))))

(defobjfun insert-char (text-manager! username char)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (objdolist (cursor! cursors)
      (insert-char-after-cursor text-manager! cursor! char))))

(defobjfun insert-new-line (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (objdolist (cursor! cursors)
      (insert-new-line-after-cursor text-manager! cursor!))))

(defobjfun backspace (text-manager! username)
  (objlet* ((cursors-manager! (gethash username cursors-manager-table)))
    (objdolist (cursor! cursors)
      (if (eq (my-list:index-of line-cursor!) 0)
	  (delete-new-line-before-cursor text-manager! cursor!)
	  (delete-char-before-cursor text-manager! cursor!)))))
