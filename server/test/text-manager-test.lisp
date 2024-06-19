(in-package :my-text)

;; 2024.06.01

;; varibles for test
(defparameter *text* (create-text!))
(defparameter *ubuntu-mono* (get-font "UbuntuMono-R"))
(defparameter *user* (make-user! :username "user123" :curr-font *ubuntu-mono*))

(objlet* ((user! *user*))
  (defparameter *a* (create-char! #\a)))
(objlet* ((user! *user*))
  (defparameter *space* (create-char! #\ )))

;; get-glyph-info
(objlet* ((char! *a*))
  glyph)

;; get-char-advance
(objlet* ((char! *a*))
  (get-char-advance))

;; get-space-width
(objlet* ((user! *user*)
	  (char! *space*))
  (get-space-advance))

;; insert-line
(objlet* ((text! *text*))
  (insert-line-in-text (create-line!)))

(objlet* ((text! *text*)
	  (line! (my-tree:get-data text-iter-cursor)))
  (insert-char-in-line *a*))

(objlet* ((user! *user*))
  (setf text (create-text!)))

(objlet* ((user! *user*)
	  (text! text))
  (add-primary-cursor 1 0))



;; load-text
(objlet* ((user! *user*)
	  (text! text))
  (objlet* ((file-path (merge-pathnames "src/multi-cursor-list.lisp" (uiop/os:getcwd))))
    (load-text text! file-path)))

(objlet* ((user! *user*))
  (init-window-variables)
  (init-render-line-height-variables)
  user!)

(objlet* ((user! *user*))
  (get-x-in-vk-coord 10.0))

(objlet* ((user! *user*)
	  (char! *a*))
  (update-render-line-height-variables)
  user!)

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-x-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-y-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-width-in-vk-coord))

(objlet* ((user! *user*)
	  (char! *a*))
  (get-char-height-in-vk-coord))

;; (step (print-text *user*))

(objlet* ((user! *user*))
  (get-render-msg))
