(require 'uiop)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(let ((cwd (uiop/os:getcwd)))
  (load (merge-pathnames "../utils/my-utils.lisp" cwd))
  (load (merge-pathnames "../utils/defobj.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-list.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-tree.lisp" cwd))
  (load (merge-pathnames "src/text-manager.lisp" cwd))
  (load (merge-pathnames "src/server.lisp" cwd)))

;; (with-compilation-unit (:policy '(optimize debug))
;;   )

;; (with-compilation-unit (:policy '(optimize speed)) 
;;   )

(defpackage export
  (:use :common-lisp))

(in-package :export)

(ql:quickload "cl-ppcre")

(export 'main)
;; main.lisp
(defun main ()
  "Main function to process command-line arguments."
  (let* ((argv sb-ext:*posix-argv*)
         (executable-name (first argv))
         (args (rest argv))
	 (ip-port (car args)))
    (format t "receive ~a as argument~%" ip-port)

    (let ((split-res (cl-ppcre:split ":" ip-port)))
      (let ((ip (coerce (mapcar #'parse-integer
				(cl-ppcre:split "\\." (car split-res)))
			'vector))
	    (port (parse-integer (cadr split-res))))

	(print ip)
	(print port)

	(text-server:run-server :ip ip :port port)))))

(cond
  ((string-equal (software-type) "Win32")
   (sb-ext:save-lisp-and-die "server.exe" :executable t :toplevel #'export::main))
  ((string-equal (software-type) "Linux")
   (sb-ext:save-lisp-and-die "server" :executable t :toplevel #'export::main)))
