(require 'uiop)

(declaim (optimize (speed 0) (space 0) (debug 3)))

(let ((cwd (uiop/os:getcwd)))
  (load (merge-pathnames "../utils/my-utils.lisp" cwd))
  (load (merge-pathnames "../utils/defobj.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-list.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-tree.lisp" cwd))
  (load (merge-pathnames "src/text-manager.lisp" cwd))
  ;; (load (merge-pathnames "src/server.lisp" cwd))
  )

(let ((cwd (uiop/os:getcwd)))
  (load (merge-pathnames "src/text-manager.lisp" cwd))
  )

(with-compilation-unit (:policy '(optimize debug))
  )

(with-compilation-unit (:policy '(optimize speed)) 
  )
