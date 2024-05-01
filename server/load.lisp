(require 'uiop)

(declaim (optimize (speed 0) (space 0) (debug 3)))
(let ((cwd (uiop/os:getcwd)))
  (load (merge-pathnames "utility/my-utils.lisp" cwd))
  (load (merge-pathnames "utility/defobj.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-list.lisp" cwd))
  (load (merge-pathnames "src/multi-cursor-tree.lisp" cwd))
  (load (merge-pathnames "src/text-manager.lisp" cwd))
  (load (merge-pathnames "server.lisp" cwd)))

(with-compilation-unit (:policy '(optimize debug))
  )

(with-compilation-unit (:policy '(optimize speed)) 
  )
