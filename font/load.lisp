(declaim (optimize (speed 0) (space 0) (debug 3)))

(require 'uiop)
(let ((cwd (uiop/os:getcwd)))
  (load (merge-pathnames "../utils/my-utils.lisp" cwd))
  (load (merge-pathnames "../utils/defobj.lisp" cwd))
  (load (merge-pathnames "generate-font.lisp" cwd))
  )
