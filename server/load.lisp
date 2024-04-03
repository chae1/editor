(declaim (optimize (speed 0) (space 0) (debug 3)))
(load "~/Desktop/chae1/github/editor/server/utility/my-utils.lisp")
(load "~/Desktop/chae1/github/editor/server/utility/defobj.lisp")
(load "~/Desktop/chae1/github/editor/server/src/multi-cursor-list.lisp")
(load "~/Desktop/chae1/github/editor/server/src/multi-cursor-tree.lisp")
(load "/home/chaewon/Desktop/chae1/github/editor/server/src/new-my-text.lisp")
(load "/home/chaewon/Desktop/chae1/github/editor/server/new-server.lisp")


(with-compilation-unit (:policy '(optimize debug))
  )

(with-compilation-unit (:policy '(optimize speed))
  
  )


;; (load "c:/Users/kcwch/Desktop/chae1/editor/server/data-engine/data-engine.lisp")
;; (load "c:/Users/kcwch/Desktop/chae1/editor/server/text-engine/text-engine.lisp")
;; (load "c:/Users/kcwch/Desktop/chae1/editor/server/server.lisp")

;; (with-compilation-unit (:policy '(optimize speed))
;;   (compile-file "c:/Users/kcwch/Desktop/chae1/editor/server/server.lisp")
;;   )
