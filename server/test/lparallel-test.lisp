(ql:quickload "lparallel")
(ql:quickload "bt-semaphore")
(ql:quickload "serapeum")

(defpackage :lparallel-user
  (:use :cl :lparallel :lparallel.queue :bt-semaphore))

(in-package :lparallel-user)

;;; initialise the kernel
(defun init ()
  (setf *kernel* (make-kernel (serapeum:count-cpus) :name "channel-queue-kernel")))

;;; shut the kernel down
(defun shutdown ()
  (end-kernel :wait t))

