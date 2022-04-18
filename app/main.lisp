(in-package :alu)


;; We can generate out our image with


;; (asdf:make :alu)

;; however if we want a compressed


(defun main ()
  (format t "args: ~A" (uiop:command-line-arguments))
  (setf *print-pretty* t)
  (in-package :aluser)
  #+ccl
  (ccl:toplevel-loop)
  #+sbcl
  (sb-impl::toplevel-init)
  #+ecl
  (si:top-level t))

;; these don't seem to do much sadly

;; (uiop/image:register-image-restore-hook (lambda () (in-package :aluser)) nil)

;; (uiop/image:register-image-restore-hook
;;  (lambda ()
;;    (setf *package* (find-package :alu))
;;    (setf *print-pretty* t))
;;  nil)

;; (uiop/image:register-image-dump-hook
;;  (lambda ()
;;    (setf *package* (find-package :alu))
;;    (setf *print-pretty* t))
;;  nil)



;; If you want compression on your asdf
(defun save-alu-and-die (&rest args)
  args
  (in-package :aluser)
  #+ccl
  (ccl:save-application "image" :prepend-kernel t
                                :toplevel-function #'main)
  #+sbcl
  (sb-ext:save-lisp-and-die #p"image"
                            :toplevel #'main
                            :executable t
                            :COMPRESSION 9))
