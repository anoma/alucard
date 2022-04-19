(in-package :alu)


;; We can generate out our image with


;; (asdf:make :alu)

;; however if we want a compressed

(defparameter +command-line-spec+
  '((("input" #\i)
     :type string :optional t :documentation "Input alucard file location")
    (("output" #\o)
     :type string :optional t :documentation "Sets compiler to compile mode and output vampir file location")
    (("help" #\h #\?)
     :type boolean :optional t :documentation "The current help message")
    ;; (("check" #\c)
    ;;  :type string  :optional t :documentation "a --check or -c flag that takes a string")
    ;; (("warn" "warning" #\w)
    ;;  :type boolean :optional t :documentation "multiple spellings possible")
    ;; (("version" #\V)
    ;;  :type boolean :optional t :documentation "--version or -V, you get the idea")
    ))

(defun main ()
  (command-line-arguments:handle-command-line
   +command-line-spec+
   #'argument-handlers
   :name "alucard"))

(defun argument-handlers (&key help output input)
  (cond (help
         (command-line-arguments:show-option-help +command-line-spec+ :sort-names t))
        ((and output input)
         (prepend-env)
         (load input)
         (alu.pipeline:dump-entry-point-to-file output))
        (output
         (format t "Need an input file in order to generate an output file~%"))
        (input
         (start-repl (lambda () (load input))))
        (t
         (start-repl))))

(defun prepend-env ()
  (setf *print-pretty* t)
  (in-package :aluser))

(defun start-repl (&optional (func (lambda () 1)))
  (prepend-env)
  (funcall func)
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
