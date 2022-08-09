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
    (("swank" #\s)
     :type boolean :optional t :documentation "Launches a swank server for text editor integration")
    (("sly" #\y)
     :type boolean :optional t :documentation "Launches a sly server for emacs integration")
    (("port" #\p)
     :type integer :optional t :documentation "The port for the swank/sly server. Defaults to 4005 ")
    ;; (("check" #\c)
    ;;  :type string  :optional t :documentation "a --check or -c flag that takes a string")
    ;; (("warn" "warning" #\w)
    ;;  :type boolean :optional t :documentation "multiple spellings possible")
    ;; (("version" #\V)
    ;;  :type boolean :optional t :documentation "--version or -V, you get the idea")
    ))

(defun main ()
  (setf uiop:*command-line-arguments* (uiop:command-line-arguments))
  (command-line-arguments:handle-command-line
   +command-line-spec+
   #'argument-handlers
   :name "alucard"))

(defun argument-handlers (&key help output input sly swank port)
  (verbose:restart-global-controller)
  (flet ((startup-function ()
           (let ((port (or port 4005)))
             (when swank
               (swank:create-server :port port :dont-close t))
             (when sly
               (slynk:create-server :port port :dont-close t)))
           (when input
             (load input))))
    (cond (help
           (command-line-arguments:show-option-help +command-line-spec+ :sort-names t))
          ((and output input)
           (prepend-env)
           (load input)
           (alu.pipeline:dump-entry-point-to-file output))
          (output
           (format t "Need an input file in order to generate an output file~%"))
          (t
           (start-repl #'startup-function)))))

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
(defun save-alu-and-die ()
  #+ccl
  (ccl:save-application "image" :prepend-kernel t
                                :toplevel-function #'main)
  #+sbcl
  (sb-ext:save-lisp-and-die #p"./build/alu.image"
                            :toplevel #'main
                            :executable t
                            :COMPRESSION 1))
