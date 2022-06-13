(in-package :alu.spec.emit)

(defparameter *circuit-body* (list nil))

(defmacro with-circuit-body (init-body &body body)
  "Creates a fresh circuit body"
  `(progn (let ((*circuit-body* ,init-body))
            ,@body)
          (setf ,init-body (remove-if #'null ,init-body))
          ,init-body))

(defun instruction (term)
  "Adds the given term to the circuit body. This adds the value to the
last slot of the given body"
  (setf (cdr *circuit-body*) (list term)
        *circuit-body*       (cdr *circuit-body*)))
