(in-package :alu.stack)

(defparameter *stack* (ref:ref nil)
  "Global stack that operands will be pushed to")

(defun push (x &optional (stack *stack*))
  (setf (ref:! stack)
        (cons x (ref:! stack))))

(defun get (&optional (stack *stack*))
  (ref:! stack))

(defun new ()
  (ref:ref nil))

(defmacro with-empty-stack (() &rest body)
  `(let ((*stack* (new)))
     ,@body))
