(in-package :alu.spec.term-op)


(defun add (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :+)
   :arguments arguments))

(defun times (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :*)
   :arguments arguments))

(defun exp (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :exp)
   :arguments arguments))
