(in-package #:alu.stepper.define)

(defmacro defun (name lambda-list &rest body &environment env)
  `(cl:defun ,name ,lambda-list ,@(step:body body env)))
