(in-package #:alu.stepper.define)

(defmacro defun (name lambda-list &rest body &environment env)
  (destructuring-bind (decs body) (step:split-declaration body)
    `(cl:defun ,name ,lambda-list
       ,@decs
       (stack:with-section ,name
         ,@(step:body body env)))))
