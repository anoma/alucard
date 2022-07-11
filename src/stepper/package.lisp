(defpackage #:alu.stepper
  (:documentation "Provides a code walker for CL that can instrument
  Stack trace information.")
  (:shadow #:step #:single)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (#:stack #:alu.stack))
  (:export #:single #:body #:mode #:*mode*))

(defpackage #:alu.stepper.define
  (:documentation "Provides custom definers that shadow CL base definers")
  (:shadow :defun)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (#:step #:alu.stepper))
  (:export #:defun))
