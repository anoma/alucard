(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype)
  (:use #:common-lisp)
  (:export :deftype :defcircuit :def))

(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use :common-lisp :trivia)
  (:shadow :=)
  (:export :defpoly :poly))
