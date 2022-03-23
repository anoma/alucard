(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype #:range #:def #:+ #:* #:= #:exp)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage))
  (:export :deftype :defcircuit :def))

(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use #:common-lisp #:serapeum)
  (:shadow :=)
  (:local-nicknames (:util :alu.utils))
  (:export :defpoly :poly))

(uiop:define-package #:alu.prelude
  (:documentation "The Alu User pacakge")
  (:mix #:alu #:common-lisp)
  (:reexport
   #:common-lisp
   #:alu)
  (:export #:+ #:* #:= #:range))

(uiop:define-package #:aluser
  (:documentation "The Alu User pacakge")
  (:mix #:alu.prelude #:common-lisp)
  (:reexport
   #:alu.prelude
   #:common-lisp))
