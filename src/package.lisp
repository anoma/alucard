(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype)
  (:use #:common-lisp)
  (:local-nicknames (:util    :alu.utils)
                    (:fmt     :alu.format)
                    (:storage :alu.storage))
  (:export :deftype :defcircuit :def))

(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use :common-lisp :trivia)
  (:shadow :=)
  (:local-nicknames (:util :alu.utils))
  (:export :defpoly :poly))
