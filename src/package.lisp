(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype #:range #:def)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:fmt     :alu.format)
                    (:storage :alu.storage))
  (:export :deftype :defcircuit :def))

(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use #:common-lisp #:serapeum)
  (:shadow :=)
  (:local-nicknames (:util :alu.utils))
  (:export :defpoly :poly))
