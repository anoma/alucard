
;; see https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3
;; for an argument on local-nicknames flag for defpackage, and which
;; compilers support the non standard feature.
;; https://github.com/phoe/trivial-package-local-nicknames
(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype #:range #:def #:+ #:* #:= #:exp)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage))
  (:export :deftype :defcircuit :def))

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
