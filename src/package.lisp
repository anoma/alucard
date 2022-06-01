
;; see https://gist.github.com/phoe/2b63f33a2a4727a437403eceb7a6b4a3
;; for an argument on local-nicknames flag for defpackage, and which
;; compilers support the non standard feature.
;; https://github.com/phoe/trivial-package-local-nicknames
(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype #:def #:coerce #:get #:array)
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage)
                    (:emit    :alu.spec.emit))
  (:export #:deftype #:defcircuit #:def #:defprimitive #:defprimitive-type
           #:entry-point #:coerce #:check #:get #:array #:to-array))

(uiop:define-package #:alu.prelude
  (:documentation "The Alu User pacakge")
  ;; we shouldn't use CL
  (:shadow #:range #:+ #:* #:= #:exp)
  (:mix #:alu #:common-lisp)
  (:export #:+ #:* #:= #:exp #:deftype #:defcircuit #:def
           #:deflex #:bool #:entry-point #:coerce
           #:check #:get #:array #:to-array))

(uiop:define-package #:aluser
  (:documentation "The Alu User pacakge")
  (:shadow #:time)
  (:mix #:alu.prelude #:common-lisp)
  (:reexport #:alu.prelude))
