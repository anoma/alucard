(uiop:define-package #:alu.prelude
  (:documentation "The Alu User pacakge")
  ;; we shouldn't use CL
  (:shadow #:range #:+ #:* #:= #:exp)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage)
                    (:emit    :alu.spec.emit)
                    (:pipeline :alu.pipeline)
                    (:pass     :alu.pass)
                    (:def      :alu.stepper.define))
  (:mix #:alu #:common-lisp #:uiop)
  (:export #:+ #:* #:= #:exp #:deftype #:defcircuit #:def
           #:deflex #:bool #:entry-point #:coerce
           #:check #:get #:array #:to-array
           #:with-constraint
           #:vampir
           #:vampir-keyword
           #:quit))

;; we should probably move this to the very end of the load order, so
;; we can make it a very nice package for users to use.
(uiop:define-package #:aluser
  (:documentation "The Alu User pacakge")
  (:shadow #:time)
  (:mix #:alu.stepper.define #:alu.prelude #:common-lisp)
  (:local-nicknames (:util     :alu.utils)
                    (:spc      :alu.spec)
                    (:storage  :alu.storage)
                    (:emit     :alu.spec.emit)
                    (:pipeline :alu.pipeline)
                    (:pass     :alu.pass))
  (:reexport #:alu.prelude))
