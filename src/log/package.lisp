(defpackage #:alu.log
  (:documentation "Provides reporting functionality to the language")
  (:local-nicknames (:spc  :alu.spec))
  (:shadow :error)
  (:use #:common-lisp #:serapeum)
  (:export
   :report
   :mode
   :*mode*
   :error
   :data))
