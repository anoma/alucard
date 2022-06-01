
(defpackage #:alu.typechecker.types
  (:documentation "Holds the various types needed for the type
checker")
  (:local-nicknames (#:ir         #:alu.ir)
                    (#:closure    #:alu.closure)
                    (#:dependency #:alu.closure.dependency))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; Context data type
   :typing-context :holes :hole-info :dependency :typing-closure

   ;; Known Type Information
   :type-info :type-info-size :type-info-type :type-info-p :make-type-info

   ;; Function return types
   :typing-result

   ;; Known Hole Information
   :hole-information :hole-information-unrefined :hole-information-term
   :make-hole-information :hole-information-p

   :hole
   :hole-conditions
   :same-as :same-as-value :make-same-as :same-as-p
   :depends-on :depends-on-value :make-depends-on :depends-on-p

   ;; Querying information
   :current-information
   :lookup-type

   ;; Primitive information
   :known-primitve-types
   :known-primitve-functions

   ;; Operations on Datatypes
   :add-hole-formula))

(defpackage #:alu.typechecker.size
  (:documentation "Calculates the size of various types found in the Alucard
language. The name typically refers to the value being calculated.")
  (:local-nicknames (#:ir      #:alu.ir)
                    (#:storage #:alu.storage)
                    (#:types   #:alu.typechecker.types))
  (:shadow :declaration)
  (:use #:common-lisp #:serapeum)
  (:export
   :reference
   :storage
   :declaration
   :declaration-contents
   :primitive))

;; This package is split-up between typecheck and unifier
(defpackage #:alu.typechecker.check
  (:local-nicknames (#:ir         #:alu.ir)
                    (#:type-op    #:alu.spec.type-op)
                    (#:closure    #:alu.closure)
                    (#:storage    #:alu.storage)
                    (#:dependency #:alu.closure.dependency)
                    (#:util       #:alu.utils)
                    (#:size       #:alu.typechecker.size))
  (:use #:common-lisp #:serapeum #:alu.typechecker.types)
  (:export
   :check
   :annotate-circuit
   :annotate-term
   :annotate-list
   :make-starting-hole
   :type-equality))

(defpackage #:alu.typechecker.intro
  (:documentation "Gives an API for introducing new variables to the compiler")
  (:local-nicknames (#:ir         #:alu.ir)
                    (#:storage    #:alu.storage)
                    (#:closure    #:alu.closure)
                    (#:dependency #:alu.closure.dependency)
                    (#:util       #:alu.utils)
                    (#:size       #:alu.typechecker.size)
                    (#:check      #:alu.typechecker.check))
  (:use #:common-lisp #:serapeum #:alu.typechecker.types)
  (:export
   :intro
   :with-intro))

(uiop:define-package #:alu.typechecker
  (:use #:common-lisp
        #:serapeum
        #:alu.typechecker.types
        #:alu.typechecker.check
        #:alu.typechecker.intro)
  (:reexport #:alu.typechecker.intro)
  ;; We don't re-export #:alu.typechecker.check as it has extra
  ;; exports we don't want.
  (:export
   :check
   :annotate-circuit
   :annotate-term
   :annotate-list
   :type-equality)
  ;; Operations from types that are good to alias
  (:export
   ;; Context data type
   :typing-context :holes :hole-info :dependency :typing-closure
   ;; Known Type Information
   :type-info :type-info-size :type-info-type :type-info-p :make-type-info))
