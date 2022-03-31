
;; Setup this package to extract戶川純
(defpackage #:alu.vampir.spec
  (:documentation "The Vampir model specification")
  (:use #:common-lisp)
  (:shadow :=)
  (:local-nicknames (#:util #:alu.utils)
                    (#:ser #:serapeum))
  (:export
   ;; New Top Level Term Variants Defined
   :statement
   :constraint
   :expression
   :normal-form
   :primitive

   ;; New Term Lists Defined
   :normal-form-list
   :constraint-list
   ;; Term ADT Constructors Defined
   :alias       :name  :inputs   :outputs :body
   :pub         :wires
   :infix       :op    :lhs      :rhs
   :application :func  :arguments
   :bind        :names :value
   :equality    :lhs   :rhs
   :wire        :var
   :constant    :const

   ;; Constructors
   :make-alias :make-pub :make-infix :make-application
   :make-bind  :make-equality :make-wire :make-constant))

(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use #:common-lisp #:serapeum)
  (:shadow :=)
  (:local-nicknames (#:util #:alu.utils)
                    (#:spc #:alu.vampir.spec))
  (:export :extract))

