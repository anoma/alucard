(defpackage #:alu.stack
  (:documentation "provides a custom stack mechanism and a way to
define and promote functions to automatically push and pull from the
stack when possible. Further, operations operate on a global dynamic
variable and offer rebinding and passing in capabilities.")
  (:local-nicknames (:ref :alu.reference))
  (:shadow :push :pop :get :cdr :cons)
  (:use #:common-lisp #:serapeum)
  (:export
   ;; Mutable interface
   :push :pop :get :new
   ;; Functional Interface
   :cons :cdr
   :emptyp
   :cdr-current-section
   ;; functional getters
   :stack
   :current-section
   :name
   ;; Helper Macros
   :with-empty-stack
   :with-section
   :*stack*))
