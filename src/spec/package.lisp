(defpackage #:alu.spec
  (:documentation "the type specification and layout of the alu
package and alu terms")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils))
  (:export

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Generic Data Manipulation
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; Mixins
   :direct-slots-mixin
   :protect-slots-mixin

   ;; Operations on Mixins
   :protected
   :protect-slots
   :direct-slots
   :direct-slot-names
   :direct-slot-keywords
   :direct-slot-values
   :update-from-alist

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; found in term
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; New Top Level Term Variants Defined
   :expression
   :term
   :term-no-binding
   :base
   :type-manipulation
   :term-type-manipulation
   :record-forms
   :array-forms
   :term-normal-form

   ;; Term ADT Constructors Defined
   :application     :func   :arguments
   :primitive       :name
   :record          :name   :contents
   :record-lookup   :record :field
   :let-node        :var    :value
   :reference       :name
   :bind-constraint :var    :value
   :type-coerce     :value :typ
   :type-check      :value :typ
   :from-data       :contents
   :array-allocate  :size  :typ
   :array-lookup    :arr   :pos
   :array-set       :arr   :pos :value

   ;; Term Applications Defined
   :make-application
   :make-record :lookup-record :make-record-lookup
   :make-type-check :make-type-coerce
   :make-from-data :make-array-allocate :make-array-lookup :make-array-set
   :make-let :make-reference :make-bind-constraint

   ;; Functions
   :record->alist

   ;; Misc pattern matching functions
   :number

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; found in type
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; New Top Level Term Variants Defined
   :type-reference
   :type-reference-full

   ;; New Types Defined Type-Storage
   :reference-type :name

   ;; Functions
   :to-type-reference-format

   ;; New Constructors Defined
   :make-type-reference   :make-primitive
   :make-type-declaration :make-record-declaration

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; found in global
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; New Top Level Term Variants Defined
   :function-type
   :type-storage

   ;; New Types Defined Function-type
   :circuit :name :arguments :expanded-arguments :return-type :body :exec

   :privacy
   :constraint :name :typ

   ;; New Types Defined Type-Storage
   :type-declaration :name :generics :options :decl

   :type-format
   :record-decl :contents :order
   :sum-decl

   ;; Functions
   :record-declaration->alist

   ;; New Constructors Defined
   :make-circuit
   :make-constraint))

(defpackage #:alu.storage
  (:documentation "Serves as the long term storage of any Alucard Circuit")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:format :alu.spec))
  (:export
   :*types*
   :*functions*
   :add-function    :add-type
   :lookup-function :lookup-type
   :swap-tables     :restore-tables
   :currently-swapped?
   ;; Entry point operations
   :get-entry-point
   :set-entry-point))

(defpackage #:alu.spec.emit
  (:documentation "Emits to the real body of a circuit declaration. By this, we mean
that we modify the current body in scope with the given instruction.")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:spc     :alu.spec)
                    (:storage :alu.storage))
  (:export :with-circuit-body :instruction))
