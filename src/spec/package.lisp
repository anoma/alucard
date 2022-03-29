(defpackage #:alu.utils
  (:documentation "provides the Alucard VAMP-IR Utils")
  (:shadow #:deftype #:serapeum)
  (:use #:common-lisp)
  (:export
   :symbol-to-keyword
   :hash-compare
   :sycamore-plist-symbol-map
   :sycamore-symbol-map-plist
   :copy-instance))

(defpackage #:alu.spec
  (:documentation "the type specification and layout of the alu
package and alu terms")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils))
  (:export

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; found in term
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; New Top Level Term Variants Defined
   :expression
   :term
   :term-no-binding
   :term-normal-form

   ;; Term ADT Constructors Defined
   :application   :func   :arguments
   :primitive     :name
   :record        :name   :contents
   :record-lookup :record :field
   :let-node      :var :value :body
   :reference     :name

   ;; Term Applications Defined
   :make-application :make-record :lookup-record
   :make-record-lookup :make-let :make-reference

   ;; Misc pattern matching functions
   :number

   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;;; found in type
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; New Top Level Term Variants Defined
   :type-reference

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
   :circuit :name :arguments :expanded-arguments :return-type :body

   :privacy
   :constraint :name :typ

   ;; New Types Defined Type-Storage
   :type-declaration :name :generics :options :decl

   :type-format
   :record-decl :contents
   :sum-decl

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
   :currently-swapped?))
