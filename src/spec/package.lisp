(defpackage #:alu.utils
  (:documentation "provides the Alucard VAMP-IR Utils")
  (:shadow #:deftype #:serapeum)
  (:use #:common-lisp)
  (:export :symbol-to-keyword :hash-compare
           :sycamore-plist-symbol-map :sycamore-symbol-map-plist))

(defpackage #:alu.spec
  (:documentation "the type specification and layout of the alu
package and alu terms")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils))
  (:export
   ;; found in term
   :term
   :expression
   :application   :func   :arguments
   :record        :name   :contents
   :record-lookup :record :field
   :let-node      :var    :term :body
   :reference     :name
   :make-application :make-record :lookup-record
   :make-record-lookup :make-let :make-reference
   ;; found in type
   :type-reference
   :refernce-type :name

   :type-storage
   :primitive :name
   :type-declaration :name :generics :options :decl

   :type-format
   :record-decl :contents
   :sum-decl
   :to-type-reference-format
   :make-type-reference :make-primitive :make-type-declaration
   :make-record-declaration
   ;; found in global
   :function-type
   :privacy
   :circuit    :name :arguments :return-type :body
   :constraint :name :typ
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
   :lookup-function :lookup-type))
