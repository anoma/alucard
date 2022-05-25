(defpackage #:alu.ir.new-terms
  (:documentation "Provides new constructors and types that can be
used in the various IR's")
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; New Types Defined
   :bind
   :multiple-bind
   :standalone-ret
   ;; New Constructors Defined
   :make-bind
   :make-multi-ret
   :make-standalone-ret
   :make-multiple-bind))

(defpackage #:alu.ir.spec
  (:documentation "Provides various simplified term structures that
has been through linearization")
  (:local-nicknames (:util  :alu.utils)
                    (:spc   :alu.spec)
                    (:terms :alu.ir.new-terms))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; New Term Variants Defined
   :linear-term
   :expanded-term
   :type-aware-term
   :fully-expanded-term
   :binders
   :starting-binders
   ;; New Term Lists Defined
   :type-aware-list
   :constraint-list
   :expanded-list
   :fully-expanded-list))

(defpackage #:alu.ir.primitive-global
  (:documentation "Provides a more low level representation of the
global structure")
  (:local-nicknames (#:spc #:alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   :prim-circuit
   :returns
   :make-prim-circuit))

(uiop:define-package #:alu.ir
  (:documentation "Defines out all the various IR forms found in this
directory, and adds their functionality to give an expanded `alu.spc'
API. This also serves as the packages `spc' contribution.")
  (:use #:common-lisp #:serapeum)
  (:use-reexport
   :alu.spec
   :alu.ir.spec
   :alu.ir.new-terms
   :alu.ir.primitive-global))
