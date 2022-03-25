;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Extending The Specification of Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.linear-term
  (:documentation "Provides a simplified term structures that has been through
linearization, use alu.pass.linear-spec for the full specification")
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; New Term Variants Defined
   :linear-term
   :expanded-term
   ;; New Term Lists Defined
   :constraint-list
   :expanded-list
   ;; New Types Defined
   :bind
   :multiple-bind
   ;; New Constructors Defined
   :make-bind
   :make-multiple-bind))

(uiop:define-package #:alu.pass.linear-spec
  (:documentation "Defines out the specification of the linear term,
reusing values from alu.spec and alu.pass.linear-term")
  (:use #:common-lisp #:serapeum)
  (:use-reexport :alu.spec :alu.pass.linear-term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Expanding Away And relocating Record Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.expanded
  (:documentation "Provides argument expansion functionality.
Including circuit declaration expansion, and function type
expansion.")
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.linear-spec)
                    (:storage :alu.storage)
                    (:closure :alu.closure))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; Type API
   :argument
   :expand :original :expanded
   :make-expanded
   ;; Core API
   :full-arguments-from-storage))

(defpackage #:alu.pass.relocation
  (:documentation "Provides mapping and functionality required to
safely relocate record instances and generate out code which lacks records")
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.linear-spec)
                    (:storage :alu.storage)
                    (:closure :alu.closure))
  (:use #:common-lisp #:serapeum)
  (:export))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.anf
  (:documentation "Provides an ANF pass for the alucard term")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec))
  (:export :normalize-expression))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.linear-spec)
                    (:anf     :alu.pass.anf)
                    (:expand  :alu.pass.expanded)
                    (:storage :alu.storage)
                    (:closure :alu.closure))
  (:export))


