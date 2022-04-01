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
   :fully-expanded-term
   ;; New Term Lists Defined
   :constraint-list
   :expanded-list
   :fully-expanded-list
   ;; New Types Defined
   :bind
   :multiple-bind
   :multi-ret
   :ret
   ;; New Constructors Defined
   :make-bind
   :make-multi-ret
   :make-ret
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
                    (:storage :alu.storage))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; Type API
   :argument
   :argument-list
   :expand :original :expanded
   :make-expanded
   ;; Core API
   :full-arguments-from-storage
   :full-arguments-from-circuit
   :full-return-values
   :full-type-reference*
   :argument-names))

(defpackage #:alu.pass.relocation
  (:documentation "Provides mapping and functionality required to
safely relocate record instances and generate out code which lacks records")
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.linear-spec)
                    (:storage :alu.storage)
                    (:expand  :alu.pass.expanded)
                    (:closure :alu.closure))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; Type API
   :rel
   :rel-closure
   :rel-forms
   :rel-p
   :make-rel
   ;; Core API
   :relocate-let
   :relocate-standalone
   :initial-closure-from-circuit
   :maps-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.anf
  (:documentation "Provides an ANF pass for the alucard term")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec))
  (:export :normalize-expression))

(defpackage #:alu.pass.extract
  (:documentation "Provides Extraction capabilities to vamp-ir")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (#:util #:alu.utils)
                    (#:aspc #:alu.pass.linear-spec)
                    (#:vspc #:alu.vampir.spec))
  (:export))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util     :alu.utils)
                    (:spc      :alu.pass.linear-spec)
                    (:anf      :alu.pass.anf)
                    (:expand   :alu.pass.expanded)
                    (:relocate :alu.pass.relocation)
                    (:storage  :alu.storage)
                    (:closure  :alu.closure)
                    (:extract  :alu.pass.extract)
                    (:vampir   :alu.vampir))
  (:export
   :pipeline
   :to-expand-away-records))


