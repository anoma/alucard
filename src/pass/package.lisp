;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Extending The Specification of Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.linear-term
  (:documentation "Provides a simplified term structures that has been through
linearization, use `alu.pass.spec' for the full specification")
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   ;; New Term Variants Defined
   :linear-term
   :expanded-term
   :fully-expanded-term
   :binders
   :starting-binders
   ;; New Term Lists Defined
   :constraint-list
   :expanded-list
   :fully-expanded-list
   ;; New Types Defined
   :bind
   :multiple-bind
   :standalone-ret
   ;; New Constructors Defined
   :make-bind
   :make-multi-ret
   :make-standalone-ret
   :make-multiple-bind))

(defpackage #:alu.pass.primitive-global
  (:documentation "Provides a more low level representation of the
global structure")
  (:local-nicknames (#:spc #:alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   :prim-circuit :returns
   :make-prim-circuit))

(uiop:define-package #:alu.pass.spec
  (:documentation "Defines out the specification of the pass terms,
namely linear terms and more primitive global type storage, this
package strictly adds to the `alu.spec' package")
  (:use #:common-lisp #:serapeum)
  (:use-reexport :alu.spec :alu.pass.linear-term :alu.pass.primitive-global))

(defpackage #:alu.pass.typecheck
  (:local-nicknames (#:spc     #:alu.pass.spec)
                    (#:closure #:alu.closure))
  (:use #:common-lisp #:serapeum))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Expanding Away And relocating Record Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.expanded
  (:documentation "Provides argument expansion functionality.
Including circuit declaration expansion, and function type
expansion.")
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.spec)
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
                    (:spc     :alu.pass.spec)
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
   :initial-closure-from-circuit
   :maps-to))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.evaluate-body
  (:documentation "Provides initial evaluation of the circuit body, modifying the
circuits execution body and tracking caching")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.pass.spec)
                    (:emit :alu.spec.emit))
  (:export :evaluate-circuit-body :evaluate-and-cache-body))

(defpackage #:alu.pass.anf
  (:documentation "Provides an ANF pass for the alucard term")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.spec))
  (:export :normalize-expression))

(defpackage #:alu.pass.extract
  (:documentation "Provides Extraction capabilities to vamp-ir")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (#:util    #:alu.utils)
                    (#:aspc    #:alu.pass.spec)
                    (#:vspc    #:alu.vampir.spec)
                    (#:storage #:alu.storage))
  (:export :circuit-to-alias))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util     :alu.utils)
                    (:spc      :alu.pass.spec)
                    (:eval     :alu.pass.evaluate-body)
                    (:anf      :alu.pass.anf)
                    (:expand   :alu.pass.expanded)
                    (:relocate :alu.pass.relocation)
                    (:storage  :alu.storage)
                    (:closure  :alu.closure)
                    (:extract  :alu.pass.extract)
                    (:vampir   :alu.vampir))
  (:export
   :linearize
   :expand-away-records
   :remove-void-bindings
   :primtitve-circuit
   :rename-primitive-circuit
   ;; Extraction
   :circuit-to-alias))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Extra Information Tracking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.dependencies
  (:documentation "Provides an API for dependency tracking")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.spec)
                    (:pass    :alu.pass)
                    (:storage :alu.storage))
  (:export
   :track-circuit-deps
   :track-circuit-deps*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding the Pipeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defpackage #:alu.pipeline
  (:documentation "Provides The Alucard Pipeline down to ANF")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.spec)
                    (:pass    :alu.pass)
                    (:vampir  :alu.vampir)
                    (:dep     :alu.pass.dependencies)
                    (:storage :alu.storage))
  (:export
   :dump-entry-point
   :dump-entry-point-to-file
   :pipeline
   :print-vampir
   ;; Intermediate steps
   :to-expand-away-records
   :to-primitive-circuit
   :to-vampir))


