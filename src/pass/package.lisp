;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages Regarding Expanding Away and Relocating Record Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:alu.pass.expanded
  (:documentation "Provides argument expansion functionality.
Including circuit declaration expansion, and function type
expansion.")
  (:local-nicknames (:util    :alu.utils)
                    (:ir      :alu.ir)
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
                    (:ir      :alu.ir)
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
                    (:ir   :alu.ir)
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
                    (#:ir      #:alu.ir)
                    (#:spc     #:alu.vampir.spec)
                    (#:storage #:alu.storage))
  (:export :circuit-to-alias))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util     :alu.utils)
                    (:ir       :alu.ir)
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
                    (:ir      :alu.ir)
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
                    (:ir      :alu.ir)
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


