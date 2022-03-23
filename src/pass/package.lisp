(defpackage #:alu.pass.linear-term
  (:documentation "Provides a simplified term structure that has been through
linearization, use alu.pass.linear-spec for the full specification")
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec))
  (:use #:common-lisp #:serapeum)
  (:export
   :constraint-list
   :linear-term
   :bind
   :make-bind))

;; I'm unsure of how to rexport values but not use them, so we make
;; this package until I figure this out
(uiop:define-package #:alu.pass.linear-spec
  (:documentation "Defines out the specification of the linear term,
reusing values from alu.spec and alu.pass.linear-term")
  (:use #:common-lisp #:serapeum)
  (:use-reexport :alu.spec :alu.pass.linear-term))

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
                    (:storage :alu.storage))
  (:export))


