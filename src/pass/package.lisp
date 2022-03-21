;; we are use define-package just for reexport. Note that we can't
;; make package nicknames on older versions of asdf
(uiop:define-package #:alu.pass.linear-term
  (:documentation "Provides a simplified term structure that has been
through linearization")
  (:use #:common-lisp #:serapeum #:alu.spec)
  (:reexport :alu.spec)
  (:export))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.pass.linear-term)
                    (:storage :alu.storage))
  (:export))
