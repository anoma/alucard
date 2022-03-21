(defpackage #:alu.pass.linear-term
  (:documentation "Provides a simplified term structure that has been
through linearization")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util :alu.utils)
                    (:spc  :alu.spec))
  (:export))

(defpackage #:alu.pass
  (:documentation "Provides simplification passes to the Alucard Language")
  (:use #:common-lisp #:serapeum)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage))
  (:export))
