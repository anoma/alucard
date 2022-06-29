(defpackage #:alu.reference
  (:documentation "Provides a mutable reference, that allows pass by
reference semantics in CL.")
  (:use #:cl)
  (:export :ref :ref-p :!))

