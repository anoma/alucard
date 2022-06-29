(in-package :alu.reference)

(defstruct ref contents)

(defun ref (x)
  "Creates a reference out of x"
  (make-ref :contents x))

(defun ! (ref)
  "Grabs the contents of a reference"
  (ref-contents ref))

(defun (setf !) (x ref)
  "sets the reference value to x"
  (setf (ref-contents ref) x))
