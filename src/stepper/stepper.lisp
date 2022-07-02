(in-package :alu.stepper)

;; The strategy we wish to implement is straight forward.

;; (step (macro ...)) ⟶ we macroexpand the macro
;;
;; (step (special-form ...)) ⟶ We handle this on a case by case basis
;;
;; (step (alucard-special-macro ...)) ⟶ Handle the same as special-form
;;
;; (step (function ...)) ⟶ generates out
;;   (prog2 (push (function …)) (function ,@(mapcar #'step …)) (pop))
;; (step number) ⟶ generates: number
;; (step string) ⟶ generates: string
;; (step symbol) ⟶ generates: symbol



(defun step (holder)
  holder)
