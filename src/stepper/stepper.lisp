(in-package :alu.stepper)

;; The strategy we wish to implement is straight forward.

;; (step (macro ...)) ⟶ we macroexpand the macro
;;
;; (step (special-form ...)) ⟶ We handle this on a case by case basis
;;
;; (step (alucard-special-macro ...)) ⟶ Handle the same as special-form
;;
;; (step (function ...)) ⟶ generates out
;;   (prog2 (push (function …)) (function ,@(map car #'step …)) (pop))
;; (step number) ⟶ generates: number
;; (step string) ⟶ generates: string
;; (step symbol) ⟶ generates: symbol

;; For the list of special forms see
;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node59.html

;; to actually check one should use `special-operator-p' instead of
;; (typep obj 'specials).
(deftype specials ()
  "The special forms of the CL language"
  `(or ,@(mapcar
          (lambda (x) `(eql ,x))
          `(block catch eval-when flet function go if
                  labels let let* load-time-value locally
                  macrolet multiple-vlaue-call
                  multiple-value-prog1 progn progv
                  quote return-from setq symbol-macrolet
                  tagbody the throw unwind-protect))))

(deftype alu-specials ()
  "The special forms of the Alucard language"
  `(or (eql alu:def) (eql alu:with-constraint)
       ;; these we count as special due to the type declaration
       ;; forcing them to be macros
       (eql alu:coerce) (eql alu:check)
       (eql alu:array)))

(deftype step-mode ()
  `(or (eql :stack) (eql :run)))

(defvar *step-mode* :stack
  "Determines what mode to run in.
:stack put user syntactical forms on the stack.
:run   leaves the user program unperturbed.")


(defun step (holder)
  holder)

(defun handle-cl-special (form)
  (typecase-of specials (car form)
    ((eql block))
    ((eql catch))
    ((eql eval-when))
    ((eql flet))
    ((eql function))
    ((eql go))
    ((eql if))
    ((eql labels))
    ((eql let))
    ((eql let*))
    ((eql load-time-value))
    ((eql locally))
    ((eql macrolet))
    ((eql multiple-vlaue-call))
    ((eql multiple-value-prog1))
    ((eql progn))
    ((eql progv))
    ((eql quote))
    ((eql return-from))
    ((eql setq))
    ((eql symbol-macrolet))
    ((eql tagbody))
    ((eql the))
    ((eql throw))
    ((eql unwind-protect))
    (otherwise
     (error "special ~A not supported yet" (car form)))))

(defun handle-alu-special (form)
  (typecase-of alu-specials (car form)
    ((eql alu:def))
    ((eql alu:with-constraint))
    ((eql alu:coerce))
    ((eql alu:check))
    ((eql alu:array))
    (otherwise (error "Alucard Special ~A handed to handle-alu special"
                      form))))
