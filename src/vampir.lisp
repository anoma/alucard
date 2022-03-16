;; Requires
;; 1. a CL compiler
;; 2. quicklisp for trivia, the pattern matcher

;; Current TODOS:
;; 1. create a pretty printer for the term back.
;; 2. figure out target and write a compiler for that.

;; 3. try out the method of scanning for unknown variables, then
;; binding it to a variable in system. We can then reuse CL functions
(defpackage #:vampir
  (:documentation "Provides a vampir representation")
  (:use :common-lisp :trivia)
  (:shadow :=)
  (:export :defpoly :poly))

(in-package :vampir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Structure Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype expression ()
  `(or symbol
       number
       custom-gate
       apply-custom-gate
       eq-relation
       block-relations
       primitive))

(deftype applyable-gates ()
  `(or custom-gate
       symbol
       apply-custom-gate))

;; This is our gate type
(deftype gate ()
  `(or apply-custom-gate
       eq-relation
       block-relations
       primitive))


(defstruct circuit-machine
  ;; HashTable Symbol gate
  (defs   (make-hash-table) :type hash-table)
  ;; HashTable Symbol table
  (tables (make-hash-table) :type hash-table))

(defstruct block-relations
  ;; list expressions
  (gates '() :type cl:list)
  ;; this will stand for the local equations like
  ;;
  ;;  mx1 my1 = curve_add x1 y1 x2 y2
  ;;
  ;; Not modeled yet however
  )

(defstruct custom-gate
  (name (error "fill in by hand") :type cl:symbol)
  (returns '() :type list)
  (fields  '() :type cl:list)
  (equation (error "fill in by hand") :type gate))

(defstruct table)

(defstruct primitive
  (op   'nil :type symbol)
  (arg1 'nil :type expression)
  (arg2 'nil :type expression))

;; a b = gate x y
;; gate x y a b
;; gate-1 (gate x y) c d

(defstruct apply-custom-gate
  (gate      (error "fill in by hand") :type applyable-gates)
  (arguments '()                       :type list))

(defstruct eq-relation
  (values '() :type list)
  gate)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Declaration for primitives and circuit machinery ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *circuit-machine*
  (let ((hash (make-hash-table)))
    (setf (gethash '- hash) '-)
    (setf (gethash '+ hash) '+)
    (setf (gethash '* hash) '*)
    (setf (gethash 'expt hash) 'expt)
    (make-circuit-machine :defs hash)))

(deftype primitive-ops ()
  `(or (eql +)
       (eql -)
       (eql *)
       (eql expt)))

(declaim (ftype (function ((or symbol primitive-ops)) boolean)
                primitive-op?))
(defun primitive-op? (symbol)
  (typep symbol 'primitive-ops))

(defun eq-op? (symbol)
  (eq '= symbol))

(defun gate-block (eqs)
  (make-block-relations :gates eqs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User Facing Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defpoly (name arguments returns cl:&rest body)
  "defpoly defines a polynomial relation to the system. If multiple
polynomial expressions are given for the body then a set of gates is constructed"
  `(let ((value
           (make-custom-gate
            :name ',name
            :fields ',arguments
            :returns ',returns
            :equation ,(if (cl:= (cl:length body) 1)
                           `(raw-syntax-into-circuit ',(cl:car body))
                           `(gate-block
                             (mapcar #'raw-syntax-into-circuit
                                     ',body))))))
     (setf (gethash ',name (circuit-machine-defs *circuit-machine*))
           value)))

(defmacro poly (body)
  "poly creates a raw polynomial equation outside of defpoly"
  `(raw-syntax-into-circuit ',body))

(defmacro gates (cl:&rest eqs)
  "gates makes a block of raw polynomial equations"
  `(make-block-relations :gates (list ,@eqs)))

(defun gate-of (symbol)
  "gate-of lookups the given polynomial of the system.
Thus if we had (defpoly range-2 ...), then we can say

(gate-of 'range-2)

to see the polynomial"
  (values
   (gethash symbol (circuit-machine-defs *circuit-machine*))
   (gethash symbol (circuit-machine-tables *circuit-machine*))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions for processing polynomials
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This in essence the parser, it takes the raw list and turns it into
;; a gate
(declaim (ftype (function ((or symbol list number expression))
                          expression)
                raw-syntax-into-circuit))
(defun raw-syntax-into-circuit (raw-syntax)
  "raw-syntax-into-circuit turns a list of raw lisp syntax, into an
appropriate circuit expression."
  ;; Normal Haskell pattern matching, with guards being haskell guards
  ;; (list x x)  ---> hd : [x]
  ;; (cons x xs) ---> hd : tl
  (match raw-syntax
    ((guard (list x to-eval)
            (lisp-run? x))
     (raw-syntax-into-circuit (eval to-eval)))
    ((cons poly-function xs)
     (let ((arguments (mapcar #'raw-syntax-into-circuit xs)))
       (cond ((is-applyable-gate poly-function)
              (make-apply-custom-gate :arguments arguments
                                      :gate poly-function))
             ((primitive-op? poly-function)
              (make-primitive :op poly-function
                              :arg1 (car arguments)
                              :arg2 (cadr arguments)))
             ((eq-op? poly-function)
              (make-eq-relation
               :values (butlast arguments)
               :gate (car (last arguments))))
             (t
              (error "Value ~A: is not a registered function please define"
                     poly-function)))))
    ;; must be a symbol, or a number, or an expression. as per the
    ;; type specification
    (_ raw-syntax)))

(defun lisp-run? (symbol)
  (eq 'lisp symbol))

(declaim (ftype (function (t) t)
                is-applyable-gate))
(defun is-applyable-gate (value)
  (let ((lookup (gate-of value)))
    (cond ((or (null value) (and (symbolp value) (primitive-op? value)))
           nil)
          ((and (not (symbolp value)) (typep value 'applyable-gates))
           t)
          (t
           (is-applyable-gate lookup)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(poly (- (+ (expt a 2) (expt b 2)) (expt c 2)))

(defpoly range-2 (x) ()
  (- (expt b₀ 2) b₀)
  (- (expt b₁ 2) b₁)
  (- (+ (* 2 b₁) b₀) x))


(poly (range-2 x))

;; #S(CUSTOM-GATE
;;    :NAME RANGE-2
;;    :RETURNS NIL
;;    :FIELDS (X)
;;    :EQUATION #S(BLOCK-RELATIONS
;;                 :GATES (#S(PRIMITIVE
;;                            :OP -
;;                            :ARG1 #S(PRIMITIVE :OP EXPT :ARG1 B0 :ARG2 2)
;;                            :ARG2 B0)
;;                         #S(PRIMITIVE
;;                            :OP -
;;                            :ARG1 #S(PRIMITIVE :OP EXPT :ARG1 B1 :ARG2 2)
;;                            :ARG2 B1)
;;                         #S(PRIMITIVE
;;                            :OP -
;;                            :ARG1 #S(PRIMITIVE
;;                                     :OP +
;;                                     :ARG1 #S(PRIMITIVE :OP * :ARG1 2 :ARG2 B1)
;;                                     :ARG2 B0)
;;                            :ARG2 X))))

(defparameter *my-global-number-calc*
  (+ 23 55))

(defpoly example-poly-using-lisp (x) ()
  (= (lisp (+ 22 *my-global-number-calc*))
     (* x₁ x₂)))

;; #S(CUSTOM-GATE
;;    :NAME EXAMPLE-POLY-USING-LISP
;;    :RETURNS NIL
;;    :FIELDS (X)
;;    :EQUATION #S(EQ-RELATION
;;                 :VALUES (100)
;;                 :GATE #S(PRIMITIVE :OP * :ARG1 X1 :ARG2 X2)))

(defpoly example-using-range (x) ()
  (range-2 a b x)
  (range-2 z x y))


;; (defun example-using-range (x)
;;   (declare output ())
;;   (let ((a-wire (gensym-wire 'a))
;;         (b-wire (gensym-wire 'b))
;;         (y-wire (gensym-wire 'y)))
;;     (constraints
;;      (range-2 a b x)
;;      (range-2 z x y))))

;; #S(CUSTOM-GATE
;;    :NAME EXAMPLE-USING-RANGE
;;    :RETURNS NIL
;;    :FIELDS (X)
;;    :EQUATION #S(BLOCK-RELATIONS
;;                 :GATES (#S(APPLY-CUSTOM-GATE :GATE RANGE-2 :ARGUMENTS (A B X))
;;                         #S(APPLY-CUSTOM-GATE
;;                            :GATE RANGE-2
;;                            :ARGUMENTS (Z X Y)))))




;; Standard library examples

(defpoly add (x y) (z)
  (- (+ x y) z))

(defpoly add-2 (x y) ()
  (+ x y))

(poly (add x y z))

;; #S(APPLY-CUSTOM-GATE :GATE ADD :ARGUMENTS (X Y Z))

(poly (= z (add x y)))

;; #S(EQ-RELATION
;;    :VALUES (Z)
;;    :GATE #S(APPLY-CUSTOM-GATE :GATE ADD :ARGUMENTS (X Y)))

(defpoly curve-add (x1 y1 x2 y2) (x3 y3)
  (= x3 (+ x1 y1))
  (= y3 (+ x2 y2)))

(defpoly curve-add-four (x1 y2 x2 y2 x3 y3 x4 y4) (x5 y5)
  (= mx1 my1 (curve-add x1 y1 x2 y2))
  (= mx2 my2 (curve-add x3 y3 x4 y4))
  (= x5 y5 (curve-add mx1 my1 mx2 my2)))


(defpoly sub (x y) (z)
  (- (- x y) z))

(defpoly mul (x y) (z)
  (- (* x y) z))

(defpoly div (x y) (z)
  ;; is this right?
  (mul y x z))
