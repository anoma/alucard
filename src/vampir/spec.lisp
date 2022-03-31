(in-package :alu.vampir.spec)

;; Here we use the language of vampir to talk about the components

;; Adapted form
;; https://github.com/heliaxdev/ark-plonk/blob/plonk-ir/plonk-ir/src/plonk_ir.pest

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sum Type Declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype statement ()
  `(or alias pub constraint))

(deftype constraint ()
  `(or application bind equality))

;; called base in the file
;; Values are called over a normal form!?!?!?
(deftype expression ()
  `(or infix application normal-form))

(deftype normal-form ()
  `(or wire constant))

(deftype primitive ()
  `(or (eql :+) (eql :-) (eql :*) (eql :^)))

(deftype normal-form-list ()
  `(satisfies normal-form-list))

(defun normal-form-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'normal-form)) list)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Product Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass alias ()
  ((name :initarg :name
         :type    keyword
         :accessor name
         :documentation "Name of the alias gate")
   (inputs :initarg :inputs
           :type    list
           :accessor inputs
           :documentation "the arguments to the circuit")
   (outputs :initarg :outputs
            :type    list
            :accessor outputs)
   ;; TODO :: layout types
   (body :initarg :body
         :accessor body))
  (:documentation "An alias gate in vamp-ir"))


(defclass pub ()
  ((wires :initarg :wires
          :type    list
          :accessor wires)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Product Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass infix ()
  ((op :initarg :op
       :accessor op
       :type     primitive
       :documentation "the alias we are calling")
   (lhs :initarg  :lhs
        :accessor lhs
        :type     expression
        :documentation "the argument to the left of the op")
   (rhs :initarg  :rhs
        :accessor rhs
        :type     expression
        :documentation "the argument to the right of the op")))

(defclass application ()
  ((name :initarg :function
         :accessor func
         :type     keyword
         :documentation "the alias we are calling")
   (arguments :initarg :args
              :initform nil
              ;; we could argue the list is just over wires and not constants!
              :type     normal-form-list
              :accessor arguments
              :documentation "The arguments in which the gate is called upon")))

(defclass bind ()
  ((name :initarg :name
         :accessor name
         :type     keyword)
   (app :initarg :value
        :accessor value
        ;; can't be a constant however!
        :type     expression)))

(defclass equality ()
  ((lhs :initarg  :lhs
        :accessor lhs
        :type     expression
        :documentation "the argument to the left of the =")
   (rhs :initarg  :rhs
        :accessor rhs
        :type     expression
        :documentation "the argument to the rigth of the =")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Normal Form Product Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass wire ()
  ((var :initarg :var
        :accessor var
        :type     keyword)))

(defclass constant ()
  ((const :initarg :const
          :accessor const)))
