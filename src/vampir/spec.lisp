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

(deftype constraint-list ()
  `(satisfies constraint-list))

(deftype normal-form-list ()
  `(satisfies normal-form-list))

(defun constraint-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'constraint)) list)))

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
         :accessor body
         :type     constarint-list))
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
  ((func :initarg :function
         :accessor func
         :type     keyword
         :documentation "the alias we are calling")
   (arguments :initarg :arguments
              :initform nil
              :type     list
              :accessor arguments
              :documentation "The arguments in which the gate is called upon")))

(defclass bind ()
  ((names :initarg :names
          :accessor names
          :type     normal-form-list)
   (value :initarg :value
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
;; Normal Form Product Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass wire ()
  ((var :initarg :var
        :accessor var
        :type     keyword))
  (:documentation "A reference in vamp-ir"))

(defclass constant ()
  ((const :initarg :const
          :accessor const)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alias
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj alias) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A -> ~A = ~A"
            (name obj)
            (inputs obj)
            (outputs obj)
            (body obj))))

(defun make-alias (&key name inputs outputs body)
  (make-instance 'alias :name name :inputs inputs :outputs outputs :body body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pub
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj pub) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (wires obj))))

(defun make-pub (&key wires)
  (make-instance 'pub :wires wires))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Infix
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj infix) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A" (lhs obj) (op obj) (rhs obj))))

(defun make-infix (&key lhs op rhs)
  (make-instance 'infix :lhs lhs :op op :rhs rhs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj application) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~{~A~^ ~}" (func obj) (arguments obj))))

(defun make-application (&key func arguments)
  (make-instance 'application :function func :arguments arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj bind) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A = ~A" (names obj) (value obj))))

(defun make-bind (&key names value)
  (make-instance 'bind :names names :value value))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Equality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj equality) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A = ~A" (lhs obj) (rhs obj))))

(defun make-equality (&key lhs rhs)
  (make-instance 'equality :lhs lhs :rhs rhs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wire
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj wire) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (var obj))))

(defun make-wire (&key var)
  (make-instance 'wire :var var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj constant) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (const obj))))

(defun make-constant (&key const)
  (make-instance 'constant :const const))
