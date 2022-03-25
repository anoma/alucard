(in-package :alu.pass.linear-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype linear-term ()
  "A Linear term is a term with no nested terms and is in proper ANF form."
  `(or spc:term-no-binding
       bind))

(deftype expanded-term ()
  "An expanded term is a `linear-term' with an expanded binder for
multiple return values"
  `(or linear-term
       multiple-bind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types List Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype constraint-list ()
  "A constraint-list is a list of linear-terms"
  `(satisfies linear-list))

(deftype expanded-list ()
  "A constraint-list is a list of linear-terms"
  `(satisfies expanded-list))

(defun linear-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'linear-term)) list)))

(defun expanded-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'expanded-term)) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bind ()
  ((variable :initarg  :variable
             :type     keyword
             :accessor spc:var
             :documentation "The variable that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is bound"))
  (:documentation "A let with no body"))

(defclass multiple-bind ()
  ((variables :initarg  :variables
              :type     keyword
              :accessor spc:var
              :documentation "The variables that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is bound"))
  (:documentation "A let that can bind many return values"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constructors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj bind) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((value spc:value) (var spc:var)) obj
      (format stream "LET ~A = ~A" var value))))

(-> make-bind (&key (:var keyword) (:val spc:term-no-binding)) bind)
(defun make-bind (&key (var  (error "Please provide the variable"))
                       (val  (error "Please provide the value field")))
  (values
   (make-instance 'bind :value val :variable var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiple Bind Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj multiple-bind) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((value spc:value) (var spc:var)) obj
      (format stream "MULTI-LET ~A = ~A" var value))))

(-> make-multiple-bind
    (&key (:var list) (:val spc:term-no-binding)) multiple-bind)
(defun make-multiple-bind (&key (var  (error "Please provide the variable"))
                                (val  (error "Please provide the value field")))
  (values
   (make-instance 'multiple-bind :value val :variable var)))
