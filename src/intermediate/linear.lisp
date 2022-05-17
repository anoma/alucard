(in-package :alu.ir.linear)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype linear-term ()
  "A Linear term is a term with no nested terms and is in proper ANF form."
  `(or spc:term-no-binding
       (starting-binders spc:term-no-binding)
       standalone-ret))

(deftype expanded-term ()
  "An expanded term is a term where all top level forms have been
expanded into lets or returns"
  `(or (starting-binders spc:term-no-binding)
       standalone-ret))

(deftype fully-expanded-term ()
  "A fully expanded term is a `expanded-term' with the records part
removed. Or as we can view it a base term, with the binders added in."
  `(or (binders spc:base)
       standalone-ret))

(deftype starting-binders (&optional contains)
  "Terms which deal with binding and naming, the input argument
represents what data may be in the value of the binders."
  (declare (ignore contains))
  `(or bind
       spc:bind-constraint))

(deftype binders (&optional contains)
  "Terms which deal with binding and naming, the input argument
represents what data may be in the value of the binders."
  `(or (starting-binders ,contains)
       multiple-bind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types List Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype constraint-list ()
  "A constraint-list is a list of linear-terms"
  `(satisfies linear-list))

(deftype expanded-list ()
  "A constraint-list is a list of expanded-terms"
  `(satisfies expanded-list))

(deftype fully-expanded-list ()
  "A constraint-list is a list of fully-expanded-terms"
  `(satisfies fully-expanded-list))

(defun linear-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'linear-term)) list)))

(defun expanded-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'expanded-term)) list)))

(defun fully-expanded-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'fully-expanded-term)) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bind ()
  ((var :initarg  :variable
             :type     keyword
             :accessor spc:var
             :documentation "The variable that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is bound"))
  (:documentation "A let with a more restrictive value type"))

(defclass multiple-bind ()
  ((variables :initarg  :variables
              :type     list
              :accessor spc:var
              :documentation "The variables of type `keyword' that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is bound"))
  (:documentation "A let that can bind many return values"))

(defclass standalone-ret ()
  ((variable :initarg  :variable
             :type     list
             :accessor spc:var
             :documentation "The name that will be returned"))
  (:documentation "Values which will be returned"))

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
   (make-instance 'multiple-bind :value val :variables var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmethod print-object ((obj standalone-ret) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~{~A~^, ~}" (spc:var obj))))


(defun make-standalone-ret (&key (var (error "Please provide the return name")))
  (values
   (make-instance 'standalone-ret :variable var)))
