(in-package :alu.pass.linear-term)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types Variants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype linear-term ()
  "A Linear term is a term with no nested terms and is in proper ANF form."
  `(or spc:term-no-binding
       spc:bind-constraint
       bind
       ret))

(deftype binders ()
  "Terms which deal with binding and naming"
  `(or bind
       multiple-bind
       multi-ret
       ret
       spc:bind-constraint))

(deftype expanded-term ()
  "An expanded term is a `linear-term' with an expanded binder for
multiple return values along with return-value types"
  `(or linear-term
       binders))

;; would use `(and expanded-term (not spc:record-forms))
;; however I'd lose exhaustion ☹
(deftype fully-expanded-term ()
  "A fully expanded term is a `expanded-term' with the records part
removed. Or as we can view it a base term, with the binders added in."
  `(or spc:base
       binders))

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
  ((variable :initarg  :variable
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

(defclass multi-ret ()
  ((variable :initarg  :variable
             :accessor spc:var
             :type     list
             :documentation "The name that will be returned")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "Values that are returned"))
  (:documentation "An explicit return which may have many values.
Many returns in a single function may be had, they are all ordered."))

(defclass ret ()
  ((variable :initarg  :variable
             :type     keyword
             :accessor spc:var
             :documentation "The name that will be returned")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is returned"))
  (:documentation "An explicit return which. Many returns in a single
function may be had, they are all ordered."))

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
;; Multiple Return Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj multi-ret) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A = ~A" (spc:var obj) (spc:value obj))))


(defun make-multi-ret (&key
                         (val (error "Please provide the return fields"))
                         (var (error "Please provide the return name")))
  (values
   (make-instance 'multi-ret :value val :variable var)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj ret) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A = ~A" (spc:var obj) (spc:value obj))))


(defun make-ret (&key (val (error "Please provide the return fields"))
                      (var (error "Please provide the return name")))
  (values
   (make-instance 'ret :value val :variable var)))
