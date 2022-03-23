(in-package :alu.pass.linear-term)

(deftype linear-term ()
  `(or spc:term-no-binding
       bind))

(deftype constraint-list ()
  `(satisfies linear-list))

(defun linear-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'linear-term)) list)))

(defclass bind ()
  ((variable :initarg  :variable
             :type     keyword
             :accessor spc:var
             :documentation "The variable that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-no-binding
          :documentation "the value that is bound")))

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
