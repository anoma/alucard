(in-package :alu.ir.new-terms)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Terms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass bind (spc:meta-mixin)
  ((var :initarg  :variable
        :type     keyword
        :accessor spc:var
        :documentation "The variable that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-type-manipulation
          :documentation "the value that is bound"))
  (:documentation "A let with a more restrictive value type"))

(defclass multiple-bind (spc:meta-mixin)
  ((variables :initarg  :variables
              :type     list
              :accessor spc:var
              :documentation "The variables of type `keyword' that will be bound")
   (value :initarg :value
          :accessor spc:value
          :type     spc:term-type-manipulation
          :documentation "the value that is bound"))
  (:documentation "A let that can bind many return values"))

(defclass standalone-ret (spc:meta-mixin)
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

(-> make-bind (&key (:var keyword) (:val spc:term-type-manipulation)) bind)
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
