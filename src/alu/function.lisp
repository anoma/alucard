(in-package :alu)

(cl:deftype alu-function-type ()
  `(or circuit))

(defclass circuit ()
  ((name
    :initarg :name
    :type    keyword
    :accessor name
    :documentation "Name of the circuit")
   (arguments
    :initarg :arguments
    :type    list
    :accessor arguments
    :documentation "Arguments for the circuit")
   (return-type
    :initarg  :return-type
    :type     alu-type-reference
    :accessor return-type
    :documentation "The return output of a given circuit")
   (body
    :initarg  :body
    :type     alu-expression
    :accessor body
    :documentation "The circuit logic")))

(cl:deftype privacy ()
  `(or (eql :private)
       (eql :public)))

(defclass constraint ()
  ((privacy
    :initarg  :privacy
    :initform :private
    :type     privacy
    :accessor privacy
    :documentation "Is the constraint public or private?")
   (name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "The name of the constraint")
   (type
    :initarg  :type
    :type     alu-type-reference
    :accessor typ
    :documentation "The name of the constraint")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuit Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj circuit) stream)
  (with-accessors ((name name) (ret return-type) (bod body)) obj
    (format stream "~A =~%~A : ~A" name bod ret)))

(defun make-circuit (&key name arguments return-type body)
  (make-instance 'circuit
                 :name name
                 :body body
                 :return-type return-type
                 :arguments arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj constraint) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A" (privacy obj) (name obj) (typ obj))))

(defun make-constraint (&key name (privacy :private) type)
  (make-instance 'constraint :type type :name name :privacy privacy))
