(in-package :alu.format)

(deftype function-type ()
  `(or circuit))

(defclass circuit ()
  ((name :initarg :name
         :type    keyword
         :accessor name
         :documentation "Name of the circuit")
   ;; arguments : sycamore.tree-map keyword constraint
   (arguments :initarg :arguments
              :type    sycamore:tree-map
              :initform (sycamore:make-tree-map #'util:hash-compare)
              :accessor arguments
              :documentation "Arguments for the circuit, saved in a
map from the argument name (keyword) to the `constraint'")
   (return-type :initarg  :return-type
                :type     type-reference
                :accessor return-type
                :documentation "The return output of a given circuit")
   (body :initarg  :body
         :type     alu-term
         :accessor body
         :documentation "The circuit logic")))

(deftype privacy ()
  `(or (eql :private)
       (eql :public)))

(defclass constraint ()
  ((privacy :initarg  :privacy
            :initform :private
            :type     privacy
            :accessor privacy
            :documentation "Is the constraint public or private?")
   (type :initarg  :type
         :type     type-reference
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
    (format stream "~A ~A" (privacy obj) (typ obj))))

(defun make-constraint (&key (privacy :private) type)
  (make-instance 'constraint :type type :privacy privacy))
