(in-package :alu.spec)

(defclass stack-mixin ()
  ((stack :initform (stack:get)
          :accessor stack))
  (:documentation
   "Provides the service of getting the stack to the given instruction"))

(defclass meta-mixin (stack-mixin) ()
  (:documentation
   "Provides out the service of all meta information. Thus we define out:

stack-mixin   service"))

(defgeneric copy-meta (obj1 obj2)
  (:method-combination progn)
  (:documentation "copies meta data from one object to another"))

(defmethod copy-meta progn ((obj1 stack-mixin) (obj2 stack-mixin))
  (setf (stack obj2) (stack obj1)))
