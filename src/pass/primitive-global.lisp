(in-package :alu.pass.primitive-global)

(defclass prim-circuit ()
  ((name :initarg :name
         :type    keyword
         :accessor spc:name
         :documentation "Name of the circuit")
   ;; a list of constraints
   (arguments :initarg :arguments
              :type    list
              :accessor spc:arguments
              :documentation "Arguments for the circuit, saved in a
a list of `keyword'")
   (returns :initarg  :returns
            :type     list
            :accessor returns
            :documentation "The return output of a given circuit
stored in a list of `keyword'")
   (body :initarg  :body
         :accessor spc:body
         :documentation "The circuit logic")))


(defmethod print-object ((obj prim-circuit) stream)
  (with-accessors ((name spc:name) (ret returns) (bod spc:body) (arg spc:arguments)) obj
    (print-unreadable-object (obj stream :type t)
      (format stream "~A~{ ~A~^~} -> ~{~A~^ ~} =~%~A : ~A" name arg ret bod ret))))

(defun make-prim-circuit (&key name arguments returns body)
  (make-instance 'prim-circuit
                 :name name
                 :body body
                 :returns returns
                 :arguments arguments))
