(in-package :alu.spec.term-op)


(defun add (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :+)
   :arguments arguments))

(defun times (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :*)
   :arguments arguments))

(defun = (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :=)
   :arguments arguments))

(defun exp (&rest arguments)
  (spc:make-application
   :function (spc:make-reference :name :exp)
   :arguments arguments))

(defun coerce (type value)
  (spc:make-type-coerce
   :typ   (spc:to-type-reference-format type)
   :value value))

(-> kind-of-pirmitive? (spc:reference (-> (spc:primitive) boolean)) boolean)
(defun kind-of-pirmitive? (ref predicate)
  (let* ((looked (storage:lookup-function (spc:name ref))))
    (etypecase-of (or spc:function-type null) looked
      ((or null spc:circuit) nil)
      (spc:primitive        (funcall predicate looked)))))

(-> void-reference? (spc:reference) boolean)
(defun void-reference? (ref)
  (kind-of-pirmitive? ref (lambda (v) (eql :void (spc:name v)))))
