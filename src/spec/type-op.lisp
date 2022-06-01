(in-package :alu.spec.type-op)

(-> kind-of-pirmitive? (spc:type-reference (-> (spc:primitive) boolean)) boolean)
(defun kind-of-pirmitive? (ref predicate)
  (let* ((name-to-lookup
           (etypecase-of spc:type-reference ref
             (spc:reference-type (spc:name ref))
             (spc:application    (spc:name (spc:func ref)))))
         (looked (storage:lookup-type name-to-lookup)))
    (etypecase-of (or spc:type-storage null) looked
      ((or null spc:type-declaration) nil)
      (spc:primitive                  (funcall predicate looked)))))

(-> record-reference? (spc:type-reference) boolean)
(defun record-reference? (ref)
  (let* ((name-to-lookup
           (etypecase-of spc:type-reference ref
             (spc:reference-type (spc:name ref))
             (spc:application    (spc:name (spc:func ref)))))
         (looked (storage:lookup-type name-to-lookup)))
    (etypecase-of (or spc:type-storage null) looked
      (spc:type-declaration    t)
      ((or null spc:primitive) nil))))

(-> primitive? (spc:type-reference) boolean)
(defun primitive? (ref)
  "Checks if the "
  (kind-of-pirmitive? ref (constantly t)))

(-> array-reference? (spc:type-reference) boolean)
(defun array-reference? (ref)
  (kind-of-pirmitive? ref (lambda (v) (eql :array (spc:name v)))))

(-> void-reference? (spc:type-reference) boolean)
(defun void-reference? (ref)
  (kind-of-pirmitive? ref (lambda (v) (eql :void (spc:name v)))))

(-> int-reference? (spc:type-reference) boolean)
(defun int-reference? (ref)
  (kind-of-pirmitive? ref (lambda (v)
                            (or (eql :int  (spc:name v))
                                (eql :bool (spc:name v))))))
