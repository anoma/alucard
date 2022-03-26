(in-package :alu-test)

(def-suite alucard.relocation
  :description "Test the relocation functionality")

(in-suite alucard.relocation)


(defparameter *example-closure*
  (closure:allocate
   :fi '((:plane . :fi-plane)
         (:point . ((:x . :fi-point-x) (:y . :fi-point-y))))))

(defparameter *example-bind*
  (spc:make-bind :var :hi
                 :val (spc:make-reference :name :fi)))

(test relocate-let-ref
  (let ((expected-let-names '(:hi-plane :hi-point-x :hi-point-y))
        (expected-let-resul '(:fi-plane :fi-point-x :fi-point-y))
        (expected-storage   '((:PLANE . :HI-PLANE)
                              (:POINT
                               (:X . :HI-POINT-X)
                               (:Y . :HI-POINT-Y))))
        (relocation (relocate::relocate-let *example-bind* *example-closure*)))
    (mapcar (lambda (input res bind)
              (is (eq input (spc:var bind)))
              (is (eq res (spc:name (spc:value bind)))))
            expected-let-names
            expected-let-resul
            (relocate::rel-forms relocation))
    (is (equalp expected-storage (closure:lookup (relocate::rel-closure relocation)
                                                 :hi)))))
