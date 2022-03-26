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

(defparameter *example-bind-app*
  (spc:make-bind
   :var :hi
   :val (spc:make-application :function (spc:make-reference :name :arg-test)
                              :arguments '(1 5 6))))

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

(test relocate-let-app
  (let ((storage:*types*     (clone storage:*types*))
        (storage:*functions* (clone storage:*functions*)))

    (alu:deftype nested ()
      (plane point)
      (time  point))

    (alu:deftype point ()
      (x int)
      (y int))

    (alu:defcircuit arg-test ((output nested))
      arg-test)

    (let ((expected-binds   '(:HI-TIME-X :HI-TIME-Y :HI-PLANE-X :HI-PLANE-Y))
          (expected-storage '((:TIME
                               (:X . :HI-TIME-X)
                               (:Y . :HI-TIME-Y))
                              (:PLANE
                               (:X . :HI-PLANE-X)
                               (:Y . :HI-PLANE-Y))))
          (relocation (relocate::relocate-let *example-bind-app*
                                              *example-closure*)))

      ;; Tests begin here
      (is (equalp expected-binds (spc:var (relocate::rel-forms relocation))))
      (is (equalp (spc:value *example-bind-app*)
                  (spc:value (relocate::rel-forms relocation)))
          "The function should not change")
      (is (equalp expected-storage
                  (closure:lookup (relocate::rel-closure relocation)
                                  :hi))))))
