(in-package :alu-test)

(def-suite alucard.dependencies
  :description "Test dependency tracking")

(in-suite alucard.dependencies)

(test circuit-dependency
  (let ((expected-deps (sort (list :* :+ :=) #'util:hash-compare))
        (ran           (sort (dep:track-circuit-deps
                              (storage:lookup-function :constrain))
                             #'util:hash-compare)))
    (is (equalp expected-deps
                ran))))

(test circuit-dependency*
  (let ((expected-deps (sort (list :constrain :* :+ :=) #'util:hash-compare))
        (ran           (sort (dep:track-circuit-deps*
                              (storage:lookup-function :use-constrain))
                             #'util:hash-compare)))
    (is (equalp expected-deps ran))))
