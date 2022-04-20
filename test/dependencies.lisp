(in-package :alu-test)

(def-suite alucard.dependencies
  :description "Test dependency tracking")

(in-suite alucard.dependencies)

(test circuit-dependency
  (let ((expected-deps '#.(sort '(:* :+ :=) #'util:hash-compare))
        (ran           (sort (dep:track-circuit-deps
                                (storage:lookup-function :constrain))
                             #'util:hash-compare)))
    (is (equalp (sort (dep:track-circuit-deps
                       (storage:lookup-function :constrain))
                      #'util:hash-compare)
                ran))))

(test circuit-dependency*
  (let ((expected-deps '#.(sort '(:constrain :* :+ :=) #'util:hash-compare))
        (ran           (sort (dep:track-circuit-deps*
                              (storage:lookup-function :use-constrain))
                             #'util:hash-compare)))
    (is (equalp expected-deps ran))))
