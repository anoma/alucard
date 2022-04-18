(in-package :alu-test)

(def-suite alucard.dependencies
  :description "Test dependency tracking")

(in-suite alucard.dependencies)

(test circuit-dependency
  (let ((expected-deps '(:* :+ :=))
        (ran           (dep:track-circuit-deps
                        (storage:lookup-function :constrain))))
    (is (equalp expected-deps ran)
        "The dependencies should track all function applications")))
