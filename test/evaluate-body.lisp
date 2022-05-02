(in-package :alu-test)

(def-suite alucard.evaluate-body
  :description "Test behavior related to adding to the body")

(in-suite alucard.evaluate-body)

(test eval:evaluate-circuit-body
  (let ((expected (list 5 6 7)))
    (is (equalp expected
                (alu.pass.evaluate-body:evaluate-circuit-body
                 `(progn (emit:instruction 5)
                         (emit:instruction 6)
                         (emit:instruction 7)))))))
