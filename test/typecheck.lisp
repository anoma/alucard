(in-package :alu-test)

(def-suite alucard.typecheck
    :description "Tests the type checker of alucard")

(in-suite alucard.typecheck)

(test type-equality-works-as-expected
  (is (check::type-equality (spc:to-type-reference-format '(int 32))
                            (spc:to-type-reference-format '(int 32))))
  (is (check::type-equality (spc:to-type-reference-format '(int bar))
                            (spc:to-type-reference-format '(int bar))))
  (is (null
       (check::type-equality (spc:to-type-reference-format '(int 64))
                             (spc:to-type-reference-format '(int 32))))))
