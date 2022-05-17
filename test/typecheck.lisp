(in-package :alu-test)

(def-suite alucard.typecheck
    :description "Tests the type checker of alucard")

(in-suite alucard.typecheck)

(test type-equality-works-as-expected
  (is (check::type-equality (ir:to-type-reference-format '(int 32))
                            (ir:to-type-reference-format '(int 32))))
  (is (check::type-equality (ir:to-type-reference-format '(int bar))
                            (ir:to-type-reference-format '(int bar))))
  (is (null
       (check::type-equality (ir:to-type-reference-format '(int 64))
                             (ir:to-type-reference-format '(int 32))))))


(test find-no-data
  (is
   (null
    (check::find-type-info :foo
                           (check::make-starting-hole
                            '(:foo :bar)
                            (make-instance 'check::typing-context))))))
