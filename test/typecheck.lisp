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

(test Running-the-type-checker
  (flet ((type-check (x)
           (pipeline:to-typecheck (storage:lookup-function x))))
    (finishes (type-check :constrain))
    (finishes (type-check :poly-check))
    (finishes (type-check :array-lookup-equation))
    (finishes (type-check :int-return))
    (finishes (type-check :array-type-check))
    (finishes (type-check :array-creation-check))
    (finishes (type-check :array-from-data-check))
    (finishes (type-check :array-from-data-check-consts))
    (finishes (type-check :basic-unification))
    (fiveam:signals error (type-check :invalid-array-type-check))
    (fiveam:signals error (type-check :invalid-type-unification))
    (fiveam:signals error (type-check :invalid-record-lookup))
    (fiveam:signals error (type-check :invalid-record-lookup-type))
    (fiveam:signals error (type-check :manual-constraint))
    (fiveam:signals error (type-check :invalid-record-primitive-2))
    ;; different error than 2
    (fiveam:signals error (type-check :invalid-record-primitive))
    (fiveam:signals error (type-check :invalid-record-creation))
    (fiveam:signals error (type-check :invalid-application-unification))
    (fiveam:signals error (type-check :invalid-type-check))))
