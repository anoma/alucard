(in-package :alu-test)

(def-suite alucard.typecheck
    :description "Tests the type checker of alucard")

(in-suite alucard.typecheck)

(test type-equality-works-as-expected
  (is (check:type-equality (ir:to-type-reference-format '(int 32))
                           (ir:to-type-reference-format '(int 32))))
  (is (check:type-equality (ir:to-type-reference-format '(int bar))
                           (ir:to-type-reference-format '(int bar))))
  (is (null
       (check:type-equality (ir:to-type-reference-format '(int 64))
                            (ir:to-type-reference-format '(int 32))))))


(test find-no-data
  (is
   (null
    (alu.typechecker.check::find-type-info
     (ir:make-reference :name :foo)
     :foo
     (check::make-starting-hole
      '(:foo :bar)
      (make-instance 'check:typing-context))))))

(test running-the-type-checker
  (flet ((type-check (x)
           (pipeline:to-typecheck (storage:lookup-function x))))
    (let ((old-categories (v:repl-categories)))
      (setf (v:repl-categories) nil)
      ;; Ignore categories
      (finishes (type-check :constrain))
      (finishes (type-check :poly-check))
      (finishes (type-check :array-lookup-equation))
      (finishes (type-check :int-return))
      (finishes (type-check :array-type-check))
      (finishes (type-check :array-creation-check))
      (finishes (type-check :array-from-data-check))
      (finishes (type-check :array-from-data-check-consts))
      (finishes (type-check :basic-unification))
      (signals error (type-check :invalid-array-type-check))
      (signals error (type-check :invalid-type-unification))
      (signals error (type-check :invalid-record-lookup))
      (signals error (type-check :invalid-record-lookup-type))
      (signals error (type-check :manual-constraint))
      (signals error (type-check :invalid-record-primitive-2))
      ;; different error than 2
      (signals error (type-check :invalid-record-primitive))
      (signals error (type-check :invalid-record-creation))
      (signals error (type-check :invalid-application-unification))
      (signals error (type-check :invalid-type-check))
      (setf (v:repl-categories) old-categories))))
