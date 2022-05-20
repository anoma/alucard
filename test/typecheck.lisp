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
  (finishes
    (check::annotate-circuit
     (storage:lookup-function :constrain)
     (alu.pass:linearize (storage:lookup-function :constrain))))
  (finishes
    (check::annotate-circuit
     (storage:lookup-function :poly-check)
     (alu.pass:linearize (storage:lookup-function :poly-check))))
  (finishes
    (check::annotate-circuit
     (storage:lookup-function :array-lookup-equation)
     (alu.pass:linearize (storage:lookup-function :array-lookup-equation))))
  (finishes
    (pipeline:to-typecheck (storage:lookup-function :int-return)))
  (finishes
    (pipeline:to-typecheck (storage:lookup-function :basic-unification)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-type-unification)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-record-lookup)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-record-lookup-type)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :manual-constraint)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-record-primitive-2)))
  ;; different error than 2
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-record-primitive)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-record-creation)))
  (fiveam:signals error
    (pipeline:to-typecheck (storage:lookup-function :invalid-application-unification))))
