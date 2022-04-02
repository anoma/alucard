(in-package :alu-test)

(def-suite alucard.pass
  :description "Test the compiler pipeline")

(in-suite alucard.pass)


(test to-expand-away-records
  (let* ((look (alu.pass:to-expand-away-records
                (storage:lookup-function :record-test)))
         (multi-lets (remove-if-not (lambda (x) (typep x 'spc:multiple-bind))
                                    look))
         (returns    (remove-if-not (lambda (x) (typep x 'spc:ret))
                                    look)))
    (is (= 3
           (~> multi-lets car spc:value spc:arguments length))
        "The point type should expand into two arguments")
    (is (= 4 (length returns))
        "The nested type should be expanded into output")))

(test void-removal
  (let ((ran (alu.pass:to-expand-away-records
              (storage:lookup-function :use-constrain))))
    (is (= 1 (length ran)))))
