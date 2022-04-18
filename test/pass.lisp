(in-package :alu-test)

(def-suite alucard.pass
  :description "Test the compiler pipeline")

(in-suite alucard.pass)

(test to-expand-away-records
  (let* ((look (pipeline:to-expand-away-records
                (storage:lookup-function :record-test)))
         (multi-ret (remove-if-not (lambda (x) (typep x 'spc:multi-ret))
                                   look)))
    (is (= 3
           (~> multi-ret car spc:value spc:arguments length))
        "The point type should expand into two arguments")
    (is (= 4 (length (spc:var (car multi-ret))))
        "The nested type should be expanded into output")))

(test to-expand-away-records-intermediate
  (let* ((look (pipeline:to-expand-away-records
                (storage:lookup-function :record-test-mult)))
         (multi-lets (remove-if-not (lambda (x) (typep x 'spc:multiple-bind))
                                    look))
         (returns    (remove-if-not (lambda (x) (typep x 'spc:ret))
                                    look)))
    (is (= 3
           (~> multi-lets car spc:value spc:arguments length))
        "The point type should expand into two arguments")
    (is (= 2 (length returns))
        "The nested type should be expanded into output")))

(test void-removal
  (let ((ran (pipeline:to-expand-away-records
              (storage:lookup-function :use-constrain))))
    (is (= 1 (length ran)))))

(test renaming
  (let ((expected-args '(:ROOT :SIG :UTX_PLANE_X :UTX_PLANE_Y
                         :UTX_TIME_X :UTX_TIME_Y))
        (ran  (pipeline:to-primitive-circuit (storage:lookup-function :record-test)))
        (ran2 (pipeline:to-primitive-circuit (storage:lookup-function :record-test-mult)))
        (ran3 (pipeline:to-primitive-circuit (storage:lookup-function :use-constrain))))
    ran2 ran3
    (is (equalp expected-args (spc:arguments ran))
        "Renaming is consistent")
    (is (every (lambda (x)
                 (not
                  (or (string-contains-p "&" x)
                      (string-contains-p "-" x)
                      (string-contains-p "%" x))))
               (spc:returns ran)))))

(test extraction
  (finishes (pipeline:pipeline (storage:lookup-function :poly-check)))
  (finishes (pipeline:pipeline (storage:lookup-function :record-test-mult))))
