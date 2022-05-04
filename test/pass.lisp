(in-package :alu-test)

(def-suite alucard.pass
  :description "Test the compiler pipeline")

(in-suite alucard.pass)

(test to-expand-away-records
  (let* ((look (pipeline:to-expand-away-records
                (storage:lookup-function :record-test)))
         (multi-let (remove-if-not (lambda (x) (typep x 'spc:multiple-bind))
                                   look))
         (multi-ret (remove-if-not (lambda (x) (typep x 'spc:standalone-ret))
                                   look)))
    (is (= 3
           (~> multi-let car spc:value spc:arguments length))
        "The point type should expand into two arguments")
    (is (= 4 (length (spc:var (car multi-ret))))
        "The nested type should be expanded into output")))

(test to-expand-away-records-intermediate
  (let* ((look (pipeline:to-expand-away-records
                (storage:lookup-function :record-test-mult)))
         (multi-lets (remove-if-not (lambda (x) (typep x 'spc:multiple-bind))
                                    look))
         (returns    (remove-if-not (lambda (x) (typep x 'spc:standalone-ret))
                                    look)))
    (is (= 3
           (~> multi-lets car spc:value spc:arguments length))
        "The point type should expand into two arguments")
    (is (= 2 (length (spc:var (car returns))))
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

(test trans-let
  (let ((term
          (spc:make-bind-constraint
           :var (list :a :b :c)
           :value
           (list
            (spc:make-application
             :function (spc:make-reference :name :=)
             :arguments
             (list (spc:make-application
                    :function (spc:make-reference :name :fun2)
                    :arguments
                    (list #1=(spc:make-application
                              :function (spc:make-reference :name :fun3)
                              :arguments
                              (list (spc:make-reference :name :hi)))
                          (spc:make-record-lookup
                           :record (spc:make-record :name :utxo
                                                    :owner 3
                                                    :amount 5
                                                    :nonce #1#)
                           :field  :nonce)))
                   (spc:make-reference :name :bob)))))))
    (is
     ;; the type check is good enough to ensure that the pass works!
     (typep (alu.pass::transform-let (anf:normalize-expression term))
            'spc:constraint-list))))

(test constrain-example
  (let* ((circuit (storage:lookup-function :manual-constraint))
         (linear  (pass:linearize circuit))
         (record  (pass:expand-away-records linear circuit)))
    (is (equalp (spc:var (car (last linear)))
                (list :a :b :c))
        "The values in the constraint are returned if they are the last value")
    (is (typep (spc:value (car record)) 'spc:fully-expanded-list))))

(test standalone-ret-expansion
  (let* ((circuit  (storage:lookup-function :record-ret))
         (expanded (pipeline:to-expand-away-records circuit)))
    (is (< 1 (length (spc:var (car (last expanded))))))))

(test extraction
  (finishes (pipeline:pipeline (storage:lookup-function :poly-check)))
  (finishes (pipeline:pipeline (storage:lookup-function :record-test-mult)))
  (finishes (pipeline:pipeline (storage:lookup-function :manual-constraint))))
