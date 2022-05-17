(in-package :alu-test)


(def-suite alucard.pass.anf
    :description "Tests the alucard package")

(in-suite alucard.pass.anf)

(test anf-linearizes-application
  (let ((normalized
          (anf:normalize-expression
           (ir:make-application
            :function (ir:make-reference :name :hi)
            :arguments
            (list (ir:make-application
                   :function (ir:make-reference :name :fun2)
                   :arguments (list (ir:make-reference :name :hi)
                                    (ir:make-application
                                     :function (ir:make-reference :name :fun3)
                                     :arguments
                                     (list (ir:make-reference :name :hi)))))
                  (ir:make-reference :name :bob))))))
    ;; We sadly lack object equality for clos classes, so thus we test it this way
    (is (= 3 (length normalized))
        "normalization makes a let term over application, at this point 3 times!")
    (is (typep (car (last normalized)) 'ir:application)
        "After the double let we should have the function application")
    (is (eq (ir:name (cadr (ir:arguments (car (last normalized)))))
            :bob)
        "bob is a straight line argument so it should be unchanged by the transform")))

(test anf-linearizes-records
  (let* ((normalized
           (anf:normalize-expression
            (ir:make-application
             :function (ir:make-reference :name :hi)
             :arguments
             (list (ir:make-application
                    :function (ir:make-reference :name :fun2)
                    :arguments
                    (list #1=(ir:make-application
                              :function (ir:make-reference :name :fun3)
                              :arguments
                              (list (ir:make-reference :name :hi)))
                          (ir:make-record-lookup
                           :record (ir:make-record :name :utxo
                                                    :owner 3
                                                    :amount 5
                                                    :nonce #1#)
                           :field  :nonce)))
                   (ir:make-reference :name :bob)))))
         (record (ir:value (caddr normalized)))
         (lookup (ir:value (cadddr normalized))))
    ;; We sadly lack object equality for clos classes, so thus we test it this way
    ;; I really should make equality objects
    (is (typep (ir:lookup-record record :nonce) 'ir:reference)
        "ANF should have made an application inside the record become a reference")
    (is (eq (ir:field lookup) :nonce)
        "The field that is looked up should not change")
    (is (typep (ir:record lookup) 'ir:reference)
        "the record lookup is now over a reference instead of the record directly")))

(test anf-constraint
  (let* ((normalized (anf:normalize-expression
                      (ir:make-bind-constraint
                       :var (list :a :b :c)
                       :value
                       (list
                        (ir:make-application
                         :function (ir:make-reference :name :=)
                         :arguments
                         (list (ir:make-application
                                :function (ir:make-reference :name :fun2)
                                :arguments
                                (list #1=(ir:make-application
                                          :function (ir:make-reference :name :fun3)
                                          :arguments
                                          (list (ir:make-reference :name :hi)))
                                      (ir:make-record-lookup
                                       :record (ir:make-record :name :utxo
                                                                :owner 3
                                                                :amount 5
                                                                :nonce #1#)
                                       :field  :nonce)))
                               (ir:make-reference :name :bob))))))))
    (is (typep normalized 'ir:bind-constraint)
        "constraint should hold the terms inside of it")
    (is (= 6 (length (ir:value normalized)))
        "The body should have ANF successfully run")))
