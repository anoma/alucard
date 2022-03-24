(in-package :alu-test)


(def-suite alucard.pass.anf
    :description "Tests the alucard package")

(in-suite alucard.pass.anf)

(test anf-linearizes-application
  (let ((normalized
          (anf:normalize-expression
           (spc:make-application
            :function (spc:make-reference :name :hi)
            :arguments
            (list (spc:make-application
                   :function (spc:make-reference :name :fun2)
                   :arguments (list (spc:make-reference :name :hi)
                                    (spc:make-application
                                     :function (spc:make-reference :name :fun3)
                                     :arguments
                                     (list (spc:make-reference :name :hi)))))
                  (spc:make-reference :name :bob))))))
    ;; We sadly lack object equality for clos classes, so thus we test it this way
    (is (typep normalized 'spc:let-node)
        "normalization makes a let term over application with inner expressions")
    (is (typep (spc:body normalized) 'spc:let-node)
        "We have two nested applications")
    (is (typep (spc:body (spc:body normalized)) 'spc:application)
        "After the double let we should have the function application")
    (is (eq (spc:name (cadr (spc:arguments (spc:body (spc:body normalized)))))
            :bob)
        "bob is a straight line argument so it should be unchanged by the transform")))

(test anf-linearizes-records
  (let* ((normalized
           (anf:normalize-expression
            (spc:make-application
             :function (spc:make-reference :name :hi)
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
                   (spc:make-reference :name :bob)))))
         (record (spc:value (spc:body (spc:body normalized))))
         (lookup (spc:value (spc:body (spc:body (spc:body normalized))))))
    ;; We sadly lack object equality for clos classes, so thus we test it this way
    ;; I really should make equality objects
    (is (typep (spc:lookup-record record :nonce) 'spc:reference)
        "ANF should have made an application inside the record become a reference")
    (is (eq (spc:field lookup) :nonce)
        "The field that is looked up should not change")
    (is (typep (spc:record lookup) 'spc:reference)
        "the record lookup is now over a reference instead of the record directly")))

