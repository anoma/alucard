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
