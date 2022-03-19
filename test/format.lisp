(in-package :alu-test)

(def-suite alucard.format
    :description "Tests the alucard storage format")

(in-suite alucard.format)

(test record-creation-and-lookup-works
  (for-all ((name  (gen-string))
            (value (gen-integer)))
    (let ((keyword (intern name :keyword)))
      (is
       (equal (fmt:lookup-record (fmt:make-record :name :example keyword value)
                                 keyword)
              value)))))

(test syntax-to-refernece-format
  (let ((applied (fmt:to-type-reference-format '(int 64)))
        (nested  (fmt:to-type-reference-format '(int (int 64)))))
    (is (eq :INT
            (fmt:name (fmt:func applied))))
    (is (= 64
           (car (fmt:arguments applied))))
    (is (eq :INT
            (fmt:name (fmt:func (car (fmt:arguments nested))))))))

