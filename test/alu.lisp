(in-package :alu-test)

(def-suite alucard
    :description "Tests the alucard package")

(in-suite alucard)

(test record-creation-and-lookup-works
  (for-all ((name  (gen-string))
            (value (gen-integer)))
    (let ((keyword (intern name :keyword)))
      (is
       (equal (alu::lookup-record (alu::make-record :name :example keyword value)
                                  keyword)
              value)))))
