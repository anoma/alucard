(in-package :alu-test)

(def-suite alucard.format
    :description "Tests the alucard storage format")

(in-suite alucard.format)

(test record-creation-and-lookup-works
  (for-all ((name  (gen-string))
            (value (gen-integer)))
    (let ((keyword (intern name :keyword)))
      (is
       (equal (spc:lookup-record (spc:make-record :name :example keyword value)
                                 keyword)
              value)))))

(test syntax-to-refernece-format
  (let ((applied (spc:to-type-reference-format '(int 64)))
        (nested  (spc:to-type-reference-format '(int (int 64)))))
    (is (eq :INT
            (spc:name (spc:func applied))))
    (is (= 64
           (car (spc:arguments applied))))
    (is (eq :INT
            (spc:name (spc:func (car (spc:arguments nested))))))))


;; Note for later, we can have exuastion
;; (match-of term (make-application :function :hi )
;;   ((application func) func))

;; (typecase-of term 3
;;   (number
;;    2)
;;   (application
;;    2)
;;   (otherwise
;;    2))
