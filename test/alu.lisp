(in-package :alu-test)

(def-suite alucard
    :description "Tests the alucard package")

(in-suite alucard)

(defun clone-hash-table (hash)
  (alexandria:plist-hash-table
   (alexandria:hash-table-plist hash)))

(test deftype-works-as-expected
  (for-all ((name          (gen-string))
            (unroll-amount (gen-integer))
            (field         (gen-string)))
    (let ((alu::*type-table* (alu-test::clone-hash-table alu::*type-table*))
          (name              (intern name))
          (field             (intern field))
          (keyword           (intern name :keyword)))
      ;; we have to eval, as we are generating the values to go in the macro
      (eval `(alu:deftype (,name :unroll ,unroll-amount) ()
               (,field (int 64))))
       ;; did we add it to the table?
      (is (gethash keyword alu::*type-table*))
      ;; did we add the right amount of unrolling?
      (is (= unroll-amount
             (gethash :unroll
                      (fmt:options (gethash keyword alu::*type-table*)))))
      ;; did we add the field correctly
      (is (typep (gethash (util:symbol-to-keyword field)
                          (fmt:contents
                           (fmt:decl (gethash keyword alu::*type-table*))))
                 'fmt:application))
      ;; we add a global defn, check if it's there
      (is (fboundp name)))))
