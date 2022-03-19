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
    (let ((storage:*types* (clone-hash-table storage:*types*))
          (name            (intern name))
          (field           (intern field))
          (keyword         (intern name :keyword)))
      ;; we have to eval, as we are generating the values to go in the macro
      (eval `(alu:deftype (,name :unroll ,unroll-amount) ()
               (,field (int 64))))
       ;; did we add it to the table?
      (is (storage:lookup-type keyword))
      ;; did we add the right amount of unrolling?
      (is (= unroll-amount
             (gethash :unroll
                      (fmt:options (storage:lookup-type keyword)))))
      ;; did we add the field correctly
      (is (typep (gethash (util:symbol-to-keyword field)
                          (fmt:contents (fmt:decl (storage:lookup-type keyword))))
                 'fmt:application))
      ;; we add a global defn, check if it's there
      (is (fboundp name)))))
