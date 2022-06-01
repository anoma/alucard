(in-package :alu-test)

(defparameter *all-tests*
  (list
   'alucard.format
   'alucard.pass.anf
   'alucard.expand
   'alucard.relocation
   'alucard.dependencies
   'alucard.pass
   'alucard.typecheck
   'alucard.evaluate-body
   'alucard.packing
   'vampir
   'alucard))

(defun run-tests ()
  (let ((swapped (storage:currently-swapped?)))
    ;; I'm sorry I destroy your custom table if it isn't the test
    ;; one... this is a bug, please FIX ME
    (swap-tables)
    (mapc #'run! *all-tests*)
    (unless swapped
      (storage:restore-tables))))


#+ccl
(defun code-coverage ()
  (ccl:reset-incremental-coverage)
  (ccl:reset-coverage)
  (setq ccl:*compile-code-coverage* t)
  (asdf:compile-system :alu :force t)
  (asdf:compile-system :alu/test :force t)
  (swap-tables)
  (ccl:report-coverage #P"./html/report.html" :tags
                       (loop with coverage = (make-hash-table)
                             for test in *all-tests*
                             do (run! test)
                             do (setf (gethash test coverage)
                                      (ccl:get-incremental-coverage))
                             finally (return coverage)))
  (setq ccl:*compile-code-coverage* nil)
  (asdf:compile-system :alu :force t)
  (asdf:compile-system :alu/test :force t))
