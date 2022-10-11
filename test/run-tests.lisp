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
   'alucard.stack
   'alucard.step
   'vampir
   'alucard))

(defun run-tests (&key (debug nil))
  (let ((swapped (storage:currently-swapped?)))
    ;; I'm sorry I destroy your custom table if it isn't the test
    ;; one... this is a bug, please FIX ME
    (swap-tables)
    (if debug
        (mapc #'debug! *all-tests*)
        (mapc #'run! *all-tests*))
    (unless swapped
      (storage:restore-tables))))

(defun profile-all ()
  (let* ((packages
           (list-all-packages))
         (alu-packages
           (remove-if-not (lambda (p)
                            (let ((search (search "ALU" (package-name p))))
                              (and search (= 0 search))))
                          packages))
         (without-aluser
             (remove-if (lambda (p)
                          (member (package-name p) '("aluser" "alu-test")
                                  :test #'equalp))
                        alu-packages)))
    (mapc (lambda (alu)
            (slynk-backend:profile-package alu t t))
          without-aluser)))

(defun unprofile-all ()
  (slynk-backend:unprofile-all))

(defun profiler-report ()
  (slynk-backend:profile-report))

(defun profiler-reset ()
  (slynk-backend:profile-reset))

#+ccl
(defun code-coverage ()
  (ccl:reset-incremental-coverage)
  (ccl:reset-coverage)

  (setq ccl:*compile-code-coverage* t)
  (asdf:compile-system :alu :force t)
  (asdf:compile-system :alu/test :force t)
  (swap-tables)

  (let ((coverage (make-hash-table)))
    ;; we want to note that some code loads before we can even test
    ;; it, so mark these under their own section
    (setf (gethash 'alucard.startup coverage)
          (ccl:get-incremental-coverage))
    (mapc (lambda (test)
            (run! test)
            (setf (gethash test coverage)
                  (ccl:get-incremental-coverage)))
          *all-tests*)
    (ccl:report-coverage #P"./html/report.html" :tags coverage))

  (setq ccl:*compile-code-coverage* nil)
  (asdf:compile-system :alu :force t)
  (asdf:compile-system :alu/test :force t))
