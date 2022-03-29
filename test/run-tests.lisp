(in-package :alu-test)

(defun run-tests ()
  (let ((swapped (storage:currently-swapped?)))
    ;; I'm sorry I destroy your custom table if it isn't the test
    ;; one... this is a bug, please FIX ME
    (swap-tables)
    (run! 'vampir)
    (run! 'alucard.format)
    (run! 'alucard.pass.anf)
    (run! 'alucard.expand)
    (run! 'alucard.relocation)
    (run! 'alucard)
    (unless swapped
      (storage:restore-tables))))
