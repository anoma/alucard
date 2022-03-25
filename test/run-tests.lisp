(in-package :alu-test)

(defun run-tests ()
  (run! 'vampir)
  (run! 'alucard.format)
  (run! 'alucard.pass.anf)
  (run! 'alucard.expand)
  (run! 'alucard))
