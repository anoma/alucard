(defpackage :alu-test
  (:use #:cl #:fiveam)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage)
                    (:anf     :alu.pass.anf))
  (:export #:run-tests))

(in-package :alu-test)
