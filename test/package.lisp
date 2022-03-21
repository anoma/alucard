(defpackage :alu-test
  (:use #:cl #:fiveam)
  (:local-nicknames (:util    :alu.utils)
                    (:spc     :alu.spec)
                    (:storage :alu.storage))
  (:export #:run-tests))

(in-package :alu-test)
