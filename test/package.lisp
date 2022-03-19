(defpackage :alu-test
  (:use #:cl #:fiveam)
  (:local-nicknames (:util    :alu.utils)
                    (:fmt     :alu.format)
                    (:storage :alu.storage))
  (:export #:run-tests))

(in-package :alu-test)
