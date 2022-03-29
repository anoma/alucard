(defpackage :alu-test
  (:use #:cl #:fiveam #:serapeum)
  (:local-nicknames (:util     :alu.utils)
                    (:spc      :alu.pass.linear-spec)
                    (:storage  :alu.storage)
                    (:anf      :alu.pass.anf)
                    (:closure  :alu.closure)
                    (:expand   :alu.pass.expanded)
                    (:relocate :alu.pass.relocation))
  (:export #:run-tests))

(in-package :alu-test)
