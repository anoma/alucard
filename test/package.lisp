(defpackage :alu-test
  (:use #:cl #:fiveam #:serapeum)
  (:local-nicknames (:util     :alu.utils)
                    (:spc      :alu.pass.spec)
                    (:storage  :alu.storage)
                    (:anf      :alu.pass.anf)
                    (:closure  :alu.closure)
                    (:expand   :alu.pass.expanded)
                    (:relocate :alu.pass.relocation)
                    (:vamp     :alu.vampir)
                    (:vspc     :alu.vampir.spec))
  (:export #:run-tests))

(in-package :alu-test)
