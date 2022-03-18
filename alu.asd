(asdf:defsystem :alu
  :depends-on (:trivia :alexandria)
  :version "0.0.0"
  :description "Powering Vamp-IR with the power of the original lineage"
  :author "Mariari"
  :license "MIT"
  :pathname "src/"
  :serial t
  :components
  ((:file "package")
   (:file "utils")
   (:file "vampir")
   (:file "alu/term")
   (:file "alu/type")
   (:file "alu/function")
   (:file "alu"))
  :in-order-to ((asdf:test-op (asdf:test-op :alu/test))))

(asdf:defsystem :alu/test
  :depends-on (:alu :fiveam)
  :description "Testing alu"
  :pathname "test/"
  :serial t
  :components
  ((:file "package")
   (:file "alu")
   (:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))
