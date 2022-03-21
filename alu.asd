(asdf:defsystem :alu
  :depends-on (:trivia :alexandria :sycamore :serapeum)
  :version "0.0.0"
  :description "Powering Vamp-IR with the power of the original lineage"
  :author "Mariari"
  :license "MIT"
  :pathname "src/"
  :components
  ((:module specification
    :serial t
    :description "Internal Alucard Specification"
    :pathname #P"spec/"
    :components ((:file "package")
                 (:file "utils")
                 (:file "term")
                 (:file "type")
                 (:file "global")
                 (:file "storage")))
   (:module pass
    :serial t
    :depends-on ("specification")
    :description "Alucard Passes"
    :components ((:file "package")))
   (:file "package" :depends-on ("specification"))
   (:file "alu"     :depends-on ("package"))
   (:file "vampir"  :depends-on ("package")))
  :in-order-to ((asdf:test-op (asdf:test-op :alu/test))))

(asdf:defsystem :alu/test
  :depends-on (:alu :fiveam)
  :description "Testing alu"
  :pathname "test/"
  :serial t
  :components
  ((:file "package")
   (:file "alu")
   (:file "spec")
   (:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))
