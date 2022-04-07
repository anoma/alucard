(asdf:defsystem :alu
  :depends-on (:trivia :alexandria :sycamore :serapeum :closer-mop)
  :version "0.0.0"
  :description "Powering Vamp-IR with the power of the original lineage"
  :author "Mariari"
  :license "MIT"
  :pathname "src/"
  :build-pathname "../build/alu.image"
  :entry-point "alu::main"
  :build-operation ;; #+(or ecl ccl)
  "program-op"
  ;; #-(or ecl ccl) "image-op"
  :components
  ((:module util
    :serial t
    :description "Internal Utility Functions"
    :components ((:file "package")
                 (:file "utils")))
   (:module specification
    :serial t
    :description "Internal Alucard Specification"
    :depends-on  ("util")
    :pathname #P"spec/"
    :components ((:file "package")
                 (:file "term")
                 (:file "type")
                 (:file "global")
                 (:file "storage")))
   (:module closure
    :serial t
    :description "Closure data type and utilities"
    :depends-on ("util")
    :pathname #P"closure/"
    :components ((:file "package")
                 (:file "closure")))
   (:module vampir
    :serial t
    :description "The Vampir Extraction Module"
    :components ((:file "package")
                 (:file "spec")
                 (:file "vampir")))
   (:module pass
    :serial t
    :depends-on ("specification" "closure" "vampir" "util")
    :description "Alucard Passes"
    :components ((:file "package")
                 (:file "linear-term")
                 (:file "primitive-global")
                 (:file "expand")
                 (:file "relocation")
                 (:file "anf")
                 (:file "extract")
                 (:file "pass")))
   ;; only folder without a package
   (:module prelude
    :serial t
    :description "Alucard Prelude"
    :depends-on  ("alu")
    :pathname #P"../alu/"
    :components ((:file "prelude")))
   (:file "package" :depends-on ("specification"))
   (:file "alu"     :depends-on ("package"))
   (:file "../app/main" :depends-on ("alu" "prelude")))
  :in-order-to ((asdf:test-op (asdf:test-op :alu/test))))

(asdf:defsystem :alu/test
  :depends-on (:alu :fiveam)
  :description "Testing alu"
  :pathname "test/"
  :serial t
  :components
  ((:file "package")
   ;; we setup our table with global-examples
   (:file "global-examples")
   (:file "alu")
   (:file "spec")
   (:file "anf")
   (:file "expand")
   (:file "relocation")
   (:file "pass")
   (:file "vampir")
   (:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))
