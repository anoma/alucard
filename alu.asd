(asdf:defsystem :alu
  :depends-on (:trivia :alexandria :sycamore :serapeum :closer-mop :command-line-arguments)
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
                 (:file "bit")
                 (:file "utils")))
   (:module closure
    :serial t
    :description "Closure data type and utilities"
    :depends-on ("util")
    :pathname #P"closure/"
    :components ((:file "package")
                 (:file "closure")))
   (:module specification
    :serial t
    :description "Internal Alucard Specification"
    :depends-on  ("util" "closure")
    :pathname #P"spec/"
    :components ((:file "package")
                 (:file "term")
                 (:file "type")
                 (:file "global")
                 (:file "storage")
                 (:file "emit")))
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
                 (:file "evaluate-body")
                 (:file "primitive-global")
                 (:file "expand")
                 (:file "relocation")
                 (:file "anf")
                 (:file "extract")
                 (:file "pass")
                 (:file "dependencies")
                 (:file "pipeline")))
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
   (:file "evaluate-body")
   (:file "dependencies")
   (:file "pass")
   (:file "vampir")
   (:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))

;; #-clpm-client
(defun activate-project ()
  "Activates the projects clmpfile after `clpm-client' is loaded as the
default project. Note that the repl must be in the ALU directory for
the fileplath to work!"
  (uiop:symbol-call :clpm-client '#:activate-context (truename "clpmfile")
                    :activate-asdf-integration t))
