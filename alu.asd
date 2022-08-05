(asdf:defsystem :alu
  :depends-on (:trivia :alexandria :sycamore :serapeum :closer-mop :command-line-arguments
                       (:version "asdf" "3.3.5")
                       :swank :slynk
                       :cl-environments
                       :verbose)
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
    :description "Internal utility functions"
    :components ((:file "package")
                 (:file "bit")
                 (:file "utils")))
   (:module reference
    :serial t
    :description "Defines a mutable pass by reference module"
    :depends-on ()
    :components ((:file "package")
                 (:file "ref")))
   (:module stack
    :serial t
    :description "Defines a simple stack data structure designed for stack traces"
    :depends-on ("reference")
    :components ((:file "package")
                 (:file "stack")))
   (:module closure
    :serial t
    :description "Closure data type and utilities"
    :depends-on ("util")
    :pathname #P"closure/"
    :components ((:file "package")
                 (:file "closure")
                 (:file "dependency")))
   (:module specification
    :serial t
    :description "Internal Alucard Specification"
    :depends-on  ("util" "closure" "stack")
    :pathname #P"spec/"
    :components ((:file "package")
                 (:file "data-traversal")
                 (:file "meta")
                 (:file "term")
                 (:file "type")
                 (:file "global")
                 (:file "storage")
                 (:file "type-op")
                 (:file "term-op")
                 (:file "emit")))
   (:module vampir
    :serial t
    :description "The Vampir Extraction Module"
    :components ((:file "package")
                 (:file "spec")
                 (:file "vampir")))
   (:module intermediate-representation
    :serial t
    :description "The various IR's of the Alucard compiler"
    :depends-on ("util" "specification")
    :pathname #p"intermediate/"
    :components ((:file "package")
                 (:file "primitive-global")
                 (:file "new-terms")
                 (:file "spec")))
   (:module typechecker
    :serial t
    :description "The type checker of the Alucard compiler"
    :depends-on ("util" "closure" "intermediate-representation" log)
    :components ((:file "package")
                 (:file "types")
                 (:file "size")
                 (:file "unifier")
                 (:file "typecheck")
                 (:file "intro")))
   (:module pass
    :serial t
    :depends-on ("intermediate-representation"
                 "closure"
                 "vampir"
                 "util"
                 typechecker
                 log)
    :description "Alucard Passes"
    :components ((:file "package")
                 (:file "evaluate-body")
                 (:file "expand")
                 (:file "relocation")
                 (:file "anf")
                 (:file "extract")
                 (:file "redundant-let")
                 (:file "pack")
                 (:file "pass")
                 (:file "array")
                 (:file "dependencies")
                 (:file "pipeline")))
   (:module stepper
    :serial t
    ;; we need symbols like `alu:def' in scope, in the future we
    ;; should remove the dependency on package and have a way of
    ;; extending our stepper with new primitives, and ways of stepping
    ;; through it. So we can instrument our specials in (:file alu)
    ;; instead.
    :depends-on (package stack)
    :description "Provides a syntax stepper that can step through the
    syntax of common lisp and allow instrumenting syntax such that a
    stack can be implemented."
    :components ((:file "package")
                 (:file "stepper")
                 (:file "define")))
   (:module log
    :serial t
    :depends-on (specification)
    :components ((:file "package")
                 (:file "log")))
   ;; only folder without a package
   (:module prelude
    :serial t
    :description "Alucard Prelude"
    :depends-on  (alu pass)
    :pathname #P"../alu/"
    :components ((:file "package")
                 (:file "prelude")))
   (:file "package"     :depends-on ("specification"))
   (:file "alu"         :depends-on (package stepper))
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
   (:file "typecheck")
   (:file "packing")
   (:file "pass")
   (:file "vampir")
   (:file "stack")
   (:file "step")
   (:file "run-tests"))
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))

;; Big TODO, figure out how to get good docs for Our project!
(asdf:defsystem :alu/documentation
  :depends-on (:fiveam
               :swank :slynk
               :staple
               :staple-server :asdf-package-system
               :staple-restructured-text)
  :description "Documenting alu"
  :pathname "test/"
  :serial t
  :components
  ()
  :perform (asdf:test-op (o s)
                         (uiop:symbol-call :alu-test :run-tests)))

;; #-clpm-client
(defun activate-project ()
  "Activates the projects clmpfile after `clpm-client' is loaded as the
default project. Note that the repl must be in the ALU directory for
the fileplath to work!"
  (uiop:symbol-call :clpm-client '#:activate-context (truename "clpmfile")
                    :activate-asdf-integration t))
