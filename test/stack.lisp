(in-package :alu-test)

(def-suite alucard.stack
  :description "Testing stack semantics")

(in-suite alucard.stack)

(test dynamic-variable-respected
  (let ((current-stack (stack:get)))
    (stack:with-empty-stack ()
      (stack:push 3)
      (stack:push 10)
      (stack:push 20)
      (is (equalp
           (list 20 10 3)
           (stack:get))
          "We should be pushing on a fresh stack"))
    (is (equalp (stack:get)
                current-stack)
        "Our operations should not have pushed globally")))
