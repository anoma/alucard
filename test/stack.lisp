(in-package :alu-test)

(def-suite alucard.stack
  :description "Testing stack semantics")

(in-suite alucard.stack)

;; I could be much more thorough in testing this, but it would just
;; involve me mimicking calls tediously.
 
(test dynamic-variable-respected
  (let ((current-stack (stack:get)))
    (stack:with-empty-stack ()
      (stack:push 3)
      (stack:push 10)
      (stack:push 20)
      (is (equalp
           (list 20 10 3)
           (stack:stack (stack:get)))
          "We should be pushing on a fresh stack"))
    (is (equalp (stack:get)
                current-stack)
        "Our operations should not have pushed globally")))

(test function-section
  (stack:with-section faz
    (stack:push '(+ 1 2 3))
    (stack:push '(+ b c d))
    (stack:push '(+ e f g))
    (is (equalp
         (stack:stack (stack:current-section (stack:get)))
         '((+ e f g)
           (+ b c d)
           (+ 1 2 3))))))
