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

;; I'm being lazy I shouldn't check the printed version, but rather
;; make equality or a quick way to check equality of my objects

(defparameter *inner-output*
"((:IN BAZ
  (/ C D)
  (* A B (/ C D)))
 (:IN FAZ
  (/ C D)
  (* A B (/ C D))))")

(defparameter *outer-output*
"((:IN FAZ
  (/ C D)
  (* A B (/ C D))))")

(defparameter *outer-cdr*
"((:IN FAZ
  (* A B (/ C D))))")


;; Being lazy about testing and using streams.
(test removing-works-as-expected
  (let ((*print-pretty* t)
        (output-stream-baz        (make-string-output-stream))
        (output-stream-faz        (make-string-output-stream))
        (output-stream-manual     (make-string-output-stream))
        (output-stream-manual-cdr (make-string-output-stream)))
    (stack:with-empty-stack ()
      (stack:with-section faz
        (stack:push '(* a b (/ c d)))
        (stack:push '(/ c d))
        (stack:with-section baz
          (stack:push '(* a b (/ c d)))
          (stack:push '(/ c d))
          ;; dump the current stack to the output stream
          (print-object (stack:get) output-stream-baz)
          ;; we cdr 3 times to pop the baz out of it!, thus we should
          ;; be the same as faz. and the printing should also work
          ;; exactly the same.
          (let ((remove-baz-by-hand
                  (stack:cdr (stack:cdr
                              (stack:cdr (stack:get))))))
            (print-object remove-baz-by-hand output-stream-manual)
            ;; this should cdr off an element off faz
            (print-object (stack:cdr remove-baz-by-hand)
                          output-stream-manual-cdr)))
        ;; weird how the formatting changes between calls
        (print-object (stack:get) output-stream-faz)
        (let ((output-faz (get-output-stream-string output-stream-faz)))
          (is (string= *inner-output*
                       (get-output-stream-string output-stream-baz))
              "Scope should be respected and we should be double
              nested")
          (is (string= output-faz
                       *outer-output*)
              "formatting for this should be precise and nice, with
              the outer section gone outside of the scope")
          (is (string= (get-output-stream-string output-stream-manual)
                       output-faz)
              "Removing the two elements in baz plus the baz function
              marker should be the same as leaving the section")
          (is (string= (get-output-stream-string output-stream-manual-cdr)
                       *outer-cdr*)
              "Cdring a nil section should lead to cdring the next
              section on the stack"))))))
