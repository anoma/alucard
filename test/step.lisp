(in-package :alu-test)

(def-suite alucard.step
  :description "Testing the stepper")

(in-suite alucard.step)

(step.def:defun base ()
  (car (list (stack:get))))

(step.def:defun calling-base ()
  (if (listp (base))
      (base)))

(defmacro expansion-test (x)
  `(progn ,x))

(step.def:defun expansion-call ()
  (expansion-test (stack:get)))

(step.def:defun king-of-confusion (w)
  "Take a cons of two lists and make a list of conses.
    Think of this function as being like a zipper."
  (prog (x y z)                         ;Initialize x, y, z to NIL
     (setq y (car w) z (cdr w))
   loop
     (cond ((null y) (return x))
           ((null z) (go err)))
   rejoin
     (setq x (cons (cons (car y) (car z)) x))
     (setq y (cdr y) z (cdr z))
     (go loop)
   err
     (cerror "Will self-pair extraneous items"
             "Mismatch - gleep!  ~S" y)
     (setq z y)
     (go rejoin)))

(step.def:defun local-expansion-test (x)
  (flet ((expansion-test (x) (list x (stack:get))))
    (expansion-test x)))

(step.def:defun lets-explore (x)
  (funcall (lambda (x) (+ x 5)) x))

(test nesting-respected
  (is (>= (length (calling-base)) 3)
      "calling a traced function should have the parents call put in there as well!"))

(test macro-expected
  (is (equalp (expansion-call)
              '((STACK:GET) (PROGN (STACK:GET)) (EXPANSION-TEST (STACK:GET))))
      "The macro should be recorded wholesale along with it's expansion")
  (is
   (equalp (local-expansion-test 5)
           '(5
             ((STACK:GET) (LIST X (STACK:GET)) (EXPANSION-TEST X)
              (FLET ((EXPANSION-TEST (X) (LIST X (STACK:GET)))) (EXPANSION-TEST X)))))
   "Fletting beats macros!"))

(test instrumentation-does-not-interfere
  (is (equalp (king-of-confusion (cons (list 1 2 3)
                                       (list 4 6 7)))
              '((3 . 7) (2 . 6) (1 . 4)))
      "Instrumenting simply adds debugging information does not change semantics")
  (is (equalp (lets-explore 10) 15)
      "lambda should not loop forever")
  (is (equalp (stack:get) nil)
      "Cleanup should be had after all these calls!"))
