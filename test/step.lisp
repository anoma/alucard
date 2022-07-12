(in-package :alu-test)

(def-suite alucard.step
  :description "Testing the stepper")

(in-suite alucard.step)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(step.def:defun base ()
  (car (list (stack:get))))

(step.def:defun calling-base ()
  (if (listp (base))
      (base)))

(defmacro expansion-test (x)
  `(progn ,x))

(step.def:defun expansion-call ()
  (expansion-test (stack:get)))

(step.def:defun local-expansion-test (x)
  (flet ((expansion-test (x) (list x (stack:get))))
    (expansion-test x)))

(step.def:defun local-expansion-test-labels (x)
  (labels ((expansion-test (x) (list x (stack:get)))
           (faz (x) (expansion-test x)))
    (faz x)))

(step.def:defun lets-explore (x)
  (funcall (lambda (x) (+ x 5)) x))

(step.def:defun macro-let-test ()
  (macrolet ((lets-explore (x) `(progn ,x)))
    (lets-explore (stack:get))))

;; just checking all these run fine, with no issues
(step.def:defun alu-primitives (x y)
  (declare (ignore y))

  (prld:def ((prld:with-constraint (b2 b3)
               (prld:= x (prld:+ b2 b3))))
    (prld:with-constraint (b2 b3)
      (prld:= x (prld:+ b2 b3)))

    (alu:array 512 (int 1))

    (def ((bit-array (aluser::reshape x 512 :type (int 1))))
      (prld:get bit-array x))

    (prld:= (prld:+ (prld:exp x 3)
                    (prld:* 3 (prld:exp x 2))
                    (prld:* 2 x)
                    4)
            0)

    (prld:def ((bar (prld:to-array 36)))
      (prld:+ (prld:check 5 (int 32))
              (prld:get bar 0)))

    (def ((with-constraint (y z)
          (prld:= x (prld:+ (prld:* y 10) z))))
      z)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Doing Examples from the wiki
;; to see if our code walks properly
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(step.def:defun prince-of-clarity (w)
  "Take a cons of two lists and make a list of conses.
   Think of this function as being like a zipper."
  (do ((y (car w) (cdr y))
       (z (cdr w) (cdr z))
       (x '() (cons (cons (car y) (car z)) x)))
      ((null y) x)
    (when (null z)
      (cerror "Will self-pair extraneous items"
              "Mismatch - gleep!  ~S" y)
      (setq z y))))

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

(step.def:defun throwing-test ()
  (catch 'foo
    (format t "The inner catch returns ~s.~%"
            (catch 'foo
              (unwind-protect (throw 'foo :first-throw)
                (throw 'foo :second-throw))))
    (the keyword :outer-catch)))

(step.def:defun rest-test ()
  (list
   (equalp (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
           '(1 / 2 3 / / 2 0.5))
   (let* ((temp '(1 2 3)))
     (equalp 1
             (multiple-value-prog1
                 (values-list temp)
               (setq temp nil)
               (values-list temp))))
   (equalp '(3 4)
           (let ((x 3))
             (progv '(x) '(4)
               (list x (symbol-value 'x)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
             ((STACK:GET)
              (LIST X (STACK:GET))
              (EXPANSION-TEST X)
              (FLET ((EXPANSION-TEST (X) (LIST X (STACK:GET)))) (EXPANSION-TEST X)))))
   "Fletting beats macros!")
  (is
   (equalp (local-expansion-test-labels 3)
           '(3
             ((STACK:GET)
              (LIST X (STACK:GET))
              (EXPANSION-TEST X)
              (FAZ X)
              (LABELS ((EXPANSION-TEST (X) (LIST X (STACK:GET)))
                       (FAZ (X) (EXPANSION-TEST X)))
                (FAZ X)))))
   "Labels removes the recursive macro calls")
  (is (= (length (macro-let-test))
         4)
      "Macro expansion from a macrolet works as expected"))

(test instrumentation-does-not-interfere
  (is (equalp (king-of-confusion (cons (list 1 2 3)
                                       (list 4 6 7)))
              '((3 . 7) (2 . 6) (1 . 4)))
      "Instrumenting simply adds debugging information does not change semantics")
  (is (equalp (prince-of-clarity (cons (list 1 2 3)
                                       (list 4 6 7)))
              '((3 . 7) (2 . 6) (1 . 4)))
      "Instrumenting simply adds debugging information does not change semantics")
  (is (equalp (lets-explore 10) 15)
      "lambda should not loop forever")
  (is (equalp (stack:get) nil)
      "Cleanup should be had after all these calls!")
  (finishes (alu-primitives 3 5))
  (is (eql (throwing-test) :outer-catch))
  (is (every #'identity (rest-test))))
