(in-package :alu-test)

(def-suite alucard.expand
  :description "Test the expanded argument functionality")

(in-suite alucard.expand)

;; We sadly can't let over lambda, as the test gets ran later ☹
(test expansion-of-flat-arguments-work
  (let ((expanded-storage
          (expand:full-arguments-from-storage :arg-test-exp))
        (arguments
          (ir:arguments (storage:lookup-function :arg-test-exp))))

    ;; Tests begin here
    (is (eq (car arguments) (car expanded-storage))
        "Bytes is a primitive type and should not be changed")
    (is (eq (cadr arguments) (cadr expanded-storage))
        "int is a primtiive type and should not be changed")
    (is (not (eq (caddr arguments) (caddr expanded-storage)))
        "utx is not a primitve type and should be expanded properly")
    (is (typep (caddr expanded-storage) 'expand:expand)
        (format nil "UTX should turn into an expand type"))
    (is (alexandria:starts-with-subseq
         "UTX"
         (symbol-name (expand:original (caddr expanded-storage))))
        "we preserve the name of the utx")
    (is (= 3 (length (expand:expanded (caddr expanded-storage))))
        "The arguments should be expanded to 3 argument wide")))

(test expansion-of-output
  (let ((expanded-nested
          `((:plane . ,(expand:full-type-reference*
                        (ir:make-type-reference :name :point)))
            (:time  . ,(expand:full-type-reference*
                        (ir:make-type-reference :name :point)))))
        (nested-lookup
          (expand:full-type-reference* (ir:make-type-reference :name :nested)))
        (arg-test-output-expansion
          (expand:full-return-values :arg-foo)))

    ;; Tests begin here
    (is (equalp expanded-nested nested-lookup)
        "Expansion of nested should expand twice")
    (is (equalp nested-lookup arg-test-output-expansion)
        "Expanding the output should have the same effect")))
