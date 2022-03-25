(in-package :alu-test)

(def-suite alucard.expand
  :description "Test the expanded argument functionality")

(in-suite alucard.expand)

(defun clone (hash)
  (alexandria:plist-hash-table
   (alexandria:hash-table-plist hash)))

;; We sadly can't let over lambda, as the test gets ran later ☹
(test expansion-of-flat-arguments-work
  (let ((storage:*types*     (clone storage:*types*))
        (storage:*functions* (clone storage:*functions*)))
    (alu:deftype utx ()
      (owner  (bytes 128))
      (amount (int   64))
      (nonce  (int   64)))

    ;; recursive example
    (alu:deftype tree ()
      (element int)
      (left    tree)
      (nonce   tree))

    (alu:defcircuit arg-test ((public  root (bytes 64))
                              (private sig  int)
                              (private utx utx)
                              (output int))
      arg-test)
    ;; (utx :owner 3 :amount 5 :nonce 10)
    (let ((expanded-storage
            (expand:full-arguments-from-storage :arg-test))
          (arguments
            (spc:arguments (storage:lookup-function :arg-test))))

      ;; Tests begin here
      (is (eq (car arguments) (car expanded-storage))
          "Bytes is a primitive type and should not be changed")
      (is (eq (cadr arguments) (cadr expanded-storage))
          "int is a primtiive type and should not be changed")
      (is (not (eq (caddr arguments) (caddr expanded-storage)))
          "utx is not a primitve type and should be expanded properly")
      (is (typep (caddr expanded-storage) 'expand:expand)
          (format nil "UTX should turn into an expand type"))
      (is (eq :utx (expand:original (caddr expanded-storage)))
          "we preserve the name of the utx")
      (is (= 3 (length (expand:expanded (caddr expanded-storage))))
          "The arguments should be expanded to 3 argument wide"))))
