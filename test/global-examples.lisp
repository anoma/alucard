(in-package :alu-test)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Table and Setup Swap
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clone (hash)
  (alexandria:plist-hash-table
   (alexandria:hash-table-plist hash)))

(defparameter *test-functions* (clone storage:*functions*))
(defparameter *test-types* (clone storage:*types*))

(defparameter *swapped* (storage:currently-swapped?))

(storage:swap-tables *test-functions*
                     *test-types*)

(defun swap-tables ()
  (storage:swap-tables *test-functions*
                       *test-types*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Examples in the test table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(alu:deftype tree ()
  (element int)
  (left    tree)
  (nonce   tree))

(alu:deftype utx ()
  (owner  (bytes 128))
  (amount (int   64))
  (nonce  (int   64)))

(alu:deftype point ()
  (x int)
  (y int))

(alu:deftype nested ()
  (plane point)
  (time  point))


(alu:defcircuit constrain ((public nest nested)
                           (output void))
  (alu:def ((plane (plane nest))
            (time  (time nest)))
    (alu::= (alu::* (x plane)
                    (y plane))
            (alu::+ (x time)
                    (y time)))))

(alu:defcircuit use-constrain ((public foo nested)
                               (output void))
  (constrain foo))

(alu:defcircuit arg-test-exp ((public  root (bytes 64))
                              (private sig  int)
                              (private utx utx)
                              (output int))
      3)


(alu:defcircuit arg-foo ((public  root int)
                         (private sig  int)
                         (private foo  int)
                         (output nested))
      3)

(alu:defcircuit arg-circuit-input ((public  root int)
                                   (private sig  nested)
                                   (output nested))
  3)

(alu:defcircuit record-test ((public  root (bytes 64))
                             (private sig  int)
                             (private utx nested)
                             (output nested))
  (arg-circuit-input 3 (plane utx)))

(alu:defcircuit record-test-mult ((public  root (bytes 64))
                                  (private sig  int)
                                  (private utx nested)
                                  (output nested))
  (alu:def ((circ (arg-circuit-input 3 (plane utx))))
    (plane circ)))

(alu:defcircuit record-ret ((public x int)
                            (public y int))
  (nested :plane (point :x x :y y)
          :time  (point :x x :y y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restoring the original table if we didn't start in the test table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless *swapped*
  (storage:restore-tables))
