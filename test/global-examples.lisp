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

(alu:defcircuit arg-test ((public  root (bytes 64))
                          (private sig  int)
                          (private utx nested)
                          (output nested))
  (arg-circuit-input 3 (plane utx)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restoring the original table if we didn't start in the test table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless *swapped*
  (storage:restore-tables))
