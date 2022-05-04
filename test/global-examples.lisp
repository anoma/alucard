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


(defcircuit constrain ((public nest nested)
                       (output void))
  (def ((plane (plane nest))
        (time  (time nest)))
    (prld:= (prld:* (x plane)
                    (y plane))
            (prld:+ (x time)
                    (y time)))))

(defcircuit use-constrain ((public foo nested)
                           (output void))
  (constrain foo))

(defcircuit arg-test-exp ((public  root (bytes 64))
                          (private sig  int)
                          (private utx utx)
                          (output int))
  3)


(defcircuit arg-foo ((public  root int)
                     (private sig  int)
                     (private foo  int)
                     (output nested))
      3)

(defcircuit arg-circuit-input ((public  root int)
                               (private sig  nested)
                               (output nested))
  3)

(defcircuit record-test ((public  root (bytes 64))
                         (private sig  int)
                         (private utx nested)
                         (output nested))
  (arg-circuit-input 3 (plane utx)))

(defcircuit record-test-mult ((public  root (bytes 64))
                              (private sig  int)
                              (private utx nested)
                              (output nested))
  (def ((circ (arg-circuit-input 3 (plane utx))))
    (plane circ)))

(defcircuit record-ret ((public x int)
                        (public y int))
  (nested :plane (point :x x :y y)
          :time  (point :x x :y y)))

(defcircuit poly-check ((public x int)
                        (output bool))
  (prld:= (prld:+ (prld:exp x 3)
                  (prld:* 3 (prld:exp x 2))
                  (prld:* 2 x)
                  4)
     0))

;; Manual adding to the storage
;; my god just add storage abstraction, as wew!
(storage:add-function
 :manual-constraint
 (spc:make-circuit
  :return-type (spc:make-type-reference :name :bool)
  :name :manual-constraint
  :arguments nil
  :body
  '(emit:instruction
    (spc:make-bind-constraint
     :var (list :a :b :c)
     :value
     (list
      (spc:make-let :var :fi
                    :val (spc:make-application
                          :function (spc:make-reference :name :record-test-mult)
                          :arguments
                          (list (spc:make-reference :name :hi)
                           (spc:make-reference :name :hi)
                           (spc:make-reference :name :hi))))
      (spc:make-application
       :function (spc:make-reference :name :=)
       :arguments
       (list
        (spc:make-application
         :function (spc:make-reference :name :+)
         :arguments
         (list (spc:make-reference :name :a)
               (spc:make-reference :name :b)
               (spc:make-reference :name :fi)
               (spc:make-record-lookup
                :record (spc:make-record :name :utxo
                                         :owner 3
                                         :amount 5
                                         :nonce (spc:make-reference :name :hi))
                :field :nonce)))
        (spc:make-reference :name :bob))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Restoring the original table if we didn't start in the test table
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless *swapped*
  (storage:restore-tables))
