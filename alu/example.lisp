(in-package :aluser)

;;
(deftype utxo ()
  (owner  (bytes 128))
  (amount (int   64))
  (nonce  (int   64)))

;; let us not support recursive data types at first
(deftype (merkle-branch :unroll 10) ()
  (hash  (bytes 64))
  (left  merkle-branch)
  (right merkle-branch))

(deftype point ()
  (x int)
  (y int))

(deftype nested ()
  (plane point)
  (time  point))

(defcircuit constrain ((public nest nested)
                       (output bool))
  (def ((plane (plane nest))
        (time  (time nest)))
    (= (* (x plane)
          (y plane))
       (* (x time)
          (y time)))))

(defcircuit constrain-2 ((public nest nested)
                         (output void))
  (flet ((formula (point)
           (* (x point)
              (y point))))
    (= (formula (plane nest))
       (formula (time nest)))))

(defcircuit constrain-3 ((public nest nested)
                         (output void))
  (reduce #'=
          (mapcar (lambda (point)
                    (* (x point)
                       (y point)))
                  (list (plane nest) (time nest)))))

(defcircuit range-check ((private input int)
                         (output void))
  (with-constraint (b1 b0)
    (with-constraint (b2 b3)
      (= input (+ b1 b2)))))

(defcircuit range-check-1 ((private input int)
                           (output void))
  (def ((with-constraint (b1 b0)
          (= input (+ b1 b0))))
    b1))

;; (alu::let-refs (b0 b1)
;;                (alu::with-constraint-standalone (b0 b1) (= input (+ b1 b0)))
;;                (list))

(def ((a 3)
      (b 5))
  a)

(defcircuit root-test ((public x int))
  (= (+ (exp x 3)
        (* 3 (exp x 2))
        (* 2 x)
        4)
     0))

;; Note from Chris, something like
;; (defun poly-check (x int) (= (+ (exp x 3) (mul 3 (exp x 2)) (mul 2 x) 4) 0)
;; is wanted, so we can skimp on the `public` and make more short hands thereof

;; Discussion
;; maybe : Add casting functions to add more constraints into a circuit input
;; want  : explicit defconstraint macro that adds constraints to values (monotonically increasing information)

(defcircuit poly-check ((public x int)
                        (output bool))
  (= (+ (exp x 3)
        (* 3 (exp x 2))
        (* 2 x)
        4)
     0))

;; (defcircuit constraint ((public const (bytes 64))
;;                         (output int))
;;   (def ((a (= (+ const 53) 0))
;;         (b (range 32 a)))
;;     (and (range 64 a)
;;          b)))

;; Something like this happens quite often when writing big hash
;; function cirucits... how do we organize information properly

;; This is rounded, same logic, different constants.
;; A function apply it 20 functions, in variations
;; 1. constant known to the entire world
;; every round it's a little different
;; (defun orgnaize-circuit-infomration-nicely (data)
;;   (fold #'list
;;         (concatenate-in-tree
;;          (shuffle
;;           (list data-bytes-bits-whatever)))))

;; Nice idea
;; Generate out diagrams arrows between data types
;; How things are related


(defcircuit poly ((public  root (bytes 64))
                  (private sig  (bytes 64))
                  (private utxo utxo)
                  ;; should consider doing the unrolling here rather than
                  (private merk merkle-branch)
                  ;; should we have return type information be here
                  (output int))
  ;; (fold-tree root merk)
  ;; (equal (owner utxo) "test")
  (= (owner utxo) 5))

(entry-point constrain-3)
