;; 1. _Priority_
;;   - Think of the data layout
;;   - The rest will follow from that
(in-package :alu)

;; Important design questions
;;
;; Should I make it backwards compatible with CL? If we can do this,
;; we can freely mix circuit functions and non circuit functions
;; without impunity.
;;
;; This could be done by doing a haskell style solution, of having the
;; functions made by `defcircuit' be a lifted type that doesn't
;; interact well with non circuits, but can compose nicely with others
;; somehow.
;;
;; If not, then I'll need to make a shimmy (lisp <logic-here>) to
;; generate out code. Seems unwise to do this, but may be needed
;; depending how I architect this.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Storage Mechanisms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *type-table* (make-hash-table :test #'eq)
  "Serves as the table which stores all the circuit types that are
relevant to the system")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression ADT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cl:deftype alu-expression ()
  "The Alu expression type"
  `(or ;; we may want to remove this, if we go for a more effectful
       ;; route rather than just binding naively on what we find.
       ;;
       ;; We can do this by pushing to some list, then collecting the
       ;; constraints at the end of the expression.
       list
       ;; from alu/term
       alu-term))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun symbol-to-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Level Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defprimitive (name body)
  "defines a primitive type"
  (let ((keyword (intern (symbol-name name) :keyword)))
    ;; always set it to be safe
    (setf (gethash keyword *type-table*)
          body)))

(defmacro def (bind-values body)
  "defines the values in the presence of the body"
  `(let
       ;; bind the values at the CL level, so we can just reference it
       ,(mapcar (lambda (bind-pair)
                  (list (car bind-pair)
                        `(make-reference
                          :name (symbol-to-keyword ',(car bind-pair)))))
         bind-values)
     ;; Declare the values as ignoreable
     ;; Should we keep the warning!?
     (declare (ignorable ,@(mapcar #'car bind-values)))
     ;; Generate out the Alucard level binding
     ,(reduce (lambda (bind-pair let-buildup)
                `(make-let
                  :var (symbol-to-keyword ',(car bind-pair))
                  :val ,(cadr bind-pair)
                  :body ,let-buildup))
              bind-values
              :from-end t
              :initial-value body)))

;; Place holders for now
(defmacro deftype (name &body type-declarations)
  ``(,',name ,@',type-declarations))

;; Place holders for now
(defmacro defcircuit (name arguments &body body)
  ``(,',name ,@',arguments ,@',body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
(deftype utxo
  (owner  (bytes 128))
  (amount (int   64))
  (nonce  (int   64)))

;; let us not support recursive data types at first
(deftype (merkel-branch :unroll 10)
  (hash  (bytes 64))
  (left  merkel-branch)
  (right merkel-branch))

(defcircuit poly ((public  root (bytes 64))
                  (private sig  (bytes 64))
                  (private utxo utxo)
                  ;; should consider doing the unrolling here rather than
                  (private merk merkel-branch))
  (fold-tree root merk)
  (equal (owner utxo) "test"))

(def ((a 3)
      (b 5))
  a)
