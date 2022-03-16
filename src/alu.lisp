;; 1. _Priority_
;;   - Think of the data layout
;;   - The rest will follow from that
(defpackage #:alu
  (:documentation "provides the Alucard VAMP-IR DSL")
  (:shadow #:deftype)
  (:use #:common-lisp)
  (:export :deftype
   :defcircuit))

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
;; Module Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (cl:deftype primitives)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Default Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; High Level Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defprimitive (name body)
  "defines a primitive type"
  (let ((keyword (intern (symbol-name name) :keyword)))
    ;; always set it to be safe
    (setf (gethash keyword *type-table*)
          body)))


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
