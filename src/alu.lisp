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
                          :name (util:symbol-to-keyword ',(car bind-pair)))))
         bind-values)
     ;; Declare the values as ignoreable
     ;; Should we keep the warning!?
     (declare (ignorable ,@(mapcar #'car bind-values)))
     ;; Generate out the Alucard level binding
     ,(reduce (lambda (bind-pair let-buildup)
                `(make-let
                  :var (util:symbol-to-keyword ',(car bind-pair))
                  :val ,(cadr bind-pair)
                  :body ,let-buildup))
              bind-values
              :from-end t
              :initial-value body)))

(defmacro deftype (name-and-options generics &body type-declarations)
  (let* ((fields (mapcar #'car type-declarations))
         (name (if (listp name-and-options)
                  (car name-and-options)
                  name-and-options))
         (options (if (listp name-and-options)
                  (cdr name-and-options)
                  nil))
         (key-name (util:symbol-to-keyword name)))
    `(progn
       ;; Register the struct in the type table, so we will always
       ;; know about it!
       (setf (gethash ,key-name alu::*type-table*)
             ;; make the top level type declaration
             (make-type-declaration
              :name ,key-name
              :generics ,generics
              :options (alexandria:plist-hash-table ,(cons 'list options))
              ;; this is where the assumption about structs come in!
              :decl
              (make-record-declaration
               ;; mapcan is the >>= for lists in Haskell
               ,@(mapcan (lambda (declaration-info)
                           (list
                            (util:symbol-to-keyword (car declaration-info))
                            ;; we want to transform the declaration
                            ;; into a lookup of the table, and if an
                            ;; application, the following
                            ;;
                            ;; 1. (utxo int)
                            ;;    -> (utxo (type-reference :int))
                            ;; 2. (utxo (int 64))
                            ;;    -> (utxo (application (type-refernece :int) 64))
                            `(to-type-reference-format ',(cadr declaration-info))))
                         type-declarations))))

       ;; Create the function that we can now call, to create an instance
       (defun ,name (&key ,@fields)
         (make-record :name ,key-name
                      ;; fill in the other slots
                      ,@(mapcan (lambda (field)
                                  (list (util:symbol-to-keyword field) field))
                                fields)))
       ;; Return the Symbol itself!
       ',name)))

(defun to-type-reference-format (term)
  "Given an application or a symbol, transform it to the correct type
storage format. So for example

1. int      -> (make-type-reference :name :int)
2. (int 64) -> (make-application :name (make-type-reference :name :int)
                                 :arguments (list 64))"
  ;; can either be a list number or atom
  (cond ((listp term)
         (let ((type-ref (mapcar #'to-type-reference-format term)))
           (make-application :name (car type-ref) :arguments (cdr type-ref))))
        ((numberp term) term)
        (t              (make-type-reference :name (util:symbol-to-keyword term)))))

;; Place holders for now
(defmacro defcircuit (name arguments &body body)
  ``(,',name ,@',arguments ,@',body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
(deftype utxo ()
  (owner  (bytes 128))
  (amount (int   64))
  (nonce  (int   64)))

;; let us not support recursive data types at first
(deftype (merkel-branch :unroll 10) ()
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
