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
;; High Level Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defprimitive (name)
  "defines a primitive type"
  (let ((keyword (util:symbol-to-keyword name)))
    ;; always set it to be safe
    `(storage:add-type ,keyword (spc:make-primitive :name ,keyword))))

(defmacro def (bind-values body)
  "defines the values in the presence of the body"
  ;; bind the values at the CL level, so we can just reference it
  `(let-refs ,(mapcar #'car bind-values)
     ;; Generate out the Alucard level binding
     ,(reduce (lambda (bind-pair let-buildup)
                `(spc:make-let
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
       (storage:add-type
        ,key-name
        ;; make the top level type declaration
        (spc:make-type-declaration
         :name ,key-name
         :generics ,generics
         :options (util:sycamore-plist-symbol-map (list ,@options))
         ;; this is where the assumption about structs come in!
         :decl
         (spc:make-record-declaration
          ;; mapcan is the >>= for lists in Haskell
          ,@(mapcan (lambda (declaration-info)
                      ;; we want to transform the declaration
                      ;; into a lookup of the table, and if an
                      ;; application, the following
                      ;;
                      ;; 1. (utxo int)
                      ;;    -> (:utxo (type-reference :int))
                      ;; 2. (utxo (int 64))
                      ;;    -> (:utxo (application (type-refernece :int) 64))
                      (list (util:symbol-to-keyword (car declaration-info))
                            `(spc:to-type-reference-format ',(cadr declaration-info))))
                    type-declarations))))

       ;; Create the function that we can now call, to create an instance
       (defun ,name (&key ,@fields)
         (spc:make-record :name ,key-name
                          ;; fill in the other slots
                          ,@(mapcan (lambda (field)
                                      (list (util:symbol-to-keyword field) field))
                                    fields)))
       ;; Return the Symbol itself!
       ',name)))


(defmacro defcircuit (name arguments &body body)
  (let* (;; Arguments are laid out like (public root (bytes 64))
         (just-args
           (remove-if-not (lambda (x) (typep (util:symbol-to-keyword x) 'spc:privacy))
                          arguments
                          :key #'car))
         (argument-names (mapcar #'cadr just-args))
         ;; Outputs are laid out like (output int)
         (just-output
           (remove-if (lambda (x) (typep (util:symbol-to-keyword x) 'spc:privacy))
                      arguments
                      :key #'car))
         (just-output
           (case (length just-output)
             (0 nil)
             (1 (car just-output))
             (t (error "In the arguments to defcircuit please only supply 1 output"))))
         (key-name (util:symbol-to-keyword name)))
    `(progn ;; make a lexical variable so we can just say it
            (serapeum:def ,name (spc:make-primitive :name ,key-name))
            ;; defun a function to apply the function
            (defun ,name (,@argument-names)
              (spc:make-application :function ,key-name
                                    :arguments (list ,@argument-names)))
            (let-refs
             ,argument-names
             (storage:add-function
              ,key-name
              (spc:make-circuit
               :return-type (spc:to-type-reference-format
                             ,(util:symbol-to-keyword (cadr just-output)))
               :name ,key-name
               :arguments (make-constraint-mapping-from-list '(,@just-args))
               ;; the body is a list of terms that we combine
               :body (list ,@body))))
            ',name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Macros and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro let-refs (variables &rest body)
  "let-refs lets the variables in the argument list be referred to in
the body at the CL level. The values are simply references to the
value in the Alucard environment. An example usage may
be (let-refs (a b) (+ a b))"
  `(let ,(mapcar (lambda (var)
                   (list var `(spc:make-reference
                               :name ,(util:symbol-to-keyword var))))
          variables)
     ;; Declare the values as ignoreable
     ;; Should we keep the warning!?
     (declare (ignorable ,@variables))
     ,@body))

(defun make-constraint-mapping-from-list (argument-list)
  "Takes a list of terms like (public root (bytes 64)) and generates out
a `sycamore:tree-map' from `keyword' to `spc:constraint'"
  (sycamore:alist-tree-map
   (mapcar (lambda (triple)
             (destructuring-bind (priv name type) triple
               (cons (util:symbol-to-keyword name)
                     (spc:make-constraint :privacy (util:symbol-to-keyword priv)
                                          :type (spc:to-type-reference-format type)))))
           argument-list)
   #'util:hash-compare))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defprimitive bytes)
(defprimitive int)

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
                  (private merk merkel-branch)
                  ;; should we have return type information be here
                  (output int))
  ;; (fold-tree root merk)
  ;; (equal (owner utxo) "test")
  poly)

(def ((a 3)
      (b 5))
  a)

(defcircuit constraint ((public const (bytes 64))
                        (output int))
  ;; (def ((a (some-constraint const))
  ;;       (b (range 32 a)))
  ;;   (range 64 a)
  ;;   b)
  constraint)
