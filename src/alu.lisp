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

(defmacro deftype (name-and-options generics &body type-declarations)
  ;; fields must not shuffle type-declaration for ordering of record
  ;; creation
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
         ;; keep this ordering up, as we rely on the correspondence
         (spc:make-record :name ,key-name
                          ;; fill in the other slots
                          ,@(mapcan (lambda (field)
                                      (list (util:symbol-to-keyword field) field))
                                    fields)))
       ;; Make accessors
       ,@(mapcar (lambda (field)
                   ;; don't overwrite what already exists
                   (if (fboundp field)
                       'nil
                       ;; we use defmethod as we will end up with
                       ;; multiple methods of the same file if
                       ;; multiple records have the same field, even
                       ;; with the check
                       `(defmethod ,field (record)
                          (spc:make-record-lookup
                           :record record
                           :field ,(util:symbol-to-keyword field)))))
                 fields)
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
    `(progn
       ;; make a lexical variable so we can just say it
       (serapeum:def ,name (spc:make-reference :name ,key-name))
       ;; defun a function to apply the function
       (defun ,name (,@argument-names)
         (spc:make-application :function (spc:make-reference :name ,key-name)
                               :arguments (list ,@argument-names)))
       (storage:add-function
        ,key-name
        (let-refs
         ,argument-names
         (storage:add-function
          ,key-name
          (spc:make-circuit
           :return-type (spc:to-type-reference-format
                         ,(util:symbol-to-keyword (cadr just-output)))
           :name ,key-name
           :arguments (mapcar #'make-constraint-from-list ',just-args)
           ;; the body is a list of terms that we combine
           :body ,(if (cl:= 1 (length body))
                      (car body)
                      `(list ,@body))))))
       ',name)))

(defmacro def (bind-values &rest body)
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
              :initial-value
              (if (cl:= (length body) 1)
                  (car body)
                  (cons 'list body)))))

(defmacro defprimitive-type (name)
  "defines a primitive type"
  (let ((keyword (util:symbol-to-keyword name)))
    ;; always set it to be safe
    `(storage:add-type ,keyword (spc:make-primitive :name ,keyword))))

(defmacro defprimitive (name)
  "defines a primitive type"
  (let ((keyword (util:symbol-to-keyword name)))
    ;; always set it to be safe
    `(progn
       ;; don't need to gensym arguments as there are no capture issues
       (defun ,name (&rest arguments)
         (spc:make-application :function (spc:make-reference :name ,keyword)
                               :arguments arguments))
       (serapeum:def ,name (spc:make-reference :name ,keyword))
       (storage:add-function ,keyword (spc:make-primitive :name ,keyword)))))

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

(-> make-constraint-from-list (list) spc:constraint)
(defun make-constraint-from-list (term)
  "Takes a terms like (public root (bytes 64)) and generates out a `spc:constraint'"
  (values
   (destructuring-bind (priv name type) term
     (spc:make-constraint :name    (util:symbol-to-keyword name)
                          :privacy (util:symbol-to-keyword priv)
                          :type    (spc:to-type-reference-format type)))))

(-> make-constraint-mapping-from-list (list) sycamore:tree-map)
(defun make-constraint-mapping-from-list (argument-list)
  "Takes a list of terms like (public root (bytes 64)) and generates out
a `sycamore:tree-map' from `keyword' to `spc:constraint'"
  (values
   (sycamore:alist-tree-map
    (mapcar (lambda (triple)
              (destructuring-bind (priv name type) triple
                (cons (util:symbol-to-keyword name)
                      (spc:make-constraint :privacy (util:symbol-to-keyword priv)
                                           :type (spc:to-type-reference-format type)))))
            argument-list)
    #'util:hash-compare)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Primitive type declaration
(defprimitive-type bytes)
(defprimitive-type int)
(defprimitive-type void)

;; Primitive function declaration
;; Add to a list, so we can get exhaustion when we write functions over this
;; ALU> (defparameter *x* '(or (eql :foo)))
;; *X*
;; ALU> (cl:deftype test () *x*)
;; TEST
;; ALU> (typep :foo 'test)
;; T
;; ALU> (typep :fooa 'test)
;; NIL
(defprimitive +)
(defprimitive *)
(defprimitive =)
(defprimitive range)
(defprimitive exp)

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

;; (defcircuit fold-tree )

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

(defcircuit constraint ((public const (bytes 64))
                        (output int))
  (def ((a (= (+ const 53) 0))
        (b (range 32 a)))
    (and (range 64 a)
         b)))

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
