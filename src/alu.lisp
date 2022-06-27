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
  (let* ((fields  (mapcar #'car type-declarations))
         (name    (if (listp name-and-options)
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
         (ensure-call-by-value
          (spc:make-record :name ,key-name
                           ;; fill in the other slots
                           ,@(mapcan (lambda (field)
                                       (list (util:symbol-to-keyword field) field))
                                     fields))))
       ;; Make accessors
       ,@(mapcar (lambda (field)
                   ;; don't overwrite what already exists
                   (if (fboundp field)
                       'nil
                       ;; we use defmethod as we will end up with
                       ;; multiple methods of the same file if
                       ;; multiple records have the same field, even
                       ;; with the check
                       `(ignore-errors
                         (defmethod ,field (record)
                           (ensure-call-by-value
                            (spc:make-record-lookup
                             :record record
                             :field ,(util:symbol-to-keyword field)))))))
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
         ;; Gensym unique names to avoid clashes
         (gensym-argument-names (mapcar #'gensym-symbol argument-names))
         ;; update just-args to use the gensymed name instead of the original name
         (just-args-gensym
           (mapcar (lambda (arg gensym)
                     ;; replace the cadr of arg
                     (list* (car arg)
                            gensym
                            (cddr arg)))
                   just-args
                   gensym-argument-names))
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
         (ensure-call-by-value
          (spc:make-application :function (spc:make-reference :name ,key-name)
                                :arguments (list ,@argument-names))))
       (storage:add-function
        ,key-name
        (spc:make-circuit
         :return-type (spc:to-type-reference-format ',(cadr just-output))
         :name ,key-name
         :arguments (mapcar #'make-constraint-from-list ',just-args-gensym)
         ;; the body is a list of terms that we combine
         :body '(emit:instruction
                 (let-refs-alist
                  ;; sad as we lose our nice naming for generated code ☹
                  ,(mapcar #'cons argument-names gensym-argument-names)
                  ,(if (cl:= 1 (length body))
                       (car body)
                       `(list ,@body))))))
       ',name)))

(defun gensym-symbol (symbol)
  "gensyms with the given symbol name"
  (gensym (symbol-name symbol)))

(defmacro defgate (name arguments &body body)
  `(defcircuit ,name ,@(mapcar (lambda (x)
                                 (if (equalp :output (util:symbol-to-keyword (car x)))
                                     x
                                     (cons 'private x)))
                               arguments)
     ,body))

(defmacro def (bind-values &rest body)
  "defines the values in the presence of the body"
  ;; bind the values at the CL level, so we can just reference it
  (flet ((constraint? (x)
           (equalp (util:symbol-to-keyword (car x)) :with-constraint)))
    (let* ((bound-names
             ;; list in the case of with-constraint, symbol otherwise
             (mapcar (lambda (x)
                       (if (constraint? x) (cadr x) (car x)))
                     bind-values))
           (gensym-names
             (mapcar (lambda (x)
                       (if (listp x)
                           (mapcar #'gensym-symbol x)
                           (gensym-symbol x)))
                     bound-names)))
      `(let-refs-alist
        ;; remove check
        ,(mapcan (lambda (bound gensym)
                   (if (listp bound)
                       (mapcar #'cons bound gensym)
                       (list (cons bound gensym))))
                 bound-names gensym-names)
        ;; Generate out the Alucard level binding
        ,@(mapcar (lambda (bind-pair gensym)
                    (if (constraint? bind-pair)
                        `(with-constraint ,gensym ,@(cddr bind-pair))
                        `(emit:instruction
                          (spc:make-let
                           :var (util:symbol-to-keyword ',gensym)
                           :val ,(cadr bind-pair)))))
                  bind-values
                  gensym-names)
        ,(if (cl:= (length body) 1)
             (car body)
             (cons 'list body))))))

(defmacro entry-point (symbol)
  "Sets the entry point of the circuit to the desired function"
  `(storage:set-entry-point ,(util:symbol-to-keyword symbol)))

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
         (ensure-call-by-value
          (spc:make-application :function (spc:make-reference :name ,keyword)
                                :arguments arguments)))
       (serapeum:def ,name (spc:make-reference :name ,keyword))
       (storage:add-function ,keyword (spc:make-primitive :name ,keyword)))))

(defmacro coerce (value type-to)
  `(ensure-call-by-value
    (spc:make-type-coerce
     :typ (spc:to-type-reference-format ',type-to)
     :value ,value)))

(defmacro check (value type-check)
  `(ensure-call-by-value
    (spc:make-type-check
     :typ (spc:to-type-reference-format ',type-check)
     :value ,value)))

(defun to-array (&rest arguments)
  (ensure-call-by-value
   (spc:make-from-data :contents arguments)))

;; eventually make the type optional
(defmacro array (length type)
  `(ensure-call-by-value
    (spc:make-array-allocate
     :typ  (spc:to-type-reference-format ',type)
     :size ,length)))

(defun get (array position)
  (ensure-call-by-value
   (spc:make-array-lookup :arr array :pos position)))

(defsetf get (array location) (value)
  `(ensure-call-by-value
     (spc:make-array-set :arr ,array
                         :pos ,location
                         :value ,value)))

(defmacro with-constraint (variable-names &rest body)
  (let ((body-list (gensym)))
    `(let ((,body-list (list nil)))
       (emit:with-circuit-body ,body-list
         (let-refs ,variable-names
                   ,@body))
       (emit:instruction
        (spc:make-bind-constraint
         :var (mapcar #'util:symbol-to-keyword ',variable-names)
         :value ,body-list))
       void)))

(serapeum:def void (alu.spec:make-reference :name :void))

(defun ensure-call-by-value (term &optional (name "G"))
  "This ensures that the value given back is a reference. This matters
as when we evaluate expression terms, they are an ast value, not a
reference to the AST value. This makes sure that information propagates
and we are given back a reference to operate on."
  (let* ((key (util:symbol-to-keyword (gensym name))))
    (emit:instruction (spc:make-let :var key :val term))
    (spc:make-reference :name key)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Macros and functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro let-refs (variables &rest body)
  "let-refs lets the variables in the argument list be referred to in
the body at the CL level. The values are simply references to the
value in the Alucard environment. An example usage may
be (let-refs (a b) (+ a b))"
  `(let-refs-alist ,(mapcar (lambda (x) (cons x x)) variables) ,@body))

(defmacro let-refs-alist (variables &rest body)
  "lets the variables in the argument list be referred to at the CL
level. However the input is an alist so it is unique at the Alucard
Level. An example usage may be
(let-refs-alist ((a . a123123) (b . b123123)) (prld:+ a b))"
  `(let ,(mapcar (lambda (var)
                   (list (car var)
                         `(spc:make-reference
                           :name ,(util:symbol-to-keyword (cdr var)))))
          variables)
     ;; Declare the values as ignoreable
     ;; Should we keep the warning!?
     (declare (ignorable ,@(mapcar #'car variables)))
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
                                           :name (util:symbol-to-keyword name)
                                           :type (spc:to-type-reference-format type)))))
            argument-list)
    #'util:hash-compare)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EXAMPLES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
