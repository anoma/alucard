(in-package :alu.spec)

;; data Alu = Number | Application ... | Record | Record-Lookup ...
(deftype term ()
  "The starting Alucard term. This is the starting AST in which alucard
is expressed from"
  `(or term-no-binding bind-constraint let-node))

(deftype term-no-binding ()
  "The starting Alucard term type with no binding terms included. This
type is often used in the value slot of a binder after linearization
since we want to ensure a binder does not contain another binder"
  `(or base record-forms))

(deftype base ()
  "Alucard terms which are apart of the core/base system. These won't be
removed until very late in the pipeline"
  `(or term-normal-form application))

(deftype record-forms ()
  "Alucard forms that relate to records"
  `(or record record-lookup))

(deftype term-normal-form ()
  "Alucard terms which are fully in normal form"
  `(or number reference))

;; An alu Expression type.
;; TODO :: Deprecated
(deftype expression ()
  "The Alu expression type. The expression type is the `term'
augmented with the common lisp list type."
  `(or ;; we may want to remove this, if we go for a more effectful
    ;; route rather than just binding naively on what we find.
    ;;
    ;; We can do this by pushing to some list, then collecting the
    ;; constraints at the end of the expression.
    ;; we also use cons so we can
    cons
    ;; from alu/term
    term))

;; we do this as number isn't a pattern in trivia, and thus our
;; exhaustion of match-of is a bit off!
(trivia:defpattern number (x)
  (alexandria:with-gensyms (it)
    `(trivia:guard1 (,it :type number)
                    (numberp ,it)
                    ,it ,x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alucard Term Declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We are using defclass as they are more flexible.... we just pay in
;; terms of verbosity, but that's not a big deal.

(defclass primitive ()
  ((name :initarg  :name
         :type     keyword
         :accessor name
         :documentation "The name of the primitive"))
  (:documentation "Primitive type in the Alu language"))

(defclass application ()
  ((name :initarg :function
         :accessor func
         :documentation "the name of the gate that we wish to
apply. This will be either a reference type or a type-reference
depending on what table it is related to.")
   (arguments :initarg :args
              :initform nil
              :type     list
              :accessor arguments
              :documentation "The arguments in which the gate is called upon"))
  (:documentation "application is the application type of the alu ADT"))

(defclass record ()
  ((name :initarg :name
         :accessor name
         :type     keyword
         :documentation "The name of the constructor of the alucard type")
   (contents :initarg :contents
             :initform (sycamore:make-tree-map #'util:hash-compare)
             :type     sycamore:tree-map
             :accessor contents
             :documentation "the storage of the initial type mapping")
   (order :initarg :order
          :initform nil
          :type     list
          :accessor order
          :documentation "For keeping a consistent order of fields between implementations"))
  (:documentation "Represents an instance of a record type"))

(defclass record-lookup ()
  ((record :initarg :record
           :accessor record
           :documentation "the record in which we are grabbing the data out of")
   (field :initarg  :field
          :type     keyword
          :accessor field
          :documentation "The field we wish to lookup from the record"))
  (:documentation "Represents a field lookup"))

(defclass let-node ()
  ((var :initarg  :variable
        :type     keyword
        :accessor var
        :documentation "The variable that will be bound")
   (value :initarg :value
          :accessor value
          :type     term
          :documentation "the value that is bound"))
  (:documentation "Represents a variable binding in the Alucard language"))

(defclass bind-constraint ()
  ((var :initarg  :variable
        :type     list
        :accessor var
        :documentation "The terms which are created in the bind-constraint binding")
   (value :initarg :value
          :accessor value
          :type     list
          :documentation "the constraints"))
  (:documentation "Represents a bind-constraint in the Alucard language"))

(defclass reference ()
  ((name :initarg  :name
         :type     keyword
         :accessor name
         :documentation "The Variable reference"))
  (:documentation "Represents a variable in the Alucard language"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj primitive) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(defun make-primitive (&key name)
  (make-instance 'primitive :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj application) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((fun func) (args arguments)) obj
      (format stream "~A ~{~A~^ ~}" fun args))))

(defun make-application (&key (function (error "Please provide a name")) arguments)
  (make-instance 'application :function function :args arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj record) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name) (cont contents)) obj
      (format stream "~A ~{:~A ~A~^ ~}"
              name (util:sycamore-symbol-map-plist cont)))))

(-> make-record (&key (:name keyword) &allow-other-keys) record)
(defun make-record (&rest arguments &key name &allow-other-keys)
  (let* ((alist (alexandria:plist-alist arguments))
         (no-name (remove-if (lambda (x) (eq :name x)) alist :key #'car))
         (hash (sycamore:alist-tree-map no-name #'util:hash-compare)))
    (assure record
      (make-instance 'record :name name
                             :contents hash
                             :order (mapcar #'car no-name)))))

(defun record->alist (record)
  (mapcar (lambda (field)
            (cons field (sycamore:tree-map-find (contents record) field nil)))
          (order record)))

(-> lookup-record (record keyword) (or term null))
(defun lookup-record (record field)
  "looks up the alu-record type by the field"
  (values
   (sycamore:tree-map-find (contents record) field)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record Lookup Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj record-lookup) stream)
  (print-unreadable-object (obj stream)
    (format stream "LOOKUP ~A.~A" (record obj) (field obj))))

(defun make-record-lookup (&key (record (error "Please provide the record"))
                                (field  (error "Please provide the record field")))
  (make-instance 'record-lookup :record record :field field))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind Constraint Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj bind-constraint) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "(~{~A~^, ~}) ~A" (var obj) (value obj))))

(defun make-bind-constraint (&key var value)
  (make-instance 'bind-constraint :value value
                                  :variable var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Let Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj let-node) stream)
  (with-accessors ((body body) (value value) (var var)) obj
    (format stream "LET ~A = ~A" var value)))

(defun make-let (&key (var  (error "Please provide the variable"))
                      (val  (error "Please provide the value field")))
  (make-instance 'let-node :value val :variable var))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj reference) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(-> make-reference (&key (:name keyword)) reference)
(defun make-reference (&key name)
  (assure reference
    (make-instance 'reference :name name)))
