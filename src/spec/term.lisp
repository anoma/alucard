(in-package :alu.spec)

;; data Alu = Number | Application ... | Record | Record-Lookup ...
(deftype term ()
  "The Alu term type, which dictates what terms can be written bound."
  `(or term-no-binding
       let-node))

(deftype term-no-binding ()
  "The Alucard term type with no binding terms"
  `(or number
       primitive
       application
       record
       record-lookup
       reference))

;; An alu Expression type.
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
             :documentation "the storage of the initial type mapping"))
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
          :documentation "the value that is bound")
   (body :initarg :body
         :accessor body
         :documentation "The body where the let value exists in"))
  (:documentation "Represents a variable binding in the Alucard language"))

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
  (let ((hash (sycamore:tree-map-remove (util:sycamore-plist-symbol-map arguments)
                                        :name)))
    (assure record
      (make-instance 'record :name name :contents hash))))

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
;; Let Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj let-node) stream)
  (with-accessors ((body body) (value value) (var var)) obj
    (format stream "LET ~A = ~A IN~%~A" var value body)))

(defun make-let (&key (var  (error "Please provide the variable"))
                      (val  (error "Please provide the value field"))
                      (body (error "Please provide the body")))
  (make-instance 'let-node :value val :variable var :body body))

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
