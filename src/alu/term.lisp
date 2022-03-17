(in-package :alu)

;; data Alu = Number | Application ... | Record | Record-Lookup ...
(cl:deftype alu-term ()
  "The Alu term type, which dictates what terms can be written bound."
  `(or number
       application
       record
       record-lookup
       let-node
       reference))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Alucard Term Declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We are using defclass as they are more flexible.... we just pay in
;; terms of verbosity, but that's not a big deal.

(defclass application ()
  ((name
    :initarg :name
    :accessor name
    :type     keyword
    :documentation "the name of the gate that we wish to apply")
   (arguments
    :initarg :args
    :initform nil
    :type     list
    :accessor arguments
    :documentation "The arguments in which the gate is called upon"))
  (:documentation "application is the application type of the alu ADT"))

(defclass record ()
  ((name
    :initarg :name
    :accessor name
    :type     keyword
    :documentation "The name of the constructor of the alucard type")
   ;; half tempted to bring in fset to have a functional hashtable
   ;; here...
   (contents
    :initarg :contents
    :initform (make-hash-table)
    :type     hash-table
    :accessor contents
    :documentation "the storage of the initial type mapping"))
  (:documentation "Represents an instance of a record type"))

(defclass record-lookup ()
  ((record
    :initarg :record
    :accessor record
    :documentation "the record in which we are grabbing the data out of")
   (field
    :initarg  :field
    :type     keyword
    :accessor field
    :documentation "The field we wish to lookup from the record"))
  (:documentation "Represents a field lookup"))

(defclass let-node ()
  ((variable
    :initarg  :variable
    :type     keyword
    :accessor var
    :documentation "The variable that will be bound")
   (value
    :initarg :value
    :accessor value
    :type     alu-term
    :documentation "the value that is bound")
   (body
    :initarg :body
    :accessor body
    :documentation "The body where the let value exists in"))
  (:documentation "Represents a variable binding in the Alucard language"))

(defclass reference ()
  ((name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "The Variable reference"))
  (:documentation "Represents a variable in the Alucard language"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Application Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj application) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((name name) (args arguments)) obj
      (format stream "~A ~{~A~^ ~}" name args))))

(defun make-application (&key (name (error "Please provide a name")) arguments)
  (make-instance 'application :name name :args arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj record) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((name name) (cont contents)) obj
      (format stream "~A ~{:~A ~A~^ ~}" name (alexandria:hash-table-plist cont)))))

(defun make-record (&rest arguments &key name &allow-other-keys)
  (let ((hash (alexandria:plist-hash-table arguments)))
    (remhash :name hash)
    (make-instance 'record :name name :contents hash)))

(defun lookup-record (record field)
  "looks up the alu-record type by the field"
  (gethash field (contents record)))

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

(defun make-reference (&key name)
  (make-instance 'reference :name name))
