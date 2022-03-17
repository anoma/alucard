(in-package :alu)

(cl:deftype alu-type-reference ()
  "When we refer to the type in the language it will be through the type
reference"
  `(or primitive
       type-reference))

(defclass primitive ()
  ()
  (:documentation "Primitive type in the Alu language"))

(defclass type-reference ()
  ((name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "Type reference"))
  (:documentation "Represents a variable in the Alucard language"))

(defclass type-declaration ()
  ((declaration
    :initarg  :decl
    :type     alu-type-format
    :accessor decl
    :documentation "The data declaration")
   (options
    :initarg  :options
    :type     list
    :accessor options
    :documentation "The Options for the declaration")
   (name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "The name of the Type"))
  (:documentation "Type declaration in the Alu language"))

(cl:deftype alu-type-format ()
  "this is the choice of the format the type declaration can be"
  `(or record-decl sum-decl))

(defclass record-decl ()
  ((contents
    :initarg :contents
    :initform (make-hash-table)
    :type     hash-table
    :accessor contents
    :documentation "Holding fields that are declared along with their type"))
  (:documentation "Record declaration"))

(defclass sum-decl ()
  ()
  (:documentation "Sum type declaration"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record Declaration Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj record-decl) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((cont contents)) obj
      (format stream "~{:~A ~A~^ ~}" (alexandria:hash-table-plist cont)))))

(defun make-record-declaration (&rest arguments &key &allow-other-keys)
  (make-instance 'record-decl
                 :contents (alexandria:plist-hash-table arguments)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj type-reference) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(defun make-type-reference (&key name)
  (make-instance 'type-reference :name name))
