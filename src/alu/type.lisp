(in-package :alu)

(cl:deftype alu-type-reference ()
  "When we refer to the type in the language it will be through the type
reference. If we are apply a type, then "
  `(or type-reference
       ;; can be found in alu/term.lisp
       application))

(defclass type-reference ()
  ((name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "Type reference"))
  (:documentation "Represents a variable in the Alucard language"))

(cl:deftype alu-type-storage ()
  "The type we store in the top level type storage"
  `(or primitive type-declaration))

(defclass primitive ()
  ((name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "The name of the primitive"))
  (:documentation "Primitive type in the Alu language"))

(defclass type-declaration ()
  ((declaration
    :initarg  :decl
    :type     alu-type-format
    :accessor decl
    :documentation "The data declaration")
   ;; currently unused
   (generics
    :initarg  :generics
    :type     list
    :accessor generics
    :documentation "Any extra generic argumentation that the type can
take (primitives take an extra integer, we may with to propagate)")
   (options
    :initarg  :options
    :type     hash-table
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
;; Reference Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj type-reference) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(defun make-type-reference (&key name)
  (make-instance 'type-reference :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Primitive Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj primitive) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type Declaration Functionalities                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj type-declaration) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((decl decl) (gen generics) (opt options) (name name)) obj
      (let ((plist (alexandria:hash-table-plist opt)))
        ;; should abstract this bit out eventually but w/e
        (if plist
            (format stream "TYPE (~A ~A) ~{~A ~}= ~A" name plist gen decl)
            (format stream "TYPE ~A ~{~A ~}= ~A" name gen decl))))))

(defun make-type-declaration (&key
                                (name (error "please provide name"))
                                options generics
                                (decl (error "please provide declaration")))
  (make-instance 'type-declaration
                 :decl decl :options options :generics generics :name name))

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
