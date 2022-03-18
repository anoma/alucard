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
  ((name
    :initarg  :name
    :type     keyword
    :accessor name
    :documentation "The name of the Type")
   (declaration
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
    :documentation "The Options for the declaration"))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Extra Functionality On Types                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-type-reference-format (term)
  "Given an application or a symbol, transform it to the correct type
storage format. So for example

1. int      -> (make-type-reference :name :int)
2. (int 64) -> (make-application :name (make-type-reference :name :int)
                                 :arguments (list 64))"
  ;; can either be a list number or atom
  (cond ((listp term)
         (let ((type-ref (mapcar #'to-type-reference-format term)))
           (make-application :function (car type-ref) :arguments (cdr type-ref))))
        ((numberp term) term)
        (t              (make-type-reference :name (util:symbol-to-keyword term)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Type Declaration Functions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(defun make-primitive (&key name)
  (make-instance 'primitive :name name))

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
