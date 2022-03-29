(in-package :alu.spec)

(deftype function-type ()
  "The type we store in the top level function storage"
  `(or primitive circuit))

(deftype type-storage ()
  "The type we store in the top level type storage"
  `(or primitive type-declaration))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                      Function Type Storage Type                            ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass circuit ()
  ((name :initarg :name
         :type    keyword
         :accessor name
         :documentation "Name of the circuit")
   ;; a list of constraints
   (arguments :initarg :arguments
              :type    list
              :accessor arguments
              :documentation "Arguments for the circuit, saved in a
a list of `constraint'")
   (expanded-arguments
    :type    list
    :accessor expanded-arguments
    :documentation "expanded arguments for a given circuit, This is
not written at creation time, but later set by a later pass which
determines and caches the appropriate expanded argument list")
   (return-type :initarg  :return-type
                :type     (or type-reference null)
                :accessor return-type
                :documentation "The return output of a given circuit")
   (body :initarg  :body
         :accessor body
         :documentation "The circuit logic")))

(deftype privacy ()
  `(or (eql :private)
       (eql :public)))

(defclass constraint ()
  ((name :initarg  :name
         :initform :name
         :type     keyword
         :accessor name
         :documentation "The name of the constraint")
   (privacy :initarg  :privacy
            :initform :private
            :type     privacy
            :accessor privacy
            :documentation "Is the constraint public or private?")
   (type :initarg  :type
         :type     type-reference
         :accessor typ
         :documentation "The type of the constraint")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                         Alu Type Storage Type                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass type-declaration ()
  ((name :initarg  :name
         :type     keyword
         :accessor name
         :documentation "The name of the Type")
   ;; currently unused
   (generics :initarg  :generics
             :type     list
             :accessor generics
             :documentation "Any extra generic argumentation that the
type can take (primitives take an extra integer, we may with to propagate)")
   (options :initarg  :options
            :initform (sycamore:make-tree-map #'util:hash-compare)
            :type     sycamore:tree-map
            :accessor options
            :documentation "The Options for the declaration")
   (declaration :initarg  :decl
                :type     type-format
                :accessor decl
                :documentation "The data declaration"))
  (:documentation "Type declaration in the Alu language"))

(deftype type-format ()
  "this is the choice of the format the type declaration can be"
  `(or record-decl sum-decl))

(defclass record-decl ()
  ((contents :initarg :contents
             :initform (sycamore:make-tree-map #'util:hash-compare)
             :type     sycamore:tree-map
             :accessor contents
             :documentation "Holding fields that are declared along with their type"))
  (:documentation "Record declaration"))

(defclass sum-decl ()
  ()
  (:documentation "Sum type declaration"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                   Function Type Storage Functions                          ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Circuit Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-object ((obj circuit) stream)
  (with-accessors ((name name) (ret return-type) (bod body) (arg arguments)) obj
    (format stream "Circuit ~A~{ ~A~^~} =~%~A : ~A"
            name
            (mapcar #'name arg)
            bod
            ret)))

(defun make-circuit (&key name arguments return-type body)
  (make-instance 'circuit
                 :name name
                 :body body
                 :return-type return-type
                 :arguments arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj constraint) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A ~A" (privacy obj) (name obj) (typ obj))))

(-> make-constraint
    (&key (:name keyword) (:privacy privacy) (:type type-reference)) constraint)
(defun make-constraint (&key (name (error "please provide name"))
                             (privacy :private)
                             type)
  (values
   (make-instance 'constraint :name name :type type :privacy privacy)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Type Declaration Functionalities                       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj type-declaration) stream)
  (print-unreadable-object (obj stream)
    (with-accessors ((decl decl) (gen generics) (opt options) (name name)) obj
      (let ((plist (util:sycamore-symbol-map-plist opt)))
        ;; should abstract this bit out eventually but w/e
        (if plist
            (format stream "TYPE (~A ~A) ~{~A ~}= ~A" name plist gen decl)
            (format stream "TYPE ~A ~{~A ~}= ~A" name gen decl))))))

(defun make-type-declaration (&key
                                (name (error "please provide name"))
                                (options (sycamore:make-tree-map #'util:hash-compare))
                                generics
                                (decl (error "please provide declaration")))
  (make-instance 'type-declaration
                 :decl decl :options options :generics generics :name name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Record Declaration Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj record-decl) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((cont contents)) obj
      (format stream "~{:~A ~A~^ ~}" (util:sycamore-symbol-map-plist cont)))))

(defun make-record-declaration (&rest arguments &key &allow-other-keys)
  (make-instance 'record-decl :contents (util:sycamore-plist-symbol-map arguments)))
