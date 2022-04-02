(in-package :alu.pass.expanded)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Declarations for expanded argument storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype argument ()
  `(or expand spc:constraint))

(deftype argument-list ()
  "A constraint-list is a list of fully-expanded-terms"
  `(satisfies argument-list))

(defclass expand ()
  ((original :initarg :original
             :type    keyword
             :accessor original
             :documentation "The original name the argument had")
   (expanded :initarg  :expanded
             :type     list
             :accessor expanded
             :documentation
             "The fully expanded argument alist from keyword to `spc:constraint'")))

(defun make-expanded (&key original expanded)
  (make-instance 'expand :original original :expanded expanded))

(defmethod print-object ((obj expand) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (original obj) (expanded obj))))


(defun argument-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'argument)) list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument Expansion API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (calculate-full-arguments-from-storage :poly)
(-> full-arguments-from-storage (keyword) argument-list)
(defun full-arguments-from-storage (name)
  "Calculates the full argument list with records being expanded into
the `expand' type"
  (let ((circuit (storage:lookup-function name)))
    (when circuit
      (etypecase-of spc:function-type circuit
        (spc:primitive nil)
        (spc:circuit   (full-arguments-from-circuit circuit))))))

(-> full-arguments-from-circuit (spc:circuit) argument-list)
(defun full-arguments-from-circuit (circuit)
  "Calculates the full argument list with records being expanded into
being the `expand' type"
  (mapcar #'expand-type-into-constituents
          (spc:arguments circuit)))

(-> argument-names (argument-list) list)
(defun argument-names (argument-list)
  (mapcan (lambda (x)
            (etypecase-of argument x
              (spc:constraint (list (spc:name x)))
              (expand         (argument-names (util:alist-values (expanded x))))))
          argument-list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Type Expansion API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> full-return-values (keyword) (or spc:type-reference list))
(defun full-return-values (name)
  "Expands the return type into the constitute fields recursively and
gives back the original output type, an empty list if primitive, or an
alist

alist-return-example:
((:TIME (:X . #<ALU.SPEC:REFERENCE-TYPE INT>)
        (:Y . #<ALU.SPEC:REFERENCE-TYPE INT>))
 (:PLANE (:X . #<ALU.SPEC:REFERENCE-TYPE INT>)
         (:Y . #<ALU.SPEC:REFERENCE-TYPE INT>)))"
  (let ((circuit (storage:lookup-function name)))
    (when circuit
      (etypecase-of spc:function-type circuit
        (spc:primitive nil)
        (spc:circuit   (full-type-reference* (spc:return-type circuit)))))))

(-> full-type-reference*
    (spc:type-reference &optional sycamore:tree-set)
    (or spc:type-reference list))
(defun full-type-reference* (ref &optional
                                   (seen-set (sycamore:tree-set #'util:hash-compare)))
  "Expands a type reference into it's expanded members recursively"
  (let* ((name
           (etypecase-of spc:type-reference ref
             (spc:application    (spc:name (spc:func ref)))
             (spc:reference-type (spc:name ref))))
         (new-set
           (sycamore:tree-set-insert seen-set name)))
    (if (sycamore:tree-set-find seen-set name)
        ref
        (let ((expand (expand-type-fields name)))
          (if expand
              (mapcar (lambda (x)
                        (let ((expand* (full-type-reference* (cdr x) new-set)))
                          (cons (car x) expand*)))
                      expand)
              ref)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expanding Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> expand-type-into-constituents (spc:constraint) argument)
(defun expand-type-into-constituents (circ)
  "Takes a constraint and expands user defined types into the proper
components, otherwise returns the type given back."
  (with-accessors ((name spc:name) (typ spc:typ) (priv spc:privacy)) circ

    (let ((expanded-list (full-type-reference* typ)))
      (when (and (typep typ 'spc:application)
                 (listp expanded-list))
        (error "Generics in custom user types is not supported yet"))
      (assure argument
        (if (listp expanded-list)
            (make-expanded
             :original name
             :expanded (mapcar (lambda (x)
                                 (constraint-alist-from-dotted-pair* x priv name))
                               expanded-list))
            circ)))))

(-> expand-type-reference (spc:type-reference) list)
(defun expand-type-reference (ref)
  (expand-type-fields
   (etypecase-of spc:type-reference ref
     (spc:application    (spc:name (spc:func ref)))
     (spc:reference-type (spc:name ref)))))

(-> expand-type-fields (keyword) list)
(defun expand-type-fields (name)
  "Expands the given type to an alist of (:field-name . `spc:type-reference')"
  (values
   (let ((lookup (storage:lookup-type name)))
     (etypecase-of (or null spc:type-storage) lookup
       (null          nil)
       (spc:primitive nil)
       (spc:type-declaration
        (etypecase-of spc:type-format (spc:decl lookup)
          (spc:record-decl
           (spc:record-declaration->alist (spc:decl lookup)))
          (spc:sum-decl
           (error "sum types are not supported yet"))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Creation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> constraint-alist-from-dotted-pair* (list spc:privacy keyword) list)
(defun constraint-alist-from-dotted-pair* (list privacy prefix)
  "creates a constraint given an alist, a privacy modified, and the
original argument name.

(constraint-alist-from-dotted-pair*
 `(:hi . ((:bah . ,(spc:make-type-reference :name :int))
          (:baz . ,(spc:make-type-reference :name :int))))
 :private
 :prefix)

===>

(:HI
  . #<EXPAND:EXPAND PREFIX-HI
     ((:BAH . #<ALU.SPEC:CONSTRAINT PRIVATE PREFIX-HI-BAH #<REFERENCE-TYPE INT>>)
      (:BAZ . #<ALU.SPEC:CONSTRAINT PRIVATE PREFIX-HI-BAZ #<REFERENCE-TYPE INT>>))>)

(constraint-alist-from-dotted-pair*
  `(:name . ,(spc:make-type-reference :name :int)) :private :prefix)

===>

(:NAME . #<ALU.SPEC:CONSTRAINT PRIVATE PREFIX-NAME #<REFERENCE-TYPE INT>>)
"
  (destructuring-bind (key . cont) list
    (let ((new-name (naming-scheme prefix key)))
      (cons
       key
       (if (listp cont)
           (make-expanded
            :original new-name
            :expanded (mapcar (lambda (p)
                                (constraint-alist-from-dotted-pair* p privacy new-name))
                              cont))
           (spc:make-constraint :name new-name :privacy privacy :type cont))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> naming-scheme (keyword keyword) keyword)
(defun naming-scheme (prefix name)
  "Derives the proper name for the expanded name from the original field
name, the record name"
  (intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name))
          'keyword))
