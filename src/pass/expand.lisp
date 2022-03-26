(in-package :alu.pass.expanded)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Declarations for expanded argument storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype argument ()
  `(or expand spc:constraint))

(defclass expand ()
  ((original :initarg :original
             :type    keyword
             :accessor original
             :documentation "The original name the argument had")
   (expanded :initarg :expanded
             :type    list
             :accessor expanded
             :documentation
             "The fully expanded argument list of `spc:constraint'")))

(defun make-expanded (&key original expanded)
  (make-instance 'expand :original original :expanded expanded))

(defmethod print-object ((obj expand) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A ~A" (original obj) (expanded obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Argument Expansion API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (calculate-full-arguments-from-storage :poly)
(-> full-arguments-from-storage (keyword) list)
(defun full-arguments-from-storage (name)
  "Calculates the full argument list with records being expanded"
  (let ((circuit (storage:lookup-function name)))
    (when circuit
      (etypecase-of spc:function-type circuit
        (spc:primitive nil)
        (spc:circuit   (mapcar #'expand-type-into-constituents
                               (spc:arguments circuit)))))))

(-> cache-expanded-arguments! (keyword) null)
(defun cache-expanded-arguments! (name)
  "Calculates the full argument list with records being expanded, and
caches them on the structure"
  (let ((circuit  (storage:lookup-function name))
        (expanded (full-arguments-from-storage name)))
    (when (and circuit expanded)
      (setf (spc:expanded-arguments circuit)
            expanded)
      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Type Expansion API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

(-> full-type-reference* (spc:type-reference &optional sycamore:tree-set) (or spc:type-reference list))
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

;; TODO :: Update with nested
(-> expand-type-into-constituents (spc:constraint) argument)
(defun expand-type-into-constituents (circ)
  "Takes a constraint and expands user defined types into the proper
components, otherwise returns the type given back."
  (with-accessors ((name spc:name) (typ spc:typ) (priv spc:privacy)) circ
    (let ((expanded-list
            (full-type-reference* typ)))
      ;; TODO :: Properly expand generic pass through for non primitive
      (when (and (typep typ 'spc:application)
                 (listp expanded-list))
        (error "Generics in custom user types is not supported yet"))
      (when (and (listp expanded-list)
                 (some (lambda (pair) (listp (cdr pair)))
                       expanded-list))
        (error "Nested type expansion is not supported yet!"))
      (assure argument
        (if (listp expanded-list)
            (make-expanded :original name
                           :expanded
                           (mapcar (lambda (x)
                                     (constraint-from-dotted-pair x priv name))
                                   expanded-list))
            circ)))))

(-> naming-scheme (keyword keyword) keyword)
(defun naming-scheme (prefix name)
  "Derives the proper name for the expanded name from the original field
name, the record name"
  (intern (concatenate 'string (symbol-name prefix) "-" (symbol-name name))
          'keyword))

(-> constraint-from-dotted-pair (list spc:privacy keyword) spc:constraint)
(defun constraint-from-dotted-pair (list privacy prefix)
  "creates a constraint given an alist, a privacy modified, and the
original argument name."
  (let ((new-name
          (naming-scheme prefix (car list))))
    (assure spc:constraint
      (spc:make-constraint :name    new-name
                           :privacy privacy
                           :type    (cdr list)))))

(-> expand-type-reference (spc:type-reference) list)
(defun expand-type-reference (ref)
  (expand-type-fields
   (etypecase-of spc:type-reference ref
     (spc:application    (spc:name (spc:func ref)))
     (spc:reference-type (spc:name ref)))))

(-> expand-type-fields (keyword) list)
(defun expand-type-fields (name)
  "Expands the given type to an alist type of the expanded values"
  (values
   (let ((lookup (storage:lookup-type name)))
     (etypecase-of (or null spc:type-storage) lookup
       (null          nil)
       (spc:primitive nil)
       (spc:type-declaration
        (match-of spc:type-format (spc:decl lookup)
          ((spc:record-decl spc:contents)
           (sycamore:tree-map-alist spc:contents))
          ((spc:sum-decl)
           (error "sum types are not supported yet"))))))))
