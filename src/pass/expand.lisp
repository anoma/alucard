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
    (with-accessors ((orig original) (exp expanded)) obj
      (format stream "~A ~A" orig exp))))

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
;; Argument Expansion API
;; We assume the code is in ANF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(-> expand-function-call (spc:application closure:typ) spc:application)
(defun expand-function-call (application closure)
  "expand function call, expand any record references into their
constituents and reforms the application, with arguments expanded
flatly."
  closure
  application)


(-> expand-record-lookup (spc:record-lookup closure:typ) (or null spc:reference))
(defun expand-record-lookup (lookup closure)
  "expands a record lookup call into the value itself"
  ;; it has to be a reference or a type error due to ANF
  ;; I need to do some type checking before this so I can give better errors
  (let ((term
          (closure:lookup closure (spc:name (spc:record lookup)))))
    (declare (type spc:term-no-binding term))
    term
    (error "Hi")))

(->  expand-def (spc:bind closure:typ) (or spc:bind spc:multiple-bind))
(defun expand-def (def closure)
  "Expands a let definition into potentially multiple lets or a single
multiple bind value"
  closure
  def)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (mapcar (lambda (x) (constraint-from-dotted-pair x :private))
;;         (expand-type-fields :utxo))

;; TODO:: Recursively run this via expand-type-into-constituents*
;; Need a better renaming strategy at that point
(-> expand-type-into-constituents (spc:constraint) argument)
(defun expand-type-into-constituents (circ)
  "Takes a constraint and expands user defined types into the proper
components, otherwise returns the type given back."
  (with-accessors ((name spc:name) (typ spc:typ) (priv spc:privacy)) circ
    (let ((expanded-list
            (expand-type-fields
             (etypecase-of spc:type-reference typ
               (spc:application    (spc:name (spc:func typ)))
               (spc:reference-type (spc:name typ))))))
      ;; TODO :: Properly expand generic pass through for non primitive
      (when (and (typep typ 'spc:application)
                 expanded-list)
        (error "Generics in custom user types is not supported yet"))
      (assure argument
        (if expanded-list
            (make-expanded :original name
                           :expanded
                           (mapcar (lambda (x)
                                     (constraint-from-dotted-pair x priv name))
                                   expanded-list))
            circ)))))

(-> naming-scheme (keyword keyword &optional fixnum) keyword)
(defun naming-scheme (prefix name &optional (iteration 0))
  "Derives the proper name for the expanded name from the original field
name, the record name, and the number of expanded iterations, starting
at 0."
  (let ((iteration-string
          (if (zerop iteration)
              ""
              (format nil "-~a" iteration))))
    (intern (concatenate 'string
                         (symbol-name prefix)
                         "-"
                         (symbol-name name)
                         iteration-string)
            'keyword)))

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
