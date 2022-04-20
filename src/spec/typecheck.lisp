(in-package :alu.basic-typecheck)

;; Here we propagate very basic type information

(defparameter *closure* (alu.closure:allocate))

(defmacro with-empty-closure (&body body)
  "resets the basic type checking closure"
  `(let ((*closure* (alu.closure:allocate)))
     (declare (ignorable *closure*))
    ,@body))

(-> type-check (spc:expression) spc:type-reference)
(defun type-check (term)
  "Type checks the given spec term. We utilize `*closure*' to
de-reference values, and expect all the values to exist therein"
  (etypecase-of spc:expression term
    (number        (spc:make-type-reference :name :int64))
    (spc:reference (or (alu.closure:lookup *closure* (spc:name term))
                       (error "The reference is undefined in the current scope")))
    (spc:record
     (spc:make-type-reference :name (spc:name term)))
    (spc:application
     (spc:return-type (or (storage:lookup-function #1=(spc:name (spc:func term)))
                          (error (format nil "Function ~A is not defined" #1#)))))
    (spc:let-node
     (type-check (spc:body term)))
    (cons
     (type-check (car (last term))))
    (spc:record-lookup
     (let* ((record      (spc:record term))
            (field       (spc:field term))
            (check       (type-check record))
            (name-check  (etypecase-of spc:type-reference check
                           (spc:reference-type (spc:name check))
                           (spc:application    (spc:name (spc:func check)))))
            (declaration (spc:decl
                          (or (type-storage-to-declaration
                               (storage:lookup-type name-check))
                              (error (format nil "The type ~A does not exist"
                                             name-check))))))
       (etypecase-of spc:type-format declaration
         (spc:sum-decl (error
                        (format nil
                                "Can not do record lookup on enumerated type: ~A"
                                name-check)))
         (spc:record-decl
          (or (sycamore:tree-map-find (spc:contents declaration)
                                      field)
              (error (format
                      nil "Field ~A, is not in type ~A" field name-check)))))))))

(-> type-storage-to-declaration (spc:type-storage) spc:type-declaration)
(defun type-storage-to-declaration (typ)
  (etypecase-of spc:type-storage typ
    (spc:primitive        (spc:make-type-reference :name (spc:name typ)))
    (spc:type-declaration typ)))

(-> add-declaration (keyword spc:type-reference) null)
(defun add-declaration (name type-reference)
  (setf *closure* (alu.closure:insert *closure* name type-reference))
  nil)
