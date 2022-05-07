(in-package :alu.pass.typecheck)

(-> annotate-types (spc:expanded-list closure:typ) closure:typ)
(defun annotate-types (terms type-closure)
  terms
  type-closure)

(-> annotate-type (spc:expanded-term closure:typ) closure:typ)
(defun annotate-type (term type-closure)
  (etypecase-of spc:expanded-term term
    (spc:standalone-ret   type-closure)
    (spc:starting-binders (error "not implemented yet"))))

(-> determine-size (spc:type-reference) fixnum)
(defun determine-size (typ)
  (etypecase-of spc:type-reference typ
    (spc:reference-type
     (let ((lookup (alu.storage:lookup-type (spc:name typ))))
       (if lookup
           (determine-size-of-storage lookup)
           (error "type not found: ~A" (spc:name typ)))))
    (spc:application
     (or (determine-size-of-primitive typ)
         (error "generics in user defined data type is not supported")))))

(-> determine-size-of-storage (spc:type-storage) fixnum)
(defun determine-size-of-storage (storage)
  (etypecase-of spc:type-storage storage
    (spc:primitive        (or (determine-size-of-primitive storage)
                              (error "type of primitive can not be resolved: ~A"
                                     storage)))
    (spc:type-declaration (determine-size-of-declaration storage))))

(-> size-of-declaration-contents (spc:type-declaration) list)
(defun size-of-declaration-contents (decl)
  (let ((format (spc:decl decl)))
    (etypecase-of spc:type-format format
      (spc:record-decl
       (mapcar (lambda (type-name)
                 (~>> type-name
                      (sycamore:tree-map-find (spc:contents format))
                      determine-size))
               (spc:order format)))
      (spc:sum-decl
       (error "Sum types are not currently supported")))))

(-> determine-size-of-declaration (spc:type-declaration) fixnum)
(defun determine-size-of-declaration (decl)
  (sum (size-of-declaration-contents decl)))

(deftype known-primitve-types ()
  `(or (eql :int)
       (eql :bool)
       (eql :void)))

(-> determine-size-of-primitive ((or spc:primitive spc:application)) (or null fixnum))
(defun determine-size-of-primitive (prim?)
  (flet ((handle-arguments (keyword-prim arguments)
           (typecase-of known-primitve-types keyword-prim
             ((eql :int)  (if arguments (car arguments) 256))
             ((eql :bool) 1)
             ((eql :void) 0)
             (otherwise   nil))))
    (etypecase-of (or spc:primitive spc:application) prim?
      (spc:primitive   (handle-arguments (spc:name prim?) nil))
      (spc:application (handle-arguments (spc:name (spc:func prim?))
                                         (spc:arguments prim?))))))
