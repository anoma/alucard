(in-package :alu.typechecker.size)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Determining the Size of the type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> reference (ir:type-reference) integer)
(defun reference (typ)
  (values
    (etypecase-of ir:type-reference typ
      (ir:reference-type
       (let ((lookup (storage:lookup-type (ir:name typ))))
         (if lookup
             (storage lookup)
             (error "type not found: ~A" (ir:name typ)))))
      (ir:application
       (or (primitive typ)
           (error "generics in user defined data type is not supported"))))))

(-> storage (ir:type-storage) integer)
(defun storage (storage)
  (values
   (etypecase-of ir:type-storage storage
     (ir:primitive        (or (primitive storage)
                              (error "type of primitive can not be resolved: ~A"
                                     storage)))
     (ir:type-declaration (declaration storage)))))

(-> contents (ir:type-declaration) list)
(defun contents (decl)
  (let ((format (ir:decl decl)))
    (etypecase-of ir:type-format format
      (ir:record-decl
       (mapcar (lambda (type-name)
                 (~>> type-name
                      (sycamore:tree-map-find (ir:contents format))
                      reference))
               (ir:order format)))
      (ir:sum-decl
       (error "Sum types are not currently supported")))))

(-> declaration (ir:type-declaration) integer)
(defun declaration (decl)
  (assure integer
    (sum (contents decl))))

(-> primitive ((or ir:primitive ir:application)) (or null integer))
(defun primitive (prim?)
  (flet ((handle-arguments (keyword-prim arguments)
           (typecase-of types:known-primitve-types keyword-prim
             ((eql :int)  (if arguments (car arguments) 256))
             ((eql :bool) 1)
             ((eql :void) 0)
             (otherwise   nil))))
    (etypecase-of (or ir:primitive ir:application) prim?
      (ir:primitive   (handle-arguments (ir:name prim?) nil))
      (ir:application (handle-arguments (ir:name (ir:func prim?))
                                        (ir:arguments prim?))))))

