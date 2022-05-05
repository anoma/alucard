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
     (error "not implemented"))
    (spc:application
     (error "not implemented"))))

(-> determine-size-of-storage (spc:type-storage) fixnum)
(defun determine-size-of-storage (storage)
  (etypecase-of spc:type-storage storage
    (spc:primitive        (error "not implemented"))
    (spc:type-declaration (error "not implemented"))))

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

(-> deterine-size-of-declaration (spc:type-declaration) fixnum)
(defun deterine-size-of-declaration (decl)
  (sum (size-of-declaration-contents decl)))
