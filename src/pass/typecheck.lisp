(in-package :alu.pass.typecheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typing structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct type-info
  "Type info is the type information we save"
  ;; if we don't know the size quite yet it will be nil
  (size nil :type (or fixnum null))
  (type nil :type (or list keyword)))

(defstruct hole-information
  unrefined
  (term (error "please supply value") :type spc:term-no-binding))

(defclass typing-context ()
  ((holes :initarg :holes
          :accessor holes
          :initform nil
          :type list                    ; list keywords
          :documentation "Represents the holes to solve. List of keywords")
   (hole-info :initarg :hole-info
              :accessor hole-info
              :initform (closure:allocate)
              :type closure:typ         ; Closure:typ (list keyword)
              :documentation "Represents information about the holes that we know")
   (dependency :initarg :dependency
               :accessor dependency
               :initform (closure:allocate)
               :type closure:typ        ; Closure:typ list
               :documentation "Represents the constraint satisfaction mapping")
   (typing-closure :initarg :typing-closure
                   :accessor typing-closure
                   :initform (closure:allocate)
                   :type closure:typ    ; Closure:typ type-info
                   :documentation
                   "This is the typing closure for terms which we already know"))
  (:documentation "This represents the typing context the terms we are analyzing belong
in"))

(defmethod print-object ((obj typing-context) stream)
  (print-unreadable-object (obj stream :type t)
    (pprint-logical-block (stream nil)
        (format stream ":HOLES ~A~_:HOLE-INFO ~A~_:DEPENDENCY ~A~_:TYPING-CLOSURE ~A"
                (holes obj) (hole-info obj) (dependency obj) (typing-closure obj)))))

(deftype result ()
  "the either monad from Haskell."
  `(or err success))

(defstruct err
  "Represents a computation which has failed in some way"
  value)

(defstruct success
  "Represents a computation which has succeeded in some way"
  value)

(deftype hole-conditions ()
  "The conditions in which a fialure can happen for "
  `(or same-as
       (eql :refine-integer)
       depends-on))

(defstruct same-as
  "Represents that the hole is the same as this other variable"
  (value (error "fill in the value") :type :keyword))

(defstruct depends-on
  "this represents that the value is tied to the list of values in some
way."
  (value nil :type list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotating the Typing context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> annotate-term (spc:expanded-term typing-context) typing-context)
(defun annotate-term (term context)
  (etypecase-of spc:expanded-term term
    (spc:standalone-ret  context)
    (spc:bind
     (multiple-value-bind (result ctx) (annotate-term-no-binder (spc:value term)
                                                                context)
       (etypecase-of result result
         (success (util:copy-instance
                   context
                   :typing-closure (closure:insert (typing-closure ctx)
                                                   (spc:var term)
                                                   (success-value result))))
         (err (let ((err (err-value result)))
                (etypecase-of hole-conditions err
                  (same-as
                   (util:copy-instance
                    ctx
                    :holes (cons (spc:var term) (holes ctx))
                    :hole-info
                    (closure:insert (hole-info ctx)
                                    (spc:var term)
                                    ;; here we can cheat and make the
                                    ;; hole the same value as the
                                    ;; reference itself
                                    (make-hole-information
                                     :term (spc:make-reference
                                            :name (same-as-value err))))
                    :dependency
                    (closure:insert (dependency ctx)
                                    (spc:var term)
                                    (list (same-as-value err)))))
                  ;; here we have an integer type, but what size of
                  ;; integer, we need to refine on this!
                  ((eql :refine-integer) (error "step not implemented yet"))
                  ;; Here we keep the original expression, and just
                  ;; note what holes need to be solved first before we
                  ;; can continue.
                  (depends-on            (error "step not implemented yet"))))))))
    (spc:bind-constraint
     (make-starting-hole (spc:var term) context)
     (error "step not implemented yet"))))

(-> annotate-term-no-binder
    (spc:term-no-binding typing-context)
    (values result typing-context))
(defun annotate-term-no-binder (term context)
  (with-accessors ((holes holes) (info hole-info)
                   (dep dependency) (closure typing-closure))
      context
    (etypecase-of spc:term-no-binding term
      (spc:reference
       (let ((lookup (closure:lookup closure (spc:name term))))
         (values (if lookup
                     (make-success :value lookup)
                     (make-err     :value (make-same-as :value (spc:name term))))
                 context)))
      (number
       (values (make-err :value :refine-integer)
               context))
      (spc:application   (error "not implemented yet"))
      (spc:record
       (let* ((name (spc:name term))
              (lookup (alu.storage:lookup-type name)))
         (if lookup
             (values (make-success :value (make-type-info
                                           :size (determine-size-of-storage lookup)
                                           :type name))
                     context)
             (error "the record type ~A: is not defined" name))))
      (spc:record-lookup (error "not implemented yet")))))

(defun make-starting-hole (keywords typing-context)
  (util:copy-instance typing-context
                      :holes (append keywords (holes typing-context))))

(defun unify (term expected-type context)
  term expected-type
  context)

(defmethod type-of? ((term spc:bind) &optional (closure (closure:allocate)))
  (multiple-value-bind (closure type) (type-of? (spc:value term) closure)
    (values (closure:insert closure (spc:var term) type)
            type)))

(defmethod type-of? ((term spc:reference) &optional (closure (closure:allocate)))
  (values closure
          (closure:lookup closure (spc:name term))))

(defmethod type-of? ((term number) &optional (closure (closure:allocate)))
  (values closure
          (make-type-info :size (integer-length term)
                          :type :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Determining the Size of the type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
