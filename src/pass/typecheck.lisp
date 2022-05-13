(in-package :alu.pass.typecheck)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typing structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct type-info
  "Type info is the type information we save"
  ;; if we don't know the size quite yet it will be nil
  (size nil :type (or fixnum null))
  (type nil :type spc:type-reference))

(defstruct hole-information
  unrefined
  ;; We should redefine term, to be a list of spc:term-no-binding
  ;; as we can be solved by various sets of equations.
  ;;
  ;; What I mean is that if we have `x = some equation', and then
  ;; later `y = x' where we solve for `y', then we've solved for `x'.
  ;;
  ;; Thus the hole-information should be (list equation #<reference y>)
  ;;
  ;; And when our dependency closure says we've solved it, try the
  ;; list until we get the equation that satisfies the constraint.
  (term nil :type list))


(defclass typing-context ()
  ((holes :initarg :holes
          :accessor holes
          :initform nil
          :type list                    ; list (list keywords)
          :documentation "Represents the holes to solve. List of keywords")
   (hole-info :initarg :hole-info
              :accessor hole-info
              :initform (closure:allocate)
              :type closure:typ         ; Closure:typ hole-information
              :documentation "Represents information about the holes that we know")
   (dependency :initarg :dependency
               :accessor dependency
               :initform (dependency:allocate)
               :type dependency:typ
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
  (value (error "fill in the value") :type keyword))

(defstruct depends-on
  "this represents that the value is tied to the list of values in some
way."
  (value nil :type list))

(deftype known-primitve-types ()
  `(or (eql :int)
       (eql :bool)
       (eql :void)))

(deftype known-primitve-functions ()
  `(or (eql :+)
       (eql :*)
       (eql :=)
       (eql :=)
       (eql :exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotating the Typing context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> annotate-term (spc:expanded-term typing-context) typing-context)
(defun annotate-term (term context)
  (assure typing-context
    (match-of spc:expanded-term term
      ((spc:standalone-ret)
       context)
      ((spc:bind :variable v :value val)
       (multiple-value-bind (result ctx) (annotate-term-no-binder val context)
         (with-accessors ((holes holes) (info hole-info)
                          (dep dependency) (closure typing-closure))
             ctx
           (match-of result result
             ((success :value succ)
              (util:copy-instance ctx
                                  :typing-closure (closure:insert closure v succ)))
             ((err :value err)
              (etypecase-of hole-conditions err
                (same-as
                 (util:copy-instance
                  ctx
                  :holes      (cons v holes)
                  :dependency (dependency:determined-by dep v (list (same-as-value err)))
                  :hole-info  (closure:insert
                               info v
                               ;; here we can cheat and make the
                               ;; hole the same value as the
                               ;; reference itself
                               (make-hole-information
                                :term (list (spc:make-reference
                                             :name (same-as-value err)))))))
                ;; here we have an integer type, but what size of
                ;; integer, we need to refine on this!
                ((eql :refine-integer)
                 (util:copy-instance
                  ctx
                  :holes     (cons v holes)
                  :hole-info (closure:insert info v
                                             (make-hole-information :unrefined :int
                                                                    :term (list val)))))
                ;; Here we keep the original expression, and just
                ;; note what holes need to be solved first before we
                ;; can continue.
                (depends-on
                 (util:copy-instance
                  ctx
                  :holes     (cons v holes)
                  :hole-info (closure:insert info v (make-hole-information :term (list v)))
                  :dependency
                  (dependency:determined-by dep v (depends-on-value err))))))))))
      ((spc:bind-constraint :variable introductions :value body)
       body
       (make-starting-hole introductions context)
       (error "step not implemented yet")))))

(-> annotate-term-no-binder
    (spc:term-no-binding typing-context)
    (values result typing-context))
(defun annotate-term-no-binder (term context)
  (with-accessors ((holes holes) (info hole-info)
                   (dep dependency) (closure typing-closure))
      context
    (match-of spc:term-no-binding term
      ((number _)
       (values (make-err :value :refine-integer)
               context))
      ((spc:reference :name name)
       (let ((lookup (closure:lookup closure name)))
         (values (if lookup
                     (make-success :value lookup)
                     (make-err     :value (make-same-as :value name)))
                 context)))
      ;; Here we have a chance for unification, as we know the type of
      ;; the fields
      ;;
      ;; TODO :: update the code to do unification of the arguments to
      ;;         the record, and thus do CSP on values we now know.
      ((spc:record :name name)
       (let* ((lookup (storage:lookup-type name)))
         (if lookup
             (values (make-success :value (make-type-info
                                           :size (determine-size-of-storage lookup)
                                           :type (spc:make-type-reference :name name)))
                     context)
             (error "the record type ~A: is not defined" name))))
      ;; This case isn't hard, just mostly tedious. All we have to do
      ;; is get the field of the record. Which can be done
      ;; mechanically...
      ;;
      ;; TODO :: Abstract out the mechanical lookup from record-lookup
      ;;         to field type.
      ;;
      ;; Further if the record isn't known yet, that's fine, add the
      ;; record reference as a dependency and solve again after we
      ;; unify.
      ((spc:record-lookup :record rec :field field)
       (let* ((rec (etypecase-of spc:term-normal-form rec
                     (number (error "can't index into a numerical literal: ~A"
                                    rec))
                     (spc:reference (spc:name rec))))
              (lookup (closure:lookup closure rec)))
         (cond ((and lookup (typep (type-info-type lookup) 'spc:reference-type))
                (let* ((field-name (spc:name (type-info-type lookup)))
                       (lookup     (storage:lookup-type field-name)))
                  (typecase-of spc:type-storage lookup
                    (spc:primitive
                     (error "~A is a primitive type not a record type" field-name))
                    (otherwise
                     (error "type ~A does not exist" field-name))
                    (spc:type-declaration
                     (etypecase-of spc:type-format (spc:decl lookup)
                       (spc:sum-decl
                        (error "Trying to index into the sum type ~A" field-name))
                       (spc:record-decl
                        (let ((lookup (sycamore:tree-map-find
                                       (spc:contents (spc:decl lookup))
                                       field)))
                          (values (make-success
                                   :value (make-type-info
                                           :type lookup
                                           :size (determine-size lookup)))
                                  context))))))))
               (lookup
                (error "Record types currently cannot be applied"))
               (t
                (values
                 (make-err :value (make-depends-on :value (list rec)))
                 context)))))
      ;; This is an interesting case. We know the types of functions,
      ;; in fact we even know the type of primitives! This opens up
      ;; the can of worms of unification and type checking.
      ;;
      ;; If the function is a known user gate, then we can unify the
      ;; arguments type wise and even report proper error messages for
      ;; typing errors.
      ;;
      ;; If the function is a primitive, then our job gets a bit more
      ;; complex. Functions like `+' and `*' can be applied to many
      ;; different integer types, however since we want explicit
      ;; casting (thus and int8 can only be added to another int8 and
      ;; not an int16), all arguments must be of the same
      ;; type. Further if we add only constants. (+ 12 35), we don't
      ;; know the type of the addition until it is used
      ;; elsewhere. Thus we build up more constraints to be solved.
      ((spc:application :name (spc:reference :name func) :arguments args)
       args
       (match-of (or spc:function-type null) (storage:lookup-function func)
         ((spc:circuit :arguments args :return-type ret)
          (let ((types (mapcar #'spc:typ args)))
            (values (make-success :value ret)
                    ;; this may fail but it'll throw an error
                    (mvfold (lambda (pair context)
                              (unify (car pair) (cdr pair) context))
                            (mapcar #'cons args types)))))
         ((spc:primitive :name name)
          (typecase-of known-primitve-functions name
            ((or (eql :*) (eql :+))
             (error "not implemented yet"))
            ((eql :=) (error "not implemented yet"))
            ((eql :exp) (error "not implemented yet"))
            (otherwise (error "not implemented yet"))))
         (null
          (error "Function ~A: is not defined" func))))
      ((spc:application :name func)
       (error "Can not apply ~A. Expecting a reference to a function not a number"
              func)))))

(defun make-starting-hole (keywords typing-context)
  (util:copy-instance typing-context
                      :holes (append keywords (holes typing-context))))

(-> unify (spc:term-normal-form spc:type-reference typing-context) typing-context)
(defun unify (term expected-type context)
  "unify tries to unify term with expected-type, This can result in
either holes being refined, or an error being thrown if the
information is contradictory."
  term expected-type
  context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Determining the Size of the type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> determine-size (spc:type-reference) integer)
(defun determine-size (typ)
  (etypecase-of spc:type-reference typ
    (spc:reference-type
     (let ((lookup (storage:lookup-type (spc:name typ))))
       (if lookup
           (determine-size-of-storage lookup)
           (error "type not found: ~A" (spc:name typ)))))
    (spc:application
     (or (determine-size-of-primitive typ)
         (error "generics in user defined data type is not supported")))))

(-> determine-size-of-storage (spc:type-storage) integer)
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

(-> determine-size-of-declaration (spc:type-declaration) integer)
(defun determine-size-of-declaration (decl)
  (assure integer
    (sum (size-of-declaration-contents decl))))

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
