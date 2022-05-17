(in-package :alu.pass.typecheck)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf trivia:*arity-check-by-test-call* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typing structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct type-info
  "Type information of a fully realized type."
  ;; if we don't know the size quite yet it will be nil
  (size nil :type (or fixnum null))
  (type nil :type spc:type-reference))

;; TODO :: with generics we should make this type a lot more rich and
;;         informative
(deftype hole ()
  "Represents the format of holes that have yet to be fully realized."
  `(or null keyword))

(deftype current-information ()
  "Represents the current knowledge we have on a given type. Thus a
hole, or if the type is known a type reference"
  `(or hole spc:type-reference))

(defstruct hole-information
  (unrefined nil :type hole)
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (deftype hole-conditions ()
    "The conditions in which a fialure can happen for "
    `(or same-as
         (eql :refine-integer)
         depends-on)))

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

(deftype lookup-type ()
  "represents the potential type of a reference.

_It can either be_
1. known

2. an unrefined value
  - which we represent with a keyword.
  - TODO :: Once we update with generics, we should move this to a
    `spc:type-reference'

3. unknown
  - which we represent with null."
  `(or type-info hole))

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
           (etypecase-of (or type-info hole-conditions) result
             (type-info
              (util:copy-instance
               ctx
               :typing-closure (closure:insert closure v result)))
             (same-as
              (util:copy-instance
               ctx
               :holes      (cons v holes)
               :dependency (dependency:determined-by
                            dep v (list (same-as-value result)))
               :hole-info  (closure:insert
                            info v
                            ;; here we can cheat and make the
                            ;; hole the same value as the
                            ;; reference itself
                            (make-hole-information
                             :term (list (spc:make-reference
                                          :name (same-as-value result)))))))
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
               :hole-info (closure:insert info
                                          v
                                          (make-hole-information :term (list v)))
               :dependency
               (dependency:determined-by dep v (depends-on-value result))))))))
      ;; The hole will be filed in via the recursive body calls.
      ((spc:bind-constraint :variable introductions :value body)
       (mvfold (flip #'annotate-term)
               body
               (make-starting-hole introductions context))))))

(-> annotate-term-no-binder
    (spc:term-no-binding typing-context)
    (values (or type-info hole-conditions) typing-context))
(defun annotate-term-no-binder (term context)
  "Annotating a term can either end up with the following results:

1. Error
  - If the system is in an invalid state with the binder, an error is
    thrown.

2. `hole-condition'
  - If the system needs more information to fully determine the type a
    `hole-condition' is returned.

3. `type-info'
  - If unification is completely successful, then we get back a
    `type-info'"
  (with-accessors ((holes holes) (info hole-info)
                   (dep dependency) (closure typing-closure))
      context
    (match-of spc:term-no-binding term
      ((number _)
       (values :refine-integer
               context))
      ((spc:reference :name name)
       (let ((lookup (closure:lookup closure name)))
         (values (if lookup
                     lookup
                     (make-same-as :value name))
                 context)))
      ;; Here we have a chance for unification, as we know the type of
      ;; the fields
      ;;
      ;; TODO :: update the code to do unification of the arguments to
      ;;         the record, and thus do CSP on values we now know.
      ((spc:record :name name)
       (let* ((lookup (storage:lookup-type name)))
         (if lookup
             (values (make-type-info
                      :size (determine-size-of-storage lookup)
                      :type (spc:make-type-reference :name name))
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
                          (values (make-type-info
                                   :type lookup
                                   :size (determine-size lookup))
                                  context))))))))
               (lookup
                (error "Record types currently cannot be applied"))
               (t
                (values
                 (make-depends-on :value (list rec))
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
       (match-of (or spc:function-type null) (storage:lookup-function func)
         ((spc:circuit :arguments circ-args :return-type ret)
          (let ((types (mapcar #'spc:typ circ-args)))
            (values (make-type-info
                     :type ret
                     :size (determine-size-of-storage (storage:lookup-function
                                                       func)))
                    ;; this may fail but it'll throw an error
                    (mvfold (lambda (pair context)
                              (unify (car pair) (cdr pair) context))
                            (mapcar #'cons args types)))))
         ((spc:primitive :name name)
          (flet ((handle-all-int-case ()
                   (let ((integer-constraint
                           (find-integer-type-from-args args context)))
                     (mvfold (lambda (context term)
                               (unify term integer-constraint context))
                             args
                             context))))
            (typecase-of known-primitve-functions name
              ((or (eql :*) (eql :+))
               (handle-all-int-case))
              ;; TODO :: Should = be to integer/bool types only? it is a
              ;;         constraint satisfaction, so we could extend it.
              ((eql :=)
               (handle-all-int-case))
              ;; TODO :: We should actually allow different integer
              ;;         types here
              ((eql :exp)
               (handle-all-int-case))
              ;; TODO :: Find a way to type custom user primitives for
              ;;         better interopt.
              (otherwise (error "unknown primitive ~A. Don't know how
                                 to type it."
                                name)))))
         (null
          (error "Function ~A: is not defined" func))))
      ((spc:application :name func)
       (error "Can not apply ~A. Expecting a reference to a function not a number"
              func)))))


(defun make-starting-hole (keywords typing-context)
  (util:copy-instance typing-context
                      :holes (append keywords (holes typing-context))))

(-> unify (spc:term-normal-form current-information typing-context) typing-context)
(defun unify (term expected-type context)
  "unify tries to unify term with expected-type, This can result in
either holes being refined, unknown values being turned into unrefined
values, or an error being thrown if the information is contradictory."
  (flet ((unification-error (type)
           (error "Could not unify Defined type ~A with int" type)))
    (match-of spc:term-normal-form term
      ;; For references we need to check if the reference is known. If
      ;; the term is known, then it is a simple equality check that
      ;; the types agree. Note that if `expected-type' is a `hole'
      ;; then we are trying to unify a known type with less
      ;; information, thus we error (why are we even trying it?!).
      ;;
      ;; If the reference is unknown, then we need to unify it with
      ;; the given type. At this point we can find contradictory
      ;; information, from the partial data we have. If this is the
      ;; case, we should throw and report to the user this issue.
      ;;
      ;; If the unification is with partial information we note the
      ;; partial information as hole information to be resolved later.
      ((spc:reference :name term-name)
       (let ((value (find-type-info term-name context)))
         (dispatch-case  ((value         lookup-type)
                          (expected-type current-information))
           ((type-info hole)
            (error "Internal compiler error. Tried to unify a fully
                   known type ~A with the hole ~A."
                   value expected-type))
           ((type-info spc:type-reference)
            (if (type-equality expected-type (type-info-type value))
                context
                (error "The types ~A and ~A are not equivalent"
                       expected-type
                       (type-info-type value))))
           ((hole spc:type-reference)
            ;; since we assume the only non refined value is `:int'
            ;; then we just need this one check.
            ;;
            ;; TODO :: Rework when we get generics, where the hole can
            ;;         be many other kinds of things
            (if (and value (not (int-reference? expected-type)))
                ;; Since we only have holes on integers, and the value
                ;; is not an integer type, we thus have a unification
                ;; error, where we refine an integer as a non integer.
                (error "Could not refine ~A to an integer type" term-name)
                ;; Here either the hole is not refined and we can
                ;; unify it without remorse, or we are unifying an
                ;; integer hole with an exact integer type.
                (solve-recursively term-name expected-type context)))
            ((hole hole)
             (refine-hole-with-hole value expected-type context)))))
      ;; we only succeed unification if the expected value of this is a
      ;; number
      ((number _)
       (etypecase-of current-information expected-type
         (hole context)
         (spc:type-reference
          (let* ((type-name (etypecase-of spc:type-reference expected-type
                              (spc:application    (spc:name (spc:func expected-type)))
                              (spc:reference-type (spc:name expected-type))))
                 (lookup (storage:lookup-type type-name)))
            (etypecase-of (or null spc:type-storage) lookup
              (spc:type-declaration (unification-error type-name))
              (null                 (error "Type ~A is not defined" type-name))
              (spc:primitive
               (typecase (spc:name lookup)
                 ((or (eql :int) (eql :bool)) context)
                 (otherwise                   (unification-error type-name))))))))))))

(-> type-equality (spc:type-reference-full spc:type-reference-full) boolean)
(defun type-equality (type-1 type-2)
  (dispatch-case ((type-1 spc:type-reference-full)
                  (type-2 spc:type-reference-full))
    ((spc:reference-type spc:reference-type)
     (eq (spc:name type-1) (spc:name type-2)))
    ((spc:application spc:application)
     (every #'type-equality
            (cons (spc:func type-1) (spc:arguments type-1))
            (cons (spc:func type-2) (spc:arguments type-2))))
    ((number number)
     (= type-1 type-2))
    ((* spc:application)
     nil)
    ((* spc:reference-type)
     nil)
    ((* number)
     nil)))

(-> int-reference? (spc:type-reference) boolean)
(defun int-reference? (ref)
  (let* ((name-to-lookup
           (match-of spc:type-reference ref
             ((spc:reference-type :name name)                     name)
             ((spc:application    :name (spc:reference spc:name)) spc:name)))
         (looked (storage:lookup-type name-to-lookup)))
    (etypecase-of (or spc:type-storage null) looked
      ((or null spc:type-declaration) nil)
      (spc:primitive                  (or (= :int (spc:name looked))
                                          (= :bool (spc:name looked)))))))

;; TODO :: Fill in the details
(-> refine-hole-with-hole (hole hole typing-context) typing-context)
(defun refine-hole-with-hole (original-hole new-hole-info context)
  "Refines the given hole with the new hole and stores it back into the
typing context."
  original-hole
  new-hole-info
  context)

(-> solve-recursively (keyword spc:type-reference typing-context) typing-context)
(defun solve-recursively (name solved-value context)
  "Solves the given keyword with given type-reference. After solving,
`solve-recursively', will attempt to solve any new variables that were
entailed by the given keyword."
  name solved-value context
  (error "not implemented yet"))

(-> find-integer-type-from-args (list typing-context) current-information)
(defun find-integer-type-from-args (args context)
  "Finds the most refined integer type in the given argument list. If
the values are contradictory or if the most refined value is not an
integer then it will error."
  (let* ((most-refined-value (find-most-refined-value args context))
         (integer-constraint
           (etypecase-of lookup-type most-refined-value
             (hole
              (etypecase-of hole most-refined-value
                (null (assure hole :int))
                (keyword (if (or (eql most-refined-value :int)
                                 (eql most-refined-value :bool))
                             (assure hole :int)
                             (error "the given type ~A is not an integer type"
                                    most-refined-value)))))
             (type-info
              (if (int-reference?
                   (type-info-type most-refined-value))
                  (type-info-type most-refined-value)
                  (error "Value to should be an Integer not a ~A"
                         (type-info-type most-refined-value)))))))
    (consistent-type-check args context)
    integer-constraint))


(-> normal-form-to-type-info (spc:term-normal-form typing-context) lookup-type)
(defun normal-form-to-type-info (arg context)
  (etypecase-of spc:term-normal-form arg
    (number        (assure hole :int))
    (spc:reference (find-type-info (spc:name arg) context))))

(-> find-integer-type-from-args (list typing-context) lookup-type)
(defun find-most-refined-value (args context)
  (mvfold (lambda (val most-refined-so-far)
            (dispatch-case ((val                 lookup-type)
                            (most-refined-so-far lookup-type))
              ((type-info hole)      val)
              ((type-info type-info) most-refined-so-far) ; these should agree
              ((hole      type-info) most-refined-so-far)
              ((hole      keyword)   most-refined-so-far)
              ((hole      null)      val)))
          (mapcar (lambda (arg) (normal-form-to-type-info arg context)) args)
          (assure hole nil)))

(-> find-integer-type-from-args (list typing-context) t)
(defun consistent-type-check (args context)
  args context
  (error "not implemented yet"))

(-> find-type-info (keyword typing-context) lookup-type)
(defun find-type-info (name context)
  "Grabs the typing value from the given keyword. If this lookup fails,
we try to get the unrefined type."
  (let ((looked (closure:lookup (typing-closure context) name)))
    (cond (looked looked)
          ((member name (holes context))
           (let ((value (closure:lookup (hole-info context) name)))
             (and value (hole-information-unrefined value))))
          (t
           (error "Internal error: Value ~A is not a known hole in ~A"
                  name context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Determining the Size of the type
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> determine-size (spc:type-reference) integer)
(defun determine-size (typ)
  (values
    (etypecase-of spc:type-reference typ
      (spc:reference-type
       (let ((lookup (storage:lookup-type (spc:name typ))))
         (if lookup
             (determine-size-of-storage lookup)
             (error "type not found: ~A" (spc:name typ)))))
      (spc:application
       (or (determine-size-of-primitive typ)
           (error "generics in user defined data type is not supported"))))))

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
