(in-package :alu.typechecker)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotating the Typing context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> annotate-term (ir:expanded-term typing-context) typing-context)
(defun annotate-term (term context)
  (assure typing-context
    (match-of ir:expanded-term term
      ((ir:standalone-ret)
       context)
      ((ir:bind :variable v :value val)
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
                             :term (list (ir:make-reference
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
      ((ir:bind-constraint :variable introductions :value body)
       (mvfold (flip #'annotate-term)
               body
               (make-starting-hole introductions context))))))

(-> annotate-term-no-binder
    (ir:term-no-binding typing-context)
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
    (match-of ir:term-no-binding term
      ((number _)
       (values :refine-integer
               context))
      ((ir:reference :name name)
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
      ((ir:record :name name)
       (let* ((lookup (storage:lookup-type name)))
         (if lookup
             (values (make-type-info
                      :size (size:storage lookup)
                      :type (ir:make-type-reference :name name))
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
      ((ir:record-lookup :record rec :field field)
       (let* ((rec (etypecase-of ir:term-normal-form rec
                     (number (error "can't index into a numerical literal: ~A"
                                    rec))
                     (ir:reference (ir:name rec))))
              (lookup (closure:lookup closure rec)))
         (cond ((and lookup (typep (type-info-type lookup) 'ir:reference-type))
                (let* ((field-name (ir:name (type-info-type lookup)))
                       (lookup     (storage:lookup-type field-name)))
                  (typecase-of ir:type-storage lookup
                    (ir:primitive
                     (error "~A is a primitive type not a record type" field-name))
                    (otherwise
                     (error "type ~A does not exist" field-name))
                    (ir:type-declaration
                     (etypecase-of ir:type-format (ir:decl lookup)
                       (ir:sum-decl
                        (error "Trying to index into the sum type ~A" field-name))
                       (ir:record-decl
                        (let ((lookup (sycamore:tree-map-find
                                       (ir:contents (ir:decl lookup))
                                       field)))
                          (values (make-type-info
                                   :type lookup
                                   :size (size:reference lookup))
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
      ((ir:application :name (ir:reference :name func) :arguments args)
       (match-of (or ir:function-type null) (storage:lookup-function func)
         ((ir:circuit :arguments circ-args :return-type ret)
          (let ((types (mapcar #'ir:typ circ-args)))
            (values (make-type-info
                     :type ret
                     :size (size:storage (storage:lookup-function func)))
                    ;; this may fail but it'll throw an error
                    (mvfold (lambda (pair context)
                              (unify (car pair) (cdr pair) context))
                            (mapcar #'cons args types)))))
         ((ir:primitive :name name)
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
      ((ir:application :name func)
       (error "Can not apply ~A. Expecting a reference to a function not a number"
              func)))))


(defun make-starting-hole (keywords typing-context)
  (util:copy-instance typing-context
                      :holes (append keywords (holes typing-context))))

(-> unify (ir:term-normal-form current-information typing-context) typing-context)
(defun unify (term expected-type context)
  "unify tries to unify term with expected-type, This can result in
either holes being refined, unknown values being turned into unrefined
values, or an error being thrown if the information is contradictory."
  (flet ((unification-error (type)
           (error "Could not unify Defined type ~A with int" type)))
    (match-of ir:term-normal-form term
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
      ((ir:reference :name term-name)
       (let ((value (find-type-info term-name context)))
         (dispatch-case  ((value         lookup-type)
                          (expected-type current-information))
           ((type-info hole)
            (error "Internal compiler error. Tried to unify a fully
                   known type ~A with the hole ~A."
                   value expected-type))
           ((type-info ir:type-reference)
            (if (type-equality expected-type (type-info-type value))
                context
                (error "The types ~A and ~A are not equivalent"
                       expected-type
                       (type-info-type value))))
           ((hole ir:type-reference)
            (etypecase-of hole value
              (null t)
              (keyword
               (typecase-of known-primitve-types value
                 ((or (eql :int)
                      (eql :bool))
                  (unless (int-reference? expected-type)
                    (error "Trying to unify an Integer type with ~A"
                           expected-type)))
                 ;; we should check that we unify it with void properly
                 ((eql :void)
                  (unless (void-reference? expected-type)
                    (error "Trying to unify a void type with ~A"
                           expected-type)))
                 (otherwise
                  (error "Unknown primitive type ~A" value)))))
            (solve-recursively term-name expected-type context))
            ((hole hole)
             (refine-hole-with-hole value expected-type context)))))
      ;; we only succeed unification if the expected value of this is a
      ;; number
      ((number _)
       (etypecase-of current-information expected-type
         (hole context)
         (ir:type-reference
          (let* ((type-name (etypecase-of ir:type-reference expected-type
                              (ir:application    (ir:name (ir:func expected-type)))
                              (ir:reference-type (ir:name expected-type))))
                 (lookup (storage:lookup-type type-name)))
            (etypecase-of (or null ir:type-storage) lookup
              (ir:type-declaration (unification-error type-name))
              (null                 (error "Type ~A is not defined" type-name))
              (ir:primitive
               (typecase (ir:name lookup)
                 ((or (eql :int) (eql :bool)) context)
                 (otherwise                   (unification-error type-name))))))))))))

(-> type-equality (ir:type-reference-full ir:type-reference-full) boolean)
(defun type-equality (type-1 type-2)
  (dispatch-case ((type-1 ir:type-reference-full)
                  (type-2 ir:type-reference-full))
    ((ir:reference-type ir:reference-type)
     (eq (ir:name type-1) (ir:name type-2)))
    ((ir:application ir:application)
     (every #'type-equality
            (cons (ir:func type-1) (ir:arguments type-1))
            (cons (ir:func type-2) (ir:arguments type-2))))
    ((number number)
     (= type-1 type-2))
    ((* ir:application)
     nil)
    ((* ir:reference-type)
     nil)
    ((* number)
     nil)))

(-> is-primitive? (ir:type-reference (-> (ir:primitive) boolean)) boolean)
(defun is-primitive? (ref predicate)
  (let* ((name-to-lookup
           (match-of ir:type-reference ref
             ((ir:reference-type :name name)                     name)
             ((ir:application    :name (ir:reference ir:name)) ir:name)))
         (looked (storage:lookup-type name-to-lookup)))
    (etypecase-of (or ir:type-storage null) looked
      ((or null ir:type-declaration) nil)
      (ir:primitive                  (funcall predicate looked)))))

(-> void-reference? (ir:type-reference) boolean)
(defun void-reference? (ref)
  (is-primitive? ref (lambda (v) (eql :void (ir:name v)))))

(-> int-reference? (ir:type-reference) boolean)
(defun int-reference? (ref)
  (is-primitive? ref (lambda (v)
                       (or (eql :int (ir:name v))
                           (eql :bool (ir:name v))))))

;; TODO :: Fill in the details
(-> refine-hole-with-hole (hole hole typing-context) typing-context)
(defun refine-hole-with-hole (original-hole new-hole-info context)
  "Refines the given hole with the new hole and stores it back into the
typing context."
  original-hole
  new-hole-info
  context)

(-> solve-recursively (keyword ir:type-reference typing-context) typing-context)
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


(-> normal-form-to-type-info (ir:term-normal-form typing-context) lookup-type)
(defun normal-form-to-type-info (arg context)
  (etypecase-of ir:term-normal-form arg
    (number        (assure hole :int))
    (ir:reference (find-type-info (ir:name arg) context))))

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
  (flet ((keyword-case (type keyword)
           (etypecase-of known-primitve-types keyword
             ((or (eql :int) (eql :bool))
              (if (int-reference? (type-info-type type))
                  type
                  (error "Type ~A is not consistent with Integer"
                         (type-info-type type))))
             ((eql :void)
              (error "The value void should not be used as an argument")))))
    (mvfold (lambda (val current-most-known-type)
              (dispatch-case ((val                     lookup-type)
                              (current-most-known-type lookup-type))
                ((*         null)      val)
                ((null      *)         current-most-known-type)
                ((type-info keyword)   (keyword-case val current-most-known-type))
                ((keyword   type-info) (keyword-case current-most-known-type val))
                ((keyword   keyword)   (if (eql val current-most-known-type)
                                           val
                                           (error "Values ~A and ~A are not consistent"
                                                  val current-most-known-type)))
                ((type-info type-info) (type-equality (type-info-type val)
                                                      (type-info-type
                                                       current-most-known-type)))))
            (mapcar (lambda (arg) (normal-form-to-type-info arg context)) args)
            (assure hole nil))))

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
