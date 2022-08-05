(in-package :alu.typechecker.check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Annotating the Typing context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> check (ir:type-aware-list ir:circuit) (values ir:expanded-list typing-context))
(defun check (body circuit)
  (values
   body
   (annotate-circuit circuit body)))

(-> annotate-circuit (ir:circuit ir:expanded-list) typing-context)
(defun annotate-circuit (circuit body)
  (annotate-list body (starting-context (ir:arguments circuit))))

(-> starting-context (list) typing-context)
(defun starting-context (constraint-list)
  (mvfold
   (lambda (ctx constraint)
     ;; update when we get no types for circuit arguments
     (util:copy-instance
      ctx
      :typing-closure (closure:insert (typing-closure ctx)
                                      (ir:name constraint)
                                      (make-type-info
                                       :size (size:reference (ir:typ constraint))
                                       :type (ir:typ constraint)))))
   constraint-list
   (make-instance 'typing-context)))

(-> annotate-list (ir:type-aware-list typing-context) typing-context)
(defun annotate-list (body context)
  (mvfold (flip #'annotate-term) body context))

(-> annotate-term (ir:type-aware-term typing-context) typing-context)
(defun annotate-term (term context)
  (assure typing-context
    (match-of ir:type-aware-term term
      ((ir:standalone-ret)
       context)
      ((ir:bind :variable v :value val)
       (multiple-value-bind (result ctx) (annotate-term-no-binder val context)
         (with-accessors ((holes holes) (info hole-info)
                          (dep dependency) (closure typing-closure))
             ctx
           (etypecase-of typing-result result
             (type-info
              (util:copy-instance
               ctx
               :typing-closure (closure:insert closure v result)))
             (same-as
              (all-mutual (list (ir:copy-meta term (ir:make-reference :name v))
                                (ir:copy-meta val (ir:make-reference
                                                   :name (same-as-value result))))
                          (util:copy-instance ctx :holes (cons v holes))))
             ;; here we have an integer type, but what size of
             ;; integer, we need to refine on this!
             ((eql :refine-integer)
              (util:copy-instance
               ctx
               :holes     (cons v holes)
               :hole-info (closure:insert info v
                                          (make-hole-information :unrefined :int
                                                                 :term (list val)))))
             ((eql :refine-array)
              (util:copy-instance
               ctx
               :holes     (cons v holes)
               :hole-info (closure:insert info v
                                          (make-hole-information :term (list val)))))
             ;; Here we keep the original expression, and just
             ;; note what holes need to be solved first before we
             ;; can continue.
             (depends-on
              (util:copy-instance
               ctx
               :holes     (cons v holes)
               :hole-info (closure:insert info
                                          v
                                          (make-hole-information :term (list val)))
               :dependency
               (dependency:determined-by dep v (depends-on-value result))))))))
      ;; The hole will be filed in via the recursive body calls.
      ((ir:bind-constraint :variable introductions :value body)
       (mvfold (flip #'annotate-term)
               body
               (make-starting-hole introductions context))))))

(-> annotate-term-no-binder
    (ir:term-type-manipulation typing-context)
    (values typing-result typing-context))
(defun annotate-term-no-binder (term context)
  "Annotating a term can either end up with the following results:

1. Error
  - If the system is in an invalid state with the binder, an error is
    thrown.

2. `hole-conditions'
  - If the system needs more information to fully determine the type a
    `hole-conditions' is returned.

3. `type-info'
  - If unification is completely successful, then we get back a
    `type-info'"
  (with-accessors ((holes holes) (info hole-info)
                   (dep dependency) (closure typing-closure))
      context
    (match-of ir:term-type-manipulation term
      ((number _)
       (values :refine-integer
               context))
      ((ir:reference :name name)
       (let ((lookup (closure:lookup closure name)))
         (values (cond (lookup
                        lookup)
                       ((term-op:void-reference? term)
                        (make-type-info :size 0
                                        :type (ir:make-type-reference :name :void)))
                       (t
                        (make-same-as :value name)))
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
             (log:error term '(:type :not-defined)
                         "the record type ~A: is not defined" name))))
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
         ;; replace with typecase-of... what are you doing, me
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
                        (let ((field-type (sycamore:tree-map-find
                                           (ir:contents (ir:decl lookup))
                                           field)))
                          (if field-type
                              (values (make-type-info
                                       :type field-type
                                       :size (size:reference field-type))
                                      context)
                              (error "the field ~A does not exist in record type: ~A"
                                     field-name (ir:name lookup))))))))))
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
                     :size (size:reference ret))
                    ;; this may fail but it'll throw an error
                    (mvfold (lambda (context pair)
                              (unify (car pair) (cdr pair) context))
                            (mapcar #'cons args types)
                            context))))
         ((ir:primitive :name name)
          (flet ((handle-all-int-case ()
                   (arguments-have-same-type args
                                             context
                                             #'find-integer-type-from-args)))
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
              func))
      ;; we actually ignore the value for type checking as we just
      ;; care about type information.
      ((ir:type-coerce :typ typ)
       (values (make-type-info :size (size:reference typ)
                               :type typ)
               context))
      ;; In this case we just unify the value against the type, and
      ;; return the type.
      ((ir:type-check :value value :typ typ)
       (values
        (make-type-info :size (size:reference typ)
                        :type typ)
        (unify value typ context)))
      ((ir:array-lookup :arr arr)
       (let* ((lookup
                (etypecase-of ir:term-normal-form arr
                  (number (error "Array lookup on the number ~A" arr))
                  (ir:reference (closure:lookup closure (ir:name arr)))))
              (value-type
                (cond ((and lookup (type-op:array-reference? (type-info-type lookup)))
                       (let ((type
                               (ir:array-type-content (type-info-type lookup))))
                         (make-type-info :type type
                                         :size (size:reference type))))
                      (lookup
                       (error "Type ~A is not of type Array" (type-info-type lookup)))
                      (t nil))))
         (values
          (if lookup
              value-type
              (make-depends-on :value (list (ir:name arr))))
          context)))
      ((ir:array-set :arr arr :value value)
       (let* ((lookup-val (normal-form-to-type-info value context))
              (lookup-arr (normal-form-to-type-info-not-number-err arr context))
              (context
                (dispatch-case ((lookup-arr lookup-type)
                                (lookup-val lookup-type))
                  ;; We will leave some holes, as currently we force the
                  ;; submission of the type with an array. This should
                  ;; change, and we should be able to understand the length
                  ;; of a hole
                  ((hole type-info)
                   ;; can't actually hit here!
                   (error "No Type Inference for the type of the array yet!"))
                  ((hole hole)
                   ;; can't actually hit here!
                   (error "No Type Inference for the type of the array yet!"))
                  ((type-info type-info)
                   (if (type-equality (ir:array-type-content
                                       (type-info-type lookup-arr))
                                      (type-info-type lookup-val))
                       context
                       (error "Array ~A does not have element type of ~A"
                              arr lookup-val)))
                  ((type-info hole)
                   (unify value
                          (ir:array-type-content (type-info-type lookup-arr))
                          context)))))
         (values (make-type-info :size 0
                                 :type (ir:make-type-reference :name :void))
                 context)))
      ((ir:array-allocate :size s :typ ty)
       ;; abstract out this make array
       (values (make-type-info
                :size (* s (size:reference ty))
                :type (ir:array-type :length s :type ty))
               context))
      ((ir:from-data :contents contents)
       (multiple-value-bind (arg context) (arguments-have-same-type contents context)
         (etypecase-of typing-result arg
           (type-info
            (values (make-type-info
                     :size (* (length contents)
                              (type-info-size arg))
                     :type (ir:array-type :length (length contents)
                                          :type (type-info-type arg)))
                    context))
           (depends-on            (values arg context))
           ((eql :refine-array)   (values arg context))
           (same-as               (values (make-depends-on
                                           :value (list (same-as-value arg)))
                                          context))
           ((eql :refine-integer) (values :refine-integer context))))))))


(-> arguments-have-same-type
    (list typing-context &optional function)
    (values typing-result typing-context))
(defun arguments-have-same-type (args context &optional
                                                (refine-f #'find-most-refined-value))
  "This function handles the case when the arguments have all the same
type. This occurs often in primitives. This function is passed the
argument list, the context, and a refine function that lets us specify
any constraints on the specification"
  (let* ((constraints
           (funcall refine-f args context))
         (argument-keywords
           (references-from-list args)))
    (consistent-type-check args context)
    (etypecase-of lookup-type constraints
      (hole
       (values (if argument-keywords
                   (make-same-as :value (car argument-keywords))
                   :refine-integer)
               (all-mutual args context)))
      (type-info
       (values constraints
               (mvfold (lambda (context term)
                         (unify term (type-info-type constraints)
                                context))
                       args
                       context))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hole Information Filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> all-mutual (list typing-context) typing-context)
(defun all-mutual (normal-forms context)
  "Updates the context to note that all the values entail each other"
  (let ((argument-keywords (references-from-list normal-forms)))
    (values
     (util:copy-instance
      context
      :hole-info  (mutual-holes normal-forms (hole-info context))
      :dependency (dependency:determine-each-other (dependency context)
                                                   argument-keywords)))))

(-> mutual-holes (list closure:typ) closure:typ)
(defun mutual-holes (normal-forms hole-map)
  "The normal form values all entail each other, and thus we"
  (mvfold (lambda (hole-closure normal)
            (etypecase-of ir:term-normal-form normal
              (number hole-closure)
              (ir:reference
               (let ((hole (closure:lookup hole-closure (ir:name normal)))
                     (removed (remove-if (lambda (x) (eql x normal)) normal-forms)))
                 (closure:insert hole-closure
                                 (ir:name normal)
                                 (mvfold #'add-hole-formula
                                         removed
                                         (if hole
                                             hole
                                             (make-hole-information))))))))
          normal-forms
          hole-map))


(defun make-starting-hole (keywords typing-context)
  (util:copy-instance typing-context
                      :holes (append keywords (holes typing-context))))

(-> dump-solved (typing-context) typing-context)
(defun dump-solved (context)
  (values
   (util:copy-instance context
                       :dependency (dependency:dump-solved
                                    (dependency context)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Normal Form/Reference/Keywords Lookups
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> normal-form-to-type-info-not-number-err (ir:term-normal-form typing-context)
    lookup-type)
(defun normal-form-to-type-info-not-number-err (arg context)
  (etypecase-of ir:term-normal-form arg
    (number       (error "Value is a number when not expected"))
    (ir:reference (find-type-info (ir:name arg) context))))

(-> normal-form-to-type-info (ir:term-normal-form typing-context) lookup-type)
(defun normal-form-to-type-info (arg context)
  (etypecase-of ir:term-normal-form arg
    (number       (assure hole :int))
    (ir:reference (find-type-info (ir:name arg) context))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refining over a list of types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> find-integer-type-from-args (list typing-context) lookup-type)
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
                             most-refined-value
                             (error "the given type ~A is not an integer type"
                                    most-refined-value)))))
             (type-info
              (if (type-op:int-reference?
                   (type-info-type most-refined-value))
                  most-refined-value
                  (error "Value to should be an Integer not a ~A"
                         (type-info-type most-refined-value)))))))
    integer-constraint))

(-> references-from-list (list) list)
(defun references-from-list (normal-forms)
  "Turns a list of normal forms into the list of their keywords"
  (filter-map (lambda (x)
                (etypecase-of ir:term-normal-form x
                  (number nil)
                  (ir:reference (ir:name x))))
              normal-forms))

(-> find-most-refined-value (list typing-context) lookup-type)
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

(-> consistent-type-check (list typing-context) (or t null))
(defun consistent-type-check (args context)
  (flet ((keyword-case (type keyword)
           (etypecase-of known-primitve-types keyword
             ((or (eql :int) (eql :bool))
              (if (type-op:int-reference? (type-info-type type))
                  type
                  (error "Type ~A is not consistent with Integer"
                         (type-info-type type))))
             ((eql :array)
              (if (type-op:array-reference? (type-info-type type))
                  type
                  (error "Type ~A is not consistent with Integer"
                         (type-info-type type))))
             ((eql :void)
              (error "The value void should not be used as an argument")))))
    (mvfold (lambda (current-most-known-type val)
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
                ((type-info type-info)
                 (if (type-equality (type-info-type val)
                                    (type-info-type
                                     current-most-known-type))
                     current-most-known-type
                     (error "Values ~A and ~A are not consistent"
                                                  val current-most-known-type)))))
            (mapcar (lambda (arg) (normal-form-to-type-info arg context)) args)
            (assure hole nil))))
