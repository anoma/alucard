(in-package :alu.typechecker.check)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Unification Algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
         (dispatch-case ((value         lookup-type)
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
                  (unless (type-op:int-reference? expected-type)
                    (error "Trying to unify an Integer type with ~A"
                           expected-type)))
                 ;; we should check that we unify it with void properly
                 ((eql :void)
                  (unless (type-op:void-reference? expected-type)
                    (error "Trying to unify a void type with ~A"
                           expected-type)))
                 ((eql :array)
                  (unless (type-op:array-reference? expected-type)
                    (error "Trying to unify an array type with ~A"
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

(-> solve-recursively (keyword ir:type-reference typing-context) typing-context)
(defun solve-recursively (name solved-value context)
  "Solves the given keyword with given type-reference. After solving,
`solve-recursively', will attempt to solve any new variables that were
entailed by the given keyword."
  (let* ((info    (make-type-info :size (size:reference solved-value)
                                  :type solved-value))
         (context (solved name info context))
         ;; IMPORTANT :: We remove the value we solved for otherwise
         ;; we try to solve for it again and error out
         (solved (remove-if (lambda (x) (eql x name))
                            (dependency:get-solved (dependency context)))))
    (labels
        ((solving-current-set (context current-resolve-symbol)
           (let ((hole (closure:lookup (hole-info context)
                                       current-resolve-symbol)))
             (if hole
                 (multiple-value-bind (context solved?)
                     (try-equations current-resolve-symbol hole context)
                   (if solved?
                       context
                       (error "could not solve value ~A"
                              current-resolve-symbol)))
                 (error "How can I solve hole ~A: if no equations exist for it"
                        current-resolve-symbol))))
         (solve-recursive (context symbol-set)
           ;; The dependency module does not remember what values have
           ;; been solved. Thus we need to manually filter out
           ;; previously dealt with values, by looking in our typing
           ;; map and seeing what we've already figured out.
           (let* ((not-solved (remove-if
                               (lambda (x)
                                 (closure:lookup (typing-closure context) x))
                               symbol-set))
                  (context  (mvfold #'solving-current-set not-solved context))
                  (resolved (dependency:get-solved (dependency context))))
             ;; We keep dumping them, eventually we should get through
             ;; them all!
             (if resolved
                 (solve-recursive (dump-solved context) resolved)
                 context))))
      (solve-recursive (dump-solved context) solved))))

(-> try-equations
    (keyword hole-information typing-context)
    (values typing-context boolean))
(defun try-equations (name hole-info context)
  "We try the series of equations until one of them works, once the
first equation that is found working, we stop trying to evaluate the other expressions.
Note that we do not recursively solve, thus the solved list may fill up"
  (mvfold (lambda (context found? term)
            (if found?
                (values context found?)
                (multiple-value-bind (result context)
                    (annotate-term-no-binder term context)
                  (etypecase-of typing-result result
                    (type-info       (values (solved name result context) t))
                    (hole-conditions (values context nil))))))
          (hole-information-term hole-info)
          context
          nil))


(-> type-equality (ir:type-reference-full ir:type-reference-full) boolean)
(defun type-equality (type-1 type-2)
  "Checks that the two types given are equal"
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Refining Partial Information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO :: Fill in the details
(-> refine-hole-with-hole (hole hole typing-context) typing-context)
(defun refine-hole-with-hole (original-hole new-hole-info context)
  "Refines the given hole with the new hole and stores it back into the
typing context."
  original-hole
  new-hole-info
  context)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Updating Typing Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> solved (keyword type-info typing-context) typing-context)
(defun solved (name info context)
  "Updates the `typing-context' with the fact that given name is solved
with the given `type-info'"
  (values
   (make-instance
    'typing-context
    :typing-closure (closure:insert (typing-closure context) name info)
    :holes          (remove-if (lambda (x) (eql x name)) (holes context))
    :hole-info      (closure:remove (hole-info context) name)
    :dependency     (dependency:solved-for (dependency context) name))))
