(in-package :alu.pass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> linearize (ir:circuit) ir:type-aware-list)
(defun linearize (circuit)
  (~> circuit
      eval:evaluate-and-cache-body
      linearize-body))

(-> linearize-body (alu.spec:expression) ir:type-aware-list)
(defun linearize-body (body)
  (~> body
      ;; need to update this point forward
      anf:normalize-expression
      transform-let
      let-all
      return-last-binding))

(-> expand-away-records (ir:expanded-list ir:circuit) ir:fully-expanded-list)
(defun expand-away-records (terms circuit)
  "expand-away-records is responsible for removing all record instances
and properly propagating arguments around them"
  (~> terms
      (relocate-records circuit)
      expand-applications))


(-> filter-redundant-lets (ir:fully-expanded-list) ir:fully-expanded-list)
(defun filter-redundant-lets (xs)
  (let ((map (redundant:find-redundant-lets xs)))
    (~> map
        (redundant:replace-references xs _)
        (redundant:remove-redundant-lets map))))


(-> primitive-circuit (ir:fully-expanded-list ir:circuit) ir:prim-circuit)
(defun primitive-circuit (terms circuit)
  (~>> (ir:make-prim-circuit :name (ir:name circuit) :body terms)
       (fill-in-arguments circuit)
       (fill-in-output    circuit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO :: Make pass that expands away useless lets
;; thus a let :name = :name-calc
;; just rename all instances of :name into :name-calc from that point
;; forth

(-> transform-let (ir:expression) ir:constraint-list)
(defun transform-let (term)
  "transform-let takes a `ir:term' in a flatten form, and removes the
`ir:let-node' for the more flat `ir:bind' type"
  (flet ((transform-let-node (term)
           ;; we keep this type around as it gives us more
           ;; information!
           (with-accessors ((var ir:var) (val ir:value)) term
             (ir:copy-meta term
                           (ir:make-bind :var var :val val)))))
    (etypecase-of ir:expression term
      ;; if it's just the term, or a list as is, then we are good
      (ir:term-type-manipulation (list term))
      (ir:let-node               (list (transform-let-node term)))
      (cons                      (mapcan #'transform-let term))
      (ir:bind-constraint        (list (ir:copy-meta
                                        term
                                        (ir:make-bind-constraint
                                         :var (ir:var term)
                                         :value (mapcan #'transform-let
                                                        (ir:value term)))))))))

(-> return-last-binding (ir:type-aware-list) ir:type-aware-list)
(defun return-last-binding (constraint-list)
  "This transforms the last let into a straight binding, since we aren't
going to be using a let"
  (and constraint-list
       (append (butlast constraint-list)
               (let ((term (car (last constraint-list))))
                 (flet ((mk (term var)
                          (ir:copy-meta term
                                        (ir:make-standalone-ret :var var))))
                   (cons
                    term
                    (etypecase-of ir:expanded-term term
                      (ir:standalone-ret  nil)
                      (ir:bind            (list (mk term (list (ir:var term)))))
                      (ir:bind-constraint (list (mk term (ir:var term)))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removing Extra Type information
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> remove-type-information (ir:type-aware-list) ir:expanded-list)
(defun remove-type-information (awares)
  (labels ((handle-binder (binder)
             (etypecase-of ir:type-aware-term binder
               (ir:standalone-ret  binder)
               (ir:bind            (util:copy-instance
                                    binder
                                    :value (handle-term (ir:value binder))))
               (ir:bind-constraint (util:copy-instance
                                    binder
                                    :value (mapcar #'handle-binder (ir:value binder))))))
           (handle-term (term)
             (etypecase-of ir:term-type-manipulation term
               (ir:term-no-binding term)
               ;; TODO :: COERCION Record --> Number
               (ir:type-manipulation (ir:value term)))))
    (mapcar #'handle-binder awares)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relocation pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> let-all (ir:constraint-list) ir:type-aware-list)
(defun let-all (term-list)
  "This function turns any value which is not the last into a let if it
isn't so already. Perhaps we should make them be an and call instead?"
  (labels ((make-binder (term)
             (ir:copy-meta term
                           (ir:make-bind :var (util:symbol-to-keyword
                                               (gensym "&G"))
                                         :val term)))
           (let-term (term)
             (etypecase-of ir:linear-term term
               (ir:term-type-manipulation      (make-binder term))
               ((or ir:bind ir:standalone-ret) term)
               (ir:bind-constraint
                (ir:copy-meta term
                              (ir:make-bind-constraint
                               :var   (ir:var term)
                               :value (mapcar #'let-term
                                              (ir:value term))))))))
    (mapcar #'let-term term-list)))

(-> relocate-records (ir:expanded-list ir:circuit) relocate:rel)
(defun relocate-records (anf-terms circuit)
  "Relocate records takes a fully anfied term where only the last form
is not a let, and generates out a `ir:expanded-list' along with
it's closure"
  (labels
      ((ingest (rel term)
         (let* ((closure (relocate:rel-closure rel))
                (new-rel (etypecase-of ir:expanded-term term
                           (ir:bind
                            (relocate:relocate-let term closure))
                           (ir:standalone-ret
                            (relocate:make-rel :closure closure :forms (list term)))
                           ;; ASSUME: how would you make records or
                           ;; other things constraints, no need to
                           ;; even think about it!?
                           (ir:bind-constraint
                            (let ((rel (mvfold #'ingest (ir:value term)
                                               (relocate:make-rel :closure closure))))
                              (relocate:make-rel
                               :closure (relocate:rel-closure rel)
                               :forms
                               (list
                                (ir:copy-meta
                                 term
                                 (ir:make-bind-constraint
                                  :var   (ir:var term)
                                  :value (reverse
                                          (relocate:rel-forms rel)))))))))))
           (relocate:make-rel
            :forms   (append (reverse (relocate:rel-forms new-rel))
                             (relocate:rel-forms rel))
            :closure (relocate:rel-closure new-rel)))))
    (let* ((initial (relocate:make-rel :closure
                                       (relocate:initial-closure-from-circuit
                                        circuit)))
           (rel     (reduce #'ingest anf-terms :initial-value initial)))
      ;; since we are reversing the list here, we have to reverse above
      (relocate:make-rel :forms   (reverse (relocate:rel-forms rel))
                         :closure (relocate:rel-closure rel)))))

;; TODO :: Remove the type change here
(-> expand-applications (relocate:rel) ir:fully-expanded-list)
(defun expand-applications (rel)
  (let ((closure (relocate:rel-closure rel)))
    (labels ((update-val (term)
               (if (typep (ir:value term) 'ir:application)
                   (util:copy-instance term :value (expand-app (ir:value term)))
                   term))
             (expand-app (app)
               (util:copy-instance app :arguments (mapcan #'expand-argument
                                                          (ir:arguments app))))
             (expand-argument (arg)
               (etypecase-of ir:term-normal-form arg
                 (number
                  (list arg))
                 (ir:reference
                  (or (mapcar (lambda (x)
                                (ir:copy-meta arg
                                              (ir:make-reference :name x)))
                              (relocate:maps-to (ir:name arg) closure))
                      (list arg)))))
             (expand-term (term)
               (etypecase-of ir:fully-expanded-term term
                 ((or ir:multiple-bind ir:bind)
                  (update-val term))
                 (ir:bind-constraint
                  (ir:copy-meta term
                                (ir:make-bind-constraint
                                 :var   (ir:var term)
                                 :value (mapcar #'expand-term (ir:value term)))))
                 (ir:standalone-ret
                  (ir:copy-meta
                   term
                   (ir:make-standalone-ret
                    :var (mapcan (lambda (x) (or (relocate:maps-to x closure)
                                             (list x)))
                                 (ir:var term))))))))
      (mapcar #'expand-term (relocate:rel-forms rel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove void returns and lets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update logic so that we can get inference on this.
(-> remove-void-bindings (ir:fully-expanded-list) ir:fully-expanded-list)
(defun remove-void-bindings (terms)
  "remove-void-bindings removes any void return value from a function
and direct references to it. It does this by returning a multi-bind
with no names. Note this does not go into other values, so the error
of the user program is preserved."
  ;; we use mutation here just because the fold pattern of trying to
  ;; mimic a map-accuml is just too much against clarity
  (let ((set (sycamore:tree-set #'util:hash-compare)))
    (labels ((value-if-void (term)
               (let ((value (ir:value term)))
                 (cond ((and (typep value 'ir:application)
                             (~> value
                                 ir:func ir:name
                                 storage:lookup-function
                                 ir:return-type
                                 voidp))
                        (mapcar (lambda (x) (sycamore:tree-set-insertf set x))
                                (if (listp (ir:var term))
                                    (ir:var term)
                                    (list (ir:var term))))
                        (ir:copy-meta
                         term
                         (ir:make-multiple-bind :var nil :val (ir:value term))))
                       ((and (typep value 'ir:reference)
                             (or (alu.spec.term-op:void-reference? value)
                                 (sycamore:tree-set-find set (ir:name value))))
                        (sycamore:tree-set-insertf set (ir:var term))
                        nil)
                       (t
                        term))))
             (remove-standalone-ret (term)
               (let ((rets (mapcan (lambda (x)
                                    (if (sycamore:tree-set-find set x)
                                        nil
                                        (list x)))
                                   (ir:var term))))
                 (and rets
                      (ir:make-standalone-ret :var rets)))))
      (filter-map (lambda (term)
                    (etypecase-of ir:fully-expanded-term term
                      ((or ir:bind ir:multiple-bind) (value-if-void term))
                      (ir:standalone-ret             (remove-standalone-ret term))
                      (ir:bind-constraint
                       (ir:copy-meta term
                                     (ir:make-bind-constraint
                                      :var   (ir:var term)
                                      :value (remove-void-bindings
                                              (ir:value term)))))))
                  terms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Circuit Filling logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All these functions should probably use mutation for efficiency
;; reasons, but alas we do a lot of extra copying

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; argument filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> fill-in-arguments (ir:circuit ir:prim-circuit) ir:prim-circuit)
(defun fill-in-arguments (alu-circuit prim-circuit)
  (values
   (util:copy-instance prim-circuit
                       :arguments (~> alu-circuit
                                      expand:full-arguments-from-circuit
                                      expand:argument-names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Type Filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> fill-in-output (ir:circuit ir:prim-circuit) ir:prim-circuit)
(defun fill-in-output (alu-circuit prim-circuit)
  (values
   (util:copy-instance
    prim-circuit
    :returns (determine-output-variables (ir:body prim-circuit)
                                         (ir:return-type alu-circuit)))))

(-> determine-output-variables
    (ir:fully-expanded-list (or ir:type-reference null)) list)
(defun determine-output-variables (body ret)
  "Determines which output variables are returned from a function. If
ret is (`ir:primitive' :void) then an empty list is returned, however
if the value is not void, then the returns in the body are given back"
  (unless (voidp ret)
    (let ((filtered (remove-if-not (lambda (x)
                                     (typep x 'ir:standalone-ret))
                                   body)))
      (mapcan #'ir:var filtered))))

(-> voidp (ir:type-reference) boolean)
(defun voidp (ret)
  (typecase-of ir:type-reference ret
    (ir:reference-type (eq (ir:name ret) :void))
    (ir:application    nil)
    (otherwise         nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming 在蒼白的月光
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> rename-primitive-circuit (ir:prim-circuit) ir:prim-circuit)
(defun rename-primitive-circuit (prim-circ)
  (with-accessors ((name ir:name)    (args ir:arguments)
                   (rets ir:returns) (body ir:body))
      prim-circ
    (values
     (ir:make-prim-circuit :name      (renaming-scheme name)
                            :arguments (mapcar #'renaming-scheme args)
                            :returns   (mapcar #'renaming-scheme rets)
                            :body      (rename-statements body)))))

;; If we do this uniformly to all terms then the references will all
;; be valid!
(-> rename-statements (ir:fully-expanded-list) ir:fully-expanded-list)
(defun rename-statements (stmts)
  (labels ((rename-vars-val (con var)
             (funcall con
                      :val (handle-base (ir:value var))
                      :var (mapcar #'renaming-scheme (ir:var var))))
           (rename-var-val (con var)
             (funcall con
                      :val (handle-base (ir:value var))
                      :var (renaming-scheme (ir:var var))))
           (handle-ref (normal)
             (etypecase-of ir:term-normal-form normal
               (ir:number    normal)
               (ir:reference (ir:make-reference
                               :name (renaming-scheme (ir:name normal))))))
           ;; recursion is fine in binds, as they can't have another
           ;; binding, has to be the `ir:application' or `ir:term-normal-form'
           (handle-term (x)
             (etypecase-of ir:fully-expanded-term x
               (ir:bind            (rename-var-val  #'ir:make-bind x))
               (ir:multiple-bind   (rename-vars-val #'ir:make-multiple-bind x))
               (ir:standalone-ret  (ir:make-standalone-ret
                                     :var (mapcar #'renaming-scheme (ir:var x))))
               (ir:bind-constraint (ir:make-bind-constraint
                                     :var   (mapcar #'renaming-scheme (ir:var x))
                                     :value (rename-statements (ir:value x))))))
           (handle-base (x)
             (etypecase-of ir:base x
               (ir:term-normal-form (handle-ref x))
               (ir:application
                (ir:make-application
                 :function (handle-ref (ir:func x))
                 :arguments (mapcar #'handle-ref (ir:arguments x)))))))
    (mapcar #'handle-term stmts)))

(-> renaming-scheme (symbol) keyword)
(defun renaming-scheme (symb)
  "Renames certain names to be valid for vampir"
  ;; the n here mutates a once only list, so no mutation at all!
  ;; at least after the first substitute
  (intern
   (~>> symb symbol-name
        (substitute #\_ #\-)
        (nsubstitute #\V #\&)
        (nsubstitute #\V #\%))
   :keyword))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Extraction passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Extraction to VAMP-I
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias circuit-to-alias #'extract:circuit-to-alias
  "Turns the circuit to a vamp-ir alias")
