(in-package :alu.pass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> linearize (spc:circuit) spc:expanded-list)
(defun linearize (circuit)
  (~> circuit
      eval:evaluate-and-cache-body
      ;; need to update this point forward
      anf:normalize-expression
      transform-let
      let-all
      return-last-binding))

(-> expand-away-records (spc:expanded-list spc:circuit) spc:fully-expanded-list)
(defun expand-away-records (terms circuit)
  "expand-away-records is responsible for removing all record instances
and properly propagating arguments around them"
  (~> terms
      (relocate-records circuit)
      expand-applications))

(-> primtitve-circuit (spc:fully-expanded-list spc:circuit) spc:prim-circuit)
(defun primtitve-circuit (terms circuit)
  (~>> (spc:make-prim-circuit :name (spc:name circuit) :body terms)
       (fill-in-arguments circuit)
       (fill-in-output    circuit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Individual Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO :: Make pass that expands away useless lets
;; thus a let :name = :name-calc
;; just rename all instances of :name into :name-calc from that point
;; forth

(-> transform-let (spc:expression) spc:constraint-list)
(defun transform-let (term)
  "transform-let takes a `spc:term' in a flatten form, and removes the
`spc:let-node' for the more flat `spc:bind' type"
  (flet ((transform-let-node (term)
           ;; we keep this type around as it gives us more
           ;; information!
           (with-accessors ((var spc:var) (val spc:value)) term
             (spc:make-bind :var var :val val))))
    (etypecase-of spc:expression term
      ;; if it's just the term, or a list as is, then we are good
      (spc:term-no-binding (list term))
      (spc:let-node        (list (transform-let-node term)))
      (cons                (mapcan #'transform-let term))
      (spc:bind-constraint (list (spc:make-bind-constraint
                                  :var (spc:var term)
                                  :value (mapcan #'transform-let
                                                 (spc:value term))))))))


(-> return-last-binding (spc:expanded-list) spc:expanded-list)
(defun return-last-binding (constraint-list)
  "This transforms the last let into a straight binding, since we aren't
going to be using a let"
  (and constraint-list
       (append (butlast constraint-list)
               (let ((term (car (last constraint-list))))
                 (cons term
                       (etypecase-of spc:expanded-term term
                         (spc:standalone-ret  nil)
                         (spc:bind            (list (spc:make-standalone-ret
                                                     :var (list (spc:var term)))))
                         (spc:bind-constraint (list (spc:make-standalone-ret
                                                     :var (spc:var term))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relocation pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> let-all (spc:constraint-list) spc:expanded-list)
(defun let-all (term-list)
  "This function turns any value which is not the last into a let if it
isn't so already. Perhaps we should make them be an and call instead?"
  (labels ((make-binder (term)
             (spc:make-bind :var (util:symbol-to-keyword (gensym "&G"))
                            :val term))
           (let-term (term)
             (etypecase-of spc:linear-term term
               (spc:standalone-ret   term)
               (spc:bind             term)
               (spc:term-no-binding  (make-binder term))
               (spc:bind-constraint (spc:make-bind-constraint
                                     :var   (spc:var term)
                                     :value (mapcar #'let-term
                                                    (spc:value term)))))))
    (mapcar #'let-term term-list)))

(-> relocate-records (spc:expanded-list spc:circuit) relocate:rel)
(defun relocate-records (anf-terms circuit)
  "Relocate records takes a fully anfied term where only the last form
is not a let, and generates out a `spc:fully-expanded-list' along with
it's closure"
  (labels
      ((ingest (rel term)
         (let* ((closure (relocate:rel-closure rel))
                (new-rel (etypecase-of spc:expanded-term term
                           (spc:bind
                            (relocate:relocate-let term closure))
                           (spc:standalone-ret
                            (relocate:make-rel :closure closure :forms (list term)))
                           ;; ASSUME: how would you make records or
                           ;; other things constraints, no need to
                           ;; even think about it!?
                           (spc:bind-constraint
                            (let ((rel (mvfold #'ingest (spc:value term)
                                               (relocate:make-rel :closure closure))))
                              (relocate:make-rel
                               :closure (relocate:rel-closure rel)
                               :forms   (list (spc:make-bind-constraint
                                               :var   (spc:var term)
                                               :value (reverse
                                                       (relocate:rel-forms rel))))))))))
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

(-> expand-applications (relocate:rel) spc:fully-expanded-list)
(defun expand-applications (rel)
  (let ((closure (relocate:rel-closure rel)))
    (labels ((update-val (term)
               (if (typep (spc:value term) 'spc:application)
                   (util:copy-instance term :value (expand-app (spc:value term)))
                   term))
             (expand-app (app)
               (util:copy-instance app :args (mapcan #'expand-argument
                                                     (spc:arguments app))))
             (expand-argument (arg)
               (etypecase-of spc:term-normal-form arg
                 (number        (list arg))
                 (spc:reference (or (mapcar (lambda (x) (spc:make-reference :name x))
                                            (relocate:maps-to (spc:name arg) closure))
                                    (list arg)))))
             (expand-term (term)
               (etypecase-of spc:fully-expanded-term term
                 (spc:bind             (update-val term))
                 (spc:multiple-bind    (update-val term))
                 (spc:bind-constraint
                  (spc:make-bind-constraint
                   :var   (spc:var term)
                   :value (mapcar #'expand-term (spc:value term))))
                 (spc:standalone-ret
                  (spc:make-standalone-ret
                   :var (mapcan (lambda (x) (or (relocate:maps-to x closure)
                                            (list x)))
                                (spc:var term)))))))
      (mapcar #'expand-term (relocate:rel-forms rel)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove void returns and lets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update logic so that we can get inference on this.
(-> remove-void-bindings (spc:fully-expanded-list) spc:fully-expanded-list)
(defun remove-void-bindings (terms)
  "remove-void-bindings removes any void return value from a function
and direct references to it. It does this by returning a multi-bind
with no names. Note this does not go into other values, so the error
of the user program is preserved."
  ;; we use mutation here just because the fold pattern of trying to
  ;; mimic a map-accuml is just too much against clarity
  (let ((set (sycamore:tree-set #'util:hash-compare)))
    (labels ((value-if-void (term)
               (let ((value (spc:value term)))
                 (cond ((and (typep value 'spc:application)
                             (~> value
                                 spc:func spc:name
                                 storage:lookup-function
                                 spc:return-type
                                 voidp))
                        (mapcar (lambda (x) (sycamore:tree-set-insertf set x))
                                (if (listp (spc:var term))
                                    (spc:var term)
                                    (list (spc:var term))))
                        (spc:make-multiple-bind :var nil :val (spc:value term)))
                       ((and (typep value 'spc:reference)
                             (sycamore:tree-set-find set (spc:name value)))
                        (sycamore:tree-set-insertf set (spc:var term))
                        nil)
                       (t
                        term))))
             (remove-standalone-ret (term)
               (let ((rets (mapcan (lambda (x)
                                    (if (sycamore:tree-set-find set x)
                                        nil
                                        (list x)))
                                   (spc:var term))))
                 (and rets
                      (spc:make-standalone-ret :var rets)))))
      (filter-map (lambda (term)
                    (etypecase-of spc:fully-expanded-term term
                      (spc:bind            (value-if-void term))
                      (spc:multiple-bind   (value-if-void term))
                      (spc:standalone-ret  (remove-standalone-ret term))
                      (spc:bind-constraint (spc:make-bind-constraint
                                            :var   (spc:var term)
                                            :value (remove-void-bindings
                                                    (spc:value term))))))
                  terms))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Primitive Circuit Filling logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; All these functions should probably use mutation for efficiency
;; reasons, but alas we do a lot of extra copying

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; argument filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> fill-in-arguments (spc:circuit spc:prim-circuit) spc:prim-circuit)
(defun fill-in-arguments (alu-circuit prim-circuit)
  (values
   (util:copy-instance prim-circuit
                       :arguments (~> alu-circuit
                                      expand:full-arguments-from-circuit
                                      expand:argument-names))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Return Type Filling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> fill-in-output (spc:circuit spc:prim-circuit) spc:prim-circuit)
(defun fill-in-output (alu-circuit prim-circuit)
  (values
   (util:copy-instance
    prim-circuit
    :returns (determine-output-variables (spc:body prim-circuit)
                                         (spc:return-type alu-circuit)))))

(-> determine-output-variables
    (spc:fully-expanded-list (or spc:type-reference null)) list)
(defun determine-output-variables (body ret)
  "Determines which output variables are returned from a function. If
ret is (`spc:primitive' :void) then an empty list is returned, however
if the value is not void, then the returns in the body are given back"
  (unless (voidp ret)
    (let ((filtered (remove-if-not (lambda (x)
                                     (typep x 'spc:standalone-ret))
                                   body)))
      (mapcan #'spc:var filtered))))

(defun voidp (ret)
  (typecase ret
    (spc:type-reference (eq (spc:name ret) :void))
    (otherwise          nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Renaming 在蒼白的月光
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> rename-primitive-circuit (spc:prim-circuit) spc:prim-circuit)
(defun rename-primitive-circuit (prim-circ)
  (with-accessors ((name spc:name)    (args spc:arguments)
                   (rets spc:returns) (body spc:body))
      prim-circ
    (values
     (spc:make-prim-circuit :name      (renaming-scheme name)
                            :arguments (mapcar #'renaming-scheme args)
                            :returns   (mapcar #'renaming-scheme rets)
                            :body      (rename-statements body)))))

;; If we do this uniformly to all terms then the references will all
;; be valid!
(-> rename-statements (spc:fully-expanded-list) spc:fully-expanded-list)
(defun rename-statements (stmts)
  (labels ((rename-vars-val (con var)
             (funcall con
                      :val (handle-base (spc:value var))
                      :var (mapcar #'renaming-scheme (spc:var var))))
           (rename-var-val (con var)
             (funcall con
                      :val (handle-base (spc:value var))
                      :var (renaming-scheme (spc:var var))))
           (handle-ref (normal)
             (etypecase-of spc:term-normal-form normal
               (spc:number    normal)
               (spc:reference (spc:make-reference
                               :name (renaming-scheme (spc:name normal))))))
           ;; recursion is fine in binds, as they can't have another
           ;; binding, has to be the `spc:application' or `spc:term-normal-form'
           (handle-term (x)
             (etypecase-of spc:fully-expanded-term x
               (spc:bind            (rename-var-val #'spc:make-bind x))
               (spc:multiple-bind   (rename-vars-val #'spc:make-multiple-bind x))
               (spc:standalone-ret  (spc:make-standalone-ret
                                     :var (mapcar #'renaming-scheme (spc:var x))))
               (spc:bind-constraint (spc:make-bind-constraint
                                     :var   (mapcar #'renaming-scheme (spc:var x))
                                     :value (rename-statements (spc:value x))))))
           (handle-base (x)
             (etypecase-of spc:base x
               (spc:term-normal-form (handle-ref x))
               (spc:application
                (spc:make-application
                 :function (handle-ref (spc:func x))
                 :arguments (mapcar #'handle-ref (spc:arguments x)))))))
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
