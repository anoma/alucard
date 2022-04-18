(in-package :alu.pass)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> linearize (spc:circuit) spc:constraint-list)
(defun linearize (circuit)
  (~> circuit
      spc:body
      anf:normalize-expression
      linearize-lets
      let-all-but-last))

(-> expand-away-records (spc:constraint-list spc:circuit) spc:fully-expanded-list)
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

(-> linearize-lets (spc:expression) spc:constraint-list)
(defun linearize-lets (term)
  "linearize-lets takes a `spc:term' in a flatten form, and removes the
`spc:let-node' for the more flat `spc:bind' type"
  (etypecase-of spc:expression term
    ;; if it's just the term, or a list as is, then we are good
    (spc:term-no-binding (list term))
    (cons                term)
    ;; if it's a let-node we should change it
    (spc:let-node
     (with-accessors ((var spc:var) (val spc:value) (body spc:body)) term
       (cons (spc:make-bind :var var :val val)
             (linearize-lets body))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Relocation pass
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> let-all-but-last (spc:constraint-list) spc:constraint-list)
(defun let-all-but-last (term-list)
  "This function turns any value which is not the last into a let if it
isn't so already. Perhaps we should make them be an and call instead?"
  (cond ((null term-list)
         term-list)
        ((and (not (typep (car term-list) 'spc:bind))
              (cdr term-list))
         (cons
          ;; value goes unsued but may be a constraint, so it's not all dead code!
          (spc:make-bind :var (util:symbol-to-keyword (gensym "&G"))
                         :val (car term-list))
          (let-all-but-last (cdr term-list))))
        (t (cons (car term-list) (let-all-but-last (cdr term-list))))))

(-> relocate-records (spc:constraint-list spc:circuit) relocate:rel)
(defun relocate-records (anf-terms circuit)
  "Relocate records takes a fully anfied term where only the last form
is not a let, and generates out a `spc:fully-expanded-list' along with
it's closure"
  (flet ((ingest (rel term)
           (let* ((closure (relocate:rel-closure rel))
                  (new-rel (etypecase-of spc:linear-term term
                             (spc:bind
                              (relocate:relocate-let term closure))
                             (spc:term-no-binding
                              (relocate:make-rel
                               :forms   (relocate:relocate-standalone term closure)
                               :closure closure)))))
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
                 (spc:term-normal-form term)
                 (spc:application      (expand-app term))
                 (spc:bind             (update-val term))
                 (spc:multiple-bind    (update-val term))
                 (spc:multi-ret        (update-val term))
                 (spc:ret              (update-val term)))))
      (mapcar #'expand-term (relocate:rel-forms rel)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Remove void returns and lets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Update logic so that we can get inference on this.
(-> remove-void-bindings (spc:fully-expanded-list) spc:fully-expanded-list)
(defun remove-void-bindings (terms)
  "remove-void-bindings removes any void return value from a function
and direct references to it. Note this does not go into other values,
so the error of the user program is preserved."
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
                        (if (listp (spc:var term))
                            (mapcar (lambda (x) (sycamore:tree-set-insertf set x))
                                    (spc:var term))
                            (sycamore:tree-set-insertf set (spc:var term)))
                        (spc:value term))
                       ((and (typep value 'spc:reference)
                             (sycamore:tree-set-find set (spc:name value)))
                        nil)
                       (t
                        term)))))
      (filter-map (lambda (term)
                    (etypecase-of spc:fully-expanded-term term
                      (spc:term-normal-form term)
                      (spc:application      term)
                      (spc:bind             (value-if-void term))
                      (spc:multiple-bind    (value-if-void term))
                      (spc:multi-ret        (value-if-void term))
                      (spc:ret              (value-if-void term))))
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
    (spc:expanded-list (or spc:type-reference null)) list)
(defun determine-output-variables (body ret)
  "Determines which output variables are returned from a function. If
ret is (`spc:primitive' :void) then an empty list is returned, however
if the value is not void, then the returns in the body are given back"
  (unless (voidp ret)
    (let ((filtered (remove-if-not (lambda (x)
                                     (typep x '#1=(or spc:ret spc:multi-ret)))
                                   body)))
      (mapcan (lambda (x)
                (etypecase-of #1# x
                  (spc:ret
                   (list (spc:var x)))
                  (spc:multi-ret
                   (spc:var x))))
              filtered))))

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
                      :val (handle-term (spc:value var))
                      :var (mapcar #'renaming-scheme (spc:var var))))
           (rename-var-val (con var)
             (funcall con
                      :val (handle-term (spc:value var))
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
               (spc:bind             (rename-var-val #'spc:make-bind x))
               (spc:ret              (rename-var-val #'spc:make-ret  x))
               (spc:multiple-bind    (rename-vars-val #'spc:make-multiple-bind x))
               (spc:multi-ret        (rename-vars-val #'spc:make-multi-ret x))
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
