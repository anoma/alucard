(in-package :alu.pass)

;; TODO :: Make a Berlin pipeline abstraction, we really need to stop
;; half way through for easier testing! until then I'll just have many
;; arrow functions for where I want to stop off!

(-> to-linearize           (spc:circuit) spc:constraint-list)
(-> to-expand-away-records (spc:circuit) spc:fully-expanded-list)
(-> to-primtitve-circuit   (spc:circuit) spc:prim-circuit)

(defun pipeline (circuit)
  (~> circuit
      to-expand-away-records))

(defun to-linearize (circuit)
  (~> circuit
      spc:body
      anf:normalize-expression
      linearize-lets
      let-all-but-last))

(defun to-expand-away-records (circuit)
  (~> circuit
      to-linearize
      (expand-away-records circuit)))

(defun to-primtitve-circuit (circuit)
  (~> circuit
      to-expand-away-records
      (primtitve-circuit circuit)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Groups of Passes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
                 (spc:primitive (list arg))
                 (spc:reference (or (relocate:maps-to (spc:name arg) closure)
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
    (null               t)
    (otherwise          nil)))
