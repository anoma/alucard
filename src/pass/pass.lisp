(in-package :alu.pass)

(defun pipeline (circuit)
  (~> circuit
      spc:body
      anf:normalize-expression
      linearize-lets
      let-all-but-last
      (relocate-records circuit)))

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

;; man I wish I could type inside of rel ☹, it would be spc:fully-expanded-term
(-> relocate-records (spc:constraint-list spc:circuit) relocate:rel)
(defun relocate-records (anf-terms circuit)
  "Relocate records takes a fully anfied term where only the last form
is not a let, and generates out a `spc:fully-expanded-term' along with
it's closure"
  (let ((rel (reduce (lambda (rel term)
                       (etypecase-of spc:linear-term term
                         (spc:bind
                          (let ((new-rel
                                  (relocate:relocate-let term
                                                         (relocate:rel-closure rel))))
                            (relocate:make-rel
                             :forms (append (relocate:rel-forms new-rel)
                                            (relocate:rel-forms rel))
                             :closure (relocate:rel-closure new-rel))))
                         (spc:term-no-binding
                          (relocate:make-rel
                           :forms   (append (relocate:relocate-standalone
                                             term
                                             (relocate:rel-closure rel))
                                            (relocate:rel-forms rel))
                           :closure (relocate:rel-closure rel)))))
                     anf-terms
                     :initial-value (relocate:make-rel
                                     :forms nil
                                     :closure (relocate:initial-closure-from-circuit
                                               circuit)))))
    (relocate:make-rel :forms   (reverse (relocate:rel-forms rel))
                       :closure (relocate:rel-closure rel))))

;; (alu.pass::pipeline (spc:body (storage:lookup-function :poly-check)))
