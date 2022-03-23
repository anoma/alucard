(in-package :alu.pass)


;; need this for pattern matching numbers easily with exhasution
(-> anfify-term (spc:expression) spc:term)
(defun anfify-term (expression)
  "Takes a potentially nested term, and flattens it with let bindings"
  (labels
      ((anf-algorithm (term constructor)
         "anf-algorithm works by taking a term, deciding if it needs
to be let abstracted. if so, then we generate a let binding over the
constructor calling it with the ref. Thus:
(anf-algorithm 3       g) =
  (g 3)
(anf-algorithm (h x y) g) =
  (make-let :var (gen-sym ...) :val (h x y) :body (g gen-sym))"
         (match-of spc:expression term
           ;; for terms which are just references or numbers we can
           ;; just call the constructor, and end the algorithm
           ((spc:primitive)
            (funcall constructor term))
           ((spc:number numb)
            (funcall constructor numb))
           ;; For nodes which are not in normal form, recurse building
           ;; up the let chain
           ((spc:let-node spc:value spc:body spc:var)
            (anf-algorithm spc:body
                           (lambda (new-body)
                             (anf-algorithm
                              spc:value
                              (lambda (new-val)
                                (spc:make-let
                                 :var spc:var
                                 :val new-val
                                 :body (funcall constructor new-body)))))))
           ((spc:application spc:name spc:arguments)
            (combine-expression (anfify-term spc:name)
                                (mapcar #'anfify-term spc:arguments)))
           ((spc:record)
            term)
           ((spc:record-lookup)
            term)
           ((spc:reference)
            term)
           ;; we get a bad exhaustive message due to number, but it will warn
           ;; us, if they aren't the same none the less!
           ((cons a b)
            (cons a b)))))
    (assure spc:term
      (anf-algorithm expression #'identity))))

(-> combine-expression (spc:expression spc:expression) spc:expression)
(defun combine-expression (expr1 expr2)
  (dispatch-case ((expr1 spc:expression)
                  (expr2 spc:expression))
    ((cons     cons)     (append expr1 expr2))
    ((spc:term spc:term) (list expr1 expr2))
    ((cons     spc:term) (append expr1 (list expr2)))
    ((spc:term cons)     (cons expr1 expr2))))

(-> linearize-lets (spc:term) spc:constraint-list)
(defun linearize-lets (term)
  "linearize-lets takes a `spc:term' in a flatten form, and removes the
`spc:let-node' for the more flat `spc:bind' type"
  (etypecase-of spc:term term
    ;; if it's just the term as is, then we are good
    (spc:term-no-binding
     (list term))
    ;; if it's a let-node we should change it
    (spc:let-node
     (with-accessors ((var spc:var) (val spc:value) (body spc:body)) term
       (cons (spc:make-bind :var var :val val)
             (linearize-lets body))))))
