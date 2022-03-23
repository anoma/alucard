(in-package :alu.pass.anf)

(-> normalize-expression (spc:expression) spc:expression)
(defun normalize-expression (expression)
  "Takes a potentially nested term, and flattens it with let bindings"
  (normalize expression #'identity))

(-> normalp (spc:expression) boolean)
(defun normalp (expr)
  (match-of spc:expression expr
    ((spc:primitive)     t)
    ((spc:number _)      t)
    ((spc:let-node)      nil)
    ((spc:application)   nil)
    ((spc:record)        nil)
    ((spc:record-lookup) nil)
    ((spc:reference)     nil)
    ((cons _ _)          nil)))

(-> normalize (spc:expression function) spc:expression)
(defun normalize (term constructor)
  "normalize works by taking a term, deciding if it needs to be let
abstracted. if so, then we generate a let binding over the constructor
calling it with the ref. Thus:

(normalize 3       g) =
  (g 3)
(normalize (h x y) g) =
  (make-let :var (gen-sym ...) :val (h x y) :body (g gen-sym))

Note that for any term which can nest, we build up a continuation that
will evaluate to this let buildup."
  (assure spc:expression
    (match-of spc:expression term
      ;; for terms which are just references or numbers we can
      ;; just call the constructor, and end the algorithm
      ((spc:primitive)   (funcall constructor term))
      ((spc:number numb) (funcall constructor numb))
      ;; For nodes which are not in normal form, recurse building
      ;; up the let chain
      ((spc:let-node spc:value spc:body spc:var)
       (normalize spc:value
                  (lambda (new-val)
                    (spc:make-let
                     :var spc:var
                     :val new-val
                     :body (normalize spc:body constructor)))))
      ((spc:application spc:name spc:arguments)
     
       (flet ((linearize-function (args)
                (normalize spc:name
                           (lambda (f)
                             (funcall
                              constructor
                              (spc:make-application :function  f
                                                    :arguments args)))))
              (process-argument (argument continuation-build-up)
                (normalize argument
                           (lambda (arg)
                             ()))
                continuation-build-up))
         (reduce #'process-argument
                 spc:arguments
                 :initial-value #'linearize-function
                 :from-end t)))
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

(-> combine-expression (spc:expression spc:expression) spc:expression)
(defun combine-expression (expr1 expr2)
  (dispatch-case ((expr1 spc:expression)
                  (expr2 spc:expression))
    ((cons     cons)     (append expr1 expr2))
    ((spc:term spc:term) (list expr1 expr2))
    ((cons     spc:term) (append expr1 (list expr2)))
    ((spc:term cons)     (cons expr1 expr2))))
