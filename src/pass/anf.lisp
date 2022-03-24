(in-package :alu.pass.anf)

(-> normalize-expression (spc:expression) spc:expression)
(defun normalize-expression (expression)
  "Takes a potentially nested term, and flattens it with let bindings"
  (normalize expression #'identity))

(-> normalp (spc:expression) boolean)
(defun normalp (expr)
  (etypecase-of spc:expression expr
    (number            t)
    (spc:primitive     t)
    (spc:reference     t)
    (spc:let-node      nil)
    (spc:application   nil)
    (spc:record        nil)
    (spc:record-lookup nil)
    (cons              nil)))

(-> normalize
    (spc:expression (-> (spc:expression) spc:expression)) spc:expression)
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
      ((spc:reference)   (funcall constructor term))
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
       (normalize-bind spc:name
                       (lambda (func-name)
                         (normalize-bind*
                          spc:arguments
                          (lambda (args)
                            (funcall constructor
                                     (spc:make-application :function func-name
                                                           :arguments args)))))))
      ((spc:record)
       term)
      ((spc:record-lookup)
       term)
      ;; we get a bad exhaustive message due to number, but it will warn
      ;; us, if they aren't the same none the less!
      ((cons a b)
       (cons a b)))))

;; replace expression with terms here!?
;; this function was taken from
;; https://matt.might.net/articles/a-normalization/
(-> normalize-bind
    (spc:expression (-> (spc:expression) spc:expression)) spc:expression)
(defun normalize-bind (expr cont)
  "normalize-bind normalizes the given expression, creating an unique
let binding if the result of normalization is itself not in normal form"
  (normalize expr
             (lambda (expr)
               (if (normalp expr)
                   (funcall cont expr)
                   (let ((var (util:symbol-to-keyword (gensym "&G"))))
                     (spc:make-let
                      :var var
                      :val expr
                      :body (funcall cont (spc:make-reference :name var))))))))

(-> normalize-bind* (list (-> (list) spc:expression)) spc:expression)
(defun normalize-bind* (list cont)
  ;; can't foldr here, as it turns out, we need access to the recursive calls ☹
  (if (null list)
      (funcall cont nil)
      (normalize-bind
       (car list)
       (lambda (ref-car)
         ;; induction!
         (normalize-bind* (cdr list)
                          (lambda (ref-cdr)
                            (funcall cont (cons ref-car ref-cdr))))))))

(-> combine-expression (spc:expression spc:expression) spc:expression)
(defun combine-expression (expr1 expr2)
  (dispatch-case ((expr1 spc:expression)
                  (expr2 spc:expression))
    ((cons     cons)     (append expr1 expr2))
    ((spc:term spc:term) (list expr1 expr2))
    ((cons     spc:term) (append expr1 (list expr2)))
    ((spc:term cons)     (cons expr1 expr2))))
