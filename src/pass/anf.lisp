(in-package :alu.pass.anf)

;;; After the let change, I'm not sure this code is correct, hard to
;;; even test, as our pass already puts it into anf form!

(-> normalize-expression (spc:expression) spc:expression)
(defun normalize-expression (expression)
  "Takes a potentially nested term, and flattens it with let bindings"
  (normalize expression #'identity))

(-> normalp (spc:expression) boolean)
(defun normalp (expr)
  (typep expr 'spc:term-normal-form))

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
      ((spc:number numb) (funcall constructor numb))
      ((spc:reference)   (funcall constructor term))
      ;; For nodes which are not in normal form, recurse building
      ;; up the let chain
      ((spc:let-node spc:value spc:var)
       (normalize spc:value
                  (lambda (new-val)
                    (funcall constructor
                             (spc:make-let :var spc:var
                                           :val new-val)))))
      ((spc:application spc:name spc:arguments)
       (normalize-bind spc:name
                       (lambda (func-name)
                         (normalize-bind*
                          spc:arguments
                          (lambda (args)
                            (funcall constructor
                                     (spc:make-application :function func-name
                                                           :arguments args)))))))
      ((spc:record spc:name spc:contents spc:order)
       ;; probably the hardest transform just due to hash table format
       ;; schenans. Note that an alist is like the following
       ;; ((:key1 . term1) (:key2 . term2))
       ;; spc:name is a keyword so no need to traverse
       (let* ((alist-contents (sycamore:tree-map-alist spc:contents))
              (keys           (mapcar #'car alist-contents))
              (values         (mapcar #'cdr alist-contents)))
         (normalize-bind*
          values
          (lambda (value-refs)
            (funcall constructor
                     (make-instance 'spc:record
                                    :name spc:name
                                    :order spc:order
                                    :contents (sycamore:alist-tree-map
                                               ;; remake our alist
                                               (mapcar #'cons keys value-refs)
                                               #'util:hash-compare)))))))
      ((spc:record-lookup spc:record spc:field)
       ;; field is a keyword, thus we are fine with it
       (normalize-bind spc:record
                       (lambda (rec-ref)
                         (funcall constructor
                                  (spc:make-record-lookup :record rec-ref
                                                          :field  spc:field)))))
      ;; we get a bad exhaustive message due to number, but it will warn
      ;; us, if they aren't the same none the less!
      ((cons _ _)
       (funcall constructor
                (mvfoldr #'combine-expression
                         (mapcar (lambda (ter) (normalize ter #'identity)) term)))))))

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
                     (combine-expression
                      (spc:make-let
                       :var var
                       :val expr)
                      (funcall cont (spc:make-reference :name var))))))))

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

;; Unused should we delete
(-> combine-expression (spc:expression spc:expression) spc:expression)
(defun combine-expression (expr1 expr2)
  (dispatch-case ((expr1 spc:expression)
                  (expr2 spc:expression))
    ((cons     cons)     (append expr1 expr2))
    ((spc:term spc:term) (list expr1 expr2))
    ((cons     spc:term) (append expr1 (list expr2)))
    ((spc:term cons)     (cons expr1 expr2))))
