(in-package :alu.typechecker.intro)

(-> intro (typing-context &rest keyword) typing-context)
(defun intro (ctx &rest keys)
  (check:make-starting-hole keys ctx))

;; TODO Finish
(-> intro-maybe-solution (typing-context &rest t) typing-context)
(defun intro-maybe-solution (ctx &rest keys)
  (let ((solutions (remove-if #'listp keys))
        (holes     (remove-if-not #'listp keys)))
    solutions
    (apply #'intro ctx holes)))

;; TODO Need an extra syntax for types to be given with the
;; introduction, since we may know them already.
(defmacro with-intro ((new-context-name &rest names) context &body body)
  "With-intro introduces the names with gensym to the given context,
binding the names for use in the body.

EXAMPLE:

;; TODO, put some computation here from the code where I use this.
(check:with-intro (ctx bar baz) context
  ctx)"
  `(let ,(mapcar (lambda (symbol)
                   (let ((symbol-name
                           (if (listp symbol) (car symbol) symbol)))
                     `(,symbol-name (util:symbol-to-keyword
                                     (gensym (symbol-name ',symbol-name))))))
                 names)
     (let ((,new-context-name (intro ,context ,@names)))
       ,@body)))
