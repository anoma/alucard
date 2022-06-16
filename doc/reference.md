###### tags: `Alucard`

# Alucard Reference

Welcome to the Alucard Reference Manual.

The reference manual is laid out via sections on specific topics. Each
topic contains various functions related to the topic along with
documentation and example usage of each feature.

For example, a section like [Definition Facilities](#Definition-Facilities) has subtopics like:

- defcircuit
- deftype
- def

The topic section itself serves to give background and details about
the entire section as a whole as to get a better sense of each
function in the topic.

The exception to this rule are the next two sections which describe
the project, how Alucard relates to its host language, and how this
barrier between languages work.


Outside of those details, this document should serve as a good way of
getting familiar with the Alucard programming language.

Happy Hacking!

## About Alucard

## How Alucard Interoperates with Common Lisp

## Definition Facilities

This section covers all the standard function defining facilities
along with functions which create binders.

A common theme among all these functions is that they all take Alucard
values, and return Alucard values. Which means that 

Note that functions from the CL standard that are often used as
Alucard definers aren't described here even though they fit the
topic. Go to the [Common Lisp Facilities](Common Lisp Facilities)
section to learn more about those.

### defcircuit

```lisp
(defcircuit func-name (parameter+ return?) body*)

parameter = (private symbol type)
          | (public symbol type)

return = (return type)
```

Binds `func-name` to a procedure. The `parameter+` determines the type
and number of arguments this procedure may have, and the `return?`
determines the return type the procedure has. The body of the
procedure is the specified list of expressions, these are evaluated in
order with the last expression being returned if there is a non `void`
return type.

Examples

```lisp
(deftype point ()
  (x-plane int)
  (y-plane int)
  (z-plane int))

(defcircuit square-root ((private p int)
                         (output int))
  (def ((with-constraint (x₁)
          (= p (* x₁ x₁))))
    x₁))

(defcircuit l2-norm ((public p point)
                     (output int))
  (flet ((sum (list)
           (apply #'+ list)))
    (square-root
     (sum (mapcar (lambda (x) (exp x 2))
                  (list (x-plane p) (y-plane p) (z-plane p)))))))

;; Calling the l-2norm
(l2-norm (point :x-plane 3 :y-plane 5 :z-plane 10))
```
### deftype

### defgate

### def

## Typing Facilities

### coerce

### check

## Common Lisp Facilities

## Numeric Facilities

### =

### +

### *

### exp

## Array Facilities

### to-array

### get

