###### tags: `Alucard`

# Alucard Reference

## defcircuit

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
order with the last expression being returned if there is a non `void` return type.

Examples

```lisp=
(defcircuit poly-check ((public x int)
                        (output bool))
  (= (+ (exp x 3)
        (* 3 (exp x 2))
        (* 2 x)
        4)
     0))
     
(alu:deftype point ()
  (x int)
  (y int))

(alu:deftype nested ()
  (plane point)
  (time  point))

(defcircuit constrain ((public nest nested)
                       (output void))
  (def ((plane (plane nest))
        (time  (time nest)))
    (= (* (x plane)
          (y plane))
       (+ (x time)
          (y time)))))
```
## deftype

## defgate

## def

## coerce

## check

## =

## +

## *

## exp

## to-array

## get

