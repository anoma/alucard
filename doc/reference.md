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

The exception to this rule are the next two sections:

- [About Alucard](#About-Alucard) section covers a wide range of
  topics such as:
  + primitive operations of circuits
  + Type inference
  + How data types are represented
- [How Alucard Interoperates with Common Lisp](#How-Alucard-Interoperates-with-Common-Lisp).
  + This section covers how the host language interacts with Alucard
    and the barriers between the two.

These sections are good to read in full as they cover information that
allows one to more effectively leverage the language as-well as have a feel
for the cost of certain features of Alucard.

Outside of those details sections, the document is meant to be jumped
around for the functions and features one wishes to learn about.

Happy Hacking!

## About Alucard

Alucard is a language for writing Zero Knolwedge Proofs (ZKP), thus
many details of how the language works is different from a more
traditional architecture.

### Circuit Model

The circuit model is an interesting one, having computation be
composed of `+`, `*`, and `-`. In essence, we can view our computation
in the form of polynomials.

```lisp
;; example
0 = a*a + b*b - c*c
```

A good metric to view the costs of these operations is that addition
(`+`) has a base cost of 0, while multiplication (`*`) has a base cost
of 1. The elements of this polynomial are
[field](https://en.wikipedia.org/wiki/Field_(mathematics)) elements,
meaning that in this case we the number is between `0` and some `p`
where `p` is prime (`[0, P)`).

The ultimate goal of these polynomials is to generate out a circuit
that will be proved by another party. Thus there are a few parties
involved with the computation. We call these parties the `prover` and
the `verifier`.

The `prover` is the creator of the circuit, and may have extra facts
about the circuit that are spelled out in polynomial form.

This takes the form of `private` inputs in Alucard programs.

The other party is the `verifier`, who wishes to verify the equation,
and does so by sending in public data into the circuit

This takes the form of `public` inputs in Alucard programs.

Further, circuit backends also have a form of
[prolog style matching](https://cseweb.ucsd.edu/~goguen/courses/130w04/prolog.html),
with variables being introduced out of nowhere.

For example:

If we want to write the square root function or the squared function we can write

```lisp
x = y * y
```

where if `x` is known, `y` is solved to be the root of x. If `y` is
known, then `x` is the square of `y`.

This also has interesting overhead in some scenarios, as the circuit
may form `n` possible solutions if an equation is under-determined

Since Alucard compiles to
[vamp-IR](https://github.com/ZK-Garage/vamp-ir), there are other
performance considerations to be had. Arrays can be made much faster
if one is using the plookup (plonk with lookup) backend.

### Representation of Data

Numeric types are represented by field elements natively within the
language.

For data types which require multiple field elements, the situation is
quite a bit harder, as computation is built around `+`, `*`, and
little else.

Therefore, We can't simply just read n-bits of the structure, and pass
around data in a packed form without massive overhead or some tricks.

With that said, Alucard supports the following data types:

- User defined records (no Cost)
- Arrays (Expensive Cost if indexed as an input)

The layout of each type is as follows

```lisp
Arrays: with elements of length n
------------------------------------------------
| element 0: n bits | .... | element m: n bits |
------------------------------------------------

(deftype point () (x (int 8)) (y (int 9)))
Point:
------------------------------------------------
|    x field: 8 bits   |    y field: 9 bits    |
------------------------------------------------
```

Records and Arrays aren't guaranteed to be packed, they may be inlined
away (records almost always are, arrays are not yet). However if code
coerces these types or do operations that rely on format, this is the
cannonical layout of each concept.

#### Records

Records are user defined types, often called structs in language like C.

An example record type is

```lisp=
(deftype point ()
  (x int)
  (y int))

(point :x 5 :y 10)
```

which defines a point which has an x and y field.

record can be compiled at zero costs in **most situations**, by
expanding out the record into its constituents.

What this means that if we have a function like

```lisp=
(defcircuit add-points ((private p point))
  ;; Adding the x field and y field of p.
  (+ (x p) (y p)))

(add-points (point :y 10 :x 5))
```

then the circuit and applications are expanded as such:

```lisp=
(defcircuit add-points ((private p-x int)
                        (private p-y int))
  ;; Adding the x field and y field of p.
  (+ p-x p-y))

(add-points 5 10)
```

There is however, a few case where records do have a cost, and that is
in conjunction with [arrays](#Arrays) and type coercions.

In both these cases, record instances must be
[packed](#Packing-in-detail) and have a canonical
[packed](#Packing-in-detail) form. The form comes in the
declaration. So in our above `point` type, the `x` covers the first
`n` bits, while `y` comes after `x`.

Arrays often have costs, as Arrays typically have to pack data,
meaning that records must incur an additional
[packing/unpacking](#Packing-in-detail) cost, as it must be unpacked
after looking it up from the array.

Type coercions have this cost in two scenarios:

- Coercing a record to an array or a number
- Coercing a number or array to a record.

These both have costs, as in the first case, the record is an
[unpacked](#Packing-in-detail) structure being casted into an array
(which are currently all packed!)  or a singular number.

The latter has a cost in that the record must be
[unpacked](#Packing-in-detail) to use the fields, meaning that we
force an [unpacking](#Packing-in-detail).

#### Arrays

Arrays are a builtin datatype to the Alucard language. They are typed
in a very similar way to Rust, with the length being in the type name
`(array length type)`.

### Packing in detail

Since the data in circuits are only field elements, all data which
must be packed has to fit within a field element. Thankfully for
Alucard, we assume the field element is arbitrarily large, with
[vamp-IR](https://github.com/ZK-Garage/vamp-ir) doing the work of
splitting this up per requested backend.

With that said, it is best to see how packing works with a practical
example.

```lisp
(defcircuit array-from-data-check ((output (array int 10)))
  (def ((foo 35)
        (bar (to-array foo 36)))
    (+ (check foo (int 32))
       (get bar 0))))

```

In this code, we do something really simple, we make an array of
`int-32`'s (we know this as `foo` is checked to be consistent against
the type.). The formal type of this array is `array 2 (int
32)`. Meaning the array is of length 2 and contains elements of type
`int32`. Then we end up adding foo to the first position of the `bar`
array.

This innocent looking code generates out the following code.

```lisp
 (pipeline:pipeline (storage:lookup-function :array-from-data-check))
def array_from_data_check -> vg538853 {
  foo = 35       ; let foo = 35
  ;; (to-array foo 36)
  vg538870 = foo * 1
  vg538871 = 36 * 4294967296
  vg538872 = vg538870 + vg538871
  g538849 = vg538872
  bar = g538849  ; let bar = (to-array foo 36)
  g538850 = foo  ; check foo (int 32)
  ;; (get bar 0)
  vg538858 = bar
  vg538859 = 0
  vg538860 = 32 * vg538859
  vg538861 = 2 ^ vg538860
  vg538862 = vg538861 * smaller_array538856
  vg538863 = vg538862 + unused_mod538855
  vg538858 = vg538863
  vg538867 = vg538858
  vg538864 = 2 ^ 32
  vg538865 = vg538864 * unused_array538854
  vg538866 = vg538865 + lookup_answer538857
  smaller_array538856 = vg538866
  vg538868 = smaller_array538856
  vg538869 = lookup_answer538857
  g538851 = vg538869
  ;; (+ foo (get bar 0))
  g538852 = g538850 + g538851
  ;; return variable name
  vg538853 = g538852
}
```

as we can see here, the packing isn't very expensive, it consists of

```lisp
  ;; (to-array foo 36)
  vg538870 = foo * 1
  vg538871 = 36 * 4294967296
  vg538872 = vg538870 + vg538871
  g538849 = vg538872
```

which works very simply. For each element in the array, store the
element in the first `n` bytes, where `n` is the size of the
type. Here the size is `2^32` bits long or `4294967296 (33 bits,
#x100000000)`. This means that when we add the next element `36`, we
can pack the number easily enough by multiplying this number by the
element we wish to store in it. If there were a third element, we
would shift the next element by another `32` bits, giving us a general
equation.

```lisp
element-at x = 2 ^ (32 * x)
```

This is overall a fairly cheap operations, as it just involves `n`
multiplications and `n` additions.


However, the same can not be said for unpacking.

```lisp
  ;; (get bar 0)
  vg538858 = bar
  vg538859 = 0
  vg538860 = 32 * vg538859
  vg538861 = 2 ^ vg538860
  vg538862 = vg538861 * smaller_array538856
  vg538863 = vg538862 + unused_mod538855
  vg538858 = vg538863
  vg538867 = vg538858
  vg538864 = 2 ^ 32
  vg538865 = vg538864 * unused_array538854
  vg538866 = vg538865 + lookup_answer538857
  smaller_array538856 = vg538866
  vg538868 = smaller_array538856
  vg538869 = lookup_answer538857
```

This is the code for unpacking any number of bits.

Before we continue lets, use a simpler example to understand unpacking.

```lisp
array = [3,1,4,1,5,1,9,6]
```

let us assume we have this array, and we wish to get 4 out of this
array, then we can visualize what we must do as such:

```lisp
[3,1] 4 [1,5,1,9,6]
```

we must split the array into three parts with a series of equations.

```lisp
array = (2 ^ 32 * (3 - 1)) * next-array + unused-mod

0 <= unused-mod <= 2^32 * 2

next-array = 2 ^ 32 * array-after-4 + answer

0 <= answer <= 2^32
```

In here our array is `[3,1,4,1,5,1,9,6]`, and the first equation makes
two unknowns. `next-array`, and `unused-mod`. The circuit knows enough
to solve this, but there may be many solutions to the problem! In our case it should be

```lisp
;; the example left part left over!
unused-mod = [3,1]

next-array = [4,1,5,1,9,6]
```

Then we do the round again, being able to separate out `4` by itself.

```lisp
;; the example greater than part!
array-after-4 = [1,5,1,9,6]
answer = 4
```

Overall it's a very expensive process to index into an array
generically. Arrays of smaller element sizes (`(int 2)` `(int 4)`)
fair a lot better.


### Coercions for fun and profit

Coercions are the idea of converting one form of data into
another. one may often see it in code like.

```lisp
(defcircuit foo ((public x (int 16))
                 (public y (int 32)))
  (+ y (coerce x (int 32))))
```

Where due to the strictness of our typing, we have to add or multiply
or subtract numeric values of the exact same type. Implicit
conversions like In Java, Lisp, or C do not occur.

Coercions between number types like this are free, and no cost is
paid.

However there are some more interesting Coercisons that we can employ
that can help general circuit array programming.

### Hidden Costs in the circuit model

### Type Checking and Inference

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
determines the return type the procedure has.

Each parameter has a privacy marker, which marks the parameter as
either private or public [to the circuit](#Circuit-Model). If the
given function defined from defcircuit is not the entry point to the
circuit, then this privacy value is ignored.

The body of the procedure is the specified list of expressions, these
are evaluated in order with the last expression being returned if
there is a non `void` return type.

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

(defcircuit l2-norm-by-hand ((public p point)
                             (output int))
  (square-root
   (+ (exp (x-plane p) 2)
      (exp (y-plane p) 2)
      (exp (z-plane p) 2))))

;; Calling the l-2norm
(l2-norm (point :x-plane 3 :y-plane 5 :z-plane 10))
```
### deftype

```lisp
(deftype name ()
  record-field*)

record-field = (field-name type)
```
<!-- TODO When sum types get added  -->

Creates a record type with the given `name`. each `record-field`
specifies both the type of the field along with the name for accessing
and creating the field.

```lisp
(deftype transfer ()
  (from-address (int 36))
  (to-address   (int 36))
  (amount       int))

;; Making a transfer
(deflex custom-transfer
    (transfer :from-address #xFFFFFFFFF
              :to-address   #x111111111
              :amount       250))

;; Grabbing a value from a transfer
(to-address custom-transfer)

(deftype account ()
  (address (int 36))
  (total   int))

;; A circuit taking the value
(defcircuit valid-transfer ((public transfer custom-transfer)
                            (private my-account account))
  (and (= (address my-account) (from-address transfer))
       (>= (total int) (amount transfer))))
```

### defgate [Not in the language yet]

### def

```lisp

```

## Typing Facilities

### coerce

```lisp
(coerce term type)
```

Coerces the given expression to the desired type.

A simple example is as follows:

```lisp
(defcircuit same-value? ((public x (int 16))
                         (public y (int 32)))
  (= x (coerce x (int 32))))
```


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

