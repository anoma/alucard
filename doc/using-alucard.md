###### tags: `Alucard`

# A quick guide to Alucard Syntax V0.1.0

This document outlines writing Alucard, every expression written here
is valid alucard, feel free to type along with the document into the
Alucard REPL or a file that you can send over!

New concepts are introduced and iterated upon in later sections.

The file <link-here> is a fully loadable version of this document

## Comments

```lisp
;; This is a comment!

;; All what you are about to see are valid expressions to write
```

## Numbers

These are all valid numbers

```lisp
12
3
4

4294967296
```

## Variables

This is how we make a constant declaration

```lisp
(deflex important 5
  "This is a very important value!")

;; we can use the value by just saying

important

;; In fact we can use many different symbols in our names

(deflex 『perectly-valid』 3
  "This is a perfeclty-valid symbol!")

『perectly-valid』


(deflex √4 2)

√4
```

Alucard supports full unicode in the names as the last few examples
show. In fact most symbols are valid in identifiers, however the
following have special meaning and thus can't be used in names

1. ()
2. '
3. `
4. ,
5. #

### Meaning Of Special Symbols

This section is for those who are curious about what these symbols
mean. It is not necessary to know for the operation of Alucard.


`()` is function application, and thus we can't name symbols with it.
```lisp
(+ 1 2 3 4)
```

```lisp
'unevaluated-symbol!

`also-unevaluated
```

Thus \` and `'` freeze values, but \` allows some extra behavior, with
`,`

```lisp
(deflex evaluated 5)

`,evaluated

`(+ ,evaluated 10)
```

Notice when we type this into the read eval print loop we get back out

`(+ 5 10)`!

That's because `,` undos the effect of `'`.

Lastly, one should avoid `|` in names

if one ran the last section then

```lisp
|EVALUATED|
```

should give back `5`. This is because of how symbols are read, they
are case insensitive, thus

```lisp
evaluated
Evaluated
EVALUATED
```

are all the same symbol, go and type it. `|symbol|` lets one make a
symbol case insensitive.

`#` also has a special meaning along with `'`. It is responsible for
turning a function symbol into a variable. More on that in the
integration section.


### Naming Convention

Symbols are typically written in `dash-case`. To illustrate our point
we will write some examples. We will use `'` to just print back the
symbols given instead of getting the symbol value.

```lisp
'normal-naming

'this-is-dash-case-reads-kind-nice!
```


## Expressions

We were already writing expressions before with our numbers and
symbols, but lets write some more complex expressions.

```lisp
(+ 1 2)

(+ 1 2 3 4 5 6 7)

;; (+ 1 2 3 4 5 6 7)
;;  ^ ^           ^
;;  | | arguments |
;;  |
;;  The function
```

The `()` here means begin a function call, with the first item between
the `()` standing for the function, and the rest of the contents
considered the arguments. We will often refer to the arguments like
"the first argument", to illustrate the point the following list will
make it more clear.

- Expression: (+ 1 2 3 4 5 6 7)

- The first   argument = 1
- The second  argument = 2
- The third   argument = 3
- The fourth  argument = 4
- The fifth   argument = 5
- The sixth   argument = 6
- The seventh argument = 7


One thing we'll notice if we run this expression is that we get a
funky return value like

`#<#<REFERENCE +> 1 2 3 #<#<REFERENCE *> 1 2 3>>`, this is because
there does not exist a circuit interpreter to evaluate alucard
expressions as of yet, and so we return the intermediate
representation in the alucard language. However, don't let this
discourage you from experimenting with sending values to the REPL.

In many cases of doing basic arithmetic over constants we actually
want to use normal addition, rather than the constraint version. We
can do this by calling into the host language's (more on this later!)
function.

```lisp
;; Here we shoud get back 10!
(cl:+ 1 2 3 4)

;; 8 here!
(cl:+ 1 2 3 (cl:* 1 2))

(deflex vector-size 16
  "The size of the vector for our blake2s compression function")

(deflex work-vector (cl:/ vector-size 2)
  "work-vector-split")
```

More examples on this integration later on.

## Declaring Custom Gates

Since Alucard compiles to vamp-ir, a zksnark system over plonkup, we
can define custom gates. We can think of a gate as a function that
takes and returns alucard values.

```lisp
(defcircuit my-constraint-gate ((public input field)
                                (private secret-1 field)
                                (private secret-2 field)
                                (output bool))
  (= input
     (+ (* (exp secret-1 2)
           5)
        (* secret-2 7)
        5)))
```

Let us break down this example:

`defcircuit` declares that we wish to make a new gate. The first
argument to `defcircuit` is the name, which in this case is
`my-constraint-gate`.

`My-constraint-gate` acts like any of the `+` and `*` primitives we
were using earlier.

The second argument

```lisp
((public input field)
 (private secret-1 field)
 (private secret-2 field)
 (output bool))
```

is the parameter list and output type declaration to
`my-constraint-gate`.

A good way to view the parameter declaration is as follows:

`(privacy wire-name type)`


where
- `privacy` can either be `public` or `private`.
- `wire-name` can be any valid identifier
- `type` is any defined type.

In our example, the input wires are `input`, `secret-1`, and
`secret-2`. With the two secrets being private.

The last piece of information is the return type in `(output
bool)`. any valid type can be used in place of `bool`.

The rest of the arguments (arguments after the second) are the body of
the circuit. We can put any expression in the body. Thus the entire
file outside of circuit declarations are fair game!

In this case we've placed the equation

`input = = secret-1^2 * 5 + secret-2 * 7 + 5`

where the input is constrained to be eual to the equation on the
right. In alucard syntax we could have written this like

```lisp
'(= input (+ (* (exp secret-1 2) 5) (* secret-2 7) 5))

;; Forming this much shorter Statement!
(defcircuit my-constraint-gate-again! ((public input field)
                                       (private secret-1 field)
                                       (private secret-2 field)
                                       (output bool))
  (= input (+ (* (exp secret-1 2) 5) (* secret-2 7) 5)))
```

We can call our custom circuit much like we've been calling `+` and
`*`.

```lisp
(my-constraint-gate 234 (exp 2 31) (* 5 27))


(defcircuit constrain-square ((public input field)
                              (private sec1 field)
                              (private sec2 field)
                              (output bool))
  (and (my-constraint-gate input sec1 sec2)
       (my-constraint-gate input sec1 (exp sec2 2)))) ; sec2^2 = sec2!
```

If you've been following with editor integration, you should be able
to jump to the definition of `my-constraint-gate` and any value used
with the exception of local variables.

## Local wire Declarations

Alucard gives a way to name intermediate wires. This is useful when we
don't want to repeat ourselves. Let us rewrite `constrain-square`
using this mechanism

```lisp
(defcircuit constrain-square% ((public input field)
                               (private sec1 field)
                               (private sec2 field)
                               (output bool))
  (def ((constraint-1 (my-constraint-gate input sec1 sec2))
        (constraint-2 (my-constraint-gate input sec1 (exp sec2 2))))
    (and constraint-1 constraint-2)))
```

We can see it with `def`, def can be viewed like

```lisp
'(def ((local₁ value₁)
       …
       (localₙ valueₙ))
   expression-with-local₁
   …
   expression-with-localₙ)
```

where we bind each `local` to each `value`, along with a body where
the locals are lexically scoped over.

Looking at our actual example we've created some amount of repetition

`(my-constraint-gate input sec1 some-sec2-expr)`.

Alucard itself can't really abstract this out, as it requires writing
a local function (closure) that has `input` and `sec1` in
scope. However since Alucard is a DSL in CL, we can call into the
language to get rid of this pattern!


```lisp
(defcircuit constrain-square%% ((public input field)
                                (private sec1 field)
                                (private sec2 field)
                                (output bool))
  ;; flet makes a normal function that will not show up in the
  ;; circuit!
  (flet ((call-constraint (var-secret)
           (my-constraint-gate input sec1 var-secret)))
    (and (call-constraint sec2)
         (call-constraint (exp sec2 2)))))
```

Here we used `flet` which in Common lisp simply makes a function. In
our case we create the function `call-constraint` which takes the
argument `var-secret` and outputs the `my-constraint-gate` call that
we wanted to remove. Thus, `constrain-square%%` generates exactly the
same circuit as `constrain-square`!

The one caveat is that we can not return `call-constraint`, however
that is more due to the alucard language having no closure mechanism.

We will see more of the interplay between alucard and common lisp
(hereby known as lisp) in a later section. For now we can view it as a
relationship where repeat expressions in alucard can be abstracted
away by lisp itself.

## Type Declarations

The last major building block of the current alucard version is the
ability to define custom data types.

A good first example is the point type

<!-- In my code I use the UTXO type but it feels hollow in that I'm
not sure what parts mean nor can we get the owner down correct -->

```lisp
(deftype point ()
  (x field)
  (y field))
```

here we created the type `point` with a `x` and `y` field. Amusingly
enough the types of the record fields are themselves fields.

We can make a point like so

```lisp
;; make a point with the x value being 3 and y value being 5.
(point :x 3 :y 5)
```

We can access the fields of the record like so


```lisp
(defcircuit point-constraint ((public point point)
                              (private var field)
                              (output bool))
  (= var (+ (x point) (y point))))

(x (point :x 3 :y 5))
```

The fields can be accessed by calling the field as a function on the
`point` itself.

### Namespaces

It would be remiss not to note that functions, variables, and types
all live in different namespaces.

This means the following example is completely fine and valid

```lisp
(defcircuit make-point ((public  pub-point point)
                        (private sec-point point)
                        (output point))
  (def ((x (x pub-point))
        (y (y pub-point)))
    (point :x (+ x (x sec-point))
           :y (+ y (y sec-point)))))
```

notice how we have the local variable `x` and the function `x` which
grabs the `x` field off of the given point.

This concept also extends to types this is why in the
`point-constraint` example we are able to call a value of type
`point`, `point`.

## Common lisp Integration

Now it is time to talk about the common lisp (hereby known as lisp)
integration with alucard. This is the feature with elevates alucard
from being a low level language into a decently expressive language.

A good point to look at for integration, is that since alucard is just
lisp syntax, we invoke lisp functions the same way as we've been
seeing alucard functions execute!

That then begs the following questions:

1. 『When is the lisp integration useful?』
2. 『What are examples of lisp functions that are common in Alucard?』
3. 『What are the limitations/barriers of this integration?』

The first question is answered by how we've used lisp integration in
this syntax tutorial so far. Basically the lisp language is useful
when there is a problem we can't elegantly solve with top level gates,
local variable binding, nor structs.

This can range from:

- Repeat local patterns
- Wanting to derive constants
- Wanting to write the same pattern of code on `n` different wires.

The list can go on adinfinitium.

In this tutorial alone we've seen the first two bullet points be used.

As for the second question, we see the use of abstractions from lisp
like:

- `deflex`
- `flet`
- constant numerical operations
- high level iterators

Yes, the `deflex` we've seen at the start of the tutorial is in fact
just a normal CL lexical variable definition.

The only point not shown off yet is the `high level iterators` point,
and so let us show off this feature!

We want to write a circuit which takes 4 `point`'s. and computes some
constraint over them. We can break these points down into pairs, with
the first of the pair being the x-y plane the values represent, and
the second pair being a 2-d time point in which we represent time as a
two-dimensional value.

Thus we shall make the `nested` point. So that we only deal with two
arguments.

```lisp
(deftype nested ()
  (plane point)
  (time  point))
```

```lisp
(defcircuit constrain ((public  nest-pub nested)
                       (private nest-priv nested)
                       (output bool))
  (def ((plane      (plane nest-pub))
        (time       (time  nest-pub))
        (plane-priv (plane nest-priv))
        (time-priv  (time  nest-priv)))
    (= 0
       (+ (* (x plane)
             (y plane))
          (* (x time)
             (y time))
          (* (x time-priv)
             (y time-priv))
          (* (x plane-priv)
             (y plane-priv))))))
```

We can see that in our `constrain` circuit we wish to multiple the x-y
coordinates together, and adding them all together should equal zero.

We've already seen some techniques which can help the verbosity of
this code.


```lisp
(defcircuit constrain-2 ((public  nest-pub nested)
                         (private nest-priv nested)
                         (output bool))
  (flet ((multiply-points (point)
           (* (x point)
              (y point))))
    (def ((plane      (plane nest-pub))
          (time       (time  nest-pub))
          (plane-priv (plane nest-priv))
          (time-priv  (time  nest-priv)))
      (= 0
         (+ (multiply-points plane)
            (multiply-points time)
            (multiply-points plane-priv)
            (multiply-points time-priv))))))
```

This is better, but we still see the huge pattern of calling the same
function 4 times in a row. Thus we can golf this even further with a
few high level iteration abstractions in lisp.


```lisp
(defcircuit constrain-3 ((public  nest-pub nested)
                         (private nest-priv nested)
                         (output bool))
  (= 0 (apply #'+
              (mapcar (lambda (point)
                        (* (x point)
                           (y point)))
                      (list (plane nest-pub)
                            (time  nest-pub)
                            (plane nest-priv)
                            (time  nest-priv))))))
```


Here we used quite a few functions from CL. I should explain them all
in full.

`list` simply puts the items into a list

```lisp
;; make a list of 1 through 5!
(list 1 2 3 4 5)
```

So we put all our wires into a list

```lisp
'(list (plane nest-pub)
      (time  nest-pub)
      (plane nest-priv)
      (time  nest-priv))
```

Next, we use the function `mapcar`. We know this function from other
languages under the name `map`. In an invocation like, `(map f list)`,
`map` applies `f` to every element in the list.

In this case we are applying `lambda`. Lambda is just a function with
no name, we could have just as easily said

```lisp
(defcircuit constrain-4 ((public  nest-pub nested)
                         (private nest-priv nested)
                         (output bool))
  (labels ((multiply-points (point)
             (* (x point)
                (y point))))
    (= 0 (apply #'+
                (mapcar #'multiply-points
                        (list (plane nest-pub)
                              (time  nest-pub)
                              (plane nest-priv)
                              (time  nest-priv)))))))
```

replacing the lambda with `multiply-points`. Note that we use
`#'multiply-points` as since functions and variables are in different
namespaces the language needs to know we are talking about the
function `multiply-points`.

Lastly we call `(apply #'+ ...)` on our list. All apply does is call
the given function on the given list, to illustrate the point, let us
write some examples

```lisp
(apply #'cl:+ (list 1 2 3 4 5))
;; we get back 15.

(apply #'cl:* (list 1 2 3 4 5))
;; we get back 120.
;; we've practically made factorial!
```

A good way to visualize it is to just move the ()'s over a bit

```lisp
(apply #'cl:+ (list 1 2 3 4 5))
;; ==> (cl:+ 1 2 3 4 5)

(apply #'cl:* (list 1 2 3 4 5))
;; ===> (cl:* 1 2 3 4 )
```

With these functions down, we can see how we got from `constrain-2` to
`constrain-3`.

It is somewhat questionable if there is a huge readability boon
between `constrain-2` vs `constrain-3`. with that said, when more
complex patterns show up in code, techniques like this can help you
reduce the complexity of your alucard code quite a bit.

In the future, when alucard has an array type, operations like `map`
which takes a function will be provided by easy to write lisp
functions rather than directly in alucard!

## Incomplete List Of Useful Functions

We shall end this interactive tutorial by telling you about various
useful standard library functions.

NOTE we don't actually have this list compiled yet!

Happy hacking!
