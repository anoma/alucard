###### tags: `Alucard`

# Alucard Specification

This document lays out the specification and documentation for the
various items found in the Alucard(Alu) programming language.

This document also serves as a companion piece to:
- A detailed [Syntax Guide](https://hackmd.io/ko5FtwIaTwOiozDgkiLaWA?view).
  + This piece gives an interactive tutorial to the langugage, meaning
    that every piece of code in the tutorial can be run in the Alucard
    REPL.
  + The syntax guide introduces new concepts and continuously builds
    on new ones as the tutorial goes on.
  + The loadable file of the tutorial can be found [here in the docs
    folder](https://github.com/heliaxdev/alu/blob/master/doc/using-alucard.lisp).
- A [Reference Manual](https://hackmd.io/emeUBiYoSqmJ95Ls2wsrMQ)
  + This document gives a more practical overview of the functions
    given by Alucard.

<!-- TODO :: Change items to the list of items like datatypes, code forms, etc. -->

### Interface to Alucard
In order to give users of Alucard flexiblity, Alucard can be used
either in a traditional batch compiler or as a fully interactive
system.

#### Scripting Mode (PARTIALLY IMPLEMENTED)

Scripting mode is the default behavior when launching the alucard
program. It gives the user an interface to input any valid alu
expression.

Sadly out of the box the user interface (hereby known as the REPL)
does not support very good editor or auto-completion facilities. To
alleviate this we recommend:

1. Launching Alucard in your editor of choice (vscode, vim, emacs, etc.).
2. Running the cli in a program like `rlwrap`.

The Alucard user interface is in reality a thin sheen over the Common
Lisp REPL interface. Therefore you are able to reuse the tooling that
comes with a Common Lisp system.


Since the Common Lisp system is an interactive system, everytime one
uses editor integration to interact with the compiler, we are using
the scripting mode of the system.  From this scripting mode, the user
can write the same program they would have in batch compilation
mode. Further, since the tooling is integrated with one's editor they
can experiment with:

1. Running tests
2. Having variations of the same circuit to optimize the performance or
   to more rapidly investigate new ideas
3. Having graphical outputs of the circuit live as they edit
4. Have multiple different strategies to compare
5. etc.

<!-- Replace Etc with some more examples and non exhasutive -->

#### Batch Compilation Mode (PARTIALLY IMPLEMENTED)

The system can easily be used in batch mode, meaning that you can give
the compiler a file with the `-i` flag and tell it to produce a vampir
file with the `-o` flag.

```bash=
 % ./build/alu.image -i alu/example.lisp -o alu/exampale.vampir
```

If the `-o` flag is not given, then it will go into scripting mode.

### Data Types

Alucard provides a variety of types, which can be broken up into three
broad groups:

1. Numerical types
2. Arrays
3. User defined data types

#### Numeric Types

Numerical types are all variations on the finite field elements.

##### Fields Elements

The `Field` data type is intended to represent a positive element of a
finite field. The value can be in `[0, p - 1]` where `p` is a
large prime number.

Overflow is at `p` and thus `0 - 1 ≡ p`

##### Integers

<!-- Should we just name ℤ to ℕ instead? -->

The `Integer` type is a constrained field element to be within some bound.

For example if we have a circuit where we have `x` of type `(int 32)`,
then `x` can have values between `0` and `2^32 - 1` (`[0, 2^32 - 1]`).

To make working with standard integer types easier the following are
defined as short hand:

- `int64`
- `int32`
- `int16`
- `int8`
- `bool` ; To be thought of as `(int 2)`

With the more general form being invoked like `(int k)` where `k` is
some natural number:

- `(int 9)` ; for integers `[0, 2^32 - 1]`
- `(int 63)`

##### Booleans


<!-- Make a proper boolean sum type when we get those in -->

Boolean types can be thought of as a field element that is constrained
to either be `0` for false, and `1` for true.

#### Arrays (PARTIALLY IMPLEMENTED)

Arrays are a fixed length data type in Alucard. Arrays are defined in
terms of both the field type, and the size of the Array. Because the
type is defined over the size, arrays of different sizes are
considered different types and are incompatible.


<!-- Merge this sentence with the above one, we can talk about the
lookup before delving into the types being inco mpatable-->
Arrays have a lookup operation, and are indexed from 0.

Although Alucard functions take arrays of a specific size, generic
functions over any size can be defined over them on the Common Lisp
side. This is often seen in functions like `map` that can take any
size array.

An example definition is shown below for map:

```lisp
;; how do we determine the type of new-arr?
;; seems we need the type of the output of the lambda
(defun map (lamb array)
  (def ((length  (arr-length array))
        (new-arr (make-array :length length)))
    (dotimes (i length)
      (= (lookup new-array i)
         (funcall lamb (lookup array i))))
    new-arr))
```

#### User Defined Types

Users can define more complex types with the `deftype` construct.

##### Structs

Structs are very much like structs in the C programming language. They
are defined with a specified type and name. The name compiles to a
record lookup function, meaning that we can lookup the field by simply
stating `(name struct-instance)`.

<!-- If a struct is returned from the circuit then the wires out of
the circuit will be the fields of the struct ordered in how they were
defined.

Structs at a -->

#### Array Types (PARTIALLY IMPLEMENTED)

### Standard Library Functions

### Utilizing Common Lisp for Higher Level Code

### Documentation and Specification of Specific Terms

#### Defcircuit

Defcircuit defines a custom gate that can be applied to other Alucard
values.

```bnf
(defcircuit name (<parameter>* <return>) body)
```

Evaluating a `defcircuit` causes the `name` to be bound as a function
that is specified by the body with the parameters being in the lexical
scope. The body consists of any number of expressions (zero or
more). These expressions are evaluated in order and the result of the
last expression is returned unless the return type specifies
otherwise. The return type is currently required and specifies the
type given to `name`. Iff the return type specifies `void` as the
return type, then no values are returned from the function.

The complete syntax for parameterers is:

```bnf=
parameter ::= (<privacy> <symbol> <type>)
privacy   ::= public | private
```

The first value specifies whether that parameter is a public or
private input field. If another circuit is specified to be compiled
and that circuit calls this circuit, then the privacy of the
parameters are deferred to the privacy controls of that `defcircuit`.

The second value specifies the name of the parameter.

The third value specifies what type the parameter is considered to
be. Curently no inference is given and the type is required.

The complete syntax for returns is:

```bnf=
return ::= (return <type>)
```
The name return specifies the value as the return type.



```lisp=
(defcircuit poly-check ((public x int)
                        (output bool))
  (= (+ (exp x 3)
        (* 3 (exp x 2))
        (* 2 x)
        4)
     0))
```

In this example we define out `poly-check` which takes a public input
`x` of type `int` and returns the type of `bool`.

The rest of the body is a list of expressions, in this example we
simply state the relation `0 = x^3 + 3 * x^2 + 2 * x + 4`.

#### Defgate (NOT IMPLEMENTED)

#### Deftype

The deftype construct is the way to make a new custom user data
type. The data types

```lisp
(deftype point ()
  (x int)
  (y int))

(deftype nested ()
  (plane point)
  (time  point))
```

In the above example we make two rec


#### Def

#### Standard Library Functions
