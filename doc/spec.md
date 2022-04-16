###### tags: `Alucard`

# Alucard Specification

This document lays out the specification and documentation for the
various items found in the Alucard programming language.

This document also serves as a companion piece to a more detailed
syntax guide hosted <link here>

### Interface to Alucard

Alucard is a language that can be played around with to see the fruits
of one's labor sooner rather than later

#### Scripting Mode (PARTIALLY IMPLEMENTED)

#### File Loading Mode (NOT IMPLEMENTED)


### Data Types

Alucard provides a variety of types, which can be broken up into three
broad groups:

1. Numerical types
2. Arrays
3. User defined data types

#### Numeric Types

Numerical types are the bread and butter

##### Fields Elements

The Field data type is intended to represent a positive element of a
finite field. Thus the value can be in `[0, p - 1]` where `p` is a
large prime number.

Overflow is thus at `p` and thus `0 - 1 ≡ p`

##### Integers

<!-- Should we just name ℤ to ℕ instead? -->

The Integer type is a constrained field element to be within some bound.

For example if we have a circuit where we have `x` of type `(int 32)`,
then `x` can have values between `0` and `2^32 - 1` (`[0, 2^32 - 1]`).

To make working with standard integer types common the following are
defined as short hand:

- `int64`
- `int32`
- `int16`
- `int8`
- `bool` ; To be thought of as `(int 2)`

With the more general form being invoked like `(int k)` where `k` is
some natural thus:

- `(int 9)` ; for integers `[0, 2^32 - 1]`
- `(int 63)`
- etc. etc. etc.

##### Booleans


<!-- Make a proper boolean sum type when we get those in -->

Boolean types can be thought of as a field element that is constrained
to be either be `0` for false, and `1` for true

#### Arrays (NOT IMPLEMENTED)

Arrays

#### User Defined Types

Users can define more complex types with the `deftype` construct

##### Structs

the

#### Array Types (NOT IMPLEMENTED)

### Standard Library Functions

### Utilizing Common Lisp for Higher Level Code

### Documentation and Specification of Specific Terms

#### Defcircuit

Defcircuit defines a custom gate that can be applied to other alucard
values.

```bnf
(defcircuit name (<parameter>* <return>) body)
```

evaluating a `defcircuit` causes the `name` to be bound as a function
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

An example use of difcircuit:

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
`x` of type int and returns the type of `bool`.

The rest of the body is a list of expressions, in this example we
simply state the relation `0 = x^3 + 3 * x^2 + 2 * x + 4` .

#### Defgate (NOT IMPLEMENTED)

#### Deftype

Deftype defines a custom record type that wraps around

The deftype construct is the way to make a new custom user data
type. The data types

```ebnf=
(deftype name () <type-field>*)
```

```ebnf=
(field-name <type>)
```

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
