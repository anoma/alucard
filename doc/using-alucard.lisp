(in-package :aluser)

;; This is a comment!

;; All what you are about to see are valid expressions to write

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Numbers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are valid numbers

12
3
4

4294967296

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a constant declaration

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

;; Alucard supports full unicode in the name, and names are broken up
;; by spaces. However note that some symbols have special meaning and
;; thus can not be used.

;; These are

;; ()
;; '
;; `
;; ,

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; In depth variable naming!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'unevaluated-symbol!

`also-unevaluated


(deflex evaluated 5)

`,evaluated


`(+ ,evaluated 10)

;; notice that when we run this we get

;; (+ 5 10)!!!!

;; |SYMBOL| = symbol!

|EVALUATED|

;; Typically though the naming convention for symbols is

'dash-case

;; We will in this document often write ' before an expression to
;; essentially comment it out.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; We can make Alucard expressions this way!

(+ 1 2)

(+ 1 2 3 4 5 6 7)

;; (+ 1 2 3 4 5 6 7)
;;  ^ ^           ^
;;  | | arguments |
;;  |
;;  The function

;; () means begin a function call, with the first item between the ()
;; standing for the function, and the rest of the contents considered
;; the arguments. We will refer to the arguments often like "the first
;; argument" this shall refer to the first item after the function, in this case 1.

;; Thus the seventh argument in this case is: 7!

;; Note as we can see this makes a circuit application, with the
;; numbers as wires, not as literals!

;; However we can do literal addition, by invoking `cl:+', to make a
;; new constant value!

(cl:+ 1 2 3 4)

;; This value comes from the host language Common Lisp, we will show
;; this integration more later on!

;; We can even store these values in a constant declaration

;; NOTE ∷ Please come up with a more realistic example for the domain!

(deflex vector-size 16
  "The size of the vector for our blake2s compression function")

(deflex work-vector (cl:/ vector-size 2)
  "work-vector-split")

;; We can nest Alucard expressions

(+ 1
   2
   3
   (* 1 2 3))

;; If you've been typing this into the REPL, you'll notice you'll get
;; funky looking returns like
;;
;; NOTE ∷ Please make the pretty printer better, this is ugly
;;
;; #<#<REFERENCE +> 1 2 3 #<#<REFERENCE *> 1 2 3>>
;;
;; This is because there is no circuit interpreter to evaluate these
;; to their proper values yet, however once there is this show just a
;; normal value!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declaring Custom gates
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Since Alucard compiles to vamp-ir, a zksnark system over PlonkUp,
;; we can define our own custom circuits/gate

(defcircuit my-constraint-gate ((public input field)
                                (private secret-1 field)
                                (private secret-2 field)
                                (output bool))
  (= input
     (+ (* (exp secret-1 2)
           5)
        (* secret-2 7)
        5)))


;; Lets break this down

;; `defcircuit' declares that we wish to make a new gate/circuit.

;; The first argument `my-constraint-gate', refers to the name of the
;; new gate. We can use it as if it a primitive!

;; The second argument
;;
;; ((public input field)
;;  (private secret-1 field)
;;  (private secret-2 field)
;;  (output bool))
;;
;; Is the parameter list and output declaration to the custom circuit,
;; a good way to read the contents of the argument list is as follows.
;;
;; (privacy wire-name type)
;;
;; So in this case we can see that input is the public input of the
;; circuit, and it's type is that of a polynomial field element.
;;
;; The Circuit also takes two private wires, secret-1 and secret-2.
;;
;; Finally the last field of the list is `(output bool)', which
;; states that the circuit returns a bool type

;; Arguments after the 2nd are the body of the circuit, and in it we
;; can invoke expressions like we've been doing this entire article!

;; In this case we are just stating the equation
;;
;; input = secret-1^2 * 5 + secret-2 * 7 + 5
;;
;; where the input is constrained to be equal to the equation on the
;; right hand side.

;; we could similarly write it in a 1 line fashion

'(= input (+ (* (exp secret-1 2) 5) (* secret-2 7) 5))

;; Forming this much shorter Statement!
(defcircuit my-constraint-gate-again! ((public input field)
                                       (private secret-1 field)
                                       (private secret-2 field)
                                       (output bool))
  (= input (+ (* (exp secret-1 2) 5) (* secret-2 7) 5)))


;; We can call our circuits like we've been calling + and *. We can
;; even pass it around as such!

(my-constraint-gate 234 (exp 2 31) (* 5 27))


(defcircuit constrain-square ((public input field)
                              (private sec1 field)
                              (private sec2 field)
                              (output bool))
  (and (my-constraint-gate input sec1 sec2)
       (my-constraint-gate input sec1 (exp sec2 2)))) ; sec2^2 = sec2!

;; If you've been following around with alu editor integration™, then
;; you can even jump to definition to any non local variable!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Local Wire Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Alucard also gives ways of naming intermediate wires, lets write
;; constrain-square again but using `def' which is our way of writing
;; intermediate circuits/values

(defcircuit constrain-square% ((public input field)
                               (private sec1 field)
                               (private sec2 field)
                               (output bool))
  (def ((constraint-1 (my-constraint-gate input sec1 sec2))
        (constraint-2 (my-constraint-gate input sec1 (exp sec2 2))))
    (and constraint-1 constraint-2)))

;; Looking at this code, we've done some amount of repetition, which
;; is rather unfortunate. Alucard itself can't abstract this out,
;; however if we call into the host language of common lisp we can get
;; rid of the pattern of

;; (my-constraint-gate input sec1 <second-constraint>)

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

;; `flet' in Common lisp simply makes a function, in this case we make
;; `call-constraint' which takes 1 argument `var-secret', then calls
;; `my-constraint-gate' over the arguments

;; Notice how this call looks like everything else we've done in this
;; file so far!

;; The one caveat of this integration is that `call-constraint' can't
;; be returned from `constrain-square%%', this is because our Alucard
;; language has no function types!

;; We will cover the ins and outs of the integration after the next
;; section on types

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Type Declaration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Arrays would go here once they are in!

;; The last major piece of the alucard language is custom user data
;; types.

;; An example:

(deftype utxo ()
  (owner  (int 128))
  (amount (int 64))
  (nonce  (int 64)))

;; Here we declare the type `utxo' with the fields `owner', `nonce',
;; and `amount'. All the values are various sizes of ints.

;; We can make an utxo like so

(utxo :owner 128                        ; the owner is the value 128
      :amount 350                       ; the amount include is 350
      :nonce 120)                       ; and the nonce is 120!

;; We can access the fields like so

(defcircuit utxo-constraint ((public utx utxo)
                             (private var field)
                             (output bool))
  (= var (+ (amount utx) (nonce utx))))

(deftype point ()
  (x int)
  (y int))

(defcircuit point-constraint ((public  pub-point point)
                              (private sec-point point)
                              (output point))
  (def ((x     (x pub-point))
        (y     (y pub-point))
        (x-sec (x sec-point))
        (y-sec (y sec-point)))
    (point :x (+ x x-sec) :y (+ y y-sec))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The Namespaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; We can just call the names `amount' and `nonce' to access those
;; fields respectively. Also note that functions and variables lives
;; in different namespaces, so the following is valid

(def ((utxo  (utxo :owner 128 :amount 350 :nonce 120))
      (value (amount utxo)))

  (= (+ value (amount (utxo :owner 250 :amount 350 :nonce 125)))
     (exp value 2)))

;; Besides just the namespace for functions and variables/values,
;; there is also a separate namespace for types. Thus when we define
;; the `utxo' type we get the type `utxo' and the function `utxo'.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common Lisp Integration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Now it is time that we talk about the Common Lisp integration of
;; Alucard. This is the feature that elevates the Alucard from being a
;; low level DSL for circuits into being a decently high level
;; language.

;; You can see this is where the markdown file took precedence in
;; writing

(deftype nested ()
  (plane point)
  (time  point))

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


