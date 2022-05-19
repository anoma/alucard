(in-package :alu.typechecker.types)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Typing Context Structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass typing-context ()
  ((holes :initarg :holes
          :accessor holes
          :initform nil
          :type list                    ; list (list keywords)
          :documentation "Represents the holes to solve. List of keywords")
   (hole-info :initarg :hole-info
              :accessor hole-info
              :initform (closure:allocate)
              :type closure:typ         ; Closure:typ hole-information
              :documentation "Represents information about the holes that we know")
   (dependency :initarg :dependency
               :accessor dependency
               :initform (dependency:allocate)
               :type dependency:typ
               :documentation "Represents the constraint satisfaction mapping")
   (typing-closure :initarg :typing-closure
                   :accessor typing-closure
                   :initform (closure:allocate)
                   :type closure:typ    ; Closure:typ type-info
                   :documentation
                   "This is the typing closure for terms which we already know"))
  (:documentation "This represents the typing context the terms we are
analyzing belong in"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Known Type Cache
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct type-info
  "Type information of a fully realized type."
  ;; if we don't know the size quite yet it will be nil
  (size nil :type (or fixnum null))
  (type nil :type ir:type-reference))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Types about Holes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct hole-information
  (unrefined nil :type hole)
  ;; We should redefine term, to be a list of ir:term-no-binding
  ;; as we can be solved by various sets of equations.
  ;;
  ;; What I mean is that if we have `x = some equation', and then
  ;; later `y = x' where we solve for `y', then we've solved for `x'.
  ;;
  ;; Thus the hole-information should be (list equation #<reference y>)
  ;;
  ;; And when our dependency closure says we've solved it, try the
  ;; list until we get the equation that satisfies the constraint.
  (term nil :type list))

;; TODO :: with generics we should make this type a lot more rich and
;;         informative
(deftype hole ()
  "Represents the format of holes that have yet to be fully realized."
  `(or null keyword))

(deftype hole-conditions ()
  "The conditions in which a fialure can happen for "
  `(or same-as
       (eql :refine-integer)
       depends-on))

;; infer-from
(defstruct same-as
  "Represents that the hole is the same as this other variable"
  (value (error "fill in the value") :type keyword))

(defstruct depends-on
  "this represents that the value is tied to the list of values in some
way."
  (value nil :type list))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Types about Querying Types
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype current-information ()
  "Represents the current knowledge we have on a given type. Thus a
hole, or if the type is known a type reference"
  `(or hole ir:type-reference))

(deftype lookup-type ()
  "represents the potential type of a reference.

_It can either be_
1. known

2. an unrefined value
  - which we represent with a keyword.
  - TODO :: Once we update with generics, we should move this to a
    `ir:type-reference'

3. unknown
  - which we represent with null."
  `(or type-info hole))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data Types about known type primitives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype known-primitve-types ()
  `(or (eql :int)
       (eql :bool)
       (eql :void)))

(deftype known-primitve-functions ()
  `(or (eql :+)
       (eql :*)
       (eql :=)
       (eql :exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on Typing Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj typing-context) stream)
  (print-unreadable-object (obj stream :type t)
    (pprint-logical-block (stream nil)
        (format stream ":HOLES ~A~_:HOLE-INFO ~A~_:DEPENDENCY ~A~_:TYPING-CLOSURE ~A"
                (holes obj) (hole-info obj) (dependency obj) (typing-closure obj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operations on hole Info
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> add-hole-formula (hole-information alu.spec:term-no-binding) hole-information)
(defun add-hole-formula (hole new-formula)
  (make-hole-information :unrefined (hole-information-unrefined hole)
                         :term (cons new-formula (hole-information-term hole))))
