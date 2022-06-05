(in-package :alu.spec)

(deftype type-reference ()
  "When we refer to the type in the language it will be through the type
reference. If we are apply a type, then "
  `(or reference-type
       ;; can be found in alu/term.lisp
       application))

;; dispatch-case has issue with sub checking so we inline type-reference below
(deftype type-reference-full ()
  "This handles the case of references to types and what they may be
applied upon"
  `(or reference-type application number))

(defclass reference-type ()
  ((name :initarg  :name
         :type     keyword
         :accessor name
         :documentation "Type reference"))
  (:documentation "Represents a variable in the Alucard language"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Array Functioanlity
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> array-type (&key (:length fixnum) (:type type-reference)) application)
(defun array-type (&key length type)
  (values
   (make-application :function  (make-type-reference :name :array)
                     :arguments (list length type))))


(-> array-type-len (application) fixnum)
(defun array-type-len (arr)
  (car (arguments arr)))

(-> array-type-content (application) type-reference)
(defun array-type-content (arr)
  (cadr (arguments arr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                     Extra Functionality On Types                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun to-type-reference-format (term)
  "Given an application or a symbol, transform it to the correct type
storage format. So for example

1. int      -> (make-type-reference :name :int)
2. (int 64) -> (make-application :name (make-type-reference :name :int)
                                 :arguments (list 64))"
  ;; can either be a list number or atom
  (cond ((listp term)
         (let ((type-ref (mapcar #'to-type-reference-format term)))
           (make-application :function (car type-ref) :arguments (cdr type-ref))))
        ((numberp term)
         term)
        (t
         (make-type-reference :name (alu.utils:symbol-to-keyword term)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                       Type Declaration Functions                           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference Functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod print-object ((obj reference-type) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (name obj))))

(defun make-type-reference (&key name)
  (make-instance 'reference-type :name name))
