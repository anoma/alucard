(in-package :alu.spec)

(deftype type-reference ()
  "When we refer to the type in the language it will be through the type
reference. If we are apply a type, then "
  `(or reference-type
       ;; can be found in alu/term.lisp
       application))

(defclass reference-type ()
  ((name :initarg  :name
         :type     keyword
         :accessor name
         :documentation "Type reference"))
  (:documentation "Represents a variable in the Alucard language"))

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