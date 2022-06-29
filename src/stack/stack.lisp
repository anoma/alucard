(in-package :alu.stack)

(defparameter *stack* (ref:ref nil)
  "Global stack that operands will be pushed to")

(defun push (x &optional (stack *stack*))
  (setf (ref:! stack)
        (cons x (ref:! stack))))

(defun get (&optional (stack *stack*))
  (ref:! stack))

(defun pop (&optional (stack *stack*))
  (setf (ref:! stack)
        (cdr (ref:! stack))))

(defun new ()
  (ref:ref nil))

(defmacro with-empty-stack (() &rest body)
  `(let ((*stack* (new)))
     ,@body))

;; Deprecated idea, committing for posterity

(defparameter *already?* nil)

(defmacro stack-compiler-macro (name)
  `(define-compiler-macro ,name (&whole form &rest args)
     "Adds pushing stack traces to the function when it can be
     expanded"
     (declare (ignorable args))
     (let ((previous-expansion *already?*))
       (setf *already?* form)
       (if (eq form previous-expansion)
           form
           `(prog2
             (push ',form)
             ,form
             (format t "form: ~A~%" ',form)
             (pop))))))

;; (defun square-1 (x)
;;   ;; (format t "evaled: ~A~%" (stack:get))
;;   (* x 3))

;; (defun shaz (x)
;;   ;; (format t "evaled: ~A~%" (stack:get))
;;   (* x 3))

;; (stack-compiler-macro square-1)
;; (stack-compiler-macro shaz)

;; (defun test-square (x)
;;   ;; (format t "~A~%" (apply #'square-1 (list x)))
;;   ;; (format t "~A~%" (funcall #'square-1 x))
;;   (format t "~A~%" (square-1 (square-1 x))))

;; (shaz (shaz (shaz 3)))

;; (shaz
;;  (* (square-1 3) 3))

;; (let ((test (square-1 7)))
;;   test)

;; (defparameter *x* 5)
;; (square-1 *x*)
;; (square-1 5)


;; (apply #'square-1 (list 3))
