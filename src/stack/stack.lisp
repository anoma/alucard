(in-package :alu.stack)

(defclass stack ()
  ((current-section
    :initarg  :current-section
    :accessor current-section
    :type     `(or nil section)
    :initform nil
    :documentation "the current section to push into if there even is one")
   (stack :initarg  :stack
          :initform nil
          :accessor stack
          :type     list
          :documentation "The Current calls ")))

(defclass section ()
  ((name :initarg :name
         :initform :section
         :accessor name
         :type symbol
         :documentation "The name of the current section")
   (stack :initarg :stack
          :initform nil
          :accessor stack
          :type list)))

(defmethod print-object ((obj stack) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (when (current-section obj)
      (format stream "~A~:@_" (current-section obj)))
    (format stream "~{~A~^~:@_~}" (stack obj))))

(defmethod print-object ((obj section) stream)
  (pprint-logical-block (stream nil :prefix "(" :suffix ")")
    (format stream ":IN ~A" (name obj))
    (format stream "~{~:@_~A~^~}" (stack obj))))

(defparameter *stack* (ref:ref (make-instance 'stack))
  "Global stack that operands will be pushed to")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mutable interface toe the functional data structure
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun push (x &optional (stack *stack*))
  (setf (ref:! stack)
        (cons x (ref:! stack))))

(-> push-section (symbol &optional ref:ref) stack)
(defun push-section (name &optional (stack *stack*))
  (setf (ref:! stack)
        (new-section name (ref:! stack))))

(defun pop (&optional (stack *stack*))
  (setf (ref:! stack)
        (cdr (ref:! stack))))

(-> pop-section (&optional ref:ref) stack)
(defun pop-section (&optional (stack *stack*))
  (setf (ref:! stack)
        (cdr-current-section (ref:! stack))))

(defun get (&optional (stack *stack*))
  (ref:! stack))

(defun new ()
  (ref:ref (make-instance 'stack)))

(defmacro with-empty-stack (() &rest body)
  `(let ((*stack* (new)))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pure functions on stacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> cons (symbol stack) stack)
(defun new-section (name stack)
  (with-accessors ((stack stack) (current current-section)) stack
    (let ((new-stack
            (if current
                (cl:cons current stack)
                stack)))
      (make-instance 'stack :stack           new-stack
                            :current-section (make-instance 'section :name name)))))

(-> cons (t stack) stack)
(defun cons (x stack)
  (with-accessors ((stack stack) (current current-section)) stack
    (if current
        (make-instance 'stack :stack           stack
                              :current-section (cons-section x current))
        (make-instance 'stack :stack           (cl:cons x stack)
                              :current-section current))))
(-> cdr (stack) stack)
(defun cdr (stack)
  (with-accessors ((stack stack) (current current-section)) stack
    (cond ((and current (not (null (stack current))))
           (make-instance 'stack :stack stack
                                 :current-section (cdr-section current)))
          (current
           (make-instance 'stack :stack stack
                                 :current-section nil))
          ((typep (car stack) 'section)
           (cdr (make-instance 'stack :stack (cl:cdr stack)
                                      :current-section (car stack))))
          (t
           (make-instance 'stack :stack (cl:cdr stack)
                                 :current-section nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions on Sections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> cons-section (t section) section)
(defun cons-section (x section)
  (with-accessors ((name name) (stack stack)) section
    (make-instance 'section :stack (cl:cons x stack) :name name)))

(-> cdr-section (section) section)
(defun cdr-section (section)
  (with-accessors ((name name) (stack stack)) section
    (make-instance 'section :stack (cl:cdr stack) :name name)))


(-> cdr-current-section (stack) stack)
(defun cdr-current-section (stack)
  (with-accessors ((stack stack)) stack
    (if (typep (car stack) 'section)
        (make-instance 'stack :stack           (cl:cdr stack)
                              :current-section (car stack))
        (make-instance 'stack :stack           stack
                              :current-section nil))))
