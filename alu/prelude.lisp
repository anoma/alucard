(in-package :alu.prelude)

(defprimitive-type int)
(defprimitive-type int64)
(defprimitive-type void)
(defprimitive-type bool)
(defprimitive-type array)

(defprimitive void)
(defprimitive +)
(defprimitive *)
(defprimitive =)
(defprimitive range)
(defprimitive exp)

(defmacro deflex (var &body (&optional val documentation))
  ;; documentation copied from def in serpaeum
  "The famous \"deflex\".

Define a top level (global) lexical VAR with initial value VAL,
which is assigned unconditionally as with DEFPARAMETER. If a DOC
string is provided, it is attached to both the name |VAR| and the name
*STORAGE-FOR-DEFLEX-VAR-|VAR|* as a documentation string of kind
'VARIABLE. The new VAR will have lexical scope and thus may be
shadowed by LET bindings without affecting its dynamic (global) value.

The original `deflex' is due to Rob Warnock.

This version of `deflex' differs from the original in the following ways:

- It is possible for VAL to close over VAR.
- On implementations that support it (SBCL, CCL, and LispWorks, at the
moment) this version creates a backing variable that is \"global\" or
\"static\", so there is not just a change in semantics, but also a
gain in efficiency.
- If VAR is a list that starts with `values`, each element is treated as
a separate variable and initialized as if by `(setf (values VAR...)
VAL)`."
  `(serapeum:def ,var ,val ,documentation))

(defmacro vampir (function-name)
  `(vampir-keyword ,(util:symbol-to-keyword function-name)))

(defun vampir-keyword (keyword)
  (let ((lookup (storage:lookup-function keyword)))
    (if lookup
        (pipeline:pipeline lookup)
        (format t "Function ~A is not found" keyword))))

(defun start-slynk (&key (port 4005))
  ;; doesn't work in CCL for some reason...
  (slynk-api:add-hook slynk-api:*new-connection-hook*
                      (lambda (connection)
                        (declare (ignore connection))
                        (slynk:set-package :aluser)
                        (setf *print-pretty* t)))
  (slynk:create-server :port port :dont-close t))

(defun start-swank (&key (port 4005))
  ;; this maybe works?
  (swank::add-hook swank::*new-connection-hook*
                   (lambda (connection)
                     (declare (ignore connection))
                     (slynk:set-package :aluser)
                     (setf *print-pretty* t)))
  (swank:create-server :port port :dont-close t))
