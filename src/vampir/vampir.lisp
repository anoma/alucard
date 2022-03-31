(in-package :alu.vampir)

;; Since slime/sly screws with pprint-new, what this means is that I
;; can't track newlines automatically and even enforce good
;; indentation automatically ☹

;; We use CL streams as they are much better for concatenating to, and
;; have us worry less. they are a mutable interface however.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TopLevel Extraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> extract (list &optional stream) stream)
(defun extract (stmts &optional (stream *standard-output*))
  (let ((last (car (last stmts))))
    (dolist (stmt stmts stream)
      (extract-statement stmt stream)
      (unless (eq last stmt)
        (format stream "~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Statement Extraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> extract-statement (spc:statement &optional stream) stream)
(defun extract-statement (stmt &optional (stream *standard-output*))
  (etypecase-of spc:statement stmt
    (spc:alias      (extract-alias stmt stream))
    (spc:pub        (extract-pub stmt stream))
    (spc:constraint (extract-constraint stmt stream))))

(-> extract-pub (spc:pub &optional stream) stream)
(defun extract-pub (pub &optional (stream *standard-output*))
  (format stream "~{pub ~(~a~)~^~%~}" (spc:wires pub))
  stream)

(-> extract-alias (spc:alias &optional stream) stream)
(defun extract-alias (alias &optional (stream *standard-output*))
  (format stream "def ~(~a~)~{ ~(~a~)~} " (spc:name alias) (spc:inputs alias))

  (when (spc:outputs alias)
    (format stream "->~{ ~(~a~)~} " (spc:outputs alias)))

  (format stream "{")
  (extract-constraint-list (spc:body alias) stream)
  (format stream "~%}")

  stream)

(-> extract-constraint-list (spc:constraint-list &optional stream) stream)
(defun extract-constraint-list (cs &optional (stream *standard-output*))
  (dolist (constraint cs stream)
    (format stream "~%~2t")
    (extract-constraint constraint stream)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Constraint Extraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> extract-constraint (spc:constraint &optional stream) stream)
(defun extract-constraint (constraint &optional (stream *standard-output*))
  (etypecase-of spc:constraint constraint
    (spc:application (extract-application constraint 2 stream))
    (spc:bind        (extract-bind        constraint 2 stream))
    (spc:equality    (extract-equality    constraint 2 stream)))
  stream)

(-> extract-bind (spc:bind &optional fixnum stream) stream)
(defun extract-bind (bind &optional (indent 2) (stream *standard-output*))
  (dolist (normal (spc:names bind))
    (extract-normal-form normal stream))
  (format stream " = ")
  (extract-expression-no-paren (spc:value bind) (+ 2 indent) stream))

(-> extract-equality (spc:equality &optional fixnum stream) stream)
(defun extract-equality (eql &optional (indent 2) (stream *standard-output*))
  (extract-expression-no-paren (spc:lhs eql) indent stream)
  (format stream " = ")
  (extract-expression-no-paren (spc:rhs eql) (+ 2 indent) stream))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expression Extraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> extract-expression (spc:expression &optional fixnum stream) stream)
(defun extract-expression (expr &optional (indent 2) (stream *standard-output*))
  ;; For safety reasons we wrap most things in ()'s as expressions
  ;; show up as part of a constraint.
  ;; recalculate indent levels here
  (etypecase-of spc:expression expr
    (spc:infix
     (write-char #\( stream)
     (extract-infix expr indent stream)
     (write-char #\) stream))
    (spc:application
     (write-char #\( stream)
     (extract-application expr indent stream)
     (write-char #\) stream))
    (spc:normal-form
     (extract-normal-form expr stream)))
  stream)

(-> extract-expression-no-paren (spc:expression &optional fixnum stream) stream)
(defun extract-expression-no-paren (expr &optional (indent 2) (stream *standard-output*))
  ;; For safety reasons we wrap most things in ()'s as expressions
  ;; show up as part of a constraint.
  ;; recalculate indent levels here
  (etypecase-of spc:expression expr
    (spc:infix       (extract-infix expr indent stream))
    (spc:application (extract-application expr indent stream))
    (spc:normal-form (extract-normal-form expr stream))))

(-> extract-infix (spc:infix &optional fixnum stream) stream)
(defun extract-infix (infix &optional (indent 2) (stream *standard-output*))
  (extract-expression (spc:lhs infix) indent stream)
  (format stream " ~A " (spc:op infix))
  (extract-expression (spc:rhs infix) indent stream)
  stream)

(-> extract-application (spc:application &optional fixnum stream) stream)
(defun extract-application (application &optional (indent 2) (stream *standard-output*))
  ;; TODO :: put indentation in application if it gets long plz
  (format stream "~(~a~)" (spc:func application))
  (dolist (expr (spc:arguments application) stream)
    (format stream " ")
    (extract-expression expr indent stream)))

(-> extract-normal-form (spc:normal-form &optional stream) stream)
(defun extract-normal-form (normal &optional (stream *standard-output*))
  (etypecase-of spc:normal-form normal
    (spc:wire
     (extract-wire normal stream))
    (spc:constant
     (extract-constant normal stream))))

(-> extract-wire (spc:wire &optional stream) stream)
(defun extract-wire (wire &optional (stream *standard-output*))
  (format stream "~(~a~)" (spc:var wire))
  stream)

(-> extract-constant (spc:constant &optional stream) stream)
(defun extract-constant (constant &optional (stream *standard-output*))
  (format stream "~A" (spc:const constant))
  stream)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defparameter *x* (make-string-output-stream))

;; (pprint-defun '(defun prod (x y) (let ((yjaskdfjksadjkf y)) (* x y))))

;; (format t "~vt~a~%" 2 "hi")

;; (format t (get-output-stream-string *x*))
