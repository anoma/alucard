(in-package :alu.stepper)

;; The strategy we wish to implement is straight forward.

;; (step (macro ...)) ⟶ we macroexpand the macro
;;
;; (step (special-form ...)) ⟶ We handle this on a case by case basis
;;
;; (step (alucard-special-macro ...)) ⟶ Handle the same as special-form
;;
;; (step (function ...)) ⟶ generates out
;;   (prog2 (push (function …)) (function ,@(map car #'step …)) (pop))
;; (step number) ⟶ generates: number
;; (step string) ⟶ generates: string
;; (step symbol) ⟶ generates: symbol

;; For the list of special forms see
;; https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node59.html

;; to actually check one should use `special-operator-p' instead of
;; (typep obj 'specials).
(deftype specials ()
  "The special forms of the CL language"
  `(or ,@(mapcar
          (lambda (x) `(eql ,x))
          `(block catch eval-when flet function go if
                  labels let let* load-time-value locally
                  macrolet multiple-vlaue-call
                  multiple-value-prog1 progn progv
                  quote return-from setq symbol-macrolet
                  tagbody the throw unwind-protect))))

(deftype alu-specials ()
  "The special forms of the Alucard language"
  `(or (eql alu:def) (eql alu:with-constraint)
       ;; these we count as special due to the type declaration
       ;; forcing them to be macros
       (eql alu:coerce) (eql alu:check)
       (eql alu:array)))

(deftype step-mode ()
  `(or (eql :stack) (eql :run)))

(defvar *step-mode* :stack
  "Determines what mode to run in.
:stack put user syntactical forms on the stack.
:run   leaves the user program unperturbed.")


(defun step (form env)
  "Runs the stepper through the code, inserting stack traces if
*step-mode* is :stack"
  (cond
    ((eql *step-mode* :run)
     form)
    ;; maybe we should macroexpand symbol macros?
    ;; I don't think for our purposes it matters
    ((or (atom form) (symbolp form))
     form)
    ((special-operator-p (car form))
     (handle-cl-special form env))
    ((macro-function (car form) env)
     ;; we thus record any function and any macro expansion that has
     ;; been ran as well. This may be helpful, as we are likely going
     ;; to interpret the stack results. Thus if an error is caused in
     ;; an expansion but not the users code. it can give information
     ;; which macro expansion screwed up (namely by comparing the
     ;; final call to the form where it's first generated... by
     ;; checking if the car is a macro, and it's the first site it
     ;; doesn't show up.).
     (run-mode form
               (step (macroexpand-1 form env) env)))
    (t
     (step-function form env))))

(defun step-function (form env)
  "Runs the stepper through a function call."
  (run-mode form
            (mapcar (lambda (x) (step x env)) form)))

(defmacro with-stack (original-form continue-form)
  `(prog2 (stack:push ',original-form)
       ,continue-form
     (stack:pop)))

(defun run-mode (original-form continue-form)
  "runs the selected user mode."
  (etypecase-of step-mode *step-mode*
    ((eql :run)   original-form)
    ((eql :stack) `(with-stack ,original-form ,continue-form))))

(defun handle-cl-special (form env)
  (typecase-of specials (car form)
    ((eql let)       (handle-let form env))
    ((eql let*)      (handle-let form env))
    ((eql eval-when) (handle-eval-when form env))
    ((eql flet)      (handle-local-function :recursive nil))
    ((eql labels)    (handle-local-function :recursive t))
    ((eql macrolet)  (handle-local-function :recursive t :macro t))
    ((eql block))
    ((eql catch))
    ((eql symbol-macrolet))
    ((eql function))
    ((eql go))
    ((eql if))
    ((eql load-time-value))
    ((eql locally))
    ((eql multiple-vlaue-call))
    ((eql multiple-value-prog1))
    ((eql progn))
    ((eql progv))
    ((eql quote))
    ((eql return-from))
    ((eql setq))
    ((eql tagbody))
    ((eql the))
    ((eql throw))
    ((eql unwind-protect))
    (otherwise
     (error "special ~A not supported yet" (car form)))))

(defun handle-alu-special (form env)
  (typecase-of alu-specials (car form)
    ((eql alu:def)             (handle-let form env t))
    ((eql alu:with-constraint) (handle-constraint form env))
    ((eql alu:coerce))
    ((eql alu:check))
    ((eql alu:array))
    (otherwise (error "Alucard Special ~A handed to handle-alu special"
                      form))))

;; TODO :: Major Flaw
;;
;; Note Early:
;; for binders like let and flet we need to freeze with a lambda
;; technique.  generate out to a lambda call, further we should pass
;; around the environment so that we refer to the correct values. Or
;; rather we should continue in that lambda, thus generate out a
;; lambda to continue this evaluation. Rather cheeky all things considered
;;
;; Note Later:
;; seems like we can just use the env variable, and update it with
;; `cltl2:augment-environment' to get it to work

(defun handle-let (form env &optional handle-constrain)
  ;; we don't need to update the environment as we don't care about
  ;; symbols as much.
  (destructuring-bind (let args &rest body) form
    (list* let (handle-binder args handle-constrain) (handle-body body env))))

(defun handle-eval-when (form env)
  (destructuring-bind (eval-when declaration &rest body) form
    (list* eval-when declaration (handle-body body env))))

(defun handle-binder (binders env &optional handle-constrain)
  (mapcar (lambda (bind-pair)
            (if (and handle-constrain (eql (car bind-pair) 'alu:with-constraint))
                (handle-constraint bind-pair env)
                ;; TODO ∷ STEP is wrong here due to introducing a
                ;; binder which is not a macro
                (step bind-pair env)))
          binders))

(defun handle-local-function (form env &key recursive macro)
  "Handles functions like flet, labels, and macrolet.

:recursive   means that the binding is recursive and should be all
             considered together
:macro       means the binding form should be considered a macro. We
             assume the macro is also :recursive t
"
  (list form env macro recursive))

(defun handle-body (body env)
  "Handles a body that may have declarations upfront"
  (mapcar (lambda (x)
            (if (and (listp x) (declarationp x)) x (step x env)))
          body))

(defun handle-constraint (form env)
  "Handles an `alu:with-constraint' form"
  env
  form)

(defun declarationp (form)
  "determines if a form is a declaration or not"
  (or (eql (car form) 'declare)
      ;; probably overkill given declaims are top level
      (eql (car form) 'declaim)))
