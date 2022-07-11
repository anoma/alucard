(in-package :alu.stepper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Comments on Techniques
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Type Declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; to actually check one should use `special-operator-p' instead of
;; (typep obj 'specials).
(deftype specials ()
  "The special forms of the CL language"
  `(or ,@(mapcar
          (lambda (x) `(eql ,x))
          `(block catch eval-when flet function go if
                  labels let let* load-time-value locally
                  macrolet multiple-value-call
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

(deftype mode ()
  `(or (eql :stack) (eql :run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Global Value Declarations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *mode* :stack
  "Determines what mode to run in.
:stack put user syntactical forms on the stack.
:run   leaves the user program unperturbed.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main Logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun step (form env)
  "Runs the stepper through the code, inserting stack traces if
*mode* is :stack"
  (cond
    ((eql *mode* :run)
     form)
    ;; maybe we should macroexpand symbol macros?
    ;; I don't think for our purposes it matters
    ((or (atom form) (symbolp form))
     form)
    ;; Work on our own specials, we should treat these with care!
    ((typep (car form) 'alu-specials env)
     (run-mode form
               (handle-alu-special form env)))
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
    ((special-operator-p (car form))
     ;; we will also record these, we will likely filter forms like
     ;; `progn' from our traces, or figure out how code relates and
     ;; use ... to represent the part the user wrote.
     (run-mode form
               (handle-cl-special form env)))
    (t
     (step-function form env))))

(defun step-function (form env)
  "Runs the stepper through a function call."
  (run-mode form
            (mapcar (lambda (x) (step x env)) form)))

(defun step-body (body env &key (handle-declaration t))
  "Handles a body that may have declarations upfront"
  ;; Maybe I should mark what argument number each are for better
  ;; tracing Further after the first non declaration, we should stop
  ;; checking for it!
  (mapcar (lambda (x)
            (if (and handle-declaration (listp x) (declarationp x))
                x
                (step x env)))
          body))

;; we make this macro to just make the generated code pretty
(defmacro with-stack (original-form continue-form)
  `(prog2 (stack:push ',original-form)
       ,continue-form
     (stack:pop)))

(defun run-mode (original-form continue-form)
  "runs the selected user mode."
  (etypecase-of mode *mode*
    ((eql :run)   original-form)
    ((eql :stack) `(with-stack ,original-form ,continue-form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alias Exports
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defalias single #'step)
(defalias body   #'step-body)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special Case Handling Dispatch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-cl-special (form env)
  (typecase-of specials (car form)
    ((eql let)                  (handle-let form env))
    ((eql let*)                 (handle-let form env))
    ((eql eval-when)            (handle-eval-when form env))
    ((eql flet)                 (handle-local-function form env :recursive nil))
    ((eql labels)               (handle-local-function form env :recursive t))
    ((eql macrolet)             (handle-local-function form env :recursive t :macro t))
    ((eql if)                   (handle-if form env))
    ((eql progn)                (handle-progn form env))
    ((eql progv)                (handle-progv form env))
    ((eql block)                (handle-block form env))
    ((eql catch)                (handle-catch form env))
    ((eql function)             (handle-function form env))
    ((eql go)                   (handle-go form env))
    ((eql quote)                (handle-quote form env))
    ((eql return-from)          (handle-return form env))
    ((eql load-time-value)      (handle-load-time-value form env))
    ((eql symbol-macrolet)      (handle-symbol-macrolet form env))
    ((eql locally)              (handle-locally form env))
    ((eql multiple-value-call)  (handle-multiple-value-call form env))
    ((eql multiple-value-prog1) (handle-multiple-value-prog1 form env))
    ((eql setq)                 (handle-setq form env))
    ((eql tagbody)              (handle-tagbody form env))
    ((eql the)                  (handle-the form env))
    ((eql throw)                (handle-throw form env))
    ((eql unwind-protect)       (handle-unwind-protect form env))
    (otherwise
     (error "special ~A not supported yet" (car form)))))

(defun handle-alu-special (form env)
  (typecase-of alu-specials (car form)
    ((eql alu:def)             (handle-let form env t))
    ((eql alu:with-constraint) (handle-constraint form env))
    ((eql alu:coerce)          (handle-coerce form env))
    ((eql alu:check)           (handle-check form env))
    ((eql alu:array)           (handle-array form env))
    (otherwise (error "Alucard Special ~A handed to handle-alu special"
                      form))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CL special handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-let (form env &optional handle-constrain)
  ;; we don't need to update the environment as we don't care about
  ;; symbols as much.
  (destructuring-bind (let args &rest body) form
    (list* let (handle-binder args handle-constrain) (step-body body env))))

(defun handle-eval-when (form env)
  (destructuring-bind (eval-when declaration &rest body) form
    (list* eval-when declaration (step-body body env))))

(defun handle-binder (binders env &optional handle-constrain)
  (mapcar (lambda (bind-pair)
            (if (and handle-constrain (eql (car bind-pair) 'alu:with-constraint))
                (run-mode bind-pair
                          (handle-constraint bind-pair env))
                ;; Should I mark the variable name in the stack trace?
                ;; would make sense, but I currently don't do it.
                (cons (car bind-pair)
                      (step-body (cdr bind-pair) env :handle-declaration nil))))
          binders))

(defun handle-generic (form env)
  (cons (car form)
        (mapcar (lambda (x) (step x env)) (cdr form))))

(defun handle-if (form env)
  (handle-generic form env))

(defun handle-progn (form env)
  (handle-generic form env))

(defun handle-progv (form env)
  (destructuring-bind (prov vars values &rest body) form
    (list* prov vars values (step-body body env :handle-declaration nil))))

(defun handle-block (form env)
  (destructuring-bind (block name &rest code) form
    (list* block name (step-body code env :handle-declaration nil))))

(defun handle-catch (form env)
  (destructuring-bind (catch name &rest body) form
    (list* catch name (step-body body env :handle-declaration nil))))

(defun handle-function (form env)
  (destructuring-bind (function thing) form
    (if (listp thing)
        (list function
              ;; sbcl has a macro which expands itslef to function,
              ;; which would cause issues
              (if (listp (cadr thing))
                  (list* (car thing) (cadr thing)
                         (step-body (cddr thing) env))
                  (list* (car thing) (cadr thing) (caddr thing)
                         (step-body (cdddr thing) env))))
        (handle-generic form env))))

(defun handle-go (form env)
  (declare (ignore env))
  form)

;; we ignore it, this has a downside if the user then writes
;;
;; (eval '(...))
;;
;; they'll have to run my special function, or perhaps I can give an
;; eval that can go over the syntax specially
(defun handle-quote (form env)
  (declare (ignore env))
  form)

(defun handle-return (form env)
  (destructuring-bind (return-from from &optional code) form
    (if code
        (list* return-from from (step code env))
        form)))

(defun handle-load-time-value (form env)
  (destructuring-bind (load-time-value form &optional read-only-p) form
    (list load-time-value (step form env) read-only-p)))

(defun handle-locally (form env)
  (step-body form env :handle-declaration t))

(defun handle-multiple-value-call (form env)
  (handle-generic form env))

(defun handle-multiple-value-prog1 (form env)
  (handle-generic form env))

;; we can do this one generically cause we don't touch symbols
(defun handle-setq (form env)
  (handle-generic form env))

;; same for tagbody
(defun handle-tagbody (form env)
  (handle-generic form env))

(defun handle-the (form env)
  (destructuring-bind (the type form) form
    (list the type (step form env))))

(defun handle-throw (form env)
  (destructuring-bind (throw cond form) form
    (list throw cond (step form env))))

;; I think we can be generic about this
(defun handle-unwind-protect (form env)
  (handle-generic form env))

;; we don't attempt to handle symbol macros, note when we do we will
;; have to rewrite many special forms that use symbols
(defun handle-symbol-macrolet (form env)
  (destructuring-bind (symbol-macrolet binds &rest body) form
    (list* symbol-macrolet binds (step-body body env))))

(defun handle-local-function (form env &key recursive macro)
  "Handles functions like flet, labels, and macrolet.

:recursive   means that the binding is recursive and should be all
             considered together
:macro       means the binding form should be considered a macro. We
             assume the macro is also :recursive t"
  (destructuring-bind (func bindings &rest body) form
    (let* ((new-env
             (if (not macro)
                 (cltl2:augment-environment
                  env :function (mapcar (lambda (x) (car x)) bindings))
                 (cltl2:augment-environment
                  env
                  :macro (mapcar (lambda (x)
                                   (destructuring-bind (name args &rest def) x
                                     (cltl2:parse-macro name args def env)))
                                 bindings))))
           (body-env
             (if (or macro recursive) new-env env)))
      (flet ((handle-binding-body (func)
               (destructuring-bind (name args &rest body) func
                 (list* name args (step-body body body-env)))))
        (list* func
               (handle-binding-body bindings)
               (step-body body new-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Alucard Special Form Handling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun handle-constraint (form env)
  "Handles an `alu:with-constraint' form"
  (destructuring-bind (with-constraint bindings &rest body) form
    (list* with-constraint bindings (step-body body env))))

(defun handle-coerce (form env)
  (destructuring-bind (coerce value type-to) form
    (list coerce (step value env) type-to)))

(defun handle-check (form env)
  (destructuring-bind (check value type-against) form
    (list check (step value env) type-against)))

(defun handle-array (form env)
  (declare (ignore env))
  form)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Checking Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun declarationp (form)
  "determines if a form is a declaration or not"
  (or (eql (car form) 'declare)
      ;; probably overkill given declaims are top level
      (eql (car form) 'declaim)))
