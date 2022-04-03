(in-package :alu.pass.extract)

(-> circuit-to-alias (aspc:prim-circuit) vspc:alias)
(defun circuit-to-alias (circuit)
  (with-accessors ((name aspc:name) (arguments aspc:arguments)
                   (body aspc:body) (ret       aspc:returns))
      circuit
    (values
     (vspc:make-alias :name name
                      :inputs  arguments
                      :outputs ret
                      :body (filter-map #'term->constraint body)))))

(-> term->constraint (aspc:fully-expanded-term) (or null vspc:constraint))
(defun term->constraint (term)
  (labels ((keywords->wire (keys)
             (mapcar (lambda (x) (vspc:make-wire :var x)) keys))
           (var-val->bind (term)
             (vspc:make-bind :names (keywords->wire
                                     (if (listp (aspc:var term))
                                         (aspc:var term)
                                         (list (aspc:var term))))
                             :value (term->expression (aspc:value term)))))
    (etypecase-of aspc:fully-expanded-term term
      ;; drop standalone constants, we can't emit it!
      (aspc:term-normal-form nil)
      (aspc:application      (app->constraint term))
      (aspc:bind             (var-val->bind term))
      (aspc:ret              (var-val->bind term))
      (aspc:multiple-bind    (var-val->bind term))
      (aspc:multi-ret        (var-val->bind term)))))

(-> term->expression ((or aspc:term-normal-form aspc:application)) vspc:expression)
(defun term->expression (app-norm)
  (etypecase-of (or aspc:term-normal-form aspc:application) app-norm
    (aspc:application      (app->expression app-norm))
    (aspc:term-normal-form (normal-form->normal-form app-norm))))

(-> normal-form->normal-form (aspc:term-normal-form) vspc:normal-form)
(defun normal-form->normal-form (anormal)
  (etypecase-of aspc:term-normal-form anormal
    (number         (alu.vampir.spec:make-constant :const anormal))
    (aspc:reference (vspc:make-wire :var (aspc:name anormal)))))


(-> app->constraint (aspc:application) vspc:constraint)
(defun app->constraint (app)
  (let ((looked    (storage:lookup-function (aspc:name (aspc:func app))))
        (deal-args (mapcar #'normal-form->normal-form (aspc:arguments app))))
    (etypecase-of aspc:function-type looked
      (aspc:circuit
       (vspc:make-application :func (aspc:name (aspc:func app))
                              :arguments deal-args))
      (aspc:primitive
       (cond ((not (eql (aspc:name app) :=))
              (error "an infix expression is not a valid constraint"))
             ((= (length deal-args) 2)
              (vspc:make-equality :lhs (car deal-args)
                                  :rhs (cadr deal-args))))))))

(-> app->expression (aspc:application) vspc:expression)
(defun app->expression (app)
  (let ((looked    (storage:lookup-function (aspc:name (aspc:func app))))
        (deal-args (mapcar #'normal-form->normal-form (aspc:arguments app))))
    (if (typep looked 'aspc:primitive)
        ;; circuits are easy, as it's a straightforward mapping!
        (prim->app (alucard-prim->vampir-name (aspc:name looked)) deal-args)
        (vspc:make-application :func (aspc:name (aspc:func app))
                               :arguments deal-args))))


(-> prim->app (keyword list) vspc:infix)
(defun prim->app (key args)
  (if (and (not (typep key 'vspc:primitive)) (= 0 (length args)))
      (error "primitive functions require 2 arguments")
      (reduce (lambda (lhs rhs)
                (vspc:make-infix :lhs lhs :op key :rhs rhs))
              args
              :from-end t)))

(-> alucard-prim->vampir-name (keyword) keyword)
(defun alucard-prim->vampir-name (keyword)
  (case keyword
    (:exp :^)
    (t    keyword)))
