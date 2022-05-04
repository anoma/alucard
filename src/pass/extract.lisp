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
                      :body (mapcan #'term->constraint body)))))

(-> term->constraint (aspc:fully-expanded-term) vspc:constraint-list)
(defun term->constraint (term)
  (labels ((keywords->wire (keys)
             (mapcar (lambda (x) (vspc:make-wire :var x)) keys))
           (var-val->bind (term)
             (let ((value (aspc:value term))
                   (names (keywords->wire (if (listp (aspc:var term))
                                              (aspc:var term)
                                              (list (aspc:var term))))))
               (if (not (equality-check value))
                   (list
                    (vspc:make-bind :names names
                                    :value (term->expression value)))
                   ;; This entire thing is a hack, please do better!

                   ;; this will have to hit the app->constraint case!
                   ;; as it's an equality application!
                   (let ((equality (app->constraint value)))
                     (list equality
                           ;; should only be one as it's a
                           (vspc:make-bind :names names
                                           :value (vspc:lhs equality))))))))
    (values
     (etypecase-of aspc:fully-expanded-term term
       ;; drop standalone constants, we can't emit it!
       (aspc:standalone-ret nil)
       ;; (aspc:application      (list (app->constraint term)))
       (aspc:bind-constraint (mapcan #'term->constraint (aspc:value term)))
       (aspc:bind            (var-val->bind term))
       (aspc:multiple-bind   (var-val->bind term))))))

(-> term->expression ((or aspc:term-normal-form aspc:application)) vspc:expression)
(defun term->expression (app-norm)
  (etypecase-of (or aspc:term-normal-form aspc:application) app-norm
    (aspc:application      (app->expression app-norm))
    (aspc:term-normal-form (normal-form->normal-form app-norm))))

(-> normal-form->normal-form (aspc:term-normal-form) vspc:normal-form)
(defun normal-form->normal-form (anormal)
  (assure vspc:normal-form
   (etypecase-of aspc:term-normal-form anormal
     (number         (alu.vampir.spec:make-constant :const anormal))
     (aspc:reference (vspc:make-wire :var (aspc:name anormal))))))


(-> app->constraint (aspc:application) vspc:constraint)
(defun app->constraint (app)
  (let ((looked    (storage:lookup-function (aspc:name (aspc:func app))))
        (deal-args (mapcar #'normal-form->normal-form (aspc:arguments app))))
    (values
     (etypecase-of aspc:function-type looked
       (aspc:circuit
        (vspc:make-application :func (aspc:name (aspc:func app))
                               :arguments deal-args))
       (aspc:primitive
        (cond ((not (eql (aspc:name looked) :=))
               (error "an infix expression is not a valid constraint"))
              ;; we should probably make = take n arguments were we can fold it
              ((= (length deal-args) 2)
               (vspc:make-equality :lhs (car deal-args)
                                   :rhs (cadr deal-args)))
              (t
               (error
                (format nil
                        "= can only be applied to 2 arguments but is applied to: ~A~%"
                        (length deal-args))))))))))

(-> app->expression (aspc:application) vspc:expression)
(defun app->expression (app)
  (let ((looked    (storage:lookup-function (aspc:name (aspc:func app))))
        (deal-args (mapcar #'normal-form->normal-form (aspc:arguments app))))
    (values
     (if (typep looked 'aspc:primitive)
         ;; circuits are easy, as it's a straightforward mapping!
         (prim->app (alucard-prim->vampir-name (aspc:name looked)) deal-args)
         (vspc:make-application :func (aspc:name (aspc:func app))
                                :arguments deal-args)))))


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

(defun equality-check (term)
  (and (typep term 'aspc:application)
       (eql (aspc:name (aspc:func term)) :=)))
