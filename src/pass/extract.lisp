(in-package :alu.pass.extract)

(-> circuit-to-alias (ir:prim-circuit) spc:alias)
(defun circuit-to-alias (circuit)
  (with-accessors ((name ir:name) (arguments ir:arguments) (body ir:body)) circuit
    (values
     (spc:make-alias :name name
                     :inputs arguments
                     :body (mapcan #'term->constraint body)))))

(-> term->constraint (ir:fully-expanded-term) spc:constraint-list)
(defun term->constraint (term)
  (labels ((keywords->wire (keys)
             (mapcar (lambda (x) (spc:make-wire :var x)) keys))
           (var-val->bind (term)
             (let ((value (ir:value term))
                   (names (keywords->wire (if (listp (ir:var term))
                                              (ir:var term)
                                              (list (ir:var term))))))
               (if (not (equality-check value))
                   (list
                    (spc:make-bind :names names
                                    :value (term->expression value)))
                   ;; This entire thing is a hack, please do better!

                   ;; this will have to hit the app->constraint case!
                   ;; as it's an equality application!
                   (let ((equality (app->constraint value)))
                     (list equality
                           ;; should only be one as it's a
                           (spc:make-bind :names names
                                           :value (spc:lhs equality))))))))
    (values
     (etypecase-of ir:fully-expanded-term term
       ;; drop standalone constants, we can't emit it!
       (ir:standalone-ret (list (return->expression term)))
       ;; (ir:application      (list (app->constraint term)))
       (ir:bind-constraint (mapcan #'term->constraint (ir:value term)))
       (ir:bind            (var-val->bind term))
       (ir:multiple-bind   (var-val->bind term))))))

(-> return->expression (ir:standalone-ret) (or spc:wire spc:tuple))
(defun return->expression (ret)
  (values
   (let ((wires (ir:var ret)))
     (if (or (null wires) (cdr wires))
         (spc:make-tuples :wires wires)
         (spc:make-wire :var (car wires))))))

(-> term->expression ((or ir:term-normal-form ir:application)) spc:expression)
(defun term->expression (app-norm)
  (etypecase-of (or ir:term-normal-form ir:application) app-norm
    (ir:application      (app->expression app-norm))
    (ir:term-normal-form (normal-form->normal-form app-norm))))

(-> normal-form->normal-form (ir:term-normal-form) spc:normal-form)
(defun normal-form->normal-form (anormal)
  (assure spc:normal-form
   (etypecase-of ir:term-normal-form anormal
     (number       (spc:make-constant :const anormal))
     (ir:reference (spc:make-wire :var (ir:name anormal))))))


(-> app->constraint (ir:application) spc:constraint)
(defun app->constraint (app)
  (let ((looked    (storage:lookup-function (ir:name (ir:func app))))
        (deal-args (mapcar #'normal-form->normal-form (ir:arguments app))))
    (values
     (etypecase-of ir:function-type looked
       (ir:circuit
        (spc:make-application :func (ir:name (ir:func app))
                              :arguments deal-args))
       (ir:primitive
        (cond ((not (eql (ir:name looked) :=))
               (error "an infix expression is not a valid constraint"))
              ;; we should probably make = take n arguments were we can fold it
              ((= (length deal-args) 2)
               (spc:make-equality :lhs (car deal-args)
                                  :rhs (cadr deal-args)))
              (t
               (error
                (format nil
                        "= can only be applied to 2 arguments but is applied to: ~A~%"
                        (length deal-args))))))))))

(-> app->expression (ir:application) spc:expression)
(defun app->expression (app)
  (let ((looked    (storage:lookup-function (ir:name (ir:func app))))
        (deal-args (mapcar #'normal-form->normal-form (ir:arguments app))))
    (values
     (if (typep looked 'ir:primitive)
         ;; circuits are easy, as it's a straightforward mapping!
         (prim->app (alucard-prim->vampir-name (ir:name looked)) deal-args)
         (spc:make-application :func (ir:name (ir:func app))
                               :arguments deal-args)))))


(-> prim->app (keyword list) spc:infix)
(defun prim->app (key args)
  (if (and (not (typep key 'spc:primitive)) (= 0 (length args)))
      (error "primitive functions require 2 arguments")
      (reduce (lambda (lhs rhs)
                (spc:make-infix :lhs lhs :op key :rhs rhs))
              args
              :from-end t)))

(-> alucard-prim->vampir-name (keyword) keyword)
(defun alucard-prim->vampir-name (keyword)
  (case keyword
    (:exp :^)
    (t    keyword)))

(defun equality-check (term)
  (and (typep term 'ir:application)
       (eql (ir:name (ir:func term)) :=)))
