(in-package :alu.pass.extract)

(-> expanded-circuit-to-alias (aspc:prim-circuit) vspc:alias)
(defun expanded-circuit-to-alias (circuit)
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
  (etypecase-of aspc:fully-expanded-term term
    ;; drop standalone constants, we can't emit it!
    (aspc:term-normal-form nil)
    (aspc:application
     ;; (vspc:)
     )
    (aspc:bind
     )
    (aspc:multiple-bind
     )
    (aspc:ret
     )
    (aspc:multi-ret
     ))
  (error "hi"))

(-> normal-form->normal-form (aspc:term-normal-form) vspc:normal-form)
(defun normal-form->normal-form (anormal)
  anormal
  (error "hi"))

(-> app-to-constraint (aspc:application) vspc:constraint)
(defun app->constraint (app)
  (etypecase-of aspc:function-type
      (storage:lookup-function (aspc:name (aspc:func app)))
    ;; circuits are easy, as it's a straightforward mapping!
    (aspc:circuit
     (vspc:make-application :func (aspc:name (aspc:func app))
                            :arguments (mapcar #'normal-form->normal-form
                                               (aspc:arguments app))))
    (aspc:primitive
     )))
