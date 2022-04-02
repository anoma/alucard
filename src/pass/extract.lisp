(in-package :alu.pass.extract)

(-> expanded-circuit-to-alias (aspc:prim-circuit) vspc:alias)
(defun expanded-circuit-to-alias (circuit)
  (with-accessors ((name aspc:name) (arguments aspc:arguments)
                   (body aspc:body) (ret       aspc:returns))
      circuit
    (vspc:make-alias :name name
                     :inputs  arguments
                     :outputs ret
                     :body (error "hi"))))

(-> expand-term-to-constraint (aspc:fully-expanded-term) (or null vspc:constraint))
(defun expand-term-to-constraint (term)
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
