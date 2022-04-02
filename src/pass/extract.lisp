(in-package :alu.pass.extract)

(-> expanded-circuit-to-alias (aspc:circuit) vspc:alias)
(defun expanded-circuit-to-alias (circuit)
  (with-accessors ((name aspc:name) (arguments aspc:arguments)
                   (body aspc:body) (ret       aspc:return-type))
      circuit
    (vspc:make-alias :name name
                     :inputs (alu.pass.expanded:argument-names arguments)
                     :outputs (determine-output-variables body ret)))
  (error "hi"))
