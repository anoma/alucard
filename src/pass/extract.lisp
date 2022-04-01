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



(-> determine-output-variables
    (aspc:expanded-list (or aspc:type-reference list)) list)
(defun determine-output-variables (body ret)
  "Determines which output variables are returned from a function. If
ret is (`aspc:primitive' :void) then an empty list is returned,
however if the value is not void, then the returns in the body are
given back"
  body ret
  (error "hi"))
