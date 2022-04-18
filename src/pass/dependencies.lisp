(in-package :alu.pass.dependencies)



(-> track-circuit-deps (spc:function-type) list)
(defun track-circuit-deps (circuit)
  "This function gives out a list of any functions this function calls in a dependency chart"
  (values
   (etypecase-of spc:function-type circuit
     (spc:primitive nil)
     (spc:circuit   (track-constraint-deps (alu.pass:linearize circuit))))))


;; we assume that `pass:linearize' has been run
(-> track-function-deps (spc:constraint-list) list)
(defun track-constraint-deps (constraint-list)
  (labels ((handle-term (term)
             (etypecase-of spc:term-no-binding term
               (spc:application      (spc:name (spc:func term)))
               (spc:term-normal-form nil)
               (spc:record           nil)
               (spc:record-lookup    nil)))
           (handle-linear-term (constraint)
             (etypecase-of spc:linear-term constraint
               (spc:term-no-binding (handle-term constraint))
               (spc:bind            (handle-term (spc:value constraint))))))
    (filter-map #'handle-linear-term constraint-list)))
