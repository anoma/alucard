(in-package :alu.pass.dependencies)

(-> track-circuit-deps* (spc:function-type &optional sycamore:tree-set) list)
(defun track-circuit-deps* (circuit &optional (exclusion-set
                                               (sycamore:tree-set
                                                #'util:hash-compare)))
  "This function works like `track-circuit-deps' however it recursively
checks all functions"
  (labels ((recursively-expand (key-name)
             (unless (sycamore:tree-set-find exclusion-set key-name)
               ;; this technique doesn't work fully as we'd want a
               ;; reference to it.
               (sycamore:tree-set-insertf exclusion-set key-name)
               (let ((circuit (storage:lookup-function key-name)))
                 (cons key-name
                       (when circuit
                         (track-circuit-deps* circuit exclusion-set)))))))
    (mapcan #'recursively-expand (track-circuit-deps circuit))))

(-> track-circuit-deps (spc:function-type) list)
(defun track-circuit-deps (circuit)
  "This function gives out a list of any functions this function calls
in a dependency chart"
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
               (spc:bind            (handle-term (spc:value constraint)))
               (spc:ret             (handle-term (spc:value constraint))))))
    (filter-map #'handle-linear-term constraint-list)))
