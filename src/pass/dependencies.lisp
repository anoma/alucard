(in-package :alu.pass.dependencies)

(-> track-circuit-deps* (ir:function-type &optional sycamore:tree-set) list)
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

(-> track-circuit-deps (ir:function-type) list)
(defun track-circuit-deps (circuit)
  "This function gives out a list of any functions this function calls
in a dependency chart"
  (values
   (etypecase-of ir:function-type circuit
     (ir:primitive nil)
     (ir:circuit   (track-constraint-deps (alu.pass:linearize circuit))))))

;; we assume that `pass:linearize' has been run
(-> track-constraint-deps (ir:type-aware-list) list)
(defun track-constraint-deps (constraint-list)
  (labels ((handle-term (term)
             (etypecase-of ir:term-type-manipulation term
               (ir:application
                (list (ir:name (ir:func term))))
               ((or ir:term-normal-form ir:record ir:record-lookup
                    ir:type-manipulation ir:array-forms)
                nil)))
           (handle-linear-term (constraint)
             (etypecase-of ir:expanded-term constraint
               (ir:bind            (handle-term (ir:value constraint)))
               (ir:bind-constraint (track-constraint-deps (ir:value constraint)))
               (ir:standalone-ret  nil))))
    (mapcan #'handle-linear-term constraint-list)))
