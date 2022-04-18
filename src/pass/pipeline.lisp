(in-package :alu.pipeline)

;; TODO :: Make a Berlin pipeline abstraction, we really need to stop
;; half way through for easier testing! until then I'll just have many
;; arrow functions for where I want to stop off!

(-> to-linearize           (spc:circuit) spc:constraint-list)
(-> to-expand-away-records (spc:circuit) spc:fully-expanded-list)
(-> to-primitive-circuit   (spc:circuit) spc:prim-circuit)
(-> to-vampir              (spc:circuit) alu.vampir.spec:alias)

(defun print-vampir (circuit &optional (stream *standard-output*))
  (vampir:extract (list (pipeline circuit)) stream))

(-> pipeline (spc:circuit) alu.vampir.spec:alias)
(defun pipeline (circuit)
  (~> circuit
      to-vampir))

(defun to-expand-away-records (circuit)
  (~> circuit
      pass:linearize
      (pass:expand-away-records circuit)
      pass:remove-void-bindings))

(defun to-primitive-circuit (circuit)
  (~> circuit
      to-expand-away-records
      (pass:primtitve-circuit circuit)
      pass:rename-primitive-circuit))

(defun to-vampir (circuit)
  (values
   (~> circuit
       to-primitive-circuit
       pass:circuit-to-alias)))
