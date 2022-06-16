(in-package :alu.pipeline)

;; TODO :: Make a Berlin pipeline abstraction, we really need to stop
;; half way through for easier testing! until then I'll just have many
;; arrow functions for where I want to stop off!

(-> to-typecheck           (ir:circuit) (values ir:expanded-list check:typing-context))
(-> to-expand-away-arrays  (ir:circuit) (values ir:expanded-list check:typing-context))
(-> to-expand-away-records (ir:circuit) ir:fully-expanded-list)
(-> to-primitive-circuit   (ir:circuit) ir:prim-circuit)
(-> to-vampir              (ir:circuit) alu.vampir.spec:alias)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compile Circuit and Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dump-entry-point (&optional (stream *standard-output*))
  (print-vampir (comp-all (storage:lookup-function (storage:get-entry-point)))
                stream))

(defun dump-entry-point-to-file (file-name)
  (with-open-file (file file-name :direction :output
                                  :if-exists :overwrite
                                  :if-does-not-exist :create)
    (dump-entry-point file)))

(defun comp-all (circuit)
  (mapcar #'pipeline
          (cons circuit
                (filter-map (alexandria:compose (lambda (circ)
                                                  (and (typep circ 'ir:circuit)
                                                       circ))
                                                #'storage:lookup-function)
                            (dep:track-circuit-deps* circuit)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline Proper
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-vampir (vampir &optional (stream *standard-output*))
  (vampir:extract vampir stream))

(-> pipeline (ir:circuit) alu.vampir.spec:alias)
(defun pipeline (circuit)
  (~> circuit
      to-vampir))

(defun to-typecheck (circuit)
  (mvlet ((body typing (~> circuit
                           pass:linearize
                           (check:check circuit))))
    (values
     (pass:remove-type-information body)
     typing)))

(defun to-expand-arrays (circuit)
  (mvlet* ((body context (to-typecheck circuit))
           (context body (~>> body
                              (array:handle-terms context))))
    (values
     body
     context)))

(defun to-expand-away-records (circuit)
  (~> circuit
      to-expand-arrays
      (pass:expand-away-records circuit)
      pass:remove-void-bindings))

(defun to-primitive-circuit (circuit)
  (~> circuit
      to-expand-away-records
      (pass:primitive-circuit circuit)
      pass:rename-primitive-circuit))

(defun to-vampir (circuit)
  (values
   (~> circuit
       to-primitive-circuit
       pass:circuit-to-alias)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Pipeline For Expressions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> type-check-expression
    (ir:expression alu.typechecker:typing-context)
    (values alu.typechecker:typing-context ir:expanded-list))
(defun type-check-expression (body context)
  (let ((body (pass:linearize-body body)))
    (values
     (check:annotate-list body context)
     (remove-if (lambda (p) (typep p 'ir:standalone-ret))
                (pass:remove-type-information body)))))
