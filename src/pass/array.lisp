(in-package :alu.pass.array)

(-> handle-terms
    (check:typing-context ir:expanded-list)
    (values check:typing-context ir:expanded-list))
(defun handle-terms (context terms)
  (mvfoldr (lambda (term context terms)
             (multiple-value-bind (context new-terms) (handle-term context term)
               (values context
                       (append new-terms terms))))
           terms
           context
           nil))

(-> handle-term (check:typing-context ir:expanded-term)
    (values check:typing-context ir:expanded-list))
(defun handle-term (context term)
  (flet ((do-nothing ()
           (values context (list term)))
         (rebind-term (term-list updated-binding)
           (append term-list
                   (list (util:copy-instance term :value updated-binding)))))
    (etypecase-of ir:expanded-term term
      (ir:standalone-ret (do-nothing))
      (ir:bind
       (etypecase-of ir:term-no-binding (ir:value term)
         (ir:base         (do-nothing))
         (ir:record-forms (do-nothing))
         ;; TODO Fix Array Set
         (ir:array-set    (do-nothing))
         (ir:array-lookup
          (multiple-value-bind (context ts) (lookup context (ir:value term))
            (values context
                    (rebind-term ts
                                 (pack:array-lookup-final-ref ts)))))
         (ir:array-allocate
          (values context
                  (util:copy-instance term :var (allocate (ir:value term)))))
         (ir:from-data
          (multiple-value-bind (context ts) (to-array context term)
            (values context
                    (rebind-term ts
                                 (pack:final-ref-from-op ts)))))))
      (ir:bind-constraint
       (multiple-value-bind (context expanded) (handle-terms context
                                                             (ir:value term))
         (values context
                 (list
                  (util:copy-instance term
                                      :value expanded))))))))

(-> to-array
    (check:typing-context ir:bind)
    (values check:typing-context ir:expanded-list))
(defun to-array (context term)
  (let* ((type  (check:typing-closure context))
         (value (closure:lookup type (ir:var term))))
    (assert (typep (ir:value term) 'ir:from-data))
    (if value
        (apply #'pack:op context value (ir:value term) (ir:contents (ir:value term)))
        (error "Value ~A not found in the typing map ~A" value term))))

(-> lookup
    (check:typing-context ir:array-lookup)
    (values check:typing-context ir:expanded-list))
(defun lookup (context term)
  (let* ((type  (check:typing-closure context))
         (value (closure:lookup type (ir:name (ir:arr term)))))
    (if value
        (pack:lookup-at context value term (ir:pos term) (ir:arr term))
        (error "Value ~A not found in the typing map" value))))

(-> allocate (ir:array-allocate) ir:term-normal-form)
(defun allocate (term)
  (declare (ignore term))
  0)

(-> arr-set
    (check:typing-context ir:array-set)
    (values check:typing-context ir:expanded-list))
(defun arr-set (context set)
  context set
  (error "not implemented"))
