(in-package :alu.pass.redundant)

(-> find-redundant-let (ir:fully-expanded-list &optional closure:typ) closure:typ)
(defun find-redundant-lets (xs &optional (map (closure:allocate)))
  (mvfold (lambda (map x)
	    (etypecase-of ir:fully-expanded-term x
	      ((or ir:standalone-ret ir:multiple-bind)
               map)
	      (ir:bind
	       (etypecase-of ir:base (ir:value x)
		 (ir:application map)
		 (number         (closure:insert map (ir:var x) (ir:value x)))
		 (ir:reference   (let* ((value (ir:value x))
                                        (find (closure:lookup map (ir:name value))))
                                   (closure:insert map (ir:var x) (or find value))))))
	      (ir:bind-constraint
               (find-redundant-lets (ir:value x) map))))
          xs map))

(-> replace-references (ir:fully-expanded-list closure:typ) ir:fully-expanded-list)
(defun replace-references (xs map)
  (mapcar
   (lambda (x)
     (etypecase-of ir:fully-expanded-term x
       (ir:multiple-bind   x)
       (ir:bind-constraint (util:copy-instance
                            x :value (replace-references (ir:value x) map)))
       (ir:standalone-ret
        (let* ((vars (ir:var x))
               (updated-rets
                 (mapcar (lambda (y)
                           (let ((find (closure:lookup map y)))
                             (if find
                                 (etypecase-of ir:term-normal-form (closure:lookup map y)
                                   (ir:reference (ir:name (closure:lookup map y)))
                                   (number       (closure:lookup map y)))
                                 y)))
                         vars)))
          (ir:make-standalone-ret :var updated-rets)))
       (ir:bind
        (etypecase-of ir:base (ir:value x)
          (ir:reference x)
          (number       x)
          (ir:application
           (let* ((value (ir:value x))
                  (updated-refs
                    (mapcar (lambda (y)
                              (etypecase-of ir:base y
                                (number         y)
                                (ir:application y)
                                (ir:reference   (or (closure:lookup map (ir:name y))
                                                    y))))
                            (ir:arguments value))))
             (util:copy-instance
              x :value (util:copy-instance value
                                           :arguments updated-refs))))))))
   xs))


(-> remove-redundant-lets (ir:fully-expanded-list closure:typ) ir:fully-expanded-list)
(defun remove-redundant-lets (xs map)
  (filter-map (lambda (x)
                (etypecase-of ir:fully-expanded-term x
                  ((or ir:standalone-ret ir:multiple-bind)
                   x)
                  (ir:bind
                   (if (closure:lookup map (ir:var x)) nil x))
                  (ir:bind-constraint
                   (util:copy-instance
                    x :value (remove-redundant-lets (ir:value x) map)))))
              xs))
