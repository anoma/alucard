(in-package :alu.pass.pack)

(-> op
    (check:type-info check:typing-context &rest ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun op (type-format closure &rest data)
  "Packs the given data into the format specified by the `type-format'"
  (let ((ref (check:type-info-type type-format)))
    (cond ((type-op:array-reference?  ref) (apply #'array type-format closure data))
          ((type-op:record-reference? ref) (apply #'record type-format closure data))
          (t (error "Reference of type ~A can not be packed" ref)))))

(-> array
    (check:type-info check:typing-context &rest ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun array (type-format closure &rest data)
  (let* ((array-info (check:type-info-type type-format))
         (length     (ir:array-type-len array-info))
         (data-size  (/ (check:type-info-size type-format)
                        length)))
    (assert (fixnump data-size))
    (pipeline:type-check-expression
     ;; We can pack the data by adding all the values together in a
     ;; smart way. Namely if we bitshift by the length of the type,
     ;; then there can be no clash, and we can just make our
     ;; constraints `util:sequence-to-number' is this same algorithm
     ;; except for CL data.
     ;;
     ;; We get back an expanded list from this, with the last let,
     ;; being the value that contains our addition.
     ;;
     ;; TODO :: Add type coercing on the element, as we will want a
     ;;         bignum out of the computation. This only matters when
     ;;         we generate code mod n
     (apply #'term-op:add
            (mapcar (lambda (element position)
                      (term-op:times element
                                     (expt 2 (* position data-size))))
                    data
                    (alexandria:iota length)))
     closure)))

(-> record
    (check:type-info check:typing-context ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun record (type-format closure data)
  type-format closure data
  (error "not implemented"))

(-> lookup-at (check.type:type-info (or fixnum keyword) ir:term-normal-form)
    ir:expanded-list)
(defun lookup-at (type to-find data)
  type to-find data
  (error "not implemented"))

(-> unpack (check.type:type-info ir:term-normal-form) ir:expanded-term)
(defun unpack (type data)
  type data
  (error "not implemented"))
