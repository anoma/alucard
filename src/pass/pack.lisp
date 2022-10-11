(in-package :alu.pass.pack)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packing Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> op
    (check:typing-context
     check:type-info
     ir:term-no-binding
     &rest ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun op (closure type-format term &rest data)
  "Packs the given data into the format specified by the
`type-format'. the `term' is used solely for meta information
propagation"
  (let ((ref (check:type-info-type type-format)))
    (cond ((type-op:array-reference?  ref)
           (apply #'array  closure type-format term data))
          ((type-op:record-reference? ref)
           (apply #'record closure type-format term data))
          (t
           (error "Reference of type ~A can not be packed" ref)))))

(-> array
    (check:typing-context
     check:type-info
     ir:term-no-binding
     &rest
     ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun array (closure type-format term &rest data)
  (let ((length    (ir:array-type-len (check:type-info-type type-format)))
        (data-size (array-data-size type-format)))
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
     (ir:copy-meta
      term
      (apply #'term-op:add
             (mapcar (lambda (element position)
                       (term-op:times element
                                      (expt 2 (* position data-size))))
                     data
                     (alexandria:iota length))))
     closure)))

(-> final-ref-from-op (ir:expanded-list) ir:reference)
(defun final-ref-from-op (result-terms)
  (ir:make-reference :name
                     (ir:var (car (last result-terms)))))


(-> record
    (check:typing-context check:type-info ir:record-lookup ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun record (closure type-format term data)
  type-format closure data term
  (error "not implemented"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indexing Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> lookup-at
    (check:typing-context
     check:type-info
     ir:term-no-binding
     (or ir:term-normal-form keyword)
     ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun lookup-at (context type term to-find data)
  (let ((ref (check:type-info-type type)))
    (cond ((and (type-op:array-reference?  ref)
                (not (keywordp to-find)))
           (array-lookup context type term to-find data))
          ((type-op:record-reference? ref)
           (error "not implemented"))
          (t
           (error "Reference of type ~A can not be packed" ref)))))


(-> array-lookup
    (check:typing-context
     check:type-info
     ir:array-lookup
     ir:term-normal-form
     ir:term-normal-form)
    (values check:typing-context ir:expanded-list))
(defun array-lookup (closure type term index data)
  (flet ((int (data)
           (term-op:coerce :int data))
         (ref (keyword)
           (ir:make-reference :name keyword))
         (+ (left right)
           (term-op:add left right))
         (× (left right)
           (term-op:times left right)))
    (let ((size        (array-data-size type))
          (target-type (ir:array-type-content (check:type-info-type type))))
      (check:with-intro (closure unused-array unused-mod
                                 smaller-array lookup-answer)
          closure
        ;; The equation here is trying to do the following equation
        ;;
        ;; want : array[index]
        ;;
        ;; how to compute :
        ;;
        ;; array = 2 ^ (index * size) * smaller-array + unused-mod
        ;;
        ;; smaller-array = 2 ^ size * unused-arr + answer
        ;;
        ;; answer
        ;;
        ;; However we have to coerce:
        ;;  - answer to the proper type.
        ;;  - index to be a bignum
        ;;  - array to be a bignum
        ;;
        ;; Thus we arrive at
        ;; (def ((with-constraint (smal unused-mod unused-arr answer)
        ;;          (= data (+ (* smal (expt 2 (* index size))) unused-mod))
        ;;          (= smal (+ (* unused-arr (expt 2 index))    answer))))
        ;;   (coerce answer type)
        ;;
        ;; However we have to use more verbose names and coerce by hand.
        ;;
        ;; TODO :: Ι was told that this creates too many solutions,
        ;;         so we have to note a range check on the size of the
        ;;         elements to properly constrain the values. Thus we
        ;;         need to do 0 <= mod value <= 2 ^ value
        (pipeline:type-check-expression
         (ir:copy-meta
          term
          (ir:make-bind-constraint
           :var (list unused-array unused-mod smaller-array lookup-answer)
           :value
           (list
            (term-op:= (int data)
                       (+ (× (term-op:exp 2 (term-op:times size (int index)))
                              (ref smaller-array))
                          (ref unused-mod)))
            ;; range check
            (term-op:= (ref smaller-array)
                       (+ (× (term-op:exp 2 size)
                              (ref unused-array))
                          (ref lookup-answer)))
            (ir:copy-meta term
                          (ir:make-type-coerce :typ target-type
                                               :value (ref lookup-answer))))))
         closure)))))

(-> array-lookup-final-ref (ir:expanded-list) ir:reference)
(defun array-lookup-final-ref (result-term)
  (ir:make-reference :name (ir:var
                            (car (last (ir:value (car result-term)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Full Unpacking logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> unpack (check:type-info ir:term-normal-form) ir:expanded-term)
(defun unpack (type data)
  type data
  (error "not implemented"))


(-> array-data-size (check:type-info) fixnum)
(defun array-data-size (format)
  (let* ((array-info (check:type-info-type format))
         (length     (ir:array-type-len array-info)))
    (/ (check:type-info-size format)
       length)))
