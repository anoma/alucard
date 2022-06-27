(in-package :alu-test)

(def-suite alucard.relocation
  :description "Test the relocation functionality")

(in-suite alucard.relocation)

(defparameter *example-closure*
  (closure:allocate
   :fi '((:plane . :fi-plane)
         (:point . ((:x . :fi-point-x) (:y . :fi-point-y))))))

(defparameter *example-bind*
  (ir:make-bind :var :hi
                 :val (ir:make-reference :name :fi)))

(defparameter *example-bind-app*
  (ir:make-bind
   :var :hi
   :val (ir:make-application :function (ir:make-reference :name :arg-foo)
                              :arguments '(1 5 6))))

(defparameter *example-bind-record*
  (ir:make-bind
   :var :hi
   :val (ir:make-record :name :test
                         :own   (ir:make-reference :name :fi)
                         :other (ir:make-reference :name :non-exist))))

(defparameter *example-bind-lookup-1*
  (ir:make-bind
   :var :hi
   :val (ir:make-record-lookup :record (ir:make-reference :name :fi)
                                :field :plane)))
(defparameter *example-bind-lookup-2*
  (ir:make-bind
   :var :hi
   :val (ir:make-record-lookup :record (ir:make-reference :name :fi)
                                :field :point)))

(test relocate-let-ref
  (let ((expected-let-names '(:hi-plane :hi-point-x :hi-point-y))
        (expected-let-resul '(:fi-plane :fi-point-x :fi-point-y))
        (expected-storage   '((:PLANE . :HI-PLANE)
                              (:POINT
                               (:X . :HI-POINT-X)
                               (:Y . :HI-POINT-Y))))
        (relocation (relocate:relocate-let *example-bind* *example-closure*)))
    (mapcar (lambda (input res bind)
              (is (eq input (ir:var bind)))
              (is (eq res (ir:name (ir:value bind)))))
            expected-let-names
            expected-let-resul
            (relocate:rel-forms relocation))
    (is (equalp expected-storage (closure:lookup (relocate:rel-closure relocation)
                                                 :hi)))))
(test relocate-let-lookup
  (let ((expected-let-names '(:hi-point-x :hi-point-y))
        (expected-let-resul '(:fi-point-x :fi-point-y))
        (expected-storage   '((:X . :HI-POINT-X)
                              (:Y . :HI-POINT-Y)))
        (relocation-1 (relocate:relocate-let *example-bind-lookup-1*
                                              *example-closure*))
        (relocation-2 (relocate:relocate-let *example-bind-lookup-2*
                                              *example-closure*)))
    (mapcar (lambda (input res bind)
              (is (eq input (ir:var bind)))
              (is (eq res (ir:name (ir:value bind)))))
            expected-let-names
            expected-let-resul
            (relocate:rel-forms relocation-2))
    (is (equalp expected-storage
                (closure:lookup (relocate:rel-closure relocation-2)
                                :hi)))
    ;; time for the easier one
    (let ((only-form (car (relocate:rel-forms relocation-1))))
      (is (eq :hi
              (ir:var only-form)))
      (is (eq :fi-plane
              (ir:name (ir:value only-form)))))))

(test relocate-let-record
  (let ((expected-let-names '(:hi-own-plane
                              :hi-own-point-x :hi-own-point-y
                              :hi-other))
        (expected-let-resul '(:fi-plane
                              :fi-point-x :fi-point-y
                              :non-exist))
        (expected-storage   '((:OWN . ((:PLANE . :HI-OWN-PLANE)
                                       (:POINT . ((:X . :HI-OWN-POINT-X)
                                                  (:Y . :HI-OWN-POINT-Y)))))
                              (:OTHER . :HI-OTHER)))
        (relocation (relocate:relocate-let *example-bind-record* *example-closure*)))
    (mapcar (lambda (input res bind)
              (is (eq input (ir:var bind)))
              (is (eq res (ir:name (ir:value bind)))))
            expected-let-names
            expected-let-resul
            (relocate:rel-forms relocation))
    (is (equalp expected-storage (closure:lookup (relocate:rel-closure relocation)
                                                 :hi)))))

(test relocate-let-app
  (let ((expected-binds   '(:HI-PLANE-X :HI-PLANE-Y :HI-TIME-X :HI-TIME-Y))
        (expected-storage '((:PLANE
                             (:X . :HI-PLANE-X)
                             (:Y . :HI-PLANE-Y))
                            (:TIME
                             (:X . :HI-TIME-X)
                             (:Y . :HI-TIME-Y))))
        (relocation (relocate:relocate-let *example-bind-app*
                                           *example-closure*)))

    ;; Tests begin here
    (is (equalp expected-binds (ir:var (car (relocate:rel-forms relocation)))))
    (is (equalp (ir:value *example-bind-app*)
                (ir:value (car (relocate:rel-forms relocation))))
        "The function should not change")
    (is (equalp expected-storage
                (closure:lookup (relocate:rel-closure relocation)
                                :hi)))))

(test initial-closure-from-circuit
  (let* ((sig-arugment
           (ir:name
            (cadr (ir:arguments (storage:lookup-function :arg-circuit-input)))))
         (expected-storage '((:X . :SIG-X) (:Y . :SIG-Y)))

         (closure (relocate:initial-closure-from-circuit
                   (storage:lookup-function :arg-circuit-input))))

    ;; Tests begin here
    (is (equalp expected-storage (closure:lookup closure :sig)))
    (is (equalp nil (closure:lookup closure :root))
        "Root is not a record, we don't ingest it.")))
