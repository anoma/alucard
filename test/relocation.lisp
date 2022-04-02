(in-package :alu-test)

(def-suite alucard.relocation
  :description "Test the relocation functionality")

(in-suite alucard.relocation)

(defparameter *example-closure*
  (closure:allocate
   :fi '((:plane . :fi-plane)
         (:point . ((:x . :fi-point-x) (:y . :fi-point-y))))))

(defparameter *example-bind*
  (spc:make-bind :var :hi
                 :val (spc:make-reference :name :fi)))

(defparameter *example-bind-app*
  (spc:make-bind
   :var :hi
   :val (spc:make-application :function (spc:make-reference :name :arg-foo)
                              :arguments '(1 5 6))))

(defparameter *example-bind-record*
  (spc:make-bind
   :var :hi
   :val (spc:make-record :name :test
                         :own   (spc:make-reference :name :fi)
                         :other (spc:make-reference :name :non-exist))))

(defparameter *example-bind-lookup-1*
  (spc:make-bind
   :var :hi
   :val (spc:make-record-lookup :record (spc:make-reference :name :fi)
                                :field :plane)))
(defparameter *example-bind-lookup-2*
  (spc:make-bind
   :var :hi
   :val (spc:make-record-lookup :record (spc:make-reference :name :fi)
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
              (is (eq input (spc:var bind)))
              (is (eq res (spc:name (spc:value bind)))))
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
              (is (eq input (spc:var bind)))
              (is (eq res (spc:name (spc:value bind)))))
            expected-let-names
            expected-let-resul
            (relocate:rel-forms relocation-2))
    (is (equalp expected-storage
                (closure:lookup (relocate:rel-closure relocation-2)
                                :hi)))
    ;; time for the easier one
    (let ((only-form (car (relocate:rel-forms relocation-1))))
      (is (eq :hi
              (spc:var only-form)))
      (is (eq :fi-plane
              (spc:name (spc:value only-form)))))))

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
              (is (eq input (spc:var bind)))
              (is (eq res (spc:name (spc:value bind)))))
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
    (is (equalp expected-binds (spc:var (car (relocate:rel-forms relocation)))))
    (is (equalp (spc:value *example-bind-app*)
                (spc:value (car (relocate:rel-forms relocation))))
        "The function should not change")
    (is (equalp expected-storage
                (closure:lookup (relocate:rel-closure relocation)
                                :hi)))))

(test initial-closure-from-circuit
  (let ((expected-storage '((:PLANE
                             (:X . :SIG-PLANE-X)
                             (:Y . :SIG-PLANE-Y))
                            (:TIME
                             (:X . :SIG-TIME-X)
                             (:Y . :SIG-TIME-Y))))
        (closure (relocate:initial-closure-from-circuit
                  (storage:lookup-function :arg-circuit-input))))

    ;; Tests begin here
    (is (equalp expected-storage (closure:lookup closure :sig)))
    (is (equalp nil (closure:lookup closure :root))
        "Root is not a record, we don't ingest it.")))