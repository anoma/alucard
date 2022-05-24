(in-package :alu-test)

(def-suite alucard.format
    :description "Tests the alucard storage format")

(in-suite alucard.format)

(defclass test-mixin ()
  ((foo :initarg :foo)))

(defclass test-class (test-mixin ir:protect-slots-mixin)
  ((bar :initarg :bar)
   (baz :initarg :baz)
   ;; Must provide this, as allocation happens on the super class level ☹
   (protected :initform (make-hash-table :test #'eq) :allocation :class)))

(ir:protect-slots 'test-class 'baz)

;; (slot-value (c2cl:class-prototype (find-class 'test)) 'protected)

(test generic-data-considerations
  (let ((expected '((:bar . 5))))
    (is
     (equalp expected
             (ir:direct-slots (make-instance 'test-class :foo 3 :bar 5 :baz 10))))))


(test record-creation-and-lookup-works
  (for-all ((name  (gen-string))
            (value (gen-integer)))
    (let ((keyword (intern name :keyword)))
      (is
       (equal (ir:lookup-record (ir:make-record :name :example keyword value)
                                 keyword)
              value)))))

(test syntax-to-refernece-format
  (let ((applied (ir:to-type-reference-format '(int 64)))
        (nested  (ir:to-type-reference-format '(int (int 64)))))
    (is (eq :INT
            (ir:name (ir:func applied))))
    (is (= 64
           (car (ir:arguments applied))))
    (is (eq :INT
            (ir:name (ir:func (car (ir:arguments nested))))))))


;; Note for later, we can have exuastion
;; (match-of term (make-application :function :hi )
;;   ((application func) func))

;; (typecase-of term 3
;;   (number
;;    2)
;;   (application
;;    2)
;;   (otherwise
;;    2))
