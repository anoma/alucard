(in-package :alu-test)

(def-suite vampir
  :description "Test the vampir layer")

(in-suite vampir)

(defparameter *vamp-example-1*
  (list (vspc:make-pub :wires (list :fi :bar))
        (vspc:make-alias
         :name :xor
         :inputs '(:a :b)
         :body (list (vspc:make-bind
                      :names (list (vspc:make-wire :var :c))
                      :value (vspc:make-infix :op :+
                                              :lhs (vspc:make-wire :var :a)
                                              :rhs (vspc:make-wire :var :b)))
                     (vspc:make-wire :var :c)))))

(defparameter *expected-1*
  (with-output-to-string (stream)
    (format stream "pub fi~%")
    (format stream "pub bar~%")
    (format stream "def xor a b {~%")
    (format stream "  def c = a + b;~%")
    (format stream "  c~%")
    (format stream "}")))

(defparameter *vamp-example-2*
  (list (vspc:make-pub :wires (list :fi :bar))
        (vspc:make-alias
         :name :xor
         :inputs '(:a :b)
         :body
         (list (vspc:make-bind
                :names (list (vspc:make-wire :var :c))
                :value #1=(vspc:make-infix :op :*
                                           :lhs (vspc:make-wire :var :a)
                                           :rhs (vspc:make-wire :var :b)))
               (vspc:make-equality
                :lhs (vspc:make-wire :var :baz)
                :rhs (vspc:make-infix :op :+
                                      :lhs (vspc:make-wire :var :a)
                                      :rhs #1#))
               (vspc:make-application
                :func :foo
                :arguments (list #1#))
               (vspc:make-wire :var :c)))))

(defparameter *expected-2*
  (with-output-to-string (stream)
    (format stream "pub fi~%")
    (format stream "pub bar~%")
    (format stream "def xor a b {~%")
    (format stream "  def c = a * b;~%")
    (format stream "  baz = a + (a * b);~%")
    (format stream "  foo (a * b);~%")
    (format stream "  c~%")
    (format stream "}")))

(test extract-alias
  (let ((ran-1 (with-output-to-string (stream)
                 (vamp:extract *vamp-example-1* stream)))
        (ran-2 (with-output-to-string (stream)
                 (vamp:extract *vamp-example-2* stream))))
    (is (equalp *expected-1* ran-1))
    (is (equalp *expected-2* ran-2))))
