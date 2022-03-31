(in-package :alu-test)

(def-suite vampir
  :description "Test the vampir layer")

(in-suite vampir)


(defparameter *vamp-example-1*
  (list (vspc:make-pub :wires (list :fi :bar))
        (vspc:make-alias
         :name :xor
         :inputs '(:a :b)
         :outputs '(:c)
         :body (list (vspc:make-bind
                      :names (list (vspc:make-wire :var :c))
                      :value (vspc:make-infix :op :+
                                              :lhs (vspc:make-wire :var :a)
                                              :rhs (vspc:make-wire :var :b)))))))

(test extract-alias
  (let ((expected-string
          (format nil "pub fi~%pub bar~%def xor a b -> c {~%~2tc = a + b~%}"))
        (ran (with-output-to-string (stream)
               (vamp:extract *vamp-example-1* stream))))
    (is (equalp expected-string ran))))
