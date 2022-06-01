(in-package :alu-test)

(def-suite alucard.packing
  :description "Packing logic")

(in-suite alucard.packing)

(test array-packing-is-expected
  (let ((int32 (ir:to-type-reference-format '(int 32))))
    (multiple-value-bind (context body)
        (check:with-intro (context bar baz) (make-instance 'check:typing-context)
          (pack:op
            (check:make-type-info
             :type (ir:array-type :length 5
                                  :type int32)
             :size (* 32 5))
            context
            35
            (ir:make-reference :name bar)
            (ir:make-reference :name baz)
            37
            (ir:make-type-check :value 38 :typ int32)))
      (is (>= (length body) 6)
          "Since we go through ANF, and we do 5 *'s for the strategy should
produce over 6 let bindings")
      (is (= (closure:length (check:typing-closure context))
             (+ 2 (length body)))
          "Every value should be without a hole here, so it's all the lets + "))))
