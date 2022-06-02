(in-package :alu-test)

(def-suite alucard.packing
  :description "Packing logic")

(in-suite alucard.packing)

(test array-packing-is-expected
  (let ((int32 (ir:to-type-reference-format '(int 32))))
    (multiple-value-bind (context body)
        (check:with-intro (context bar baz) (make-instance 'check:typing-context)
          (pack:op
            context
            (check:make-type-info
             :type (ir:array-type :length 5
                                  :type int32)
             :size (* 32 5))
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
          "Every value should be without a hole here, so it's all the lets +")))

  (check:with-intro (context bar foo) (make-instance 'check:typing-context)
    (finishes
      (alu.pass.array::handle-term
       (alu.typechecker::solved
        bar
        (check:make-type-info
         :type (ir:array-type :length 5
                              :type (ir:to-type-reference-format '(int 32)))
         :size (* 32 5))
        context)
       (ir:make-bind :var bar
                     :val (ir:make-from-data :contents (list (ir:make-reference :name foo)
                                                             (ir:make-reference :name foo)
                                                             (ir:make-reference :name foo)
                                                             (ir:make-reference :name foo))))))))

;; TODO Make a real test here. Namely for types and proper logic generaiton
(test array-lookup-is-expected
  (finishes
    (check:with-intro (context bar) (make-instance 'check:typing-context)
      (pack:lookup-at
       context
       (check:make-type-info
        :type (ir:array-type :length 5
                             :type (ir:to-type-reference-format '(int 32)))
        :size (* 32 5))
       3
       (ir:make-reference :name bar)))))
