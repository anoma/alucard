(in-package :alu.pass.relocation)

;; This module will have the arduous task of taking code like
;; <let ref-point = <POINT :x 300 :y :ref2>
;; <let fi        = <Nested-CORDS :PLANE :ref1 :Point :ref-point>

;; and generating out the following constructs

;; (<let ref-point-x = 300
;;  <let ref-point-y = :ref2>
;;  <let fi-plane    = :ref1>
;;  <let fi-point-x  = :ref-point-x>
;;  <let fi-point-y  = :ref-point-y>)

;; Along with constructing the following closure mapping

;; {} = sycamore mapping, ( . ) = alist

;; {:ref-point --> ((:x . :ref-poin-tx)  (:y . :ref-point-y)
;;  :fi        --> ((:plane . :fi-plane) (:point . ((:x . fi-point-x) (:y . fi-point-y))))
;; }
