
(defpackage #:alu.closure
  (:documentation "Provides a simplified term structure that has been through
linearization, use alu.pass.linear-spec for the full specification")
  (:local-nicknames (:syc  :sycamore)
                    (:util :alu.utils))
  (:shadow #:remove)
  (:use #:common-lisp #:serapeum)
  (:export
   :allocate
   :from-plist
   :from-alist
   :insert
   :lookup
   :remove))
