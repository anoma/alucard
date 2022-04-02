
(defpackage #:alu.closure
  (:documentation "Provides a simple closure structure")
  (:local-nicknames (:syc  :sycamore)
                    (:util :alu.utils))
  (:shadow #:remove)
  (:use #:common-lisp #:serapeum)
  (:export
   :typ
   :allocate
   :from-plist
   :from-alist
   :insert
   :lookup
   :remove))
