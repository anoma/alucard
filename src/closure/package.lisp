
(defpackage #:alu.closure
  (:documentation "Provides a simple closure structure")
  (:local-nicknames (:syc  :sycamore)
                    (:util :alu.utils))
  (:shadow #:remove #:length)
  (:use #:common-lisp #:serapeum)
  (:export
   :typ
   :allocate
   :from-plist
   :from-alist
   :insert
   :length
   :lookup
   :remove
   :keys))

(defpackage #:alu.closure.dependency
  (:documentation "Provides a dependency closure that shows dependency
  between values")
  (:shadow #:remove #:reverse #:length)
  (:local-nicknames (:syc     #:sycamore)
                    (:util    #:alu.utils)
                    (:closure #:alu.closure))
  (:use #:common-lisp #:serapeum)
  (:export
   :typ
   :allocate
   :determined-by
   :lookup
   :solved-for
   :solved-for*
   :get-solved
   :dump-solved
   :add-dependencies
   :determine-each-other))
