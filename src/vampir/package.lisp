
;; Setup this package to extract戶川純
(defpackage #:alu.vampir
  (:documentation "Provides a vampir representation")
  (:use #:common-lisp #:serapeum)
  (:shadow :=)
  (:local-nicknames (#:util #:alu.utils))
  (:export :defpoly :poly))


(defpackage #:alu.vampir.spec
  (:documentation "The Vampir model specification")
  (:use #:common-lisp)
  (:shadow :=)
  (:local-nicknames (#:util #:alu.utils)
                    (#:ser #:serapeum))
  (:export))

(defpackage #:alu.vampir.extraction
  (:documentation "Extracting Vamp-IR")
  (:use #:common-lisp #:serapeum)
  (:shadow :=)
  (:local-nicknames (:util :alu.utils))
  (:export))

