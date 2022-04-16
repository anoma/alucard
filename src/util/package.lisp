(defpackage #:alu.utils
  (:documentation "provides the utility functions for the Alucard project")
  (:shadow #:deftype)
  (:use #:common-lisp #:serapeum)
  (:export
   :symbol-to-keyword
   :hash-compare
   :sycamore-plist-symbol-map
   :sycamore-symbol-map-plist
   :copy-instance

   ;; Alist Helpers
   :alist-values
   :leaf-alist-keys
   :nested-alist-keys

   ;; Bit packing functionality
   :string-to-bit-array
   :char-to-bit-array))
