(in-package :alu.utils)

(defun symbol-to-keyword (symbol)
  (intern (symbol-name symbol) :keyword))

(defun hash-compare (x y)
  "hash compare compare twos symbols"
  (let ((hash-x (sxhash x))
        (hash-y (sxhash y)))
    (cond  ((< hash-x hash-y) -1)
           ((> hash-x hash-y)  1)
           (t                  0))))

(defun sycamore-plist-symbol-map (plist)
  (sycamore:alist-tree-map (alexandria:plist-alist plist) #'hash-compare))

(defun sycamore-symbol-map-plist (tree-map)
  (alexandria:alist-plist (sycamore:tree-map-alist tree-map)))
