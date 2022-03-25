(in-package :alu.closure)

;; This is just a glorified hash table
(defclass typ ()
  ((table :initarg  :table
          :type     syc:tree-map
          :initform (syc:make-tree-map #'util:hash-compare)
          :accessor table))
  (:documentation "The Closure type"))

(defmethod print-object ((obj typ) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "~A" (table obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Creation Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun allocate (&rest arguments)
  (from-plist arguments))

(-> from-plist (list) typ)
(defun from-plist (plist)
  (values
   (make-instance 'typ
                  :table (util:sycamore-plist-symbol-map plist))))

(-> from-alist (list) typ)
(defun from-alist (alist)
  (values
   (make-instance 'typ
                  :table (syc:alist-tree-map alist #'util:hash-compare))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Addition Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> insert (typ keyword t) typ)
(defun insert (closure name bound-value)
  (values
   (make-instance 'typ
                  :table (syc:tree-map-insert (table closure) name bound-value))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Lookup Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> lookup (typ keyword) (or t null))
(defun lookup (closure name)
  (values
   (syc:tree-map-find (table closure) name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Removal Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(-> remove (typ keyword) typ)
(defun remove (closure name)
  (values
   (make-instance 'typ
                  :table (syc:tree-map-remove (table closure) name))))
