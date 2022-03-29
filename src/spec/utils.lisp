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

;; from
;; https://stackoverflow.com/questions/11067899/is-there-a-generic-method-for-cloning-clos-objects
(defgeneric copy-instance (object &rest initargs &key &allow-other-keys)
  (:documentation "Makes and returns a shallow copy of OBJECT.

  An uninitialized object of the same class as OBJECT is allocated by
  calling ALLOCATE-INSTANCE.  For all slots returned by
  CLASS-SLOTS, the returned object has the
  same slot values and slot-unbound status as OBJECT.

  REINITIALIZE-INSTANCE is called to update the copy with INITARGS.")
  (:method ((object standard-object) &rest initargs &key &allow-other-keys)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'c2mop:slot-definition-name (c2mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name)
                (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

;; I should use this for object equality, namely the slot values trick
