(in-package :alu.spec)

(defclass direct-slots-mixin () ()
  (:documentation
   "Provides the service of giving generic access to all direct fields in
the class. This allows generic programming to be had "))

;; Issue is that we have to include protected by hand on all classes
;; that I want to use this ☹
(defclass protect-slots-mixin (direct-slots-mixin)
  ((protected :initform (make-hash-table :test #'eq)
              :allocation :class
              :accessor protected))
  (:documentation
   "Extends the service of `direct-slot-mixin' by allowing the filtering
of fields that the programmer does not want to be treated
generically. Further, class allocated slots are ignored."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric direct-slot-names (obj)
  (:documentation "Grabs all the direct slots of the class."))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Protect slots custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> protect-slots (symbol &rest symbol) (or t null))
(defun protect-slots (class-name &rest protected-symbols)
  "Add the following protected symbols to the class's direct-slot list"
  (let ((table (slot-value (c2mop:class-prototype (find-class class-name))
                           'protected)))
    (mapc (lambda (symbol)
            (setf (gethash symbol table) t))
          protected-symbols)))

(defmethod direct-slot-names ((obj protect-slots-mixin))
  (filter-map (lambda (x)
                (let ((name (c2mop:slot-definition-name x)))
                  (if (or (eq (c2mop:slot-definition-allocation x) :class)
                          (gethash name (protected obj)))
                      nil
                      (c2mop:slot-definition-name x))))
              (c2mop:class-direct-slots (class-of obj))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Direct slots custom functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod direct-slot-names ((obj direct-slots-mixin))
  (mapcar #'c2mop:slot-definition-name
          (c2mop:class-direct-slots (class-of obj))))

(defmethod direct-slot-keywords ((obj direct-slots-mixin))
  "Grabs all the direct slots of the class, interned to being keywords"
  (mapcar #'util:symbol-to-keyword (direct-slot-names obj)))

(defmethod direct-slots ((obj direct-slots-mixin))
  "Grabs all the direct slots of the class and gives back a plist"
  (mapcar (lambda (x)
            (cons (util:symbol-to-keyword x)
                  (slot-value obj x)))
          (direct-slot-names obj)))

(defmethod direct-slot-values ((obj direct-slots-mixin))
  "Grabs the data from the class"
  (mapcar (lambda (x) (slot-value obj x))
          (direct-slot-names obj)))

(defun update-from-alist (obj alist)
  (apply #'util:copy-instance obj (alexandria:alist-plist alist)))
