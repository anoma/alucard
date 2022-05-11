(in-package :alu.closure.dependency)

;; This is just a glorified hash table
(defclass typ ()
  ((direct :initarg  :direct
           :type     closure:typ        ; closure:typ (list keyword)
           :initform (closure:allocate)
           :accessor direct
           :documentation "The direct dependency mapping")
   (reverse :initarg  :reverse
            :type     closure:typ       ; closure:typ (list keyword)
            :initform (closure:allocate)
            :accessor reverse
            :documentation "The reverse dependency mapping")
   (cyclic :initarg :cyclic
           :type     closure:typ        ; clsoure:typ (list keyword)
           :initform (closure:allocate)
           :accessor cyclic
           :documentation "Cyclic dependencies")
   (solved :initarg :solved
           :type    list                ; list keyword
           :initform nil
           :accessor solved
           :documentation "The currently newly solved values that have
           not been cleared"))
  (:documentation "The Closure type"))

(defmethod print-object ((obj typ) stream)
  (print-unreadable-object (obj stream :type t)
    (pprint-logical-block (stream nil)
      (format stream ":DIRECT ~A~_:REVERSE ~A~_:CYCLIC ~A~_:SOLVED ~A"
              (direct obj) (reverse obj) (cyclic obj) (solved obj)))))

(-> allocate () typ)
(defun allocate ()
  (make-instance 'typ))

(-> insert (typ keyword list) typ)
(defun insert (dependency term depends-list)
  (apply #'add-dependencies dependency term depends-list))

(-> lookup (typ keyword) list)
(defun lookup (dependency term)
  (closure:lookup (direct dependency) term))

(defun solved-for (dependency term)
  (mvfold (lambda (dependency deps-on-term)
            (let ((direct (remove-from (direct dependency) deps-on-term term))
                  (cyclic (remove-from-if-exists (cyclic dependency) deps-on-term term)))
              (util:copy-instance dependency
                                  :direct direct
                                  :cyclic cyclic
                                  :solved
                                  (if (null (closure:lookup direct deps-on-term))
                                      (adjoin deps-on-term (solved dependency))
                                      (solved dependency)))))
          (closure:lookup (reverse dependency) term)
          (util:copy-instance dependency
                              :reverse (closure:remove (reverse dependency) term)
                              :cyclic  (closure:remove (cyclic dependency) term)
                              :solved  (adjoin term (solved dependency)))))

(defun add-dependencies (dependency term &rest depends-on)
  (let ((typ (handle-cyclic dependency term depends-on)))
    (util:copy-instance typ
                        :direct  (closure:insert (direct typ) term depends-on)
                        :reverse (add-reverse (reverse typ) term depends-on))))

(-> dump-solved (typ) typ)
(defun dump-solved (dependency)
  "Removes the solved values"
  (util:copy-instance dependency :solved nil))

(-> handle-cyclic (typ keyword list) typ)
(defun handle-cyclic (dependency term depends-on)
  (util:copy-instance
   dependency
   :cyclic (mvfold (lambda (closure dep)
                     (if (member term (closure:lookup (direct dependency) dep))
                         (adjoin-onto (adjoin-onto closure dep term) term dep)
                         closure))
                   depends-on
                   (cyclic dependency))))

(-> add-reverse (closure:typ keyword list) closure:typ)
(defun add-reverse (closure value depends-on)
  (mvfold (lambda (closure dependency)
            (adjoin-onto closure dependency value))
          depends-on
          closure))

(-> adjoin-onto (closure:typ keyword t) closure:typ)
(defun adjoin-onto (closure key value)
  (closure:insert closure key (adjoin value (closure:lookup closure key))))

(-> remove-from (closure:typ keyword keyword) closure:typ)
(defun remove-from (closure key member)
  (let ((value (remove-if (alexandria:curry #'eq member)
                          (closure:lookup closure key))))
    (if value
        (closure:insert closure key value)
        (closure:remove closure key))))

(-> remove-from-if-exists (closure:typ keyword keyword) closure:typ)
(defun remove-from-if-exists (closure key member)
  (if (closure:lookup closure key)
      (remove-from closure key member)
      closure))
