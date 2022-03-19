(in-package :alu.storage)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Storage Locations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; *type-table* : Hash-Table keyword type-storage
(defvar *types* (make-hash-table :test #'eq)
  "Serves as the table which stores all the circuit types that are
relevant to the system")

;; *function-table* : Hash-Table keyword function-type
(defvar *functions* (make-hash-table :test #'eq)
  "Serves as the table which stores all custom circuits that are
defined")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Storage Addition Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declaim (ftype (function (keyword format:function-type)) add-function))
(defun add-function (name func)
  "Adds the given function to the `*functions' table"
  (setf (gethash name *functions*) func))

(declaim (ftype (function (keyword format:type-storage)) add-type))
(defun add-type (name type)
  "Adds the given Type to the `*types*' table"
  (setf (gethash name *types*) type))

(declaim (ftype (function (keyword) (or format:function-type nil)) lookup-function))
(defun lookup-function (name)
  (gethash name *functions*))

(declaim (ftype (function (keyword) (or format:type-storage nil)) lookup-type))
(defun lookup-type (name)
  (gethash name *types*))

