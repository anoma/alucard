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

(-> add-function (keyword format:function-type) t)
(defun add-function (name func)
  "Adds the given function to the `*functions' table"
  (setf (gethash name *functions*) func))

(-> add-type (keyword format:type-storage) t)
(defun add-type (name type)
  "Adds the given Type to the `*types*' table"
  (setf (gethash name *types*) type))

(-> lookup-function (keyword) (or format:function-type null))
(defun lookup-function (name)
  (gethash name *functions*))

(-> lookup-type (keyword) (or format:type-storage null))
(defun lookup-type (name)
  (gethash name *types*))

