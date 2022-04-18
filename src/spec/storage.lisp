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

(defvar *entry-point* nil
  "Serves as the entry point to the generated circuit")

(defvar *cannonical-function-table* nil
  "serves as the backup of the original function table. Useful when
swapping to another env")

(defvar *cannonical-type-table* nil
  "serves as the backup of the original type table. Useful when
swapping to another env")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entry Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-entry-point ()
  "Grabs the entry point function for the alucard program"
  *entry-point*)

(defun set-entry-point (keyword)
  (setf *entry-point* keyword))
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


(-> swap-tables (hash-table hash-table) null)
(defun swap-tables (func-table type-table)
  (when (and (not *cannonical-function-table*)
             (not *cannonical-type-table*))
    (setf *cannonical-function-table* *functions*
          *cannonical-type-table*     *types*))

  (setf *functions* func-table
        *types* type-table)
  nil)

(-> restore-tables () null)
(defun restore-tables ()
  (if (or (not *cannonical-function-table*)
          (not *cannonical-type-table*))
      (error "Must swap the tables to restore the original tables!")
      (progn
        (setf *functions* *cannonical-function-table*
              *types*     *cannonical-type-table*)
        (setf *cannonical-function-table* nil
              *cannonical-type-table*     nil)
        nil)))

(-> currently-swapped? () boolean)
(defun currently-swapped? ()
  (and (hash-table-p *cannonical-function-table*)
       (hash-table-p *cannonical-function-table*)))
