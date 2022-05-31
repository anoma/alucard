(in-package :alu.pass.pack)

(-> op (check.type:type-info &rest ir:term-normal-form) ir:expanded-list)
(defun op (type-format &rest data)
  type-format data
  (error "not implemented"))

(-> lookup-at (check.type:type-info (or fixnum keyword) ir:term-normal-form)
    ir:expanded-list)
(defun lookup-at (type to-find data)
  type to-find data
  (error "not implemented"))

(-> lookup-at (check.type:type-info ir:term-normal-form) ir:expanded-term)
(defun unpack (type data)
  type data
  (error "not implemented"))
