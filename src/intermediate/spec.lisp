(in-package :alu.ir.spec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IR Variants through the pipeline
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The Pipeline for how these types get used is roughly as follows.
;;
;;        +-----------------------------------+
;;        |             Linear-Term           |
;;        +-----------------------------------+
;;        | Alu Term through ANF              |
;;        |                                   |
;;        | Binders contain term-type-manip… |
;;        |                                   |
;;        | No other changes                  |
;;        |                                   |
;;        +-----------------------------------+
;;                          |
;;                          |
;;                          v
;;        +-----------------------------------+
;;        |           Type Aware Term         |
;;        +-----------------------------------+
;;        | Term through removal of top level |
;;        | normal form                       |
;;        |                                   |
;;        | Binders contain term-type-manip… |
;;        |                                   |
;;        |                                   |
;;        | We run algorithms like            |
;;        | - type checking                   |
;;        | - Expansion                       |
;;        | - ETC                             |
;;        +-----------------------------------+
;;                          |
;;                          |
;;                          v
;;        +-----------------------------------+
;;        |             Expanded-Term         |
;;        +-----------------------------------+
;;        | Term through removal of top level |
;;        | normal forms                      |
;;        |                                   |
;;        | Binders contain term-no-binders   |
;;        |                                   |
;;        |                                   |
;;        | We run algorithms like            |
;;        | - type checking                   |
;;        | - Expansion                       |
;;        | - ETC                             |
;;        +-----------------------------------+
;;                        |
;;                        |
;;                        v
;;        +-----------------------------------+
;;        |        Fully-Expanded-Term        |
;;        +-----------------------------------+
;;        | Term where concepts like          |
;;        | Arrays and records are            |
;;        | expanded out.                     |
;;        |                                   |
;;        | Binders contain spc:base          |
;;        |                                   |
;;        | We run algorithms like            |
;;        | - void removal                    |
;;        | - extra let removal               |
;;        | - ETC                             |
;;        +-----------------------------------+
;;

(deftype linear-term ()
  "A Linear term is a term with no nested terms and is in proper ANF form."
  `(or spc:term-no-binding
       (starting-binders spc:term-type-manipulation)
       terms:standalone-ret))

(deftype type-aware-term ()
  "An expanded term is a term where all top level forms have been
expanded into lets or returns"
  `(or (starting-binders spc:term-type-manipulation)
       terms:standalone-ret))

(deftype expanded-term ()
  "An expanded term is a term where all top level forms have been
expanded into lets or returns, and type coercions have been removed"
  `(or (starting-binders spc:term-no-binding)
       terms:standalone-ret))

(deftype fully-expanded-term ()
  "A fully expanded term is a `expanded-term' with the records part
removed. Or as we can view it a base term, with the binders added in."
  `(or (binders spc:base)
       terms:standalone-ret))

(deftype starting-binders (&optional contains)
  "Terms which deal with binding and naming, the input argument
represents what data may be in the value of the binders."
  (declare (ignore contains))
  `(or terms:bind
       spc:bind-constraint))

(deftype binders (&optional contains)
  "Terms which deal with binding and naming, the input argument
represents what data may be in the value of the binders."
  `(or (starting-binders ,contains)
       terms:multiple-bind))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Linearized types List Aliases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype constraint-list ()
  "A constraint-list is a list of linear-terms"
  `(satisfies linear-list))

(deftype expanded-list ()
  "A constraint-list is a list of expanded-terms"
  `(satisfies expanded-list))

(deftype fully-expanded-list ()
  "A constraint-list is a list of fully-expanded-terms"
  `(satisfies fully-expanded-list))

(defun linear-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'linear-term)) list)))

(defun expanded-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'expanded-term)) list)))

(defun fully-expanded-list (list)
  (and (listp list)
       (every (lambda (x) (typep x 'fully-expanded-term)) list)))
