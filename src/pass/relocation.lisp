(in-package :alu.pass.relocation)

;; This module will have the arduous task of taking code like
;; (<let ref-point = <POINT :x 300 :y :ref2>
;;  <let fi        = <Nested-CORDS :PLANE :ref1 :Point :ref-point>
;;  <let faz       = <APP function-to-point ...>
;;  ...)

;; and generating out the following constructs

;; (<let ref-point-x = 300
;;  <let ref-point-y = :ref2>
;;  <let fi-plane    = :ref1>
;;  <let fi-point-x  = :ref-point-x>
;;  <let fi-point-y  = :ref-point-y>
;;  <multi (faz-point-x faz-point-y) = <APP function-to-point ...>>
;;  ...)

;; Along with constructing the following closure mapping

;; {} = sycamore mapping, ( . ) = alist

;; { :ref-point --> ((:x . :ref-point-x)  (:y . :ref-point-y))
;;   :faz       --> ((:x . :faz-point-x)  (:y . :faz-point-y))
;;   :fi        --> ((:plane . :fi-plane) (:point . ((:x . fi-point-x) (:y . fi-point-y))))
;; }

;; Thus the type of this is keyword --> #1=alist keyword (or keyword #1#)

;; short lived structure, it's fine for it to be a struct
(defstruct rel
  "Deals with return values that need to update the closure and expanded let forms"
  forms
  closure)

(-> relocate-let (spc:bind closure:typ) rel)
(defun relocate-let (bind closure)
  (let ((no-change (make-rel :forms bind :closure closure)))
    (with-accessors ((name spc:var) (val spc:value)) bind
      (etypecase-of spc:term-no-binding val
        (spc:application
         no-change)
        (spc:record
         no-change)
        (spc:record-lookup
         no-change)
        ;; don't need to be a pointer chaser, just lookup in closure, if
        ;; I need to relocate!
        (spc:reference
         (let ((checked (closure:lookup closure (spc:name val))))
           (if checked
               ;; if it's there we have to clone the alist on the other side!
               (let ((new-closure-content (update-alist-values-with-preifx name checked)))
                 (make-rel
                  :forms (generate-binds (alist-values checked)
                                         (alist-values new-closure-content))
                  :closure (closure:insert closure name new-closure-content)))
               ;; if it's not a pointer which is a record itself, we're good!
               no-change)))
        (spc:number
         no-change)
        (spc:primitive
         no-change)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generate-binds (from to)
  "this function generates a bind over the two given lists

Example:
(generate-binds
  '(:FI-PLANE :FI-POINT-X :FI-POINT-Y)
  '(:HI-PLANE :HI-POINT-X :HI-POINT-Y))

==>
(#<LET HI-PLANE   = #<REFERENCE FI-PLANE>>
 #<LET HI-POINT-X = #<REFERENCE FI-POINT-X>>
 #<LET HI-POINT-Y = #<REFERENCE FI-POINT-Y>>)"
  (mapcar (lambda (from to)
            (spc:make-bind :var to :val (spc:make-reference :name from)))
          from to))

(-> update-alist-values-with-preifx (keyword list) list)
(defun update-alist-values-with-preifx (prefix alist)
  "This function clones the values from one given alist into a new prefix.

Example:
(update-alist-values-with-preifx
 :hi
 '((:plane . :fi-plane) (:point . ((:x . :fi-point-x) (:y . :fi-point-y)))))

result = ((:plane . :hi-plane) (:point . ((:x . :hi-point-x) (:y . :hi-point-y))))"
  (mapcar (lambda (apair)
            (destructuring-bind (key  . value) apair
              (let ((new-prefix (keyword-combine prefix :- key)))
                (cons key
                      (if (keywordp value)
                          new-prefix
                          (update-alist-values-with-preifx new-prefix value))))))
          alist))

(defun alist-values (alist)
  "Takes a potentially nested alist and returns the values"
  (mapcan (lambda (apair)
            (if (not (listp (cdr apair)))
                (list (cdr apair))
                (alist-values (cdr apair))))
          alist))

(defun keyword-combine (&rest keywords)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name keywords))
          'keyword))
