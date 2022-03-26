(in-package :alu.pass.relocation)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We assume the code is in ANF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "relocate-let generates out let bindings which remove the original let
bound to a record. This function also builds up a closure to where the
old value was relocated to."
  (let ((no-change
          (make-rel :forms bind :closure closure)))
    (with-accessors ((name spc:var) (val spc:value)) bind
      (etypecase-of spc:term-no-binding val
        (spc:application
         (let* ((func-name (spc:name (spc:func val)))
                (exp  (expand:full-return-values func-name)))
           (if (consp exp)
               ;; see doc on `expand:full-return-values' to see that
               ;; the type coincides with our nested alist representation
               (let* ((new-closure-value (update-alist-values-with-preifx name exp))
                      (new-bindings      (alist-values new-closure-value)))
                 (make-rel
                  :closure (closure:insert closure name new-closure-value)
                  :forms (spc:make-multiple-bind :var new-bindings :val val)))
               ;; If we don't get back a cons, then we aren't dealing
               ;; with a record return type or the record is not found
               no-change)))
        (spc:record
         (error "hi"))
        (spc:record-lookup
         (error "hi"))
        (spc:reference
         (let ((checked (closure:lookup closure (spc:name val))))
           (if checked
               ;; if it's there we have a reference to a record, expand!
               (let ((new-closure-content
                       (update-alist-values-with-preifx name checked)))
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

(-> relocate-standalone (spc:term-no-binding closure:typ) spc:expanded-term)
(defun relocate-standalone (term closure)
  "relocate-standalone is similar to `relocate-let' however, instead of
being let bound the value stands by itself, simply generates out the
term with the proper relocation."
  term closure
  (error "hi"))

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
                      (if (listp value)
                          (update-alist-values-with-preifx new-prefix value)
                          new-prefix)))))
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
