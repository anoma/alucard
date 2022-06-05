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
  forms closure)

;; TODO.
;; simplify this code with another pass.
;;
;; Instead of generating out multiple lets for records, do a
;; multi-bind, have a pass that then remove the mutli-binds on a
;; record, and remove the last records.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Core API
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO Update for Record FORMS from array lookup!
(-> relocate-let (ir:bind closure:typ) rel)
(defun relocate-let (bind closure)
  "relocate-let generates out let bindings which remove the original let
bound to a record. This function also builds up a closure to where the
old value was relocated to."
  (let ((no-change
          (make-rel :forms (list bind) :closure closure)))
    (with-accessors ((name ir:var) (val ir:value)) bind
      (etypecase-of ir:term-no-binding val
        (ir:number no-change)
        ;; TODO Update for Record FORMS from array lookup!
        (ir:array-forms no-change)
        (ir:reference
         (let ((checked (closure:lookup closure (ir:name val))))
           (if checked
               (make-rel-from-alist name checked closure)
               no-change)))
        (ir:application
         (let* ((func-name (ir:name (ir:func val)))
                (exp       (expand:full-return-values func-name)))
           (if (consp exp)
               ;; see doc on `expand:full-return-values' to see that
               ;; the type coincides with our nested alist representation
               (let* ((new-closure-value (update-alist-values-with-preifx name exp))
                      (new-bindings      (util:alist-values new-closure-value)))
                 (make-rel
                  :closure (closure:insert closure name new-closure-value)
                  :forms   (list
                            (ir:make-multiple-bind :var new-bindings :val val))))
               ;; If we don't get back a cons, then we aren't dealing
               ;; with a record return type or the record is not found
               no-change)))
        (ir:record-lookup
         ;; has to be a ref due to ANF, gotta love ANF
         (let* ((lookup (closure:lookup closure (ir:name (ir:record val))))
                (find   (find (ir:field val) lookup :key #'car)))
           ;; use something better than error for error reporting
           (cond ((null lookup)
                  (error
                   "Trying to do a lookup on an unknown record ~A" val))
                 ((null find)
                  (error "Trying to do a lookup on a non existant field"))
                 ;; format must be a record, as the format is akin to
                 ;; find = (PLANE (X . NEST-PLANE-X) (Y . NEST-PLANE-Y))
                 ((listp (cdr find))
                  (let* ((rel (make-rel-from-alist name (list find) closure))
                         (clos (rel-closure rel)))
                    ;; need to cdr away the nested label that we used to generate
                    (make-rel
                     :forms   (rel-forms rel)
                     :closure (closure:insert clos
                                              name
                                              (cdar (closure:lookup clos name))))))
                 ;; must be an atom, meaning that it must not be bound to a record
                 ;; find = (X . PLANE-PLANE-X)
                 (t (make-rel
                     :forms (list (generate-bind (cdr find) name))
                     :closure closure)))))
        (ir:record
         (let* ((alist (ir:record->alist val))
                ;; we now have to recursively update the alist such that
                ;; all the nested terms works out, and save it under the
                ;; original name. we can do this via induction/recursion.
                ;;
                ;; Thus this happens when the value is a reference to
                ;; another record, and thus we expand to another set
                ;; of lets.
                (recursed-on-args
                  (reduce (lambda (pair rel)
                            ;; since the pair comes from the alist, it
                            ;; must be a normal form, on the right
                            ;; thus either:
                            ;;
                            ;; 1. number
                            ;; 2. reference
                            ;;
                            ;; pair: (X . #<REFERENCE X>)
                            ;; pair: (Y . 5)
                            (destructuring-bind (field-name . value) pair
                              (let* ((name    (append-keywords name field-name))
                                     (recurse (relocate-let
                                               (ir:make-bind :var name :val value)
                                               (rel-closure rel))))
                                (make-rel
                                 :closure (rel-closure recurse)
                                 :forms   (append (rel-forms recurse)
                                                  (rel-forms rel))))))
                          alist
                          :from-end t
                          :initial-value (make-rel :forms nil :closure closure)))
                (closure-mapping-for-current
                  ;; here we need to construct the closure mapping for
                  ;; the current name. If the field itself is a nested
                  ;; structure then we grab the names that were added
                  ;; during the recursion. Once we have all the names,
                  ;; we have the complete mapping for the current
                  ;; record binding term.
                  (mapcar (lambda (field-name)
                            (let* ((name      (append-keywords name field-name))
                                   (clos-look (closure:lookup (rel-closure
                                                               recursed-on-args)
                                                              name)))
                              (cons field-name
                                    (or clos-look name))))
                          (mapcar #'car alist))))
           (make-rel
            :closure (closure:insert (rel-closure recursed-on-args)
                                     name
                                     closure-mapping-for-current)
            :forms (rel-forms recursed-on-args))))))))

(-> initial-closure-from-circuit (ir:circuit &optional closure:typ) closure:typ)
(defun initial-closure-from-circuit (circ &optional (closure (closure:allocate)))
  (let ((expanded-arguments (expand:full-arguments-from-circuit circ)))
    (reduce (lambda (const closure)
              (etypecase-of expand:argument const
                (ir:constraint closure)
                (expand:expand  (closure:insert closure
                                                (expand:original const)
                                                (argument-list-to-closure-alist
                                                 (expand:expanded const))))))
            expanded-arguments
            :from-end t
            :initial-value closure)))

(-> maps-to (keyword closure:typ) list)
(defun maps-to (name closure)
  (values
   (util:alist-values (closure:lookup closure name))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(-> argument-list-to-closure-alist (list) list)
(defun argument-list-to-closure-alist (arglist)
  (mapcar (lambda (x)
            (destructuring-bind (field . constraint) x
              (cons field
                    (etypecase-of expand:argument constraint
                      (ir:constraint (ir:name constraint))
                      (expand:expand  (argument-list-to-closure-alist
                                       (expand:expanded constraint)))))))
          arglist))

(-> make-rel-from-alist (keyword list closure:typ) rel)
(defun make-rel-from-alist (prefix alist closure)
  "Takes an alist and a prefix and generates the proper relocation
relation"
  ;; we have a reference to a record, now lets expand!
  (let ((new-closure-content (update-alist-values-with-preifx prefix alist)))
    (make-rel
     :forms (generate-binds (util:alist-values alist)
                            (util:alist-values new-closure-content))
     :closure (closure:insert closure prefix new-closure-content))))

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
  (mapcar #'generate-bind from to))

(-> generate-bind (keyword keyword) ir:bind)
(defun generate-bind (from to)
  (ir:make-bind :var to :val (ir:make-reference :name from)))

(-> update-alist-values-with-preifx (keyword list) list)
(defun update-alist-values-with-preifx (prefix alist)
  "This function clones the values from one given alist into a new prefix.

Example:
(update-alist-values-with-preifx
 :hi
 '((:plane . :fi-plane) (:point . ((:x . :fi-point-x) (:y . :fi-point-y)))))

==>

((:plane . :hi-plane) (:point . ((:x . :hi-point-x) (:y . :hi-point-y))))"
  (mapcar (lambda (apair)
            (destructuring-bind (key . value) apair
              (let ((new-prefix (append-keywords prefix key)))
                (cons key
                      (if (listp value)
                          (update-alist-values-with-preifx new-prefix value)
                          new-prefix)))))
          alist))

(-> append-keywords (keyword keyword) keyword)
(defun append-keywords (prefix end)
  (values
   (keyword-combine prefix :- end)))

(defun keyword-combine (&rest keywords)
  (intern (apply #'concatenate 'string (mapcar #'symbol-name keywords))
          'keyword))
