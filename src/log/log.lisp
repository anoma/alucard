(in-package :alu.log)

(deftype mode ()
  "Determines the behavior of the "
  `(or (eql :report) (eql :error)))

(defparameter *mode*
  :error
  "The mode in which the error reporter works in.
:report    Reports the error to the user, with the object being returned
:error     Reports the error to the user, with a simple error thrown")

(define-condition error (cl:error)
  ((data :initarg :data
         :reader  data)))

(defun error (object-in-question cat message &rest args-to-message)
  "Reports Errors to the user"
  (let ((stack   (ignore-errors (spc:stack object-in-question)))
        (message (apply #'format nil message args-to-message)))
    (v:error cat "~A~%[STACK]~%~A" message stack)
    (etypecase-of mode *mode*
      ((eql :report) object-in-question)
      ((eql :error)  (cl:error 'error :data object-in-question)))))

(defmethod v:format-message ((stream stream) (message v:message))
  (format stream "~a~%[~5,a] ~{<~a>~}:~%~A"
          (local-time:format-timestring
           NIL (v:timestamp message) :format v:*timestamp-format*)
          (v:level message)
          (v:categories message)
          (v:format-message NIL (v:content message))))
