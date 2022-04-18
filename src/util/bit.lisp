(in-package :alu.utils)

(defconstant +byte-size+ 8)

(-> string-to-number (string) integer)
(defun string-to-number (string)
  "converts a string to a numerical encoding"
  ;; if we had map-accum-r, we could do this with an accumulator
  (let ((cont 0))
    (apply #'+
           (map 'list (lambda (c)
                        (prog1 (ash (char-code c) (* cont +byte-size+))
                          (incf cont (char-byte-size c))))
                (reverse string)))))

;; I can speed this up by manually setfing the fill pointer instead,
;; or tracking it, but makes the code less clear
(-> string-to-bit-array (string) bit-vector)
(defun string-to-bit-array (string)
  "converts a string to a bit-vector encoding. Should agree with `string-to-number'"
  (let* ((size      (string-bit-size string))
         (bit-array (make-array size :element-type 'bit :fill-pointer 0)))
    (map nil (lambda (c) (char-to-bit-array c bit-array)) string)
    bit-array))

(-> char-to-bit-array (character &optional bit-vector) bit-vector)
(defun char-to-bit-array (char &optional
                                 (bit-array (make-array (char-code char)
                                                        :element-type 'bit
                                                        :fill-pointer 0)))
  (let ((numb (char-code char))
        (size (char-bit-size char)))
    (dotimes (i (char-bit-size char) bit-array)
      (vector-push (if (logbitp (- size i 1) numb) 1 0) bit-array))))

(-> string-bit-size (string) fixnum)
(defun string-bit-size (string)
  (* (string-byte-size string) +byte-size+))

(-> char-bit-size (character) fixnum)
(defun char-bit-size (char)
  (* (char-byte-size char) +byte-size+))

(-> string-byte-size (string) fixnum)
(defun string-byte-size (string)
  (values
   (sum (map 'list #'char-byte-size string))))

(-> char-byte-size (character) fixnum)
(defun char-byte-size (char)
  "Calculates how many bytes is needed to model the current char"
  (ceiling (integer-length (char-code char))
           +byte-size+))
