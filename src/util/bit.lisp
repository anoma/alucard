(in-package :alu.utils)

(defconstant +byte-size+ 8)

;; I can speed this up by manually setfing the fill pointer instead,
;; or tracking it, but makes the code less clear
(-> string-to-bit-array (string) (array bit))
(defun string-to-bit-array (string)
  (let* ((size      (string-bit-size string))
         (bit-array (make-array size :element-type 'bit :fill-pointer 0)))
    (map 'nil (lambda (c) (char-to-bit-array c bit-array)) string)
    bit-array))

(-> char-to-bit-array (character &optional (array bit)) (array bit))
(defun char-to-bit-array (char &optional
                                 (bit-array (make-array (char-code char)
                                                        :element-type 'bit
                                                        :fill-pointer 0)))
  (let ((numb (char-code char)))
    (dotimes (i (char-bit-size char) bit-array)
      (vector-push (if (logbitp i numb) 1 0) bit-array))))

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
