;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(in-package :cl-io-utilities)

(defmacro define-binary-encoder (name &key (bytes 1) (order :little-endian))
  "Defines a function NAME with one mandatory argument, a simple-array
of unsigned-byte 8, and one optional argument, a fixnum index in that
array that defaults to 0. The function returns the Lisp integer whose
value is given by the bytes at that index in the byte array.

Key:

- bytes (fixnum): the number of bytes that comprise the number.
- order (sumbol): the byte order of the array, may be one
  of :little-endian or :big-endian ( :network-byte-order may be used
  as a synonym for :big-endian )."
  (let ((byte-shifts
         (ecase order
           (:little-endian
            (loop
               for i from 0 to (* 8 (1- bytes)) by 8
               collect i))
           ((:big-endian :network-byte-order)
            (loop
               for i from (* 8 (1- bytes)) downto 0 by 8
               collect i)))))
    `(progn
      (declaim (inline ,name))
      (defun ,name (value buffer &optional (index 0))
        ;; (declare (optimize (speed 3) (safety 1)))
        (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                 (type array-index index))
        ,@(loop
             for i from 0 below bytes
             for shift in byte-shifts
             collect `(setf (aref buffer (+ index ,i))
                            (ldb (byte 8 ,shift) value)))
        buffer))))

(defmacro define-binary-decoder (name &key (bytes 1) (order :little-endian)
                                 signed)
  "Defines a function NAME with one mandatory argument, a simple-array
of unsigned-byte 8, and one optional argument, a fixnum index in that
array that defaults to 0. The function returns the Lisp integer whose
value is given by the bytes at that index in the byte array.

Key:

- bytes (fixnum): the number of bytes that comprise the number.
- order (sumbol): the byte order of the array, may be one
  of :little-endian or :big-endian ( :network-byte-order may be used
  as a synonym for :big-endian ).
- signed (boolean): T if the bytes are a two's complement
  representation of a signed integer."
  (let ((byte-shifts
         (ecase order
           (:little-endian
            (loop
               for i from 0 to (* 8 (1- bytes)) by 8
               collect i))
           ((:big-endian :network-byte-order)
            (loop
               for i from (* 8 (1- bytes)) downto 0 by 8
               collect i))))
        (mask (1- (ash 1 (* 8 bytes))))
        (sign-bit (1- (* 8 bytes))))
    `(progn
      (declaim (inline ,name))
      (defun ,name (buffer &optional (index 0))
        ,(format nil (txt "Decodes ~:[an unsigned~;a signed~] ~a byte ~a"
                          "integer stored as consecutive bytes in BUFFER,"
                          "starting at INDEX.") signed bytes
                          (string-downcase (symbol-name order)))
        ;; (declare (optimize (speed 3) (safety 1)))
        (declare (type (simple-array (unsigned-byte 8) (*)) buffer)
                 (type array-index index))
        (let ((value (logior ,@(loop
                                  for i from 0 below bytes
                                  for shift in byte-shifts
                                  collect `(ash (aref buffer (+ index ,i))
                                            ,shift)))))
          ,(if signed
               `(if (logbitp ,sign-bit value)
                    (- (1+ (logxor value ,mask)))
                  value)
               'value))))))

(define-binary-encoder encode-int64le :bytes 8 :order :little-endian)
(define-binary-encoder encode-int32le :bytes 4 :order :little-endian)
(define-binary-encoder encode-int16le :bytes 2 :order :little-endian)
(define-binary-encoder encode-int8le :bytes 1 :order :little-endian)

(define-binary-decoder decode-uint64le :bytes 8 :order :little-endian)
(define-binary-decoder decode-uint32le :bytes 4 :order :little-endian)
(define-binary-decoder decode-uint16le :bytes 2 :order :little-endian)
(define-binary-decoder decode-uint8le :bytes 1 :order :little-endian)

(define-binary-decoder decode-int64le :bytes 8 :order :little-endian :signed t)
(define-binary-decoder decode-int32le :bytes 4 :order :little-endian :signed t)
(define-binary-decoder decode-int16le :bytes 2 :order :little-endian :signed t)
(define-binary-decoder decode-int8le :bytes 1 :order :little-endian :signed t)

(define-binary-encoder encode-int64be :bytes 8 :order :big-endian)
(define-binary-encoder encode-int32be :bytes 4 :order :big-endian)
(define-binary-encoder encode-int16be :bytes 2 :order :big-endian)
(define-binary-encoder encode-int8be :bytes 1 :order :big-endian)

(define-binary-decoder decode-uint64be :bytes 8 :order :big-endian)
(define-binary-decoder decode-uint32be :bytes 4 :order :big-endian)
(define-binary-decoder decode-uint16be :bytes 2 :order :big-endian)
(define-binary-decoder decode-uint8be :bytes 1 :order :big-endian)

(define-binary-decoder decode-int64be :bytes 8 :order :big-endian :signed t)
(define-binary-decoder decode-int32be :bytes 4 :order :big-endian :signed t)
(define-binary-decoder decode-int16be :bytes 2 :order :big-endian :signed t)
(define-binary-decoder decode-int8be :bytes 1 :order :big-endian :signed t)
