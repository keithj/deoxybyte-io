;;;
;;; Copyright (c) 2007-2012 Keith James. All rights reserved.
;;;
;;; This file is part of deoxybyte-io.
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

(in-package :uk.co.deoxybyte-io-test)

;; NaN, +Inf and -Inf values serialized by Java (i.e. network byte
;; order). 
;; 
;;  NaN: 7f c0 00 00
;; +Inf: 7f 80 00 00
;; -Inf: ff 80 00 00
;;
;;  NaN: 7f f8 00 00 00 00 00 00
;; +Inf: 7f f0 00 00 00 00 00 00
;; -Inf: ff f0 00 00 00 00 00 00 

(defvar *single-float-nan-bytes* '(#x7f #xc0 #x00 #x00)
  "NaN serialized by Java.")
(defvar *single-float-positive-infinity-bytes* '(#x7f #x80 #x00 #x00)
  "+Inf serialized by Java.")
(defvar *single-float-negative-infinity-bytes* '(#xff #x80 #x00 #x00)
  "-Inf serialized by Java.")

(defvar *double-float-nan-bytes* '(#x7f #xf8 #x00 #x00 #x00 #x00 #x00 #x00)
  "NaN serialized by Java.")
(defvar *double-float-positive-infinity-bytes*
  '(#x7f #xf0 #x00 #x00 #x00 #x00 #x00 #x00)
  "+Inf serialized by Java.")
(defvar *double-float-negative-infinity-bytes*
  '(#xff #xf0 #x00 #x00 #x00 #x00 #x00 #x00)
  "-Inf serialized by Java.")

(defun make-byte-array (bytes)
  (make-array (length bytes) :element-type 'octet :initial-contents bytes))

(defun test-round-trip-int (num-bytes encoder decoder &optional signed)
  (let ((bytes (make-byte-array (loop
                                   repeat num-bytes
                                   collect 0)))
        (max (expt 2 (1- (* 8 num-bytes)))))
    (loop
       repeat 1000000
       always (let ((x (if signed
                           (- (random max) (/ max 2))
                         (random max))))
                (= (funcall decoder (funcall encoder x bytes)) x)))))

(defun test-round-trip-float (size encoder decoder &optional signed)
  (let ((range (ecase size
                 (4 most-positive-single-float)
                 (8 most-positive-double-float))))
    (loop
       repeat 1000000
       always (let ((x (if (and signed (zerop (random 2)))
                           (- (random range))
                         (random range))))
                (= (funcall decoder (funcall encoder x)) x)))))

(addtest (deoxybyte-io-tests) decode-unsigned-int-le/1
  (let ((bytes (make-byte-array '(#x78 #x56 #x34 #x12))))
    (ensure (= #x12345678 (decode-uint32le bytes 0)))
    (ensure (= #x5678 (decode-uint16le bytes 0)))
    (ensure (= #x1234 (decode-uint16le bytes 2)))))

(addtest (deoxybyte-io-tests) decode-unsigned-int-be/1
  (let ((bytes (make-byte-array '(#x12 #x34 #x56 #x78))))
    (ensure (= #x12345678 (decode-uint32be bytes 0)))
    (ensure (= #x1234 (decode-uint16be bytes 0)))
    (ensure (= #x5678 (decode-uint16be bytes 2)))))

(addtest (deoxybyte-io-tests) decode-signed-int-le/1
  (let ((bytes (make-byte-array '(#xf8 #xff #xff #xff))))
    (ensure (= -8 (decode-int32le bytes 0)))
    (ensure (= -8 (decode-int16le bytes 0)))
    (ensure (= -1 (decode-int16le bytes 2)))))

(addtest (deoxybyte-io-tests) decode-signed-int-be/1
  (let ((bytes (make-byte-array '(#xff #xff #xff #xf8))))
    (ensure (= -8 (decode-int32be bytes 0)))
    (ensure (= -1 (decode-int16be bytes 0)))
    (ensure (= -8 (decode-int16be bytes 2)))))

(addtest (deoxybyte-io-tests) decode-float32-le/1
  (mapc (lambda (bytes result)
          (ensure (eql result (decode-float32le (reverse bytes)))))
        (mapcar #'make-byte-array
                (list *single-float-nan-bytes*
                      *single-float-positive-infinity-bytes*
                      *single-float-negative-infinity-bytes*))
        '(:not-a-number :positive-infinity :negative-infinity)))

(addtest (deoxybyte-io-tests) decode-float32-be/1
  (mapc (lambda (bytes result)
          (ensure (eql result (decode-float32be bytes))))
        (mapcar #'make-byte-array
                (list *single-float-nan-bytes*
                      *single-float-positive-infinity-bytes*
                      *single-float-negative-infinity-bytes*))
        '(:not-a-number :positive-infinity :negative-infinity)))

(addtest (deoxybyte-io-tests) decode-float64-le/1
  (mapc (lambda (bytes result)
          (ensure (eql result (decode-float64le (reverse bytes)))))
        (mapcar #'make-byte-array
                (list *double-float-nan-bytes*
                      *double-float-positive-infinity-bytes*
                      *double-float-negative-infinity-bytes*))
        '(:not-a-number :positive-infinity :negative-infinity)))

(addtest (deoxybyte-io-tests) decode-float64-be/1
  (mapc (lambda (bytes result)
          (ensure (eql result (decode-float64be bytes))))
        (mapcar #'make-byte-array
                (list *double-float-nan-bytes*
                      *double-float-positive-infinity-bytes*
                      *double-float-negative-infinity-bytes*))
        '(:not-a-number :positive-infinity :negative-infinity)))


(addtest (deoxybyte-io-tests) round-trip-unsigned-int64-le/1
  (ensure (test-round-trip-int 8 #'encode-int64le #'decode-int64le)))

(addtest (deoxybyte-io-tests) round-trip-signed-int64-le/1
  (ensure (test-round-trip-int 8 #'encode-int64le #'decode-int64le t)))

(addtest (deoxybyte-io-tests) round-trip-unsigned-int32-le/1
  (ensure (test-round-trip-int 4 #'encode-int32le #'decode-int32le)))

(addtest (deoxybyte-io-tests) round-trip-signed-int32-le/1
  (ensure (test-round-trip-int 4 #'encode-int32le #'decode-int32le t)))

(addtest (deoxybyte-io-tests) round-trip-unsigned-int16-le/1
  (ensure (test-round-trip-int 2 #'encode-int16le #'decode-int16le)))

(addtest (deoxybyte-io-tests) round-trip-signed-int16-le/1
  (ensure (test-round-trip-int 2 #'encode-int16le #'decode-int16le t)))

(addtest (deoxybyte-io-tests) round-trip-signed-int8-le/1
  (ensure (test-round-trip-int 1 #'encode-int8le #'decode-int8le t)))


(addtest (deoxybyte-io-tests) round-trip-unsigned-int64-be/1
  (ensure (test-round-trip-int 8 #'encode-int64be #'decode-int64be)))

(addtest (deoxybyte-io-tests) round-trip-signed-int64-be/1
  (ensure (test-round-trip-int 8 #'encode-int64be #'decode-int64be t)))

(addtest (deoxybyte-io-tests) round-trip-unsigned-int32-be/1
  (ensure (test-round-trip-int 4 #'encode-int32be #'decode-int32be)))

(addtest (deoxybyte-io-tests) round-trip-signed-int32-be/1
  (ensure (test-round-trip-int 4 #'encode-int32be #'decode-int32be t)))

(addtest (deoxybyte-io-tests) round-trip-unsigned-int16-be/1
  (ensure (test-round-trip-int 2 #'encode-int16be #'decode-int16be)))

(addtest (deoxybyte-io-tests) round-trip-signed-int16-be/1
  (ensure (test-round-trip-int 2 #'encode-int16be #'decode-int16be t)))

(addtest (deoxybyte-io-tests) round-trip-signed-int8-be/1
  (ensure (test-round-trip-int 1 #'encode-int8be #'decode-int8be t)))

(addtest (deoxybyte-io-tests) round-trip-ieee-float32/1
  (ensure (test-round-trip-float 4 #'encode-ieee-float32
                                 #'decode-ieee-float32 t))
  (mapcar (lambda (n)
            (ensure (eql n (decode-ieee-float32 (encode-ieee-float32 n)))))
          '(:not-a-number :positive-infinity :negative-infinity)))

(addtest (deoxybyte-io-tests) round-trip-ieee-float64/1
  (ensure (test-round-trip-float 4 #'encode-ieee-float64
                                 #'decode-ieee-float64 t))
  (mapcar (lambda (n)
            (ensure (eql n (decode-ieee-float64 (encode-ieee-float64 n)))))
          '(:not-a-number :positive-infinity :negative-infinity)))
