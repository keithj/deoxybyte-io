;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(in-package :cl-io-utilities-test)

(defun make-byte-array (bytes)
  (make-array (length bytes) :element-type '(unsigned-byte 8)
              :initial-contents bytes))

(defun test-round-trip (num-bytes encoder decoder &optional signed)
  (let ((bytes (make-byte-array (loop
                                   repeat num-bytes
                                   collect 0)))
        (max (expt 2 (1- (* 8 num-bytes)))))
    (loop
       repeat 1000000
       always (let ((x (if signed
                           (- (random max) (/ max 2))
                         (random max))))
                (= (funcall decoder (funcall encoder x bytes)))))))

(addtest (cl-io-utilities-tests) decode-unsigned-int-le/1
  (let ((bytes (make-byte-array '(#x78 #x56 #x34 #x12))))
    (ensure (= #x12345678 (decode-uint32le bytes 0)))
    (ensure (= #x5678 (decode-uint16le bytes 0)))
    (ensure (= #x1234 (decode-uint16le bytes 2)))))

(addtest (cl-io-utilities-tests) decode-unsigned-int-be/1
  (let ((bytes (make-byte-array '(#x12 #x34 #x56 #x78))))
    (ensure (= #x12345678 (decode-uint32be bytes 0)))
    (ensure (= #x1234 (decode-uint16be bytes 0)))
    (ensure (= #x5678 (decode-uint16be bytes 2)))))

(addtest (cl-io-utilities-tests) decode-signed-int-le/1
  (let ((bytes (make-byte-array '(#xf8 #xff #xff #xff))))
    (ensure (= -8 (decode-int32le bytes 0)))
    (ensure (= -8 (decode-int16le bytes 0)))
    (ensure (= -1 (decode-int16le bytes 2)))))

(addtest (cl-io-utilities-tests) decode-signed-int-be/1
  (let ((bytes (make-byte-array '(#xff #xff #xff #xf8))))
    (ensure (= -8 (decode-int32be bytes 0)))
    (ensure (= -1 (decode-int16be bytes 0)))
    (ensure (= -8 (decode-int16be bytes 2)))))

(addtest (cl-io-utilities-tests) round-trip-unsigned-int64-le
  (ensure (test-round-trip 8 #'encode-int64le #'decode-int64le)))

(addtest (cl-io-utilities-tests) round-trip-signed-int64-le
  (ensure (test-round-trip 8 #'encode-int64le #'decode-int64le t)))

(addtest (cl-io-utilities-tests) round-trip-unsigned-int32-le
  (ensure (test-round-trip 4 #'encode-int32le #'decode-int32le)))

(addtest (cl-io-utilities-tests) round-trip-signed-int32-le
  (ensure (test-round-trip 4 #'encode-int32le #'decode-int32le t)))

(addtest (cl-io-utilities-tests) round-trip-unsigned-int16-le
  (ensure (test-round-trip 2 #'encode-int16le #'decode-int16le)))

(addtest (cl-io-utilities-tests) round-trip-signed-int16-le
  (ensure (test-round-trip 2 #'encode-int16le #'decode-int16le t)))

(addtest (cl-io-utilities-tests) round-trip-signed-int8-le
  (ensure (test-round-trip 1 #'encode-int8le #'decode-int8le t)))


(addtest (cl-io-utilities-tests) round-trip-unsigned-int64-be
  (ensure (test-round-trip 8 #'encode-int64be #'decode-int64be)))

(addtest (cl-io-utilities-tests) round-trip-signed-int64-be
  (ensure (test-round-trip 8 #'encode-int64be #'decode-int64be t)))

(addtest (cl-io-utilities-tests) round-trip-unsigned-int32-be
  (ensure (test-round-trip 4 #'encode-int32be #'decode-int32be)))

(addtest (cl-io-utilities-tests) round-trip-signed-int32-be
  (ensure (test-round-trip 4 #'encode-int32be #'decode-int32be t)))

(addtest (cl-io-utilities-tests) round-trip-unsigned-int16-be
  (ensure (test-round-trip 2 #'encode-int16be #'decode-int16be)))

(addtest (cl-io-utilities-tests) round-trip-signed-int16-be
  (ensure (test-round-trip 2 #'encode-int16be #'decode-int16be t)))

(addtest (cl-io-utilities-tests) round-trip-signed-int8-be
  (ensure (test-round-trip 1 #'encode-int8be #'decode-int8be t)))
