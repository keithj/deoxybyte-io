;;;
;;; Copyright (c) 2010-2012 Keith James. All rights reserved.
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

(in-package :uk.co.deoxybyte-io)

(defconstant +frac-factor-32+ (expt 2 23)
  "Mantissa fraction float32 multiplication factor")
(defconstant +min-norm-exponent-32+ -126
  "Smallest normalized float32 exponent")
(defconstant +exponent-bias-32+ 127
  "Exponent bias for float32")
(defconstant +not-a-number-32+ #b1111111110000000000000000000000)
(defconstant +positive-infinity-32+ #b1111111100000000000000000000000)
(defconstant +negative-infinity-32+ #b11111111100000000000000000000000)

(defconstant +frac-factor-64+ (expt 2 52)
  "Mantissa fraction float64 multiplication factor")
(defconstant +min-norm-exponent-64+ -1022
  "Smallest normalized float64 exponent")
(defconstant +exponent-bias-64+ 1023
  "Exponent bias for float64")
(defconstant +not-a-number-64+
  #b111111111111000000000000000000000000000000000000000000000000000)
(defconstant +positive-infinity-64+
  #b111111111110000000000000000000000000000000000000000000000000000)
(defconstant +negative-infinity-64+
  #b1111111111110000000000000000000000000000000000000000000000000000)

;;; IEEE 754 float to integer conversions with support for reading and
;;; writing NaN and infinity.

(defun encode-ieee-float32 (f)
  "Encodes single-float F as a 32-bit integer using IEEE 754
encoding. NaN, +Inf and -Inf may be represented by using the keywords
:not-a-number :positive-infinity or :negative-infinity respectively,
as an argument."
  (etypecase f
    (symbol (ecase f
              (:not-a-number +not-a-number-32+)
              (:positive-infinity +positive-infinity-32+)
              (:negative-infinity +negative-infinity-32+)))
    (number
     (let ((f (coerce f 'single-float)))
       (multiple-value-bind (significand exponent sign)
           (decode-float f)
         (let ((n 0))
           (setf (ldb (byte 1 31) n) (if (minusp sign)
                                         1
                                         0))
           (if (>= (abs f) least-positive-normalized-single-float)
               (let ((normalized-signif (* 2 significand)) ; a normalized float
                     (normalized-exp (1- exponent)))
                 (setf (ldb (byte 8 23) n) (+ normalized-exp +exponent-bias-32+)
                       (ldb (byte 23 0) n) (round (* (1- normalized-signif)
                                                     +frac-factor-32+))))
               (setf (ldb (byte 23 0) n)     ; a denormalized float
                     (ash (round (* significand +frac-factor-32+))
                          (- exponent +min-norm-exponent-32+))))
           n))))))

(defun decode-ieee-float32 (n)
  "Decodes an IEEE 754 encoded single-float from 32-bit integer
N. NaN, +Inf and -Inf are represented by the keywords :not-a-number
:positive-infinity or :negative-infinity respectively."
  (let ((sign (if (zerop (ldb (byte 1 31) n))
                  1.0
                  -1.0))
        (exponent (ldb (byte 8 23) n))
        (significand (ldb (byte 23 0) n)))
    (cond ((= #b11111111 exponent)      ; nan or infinity
           (cond ((zerop significand)
                  (cond ((plusp sign)
                         :positive-infinity)
                        (t
                         :negative-infinity)))
                 (t
                  :not-a-number)))
          (t
           (cond ((zerop exponent)      ; a denormalized float
                  (setf exponent +min-norm-exponent-32+))
                 (t                     ; a normalized float
                  (decf exponent +exponent-bias-32+)
                  (setf (ldb (byte 1 23) significand) 1))) ; replace hidden bit
           (* sign (scale-float (float significand) (- exponent 23)))))))

(defun encode-ieee-float64 (f)
  "Encodes double-float F as a 64-bit integer using IEEE 754
encoding. NaN, +Inf and -Inf may be represented by using the keywords
:not-a-number :positive-infinity or :negative-infinity respectively,
as an argument."
  (etypecase f
    (symbol (ecase f
              (:not-a-number +not-a-number-64+)
              (:positive-infinity +positive-infinity-64+)
              (:negative-infinity +negative-infinity-64+)))
    (number
     (let ((f (coerce f 'double-float)))
       (multiple-value-bind (significand exponent sign)
           (decode-float f)
         (let ((n 0))
           (setf (ldb (byte 1 63) n) (if (minusp sign)
                                         1
                                         0))
           (if (>= (abs f) least-positive-normalized-double-float)
               (let ((norm-signif (* 2 significand)) ; a normalized float
                     (norm-exp (1- exponent)))
                 (setf (ldb (byte 11 52) n) (+ norm-exp +exponent-bias-64+)
                       (ldb (byte 52 0) n) (round (* (1- norm-signif)
                                                     +frac-factor-64+))))
               (setf (ldb (byte 52 0) n)     ; a denormalized float
                     (ash (round (* significand +frac-factor-64+))
                          (- exponent +min-norm-exponent-64+))))
           n))))))

(defun decode-ieee-float64 (n)
  "Decodes an IEEE 754 encoded double-float from 64-bit integer
  N. NaN, +Inf and -Inf are represented by the keywords
:not-a-number :positive-infinity or :negative-infinity respectively."
  (let ((sign (if (zerop (ldb (byte 1 63) n))
                  1.0d0
                -1.0d0))
        (exponent (ldb (byte 11 52) n))
        (significand (ldb (byte 52 0) n)))
    (cond ((= #b11111111111 exponent)      ; nan or infinity
           (cond ((zerop significand)
                  (cond ((plusp sign)
                         :positive-infinity)
                        (t
                         :negative-infinity)))
                 (t
                  :not-a-number)))
          (t
           (cond ((zerop exponent)             ; a denormalized float
                  (setf exponent +min-norm-exponent-64+))
                 (t                            ; a normalized float
                  (decf exponent +exponent-bias-64+)
                  (setf (ldb (byte 1 52) significand) 1))) ; replace hidden bit
           (* sign (scale-float (float significand 1.0d0) (- exponent 52)))))))
