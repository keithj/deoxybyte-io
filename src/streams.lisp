;;;
;;; Copyright (c) 2008-2011 Keith James. All rights reserved.
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

(defclass wrapped-stream-mixin ()
  ((stream :initarg :stream
           :reader stream-of
           :documentation "The underlying stream from which data are
read."))
  (:documentation "A Gray-stream wrapping a standard Lisp stream."))

(defclass stream-filter-mixin ()
  ((test :initarg :test
         :reader test-of
         :documentation "A function designator for a test that returns
T when the next datum read from the stream is to be ignored."))
  (:documentation "A mixin that provides a filtering function for
streams. Any data encountered while reading or writing for which the
test returns T are ignored and skipped."))

(defclass io-stream-mixin (fundamental-input-stream
                           fundamental-output-stream)
  ())

;;; Deoxybyte Gray streams generic functions
(defgeneric stream-open (filespec class &rest initargs)
  (:documentation "Returns a Gray stream of CLASS"))

(defgeneric stream-close (stream &key abort)
  (:documentation "Closes STREAM, returning T if STREAM was open. If
ABORT is T, attempts to clean up any side effects of having created stream."))

(defgeneric stream-delete-file (stream)
  (:documentation "Equivalent to CL:DELETE-FILE."))

;;; Methods common to all Gray streams
(defmethod stream-element-type ((stream wrapped-stream-mixin))
  (stream-element-type (slot-value stream 'stream)))

(defmethod open-stream-p ((stream wrapped-stream-mixin))
  (open-stream-p (slot-value stream 'stream)))

(defmethod stream-file-position ((stream wrapped-stream-mixin)
                                 &optional position)
  (file-position (slot-value stream 'stream) position))

;;; Deoxybyte Gray streams methods
(defmethod stream-open (filespec class &rest initargs)
  (make-instance class :stream (apply #'open filespec initargs)))

(defmethod stream-close ((stream wrapped-stream-mixin) &key abort)
  (close (slot-value stream 'stream) :abort abort))

(defmethod stream-delete-file ((stream wrapped-stream-mixin))
  (delete-file (slot-value stream 'stream)))

;;; Methods common to Gray input streams
(defmethod stream-clear-input ((stream io-stream-mixin))
  nil)

;;; Methods common to Gray output streams
(defmethod stream-clear-output ((stream io-stream-mixin))
  nil)

(defmethod stream-finish-output ((stream io-stream-mixin))
  nil)

(defmethod stream-force-output ((stream io-stream-mixin))
  nil)

;;; Initialization
(defmethod initialize-instance :after ((stream wrapped-stream-mixin) &key)
  (check-arguments (streamp (slot-value stream 'stream)) (stream)
                   "expected a stream argument"))
