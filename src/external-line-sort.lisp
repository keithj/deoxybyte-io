;;;
;;; Copyright (C) 2009 Keith James. All rights reserved.
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

;;; An implementation of the external merge sort protocol for sorting
;;; lines in text files.
(defclass line-sort-input-stream (sort-input-stream wrapped-stream-mixin)
  ())

(defclass line-sort-output-stream (sort-output-stream wrapped-stream-mixin)
  ())

(defclass line-merge-stream (merge-stream wrapped-stream-mixin)
  ())

(defmethod stream-read-element ((stream line-sort-input-stream))
  (read-line (stream-of stream) nil nil))

(defmethod stream-write-element ((line string) (stream line-sort-output-stream))
  (write-line line (stream-of stream)))

(defmethod make-merge-stream ((stream line-sort-input-stream) predicate
                              &key key (buffer-size 100000))
  (let ((out (open (make-tmp-pathname :basename "line-merge-sort")
                   :direction :io
                   :element-type (stream-element-type stream)))
        (elements (make-array buffer-size :adjustable t :fill-pointer 0)))
    (loop
       for i from 0 below buffer-size
       for elt = (stream-read-element stream)
       while elt
       do (vector-push elt elements))
    (cond ((plusp (length elements))
           (loop
              for elt across (sort elements predicate :key key)
              do (write-line elt out)
              finally (if (file-position out 0)
                          (return
                            (make-instance 'line-merge-stream :stream out))
                          (error 'file-error :pathname out))))
          (t
           (close out :abort t)
           nil))))

(defmethod initialize-instance :after ((stream line-merge-stream) &key)
  (with-accessors ((s stream-of) (e element-of))
      stream
    (setf e (read-line s nil nil))))

(defmethod stream-merge ((stream line-merge-stream))
  (with-accessors ((s stream-of) (e element-of))
      stream
    (setf e (read-line s nil nil))))
