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

;;; This external merge sort protocol is slightly slower than the
;;; previous implementation, but is extendable to sort things other
;;; than line-based text files. Speed is roughly 70% of the speed of
;;; Unix sort using SBCL 1.0.30 on X86_64.

(defclass sort-input-stream (fundamental-input-stream)
  ()
  (:documentation "An input stream for reading and sorting the stream
contents."))

(defclass sort-output-stream (fundamental-output-stream)
  ()
  (:documentation "An output stream for sorting and writing the stream
contents."))

(defclass merge-stream (io-stream-mixin)
  ((element :initform nil
            :initarg :element
            :accessor element-of
            :documentation ""))
  (:documentation "An IO stream for merging sorted data."))

(defgeneric stream-peek (merge-stream)
  (:documentation "Returns the next element from MERGE-STREAM without
  removing it. Part of the external merge sort protocol."))

(defgeneric stream-merge (merge-stream)
  (:documentation "Returns the next element from MERGE-STREAM as part
  of the a merging operation between several {defclass merge-stream }
  s."))

(defgeneric stream-read-element (sort-input-stream)
  (:documentation "Returns the next element from SORT-INPUT-STREAM."))

(defgeneric stream-write-element (element sort-output-stream)
  (:documentation "Writes ELEMENT to SORT-OUTPUT-STREAM."))

(defgeneric make-merge-stream (sort-input-stream predicate
                               &key key buffer-size)
  (:documentation "Returns a new {defclass merge-stream} appropriate
to SORT-INPUT-STREAM. The new stream must return sorted elements read
from SORT-INPUT-STREAM.

Arguments:

- sort-input-stream (sort-input-stream): the stream whose elements are
to be sorted.

- predicate (function designator): the sorting predicate, as in
CL:SORT, a function of two arguments that returns a generalized
boolean.

Key:

- key (function designator): a function of one argument, or nil.

- buffer-size (fixnum): the size of the in-memory sort buffer and
  hence the number of elements written to disk in the external merge
  file.

Returns:

- a {defclass merge-stream} from which sorted elements may be read."))

(defgeneric external-merge-sort (sort-input-stream sort-output-stream predicate
                                 &key key buffer-size)
  (:documentation "Performs an external merge sort on the elements
read from SORT-INPUT-STREAM and writes the sorted elements to
SORT-OUTPUT-STREAM.

Arguments:

- sort-input-stream (sort-input-stream): the stream whose elements are
to be sorted.
- sort-output-stream (sort-output-stream): a stream whose elements are
sorted.

- predicate (function designator): the sorting predicate, as in
CL:SORT, a function of two arguments that returns a generalized
boolean.

Key:

- key (function designator): a function of one argument, or nil.

- buffer-size (fixnum): the size of the in-memory sort buffer and
  hence the number of elements written to disk in the external merge
  file.

Returns:

- the total number of elements sorted (fixnum).
- the number of {defclass merge-stream} s used in sorting (fixnum)."))

(defmethod stream-delete-file ((stream merge-stream))
  (delete-file (stream-of stream)))

(defmethod stream-peek ((stream merge-stream))
  (element-of stream))

(defmethod external-merge-sort ((in sort-input-stream) (out sort-output-stream)
                                predicate &key key (buffer-size 100000))
  (let* ((merge-streams
          (loop
             for stream = (make-merge-stream
                           in predicate :key key :buffer-size buffer-size)
             while stream
             collect stream into streams
             finally (return (make-array (length streams)
                                         :initial-contents streams))))
         (key (cond ((null key)
                     #'identity)
                    ((functionp key)
                     key)
                    (t
                     (fdefinition key)))))
    (unwind-protect
         (loop
            for elt = (merge-element merge-streams predicate key)
            while elt
            count elt into total
            do (stream-write-element elt out)
            finally (return (values total (length merge-streams))))
      (loop
         for stream across merge-streams
         do (progn
              (when (open-stream-p stream)
                #+:sbcl (ignore-errors  ; FIXME -- Workaround for bug
                                        ; 406271 in sbcl
                          (stream-delete-file stream))
                #-:sbcl(stream-delete-file stream)
                (close stream :abort t)))))))

(defun merge-element (merge-streams predicate key)
  "Returns the next element from one of MERGE-STREAMS. The returned
element is the on that sorts first according to PREDICATE and KEY, as
required by the merge-sort algorithm."
  (declare (optimize (speed 3)))
  (declare (type (simple-array merge-stream (*)) merge-streams)
           (type function predicate key))
  (loop
     with x = (stream-peek (aref merge-streams 0))
     and x-index = 0
     for y-index from 0 below (length merge-streams)
     for y = (stream-peek (aref merge-streams y-index))
     when (and y (or (null x)
                     (funcall predicate (funcall key y)
                              x)))
     do (setf x y
              x-index y-index)
     finally (progn
               (stream-merge (aref merge-streams x-index))
               (return x))))
