;;;
;;; Copyright (c) 2007-2011 Keith James. All rights reserved.
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

(defconstant +octet-buffer-size+ 4096
  "Buffer size for {defclass octet-line-input-stream} internal
buffer.")

(deftype octet-buffer ()
  "Buffer type for {defclass octet-line-input-stream} internal
buffer."
  `(simple-array (unsigned-byte 8) (,+octet-buffer-size+)))

(deftype octet-buffer-index ()
  "Index type for {defclass octet-line-input-stream} internal
buffer."
  `(integer 0 ,+octet-buffer-size+))

(defclass line-stream ()
  ()
  (:documentation "A line-based stream. Useful for building readers
and writers for ad-hoc text data sources."))

(defclass line-input-stream (line-stream fundamental-input-stream)
  ((line-stack :initform nil
               :accessor line-stack-of
               :documentation "A list of lines that have been pushed
back into the stream to be read again."))
  (:documentation "A line-based stream that allows lines to be pushed
back into a stack to be re-read."))

(defclass character-line-input-stream (wrapped-stream-mixin line-input-stream)
  ())

(defclass octet-line-input-stream (wrapped-stream-mixin line-input-stream)
  ((buffer :initarg :buffer
           :initform (make-array +octet-buffer-size+
                                 :element-type 'octet :initial-element 0)
           :documentation "The buffer from which lines are read.")
   (eol-code :initarg :eol-code
             :initform (char-code #\Newline)
             :documentation "The end of line character code. If two
characters are used, this is the first of the pair.")
   (num-bytes :initform 0
              :documentation "The number of bytes that were read into
the buffer from the stream.")
   (offset :initform 0
           :documentation "The offset in the byte buffer from which
the next byte is to be read."))
  (:documentation "A {defclass line-input-stream} whose lines are
arrays of bytes. Allows buffered reading of lines of (unsigned-byte 8)
from a stream."))

(defclass line-output-stream (line-stream fundamental-output-stream)
  ())

(defclass character-line-output-stream (line-output-stream)
  ())

(defclass octet-line-output-stream (line-output-stream)
  ())

(defgeneric make-line-stream (stream)
  (:method ((stream stream))
    (let* ((elt-type (stream-element-type stream))
           (class (cond ((input-stream-p stream)
                         (cond ((subtypep elt-type 'character)
                                'character-line-input-stream)
                               ((subtypep 'octet elt-type)
                                'octet-line-input-stream)))
                        ((output-stream-p stream)
                         (cond ((subtypep elt-type 'character)
                                'character-line-output-stream)
                               ((subtypep 'octet elt-type)
                                'octet-line-output-stream))))))
      (check-arguments class (stream)
                       "unable to make a line-input stream from ~a" stream)
      (make-instance class :stream stream)))
  (:documentation "Returns a new {defclass line-stream} created from
STREAM."))

(defgeneric pop-line (line-input-stream line)
  (:method ((stream line-input-stream) (line string))
    (pop (slot-value stream 'line-stack)))
  (:documentation "Pops one line from {defclass line-input-stream} ."))

(defgeneric push-line (line-input-stream line)
  (:method ((stream line-input-stream) (line string))
    (push line (slot-value stream 'line-stack)))
  (:documentation "Pushes LINE back onto {defclass line-input-stream} ."))

(defgeneric find-line (line-input-stream test &optional max-lines)
  (:method ((stream line-input-stream) test &optional max-lines)
    (do* ((line (stream-read-line stream) (stream-read-line stream))
          (matching-line-p (and (vectorp line) (funcall test line))
                           (and (vectorp line) (funcall test line)))
          (line-count 1 (1+ line-count)))
         ((or (eql :eof line)
              matching-line-p
              (and (not (null max-lines))
                   (= line-count max-lines)))
          (values line matching-line-p line-count))))
  (:documentation "Iterates through lines read from LINE-INPUT-STREAM
until a line matching predicate TEST is found or until a number of
lines equal to MAX-LINES have been examined."))

(defgeneric more-lines-p (line-input-stream)
  (:method ((stream character-line-input-stream))
    (with-slots ((s stream) line-stack)
        stream
      (or line-stack (not (eql :eof (peek-char nil s nil :eof))))))
  (:method ((stream octet-line-input-stream))
    (with-slots ((s stream) buffer offset num-bytes line-stack)
        stream
      (when (zerop num-bytes)
        (setf offset 0
              num-bytes (read-sequence buffer s)))
      (or line-stack (not (zerop num-bytes)))))
  (:documentation "Returns T if {defclass line-input-stream} contains
unread data."))

;;; line-stream-methods
(defmethod stream-element-type ((stream line-stream))
  'string)

;;; character-line-input-stream methods
(defmethod stream-element-type ((stream character-line-input-stream))
  'string)

(defmethod stream-file-position ((stream character-line-input-stream)
                                 &optional position)
  (with-slots ((s stream) line-stack)
      stream
    (cond (position
           (setf line-stack ())
           (file-position s position))
          (t
           (let ((buffered-chars (loop
                                    for line in line-stack
                                    sum (1+ (length line))))) ; eol
             (- (file-position s) buffered-chars))))))

(defmethod stream-clear-input ((stream character-line-input-stream))
  (setf (slot-value stream 'line-stack) nil))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream character-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-read-sequence stream sequence start end)))

(defmethod stream-read-line ((stream character-line-input-stream))
  (with-slots ((s stream) line-stack)
      stream
    (if (null line-stack)
        (multiple-value-bind (line missing-eol-p)
            (read-line s nil :eof)
          (values line missing-eol-p))
        (pop line-stack))))

;;; octet-line-input-stream methods
(defmethod stream-element-type ((stream octet-line-input-stream))
  'string)

(defmethod stream-file-position ((stream octet-line-input-stream)
                                 &optional position)
  (with-slots ((s stream) offset num-bytes line-stack)
      stream
    (cond (position
           (setf num-bytes 0
                 offset 0
                 line-stack ())
           (file-position s position))
          (t
           (- (file-position s) (- num-bytes offset))))))

(defmethod stream-clear-input ((stream octet-line-input-stream))
  (with-slots (offset num-bytes line-stack)
      stream
    (setf offset 0
          num-bytes 0
          line-stack ())))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream octet-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-read-sequence stream sequence start end)))

(defmethod stream-read-line ((stream octet-line-input-stream))
  (declare (optimize (speed 3)))
  (flet ((build-string (chunks) ; this is 3x faster than with-output-to-string
           (let ((length (if (endp (rest chunks))
                             (length (the simple-octet-vector (first chunks)))
                             (reduce #'+ chunks :key #'length))))
             (declare (type vector-index length))
             (loop
                with line = (make-array length :element-type 'base-char)
                with offset of-type vector-index = 0
                for chunk of-type simple-octet-vector in chunks
                for clength = (length chunk)
                do (unless (zerop clength)
                     (copy-vector chunk 0 clength
                                  line offset #'code-char)
                     (incf offset clength))
                finally (return line)))))
    (with-slots (line-stack)
        stream
      (if (null line-stack)
          (multiple-value-bind (chunks has-eol-p)
              (read-octet-line stream)
            (if (null chunks)
                (values :eof t)
                (values (build-string chunks) has-eol-p)))
          (pop line-stack)))))

;;; line-output-stream methods
#+(or :sbcl :ccl)
(defmethod stream-write-sequence ((stream line-output-stream)
                                  sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-write-sequence stream sequence start end)))

(defgeneric stream-write-line (string stream &optional start end)
  (:method ((str string) (stream character-line-output-stream)
            &optional (start 0) end)
    (write-line str (slot-value stream 'stream) :start start :end end))
  (:method ((str string) (stream octet-line-output-stream)
            &optional (start 0) end)
    (let ((end (or end (length str)))
          (octets (make-array (- end start) :element-type 'octet)))
      (copy-vector str 0 end
                   octets 0 #'char-code)
      (write-sequence octets (slot-value stream 'stream)))))

(defun read-octet-line (stream)
  "Reads chunks of bytes up to the next newline or end of stream,
returning them in a list. The newline is not included. Returns two
values - a list of chunks and either NIL or T to indicate whether a
terminating newline was missing. When the stream underlying the buffer
is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 0)))
  (with-slots ((s stream) buffer offset num-bytes eol-code)
      stream
    (declare (type octet-buffer buffer)
             (type octet-buffer-index offset num-bytes))
    (labels ((buffer-empty-p ()
               (= offset num-bytes))
             (fill-buffer ()
               (setf offset 0
                     num-bytes (read-sequence buffer s)))
             (find-eol ()
               (let ((eol-pos (position eol-code buffer
                                        :start offset :end num-bytes)))
                 (cond ((and eol-pos (plusp (- eol-pos offset)))
                        ;; There is a newline in the buffer, but not
                        ;; at the zeroth position. Make a chunk up to
                        ;; the newline
                        (let ((chunk (make-array (- eol-pos offset)
                                                 :element-type 'octet
                                                 :initial-element 0)))
                          (replace chunk buffer :start2 offset :end2 eol-pos)
                          (setf offset (1+ eol-pos))
                          (values (list chunk) nil)))
                       ((and eol-pos (zerop (- eol-pos offset)))
                        ;; There is a newline in the buffer at the
                        ;; zeroth position.
                        (setf offset (1+ eol-pos))
                        (values nil nil))
                       ((zerop num-bytes)
                        ;; The buffer is empty
                        (values nil t))
                       (t
                        ;; There is no newline in the buffer. Make a
                        ;; chunk and recurse to find the newline
                        (let ((chunk (make-array (- num-bytes offset)
                                                 :element-type 'octet
                                                 :initial-element 0)))
                          (replace chunk buffer :start2 offset :end2 num-bytes)
                          (fill-buffer)
                          (multiple-value-bind (chunks missing-eol-p)
                              (find-eol)
                            (values (cons chunk chunks) missing-eol-p))))))))
      (when (buffer-empty-p)
        (fill-buffer))
      (find-eol))))

(defun %stream-read-sequence (stream sequence start end)
  (loop
     with n = 0
     for i from start below end
     for line = (stream-read-line stream)
     until (or (eql :eof line) (= end i))
     do (setf (elt sequence i) line
              n (1+ n))
     finally (return n)))

(defun %stream-write-sequence (stream sequence start end)
  (loop
     for i from start below end
     do (write-sequence (elt sequence i) stream)))
