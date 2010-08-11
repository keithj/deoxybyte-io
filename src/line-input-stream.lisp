;;;
;;; Copyright (C) 2007-2010 Keith James. All rights reserved.
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

(defconstant +byte-buffer-size+ 8192
  "Buffer size for {defclass binary-line-input-stream} internal
buffer.")

(deftype byte-buffer ()
  "Buffer type for {defclass binary-line-input-stream} internal
buffer."
  `(simple-array (unsigned-byte 8) (,+byte-buffer-size+)))

(deftype byte-buffer-index ()
  "Index type for {defclass binary-line-input-stream} internal
buffer."
  `(integer 0 ,+byte-buffer-size+))

(defclass line-input-stream (wrapped-stream-mixin)
  ((line-stack :initform nil
               :documentation "A list of lines that have been pushed
back into the stream to be read again."))
  (:documentation "A line-based stream that allows lines to be pushed
back into a stack to be re-read."))

(defclass character-line-input-stream (line-input-stream
                                       fundamental-character-input-stream)
  ()
  (:documentation "A {defclass line-input-stream} whose lines are
strings."))

(defclass binary-line-input-stream (line-input-stream
                                    fundamental-binary-input-stream)
  ((buffer :initarg :buffer
           :initform nil
           :documentation "The buffer from which lines are read.")
   (nl-code :initarg :nl-code
            :documentation "The newline character code.")
   (num-bytes :initform 0
              :documentation "The number of bytes that were read into
the buffer from the stream.")
   (offset :initform 0
           :documentation "The offset in the byte buffer from which
the next byte is to be read."))
  (:documentation "A {defclass line-input-stream} whose lines are
arrays of bytes. Allows buffered reading of lines of (unsigned-byte 8)
from a stream."))


;;; line-input-stream generic functions
(defgeneric more-lines-p (line-input-stream)
  (:documentation "Returns T if {defclass line-input-stream} contains
unread data."))

(defgeneric push-line (line-input-stream line)
  (:documentation "Pushes LINE back into {defclass line-input-stream} ."))

(defgeneric find-line (line-input-stream test &optional max-lines)
  (:documentation "Iterates through lines read from LINE-INPUT-STREAM
until a line matching predicate TEST is found or until a number of
lines equal to MAX-LINES have been examined."))

;;; line-input-stream constructor
(defun make-line-input-stream (stream)
  "Returns a new {defclass character-line-input-stream} or
{defclass binary-line-input-stream} wrapping STREAM. The element type
of STREAM must be either a subclass of  CHARACTER or (UNSIGNED-BYTE 8)."
  (check-arguments (and (streamp stream)
                        (input-stream-p stream)
                        (open-stream-p stream)) (stream)
                        "expected an open input-stream")
  (let ((elt-type (stream-element-type stream)))
    (cond ((subtypep elt-type 'character)
           (make-instance 'character-line-input-stream :stream stream))
          ((subtypep 'octet elt-type)
           (make-instance 'binary-line-input-stream
                          :stream stream
                          :nl-code (char-code #\Newline)
                          :buffer (make-array +byte-buffer-size+
                                              :element-type 'octet
                                              :initial-element 0)))
          (t
           (check-arguments nil (stream)
                            "invalid element type ~a from stream ~a"
                            (list elt-type stream))))))

;;; line-input-stream methods
(defmethod find-line ((stream line-input-stream) test
                      &optional max-lines)
  (do* ((line (stream-read-line stream) (stream-read-line stream))
        (matching-line-p (and (vectorp line) (funcall test line))
                         (and (vectorp line) (funcall test line)))
        (line-count 1 (1+ line-count)))
       ((or (eql :eof line)
            matching-line-p
            (and (not (null max-lines))
                 (= line-count max-lines)))
        (values line matching-line-p line-count))))


;;; character-line-input-stream methods
(defmethod stream-clear-input ((stream character-line-input-stream))
  (setf (slot-value stream 'line-stack) nil))

(defmethod stream-read-char ((stream character-line-input-stream))
  (with-slots ((s stream) line-stack)
      stream
    (if (null line-stack)
        (read-char s nil :eof)
        (read-elt-from-line-stack s))))

(defmethod stream-unread-char ((stream character-line-input-stream)
                               (char character))
  (with-slots ((s stream) line-stack)
      stream
    (cond ((null line-stack)
           (unread-char char s))
          ((char= #\Newline char)
           (push (make-array 0 :element-type (cl:type-of char)) line-stack))
          (t
           (let* ((line (pop line-stack))
                  (copy (make-array (1+ (length line))
                                    :element-type (array-element-type line)
                                    :initial-element #\Nul)))
             (setf (aref copy 0) char)
             (push (replace copy line :start1 1) line-stack))))
    nil))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream character-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (stream-read-sequence-with-line-stack stream sequence start end)))

#+:lispworks
(defmethod stream-read-sequence ((stream character-line-input-stream)
                                 sequence start end)
  (stream-read-sequence-with-line-stack stream sequence start end))

(defmethod stream-read-line ((stream character-line-input-stream))
   (with-slots ((s stream) line-stack)
       stream
     (if (null line-stack)
         (multiple-value-bind (line missing-newline-p)
             (read-line s nil :eof)
           (values line missing-newline-p))
         (pop line-stack))))

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
                                  sum (1+ (length line))))) ; 1+ for newline
           (- (file-position s) buffered-chars))))))

(defmethod more-lines-p ((stream character-line-input-stream))
  (with-slots ((s stream) line-stack)
      stream
    (or line-stack (not (eql :eof (peek-char nil s nil :eof))))))

(defmethod push-line ((stream character-line-input-stream) (line string))
  (push line (slot-value stream 'line-stack)))

;;; binary-line-input-stream methods
(defmethod stream-clear-input ((stream binary-line-input-stream))
  (with-slots (offset num-bytes line-stack)
      stream
    (setf offset 0
          num-bytes 0
          line-stack ())))

(defmethod stream-read-byte ((stream binary-line-input-stream))
  (with-slots ((s stream) buffer offset num-bytes line-stack)
      stream
    (cond ((and (null line-stack) (buffer-empty-p offset num-bytes))
           (read-byte s))
          ((null line-stack)
           (prog1
               (aref buffer offset)
             (incf offset)))
          (t
           (read-elt-from-line-stack stream)))))

#+(or :sbcl :ccl)
(defmethod stream-read-sequence ((stream binary-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (stream-read-sequence-with-line-stack stream sequence start end)))

#+:lispworks
(defmethod stream-read-sequence ((stream binary-line-input-stream)
                                 sequence start end)
  (stream-read-sequence-with-line-stack stream sequence start end))

(defmethod stream-read-line ((stream binary-line-input-stream))
  (with-slots (line-stack)
      stream
    (if (null line-stack)
        (multiple-value-bind (chunks has-newline-p)
            (read-chunks stream)
          (cond ((null chunks)
                 (values :eof t))
                ((zerop (length (first chunks)))
                 (first chunks))
                ((= 1 (length chunks))
                 (values (first chunks) has-newline-p))
                (t
                 (values (concatenate-chunks chunks) has-newline-p))))
        (pop line-stack))))

(defmethod stream-file-position ((stream binary-line-input-stream)
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

(defmethod more-lines-p ((stream binary-line-input-stream))
  (with-slots ((s stream) buffer offset num-bytes line-stack)
      stream
    (when (zerop num-bytes)
      (setf offset 0
            num-bytes (read-sequence buffer s))
      (or line-stack
          (not (zerop num-bytes))))))

(defmethod push-line ((stream binary-line-input-stream) (line vector))
  (with-slots (line-stack)
      stream
    (push line line-stack)))

(defun read-chunks (stream)
  "Reads chunks of bytes up to the next newline or end of stream,
returning them in a list. The newline is not included. Returns two
values - a list of chunks and either NIL or T to indicate whether a
terminating newline was missing. When the stream underlying the buffer
is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 0)))
  (with-slots ((s stream) buffer offset num-bytes nl-code)
      stream
    (declare (type byte-buffer buffer)
             (type byte-buffer-index offset num-bytes))
    (labels ((fill-buffer ()
               (setf offset 0
                     num-bytes (read-sequence buffer s)))
             (more-chunks ()
               (let ((nl-position (position nl-code buffer
                                            :start offset :end num-bytes)))
                 (cond ((and nl-position(plusp (- nl-position offset)))
                        ;; There is a newline in the buffer, but not
                        ;; at the zeroth position. Make a chunk, copy
                        ;; up to the newline into it, move the offset
                        ;; beyond the newline.
                        (let ((chunk (make-array (- nl-position offset)
                                                 :element-type 'octet
                                                 :initial-element 0)))
                          (replace chunk buffer :start2 offset
                                   :end2 nl-position)
                          (setf offset (1+ nl-position))
                          (values (list chunk) nil)))
                       ((and nl-position (zerop (- nl-position offset)))
                        ;; There is a newline in the buffer at the
                        ;; zeroth position. Make an empty chunk (for
                        ;; sake of consistency), move the offset
                        ;; beyond the newline.
                        (let ((chunk (make-array 0 :element-type 'octet)))
                          (setf offset (1+ nl-position))
                          (values (list chunk) nil)))
                       ((zerop num-bytes)
                        ;; The buffer is empty
                        (values nil t))
                       (t
                        ;; There is no newline in the buffer. Make a
                        ;; chunk to contain the rest of the buffered
                        ;; bytes and copy into it, fill the buffer,
                        ;; recursively call to search for the next
                        ;; newline.
                        (let ((chunk (make-array (- num-bytes offset)
                                                 :element-type 'octet
                                                 :initial-element 0))
                              (chunks nil)
                              (missing-nl-p t))
                          (replace chunk buffer :start2 offset :end2 num-bytes)
                          (fill-buffer)
                          (multiple-value-setq (chunks missing-nl-p)
                            (more-chunks))
                          (values (cons chunk chunks) missing-nl-p)))))))
      (when (buffer-empty-p offset num-bytes)
        (fill-buffer))
      (more-chunks))))

(defun buffer-empty-p (offset num-bytes)
  "Returns T if the internal byte buffer of BINARY-LINE-INPUT-STREAM
is empty."
  (= offset num-bytes))

(defun read-elt-from-line-stack (stream)
  (with-slots (line-stack)
      stream
    (let ((line (pop line-stack)))
      (cond ((= 1 (length line))
             (aref line 0))
            (t
             (let ((copy (make-array (1- (length line))
                                     :element-type (array-element-type line))))
               (push (replace copy line :start2 1) line-stack)
               (aref line 0)))))))

(defun stream-read-sequence-with-line-stack (stream sequence start end)
  (with-slots ((s stream) line-stack)
      stream
    (cond ((null line-stack)
           (read-sequence sequence s :start start :end end))
          (t
           (let ((seq-index 0)
                 (line-part nil))
             (loop
                while (and line-stack (< seq-index end))
                do (loop
                      with line = (pop line-stack)
                      for i from seq-index below end
                      for j from 0 below (length line)
                      do (setf (elt sequence i) (aref line j))
                      finally (progn
                                (incf seq-index j)
                                (when (< j (length line))
                                  (setf line-part (subseq line j)))))
                finally (when line-part
                          (push line-part line-stack)))
             (read-sequence sequence s :start seq-index :end end))))))

(defun concatenate-chunks (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type 'octet :initial-element 0)))
    (loop
       for chunk of-type simple-octet-vector in chunks
       for chunk-length = (length chunk)
       with offset = 0
       do (unless (zerop chunk-length)
            (replace line chunk :start1 offset)
            (incf offset chunk-length)))
    line))

(defmacro with-li-stream ((stream filespec &rest options)
                          &body body)
  "Uses WITH-OPEN-FILE to create a file stream to a file named by
FILESPEC. The file stream is wrapped in a {defclass line-input-stream}
and returned."
  (with-gensyms (fs)
    `(with-open-file (,fs ,filespec ,@options)
      (let ((,stream (make-line-input-stream ,fs)))
        ,@body))))

(defmacro with-ascii-li-stream ((stream filespec)
                                &body body)
   "Uses WITH-LI-STREAM to create a file stream with element-type
BASE-CHAR and external format ASCII to a file named by FILESPEC. The
file stream is wrapped in a {defclass line-input-stream} and
returned."
  `(with-li-stream (,stream ,filespec :direction :input
                    :element-type 'base-char
                    :external-format :ascii)
    ,@body))
