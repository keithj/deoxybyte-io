
(in-package :cl-io-utilities)

(defconstant +byte-buffer-size+ 8192)

(deftype byte-buffer ()
  `(simple-array (unsigned-byte 8) (,+byte-buffer-size+)))

(deftype byte-buffer-subscript ()
  `(integer 0 ,+byte-buffer-size+))

(defclass line-buffer ()
  ((stream :initarg :stream
           :initform nil
           :reader stream-of
           :documentation "The underlying stream from which lines of
bytes are read, via the buffer.")
   (buffer :initarg :buffer
           :initform nil
           :reader buffer-of
           :documentation "The buffer from which lines are read.")
   (nl-code :initarg :nl-code
            :reader nl-code-of
            :documentation "The newline character code.")
   (num-bytes :initform 0
              :accessor num-bytes-of
              :documentation "The number of bytes that were read into
the buffer from the stream.")
   (offset :initform 0
           :accessor offset-of
           :documentation "The offset in the byte buffer from which
the next byte is to be read.")
   (pushback :initform nil
             :accessor pushback-of
             :documentation "A list of lines that have been pushed
back into the reader to be read again."))
  (:documentation "Allows buffered reading of lines of bytes from a
stream."))

(defmethod initialize-instance :after ((obj line-buffer) &key)
  (fill-buffer obj))

(defgeneric pull-line (line-buffer)
  (:documentation "Reads up to the next newline from LINE-BUFFER, or
end of stream, returning an array. The newline is not
included. Returns two values - the array and either T or NIL to
indicate whether a terminating newline was missing."))

(defgeneric push-line (line-buffer line)
  (:documentation "Pushes LINE back into LINE-BUFFER."))

(defgeneric more-lines-p (line-buffer)
  (:documentation "Returns T if LINE-BUFFER contains unread data."))

(defgeneric find-line (line-buffer predicate &optional max-lines)
  (:documentation "Iterates through lines pulled from LINE-BUFFER
until a line matching PREDICATE is found or until a number of lines
equal to MAX-LINES have been examined."))

(defmethod pull-line ((obj line-buffer))
  (if (null (pushback-of obj))
      (multiple-value-bind (chunks has-newline-p)
          (read-chunks obj)
        (cond ((null chunks)
               (values nil nil))
              ((zerop (length (first chunks)))
               (first chunks))
              ((= 1 (length chunks))
               (values (first chunks) has-newline-p))
              (t
               (values (concatenate-chunks chunks) has-newline-p))))
    (pop (pushback-of obj))))

(defmethod push-line ((obj line-buffer) (line vector))
  (push line (pushback-of obj)))

(defmethod more-lines-p ((obj line-buffer))
  (or (pushback-of obj)
      (not (zerop (num-bytes-of obj)))))

(defmethod find-line ((obj line-buffer) predicate &optional (max-lines 1))
  (flet ((match-p (p x)
            (and x (funcall p x))))
    (do* ((line (pull-line obj) (pull-line obj))      
          (matching-line-p (match-p predicate line) (match-p predicate line))
          (line-count 1 (1+ line-count)))
         ((or (null line)
              matching-line-p
              (= line-count max-lines))
          (values line matching-line-p line-count)))))

(defun make-line-buffer (stream &optional (nl-char #\Newline))
  (make-instance 'line-buffer :stream stream
                 :nl-code (char-code nl-char)
                 :buffer (make-array +byte-buffer-size+
                                     :element-type '(unsigned-byte 8)
                                     :initial-element 0)))

(defmethod is-empty-p ((obj line-buffer))
  "Returns TRUE if the buffer OBJ is empty."
  (= (offset-of obj) (num-bytes-of obj)))

(defmethod fill-buffer ((obj line-buffer))
  "Fills the buffer OBJ from the stream, updating its record of the
number of bytes read."
  (setf (offset-of obj) 0
        (num-bytes-of obj) (read-sequence (buffer-of obj) (stream-of obj))))

(defmethod read-chunks ((obj line-buffer))
  "Reads chunks of bytes from buffer OBJ, up to the next newline or
end of stream, returning them in a list. The newline is not
included. Returns two values - a list of chunks and either NIL or T to
indicate whether a terminating newline was missing. When the stream
underlying the buffer is exhausted the list of chunks will be empty."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((offset (offset-of obj))
        (num-bytes (num-bytes-of obj))
        (buffer (buffer-of obj)))
    (declare (type byte-buffer buffer)
             (type byte-buffer-subscript offset num-bytes))
    (let ((nl-position (position (nl-code-of obj) buffer
                                 :start offset :end num-bytes)))
      (cond ((and nl-position
                  (plusp (- nl-position offset)))
           ;; There is a newline in the buffer, but not at the zeroth
           ;; position. Make a chunk and copy up to the newline into
           ;; it. Move the offset beyond the newline. Fill the buffer
           ;; if necessary.
             (let ((chunk (make-array (- nl-position offset)
                                      :element-type '(unsigned-byte 8))))
               (copy-array buffer offset (1- nl-position)
                           chunk 0)
               (setf (offset-of obj) (1+ nl-position))
               (when (is-empty-p obj)
                 (fill-buffer obj))
               (values (list chunk) nil)))
            ((and nl-position
                  (zerop (- nl-position offset)))
             ;; There is a newline in the buffer at the zeroth
             ;; position. Make an empty chunk (for sake of
             ;; consistency). Move the offset beyond the newline. Fill
             ;; the buffer if necessary.
             (let ((chunk (make-array 0 :element-type '(unsigned-byte 8))))
               (setf (offset-of obj) (1+ nl-position))
               (when (is-empty-p obj)
                 (fill-buffer obj))
               (values (list chunk) nil)))
            ((zerop num-bytes)
             ;; The buffer is empty
             (values nil t))
            (t
             ;; There is no newline in the buffer. Make a chunk to
             ;; contain the rest of the buffered bytes and copy into
             ;; it. Fill the buffer. Recursively call read chunks to
             ;; search for the next newline.
             (let ((chunk (make-array (- num-bytes offset)
                                      :element-type '(unsigned-byte 8)))
                   (chunks nil)
                   (missing-nl t))
               (copy-array buffer offset (1- num-bytes)
                           chunk 0)
               (fill-buffer obj)
               (multiple-value-setq (chunks missing-nl)
                 (read-chunks obj))
               (values (cons chunk chunks) missing-nl)))))))

(defun concatenate-chunks (chunks)
  "Concatenates the list of byte arrays CHUNKS by copying their
contents into a new fixed length array, which is returned."
  (let ((line (make-array (reduce #'+ chunks :key #'length)
                          :element-type '(unsigned-byte 8))))
    (loop for chunk of-type (simple-array (unsigned-byte 8)) in chunks
          for chunk-length = (length chunk)
          with offset = 0
          do (unless (zerop chunk-length)
               (copy-array chunk 0 (1- chunk-length)
                           line offset)
               (incf offset chunk-length)))
    line))
