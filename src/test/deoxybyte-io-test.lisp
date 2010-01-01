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

(in-package :uk.co.deoxybyte-io-test)

(deftestsuite deoxybyte-io-tests ()
  ())

(defun as-bytes (str)
  (make-array (length str) :element-type 'octet
              :initial-contents (loop for c across str
                                     collect (char-code c))))

(defun as-chars (bytes)
  (make-array (length bytes) :element-type 'base-char
              :initial-contents (loop for b across bytes
                                     collect (code-char b))))

;;; Gray-streams methods to be tested

;; input streams
;; sb-gray:stream-clear-input stream
;; sb-gray:stream-read-sequence stream seq &optional start binary

;; end streams
;; sb-gray:stream-read-byte stream
;; sb-gray:stream-write-byte stream integer

;; character input streams
;; sb-gray:stream-peek-char stream
;; sb-gray:stream-read-char-no-hang stream
;; sb-gray:stream-read-char stream
;; sb-gray:stream-read-line stream
;; sb-gray:stream-listen stream
;; sb-gray:stream-unread-char stream character

(addtest (deoxybyte-io-tests) gray-common/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream)))
      (ensure (subtypep (stream-element-type s) 'character))
      (ensure (zerop (stream-file-position s)))
      (ensure (open-stream-p s))
      (ensure (close s))
      (ensure (not (open-stream-p s)))
      (ensure-error
       (stream-read-line s)))))

(addtest (deoxybyte-io-tests) gray-common/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream)))
      (ensure (subtypep (stream-element-type s) 'octet))
      (ensure (zerop (stream-file-position s)))
      (ensure (open-stream-p s))
      (ensure (close s))
      (ensure (not (open-stream-p s)))
      (ensure-error
       (stream-read-line s)))))

(addtest (deoxybyte-io-tests) gray-input/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type 'character)))
      ;; stream-clear-input should empty the line buffer
      (push-line s "aaaa")
      (ensure (deoxybyte-io::line-stack-of s))
      (ensure-null (stream-clear-input s))
      (ensure (not (deoxybyte-io::line-stack-of s)))
      (ensure (= 10 (stream-read-sequence s b 0 (length b))))
      (ensure (string= "abcdefghij" b)))))

(addtest (deoxybyte-io-tests) gray-input/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (b (make-array 10 :element-type 'octet)))
      (ensure-null (stream-clear-input s))
      (ensure (= 10 (stream-read-sequence s b 0 (length b))))
      (ensure (equalp (as-bytes "abcdefghij") b)))))

(addtest (deoxybyte-io-tests) gray-binary/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (loop for byte across line
           do (ensure (= byte (stream-read-byte s)))))))

(addtest (deoxybyte-io-tests) gray-char/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (ensure (char= #\a (stream-read-char s)))
      (ensure-null (stream-unread-char s #\a))
      (loop for char across line
         do (ensure (char= char (stream-read-char s)))))))

(addtest (deoxybyte-io-tests) stream-read-line/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line2 line))
        (ensure (not missing-newline-p)))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (eql :eof line2))
        (ensure missing-newline-p)))))

(addtest (deoxybyte-io-tests) stream-read-line/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line bytes))
        (ensure (not missing-newline-p)))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure-same :eof bytes)
        (ensure missing-newline-p)))))

(addtest (deoxybyte-io-tests) push-line/1
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (line "abcdefghijklmnopqrstuvwxyz"))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line line2))
        (ensure (not missing-newline-p))
        (push-line s line2)
        (ensure (equalp line (stream-read-line s)))))))

(addtest (deoxybyte-io-tests) push-line/2
  (with-open-file (stream (merge-pathnames "data/test1.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (line (as-bytes "abcdefghijklmnopqrstuvwxyz")))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp line bytes))
        (ensure (not missing-newline-p))
        (push-line s bytes)
        (ensure (equalp line (stream-read-line s)))))))

(addtest (deoxybyte-io-tests) missing-newline-p/1
  (with-open-file (stream (merge-pathnames "data/test2.txt")
                   :direction :input
                   :element-type 'base-char
                   :external-format :ascii)
    (let ((s (make-line-input-stream stream))
          (lines '("1234567890"
                   "0987654321"
                   "abcdefghij"
                   "klmnopqrst")))
      (dolist (line (butlast lines))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (equalp line line2))
          (ensure (not missing-newline-p))))
      (multiple-value-bind (line2 missing-newline-p)
          (stream-read-line s)
        (ensure (equalp (car (last lines)) line2))
        (ensure missing-newline-p)))))

(addtest (deoxybyte-io-tests) missing-newline-p/2
  (with-open-file (stream (merge-pathnames "data/test2.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (lines (mapcar #'as-bytes '("1234567890"
                                      "0987654321"
                                      "abcdefghij"
                                      "klmnopqrst"))))
      (dolist (line (butlast lines))
        (multiple-value-bind (bytes missing-newline-p)
            (stream-read-line s)
          (ensure (equalp line bytes))
          (ensure (not missing-newline-p))))
      (multiple-value-bind (bytes missing-newline-p)
          (stream-read-line s)
        (ensure (equalp (car (last lines)) bytes))
        (ensure missing-newline-p)))))

(addtest (deoxybyte-io-tests) find-line/1
  (with-open-file (stream (merge-pathnames "data/test3.txt")
                   :direction :input
                   :element-type 'octet)
    (let ((s (make-line-input-stream stream))
          (lines (mapcar #'as-bytes '("abc"
                                      "def"
                                      "ghi"
                                      "jkl"
                                      "mno"
                                      "pqr"
                                      "stu"
                                      "vwx"
                                      "yz"))))
      ;; Success at line 1 of max 1 -> "abc"
      (multiple-value-bind (line found line-count)
          (find-line s (lambda (a)
                         (= (aref a 0) (char-code #\a))) 1)
        (ensure (equalp (nth 0 lines) line))
        (ensure found)
        (ensure (= 1 line-count)))
      ;; Fail at line 1 of max 1 -> "def"
      (multiple-value-bind (line found line-count)
          (find-line s (lambda (a)
                         (= (aref a 0) (char-code #\Z))) 1)
        (ensure (equalp (nth 1 lines) line))
        (ensure (not found))
        (ensure (= 1 line-count)))
      ;; Success at line 3 of max 4
      (multiple-value-bind (line found line-count)
          (find-line s (lambda (a)
                         (= (aref a 0) (char-code #\m))) 4)
        (ensure (equalp (nth 4 lines) line))
        (ensure found)
        (ensure (= 3 line-count)))
      ;; Fall through to eof
      (multiple-value-bind (line found line-count)
          (find-line s (lambda (a)
                         (declare (ignore a))
                         nil))
        (ensure-same :eof line)
        (ensure (not found))
        (ensure (= 5 line-count))))))

(addtest (deoxybyte-io-tests) pathstring/1
  (ensure (string= "/foo/bar/baz.txt"
                   (pathstring (pathname "/foo/bar/baz.txt"))))
  ;; These tests ensure that we can unescape all the dots under CCL
  (ensure (string= "/foo/bar.baz.txt"
                   (pathstring (pathname "/foo/bar.baz.txt"))))
  (ensure (string= "/foo/bar..baz.txt"
                   (pathstring (pathname "/foo/bar..baz.txt")))))

(addtest (deoxybyte-io-tests) make-tmp-pathname/1
  ;; Test defaults
  (ensure (pathnamep (make-tmp-pathname)))
  (ensure (string= "/tmp/" (directory-namestring (make-tmp-pathname))))
  (ensure (integerp (parse-integer (pathname-name (make-tmp-pathname)))))
  (ensure-null (pathname-type (make-tmp-pathname)))
  ;; Test optional arguments
  (ensure (string= "/" (directory-namestring (make-tmp-pathname
                                              :tmpdir "/"))))
  (ensure (string= "tmp" (pathname-type (make-tmp-pathname :type "tmp"))))
  (ensure (string= "foo" (pathname-name (make-tmp-pathname :tmpdir "/tmp"
                                                           :basename "foo"))
                   :end2 3))
  (ensure (string= "bar" (pathname-type
                          (make-tmp-pathname :tmpdir "/tmp"
                                             :basename "foo"
                                             :type "bar"))))
  ;; Test error condition
  (let ((bad-dir "/this-directory-does-not-exist/"))
    (ensure (and (not (fad:directory-exists-p bad-dir))
                 (ensure-condition invalid-argument-error
                                   (make-tmp-pathname :tmpdir bad-dir))))))

(addtest (deoxybyte-io-tests) make-tmp-directory/1
  ;; Test defaults
  (let ((tmpdir (make-tmp-directory)))
    (unwind-protect
         (progn
           (ensure (pathnamep tmpdir))
           (ensure (equalp '(:absolute "tmp")
                           (subseq (pathname-directory tmpdir) 0 2)))
           (ensure (parse-integer (third (pathname-directory tmpdir))))
           (ensure-null (pathname-type tmpdir))
           (ensure (fad:directory-exists-p tmpdir))
           (ensure (fad:directory-pathname-p tmpdir)))
      (fad:delete-directory-and-files tmpdir)))
  ;; Test optional arguments. There need to be more tests in here to
  ;; check :if-exists arguments
  (ensure-directories-exist "/tmp/bar/")
  (let ((tmpdir (make-tmp-directory :tmpdir "/tmp/bar" :basename "foo")))
    (unwind-protect
         (progn
           (ensure (string= "foo" (fourth (pathname-directory tmpdir)) :end2 3))
           (ensure (string= "bar" (third (pathname-directory tmpdir)) :end2 3)))
      (fad:delete-directory-and-files tmpdir)))
  ;; Test error conditions
  (let ((bad-dir "/this-directory-does-not-exist/"))
    (ensure (and (not (fad:directory-exists-p bad-dir))
                 (ensure-condition invalid-argument-error
                   (make-tmp-directory :tmpdir bad-dir))))))

(addtest (deoxybyte-io-tests) ensure-file-exists/1
  (let ((test-file (merge-pathnames "data/touch_test.txt")))
    (ensure (not (probe-file test-file)))
    (ensure-file-exists test-file)
    (ensure (probe-file test-file))
    (delete-file test-file)))
