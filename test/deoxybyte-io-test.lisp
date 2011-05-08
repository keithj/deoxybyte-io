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

(in-package :uk.co.deoxybyte-io-test)

(deftestsuite deoxybyte-io-tests ()
  ())

(defun test-data-file (filespec)
  (asdf:system-relative-pathname 'deoxybyte-io filespec))

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
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test1.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream)))
        (ensure (subtypep (stream-element-type s) 'string))
        (ensure (zerop (stream-file-position s)))
        (ensure (open-stream-p s))
        (ensure (stream-close s))
        (ensure (not (open-stream-p s)))
        (ensure-error
          (stream-read-line s))))))

(addtest (deoxybyte-io-tests) gray-input/1
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test3.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream))
            (b (make-array 9 :element-type t)))
        ;; stream-clear-input should empty the line buffer
        (push-line s "aaaa")
        (ensure (slot-value s 'deoxybyte-io::line-stack))
        (ensure-null (stream-clear-input s))
        (ensure (not (slot-value s 'deoxybyte-io::line-stack)))
        (ensure (= 1 (stream-read-sequence s b 0 1)))
        (ensure (= 4 (stream-read-sequence s b 1 5)))
        (ensure (= 4 (stream-read-sequence s b 5)))
        (ensure (equalp #("abc"
                          "def"
                          "ghi"
                          "jkl"
                          "mno"
                          "pqr"
                          "stu"
                          "vwx"
                          "yz") b))))))

(addtest (deoxybyte-io-tests) stream-read-line/1
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test1.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (equal line line2))
          (ensure (not missing-newline-p)))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (eql :eof line2))
          (ensure missing-newline-p))))))

(addtest (deoxybyte-io-tests) push-line/1
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test1.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream))
            (line "abcdefghijklmnopqrstuvwxyz"))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (equal line line2))
          (ensure (not missing-newline-p))
          (push-line s line2)
          (ensure (equal line (stream-read-line s))))))))

(addtest (deoxybyte-io-tests) missing-newline-p/1
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test2.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream))
            (lines '("1234567890"
                     "0987654321"
                     "abcdefghij"
                     "klmnopqrst")))
        (dolist (line (butlast lines))
          (multiple-value-bind (line2 missing-newline-p)
              (stream-read-line s)
            (ensure (equal line line2))
            (ensure (not missing-newline-p))))
        (multiple-value-bind (line2 missing-newline-p)
            (stream-read-line s)
          (ensure (equal (car (last lines)) line2))
          (ensure missing-newline-p))))))

(addtest (deoxybyte-io-tests) find-line/1
  (dolist (elt-type '(base-char octet))
    (with-open-file (stream (test-data-file "data/test3.txt")
                            :direction :input :element-type elt-type)
      (let ((s (make-line-stream stream))
            (lines '("abc"
                     "def"
                     "ghi"
                     "jkl"
                     "mno"
                     "pqr"
                     "stu"
                     "vwx"
                     "yz")))
        ;; Success at line 1 of max 1 -> "abc"
        (multiple-value-bind (line found line-count)
            (find-line s (lambda (a)
                           (char= (aref a 0) #\a)) 1)
          (ensure (equal (nth 0 lines) line))
          (ensure found)
          (ensure (= 1 line-count)))
        ;; Fail at line 1 of max 1 -> "def"
        (multiple-value-bind (line found line-count)
            (find-line s (lambda (a)
                           (char= (aref a 0) #\Z)) 1)
          (ensure (equal (nth 1 lines) line))
          (ensure (not found))
          (ensure (= 1 line-count)))
        ;; Success at line 3 of max 4
        (multiple-value-bind (line found line-count)
            (find-line s (lambda (a)
                           (char= (aref a 0) #\m)) 4)
          (ensure (equal (nth 4 lines) line))
          (ensure found)
          (ensure (= 3 line-count)))
        ;; Fall through to eof
        (multiple-value-bind (line found line-count)
            (find-line s (lambda (a)
                           (declare (ignore a))
                           nil))
          (ensure-same :eof line)
          (ensure (not found))
          (ensure (= 5 line-count)))))))

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
    (ensure (not (fad:directory-exists-p bad-dir)))
    (ensure-condition invalid-argument-error
      (make-tmp-directory :tmpdir bad-dir))))

(addtest (deoxybyte-io-tests) with-tmp-directory/1
  (let ((pathname (with-tmp-directory (tmpdir)
                    (ensure (fad:directory-exists-p tmpdir))
                    tmpdir)))
    (ensure (not (fad:directory-exists-p pathname)))))

(addtest (deoxybyte-io-tests) with-tmp-directory/2
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'leave-tmp-directory))))
    (let ((pathname (with-tmp-directory (tmpdir)
                      (error "Error"))))
      (ensure (fad:directory-exists-p pathname))
      (fad:delete-directory-and-files pathname))))

(addtest (deoxybyte-io-tests) with-tmp-directory/3
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'delete-tmp-directory))))
    (let ((pathname (with-tmp-directory (tmpdir)
                      (error "Error"))))
      (ensure (not (fad:directory-exists-p pathname))))))

(addtest (deoxybyte-io-tests) with-tmp-pathname/1
  (with-tmp-pathname (tmpfile)
    (ensure (pathnamep tmpfile))))

(addtest (deoxybyte-io-tests) with-tmp-pathname/2
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (invoke-restart 'leave-tmp-pathname))))
    (let ((pathname (with-tmp-pathname (tmpfile)
                      (with-open-file (stream tmpfile :direction :output)
                        stream)
                      (ensure (fad:file-exists-p tmpfile))
                      (error "Error"))))
      (ensure (fad:file-exists-p pathname))
      (delete-file pathname))))

(addtest (deoxybyte-io-tests) with-tmp-pathname/3
  (handler-bind ((error (lambda (c)
                          (declare (ignore c))
                          (when (find-restart 'delete-tmp-pathname)
                            (invoke-restart 'delete-tmp-pathname)))))
    (let ((pathname (with-tmp-pathname (tmpfile)
                      (with-open-file (stream tmpfile :direction :output)
                        stream)
                      (ensure (fad:file-exists-p tmpfile))
                      (error "Error"))))
      (ensure (not (fad:file-exists-p pathname))))))

(addtest (deoxybyte-io-tests) ensure-file-exists/1
  (let ((test-file (test-data-file "data/touch_test.txt")))
    (ensure (not (probe-file test-file)))
    (ensure-file-exists test-file)
    (ensure (probe-file test-file))
    (delete-file test-file)))
