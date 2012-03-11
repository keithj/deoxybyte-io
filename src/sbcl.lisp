;;;
;;; Copyright (c) 2012 Keith James. All rights reserved.
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

;;; command-line-interface
(defun get-system-argv ()
  (rest sb-ext:*posix-argv*))

(defun print-backtrace (stream &optional (depth 20))
  (sb-debug:backtrace depth stream))

(defun quit-lisp (&key (status 0))
  (sb-ext:quit :unix-status status))

;;; environment
(defun environment-variable (variable-name)
  "Returns the string value of VARIABLE-NAME, or NIL."
  (sb-posix:getenv (etypecase variable-name
                     (string variable-name)
                     (symbol (symbol-name variable-name)))))

(defun (setf environment-variable) (value variable-name)
  "Sets the value of VARIABLE-NAME to VALUE, which maye be a string or
a symbol."
  (declare (ignore value variable-name))
  (error "Not implemented on ~a" (lisp-implementation-type)))

;;; line-stream
(defmethod stream-read-sequence ((stream character-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-read-sequence stream sequence start end)))

(defmethod stream-read-sequence ((stream octet-line-input-stream)
                                 sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-read-sequence stream sequence start end)))

(defmethod stream-write-sequence ((stream line-output-stream)
                                  sequence &optional (start 0) end)
  (let ((end (or end (length sequence))))
    (%stream-write-sequence stream sequence start end)))
