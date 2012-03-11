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
  (rest ccl:*command-line-argument-list*))

(defun print-backtrace (stream &optional (depth 20))
  (let ((*debug-io* stream))
    (ccl:print-call-history :count depth :detailed-p nil)))

(defun quit-lisp (&key (status 0))
  (ccl:quit status))

;;; environment
(defun environment-variable (variable-name)
  "Returns the string value of VARIABLE-NAME, or NIL."
  (ccl:getenv variable-name))

(defun (setf environment-variable) (value variable-name)
  "Sets the value of VARIABLE-NAME to VALUE, which maye be a string or
a symbol."
  (ccl:setenv variable-name value))
