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
  (error "GET-SYSTEM-ARGV not supported on this Lisp implementation."))

(defun print-backtrace (stream)
  (error "PRINT-BACKTRACE not supported on this Lisp implementation."))

(defun quit-lisp ()
  (error "QUIT-LISP not supported on this Lisp implementation."))

;;; environment

(defun environment-variable (variable-name)
  "Returns the string value of VARIABLE-NAME, or NIL."
  (declare (ignore variable-name))
  (error "ENVIRONMENT-VARIABLE not supported on this Lisp implementation."))

(defun (setf environment-variable) (value variable-name)
  "Sets the value of VARIABLE-NAME to VALUE, which maye be a string or
a symbol."
  (declare (ignore value variable-name))
  (error "ENVIRONMENT-VARIABLE not supported on this Lisp implementation."))
