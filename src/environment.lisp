;;;
;;; Copyright (C) 2009-2010 Keith James. All rights reserved.
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

(defun environment-variable (variable-name)
  "Returns the string value of VARIABLE-NAME, or NIL."
  #+:sbcl(sb-posix:getenv (etypecase variable-name
                            (string variable-name)
                            (symbol (symbol-name variable-name))))
  #+:ccl (ccl:getenv variable-name)
  #+:lispworks (lispworks:environment-variable variable-name)
  #-(or :sbcl :ccl :lispworks)
  (progn
    (declare (ignore variable-name))
    (error "Not implemented on ~a" (lisp-implementation-type))))

(defun (setf environment-variable) (value variable-name)
  "Sets the value of VARIABLE-NAME to VALUE, which maye be a string or
a symbol."
  #+:ccl (ccl:setenv variable-name value)
  #+:lispworks (setf (lispworks:environment-variable variable-name) value)
  #-(or :ccl :lispworks)
  (declare (ignore value variable-name))
  #-(or :ccl :lispworks)
  (error "Not implemented on ~a" (lisp-implementation-type)))
