;;;
;;; Copyright (C) 2007-2008, Keith James. All rights reserved.
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

(in-package :cl-io-utilities)


;;; Parse conditions
(define-condition general-parse-error (error)
  ((text :initform nil
         :initarg :text
         :reader text-of
         :documentation "Error message text."))
  (:report (lambda (condition stream)
             (format stream "General parse error~@[: ~a~]"
                     (text-of condition)))))

(define-condition malformed-record-error (general-parse-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Malformed record error~@[: ~a~]"
                     (text-of condition)))))

(define-condition record-validation-error (malformed-record-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Record validation error~@[: ~a~]"
                     (text-of condition)))))

(define-condition malformed-field-error (malformed-record-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Malformed field error~@[: ~a~]"
                     (text-of condition)))))

(define-condition field-validation-error (malformed-field-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Field validation error~@[: ~a~]"
                     (text-of condition)))))
