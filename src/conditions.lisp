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

;;; General IO error conditions

(define-condition io-error (error)
  ()
  (:documentation "The parent type of all IO error conditions."))

(define-condition io-warning (warning)
  ()
  (:documentation "The parent type of all IO warning conditions."))

;;; Command line interface conditions
(define-condition cli-error (error)
  ((cli :initarg :cli
        :initform nil
        :reader cli-of
        :documentation "The CLI object relevant to the error."))
  (:documentation "The parent type of all CLI error conditions."))

(define-condition cli-warning (warning)
  ((cli :initarg :cli
        :initform nil
        :reader cli-of
        :documentation "The CLI object relevant to the warning."))
  (:documentation "The parent type of all CLI warning conditions."))

(define-condition cli-option-error (cli-error)
  ((name :initarg :name
         :reader name-of
         :documentation "The option name."))
  (:documentation "The parent type of all CLI option error conditions."))

(define-condition cli-option-warning (cli-warning)
  ((name :initarg :name
         :reader name-of
         :documentation "The option name."))
  (:documentation "The parent type of all CLI option warning conditions."))

(define-condition unknown-command (cli-error)
  ((command :initarg :command
            :reader command-of
            :documentation "The unknown command."))
  (:report (lambda (condition stream)
             (format stream "Unknown command ~a." (command-of condition))))
  (:documentation "An error that is raised when the main command is
not recognised."))

(define-condition missing-required-option (cli-option-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Missing required option --~a."
                     (name-of condition))))
  (:documentation "An error that is raised when a required option is
missing."))

(define-condition missing-required-value (cli-option-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Missing required value for --~a."
                     (name-of condition))))
  (:documentation "An error that is raised when a required value
for an option is missing."))

(define-condition incompatible-value (cli-option-error)
  ((type :initarg :type
         :reader type-of
         :documentation "The expected type of option value.")
   (value :initarg :value
          :reader value-of
          :documentation "The invalid value."))
  (:report (lambda (condition stream)
             (format stream "Invalid value ~s supplied for option --~a (~a)."
                     (value-of condition) (name-of condition) 
                     (type-of condition))))
  (:documentation "An error that is raised when an option is supplied
with an invalid value."))

(define-condition unmatched-option (cli-option-warning)
  ()
  (:report (lambda (condition stream)
             (format stream "unmatched option --~a" (name-of condition)))))

(define-condition unknown-option (cli-option-warning)
  ()
  (:report (lambda (condition stream)
             (format stream "unknown option --~a" (name-of condition)))))


;;; Parse conditions
(define-condition general-parse-error (io-error formatted-condition)
  ()
  (:report (lambda (condition stream)
             (format stream "General parse error~@[: ~a~]"
                     (message-of condition))))
  (:documentation "The parent type of all parse error conditions."))

(define-condition malformed-file-error (general-parse-error)
  ((file :initform nil
         :initarg :file
         :reader file-of
         :documentation "The malformed file."))
  (:report (lambda (condition stream)
             (format stream "Malformed file~@[ ~a ~]~@[: ~a~]"
                     (file-of condition) (message-of condition))))
  (:documentation "An error that is raised when a file is malformed
for any reason."))

(define-condition malformed-record-error (general-parse-error)
  ((record :initform nil
           :initarg :record
           :reader record-of
           :documentation "The malformed record."))
  (:report (lambda (condition stream)
             (format stream "Malformed record~@[ ~a ~]~@[: ~a~]"
                     (record-of condition) (message-of condition))))
  (:documentation "An error that is raised when a record is malformed
for any reason."))

(define-condition record-validation-error (malformed-record-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid record~@[ ~a ~]~@[: ~a~]"
                     (record-of condition) (message-of condition))))
  (:documentation "An error that is raised when a record fails
validation of one or more of its parts."))

(define-condition malformed-field-error (malformed-record-error)
  ((field :initform nil
          :initarg :field
          :reader field-of
          :documentation "The malformed field."))
  (:report (lambda (condition stream)
             (format stream
                     "Malformed field~@[ ~a~]~@[ in record ~a~]~@[: ~a~]"
                     (field-of condition) (record-of condition)
                     (message-of condition))))
  (:documentation "An error that is raised when a field-based record
contains a malformed field within it."))

(define-condition field-validation-error (malformed-field-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid field~@[ ~a ~]~@[: ~a~]"
                     (field-of condition) (message-of condition))))
  (:documentation "An error that is raised when a record field fails
validation."))
