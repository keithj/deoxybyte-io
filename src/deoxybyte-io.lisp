;;;
;;; Copyright (C) 2010 Keith James. All rights reserved.
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

(defmacro check-record (test-form record &optional error-message
                        &rest message-arguments)
  "Checks RECORD. If TEST-FORM returns false a
{define-condition malformed-record-error} is raised. The default error
message may be refined with an additional ERROR-MESSAGE.

Arguments:

- test-form (form): A form to be evaluated. If the form returns NIL,
  an error is raised.
- record (form): A form that evaluates to a record.

Optional:

- error-message (string): An error message string.

Rest:

- message-arguments (forms): Forms that evaluate to arguments for the
  error message."
  `(progn
     (unless ,test-form
       (error 'malformed-record-error
              :record ,record
              :format-control ,error-message
              :format-arguments (list ,@message-arguments)))
     t))

(defmacro check-field (test-form record field &optional error-message
                       &rest message-arguments)
  "Checks FIELD of RECORD. If TEST-FORM returns false a
{define-condition malformed-field-error} is raised. The default error
message may be refined with an additional ERROR-MESSAGE.

Arguments:

- test-form (form): A form to be evaluated. If the form returns NIL,
  an error is raised.
- record (form): A form that evaluates to a record.
- field (form): A form that evaluates to a field in RECORD.

Optional:

- error-message (string): An error message string.

Rest:

- message-arguments (forms): Forms that evaluate to arguments for the
  error message."
  `(progn
     (unless ,test-form
       (error 'malformed-field-error
              :record ,record
              :field ,field
              :format-control ,error-message
              :format-arguments (list ,@message-arguments)))
     t))
