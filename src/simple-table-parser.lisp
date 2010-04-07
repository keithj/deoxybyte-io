;;;
;;; Copyright (C) 2008-2010 Keith James. All rights reserved.
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

(defparameter *empty-field* (make-array 0 :element-type 'base-char)
  "The default empty field string.")

(defmacro define-line-parser (parser-name delimiter fields
                              &optional constraints)
"Defines a line parser function that splits lines and then parses and
validates fields according to the FIELDS contraints and validates
combinations of fields according to CONSTRAINTS.

Arguments:

- parser-name (symbol): a symbol naming the new function.
- delimiter (character): a character used to split the lines being
parsed.
- fields (list list): a list of field definition lists. The form of a
definition list is

;;; (symbolic-field-name &keys type ignore parser validator null-str)

which indicates the symbol to which the parsed field value will be
stored in the alist returned by the function

- parser-name (symbol): the expected type of the field value ( :string,
:integer or :float, defaulting to :string), an optional boolean flag
indicating that the field should be ignored, an optional parser
function (defaults to a pass-through string parser), an optional
validator function that returns T when the parsed field is valid and
an optional string to indicate an null field (defaults to
{defparameter *empty-field*} ).

Optional:

- constraints (list list): a list of field constraint definition
lists. The form of a constraint definition list is

;;; (symbolic-constraint-name (symbolic-field-name list) validator)

The symbolic-field-name list should contain names of one or more
fields and the validator must then be a function which accepts the
parsed values of those fields in the same order and returns T when
those fields have acceptable values."
  (let ((field-count (length fields))
        (field-names `(list ,@(mapcar #'car fields)))
        (field-args (mapcar #'collect-parser-args fields))
        (constraint-names `(list ,@(mapcar #'car constraints)))
        (constraint-args (mapcar #'collect-constraint-args constraints)))
    `(progn
      (defun ,parser-name (line)
        (declare (optimize (speed 3)))
        (declare (type simple-string line))
        (multiple-value-bind (field-starts field-ends)
            (string-split-indices line ,delimiter)
          (declare (type list field-starts))
          (check-record (= ,field-count (length field-starts)) line
                          "invalid line: ~d fields instead of ~d"
                          (length field-starts) ,field-count)
          (let* ((fargs (list ,@field-args))
                 (cargs (list ,@constraint-args))
                 (parsed-fields
                  (loop
                     for name in ,field-names
                     for arg-list in fargs
                     for start in field-starts
                     for end in field-ends
                     unless (key-value :ignore arg-list)
                     collect (cons name
                                   (apply #'parse-field
                                          name line
                                          start end arg-list)))))
            (let* ((record-constraints
                    (loop
                       for name in ,constraint-names
                       for form in cargs
                       collect (apply #'validate-record
                                      name parsed-fields form)))
                   (failed-constraints
                    (loop
                       for (result . nil) in record-constraints
                       when (null (cdr result))
                       collect (car result))))
              (when failed-constraints
                (error 'record-validation-error
                       :record line
                       :text (format nil "constraints ~a failed"
                                     failed-constraints))))
            parsed-fields))))))

(declaim (inline default-validator))
(defun default-validator (value)
  "The default validator always returns T."
  (declare (ignore value))
  t)

(defun default-string-parser (field-name str &key (start 0) end
                              (null-str *empty-field*))
  "Returns a string subsequence from simple-string record STR between
START and END, or NIL if STR is STRING= to NULL-STR between START and
END."
  (declare (optimize (speed 3)))
  (declare (type simple-string str null-str))
  (let ((end (or end (length str))))
    (if (string= null-str str :start2 start :end2 end)
        nil
      (handler-case
          (subseq str start end)
        (parse-error (condition)
          (error 'malformed-field-error
                 :field field-name
                 :text (format nil "invalid field ~a: ~a"
                               (subseq str start end) condition)))))))

(defun default-integer-parser (field-name str &key (start 0) end
                               (null-str *empty-field*))
  "Returns an integer parsed from simple-base-string record STR
between START and END, or NIL if STR is STRING= to NULL-STR between
START and END."
  (declare (optimize (speed 3)))
  (declare (type simple-string str null-str))
  (let ((end (or end (length str))))
    (if (string= null-str str :start2 start :end2 end)
        nil
      (handler-case
          (parse-integer str :start start :end end)
        (parse-error (condition)
          (error 'malformed-field-error
                 :field field-name
                 :text (format nil "invalid field ~a: ~a"
                               (subseq str start end) condition)))))))

(defun default-float-parser (field-name str &key (start 0) end
                             (null-str *empty-field*))
  "Returns a float parsed from simple-base-string record STR between
START and END, or NIL if STR is STRING= to NULL-STR between START and
END."
  (declare (optimize (speed 3)))
  (declare (type simple-string str null-str))
  (let ((end (or end (length str))))
    (if (string= null-str str :start2 start :end2 end)
        nil
      (handler-case
          (parse-float str :start start :end end)
        (parse-error (condition)
          (error 'malformed-field-error
                 :field field-name
                 :text (format nil "invalid field ~a: ~a"
                               (subseq str start end) condition)))))))
  
(defun parse-field (field-name line start end null-str parser
                    &optional (validator #'default-validator))
  "Returns a value parsed from LINE between START and END using PARSER
and VALIDATOR."
  (declare (optimize (speed 3)))
  (declare (type function parser validator)
           (type simple-string line))
  (let ((parsed-value (funcall parser field-name line
                               :start start :end end
                               :null-str (or null-str *empty-field*))))
    (if (funcall validator parsed-value)
        parsed-value
      (error 'field-validation-error
             :field field-name
             :text (format nil "~s with parsed value ~a"
                           (subseq line start end) parsed-value)))))

(defun validate-record (name fields validator &rest field-names)
  "Returns a pair of constraint NAME and either T or NIL, indicating
the result of applying VALIDATOR to values from the alist of parsed
FIELDS named by FIELD-NAMES."
  (let ((field-values (mapcar (lambda (key)
                                (assocdr key fields)) field-names)))
    (cons name (handler-case
                   (apply validator field-values)
                 (error (condition)
                   (error 'record-validation-error
                          :record name
                          :text (format nil (txt "validator raised an error"
                                                 "on field values ~a: ~a")
                                        field-values condition)))))))

(defun collect-parser-args (field)
  "Returns an argument list form for FIELD to be used by PARSE-FIELD
which has suitable parsers and validators set up for the standard
field types: :string , :integer and :float ."
  (destructuring-bind (field-name &key ignore (type :string)
                                  null-str parser validator)
      field
    (declare (ignore field-name))
    (if ignore
        '(list :ignore t)
      (let ((field-parser
             (or parser (ecase type
                          (:string '#'default-string-parser)
                          (:integer '#'default-integer-parser)
                          (:float '#'default-float-parser))))
            (field-validator
             (or validator (ecase type
                             (:string '#'default-validator)
                             (:integer '#'default-validator)
                             (:float '#'default-validator)))))
        `(list ,null-str ,field-parser ,field-validator)))))

(defun collect-constraint-args (form)
  "Returns an argument list form to be used by CROSS-VALIDATE by
quoting the field-names in FORM and re-ordering the elements."
  (destructuring-bind (constraint-name field-names validator)
      form
    (declare (ignore constraint-name))
    `(list ,validator ,@(mapcar (lambda (n)
                                  `(quote ,n))
                                field-names))))
