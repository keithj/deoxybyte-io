;;;
;;; Copyright (c) 2008-2012 Keith James. All rights reserved.
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

(defparameter *list-separator-char* #\,
  "The separator character used in multi-value arguments.")

(defmacro with-argv ((argv) &body body)
   "Executes BODY with ARGV bound to system argv list."
  `(let ((,argv (get-system-argv)))
    ,@body))

(defmacro define-cli (name direct-superclasses option-specs &rest options)
  "Defines a new CLI class NAME.

Arguments: 

- name (symbol): The class name.
- direct-superclasses (list symbols): The direct superclasses.
- option-specs (list option-specs): The CLI option specifiers.

Each option-spec specifies a slot and its corresponding CLI option
instance.

  Arguments:

  - slot-name (symbol)
  - option-name (string)

  Rest:

  - cl-option instance initargs 

  Key:

  - documentation (string): Slot (and hence cli-option) documentation.

e.g.

;;; (a \"a\" :required-option t :value-type 'string
;;;      :documentation \"Required option A.\")

Rest:

- class-options as in defclass.

e.g.

;;; (define-cli example-cli (cli)
;;;   ((a \"a\" :required-option t :value-type 'string
;;;       :documentation \"Required option A.\")
;;;    (b \"b\" :required-option t :value-type 'integer
;;;       :documentation \"Required option D.\"))
;;;   (:documentation \"An example CLI class definition.\"))"
  `(defclass ,name ,direct-superclasses
     ,(mapcar (lambda (option-spec)
                (destructuring-bind (slot-name option-name
                                               &rest args &key documentation
                                               (class 'cli-option)
                                               &allow-other-keys)
                    option-spec
                  (list slot-name
                        :initform `(make-instance ',class :name ,option-name
                                                  ,@(remove-key-values
                                                     '(:documentation) args))
                        :documentation documentation)))
              option-specs)
     ,@options))

(defclass cli ()
  ()
  (:documentation "The base class of all command line
interfaces. Command line options are added as slots, the slot
documentation acting as the coomand line option documentation."))

(defclass cli-option ()
  ((name :initform (error "An option name is required")
         :initarg :name
         :reader name-of
         :documentation "The name string of the option.")
   (required-option :initform nil
                    :initarg :required-option
                    :reader required-option-p
                    :documentation "T if option is required on the
 command line, or NIL otherwise.")
   (value-type :initform 'string
               :initarg :value-type
               :reader value-type-of
               :documentation "The type of the value of the option. A
type of T indicates a boolean option. Valid types are the symbols
string, integer, character, float, string-list, integer-list,
character-list, float-list and T (indicating boolean, i.e. no
value).")
   (value-parser :initform nil
                 :reader value-parser-of
                 :documentation "A value parser function for option
that is capable of parsing a string to the correct type"))
  (:documentation "The base class of all command line options."))

(defmethod initialize-instance :after ((option cli-option) &key)
  (with-slots (required-option value-type value-parser)
      option
    (check-arguments (not (and required-option (eql t value-type)))
                     (required-option value-type)
                     "a boolean type is incompatible with a required option")
    (setf value-parser (ecase value-type
                         (string nil)
                         (character #'parse-character)
                         (integer #'parse-integer)
                         (float #'parse-float)
                         (string-list #'parse-string-list)
                         (character-list #'parse-character-list)
                         (integer-list #'parse-integer-list)
                         (float-list #'parse-float-list)
                         (t nil))))) ; boolean

(defmethod print-object ((cli cli) stream)
  (print-unreadable-object (cli stream :type t)
    (princ (options-of cli) stream)))

(defmethod print-object ((option cli-option) stream)
  (print-unreadable-object (option stream :type t)
    (with-slots (name required-option value-type)
        option
      (format stream "~a, ~:[required~;not required~], type: ~a"
              name required-option value-type))))

(defgeneric parse-command-line (cli arglist)
  (:documentation "Parses a system command line to create a mapping of
option keywords to Lisp objects. Where multiple values are to be
accepted for an argument e.g. 'integer-list , they must be
comma-separated on the command line e.g. 1,2,3,4.

Arguments:

- cli (object): A CLI instance.
- arglist (list string): A list of CLI string arguments.

Returns:

- alist mapping slots to parsed values
- list of unmatched argument strings
- list of unknown argument strings

e.g

;;; (define-cli example-cli (cli)
;;;   ((a \"a\" :required-option t :value-type 'string
;;;       :documentation \"Required option A.\")
;;;    (b \"b\" :required-option t :value-type 'integer
;;;       :documentation \"Required option D.\"))
;;;   (:documentation \"An example CLI class definition.\"))

;;; (parse-command-line (make-instance 'example-cli)
;;;                     (list \"--a\" \"aaa\" \"--b\" \"1\")))")
  (:method ((cli cli) arglist)
    (flet ((parse-arg (option matched-args)
             (with-accessors ((name name-of))
                 option
               (let ((value (assocdr name matched-args :test #'string=)))
                 (cond ((boolean-option-p option)
                        (when (find name matched-args :key #'first)
                          t))
                       ((and (value-parser-of option) value)
                        (parse-safely cli option value))
                       (t                 ; plain strings
                        value)))))
           (getopt-style (option)
             (list (name-of option) (cond ((required-value-p option)
                                           :required)
                                          ((eql t (value-type-of option))
                                           :none)
                                          (t
                                           :optional)))))
      (let* ((slots (option-slots-of cli))
             (options (options-of cli)))
        (multiple-value-bind (remaining-args matched-args unmatched-args)
            (getopt:getopt arglist (mapcar #'getopt-style options))
          (dolist (option options)
            (with-accessors ((name name-of))
                option
              (when (find name unmatched-args :test #'string=)
                (error 'missing-required-value :cli cli :name name))
              (when (and (required-option-p option)
                         (not (find name matched-args :key #'first
                                    :test #'string=)))
                (error 'missing-required-option :cli cli :name name))))
          (dolist (name unmatched-args)
            (if (find name options :key #'name-of :test #'string=)
                (warn 'unmatched-option :cli cli :name name)
                (warn 'unknown-option :cli cli :name name)))
          (values (pairlis slots (mapcar (lambda (option)
                                           (parse-arg option matched-args))
                                         options))
                  remaining-args unmatched-args))))))

(defgeneric option-slot-p (cli slot)
  (:documentation "Returns T if SLOT is an option slot in CLI, or NIL
otherwise. The default implementation returns T, meaning that all
slots are expected to contain option objects. Overriding this method
means that slots may be added for other purposes.")
  (:method ((cli cli) (slot symbol))
    t))

(defgeneric option-slots-of (cli)
  (:documentation "Returns a new, sorted list of CLI option slots.")
  (:method ((cli cli))
    (remove-if (lambda (slot)
                 (not (option-slot-p cli slot)))
               (all-slots (class-of cli)))))

(defgeneric options-of (cli)
  (:documentation "Returns a new, sorted list of CLI options.")
  (:method ((cli cli))
    (mapcar (lambda (slot)
              (slot-value cli slot)) (option-slots-of cli))))

(defgeneric option-of (cli name)
  (:documentation "Returns the CLI option identified by NAME.")
  (:method ((cli cli) (name string))
    (find name (options-of cli) :key #'name-of :test #'string=))
  (:method ((cli cli) (name symbol))
    (slot-value cli name)))

(defgeneric documentation-of (cli &optional name)
  (:documentation "Returns documentation of CLI or a CLI option
identified by NAME.")
  (:method ((cli cli) &optional name)
    (let ((class (class-of cli)))
      (if name
          (let ((slot (etypecase name
                        (symbol name)
                        (string (find-option-slot cli name)))))
            (slot-documentation slot class))
          (documentation (class-name class) 'type)))))

(defgeneric cli-help (cli &optional stream)
  (:documentation "Prints the help string for CLI to STREAM (which
defaults to *ERROR-OUTPUT*). This is usually the class documentation
string of CLI, plus all of the option help.")
  (:method ((cli cli) &optional stream)
    (let ((msg (documentation-of cli)))
      (help-message cli (or msg "") stream))))

(defgeneric option-help (cli name &optional stream)
  (:documentation "Prints the help string for option NAME to
STREAM (which defaults to *ERROR-OUTPUT*).")
  (:method ((cli cli) (name string) &optional stream)
    (let* ((slot (find-option-slot cli name))
           (option (slot-value cli slot)))
      (format stream
              "  ~20a <~@[~a, ~]~:[optional~;required~]>~%    ~a~%"
              (key-string option) (value-type-string option)
              (required-option-p option)
              (wrap-string (documentation-of cli slot)))))
  (:method ((cli cli) (slot symbol) &optional stream)
    (let ((option (slot-value cli slot)))
      (format stream
              "  ~20a <~@[~a, ~]~:[optional~;required~]>~%    ~a~%"
              (key-string option) (value-type-string option)
              (required-option-p option)
              (wrap-string (documentation-of cli slot))))))

(defgeneric help-message (cli message &optional stream)
  (:documentation "Prints a help MESSAGE and help for each avaliable
option in CLI to STREAM.")
  (:method ((cli cli) (message string) &optional stream)
    (write-line (wrap-string message) stream)
    (terpri stream)
    (write-line "  Options:" stream)
    (dolist (slot (option-slots-of cli))
      (option-help cli slot stream)
      (terpri stream))
    t))

(defgeneric required-value-p (option)
  (:documentation "Returns T if OPTION requires a value, or NIL
otherwise.")
  (:method ((option cli-option))
    (not (boolean-option-p option))))

(defgeneric boolean-option-p (option)
   (:documentation "Returns T if OPTION does not require a value, or
NIL otherwise.")
  (:method ((option cli-option))
    (eql t (value-type-of option))))

(defun option-value (key parsed-args &optional (default nil default-supplied-p))
  "Returns the value from alist PARSED-ARGS for the option named by the
symbol KEY."
  (unless default-supplied-p
    (check-arguments (member key parsed-args :key #'first)
                     (key parsed-args)
                     "there is no value for this key in the parsed arguments"))
  (if (and (not (assocdr key parsed-args)) default-supplied-p)
      default
      (assocdr key parsed-args)))

(defun find-option-slot (cli name)
  (nth (position name (options-of cli) :key #'name-of :test #'string=)
       (option-slots-of cli)))

(defgeneric parse-safely (cli option value)
  (:documentation "Returns a parsed VALUE of the correct Lisp type for
OPTION or raises an {define-condition incompatible-argument} error.")
  (:method ((cli cli) (option cli-option) value)
    (handler-case
        (funcall (value-parser-of option) value)
      (parse-error (condition)
        (declare (ignore condition))
        (error 'incompatible-value :cli cli :name (name-of option)
               :type (value-type-of option) :value value)))))

(defun parse-character (string)
  "Returns a character parsed from STRING of length 1 character."
  (let ((trimmed (string-trim '(#\Space) string)))
    (if (= 1 (length trimmed))
        (char trimmed 0)
        (error 'parse-error "expected a string of one character"))))

(defun parse-string-list (string)
  "Returns a list of strings parsed from STRING by splitting on the
*list-separator-char* character."
  (string-split string *list-separator-char* :remove-empty-substrings t))

(defun parse-integer-list (string)
  "Returns a list of integers parsed from STRING after splitting on
the *list-separator-char* character."
  (mapcar #'parse-integer (parse-string-list string)))

(defun parse-character-list (string)
  "Returns a list of integers parsed from STRING after splitting on
the *list-separator-char* character."
  (mapcar #'parse-character (parse-string-list string)))

(defun parse-float-list (string)
  "Returns a list of floats parsed from STRING after splitting on the
*list-separator-char* character."
  (mapcar #'parse-float (parse-string-list string)))

(defun subst-chars (str char &rest chars)
  "Returns a copy of STR where any occurrence of CHARS are substituted
by CHAR."
  (substitute-if char (lambda (char)
                        (member char chars :test #'char=)) str))

(defun wrap-string (str &optional stream)
  "Format STR, wrapped at 70 characters, to STREAM."
  (format stream "~{~<~%~,70:;~a~> ~}"
          (dxu:string-split (normalise-whitespace
                             (subst-chars str #\Space
                                          #\Newline #\Return #\Linefeed))
                            #\Space)))

(defun normalise-whitespace (str)
  (with-input-from-string (in str)
    (with-output-to-string (out)
      (do ((char (read-char in nil nil) (read-char in nil nil)))
          ((null char))
        (write-char char out)
        (when (whitespace-char-p char)
          (peek-char t in nil nil))))))

(defun key-string (option)
  (format nil "--~a" (name-of option)))

(defun value-type-string (option)
  (string-downcase (symbol-name (if (boolean-option-p option)
                                    'boolean
                                    (value-type-of option)))))

(defun print-error-message (condition &optional (stream *error-output*))
  (let ((str (format nil "~a" condition)))
    (write-line (string-capitalize str :end (min 1 (length str))) stream)))
