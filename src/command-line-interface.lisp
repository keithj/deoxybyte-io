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

(defparameter *list-separator-char* #\,
  "The separator character used in multi-value arguments.")

(defmacro with-argv (argv &body body)
  "Executes BODY with ARGV bound to system argv list."
  `(let ((,argv (get-system-argv)))
    ,@body))

(defmacro with-backtrace ((&key quit error-status error-file) &body body)
  "Executes BODY with an error handler that prints a stack trace when
an error is encountered.

Key:

- quit (boolean): If T, quit Lisp after printing the stack trace.
- error-status (integer): The exit code with which to quit Lisp.
- error-file (filespec): A pathname designator for a file to which the
  stack trace will be written, instead of printing it to
  *ERROR-OUTPUT*."
  `(handler-bind
    ((error (lambda (condition)
              (format *error-output* "Error: ~a~%" condition)
              ,(if error-file
                   `(with-open-file (stream ,error-file
                                     :direction :output
                                     :if-does-not-exist :create
                                     :if-exists :append)
                      (format stream "Error: ~a~%" condition)
                      (terpri stream)
                      (write-line "Backtrace:" stream)
                      (print-backtrace condition stream))
                   '(print-backtrace condition *error-output*))
              ,(when quit
                     `(quit-lisp :status ,error-status)))))
    ,@body))

(defmacro with-cli ((argv &key quit error-status error-file) &body body)
  "Executes BODY within the combined environments of {defmacro WITH-ARGV}
and {defmacro WITH-BACKTRACE} ."
  `(with-backtrace (:quit ,quit :error-status ,error-status
                    ,@(when error-file
                            `(:error-file ,error-file)))
    (with-argv ,argv
      ,@body)))

#+:sbcl
(defun get-system-argv ()
  (rest sb-ext:*posix-argv*))

#+:lispworks
(defun get-system-argv ()
  (rest *line-arguments-list*))

#+:ccl
(defun get-system-argv ()
  (rest ccl:*command-line-argument-list*))

#-(or :sbcl :lispworks :ccl)
(defun get-system-argv ()
  (error "Not implemented on ~a" (lisp-implementation-type)))

#+:sbcl
(defun print-backtrace (condition stream)
  (declare (ignore condition))
  (sb-debug:backtrace 20 stream))

#+:lispworks
(defun print-backtrace (condition stream)
  (declare (ignore condition))
  (let ((*debug-io* stream))
    (dbg:with-debugger-stack ()
      (dbg:bug-backtrace nil))))

#+:ccl
(defun print-backtrace (condition stream)
  (declare (ignore condition))
  (let ((*debug-io* stream))
    (ccl:print-call-history :count 20 :detailed-p nil)))

#-(or :sbcl :lispworks :ccl)
(defun print-backtrace (condition)
  (error "Not implemented on ~a" (lisp-implementation-type)))

#+:sbcl
(defun quit-lisp (&key (status 0))
  (sb-ext:quit :unix-status status))

#+:lispworks
(defun quit-lisp (&key (status 0))
  (lw:quit :status status))

#+:ccl
(defun quit-lisp (&key (status 0))
  (ccl:quit status))

#-(or :sbcl :lispworks :ccl)
(defun quit-lisp ()
  (error "Not implemented on ~a" (lisp-implementation-type)))

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
                       ((and (required-option-p option)
                             (value-parser-of option))
                        (and value (parse-safely option value)))
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
                (error 'missing-required-value :name name))
              (when (and (required-option-p option)
                         (not (find name matched-args :key #'first
                                    :test #'string=)))
                (error 'missing-required-option :name name))))
          (dolist (name unmatched-args)
            (if (find name options :key #'name-of :test #'string=)
                (warn 'unmatched-option :name name)
                (warn 'unknown-option :name name)))
          (values (pairlis slots (mapcar (lambda (option)
                                           (parse-arg option matched-args))
                                         options))
                  remaining-args unmatched-args))))))

(defgeneric option-slot-p (cli slot)
  (:documentation "Returns T if SLOT is an option slot in CLI, or NIL
otherwise.")
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

(defgeneric option-help (cli name &optional stream)
  (:documentation "Prints the help string for option NAME to
STREAM (which defaults to *ERROR-OUTPUT*).")
  (:method ((cli cli) (name string) &optional stream)
    (let* ((options (options-of cli))
           (slot (nth (position name options :key #'name-of :test #'string=)
                      (option-slots-of cli)))
           (option (slot-value cli slot)))
      (format stream
              "  --~20a <~@[~a, ~]~:[optional~;required~]>~%    ~a~%"
              name (value-type-of option) (required-option-p option)
              (wrap-string (slot-documentation slot (class-of cli))))))
  (:method ((cli cli) (slot symbol) &optional stream)
    (let ((option (slot-value cli slot)))
      (format stream
              "  --~20a <~@[~a, ~]~:[optional~;required~]>~%    ~a~%"
              (name-of option) (value-type-of option) (required-option-p option)
              (wrap-string (slot-documentation slot (class-of cli)))))))

(defgeneric help-message (cli message &optional stream)
  (:documentation "Prints a help MESSAGE and help for each avaliable
option in CLI to STREAM.")
  (:method ((cli cli) message &optional stream)
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

(defun option-value (key parsed-args)
  "Returns the value from alist PARSED-ARGS for the option named by the
symbol KEY."
  (check-arguments (member key parsed-args :key #'first)
                   (key parsed-args)
                   "there is no value for this key in the parsed arguments")
  (assocdr key parsed-args))

(defgeneric parse-safely (option value)
  (:documentation "Returns a parsed VALUE of the correct Lisp type for
OPTION or raises an {define-condition incompatible-argument} error.")
  (:method ((option cli-option) value)
    (handler-case
        (funcall (value-parser-of option) value)
      (parse-error (condition)
        (declare (ignore condition))
        (error 'incompatible-value :name (name-of option)
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
