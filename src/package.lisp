;;;
;;; Copyright (c) 2007-2012 Keith James. All rights reserved.
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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun gray-streams-symbols ()
    "Returns a list of the symbols required for Gray streams."
    `(;; Classes
      #:fundamental-stream
      #:fundamental-input-stream
      #:fundamental-output-stream
      #:fundamental-character-stream
      #:fundamental-binary-stream
      #:fundamental-character-input-stream
      #:fundamental-character-output-stream
      #:fundamental-binary-input-stream
      #:fundamental-binary-output-stream

      ;; Common generic functions
      #:streamp
      #:input-stream-p
      #:output-stream-p
      #:open-stream-p
      #:stream-element-type
      ;; #:close

      ;; Input stream generic functions
      #:stream-clear-input

      ;; Output stream generic functions
      #:stream-finish-output
      #:stream-force-output
      #:stream-clear-output

      ;; Binary stream generic functions
      #:stream-read-byte
      #:stream-write-byte

      ;; Character stream generic functions
      #:stream-listen

      #:stream-peek-char
      #:stream-read-char
      #:stream-unread-char
      #:stream-read-char-no-hang
      #:stream-read-line

      #:stream-advance-to-column
      #:stream-fresh-line
      #:stream-line-column
      #:stream-write-char
      #:stream-start-line-p
      #:stream-write-string
      #:stream-terpri

      ;; Extra methods provided by implementations
      #+:sbcl ,@(list :stream-file-position
                      :stream-read-sequence
                      :stream-write-sequence))))

(defmacro define-deoxybyte-io-package ()
  `(defpackage :uk.co.deoxybyte-io
    (:use #:common-lisp #:deoxybyte-utilities)
    (:nicknames
     #:deoxybyte-io
     #:dxi)
    (:shadow #:type-of)
    (:import-from
     #+:sbcl :sb-gray
     #+:ccl :ccl
     ,@(gray-streams-symbols))
    (:export
     ;; IO conditions
     #:io-error
     #:io-warning

     ;; Parse conditions
     #:general-parse-error
     #:malformed-file-error
     #:malformed-record-error
     #:malformed-field-error
     #:record-validation-error
     #:field-validation-error
     #:record-of
     #:field-of

     ;; Parse utility macros
     #:check-record
     #:check-field

     ;; Environment variables
     #:environment-variable

     ;; Files and directories
     #:*default-tmpdir*
     #:*default-tmpfile-defaults*
     #:absolute-pathname-p
     #:relative-pathname-p
     #:file-pathname
     #:directory-pathname
     #:leaf-directory-pathname
     #:ensure-file-exists
     #:pathstring
     #:tmp-pathname
     #:with-tmp-pathname
     #:delete-tmp-pathname
     #:leave-tmp-pathname
     #:make-tmp-directory
     #:with-tmp-directory
     #:delete-tmp-directory
     #:leave-tmp-directory
     #:pathname-generator
     #:pathname-extender

     ;; CLI definition
     #:cli-error
     #:cli-option-error
     #:cli-warning
     #:cli-option-warning
     #:unknown-command
     #:missing-required-option
     #:missing-required-value
     #:incompatible-value
     #:unmatched-option
     #:unknown-option

     #:string-list
     #:integer-list
     #:character-list
     #:float-list

     #:define-cli
     #:cli
     #:cli-option
     #:cli-help

     #:option-slot-p
     #:option-slots-of
     #:options-of
     #:option-of
     #:option-help
     #:option-value
     #:help-message
     #:name-of
     #:required-option-p
     #:boolean-option-p
     #:required-value-p
     #:value-type-of
     #:value-parser-of
     #:value-default-of
     #:documentation-of
     #:parse-command-line

     #:print-backtrace
     #:quit-lisp
     #:with-argv

     ,@(gray-streams-symbols)

     #:stream-open
     #:stream-close
     #:stream-file-position
     #:stream-read-sequence
     #:stream-write-sequence

     #:wrapped-stream-mixin
     #:stream-of
     #:stream-filter-mixin
     #:test-of
     #:io-stream-mixin

     #:make-line-stream
     #:line-stream
     #:line-input-stream
     #:line-output-stream
     #:character-line-input-stream
     #:octet-line-input-stream
     #:line-stack-of
     #:pop-line
     #:push-line
     #:more-lines-p
     #:find-line

     ;; Table parser
     #:*empty-field*
     #:define-line-parser
     #:default-integer-parser
     #:default-float-parser

     ;; Binary operations
     #:encode-uint64le
     #:encode-uint32le
     #:encode-uint16le
     #:encode-uint8le

     #:encode-int64le
     #:encode-int32le
     #:encode-int16le
     #:encode-int8le

     #:decode-uint64le
     #:decode-uint32le
     #:decode-uint16le
     #:decode-uint8le

     #:decode-int64le
     #:decode-int32le
     #:decode-int16le
     #:decode-int8le

     #:encode-uint64be
     #:encode-uint32be
     #:encode-uint16be
     #:encode-uint8be

     #:encode-int64be
     #:encode-int32be
     #:encode-int16be
     #:encode-int8be

     #:decode-uint64be
     #:decode-uint32be
     #:decode-uint16be
     #:decode-uint8be

     #:decode-int64be
     #:decode-int32be
     #:decode-int16be
     #:decode-int8be

     #:encode-ieee-float32
     #:encode-ieee-float64

     #:decode-ieee-float32
     #:decode-ieee-float64

     #:encode-float64le
     #:encode-float32le

     #:decode-float64le
     #:decode-float32le

     #:encode-float64be
     #:encode-float32be

     #:decode-float64be
     #:decode-float32be

     #:define-integer-encoder
     #:define-integer-decoder

     ;; Misc
     #:parse-float

     ;; External merge sort protocol
     #:sort-input-stream
     #:sort-output-stream
     #:merge-stream
     #:stream-head-of
     #:stream-merge
     #:stream-read-element
     #:stream-write-element
     #:external-merge-sort

     ;; External merge sort by line implementation
     #:line-sort-input-stream
     #:line-sort-output-stream
     #:line-merge-stream
     #:make-merge-stream)
    (:documentation "The deoxybyte-io system is a selection of utility
code focused on transfer of data between Lisp and its environment. It
includes:

- IO and parser conditions
- File and directory utilities
- Command line interface definition utilities
- Stream classes and methods
- Tabular text parsing
- Binary encoding and decoding")))

(define-deoxybyte-io-package)
