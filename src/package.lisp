;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(defpackage :uk.co.deoxybyte-io
  (:use #:common-lisp #:trivial-gray-streams #:deoxybyte-utilities)
  (:nicknames
   #:deoxybyte-io
   #:dxi)
  (:shadow #:type-of)
  (:export
   ;; IO conditions
   #:io-error
   #:io-warning
   #:text-of

   ;; Parse conditions
   #:general-parse-error
   #:malformed-file-error
   #:malformed-record-error
   #:malformed-field-error
   #:record-validation-error
   #:field-validation-error
   #:record-of
   #:field-of

   ;; Files and directories
   #:*default-tmpdir*
   #:absolute-pathname-p
   #:relative-pathname-p
   #:parse-file
   #:parse-directory
   #:ensure-file-exists
   #:make-tmp-pathname
   #:make-tmp-directory
   #:with-tmp-directory
   #:make-pathname-gen
   #:make-pathname-ext

   ;; CLI definition
   #:cli-error
   #:cli-warning
   #:unknown-command
   #:missing-required-option
   #:incompatible-argument
   #:unmatched-option
   #:unknown-option
   #:parse-command-line
   #:print-cli-help
   #:cli-option
   #:print-option-help
   #:cli-opt-key
   #:cli-opt-name
   #:cli-opt-required-p
   #:cli-opt-documentation
   #:cli-arg-required-p
   #:cli-arg-type
   #:cli-arg-parser
   #:cli-arg-value
   #:print-backtrace
   #:quit-lisp
   #:with-cli
   #:with-argv
   #:with-backtrace

   ;; Streams
   #:line-input-stream
   #:character-line-input-stream
   #:binary-line-input-stream
   #:stream-filter-mixin
   #:push-line
   #:more-lines-p
   #:find-line
   #:test-of
   #:make-line-input-stream
   #:with-li-stream
   #:with-ascii-li-stream

   ;; Table parser
   #:*empty-field*
   #:define-line-parser
   #:default-integer-parser
   #:default-float-parser

   ;; Binary operations
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

   #:encode-float64le
   #:encode-float32le

   #:decode-float64le
   #:decode-float32le

   #:encode-float64le
   #:encode-float32be

   #:decode-float64le
   #:decode-float32be

   #:define-integer-encoder
   #:define-integer-decoder

   ;; Misc
   #:parse-float
   #:external-merge-sort)   
  (:documentation "The deoxybyte-io system is a selection of utility
code focused on transfer of data between Lisp and its environment. It
includes:

- IO and parser conditions
- File and directory utilities
- Command line interface definition utilities
- Stream classes and methods
- Tabular text parsing
- Binary encoding and decoding"))
