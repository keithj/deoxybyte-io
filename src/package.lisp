;;;
;;; Copyright (C) 2007-2009 Keith James. All rights reserved.
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

(defpackage #:cl-io-utilities
  (:use #:common-lisp #:cl-gp-utilities #:trivial-gray-streams)
  (:nicknames #:iou)
  (:documentation "IO utilities.")
  (:shadow #:type-of)
  (:export
   ;; Specials
   #:*empty-field*
   #:*default-tmpdir*
   #:*default-remote-host*
   #:*remote-pathname-defaults*

   ;; Conditions
   #:io-error
   #:io-warning
   #:cli-error
   #:cli-warning
   #:unknown-command
   #:missing-required-option
   #:incompatible-argument
   #:unmatched-option
   #:unknown-option
   #:non-zero-exit-error

   #:general-parse-error
   #:malformed-file-error
   #:malformed-record-error
   #:malformed-field-error
   #:record-validation-error
   #:field-validation-error

   ;; Classes
   #:line-input-stream
   #:character-line-input-stream
   #:binary-line-input-stream
   #:stream-filter-mixin

   #:external-program
   #:rsh
   
   ;; Generics
   #:push-line
   #:more-lines-p
   #:find-line
   #:text-of
   #:test-of
   #:record-of
   #:field-of

   #:program-of
   #:args-of
   #:process-of
   #:input-of
   #:output-of
   #:error-of
   #:wait-for
   #:status-of
   #:exit-code-of
   #:close-process
   #:kill-process
   #:run
   #:runningp

   ;; Functions
   #:make-line-input-stream
   #:parse-float
   #:default-integer-parser
   #:default-float-parser

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

   #:external-merge-sort

   #:rsh-exec
   #:host-of
   #:merge-remote-pathnames
   #:rsh-list-directory
   #:rsh-file-exists-p
   #:rsh-directory-exists-p
   #:rsh-files-exist-p
   #:rsh-directories-exist-p
   #:rsh-make-directory
   #:rsh-ensure-directories-exist

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

   ;; Macros
   #:define-line-parser
   #:with-li-stream
   #:with-ascii-li-stream
   #:with-cli
   #:with-argv
   #:with-backtrace
   #:define-integer-encoder
   #:define-integer-decoder))
