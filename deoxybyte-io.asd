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

(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (asdf:find-system :deoxybyte-systems nil)
    (asdf:operate 'asdf:load-op :deoxybyte-systems)))

(defpackage :uk.co.deoxybyte-io-system
  (:use :common-lisp :asdf :deoxybyte-systems))

(in-package :uk.co.deoxybyte-io-system)

(defsystem deoxybyte-io
    :name "deoxybyte-io"
    :author "Keith James"
    :licence "GPL v3"
    :in-order-to ((test-op (load-op :deoxybyte-io :deoxybyte-io-test)))
    :depends-on (:deoxybyte-utilities :cl-fad :getopt
                 :trivial-gray-streams :ieee-floats)
    :components
    ((:module :core
              :serial t
              :pathname "src/"
              :components ((:file "package")
                           (:file "conditions")
                           (:file "parse-float")
                           (:file "binary-operations")
                           (:file "streams")
                           (:file "line-input-stream")
                           (:file "command-line-interface")
                           (:file "files-and-directories")
                           (:file "simple-table-parser")
                           (:file "external-merge-sort")))
     (:lift-test-config :deoxybyte-io-test
                        :target-system :deoxybyte-io)
     (:cldoc-config :deoxybyte-io-doc
                    :target-system :deoxybyte-io
                    :pathname "doc/html/")))
