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

(in-package :uk.co.deoxybyte-io-test)

(addtest (deoxybyte-io-tests) pathstring/1
  (ensure (string= "/foo/bar/baz.txt"
                   (pathstring (pathname "/foo/bar/baz.txt"))))
  ;; These tests ensure that we can unescape all the dots under CCL
  (ensure (string= "/foo/bar.baz.txt"
                   (pathstring (pathname "/foo/bar.baz.txt"))))
  (ensure (string= "/foo/bar..baz.txt"
                   (pathstring (pathname "/foo/bar..baz.txt")))))

(addtest (deoxybyte-io-tests) make-tmp-pathname/1
  ;; Test defaults
  (ensure (pathnamep (make-tmp-pathname)))
  (ensure (string= "/tmp/" (directory-namestring (make-tmp-pathname))))
  (ensure (integerp (parse-integer (pathname-name (make-tmp-pathname)))))
  (ensure-null (pathname-type (make-tmp-pathname)))
  ;; Test optional arguments
  (ensure (string= "/" (directory-namestring (make-tmp-pathname
                                              :tmpdir "/"))))
  (ensure (string= "tmp" (pathname-type (make-tmp-pathname :type "tmp"))))
  (ensure (string= "foo" (pathname-name (make-tmp-pathname :tmpdir "/tmp"
                                                           :basename "foo"))
                   :end2 3))
  (ensure (string= "bar" (pathname-type
                          (make-tmp-pathname :tmpdir "/tmp"
                                             :basename "foo"
                                             :type "bar"))))
  ;; Test error condition
  (let ((bad-dir "/this-directory-does-not-exist/"))
    (ensure (and (not (fad:directory-exists-p bad-dir))
                 (ensure-condition invalid-argument-error
                                   (make-tmp-pathname :tmpdir bad-dir))))))

(addtest (deoxybyte-io-tests) absolute-pathname-p/1
  (ensure (not (absolute-pathname-p "foo")))
  (ensure (absolute-pathname-p "/foo")))

(addtest (deoxybyte-io-tests) relative-pathname-p/1
  (ensure (relative-pathname-p "foo"))
  (ensure (relative-pathname-p "./foo"))
  (ensure (relative-pathname-p "../foo")))

(addtest (deoxybyte-io-tests) directory-pathname/1
  (ensure (equal (pathname "/") (directory-pathname "/foo")))
  (ensure (equal (pathname "/foo/") (directory-pathname "/foo/")))
  (ensure (equal (pathname "/foo/bar/") (directory-pathname "/foo/bar/"))))

(addtest (deoxybyte-io-tests) file-pathname/1
  (ensure (equal (pathname "foo") (file-pathname "/foo")))
  (ensure (equal (pathname "") (file-pathname "/foo/")))
  (ensure (equal (pathname "bar") (file-pathname "/foo/bar"))))

(addtest (deoxybyte-io-tests) leaf-directory-pathname/1
  (ensure (equal (pathname "/") (leaf-directory-pathname "/foo")))
  (ensure (equal (pathname "/foo/") (leaf-directory-pathname "/foo/")))
  (ensure (equal (pathname "bar/") (leaf-directory-pathname "/foo/bar/"))))
