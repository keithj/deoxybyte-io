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

(in-package :uk.co.deoxybyte-io)

(defparameter *default-tmpdir* (pathname "/tmp/")
  "The default temporary file directory pathname.")

(defparameter *default-tmpfile-defaults*
  (make-pathname :directory (pathname-directory *default-tmpdir*))
  "The defaults used to fill in temporary file pathnames.")

(defmacro with-tmp-directory ((directory &key (tmpdir *default-tmpdir*)
                                         (basename "") (if-exists :error)
                                         (mode 511))
                              &body body)
  "Executes BODY with DIRECTORY bound to a temporary directory that
has been created with the MAKE-TMP-DIRECTORY function. The temporary
directory is deleted afterwards."
  `(let ((,directory (make-tmp-directory :tmpdir ,tmpdir :basename
                                         ,basename
                                         :if-exists ,if-exists :mode ,mode)))
    (unwind-protect
         (progn
           ,@body)
      (fad:delete-directory-and-files ,directory))))

(defun absolute-pathname-p (pathname)
  "Returns T if PATHSPEC is a pathname designator for an absolute file
or directory, or NIL otherwise."
  (eql :absolute (first (pathname-directory (pathname pathname)))))

(defun relative-pathname-p (pathname)
  "Returns T if PATHSPEC is a pathname designator for a relative file
or directory, or NIL otherwise."
  (eql :relative (first (pathname-directory (pathname pathname)))))

(defun parse-file (pathname)
  "Returns a new pathame that represents the file component of
PATHSPEC. Deprecated."
  (warn 'deprecation-warning :feature 'parse-file :in-favour 'file-pathname)
  (file-pathname pathname))

(defun parse-directory (pathname)
  "Returns a new pathame that represents the last directory component
of PATHSPEC. Deprecated."
  (warn 'deprecation-warning :feature 'parse-directory
        :in-favour 'directory-pathname)
  (directory-pathname pathname))

(defun file-pathname (pathname)
  "Returns a new pathame that represents the file component of
PATHSPEC."
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun directory-pathname (pathname)
  "Returns a new pathame that represents the last directory component
of PATHSPEC."
  (make-pathname :directory (pathname-directory pathname)))

(defun ensure-file-exists (filespec)
  "Creates the file designated by FILESPEC, if it does not
exist. Returns the pathname of FILESPEC."
  (with-open-file (stream filespec :direction :output
                   :if-does-not-exist :create
                   :if-exists nil)
    (declare (ignorable stream)))
  (pathname filespec))

(defun pathstring (pathname)
  "Returns a string representing PATHNAME. This function is similar to
CL:NAMESTRING, but is designed to be portable whereas the return value
of CL:NAMESTRING is implementation-dependent."
  (labels ((unescape (str)
             (let ((esc-pos (search "\\." str )))
               (if esc-pos
                   (cons (subseq str 0 esc-pos)
                         (unescape (subseq str (1+ esc-pos))))
                 (list str)))))
    #+:ccl (apply #'concatenate 'string (unescape (namestring pathname)))
    #-:ccl (namestring pathname)))

(defun merge-pathstrings (pathname &optional
                          (default-pathname *default-pathname-defaults*)
                          (default-version :newest))
  "Merges PATHNAME with defaults, using CL:MERGE-PATHNAMES, and calls
{defun pathstring} on the result."
  (pathstring (merge-pathnames (pathname pathname)
                               default-pathname default-version)))

(defun make-tmp-pathname (&key (tmpdir *default-tmpdir*) (basename "") type)
  "Returns a pathname suitable for use as a temporary file or
directory. The directory component of the new pathname is TMPDIR,
defaulting to *DEFAULT-TMPDIR*. The NAME component of the new pathname
is a concatenation of BASENAME, defaulting to an empty string, and a
pseudo-random number. The type component of the new pathname is TYPE,
defaulting to NIL."
  (unless (cl-fad:directory-exists-p tmpdir)
    (error 'invalid-argument-error
           :params 'tmpdir
           :args tmpdir
           :text "temporary file directory does not exist"))
  (merge-pathnames (fad:pathname-as-directory tmpdir)
                   (make-pathname :directory '(:relative)
                                  :name (format nil "~a~a" basename
                                                (random most-positive-fixnum))
                                  :type type)))

(defun make-tmp-directory (&key (tmpdir *default-tmpdir*) (basename "")
                           (if-exists :error))
  "Creates a new temporary directory and returns its pathname. The new
directory's pathname is created using {defun make-tmp-pathname} . The
IF-EXISTS keyword argument determines what happens if a directory by
that name already exists; options are :error which causes a FILE-ERROR
to be raised, :supersede which causes the existing directory to be
deleted and a new, empty one created and NIL where no directory is
created an NIL is returned to indicate failure."
  (let ((pathname (make-tmp-pathname :tmpdir tmpdir :basename basename)))
    (ecase if-exists
      (:error (if (fad:directory-exists-p pathname)
                  (error 'file-error :pathname pathname)))
      (:supersede (if (fad:directory-exists-p pathname)
                      (fad:delete-directory-and-files pathname)))
      ((nil) nil))
    ;; :mode is a non-ANSI extension to ensure-directories-exist in SBCL
    (ensure-directories-exist (fad:pathname-as-directory pathname))))

(defun make-pathname-gen (directory name &key type separator generator)
  "Returns a function of zero arity that generates pathnames when
called. The generated pathnames are relative to DIRECTORY and have a
namestring composed of NAME, SEPARATOR (defaults to NIL) and a value
taken from calling the function GENERATOR (defaults to a numeric
generator starting from 0, incrementing by 1). TYPE may be used to
specify the type of the new pathnames."
  (let ((gen (or generator (make-number-gen))))
    (flet ((gen-pname (d n s g y)
             (merge-pathnames
              (fad:pathname-as-directory d)
              (make-pathname :directory '(:relative)
                             :name (format nil "~a~@[~a~]~a" n s (next g))
                             :type y))))
      (lambda (op)
        (let ((current (gen-pname directory name separator
                                  gen type)))
          (ecase op
            (:current current)
            (:next (prog1
                       current
                     (setf current (gen-pname directory name separator
                                              gen type))))
            (:more t)))))))

(defun make-pathname-ext (pathname &key type separator generator)
  "Returns a function of arity 1 that returns modified copies of a
pathname argument. The pathname is modified by extending its
namestring. The new namestring is composed of the original namestring
SEPARATOR (defaults to NIL) and a value taken from calling the
function GENERATOR (defaults to a numeric generator starting from 0,
incrementing by 1). TYPE may be used to specify the type of the new
pathname, otherwise the original type will be used."
  (let ((gen (or generator (make-number-gen))))
    (lambda ()
      (make-pathname :directory (pathname-directory pathname)
                     :name (format nil "~a~@[~a~]~a" (pathname-name pathname)
                                   separator (next gen))
                     :type (or type (pathname-type pathname))))))
