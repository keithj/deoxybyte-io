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

(define-cli test-cli (cli)
  ((a "a" :required-option t :value-type 'string
      :documentation "Required option A.")
   (b "b" :required-option t :value-type 'integer
      :documentation "Required option B.")
   (c "c" :required-option t :value-type 'character
      :documentation "Required option C.")
   (d "d" :required-option t :value-type 'float
      :documentation "Required option D.")))

(define-cli test-cli-subclass (test-cli)
  ((e "e" :required-option t :value-type 'string-list
      :documentation "Required option E.")
   (f "f" :required-option t :value-type 'integer-list
      :documentation "Required option F.")
   (g "g" :required-option t :value-type 'character-list
      :documentation "Required option G.")
   (h "h" :required-option t :value-type 'float-list
      :documentation "Required option H.")))

(define-cli test-optional-cli (cli)
  ((a "a" :required-option nil :value-type 'string)))

(define-cli test-boolean-cli (cli)
  ((a "a" :required-option nil :value-type t)))

(addtest (deoxybyte-io-tests) option-slot-p/1
  (let ((cli (make-instance 'test-cli)))
    (ensure (every (lambda (slot)
                     (option-slot-p cli slot)) '(a b c d)))))

(addtest (deoxybyte-io-tests) option-slots-of/1
  (ensure (equal '(a b c d) (option-slots-of (make-instance 'test-cli)))))

(addtest (deoxybyte-io-tests) option-of/1
  (let ((cli (make-instance 'test-cli)))
    (ensure (every #'equal '("a" "b" "c" "d")
                   (mapcar #'name-of (mapcar (lambda (name)
                                               (option-of cli name))
                                             '(a b c d)))))))

(addtest (deoxybyte-io-tests) option-of/2
  (let ((cli (make-instance 'test-cli)))
    (ensure (every #'equal '("a" "b" "c" "d")
                   (mapcar #'name-of (mapcar (lambda (name)
                                               (option-of cli name))
                                             '("a" "b" "c" "d")))))))

(addtest (deoxybyte-io-tests) parse-command-line/1
  (ensure (make-instance 'test-cli-subclass))
  (let* ((arglist (list "--a" "aaa"
                        "--b" "1"
                        "--c" "c"
                        "--d" "0.1"
                        "--e" "aaa,bbb,ccc"
                        "--f" "1,2,3"
                        "--g" "a,b,c"
                        "--h" "0.1,0.2,0.3"))
         (super-expected '((D . 0.1) (C . #\c) (B . 1) (A . "aaa")))
         (super-parsed (parse-command-line (make-instance 'test-cli) arglist))
         (sub-expected '((H 0.1 0.2 0.3)
                         (G #\a #\b #\c)
                         (F 1 2 3)
                         (E "aaa" "bbb" "ccc")
                         (D . 0.1) (C . #\c) (B . 1) (A . "aaa")))
         (sub-parsed (parse-command-line
                      (make-instance 'test-cli-subclass) arglist)))
    (ensure (equalp super-expected super-parsed)
            :report "Expected ~a but found ~a"
            :arguments (super-expected super-parsed))
    (ensure (equalp sub-expected sub-parsed)
            :report "Expected ~a but found ~a"
            :arguments (sub-expected sub-parsed))))

(addtest (deoxybyte-io-tests) parse-command-line/2
  (let ((cli (make-instance 'test-cli)))
    (multiple-value-bind (parsed remaining unknown)
        (parse-command-line cli (list "--a" "aaa"
                                      "--b" "1"
                                      "--c" "c"
                                      "--d" "0.1"
                                      "--w"
                                      "x" "y" "z"))
      (ensure (equal '((D . 0.1) (C . #\c) (B . 1) (A . "aaa")) parsed))
      (ensure (equal '("x" "y" "z") remaining))
      (ensure (equal '("w") unknown)))))

(addtest (deoxybyte-io-tests) parse-command-line/3
  (let ((cli (make-instance 'test-cli)))
    (ensure-condition missing-required-option
      (parse-command-line cli (list "--a" "aaa"
                                    "--b" "1"
                                    "--c" "c")))
    (ensure-condition missing-required-value
      (parse-command-line cli (list "--a"
                                    "--b" "1"
                                    "--c" "c"
                                    "--d" "0.1")))))

(addtest (deoxybyte-io-tests) parse-command-line/4
  (let ((cli (make-instance 'test-boolean-cli)))
    (ensure (equal '((A . T)) (parse-command-line cli (list "--a"))))
    (ensure (equal '((A)) (parse-command-line cli nil)))))

(addtest (deoxybyte-io-tests) option-value/1
  (let ((parsed (parse-command-line (make-instance 'test-cli)
                                    (list "--a" "aaa"
                                          "--b" "1"
                                          "--c" "c"
                                          "--d" "0.1"))))
    (ensure (equal "aaa" (option-value 'a parsed)))
    (ensure-condition invalid-argument-error
      (option-value 'w parsed))))
