;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defpackage :cl-def-test
    (:use :common-lisp :cl-def :metabang-bind :alexandria :iterate :stefil)
  (:shadow)
  (:export #:test))

(eval-always
  (import
   '(enable-sharp-boolean-syntax rebind eval-always)
   (find-package :cl-def-test)))

(in-package :cl-def-test)

(in-root-suite)

(defsuite* (test :description "cl-def tests"))

(deftest test/function ()
  (is (equal '(progn
               (declaim (inline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (export 'foo)
                 (defun foo (bar baz unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (function ioed) foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/method ()
  (is (equal '(progn
               (locally
                   (declare (optimize (speed 0) (debug 3)))
                 (export 'foo)
                 (defmethod foo ((bar integer) (baz string) unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (method oed) foo ((bar integer) (baz string) unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/test ()
  (is (equal '(progn
               (locally
                   (deftest foo (bar baz unused)
                     "documentation"
                     (declare (ignore unused))
                     (+ bar baz))))
             (macroexpand-1 '(def test foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/constant ()
  (is (equal '(progn
               (defconstant +foo+ (cl-def::%reevaluate-constant '+foo+ 1 :test 'eql)
                 "documentation"))
             (macroexpand-1 '(def constant +foo+ 1 "documentation")))))

(deftest test/special-variable ()
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)
                 (setf +foo+ 1)))
             (macroexpand-1 '(def special-variable +foo+ 1 "documentation"))))
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)))
             (macroexpand-1 '(def (special-variable :documentation "documentation") +foo+)))))
