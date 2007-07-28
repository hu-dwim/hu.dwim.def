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

(deftest test-function ()
  (is (equal '(progn
               (declaim (inline foo))
               (export 'foo)
               (defun foo (bar baz unused)
                 "documentation"
                 (declare (optimize (speed 0) (debug 3)))
                 (declare (ignore unused))
                 (+ bar baz)))
             (macroexpand-1 '(def (function ioed) foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test-method ()
  (is (equal '(progn
               (declaim (inline foo))
               (export 'foo)
               (defmethod foo ((bar integer) (baz string) unused) "documentation"
                          (declare (optimize (speed 0) (debug 3)))
                          (declare (ignore unused)) (+ bar baz)))
             (macroexpand-1 '(def (method ioed) foo ((bar integer) (baz string) unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test-test ()
  (is (equal '(progn
               (deftest foo (bar baz unused) "documentation"
                        (declare (ignore unused)) (+ bar baz)))
             (macroexpand-1 '(def test foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test-constant ()
  (is (equal '(defconstant +foo+ 1 "documentation")
             (macroexpand-1 '(def constant +foo+ 1 "documentation")))))

(deftest test-variable ()
  (is (equal '(defvar +foo+ 1 "documentation")
             (macroexpand-1 '(def variable +foo+ 1 "documentation")))))
