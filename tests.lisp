;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defpackage :cl-def.test
    (:use :common-lisp :cl-def :metabang-bind :alexandria :iterate :stefil)
  (:shadow)
  (:export #:test))

(eval-always
  (import
   '(enable-sharp-boolean-syntax rebind eval-always)
   (find-package :cl-def.test)))

(in-package :cl-def.test)

(in-root-suite)

(defsuite* (test :description "cl-def tests"))

(deftest foo ()
  (is #t))

