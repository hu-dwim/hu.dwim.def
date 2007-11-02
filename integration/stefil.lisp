;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

;; we define "test" with a string, so it will match (def test ...) no matter which package 'test is in.
;; this is done like that to be able to write (def any-package::test some-lib::test ...) in any
;; library to create the toplevel test (a defun) called some-lib::test.
(def definer "TEST" ()
  (function-like-definer -definer- 'stefil:deftest -whole- -environment- -options-))

