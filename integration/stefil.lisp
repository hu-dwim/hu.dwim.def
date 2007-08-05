;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def definer test ()
  (function-like-definer 'stefil:deftest -whole- -environment- -options-))

