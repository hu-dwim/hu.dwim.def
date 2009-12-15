;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.def.test
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.asdf
        :hu.dwim.def
        :hu.dwim.stefil
        :iterate
        :metabang-bind))
