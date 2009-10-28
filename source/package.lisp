;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.def
  (:use :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.syntax-sugar)

  (:export #:def
           #:definer
           #:find-definer
           #:with-standard-definer-options
           #:function-like-definer-declarations

           #:-whole-
           #:-environment-
           #:-definer-
           #:-options-
           #:-body-
           #:-self-))
