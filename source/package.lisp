;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :common-lisp-user)

(defpackage :hu.dwim.def
  (:use :alexandria
        :anaphora
        :common-lisp
        :hu.dwim.asdf
        :iterate
        :metabang-bind)
  (:export #:def
           #:definer
           #:find-definer
           #:with-standard-definer-options
           #:function-like-definer-declarations

           ;; lexically visible non-hygienic names
           #:-body-
           #:-child-
           #:-children-
           #:-content-
           #:-definer-
           #:-environment-
           #:-form-
           #:-name-
           #:-options-
           #:-recurse-
           #:-self-
           #:-this-definition/name-
           #:-visitor-
           #:-whole-
           #:-with-macro/body-
           ))
