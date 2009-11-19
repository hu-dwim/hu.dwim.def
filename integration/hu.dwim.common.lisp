;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;;;;;
;;; set up a hu.dwim.common:in-package that shadows cl:in-package and will set up the readtable at compile time.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'common-lisp:in-package :hu.dwim.common)
  (shadow "IN-PACKAGE" :hu.dwim.common))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; this export is not inside the above eval-when for a reason.
  (export 'hu.dwim.common::in-package :hu.dwim.common)
  (assert (not (eq 'hu.dwim.common::in-package 'common-lisp:in-package))))

(def macro hu.dwim.common::in-package (package-name)
  `(progn
     (eval-when (:compile-toplevel :execute)
       (bind ((extended-package (find-extended-package ,(string package-name) :otherwise nil)))
         (awhen (and extended-package
                     (readtable-setup-form-of extended-package))
           ;; external forces make sure that *readtable* is rebound, but not that it's copied. keep that in mind when using :setup-readtable for (def package ...)
           (eval it))))
     (common-lisp:in-package ,package-name)))
