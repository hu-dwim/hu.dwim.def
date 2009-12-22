;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;;;;;
;;; Set up a HU.DWIM.COMMON:IN-PACKAGE that shadows CL:IN-PACKAGE and sets up the readtables of extended packages at compile time.
;;;
;;; The seemingly unnecessary complexity around the interning of HU.DWIM.COMMON:IN-PACKAGE and defining the macro
;;; with eval is required due to a bug (or the standard allows it?) in ECL, namely that its reader and
;;; (eval-when (:compile-toplevel)) does not happen form by form. Thus, a HU.DWIM.COMMON::IN-PACKAGE anywhere
;;; in the file will be read to CL:IN-PACKAGE, even when after a compile-toplevel shadow of IN-PACKAGE.
;;; But take it all with a piece of salt... - lendvai

(def function expand-in-package (package-name)
  `(progn
     (eval-when (:compile-toplevel :execute)
       (bind ((extended-package (find-extended-package ,(string package-name) :otherwise nil)))
         (awhen (and extended-package
                     (readtable-setup-form-of extended-package))
           ;; external forces make sure that *readtable* is rebound, but not that it's copied. keep that in mind when using :setup-readtable for (def package ...)
           (eval it))))
     (common-lisp:in-package ,package-name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unintern 'common-lisp:in-package :hu.dwim.common)
  (shadow '#:in-package :hu.dwim.common)
  (let ((hu.dwim.common/in-package (intern "IN-PACKAGE" :hu.dwim.common)))
    (export hu.dwim.common/in-package :hu.dwim.common)
    (assert (not (eq hu.dwim.common/in-package 'common-lisp:in-package)))
    ;; Please don't try using compile.
    (eval `(def macro ,hu.dwim.common/in-package (package-name)
             (expand-in-package package-name)))))

#||

The above in a bit clearer form (which doesn't work on ECL as of this writing).

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

||#
