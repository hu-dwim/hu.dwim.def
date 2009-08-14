;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (values))
