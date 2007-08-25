;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

;;; These definitions need to be available by the time we are reading the other files, therefore
;;; they are in a standalone file.

(defun setup-readtable ()
  (enable-sharp-boolean-syntax)
  (values))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(setup-swank-readtable-alist
 '(:cl-def :cl-def-test) 'setup-readtable)
