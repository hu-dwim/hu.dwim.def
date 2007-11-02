;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

#.(setup-readtable)

(defun definer-lookup-hook (form)
  (when (typep form 'definer-name)
    (awhen (find-definer form #f)
      (values it #t))))

(awhen (find-symbol (symbol-name '#:*inspector-dwim-lookup-hooks*) :swank)
  (pushnew 'definer-lookup-hook (symbol-value it)))
