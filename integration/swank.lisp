;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def function definer-lookup-hook (form)
  (when (typep form 'definer-name)
    (awhen (find-definer form nil)
      (values it t))))

(awhen (find-symbol (symbol-name '#:*inspector-dwim-lookup-hooks*) :swank)
  (pushnew 'definer-lookup-hook (symbol-value it)))

(register-readtable-for-swank
 '(:hu.dwim.def :hu.dwim.def.test) 'setup-readtable)
