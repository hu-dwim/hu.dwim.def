;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def (definer :available-flags "eod") contextl:layered-method ()
  (function-like-definer -definer- 'contextl:define-layered-method -whole- -environment- -options-))

(def (definer :available-flags "eod") contextl:layered-function ()
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         ,@outer-declarations
       ,@(when (getf -options- :export)
               `((export ',name)))
       (contextl:define-layered-function ,name ,@body))))

(contextl:deflayer foo ()
  ())

(def (definer :available-flags "eas") contextl::layer (name supers slots &rest class-options)
  (with-class-definer-options name slots
    `(contextl::deflayer ,name ,supers
       ,slots
       ,@class-options)))

(dolist (symbol '(contextl:layered-method
                  contextl:layered-function
                  contextl::layer))
  (integrated-export symbol :cl-def))
