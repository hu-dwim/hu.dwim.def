;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.def.documentation
  :description "Documentation for hu.dwim.def"
  :depends-on (:hu.dwim.def
               :hu.dwim.stefil
               :hu.dwim.def
               :hu.dwim.walker
               :swank)
  :components ((:module "documentation"
                :components ((:file "package")))))
