;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def.test
  :class hu.dwim.test-system
  :depends-on (:hu.dwim.stefil+hu.dwim.def)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "test" :depends-on ("package"))))))
