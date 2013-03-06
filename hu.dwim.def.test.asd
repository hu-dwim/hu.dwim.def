;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.def.test
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.test-system"
  :depends-on (:hu.dwim.common
               :hu.dwim.stefil+hu.dwim.def
               :optima)
  :components ((:module "test"
                :components ((:file "iterator" :depends-on ("suite"))
                             (:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "with-macro" :depends-on ("suite"))))))
