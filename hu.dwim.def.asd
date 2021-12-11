;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(unless (or #+asdf3 (uiop:version<= "2.31.1" (asdf-version)))
  (error "You need ASDF >= 2.31.1 to load this system correctly."))

(defsystem :hu.dwim.def
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "General purpose, homogenous, extensible definer macro."
  :depends-on (:alexandria
               :anaphora
               :iterate
               :metabang-bind)
  :components ((:module "source"
                :components ((:file "infrastructure" :depends-on ("duplicates"))
                             (:file "definers" :depends-on ("definers-early"))
                             (:file "definers-early" :depends-on ("infrastructure"))
                             (:file "duplicates" :depends-on ("package"))
                             (:file "extended-package" :depends-on ("namespace"))
                             (:file "iterator" :depends-on ("definers" "with-macro"))
                             (:file "namespace" :depends-on ("definers" "with-macro"))
                             (:file "package")
                             (:file "with-macro" :depends-on ("definers"))))))

(defsystem :hu.dwim.def/namespace
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Thread safe namespace (global hashtable) definer."
  :depends-on (:bordeaux-threads
               :hu.dwim.def
               :hu.dwim.util
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "namespace-late")))))

(defsystem :hu.dwim.def/test
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

(defsystem :hu.dwim.def/documentation
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.documentation-system"
  :depends-on (:hu.dwim.def/test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "def" :depends-on ("package"))
                             (:file "package")))))
