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
