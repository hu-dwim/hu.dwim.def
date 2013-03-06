;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.def.namespace
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "Thread safe namespace (global hashtable) definer."
  :depends-on (:bordeaux-threads
               :hu.dwim.def
               :hu.dwim.util
               :trivial-garbage)
  :components ((:module "source"
                :components ((:file "namespace-late")))))
