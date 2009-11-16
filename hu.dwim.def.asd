;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.def
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :licence "BSD / Public domain"
  :description "General purpose, extensible definer macro."
  :depends-on (:hu.dwim.syntax-sugar)
  :components ((:module "source"
                :components ((:file "configuration" :depends-on ("package"))
                             (:file "def" :depends-on ("configuration" "duplicates"))
                             (:file "definers" :depends-on ("definers-early"))
                             (:file "definers-early" :depends-on ("def"))
                             (:file "duplicates" :depends-on ("package"))
                             (:file "package")))))
