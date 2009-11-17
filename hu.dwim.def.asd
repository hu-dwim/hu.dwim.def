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
  :depends-on (:alexandria
               :anaphora
               :iterate
               :metabang-bind)
  :components ((:module "source"
                :components ((:file "def" :depends-on ("duplicates"))
                             (:file "definers" :depends-on ("definers-early"))
                             (:file "definers-early" :depends-on ("def"))
                             (:file "duplicates" :depends-on ("package"))
                             (:file "package")))))
