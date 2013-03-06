;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.def+cl-l10n
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :depends-on (:cl-l10n
               :hu.dwim.def)
  :components ((:module "integration"
                :components ((:file "cl-l10n")))))
