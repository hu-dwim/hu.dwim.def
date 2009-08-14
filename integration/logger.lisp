;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer :available-flags "e") logger (name ancestors &key compile-time-level level appender appenders documentation)
  `(progn
     (hu.dwim.logger:deflogger ,name ,ancestors
       :compile-time-level ,compile-time-level
       :level ,level
       :appender ,appender
       :appenders ,appenders
       :documentation ,documentation)
     ,@(when (getf -options- :export)
             `((export ',name)))))

(integrated-export '(logger) :hu.dwim.logger)
