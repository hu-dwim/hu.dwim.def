;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def (definer :available-flags "e") logger (name ancestors &key compile-time-level level appender appenders documentation)
  `(cl-yalog:deflogger ,name ,ancestors
     :compile-time-level ,compile-time-level
     :level ,level
     :appender ,appender
     :appenders ,appenders
     :documentation ,documentation))

(integrated-export '(logger) :cl-yalog)
