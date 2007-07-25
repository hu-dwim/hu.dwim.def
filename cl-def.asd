;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

(defpackage #:cl-def.system
  (:use :cl :asdf)

  (:shadow
   ;; bah, i don't want to give them new names...
   #:defsystem
   #:cl-source-file
   #:system))

(in-package #:cl-def.system)

(defclass readtable-support-mixin ()
  ((setup-readtable-function :initform nil :initarg :setup-readtable-function :accessor setup-readtable-function-of)))

(defclass system (asdf:system readtable-support-mixin)
  ())

(defclass cl-source-file (asdf:cl-source-file readtable-support-mixin)
  ())

(defmethod perform :around ((op operation) (component readtable-support-mixin))
  (let* ((*readtable* *readtable*)
         (system (loop for parent = (component-parent component) :then (component-parent parent)
                       while parent
                       when (typep parent 'system)
                       return parent))
         (setup-readtable-function (or (setup-readtable-function-of component)
                                       (setup-readtable-function-of system))))
    (when setup-readtable-function
      (let ((fn (ignore-errors
                  (fdefinition
                   (read-from-string
                    (string-upcase setup-readtable-function))))))
        (when fn 
          (funcall fn))))
    (call-next-method)))

(defmacro defsystem (name &body rest)
  `(asdf:defsystem ,name
    :default-component-class cl-source-file
    :class system
    ,@rest))

(defsystem :cl-def
  :version "0.1"
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
	   "Levente Mészáros <levente.meszaros@gmail.com>")
  :licence "BSD / Public domain"
  :description "cl-def - (def function ioe name (arg1) ...)"
  :depends-on (:alexandria :iterate :metabang-bind)
  :setup-readtable-function "cl-def::setup-readtable"
  :serial t
  :components
  ((:file "package")
   (:file "duplicates")
   (:file "configuration")
   (:file "def")
   (:file "definers")))

#+nil((defsystem :cl-def.test
  :description "Tests for the cl-def test system."
  :depends-on (:cl-def :stefil)
  :components
  ((:file "tests")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-def))))
  (operate 'load-op :cl-def.test)
  (in-package :cl-def.test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-def.test:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-def))))
  nil))
