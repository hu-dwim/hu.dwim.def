;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-user)

;;; try to load asdf-system-connections
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (asdf:find-system :asdf-system-connections nil)
    (when (find-package :asdf-install)
      (eval (read-from-string "(asdf-install:install '#:asdf-system-connections)")))
    (unless (asdf:find-system :asdf-system-connections nil)
      (error "The cl-def system requires asdf-system-connections. See http://www.cliki.net/asdf-system-connections for details and download instructions.")))
  (asdf:operate 'asdf:load-op :asdf-system-connections))

(defpackage #:cl-def.system
    (:use :cl :asdf :asdf-system-connections)

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

(defsystem-connection cl-def-and-stefil
  :requires (:cl-def :stefil)
  :components ((:module :integration
                        :components ((:file "stefil")))))

(defsystem-connection cl-def-and-slime
  :requires (:cl-def :swank)
  :components ((:module :integration
                        :components ((:file "slime")))))

(defsystem :cl-def-test
  :description "Tests for the cl-def test system."
  :depends-on (:cl-def :stefil)
  :components
  ((:file "test")))

(defmethod perform ((op test-op) (system (eql (find-system :cl-def))))
  (operate 'load-op :cl-def-test)
  (in-package :cl-def-test)
  (declaim (optimize (debug 3)))
  (warn "(declaim (optimize (debug 3))) was issued to help later C-c C-c'ing")
  (eval (read-from-string "(stefil:funcall-test-with-feedback-message 'cl-def-test:test)"))
  (values))

(defmethod operation-done-p ((op test-op) (system (eql (find-system :cl-def))))
  nil)
