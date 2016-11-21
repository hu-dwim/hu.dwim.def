;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;;;;;
;;; define a (def package ...) that can store custom package options into the namespace called extended-package

(def (namespace e :test 'equal) extended-package)

(def special-variable *extended-package-definition-hooks* nil)

(def function call-extended-package-definition-hooks (extended-package)
  (dolist (hook *extended-package-definition-hooks*)
    (funcall hook extended-package)))

(def class extended-package ()
  ((name :initarg :name :accessor name-of)
   (standard-options :initarg :standard-options :accessor standard-options-of)
   (extended-options :initarg :extended-options :accessor extended-options-of)
   (readtable-setup-form :initform nil :initarg :readtable-setup-form :accessor readtable-setup-form-of)))

(def print-object extended-package
  (write (name-of -self-)))

(def method (setf readtable-setup-form-of) :after (new-value (extended-package extended-package))
  (declare (ignore new-value))
  (call-extended-package-definition-hooks extended-package))

(defun ensure-global-package-nickname (nickname package)
  (let ((nickname (string nickname))
        (nicknames (package-nicknames package)))
    (unless (member nickname nicknames :test 'equal)
      (pushnew nickname nicknames)
      (rename-package package package nicknames))
    nicknames))

(defun supports-local-package-nicknames? ()
  (featurep :sbcl))

(def function %define-extended-package (name readtable-setup-form standard-options extended-options)
  (check-type name string)
  #+nil ; TODO this is broken because of the usual compile-time/run-time redefinition...
  (when (find-extended-package name :otherwise nil)
    (simple-style-warning "Redefining extended package ~S" name))
  (bind ((extended-package (make-instance 'extended-package
                                          :name name
                                          :readtable-setup-form readtable-setup-form
                                          :standard-options standard-options
                                          :extended-options extended-options)))
    (setf (find-extended-package name) extended-package)
    (when (not (supports-local-package-nicknames?))
      (let ((nicknames (getf extended-options :local-nicknames)))
        (warn "Local package nicknames are not supported on your lisp. The requested nicknames of package ~S (~S) will be installed globally which may lead to name clashes."
              name nicknames)
        (loop
          :for (nickname target-package) :in nicknames
          :do (progn
                (format t ";;; Installing global package nickname ~S on package ~S~%" nickname target-package)
                (ensure-global-package-nickname nickname target-package)))))
    (call-extended-package-definition-hooks extended-package)))

(def (function e) setup-readtable/same-as-package (package-name)
  (bind (#+sbcl (sb-ext:*evaluator-mode* :interpret))
    (eval
     (readtable-setup-form-of
      (find-extended-package (string package-name)
                             :otherwise `(:error "SETUP-READTABLE-AS-PACKAGE: Could not find package ~S" ,package-name))))))

(def (definer e :available-flags "e") package (name &body options)
  (bind ((standard-options nil)
         (extended-options nil)
         (readtable-setup-form nil))
    (loop
      :for option :in options
      :if (member (first option) '(:nicknames :documentation :use :shadow :shadowing-import-from :import-from :export :intern :size))
        :collect option :into standard
      :else
        :collect option :into extended
      :finally (progn
                 (setf standard-options standard)
                 (setf extended-options extended)))
    (awhen (assoc :readtable-setup extended-options)
      (setf readtable-setup-form `(progn
                                    ,@(rest it)))
      (removef extended-options :readtable-setup :key #'first))
    (with-standard-definer-options name
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (%define-extended-package
            ,(string name)
            ',readtable-setup-form
            ',standard-options
            (list ,@(loop
                      :for (option . args) :in extended-options
                      :appending (list* `(quote ,option) (case option
                                                           (:local-nicknames
                                                            (unless (every (curry 'length= 2) args)
                                                              (error "Malformed :local-nicknames entry in the definition of package ~/hu.dwim.def::print-symbol-with-prefix/: ~S"
                                                                     name args))
                                                            (list (list 'quote args)))
                                                           (t args)))))))
         (defpackage ,name ,@(append standard-options
                                     (awhen (and (supports-local-package-nicknames?)
                                                 (assoc :local-nicknames extended-options))
                                       (list (list* :local-nicknames (cdr it))))))))))
