;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(define-condition simple-style-warning (style-warning simple-warning)
  ())

(defun simple-style-warning (message &rest args)
  (warn 'simple-style-warning :format-control message :format-arguments args))

;; TODO this is not thread-safe, but we don't want to depend on bordeaux-threads
(defparameter *definers* (make-hash-table :test 'eql))

(defun find-definer (name &optional (errorp #t))
  (bind (((values definer found) (gethash name *definers*)))
    (when (and errorp
               (not found))
      (error "No definer for ~S" name))
    (values definer found)))

(defun (setf find-definer) (value name)
  (bind ((definer (gethash name *definers*)))
    (when definer
      (simple-style-warning "Redefining definer ~S" name))
    (if value
        (setf (gethash name *definers*) value)
        (remhash name *definers*))))

(defclass definer ()
  ((name :initarg :name :accessor name-of)
   (expander :initarg :expander :accessor expander-of)))

(defprint-object (self definer :identity #f :type #f)
  (format t "definer ~S" (name-of self)))

(defun make-definer (name expander)
  (make-instance 'definer :name name :expander expander))

(defmacro def (&whole whole name &rest rest &environment environment)
  (declare (ignore rest))
  (funcall (expander-of (find-definer name)) whole environment))

(setf (find-definer 'definer)
      (make-definer 'definer
                    (lambda (-whole- -environment-)
                      (declare (ignorable -environment-))
                      (setf -whole- (copy-seq -whole-))
                      (bind (((name args &rest body) (nthcdr 2 -whole-))
                             (expander-forms `(lambda (-whole- -environment-)
                                               (declare (ignorable -environment-))
                                               (bind (,@(when args
                                                          `(,args (nthcdr 2 -whole-))))
                                                 ,@body))))
                        ;;(break "~S" expander-forms)
                        (setf (find-definer name)
                              (make-definer name (compile nil expander-forms)))
                        name))))

