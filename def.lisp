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
(defparameter *definers* (make-hash-table :test 'equal))

(defun find-definer (name &optional (errorp #t))
  (bind (((values definer found) (gethash (symbol-name name) *definers*)))
    (when (and errorp
               (not found))
      (error "No definer for ~S" name))
    (values definer found)))

(defun (setf find-definer) (value name)
  (bind ((key (symbol-name name))
         (definer (gethash key *definers*)))
    (when definer
      (simple-style-warning "Redefining definer ~S" name))
    (if value
        (setf (gethash key *definers*) value)
        (remhash key *definers*))))

(defclass definer ()
  ((name :initarg :name :accessor name-of)
   (expander :initarg :expander :accessor expander-of)))

(defprint-object (self definer :identity #f :type #f)
  (format t "definer ~S" (name-of self)))

(defun make-definer (name expander)
  (make-instance 'definer :name name :expander expander))

(defmacro def (&whole whole name &rest rest &environment environment)
  (declare (ignore rest name))
  (funcall (expander-of (find-definer (parse-definer-name-and-options whole))) whole environment))

(defun parse-definer-name-and-options (whole)
  (bind ((name-and-options (ensure-list (second whole))))
    (values (first name-and-options) (rest name-and-options))))

(setf (find-definer 'definer)
      (make-definer 'definer
                    (lambda (-whole- -environment-)
                      (declare (ignorable -environment-))
                      (setf -whole- (copy-seq -whole-))
                      (bind (((name-and-options args &rest body) (nthcdr 2 -whole-)))
                        (setf name-and-options (ensure-list name-and-options))
                        (with-unique-names (name)
                          `(eval-when (:compile-toplevel :load-toplevel)
                            (bind ((,name ',(first name-and-options)))
                              ;;(break "~S" expander-forms)
                              (setf (find-definer ,name)
                                    (make-definer ,name (lambda (-whole- -environment-)
                                                          (declare (ignorable -environment-))
                                                          (bind ((-options- (nth-value 1 (parse-definer-name-and-options -whole-)))
                                                                 ,@(when args
                                                                         `(,args (nthcdr 2 -whole-))))
                                                            (declare (ignorable -options-))
                                                            ,@body)))))))))))

