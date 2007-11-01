;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defun warn-redefining-definer (name)
  (simple-style-warning "Redefining definer ~S" name))

;; TODO this is not thread-safe, but we don't want to depend on bordeaux-threads
(defparameter *definers* (make-hash-table :test 'equal))

(defun find-definer (name &optional (errorp #t))
  (bind (((values definer found) (gethash (symbol-name name) *definers*)))
    (when (and errorp
               (not found))
      (error "No definer for ~S" name))
    (values definer found)))

(defun (setf find-definer) (value name &key (if-exists :warn))
  (check-type if-exists (member :warn :replace))
  (bind ((key (symbol-name name))
         (definer (gethash key *definers*)))
    (when (and value
               definer
               (eq if-exists :warn)
               (not (defined-at-compile-time? definer)))
      (warn-redefining-definer name))
    (if value
        (setf (gethash key *definers*) value)
        (remhash key *definers*))))

(defclass definer ()
  ((name :initarg :name :accessor name-of)
   (expander :initarg :expander :accessor expander-of)
   (documentation :initarg :documentation :accessor documentation-of)
   (available-flags :initform nil :initarg :available-flags :accessor available-flags-of)
   (defined-at-compile-time :initform nil :type boolean :accessor defined-at-compile-time?)))

(defprint-object (self definer :identity #f :type #f)
  (format t "definer ~S" (name-of self)))

(defmethod initialize-instance :after ((self definer) &key &allow-other-keys)
  (setf (available-flags-of self) (coerce (string-downcase (available-flags-of self)) 'list)))

(defun make-definer (name expander &rest initargs)
  (apply #'make-instance 'definer :name name :expander expander initargs))

(defmacro def (&whole whole name &rest rest &environment environment)
  (declare (ignore rest name))
  (bind ((definer (find-definer (parse-definer-name-and-options whole))))
    (funcall (expander-of definer) definer whole environment)))

(defun parse-definer-name-and-options (whole &optional definer)
  (bind ((name-and-options (ensure-list (second whole)))
         (name (first name-and-options))
         (options (rest name-and-options)))
    (when (and definer
               (not (keywordp (first options)))
               (not (null options)))
      (iter (for flag :in-vector (string-downcase (pop options)))
            (if (member flag (available-flags-of definer) :test #'char=)
                (ecase flag
                  (#\i
                   (push #t options)
                   (push :inline options))
                  (#\o
                   (push #t options)
                   (push :optimize options))
                  (#\e
                   (push #t options)
                   (push :export options))
                  (#\d
                   (push #t options)
                   (push :debug options))
                  (#\a
                   (push #t options)
                   (push :export-accessor-names options))
                  (#\s
                   (push #t options)
                   (push :export-slot-names options)))
                (error "Flag '~A' is not available for definer ~S" flag (name-of definer)))))
    (values name options)))

(defmacro with-standard-definer-options (name &body body)
  ``(progn
    ,@(when (getf -options- :inline)
       `((declaim (inline ,,name))))
    ,@(when (getf -options- :export)
       `((export ',,name)))
    ,,@body))

(defmacro with-class-definer-options (name slots &body body)
  ``(progn
    ,@(when (getf -options- :export)
       `((export ',,name)))
    ,@(when (getf -options- :export-slot-names)
       `((export ',(mapcar #'first ,slots))))
    ,@(when (getf -options- :export-accessor-names)
       `((export ',(iter (for slot :in slots)
                         (for slot-options = (rest slot))
                         (awhen (getf slot-options :accessor)
                           (collect it))
                         (awhen (getf slot-options :reader)
                           (collect it))
                         (awhen (getf slot-options :writer)
                           (collect it))))))
    ,,@body))

(bind ((definer-definer (make-definer 'definer nil :available-flags "e")))
  (setf (expander-of definer-definer)
        (lambda (-definer- -whole- -environment-)
          (declare (ignorable -definer- -environment-))
          (setf -whole- (copy-seq -whole-))
          (bind (((name-and-options args &rest body) (nthcdr 2 -whole-))
                 ((values nil options) (parse-definer-name-and-options -whole- definer-definer))
                 ((values body declarations doc-string) (parse-body body :documentation #t :whole -whole-)))
            (setf name-and-options (ensure-list name-and-options))
            (with-unique-names (name)
              `(progn
                (eval-when (:compile-toplevel :load-toplevel)
                  (bind ((,name ',(first name-and-options)))
                    ;;(break "~S" expander-forms)
                    ,@(when (getf options :export)
                            (remove-from-plistf options :export)
                            `((export ,name)))
                    (setf (find-definer ,name)
                          (make-definer ,name
                                        (lambda (-definer- -whole- -environment-)
                                          (declare (ignorable -definer- -environment-))
                                          (bind ((-options- (nth-value 1 (parse-definer-name-and-options
                                                                          -whole- -definer-)))
                                                 ,@(when args
                                                         `((,(substitute '&rest '&body args) (nthcdr 2 -whole-)))))
                                            (declare (ignorable -options-))
                                            ,@declarations
                                            ,@body))
                                        :documentation ,doc-string
                                        ,@options))))
                (eval-when (:compile-toplevel)
                  (setf (defined-at-compile-time? (find-definer ',(first name-and-options))) t)))))))
  (setf (find-definer 'definer) definer-definer))

