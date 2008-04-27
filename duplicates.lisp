;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

(defmacro if-bind (var test &body then/else)
  (assert (first then/else)
          (then/else)
          "IF-BIND missing THEN clause.")
  (destructuring-bind (then &optional else)
      then/else
    `(let ((,var ,test))
       (if ,var ,then ,else))))

(defmacro aif (test then &optional else)
  `(if-bind it ,test ,then ,else))

(defmacro when-bind (var test &body body)
  `(if-bind ,var ,test (progn ,@body)))

(defmacro awhen (test &body body)
  `(when-bind it ,test ,@body))

(defmacro prog1-bind (var ret &body body)
  `(let ((,var ,ret))
    ,@body
    ,var))

(defmacro aprog1 (ret &body body)
  `(prog1-bind it ,ret ,@body))


(defmacro eval-always (&body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
    ,@body))


(defun concatenate-symbol (&rest args)
  "A DWIM symbol concatenate: Args will be converted to string and be concatenated
to form the resulting symbol with one exception: when a package is encountered then
it is stored as the target package to use at intern. If there was no package
among the args then the symbol-package of the first symbol encountered will be
used. If there are neither packages nor symbols among the args then the result will
be interned into the current package at the time of calling."
  (let* ((package nil)
         (symbol-name (string-upcase
                       (with-output-to-string (str)
                                              (dolist (arg args)
                                                (typecase arg
                                                  (string (write-string arg str))
                                                  (package (setf package arg))
                                                  (symbol (unless package
                                                            (setf package (symbol-package arg)))
                                                          (write-string (symbol-name arg) str))
                                                  (integer (write-string (princ-to-string arg) str))
                                                  (character (write-char arg) str)
                                                  (t (error "Cannot convert argument ~S to symbol" arg))))))))
    (if package
        (intern symbol-name package)
        (intern symbol-name))))

;; from arnesi
(defmacro defprint-object ((self class-name &key (identity t) (type t) with-package)
                           &body body)
  "Define a print-object method using print-unreadable-object.
  An example:
  (defprint-object (self parenscript-dispatcher)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream)
    `(defmethod print-object ((,self ,class-name) ,stream)
      (print-unreadable-object (,self ,stream :type ,type :identity ,identity)
        (let ((*standard-output* ,stream)
              ,@(when with-package `((*package* ,(find-package with-package)))))
          ,@body)))))

(defmacro rebind (bindings &body body)
  `(let ,(loop
            for symbol-name in bindings
            collect (list symbol-name symbol-name))
     ,@body))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; lambda list processing from stefil

(defmacro with-lambda-parsing ((lambda-form &key finally) &body body)
  (with-unique-names (cell)
    `(iter
      (with -in-keywords- = #f)
      (with -in-optionals- = #f)
      (with -rest-variable-name- = nil)
      (for ,cell :first ,lambda-form :then (cdr ,cell))
      (while ,cell)
      (for -variable-name- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (first (ensure-list (car ,cell)))
                                 (car ,cell)))
      (for -default-value- = (if (or -in-optionals-
                                     -in-keywords-)
                                 (second (ensure-list (car ,cell)))
                                 (car ,cell)))
      (case -variable-name-
        (&optional (setf -in-optionals- #t))
        (&key (setf -in-keywords- #t)
              (setf -in-optionals- #f))
        (&allow-other-keys)
        (&rest (setf -rest-variable-name- (car (cdr ,cell)))
               (setf ,cell (cdr ,cell)))
        (t ,@body))
      (finally ,@finally))))

(defun lambda-list-to-funcall-list (args)
  (with-lambda-parsing (args :finally ((return (values result -rest-variable-name-))))
    (if -in-keywords-
        (progn
          (collect (intern (symbol-name (first (ensure-list -variable-name-)))
                           #.(find-package "KEYWORD")) :into result)
          (collect -variable-name- :into result))
        (collect -variable-name- :into result))))

;; from dwim-utils
(defun tree-substitute (new old list
                        &key from-end (test #'eql) (test-not nil)
                        (end nil) (count nil) (key nil) (start 0))
  "Starting from LIST non-destructively replaces OLD with NEW."
  (if (consp list)
      (prog1-bind result
          (setf list (substitute new old list :from-end from-end :test test :test-not test-not
                                 :end end :count count :key key :start start))
        (iter (for node :first result :then (cdr node))
              (until (null node))
              (for el = (car node))
              (setf (car node) (tree-substitute new old el :from-end from-end :test test :test-not test-not
                                                :end end :count count :key key :start start))))
      (if (funcall test list old)
          new
          list)))

(defun integrated-export (symbol other-package)
  "Export SYMBOL from both its own package and OTHER-PACKAGE"
  (export symbol (symbol-package symbol))
  (shadowing-import symbol other-package)
  (export symbol other-package))
