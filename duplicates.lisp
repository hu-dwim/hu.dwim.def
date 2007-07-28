;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

;; dwim utils
(defun setup-swank-readtable-alist (&rest package-name/readtable-setup-function-pairs)
  (loop for (package-names setup-function) :on package-name/readtable-setup-function-pairs :by #'cddr do
        (bind ((*readtable* (copy-readtable)))
          (funcall setup-function)
          (dolist (package-name (ensure-list package-names))
            (setf package-name (string package-name))
            (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
              (unless entry
                (setf entry (cons package-name nil))
                (push entry swank:*readtable-alist*))
              (setf (cdr entry) *readtable*))))))

(defmacro enable-sharp-boolean-syntax ()
  "Copies *readtable* and enables #t and #f readers for t and nil in the copy."
  '(eval-when (:compile-toplevel :execute)
    (setf *readtable* (copy-readtable *readtable*))
    (%enable-sharp-boolean-syntax)))

(defun %enable-sharp-boolean-syntax ()
  (set-dispatch-macro-character
   #\# #\t
   (lambda (s c n)
     (declare (ignore s c n))
     t))
  (set-dispatch-macro-character
   #\# #\f
   (lambda (s c n)
     (declare (ignore s c n))
     nil)))

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
