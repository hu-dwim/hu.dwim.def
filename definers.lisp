;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(defun function-definer-option-transformer (name)
  (awhen (find-symbol "TRANSFORM-FUNCTION-DEFINER-OPTIONS"
                      (symbol-package (if (consp name)
                                          (second name)
                                          name)))
    (fdefinition it)))

(defun function-like-definer (-definer- def-macro-name -whole- -environment- -options-)
  (declare (ignore -environment- -definer-))
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (args (pop body)))
    (awhen (function-definer-option-transformer name)
      (setf -options- (funcall it -options-)))
    (flet ((get-option (option)
             (getf -options- option)))
      (bind (((values body declarations documentation) (parse-body body :documentation #t :whole -whole-)))
        (if (get-option :debug)
            (progn
              (push '(declare (optimize (speed 0) (debug 3))) declarations)
              (when (get-option #\o)
                (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified")))
            (when (get-option :optimize)
              (push '(declare (optimize (speed 3) (debug 0))) declarations)))

        `(progn
          ,@(when (get-option :inline)
                  `((declaim (inline ,name))))
          ,@(when (get-option :export)
                  `((export ',name)))
          (,def-macro-name ,name ,args
            ,@(when documentation
                    (list documentation))
            ,@declarations
            ,@body))))))

(def (definer e :available-flags "ioed") function ()
  (function-like-definer -definer- 'defun -whole- -environment- -options-))

(def (definer e :available-flags "eod") method ()
  (function-like-definer -definer- 'defmethod -whole- -environment- -options-))

(def (definer e :available-flags "eod") macro ()
  (function-like-definer -definer- 'defmacro -whole- -environment- -options-))

(defun extract-function-name (spec)
  "Useful for macros that want to emulate the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

(def (definer e :available-flags "e") constant (name initial-value &optional documentation)
  "Use like: (def (constant e :test #'string=) alma \"korte\")"
  (check-type name symbol)
  (bind ((test (getf -options- :test 'eq)))
    (setf test (extract-function-name test))
    (with-standard-definer-options name
      `(defconstant ,name
        (let ((new ,initial-value))
          (if (boundp ',name)
              (let ((old (symbol-value ',name)))
                (cond
                  ((constantp ',name)
                   (cond
                     ((,test old new)
                      old)
                     (t
                      (cerror "Try to redefine the constant."
                              "~@<~S is an already defined constant whose value ~
                               ~S is not equal to the provided initial value ~S ~
                               under ~S.~:@>" ',name old new ',test)
                      new)))
                  (t
                   (cerror "Try to redefine the variable as a constant."
                           "~@<~S is an already bound non-constant variable ~
                            whose value is ~S.~:@>" ',name old)
                   new)))
              new))
        ,@(when documentation
                (list documentation))))))

(def (definer e :available-flags "e") variable (name &optional value documentation)
  (with-standard-definer-options name
    `(defvar ,name
      ,@(when (> (length -whole-) 3)
              (list value))
      ,@(when documentation
              (list documentation)))))

(def (definer e) constructor (class-name* &body body)
  (let ((key-args (when (listp class-name*)
                    (rest class-name*)))
        (class-name (if (listp class-name*)
                        (first class-name*)
                        class-name*)))
    `(defmethod initialize-instance :after ((,(intern "SELF") ,class-name) &key ,@key-args &allow-other-keys)
      ,@body)))

(def (definer e) print-object (class-name* &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT.
  An example:
  (def print-object parenscript-dispatcher ; could be (parenscript-dispatcher :identity nil)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (bind ((args (ensure-list class-name*))
           ((class-name &key (identity t) (type t) with-package (muffle-errors t)) args))
      `(defmethod print-object ((,(intern "SELF") ,class-name) ,stream)
        (print-unreadable-object (,(intern "SELF") ,stream :type ,type :identity ,identity)
          (let ((*standard-output* ,stream))
            (block ,printing
              (,@(if muffle-errors
                     `(handler-bind ((error (lambda (error)
                                              (declare (ignore error))
                                              (write-string "<<error printing object>>")
                                              (return-from ,printing)))))
                     `(progn))
                 (let (,@(when with-package `((*package* ,(find-package with-package)))))
                   ,@body)))))))))
