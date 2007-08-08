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

(def (definer e :available-flags "e") constant (name value &optional documentation)
  `(defconstant ,name
    ,@(when (> (length -whole-) 3)
            (list value))
    ,@(when documentation
            (list documentation))))

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
