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

(defun function-like-definer (-definer- def-macro-name -whole- -environment- -options- &optional (available-flags "ioed"))
  (declare (ignore -environment-))
  (when (and (not (keywordp (first -options-)))
             (not (null -options-)))
    (iter (for flag :in-vector (string-downcase (pop -options-)))
          (if (member flag (coerce available-flags 'list) :test #'char=)
              (ecase flag
                (#\i (push #t -options-)
                     (push :inline -options-))
                (#\o (push #t -options-)
                     (push :optimize -options-))
                (#\e (push #t -options-)
                     (push :export -options-))
                (#\d (push #t -options-)
                     (push :debug -options-)))
              (error "Flag '~A' is not available for definer ~S" flag (name-of -definer-)))))
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

(def definer function ()
  (function-like-definer -definer- 'defun -whole- -environment- -options-))

(def definer method ()
  (function-like-definer -definer- 'defmethod -whole- -environment- -options-))

(def definer macro ()
  (function-like-definer -definer- 'defmacro -whole- -environment- -options- "e"))

(def definer constant ()
  (bind ((name (third -whole-))
         (value (fourth -whole-))
         (documentation (fifth -whole-)))
    `(defconstant ,name
      ,@(when (> (length -whole-) 3)
              (list value))
      ,@(when documentation
              (list documentation)))))

(def definer variable ()
  (bind ((name (third -whole-))
         (value (fourth -whole-))
         (documentation (fifth -whole-)))
    `(defvar ,name
      ,@(when (> (length -whole-) 3)
              (list value))
      ,@(when documentation
              (list documentation)))))
