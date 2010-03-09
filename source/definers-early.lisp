;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(defun find-function-definer-option-transformer (name)
  (bind ((name (find-symbol "TRANSFORM-FUNCTION-DEFINER-OPTIONS"
                            (symbol-package (if (consp name)
                                                (second name)
                                                name)))))
    (if (and name
             (fboundp name))
        (fdefinition name)
        #'transform-function-definer-options)))

(defun transform-function-definer-options (options)
  (if *load-as-production?*
      options
      (remove-from-plist options :inline :optimize)))

(defun function-like-definer-declarations (-options-)
  (if (getf -options- :debug)
      (progn
        (when (getf -options- #\o)
          (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified"))
        '((declare (optimize (speed 0) (debug 3)))))
      (when (getf -options- :optimize)
        '((declare (optimize (speed 3) (debug 0) (safety 2)))))))

(defun %function-like-definer (definer-macro-name &key whole options)
  (bind ((body (nthcdr 2 whole))
         (name (pop body))
         (args (pop body)))
    (awhen (find-function-definer-option-transformer name)
      (setf options (funcall it options)))
    (bind (((:values body declarations documentation) (parse-body body :documentation t :whole whole))
           (outer-declarations (function-like-definer-declarations options)))
      `(progn
         ,@(when (getf options :inline)
                 `((declaim (inline ,name))))
         ,@(when (getf options :debug)
                 `((declaim (notinline ,name))))
         (locally
             ,@outer-declarations
           ,@(when (getf options :export)
                   `((eval-when (:compile-toplevel :load-toplevel :execute)
                       (export ',name))))
           (,definer-macro-name ,name ,args
             ,@(when documentation
                     (list documentation))
             ,@declarations
             ,@body))))))

(defmacro function-like-definer (definer-macro-name)
  `(%function-like-definer ',definer-macro-name :whole -whole- :options -options-))

(defun %defmethods-like-definer (definer-macro-name -whole- -options-)
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         ,@outer-declarations
       ,@(when (getf -options- :export)
               `((export ',name)))
       ,@(iter (for entry :in body)
               (when (eq (first entry) :method)
                 (pop entry))
               (collect `(,definer-macro-name ,name ,@entry))))))

(defmacro defmethods-like-definer (definer-macro-name)
  `(%defmethods-like-definer ',definer-macro-name -whole- -options-))
