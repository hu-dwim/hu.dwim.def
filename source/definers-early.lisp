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
      (bind ((debug-level (getf options :debug (max #+sbcl (sb-c::policy-quality sb-c::*policy* 'debug)
                                                    1))))
        (list* :debug debug-level
               (remove-from-plist options :inline :optimize :debug)))))

(defun function-like-definer-declarations (-options-)
  (bind ((debug-level (getf -options- :debug)))
    (when (eq debug-level t)
      (setf debug-level 3))
    (if debug-level
        (progn
          (when (getf -options- :optimize)
            (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified"))
          `((declare (optimize (speed 0) (debug ,debug-level)))))
        (when (getf -options- :optimize)
          '((declare (optimize (speed 3) (debug 0) (safety 2))))))))

(defun %function-like-definer (definer-macro-name &key whole options allow-compound-name)
  (bind ((body (nthcdr 2 whole))
         (name (pop body))
         (args (pop body)))
    (when (and allow-compound-name
               (consp name))
      (setf name (first name)))
    (awhen (find-function-definer-option-transformer name)
      (setf options (funcall it options)))
    (bind (((:values body declarations documentation) (parse-body body :documentation t :whole whole))
           (outer-declarations (function-like-definer-declarations options))
           ;; KLUDGE https://bugs.launchpad.net/sbcl/+bug/562911
           (process-inline? (not (eq definer-macro-name 'defmacro))))
      `(progn
         ,@(when (and process-inline?
                      (getf options :inline))
                 `((declaim (inline ,name))))
         ,@(when (and process-inline?
                      (getf options :debug))
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

(defmacro function-like-definer (definer-macro-name &key allow-compound-name)
  `(%function-like-definer ',definer-macro-name :whole -whole- :options -options-
                           :allow-compound-name ,allow-compound-name))

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
