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

(def definer function ()
   (unless (keywordp (first -options-))
     (iter (for flag :in-vector (string-downcase (pop -options-)))
           (ecase flag
             (#\i (push #t -options-)
                  (push :inline -options-))
             (#\o (push #t -options-)
                  (push :optimize -options-))
             (#\e (push #t -options-)
                  (push :export -options-))
             (#\d (push #t -options-)
                  (push :debug -options-)))))
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
           ,(if (get-option :inline)
                `(declaim (inline ,name))
                (values))
           ,(if (get-option :export)
                `(export ',name)
                (values))
           (defun ,name ,args
             ,documentation
             ,@declarations
             ,@body))))))

#|

(defun transform-function-definer-options (options)
  (append (list :inline #t) options))

(def (function ioed :arg 42) foo (bar baz xxx)
  "doc"
  (declare (ignore xxx))
  (+ bar baz))

||#
