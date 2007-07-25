;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2007 by the authors.
;;;
;;; See LICENCE for details.

(in-package :cl-def)

(def definer function ()
  (bind ((body (nthcdr 2 -whole-)))
    (bind ((name (pop body))
           flags
           args)
      (setf flags (pop body))
      (if (consp flags)
          (setf args flags
                flags nil)
          (setf args (pop body)))
      (setf flags (string-downcase flags))
      (flet ((has-flag (flag)
               (find flag flags :test #'char=)))
        (bind (((values body declarations documentation) (parse-body body :documentation #t :whole -whole-)))
          (if (has-flag #\d)
              (progn
                (push '(declare (optimize (speed 0) (debug 3))) declarations)
                (when (has-flag #\o)
                  (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified")))
              (when (has-flag #\o)
                (push '(declare (optimize (speed 3) (debug 0))) declarations)))
          `(progn
            ,(if (has-flag #\i)
                 `(declaim (inline ,name))
                 (values))
            ,(if (has-flag #\e)
                 `(export ',name)
                 (values))
            (defun ,name ,args
              ,documentation
              ,@declarations
              ,@body)))))))

#|

(def function foo ioed (bar baz xxx)
  "doc"
  (declare (ignore xxx))
  (+ bar baz))

||#
