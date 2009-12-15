;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer e) iterator (&whole whole name args &body body)
  (bind ((iterator-name (symbolicate '#:iterate- name))
         (do-name (symbolicate '#:do- name))
         (do/body-name (symbolicate '#:do- name '#:/body))
         ((:values body declarations documentation) (parse-body body :documentation t :whole whole))
         ((:values macro-args funcall-list rest-variable-name) (compute-arguments-for-function-bridge-macro args)))
    (with-unique-names (element-var)
      `(progn
         ,@(when (getf -options- :export)
             `((export '(,iterator-name ,do-name))))
         (def function ,iterator-name (-visitor- ,@args)
           ,@(when documentation
                   (list documentation))
           ,@declarations
           ,@body)
         (def macro ,do-name ((,element-var ,@(if rest-variable-name
                                              `(&rest ,rest-variable-name)
                                              macro-args))
                               &body body)
           `(block nil
              (flet ((,',do/body-name (,,element-var)
                       ,@body))
                ,,(if rest-variable-name
                      ``(apply ',',iterator-name #',',do/body-name ,rest-variable-name)
                      ``(,',iterator-name #',',do/body-name ,@,@funcall-list)))))))))
