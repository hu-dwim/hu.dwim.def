;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

;;; THE CONTENT OF THIS FILE IS COPIED OVER FROM SOME OTHER LIBRARIES TO DECREASE DEPENDENCIES

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

(defun integrated-export (symbol other-package)
  "Export SYMBOL from both its own package and OTHER-PACKAGE"
  (dolist (symbol (ensure-list symbol))
    (export symbol (symbol-package symbol))
    (shadowing-import symbol other-package)
    (export symbol other-package)))

;;;;;;
;;; lambda list processing from stefil

(defun lambda-list-to-funcall-list (args)
  (bind (((:values requireds optionals rest keywords) (parse-ordinary-lambda-list args)))
    (values (append requireds
                    (loop
                      :for entry :in optionals
                      :collect (first entry))
                    (unless rest
                      (loop
                        :for entry :in keywords
                        :appending (list (first (first entry)) (second (first entry))))))
            rest)))

(defun lambda-list-to-lambda-list-with-quoted-defaults (args)
  (bind (((:values requireds optionals rest keywords allow-other-keys?) (parse-ordinary-lambda-list args)))
    (values `(,@requireds
              ,@(when optionals (cons '&optional optionals))
              ,@(if rest
                    `(&rest ,rest)
                    (when keywords (cons '&key keywords)))
              ,@(when (and allow-other-keys? (not rest))
                  (list '&allow-other-keys)))
            rest)))
