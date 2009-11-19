;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (function e) register-readtable-for-swank (package-names readtable)
  (dolist (package-name (ensure-list package-names))
    (setf package-name (string package-name))
    (let ((entry (find package-name swank:*readtable-alist* :test #'string= :key #'car)))
      (unless entry
        (setf entry (cons package-name nil))
        (push entry swank:*readtable-alist*))
      (setf (cdr entry) readtable))))

(def function definer-lookup-hook (form)
  (when (typep form 'definer-name)
    (awhen (find-definer form nil)
      (values it t))))

(awhen (find-symbol (symbol-name '#:*inspector-dwim-lookup-hooks*) :swank)
  (pushnew 'definer-lookup-hook (symbol-value it)))

(def function notify-swank-about-package-readtable (extended-package)
  (awhen (readtable-setup-form-of extended-package)
    (register-readtable-for-swank (name-of extended-package)
                                  (bind ((*readtable* (copy-readtable *readtable*)))
                                    (eval it)
                                    *readtable*))))

(pushnew 'notify-swank-about-package-readtable *extended-package-definition-hooks*)

;; notify swank about the readtable of the already defined extended-packages
(iterate-extended-package-namespace
 (lambda (name extended-package)
   (declare (ignore name))
   (notify-swank-about-package-readtable extended-package)))
