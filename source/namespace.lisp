;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer e :available-flags "e") namespace (name &optional definer-args &body definer-forms)
  (bind ((hashtable-var (symbolicate "*" name '#:-namespace*))
         (lock-var (symbolicate "%" name '#:-namespace-lock%))
         (finder-name (getf -options- :finder-name (symbolicate '#:find- name)))
         (collector-name (symbolicate '#:collect- name '#:-namespace-values))
         (iterator-name (symbolicate '#:iterate- name '#:-namespace))
         (test-function (getf -options- :test '#'eq))
         (weakness (getf -options- :weakness)))
    (remove-from-plistf -options- :test :weakness :finder-name)
    `(progn
       ,@(when (getf -options- :export)
           `((export '(,finder-name ,collector-name ,iterator-name))))
       (def global ,hashtable-var (trivial-garbage:make-weak-hash-table :test ,test-function :weakness ,weakness))
       (def global ,lock-var (bordeaux-threads:make-recursive-lock ,(concatenate 'string "lock for " (string hashtable-var))))
       (def function ,finder-name (name &key (otherwise nil otherwise?))
         (bordeaux-threads:with-recursive-lock-held (,lock-var)
           (or (gethash name ,hashtable-var)
               (if otherwise?
                   (hu.dwim.util:handle-otherwise otherwise)
                   (error "Cannot find ~A in namespace ~A" name ',name)))))
       (def function (setf ,finder-name) (value name)
         (bordeaux-threads:with-recursive-lock-held (,lock-var)
           (if value
               (setf (gethash name ,hashtable-var) value)
               (remhash name ,hashtable-var))))
       (def function ,collector-name ()
         (bordeaux-threads:with-recursive-lock-held (,lock-var)
           (hash-table-values ,hashtable-var)))
       (def function ,iterator-name (visitor)
         (bordeaux-threads:with-recursive-lock-held (,lock-var)
           (maphash visitor ,hashtable-var)))
       ,@(unless (and (zerop (length definer-args))
                      (zerop (length definer-forms)))
          `((def (definer ,@-options-) ,name (name ,@definer-args)
              `(setf (,',finder-name ',name) ,,@definer-forms)))))))
