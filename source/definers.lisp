;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer e :available-flags "ioed") function ()
  (function-like-definer defun))

(def (definer e :available-flags "eod") method ()
  (function-like-definer defmethod))

(def (definer e :available-flags "eod") methods ()
  (defmethods-like-definer defmethod))

(def (definer e :available-flags "eod") macro ()
  (function-like-definer defmacro))

(def (definer e :available-flags "eod") compiler-macro ()
  (function-like-definer define-compiler-macro))

(def (definer e :available-flags "e") symbol-macro (name expansion &optional documentation)
  (check-type name symbol)
  (with-standard-definer-options name
    `(progn
       (define-symbol-macro ,name ,expansion)
       (setf (documentation ',name 'variable) ,documentation))))

(def (definer e :available-flags "eod") generic ()
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (outer-declarations (function-like-definer-declarations -options-)))
    `(locally
         ,@outer-declarations
       ,@(when (getf -options- :export)
               `((export ',name)))
       (defgeneric ,name ,@body))))

(def (definer :available-flags "e") type (name args &body forms)
  (with-standard-definer-options name
    `(deftype ,name ,args
       ,@forms)))

(def (definer e :available-flags "e") member-type (name &body values)
  `(def (type ,@-options-) ,name ()
     ,(if (length= values 1)
          ``(eql ,@',values)
          ``(member ,@',values))))

(def macro with-class-definer-options (name slots &body body)
  ``(progn
    ,@(when (getf -options- :export)
       `((export ',,name)))
    ,@(awhen (and (getf -options- :export-slot-names)
                  (mapcar (lambda (slot)
                            (first (ensure-list slot)))
                          ,slots))
       `((export ',it)))
    ,@(awhen (and (getf -options- :export-accessor-names)
                  (iter (for slot :in slots)
                        (setf slot (ensure-list slot))
                        (for slot-options = (rest slot))
                        (awhen (getf slot-options :accessor)
                          (collect it))
                        (awhen (getf slot-options :reader)
                          (collect it))
                        (awhen (getf slot-options :writer)
                          (collect it))))
       `((export ',it)))
    ,,@body))

(def (definer :available-flags "eas") class (name supers slots &rest options)
  "Example that exports all the class name and all the readers, writers and slot names:
    (def (class eas) foo \(bar baz)
     \(\(slot1 :reader readerr)
      \(slot2 :writer writerr :accessor accessorr))
     \(:metaclass fofofo))"
  (with-class-definer-options name slots
    `(defclass ,name ,supers
       ,slots
       ,@options)))

(def (definer :available-flags "eas") condition (name supers slots &rest options)
  "See the CLASS definer."
  (with-class-definer-options name slots
    `(define-condition ,name ,supers
       ,slots
       ,@options)))

(def (definer :available-flags "eas") structure (name &body slots)
  (bind ((original-name name)
         (name (first (ensure-list original-name)))
         (options (rest (ensure-list original-name)))
         (documentation (when (stringp (first slots))
                          (pop slots)))
         (slot-names (mapcar (lambda (slot)
                               (first (ensure-list slot)))
                             slots))
         (export? (getf -options- :export)))
    (when (and documentation (getf -options- :documentation))
      (error "Multiple documentations for ~S" -whole-))
    (setf documentation (or documentation (getf -options- :documentation)))
    `(progn
       ,@(when export?
           `((export '(,name))))
       ,@(when (and export?
                    (getf -options- :export-constructor t))
           (bind ((constructor (aif (assoc-value options :constructor)
                                    (first it)
                                    (symbolicate '#:make- name))))
             `((export '(,constructor)))))
       ,@(when (getf -options- :export-slot-names)
           `((export ',slot-names)))
       ,@(when (getf -options- :export-accessor-names)
           (bind ((accessor-prefix (aif (assoc-value options :conc-name)
                                        (first it)
                                        (concatenate 'string (string name) "-"))))
             `((export ',(mapcar (curry #'symbolicate accessor-prefix)
                                 slot-names)))))
       ,@(when documentation
           `((setf (documentation ',name 'structure) ,documentation)))
       (defstruct ,original-name
         ,@slots))))

(def function %reevaluate-constant (name value &key (test 'eql))
  (if (not (boundp name))
      value
      (let ((old (symbol-value name))
            (new value))
        (if (not (constantp name))
            (prog1 new
              (cerror "Try to redefine the variable as a constant."
                      "~@<~S is an already bound non-constant variable ~
                       whose value is ~S.~:@>" name old))
            (if (funcall test old new)
                old
                (prog1 new
                  (cerror "Try to redefine the constant."
                          "~@<~S is an already defined constant whose value ~
                           ~S is not equal to the provided initial value ~S ~
                           under ~S.~:@>" name old new test)))))))

(def (definer e :available-flags "e") constant (name initial-value &optional documentation)
  "Use like: (def (constant e :test #'string=) alma \"korte\") test defaults to equal."
  (check-type name symbol)
  (bind ((test (getf -options- :test ''equal)))
    (with-standard-definer-options name
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (defconstant ,name (%reevaluate-constant ',name ,initial-value :test ,test)
           ,@(when documentation `(,documentation)))))))

(def (definer e :available-flags "e") load-time-constant (name initial-value &optional documentation)
  (check-type name symbol)
  (bind ((variable-name (format-symbol *package* "%%%~A" name)))
    (with-standard-definer-options name
      `(progn
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (defvar ,variable-name)
           (setf (documentation ',name 'variable) ,documentation)
           (unless (boundp ',variable-name)
             (setf ,variable-name ,initial-value)))
         (define-symbol-macro ,name (load-time-value ,variable-name))))))

(def (definer e :available-flags "e") special-variable (name &optional value documentation)
  "Uses defvar/defparameter based on whether a value was provided or not, and accepts :documentation definer parameter for value-less defvars."
  (assert (not (and documentation (getf -options- :documentation))) () "Multiple documentations for ~S" -whole-)
  (setf documentation (or documentation (getf -options- :documentation)))
  (bind ((has-value? (> (length -whole-) 3)))
    (with-standard-definer-options name
      `(progn
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))
        (defvar ,name)
        (makunbound ',name)
        ,@(when has-value?
            `((setf ,name ,value)))))))

(def (definer e :available-flags "o") constructor (class-name* &body body)
  (let ((key-args (when (listp class-name*)
                    (rest class-name*)))
        (class-name (if (listp class-name*)
                        (first class-name*)
                        class-name*)))
    (bind ((declarations (function-like-definer-declarations -options-)))
      `(locally
           ,@declarations
         ;; TODO this is a bad idea: a headache for macro writing macros...
         ;; use -self- instead. same for print-object and friends...
         (defmethod initialize-instance :after ((-self- ,class-name) &key ,@key-args)
           ,@body)))))

(def (definer e) print-object (&whole whole class-name* &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT.
  An example:
  (def print-object parenscript-dispatcher ; could be (parenscript-dispatcher :identity nil)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (bind ((args (ensure-list class-name*))
           ((class-name &key (identity t) (type t) with-package (muffle-errors t)) args)
           ((:values body declarations documentation) (parse-body body :documentation t :whole whole)))
      `(defmethod print-object ((-self- ,class-name) ,stream)
         ,@(when documentation
             (list documentation))
         ,@declarations
         (print-unreadable-object (-self- ,stream :type ,type :identity ,identity)
           (let ((*standard-output* ,stream))
             (block ,printing
               (,@(if muffle-errors
                      `(handler-bind ((error (lambda (error)
                                               (declare (ignore error))
                                               (write-string "<<error printing object>>")
                                               (return-from ,printing)))))
                      `(progn))
                  (let (,@(when with-package `((*package* ,(find-package with-package)))))
                    ,@body)))))
         ;; primary PRINT-OBJECT methods are supposed to return the object
         -self-))))

;; TODO this is very much a duplicate, but it's used by (def extended-package ...) and :hu.dwim.def.namespace brings in too many (circular) dependencies at too many places...
(def (definer :available-flags "e") dumb-namespace (name &optional definer-args &body definer-forms)
  (bind ((hashtable-var (symbolicate "*" name '#:-namespace*))
         (lock-var (symbolicate "%" name '#:-namespace-lock%))
         (finder-name (or (getf -options- :finder-name) (symbolicate '#:find- name)))
         (collector-name (symbolicate '#:collect- name '#:-namespace-values))
         (iterator-name (symbolicate '#:iterate- name '#:-namespace))
         (global-var-definer #+sbcl 'sb-ext:defglobal
                             #-sbcl 'defvar)
         (test-function (or (getf -options- :test) '#'eq)))
    (remove-from-plistf -options- :test :finder-name)
    `(progn
       ,@(when (getf -options- :export)
           `((export '(,finder-name ,collector-name ,iterator-name))))
       (,global-var-definer ,hashtable-var (make-hash-table :test ,test-function))
       (,global-var-definer ,lock-var (make-lock :name ,(concatenate 'string "lock for " (string hashtable-var))))
       (def function ,finder-name (name &key (otherwise nil otherwise?))
         (or (with-lock ,lock-var
               (gethash name ,hashtable-var))
             (if otherwise?
                 otherwise
                 (error "Cannot find ~A in namespace ~A" name ',name))))
       (def function (setf ,finder-name) (value name)
         (with-lock ,lock-var
           (if value
               (setf (gethash name ,hashtable-var) value)
               (remhash name ,hashtable-var))))
       (def function ,collector-name ()
         (with-lock ,lock-var
           (hash-table-values ,hashtable-var)))
       (def function ,iterator-name (visitor)
         (with-lock ,lock-var
           (maphash visitor ,hashtable-var)))
       ,@(unless (and (zerop (length definer-args))
                      (zerop (length definer-forms)))
          `((def (definer ,@-options-) ,name (name ,@definer-args)
              `(setf (,',finder-name ',name) ,,@definer-forms)))))))

(def (definer e :available-flags "e") global-variable (name value &optional documentation)
  (assert (not (and documentation (getf -options- :documentation))) () "Multiple documentations for ~S" -whole-)
  (setf documentation (or documentation (getf -options- :documentation)))
  (bind ((global-definer-name #+sbcl 'sb-ext:defglobal
                              #-sbcl 'defvar))
    (with-standard-definer-options name
      `(progn
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))
        (,global-definer-name ,name ,value)))))
