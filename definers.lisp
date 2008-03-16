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

(defun function-like-definer (-definer- def-macro-name -whole- -environment- -options-)
  (declare (ignore -environment- -definer-))
  (bind ((body (nthcdr 2 -whole-))
         (name (pop body))
         (args (pop body)))
    (awhen (function-definer-option-transformer name)
      (setf -options- (funcall it -options-)))
    (flet ((get-option (option)
             (getf -options- option)))
      (bind (((:values body declarations documentation) (parse-body body :documentation #t :whole -whole-))
             (outer-declarations (if (get-option :debug)
                                     (progn
                                       (when (get-option #\o)
                                         (warn "Ignoring 'O'ptimize flag because 'D'ebug was also specified"))
                                       '((declare (optimize (speed 0) (debug 3)))))
                                     (when (get-option :optimize)
                                       '((declare (optimize (speed 3) (debug 0) (safety 2))))))))
        `(progn
           ,@(when (get-option :inline)
               `((declaim (inline ,name))))
           (locally
             ,@outer-declarations
             ,@(when (get-option :export)
                 `((export ',name)))
             (,def-macro-name ,name ,args
               ,@(when documentation
                   (list documentation))
               ,@declarations
               ,@body)))))))

(def (definer e :available-flags "ioed") function ()
  (function-like-definer -definer- 'defun -whole- -environment- -options-))

(def (definer e :available-flags "eod") method ()
  (function-like-definer -definer- 'defmethod -whole- -environment- -options-))

(def (definer e :available-flags "eod") macro ()
  (function-like-definer -definer- 'defmacro -whole- -environment- -options-))

(defun extract-function-name (spec)
  "Useful for macros that want to emulate the functional interface for functions
like #'eq and 'eq."
  (if (and (consp spec)
           (member (first spec) '(quote function)))
      (second spec)
      spec))

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

(def (definer e :available-flags "e") constant (name initial-value &optional documentation)
  "Use like: (def (constant e :test #'string=) alma \"korte\")"
  (check-type name symbol)
  (bind ((test (getf -options- :test 'eq)))
    (setf test (extract-function-name test))
    (with-standard-definer-options name
      `(defconstant ,name
        (let ((new ,initial-value))
          (if (boundp ',name)
              (let ((old (symbol-value ',name)))
                (cond
                  ((constantp ',name)
                   (cond
                     ((,test old new)
                      old)
                     (t
                      (cerror "Try to redefine the constant."
                              "~@<~S is an already defined constant whose value ~
                               ~S is not equal to the provided initial value ~S ~
                               under ~S.~:@>" ',name old new ',test)
                      new)))
                  (t
                   (cerror "Try to redefine the variable as a constant."
                           "~@<~S is an already bound non-constant variable ~
                            whose value is ~S.~:@>" ',name old)
                   new)))
              new))
        ,@(when documentation
                (list documentation))))))

(def (definer e :available-flags "e") special-variable (name &optional value documentation)
  "Uses defvar/defparameter based on whether a value was provided or not, and accepts :documentation definer parameter for value-less defvars."
  (assert (not (and documentation (getf -options- :documentation))) () "Multiple documentations for ~S" -whole-)
  (setf documentation (or documentation (getf -options- :documentation)))
  (bind ((has-value? (> (length -whole-) 3)))
    (with-standard-definer-options name
      `(progn
        ,@(when documentation
            `((setf (documentation ',name 'variable) ,documentation)))
        (,(if has-value? 'defparameter 'defvar)
         ,name
         ,@(when has-value? (list value)))))))

(def (definer e) constructor (class-name* &body body)
  (let ((key-args (when (listp class-name*)
                    (rest class-name*)))
        (class-name (if (listp class-name*)
                        (first class-name*)
                        class-name*)))
    `(defmethod initialize-instance :after ((,(intern "SELF") ,class-name) &key ,@key-args)
      ,@body)))

(def (definer e) print-object (class-name* &body body)
  "Define a PRINT-OBJECT method using PRINT-UNREADABLE-OBJECT.
  An example:
  (def print-object parenscript-dispatcher ; could be (parenscript-dispatcher :identity nil)
    (when (cachep self)
      (princ \"cached\")
      (princ \" \"))
    (princ (parenscript-file self)))"
  (with-unique-names (stream printing)
    (bind ((args (ensure-list class-name*))
           ((class-name &key (identity t) (type t) with-package (muffle-errors t)) args))
      `(defmethod print-object ((,(intern "SELF") ,class-name) ,stream)
        (print-unreadable-object (,(intern "SELF") ,stream :type ,type :identity ,identity)
          (let ((*standard-output* ,stream))
            (block ,printing
              (,@(if muffle-errors
                     `(handler-bind ((error (lambda (error)
                                              (declare (ignore error))
                                              (write-string "<<error printing object>>")
                                              (return-from ,printing)))))
                     `(progn))
                 (let (,@(when with-package `((*package* ,(find-package with-package)))))
                   ,@body)))))))))

;; TODO support (-body- foo bar baz)
(def (definer e :available-flags "e") with-macro (name args &body body)
  "(def with-macro with-foo (arg1 arg2 &key alma)
     (let ((*zyz* 42))
       (do something)
       -body-))"
  (bind ((call-funcion-name (concatenate-symbol *package* "call-" name)))
    (with-unique-names (trunk with-body)
      (with-standard-definer-options name
        `(progn
           (defun ,call-funcion-name (,trunk ,@args)
             ,@(tree-substitute `(funcall ,trunk) '-body- body))
           (defmacro ,name (,@args &body ,with-body)
             `(,',call-funcion-name
               (lambda ()
                 ,@,with-body)
               ,,@(lambda-list-to-funcall-list args))))))))

(def (definer e :available-flags "e") with/without (name)
  (bind ((variable-name (concatenate-symbol "*" name "*"))
         (with-macro-name (concatenate-symbol "with-" name))
         (without-macro-name (concatenate-symbol "without-" name)))
    `(progn
       ,@(when (getf -options- :export)
               `((export '(,variable-name ,with-macro-name ,without-macro-name))))
       (defvar ,variable-name)
       (defmacro ,with-macro-name (&body forms)
         `(let ((,',variable-name #t))
            ,@forms))
       (defmacro ,without-macro-name (&body forms)
         `(let ((,',variable-name #f))
            ,@forms)))))
