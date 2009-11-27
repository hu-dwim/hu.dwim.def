;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def (definer e :available-flags "ioed") function ()
  (function-like-definer -definer- 'defun -whole- -environment- -options-))

(def (definer e :available-flags "eod") method ()
  (function-like-definer -definer- 'defmethod -whole- -environment- -options-))

(def (definer e :available-flags "eod") methods ()
  (defmethods-like-definer 'defmethod -whole- -options-))

(def (definer e :available-flags "eod") macro ()
  (function-like-definer -definer- 'defmacro -whole- -environment- -options-))

(def (definer e :available-flags "eod") compiler-macro ()
  (function-like-definer -definer- 'define-compiler-macro -whole- -environment- -options-))

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
  (bind ((documentation (when (stringp (first slots))
                          (pop slots)))
         (options (rest (ensure-list name)))
         (real-name (first (ensure-list name))))
    `(progn
       ,@(when (getf -options- :export)
           (bind ((constructor (aif (assoc-value options :constructor)
                                    (first it)
                                    (symbolicate '#:make- real-name))))
             `((export '(,real-name ,constructor)))))
       ,@(awhen (and (getf -options- :export-slot-names)
                     (mapcar (lambda (slot)
                               (first (ensure-list slot)))
                             slots))
            `((export ',it)))
       (defstruct ,name
         ,@(when documentation
             (list documentation))
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

(def function expand-with-macro/compute-macro-arguments (args fn-args)
  (bind ((macro-args nil)
         (funcall-list nil)
         (rest-variable-name nil))
    (bind (((:values requireds optionals rest keywords allow-other-keys?) (parse-ordinary-lambda-list args)))
      (setf macro-args `(,@requireds
                         ,@(when optionals (cons '&optional optionals))
                         ,@(if rest
                               `(&rest ,rest)
                               (when keywords (cons '&key keywords)))
                         ,@(when (and allow-other-keys? (not rest))
                                 (list '&allow-other-keys)))))
    (bind (((:values requireds optionals rest keywords) (parse-ordinary-lambda-list fn-args)))
      (setf funcall-list (append requireds
                                 (loop
                                   :for entry :in optionals
                                   :collect (first entry))
                                 (unless rest
                                   (loop
                                     :for entry :in keywords
                                     :appending (list (first (first entry)) (second (first entry)))))))
      (setf rest-variable-name rest))
    (values macro-args
            funcall-list
            rest-variable-name)))

(def function expand-with-macro (name args body -options- flat? must-have-args?)
  (flet ((simple-lambda-list? (args)
           (bind (((:values nil optionals rest keywords allow-other-keys?) (parse-ordinary-lambda-list args)))
             (and (not rest)
                  (not optionals)
                  (not keywords)
                  (not allow-other-keys?)))))
    (unless (or (not flat?)
                (simple-lambda-list? args))
      (error "Can not generate a flat with-macro when using &rest, &optional or &key in its lambda list. Use with-macro* for that.")))
  (with-unique-names (fn with-body)
    (with-standard-definer-options name
      (bind ((call-funcion-name (format-symbol *package* "CALL-~A" name))
             (body-invocation-arguments 'undefined))
        (labels ((process-body (form)
                   (cond ((consp form)
                          (cond
                            ((eq (first form) '-body-)
                             (unless (or (eq body-invocation-arguments 'undefined)
                                         (equal body-invocation-arguments (rest form)))
                               (error "Used -BODY- multiple times and they have different argument lists: ~S, ~S" body-invocation-arguments (rest form)))
                             (setf body-invocation-arguments (rest form))
                             ;; use an flet instead of `(funcall ,fn ,@body-invocation-arguments) so that #'-body- also works as expected
                             `(,(first form) ,@(mapcar (lambda (el)
                                                         (first (ensure-list el)))
                                                       (rest form))))
                            ((and (eq (first form) 'function)
                                  (eq (second form) '-body-)
                                  (length= 2 form))
                             ;; shut up if there's a #'-body- somewhere
                             (setf body-invocation-arguments nil)
                             form)
                            (t
                             (iter (for entry :first form :then (cdr entry))
                                   (collect (process-body (car entry)) :into result)
                                   (cond
                                     ((consp (cdr entry))
                                      ;; nop, go on looping
                                      )
                                     ((cdr entry)
                                      (setf (cdr (last result)) (cdr entry))
                                      (return result))
                                     (t (return result)))))))
                         ((typep form 'standard-object)
                          ;; NOTE: to avoid warning for quasi-quote literal STANDARD-OBJECT AST nodes wrapping -body-
                          (setf body-invocation-arguments nil)
                          form)
                         (t form))))
          (setf body (process-body body))
          (when (eq body-invocation-arguments 'undefined)
            (simple-style-warning "You probably want to have at least one (-body-) form in the body of a WITH-MACRO to invoke the user provided body...")
            (setf body-invocation-arguments nil))
          (bind ((args-to-remove-from-fn ())
                 (fn-args args)
                 (inner-arguments/macro-body ())
                 (inner-arguments/fn-body ()))
            (dolist (el body-invocation-arguments)
              (if (consp el)
                  (progn
                    (unless (and (length= 2 el)
                                 (notany (lambda (part)
                                           (or (not (symbolp part))
                                               (not (symbolp part))
                                               (member part '(&rest &optional &key &allow-other-keys))))
                                         el))
                      (error "The arguments used to invoke (-body- foo1 foo2) may only contain symbols, or (var-name-inside-macro-body lexically-visible-var-name-for-user-forms) pairs denoting variables that are \"transferred\" from the lexical scope of the with-macro into the lexical scope of the user provided body forms."))
                    (push (second el) args-to-remove-from-fn)
                    (push (first el) inner-arguments/macro-body)
                    (push (second el) inner-arguments/fn-body))
                  (progn
                    (push el inner-arguments/macro-body)
                    (push `(quote ,el) inner-arguments/fn-body))))
            (reversef inner-arguments/macro-body)
            (reversef inner-arguments/fn-body)
            (bind ()
              (dolist (arg args-to-remove-from-fn)
                (removef fn-args arg))
              (bind (((:values macro-args funcall-list rest-variable-name) (expand-with-macro/compute-macro-arguments args fn-args))
                     (body-fn-name (format-symbol *package* "~A-BODY" name)))
                `(progn
                   (defun ,call-funcion-name (,fn ,@fn-args)
                     (declare (type function ,fn))
                     ,@(function-like-definer-declarations -options-)
                     (flet ((-body- (,@inner-arguments/macro-body)
                              (funcall ,fn ,@inner-arguments/macro-body)))
                       (declare (inline -body-))
                       (block ,name
                         ,@body)))
                   (defmacro ,name (,@(when (or args must-have-args?)
                                        (if flat? macro-args (list macro-args)))
                                    &body ,with-body)
                     `(,',call-funcion-name
                       (named-lambda ,',body-fn-name ,(list ,@inner-arguments/fn-body)
                         ,@,with-body)
                       ,,@funcall-list
                       ,@,rest-variable-name)))))))))))

(def (definer e :available-flags "eod") with-macro (name args &body body)
  "(def with-macro with-foo (arg1 arg2)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo arg1 arg2
     (...))"
  (expand-with-macro name args body -options- t nil))

(def (definer e :available-flags "eod") with-macro* (name args &body body)
  "(def with-macro* with-foo (arg1 arg2 &key alma)
     (let ((*zyz* 42)
           (local 43))
       (do something)
       (-body- local)))
   Example:
   (with-foo (arg1 arg2 :alma alma)
     (...))"
  (expand-with-macro name args body -options- nil t))

(def (definer e :available-flags "e") with/without (name)
  (bind ((package (symbol-package name))
         (variable-name (format-symbol package "*~A*" name))
         (with-macro-name (format-symbol package "WITH-~A" name))
         (without-macro-name (format-symbol package "WITHOUT-~A" name)))
    `(progn
       ,@(when (getf -options- :export)
               `((export '(,variable-name ,with-macro-name ,without-macro-name))))
       (defvar ,variable-name)
       (defmacro ,with-macro-name (&body forms)
         `(let ((,',variable-name t))
            ,@forms))
       (defmacro ,without-macro-name (&body forms)
         `(let ((,',variable-name nil))
            ,@forms)))))

(def (definer e :available-flags "e") namespace (name &optional definer-args &body definer-forms)
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
