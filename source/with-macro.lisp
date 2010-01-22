;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def function compute-arguments-for-function-bridge-macro (args &optional body-invocation-arguments quoted-arguments)
  (unless args
    (return-from compute-arguments-for-function-bridge-macro
      (values nil nil nil)))
  (labels ((maybe-quote (arg)
             (if (member arg quoted-arguments :test #'eq)
                 ``(quote ,,arg)
                 arg))
           (process-required-argument (arg)
             ;; leave out args that are used to input new lexical names to local vars
             (unless (member arg body-invocation-arguments
                             :key (lambda (el)
                                    (second (ensure-list el))) 
                             :test #'eq)
               (maybe-quote arg))))
    (bind (((:values requireds optionals rest-variable-name keywords allow-other-keys?) (parse-ordinary-lambda-list args))
           ((:values nil raw-optionals nil raw-keywords) (parse-ordinary-lambda-list args :normalize nil))
           (macro-args requireds)
           (funcall-list (list `(list ,@(remove nil (mapcar #'process-required-argument requireds))))))
      (when optionals
        (appendf macro-args '(&optional)))
      (loop
        :for entry :in optionals
        :for raw-entry :in raw-optionals
        :for local-var = (first entry)
        :for provided? = (or (third entry)
                             (gensym (concatenate 'string (string local-var) (string '#:-provided?))))
        :do (progn
              (appendf funcall-list `((when ,provided? (list ,(maybe-quote local-var)))))
              (appendf macro-args (list (if (consp raw-entry)
                                            (list (first raw-entry) `(quote ,(second raw-entry)) provided?)
                                            (list raw-entry nil provided?))))))
      (if rest-variable-name
          (appendf macro-args `(&rest ,rest-variable-name))
          (progn
            (when keywords
              (appendf macro-args '(&key))
              (loop
                :for entry :in keywords
                :for raw-entry :in raw-keywords
                :for keyword = (first (first entry))
                :for local-var = (second (first entry))
                :for provided? = (or (third entry)
                                     (gensym (concatenate 'string (string local-var) (string '#:-provided?))))
                :do (progn
                      (appendf funcall-list `((when ,provided? (list ',keyword ,(maybe-quote local-var)))))
                      (appendf macro-args (list (if (consp raw-entry)
                                                    (list (first (ensure-list (first raw-entry))) `(quote ,(second raw-entry)) provided?)
                                                    (list raw-entry nil provided?)))))))
            (when allow-other-keys?
              (appendf macro-args '(&allow-other-keys)))))
      (values macro-args
              funcall-list
              rest-variable-name))))

(def function expand-with-macro/process-body (body-form)
  (bind ((body-invocation-arguments 'undefined))
    (labels
        ((recurse (form)
           (cond
             ((consp form)
              (cond
                ;; TODO obsolete -body- (search this file for it)
                ((member (first form) '(-body- -with-macro/body-))
                 (unless (or (member body-invocation-arguments '(undefined ignore))
                             (equal body-invocation-arguments (rest form)))
                   (error "Used -WITH-MACRO/BODY- multiple times and they have different argument lists: ~S, ~S" body-invocation-arguments (rest form)))
                 (setf body-invocation-arguments (rest form))
                 ;; use an flet instead of `(funcall ,fn ,@body-invocation-arguments) so that #'-with-macro/body- also works as expected
                 `(,(first form) ,@(mapcar (lambda (el)
                                             (first (ensure-list el)))
                                           (rest form))))
                ((and (eq (first form) 'function)
                      (member (second form) '(-body- -with-macro/body-))
                      (length= 2 form))
                 ;; shut up if there's a #'-with-macro/body- somewhere
                 (setf body-invocation-arguments nil)
                 form)
                (t
                 (iter (for entry :first form :then (cdr entry))
                       (collect (recurse (car entry)) :into result)
                       (cond
                         ((consp (cdr entry))
                          ;; nop, go on looping
                          )
                         ((cdr entry)
                          (setf (cdr (last result)) (cdr entry))
                          (return result))
                         (t (return result)))))))
             ((typep form 'standard-object)
              ;; FIXME: KLUDGE to avoid a warning when quasi-quote literal STANDARD-OBJECT AST nodes are "hiding" -with-macro/body- references
              (setf body-invocation-arguments 'ignore)
              form)
             (t form))))
      (values (recurse body-form) (if (eq body-invocation-arguments 'ignore)
                                      nil
                                      body-invocation-arguments)))))

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
      (bind ((define-function? (getf -options- :define-function t))
             (define-macro? (getf -options- :define-macro t))
             (call-with-fn/name (format-symbol *package* "CALL-~A" name))
             (ignorable-variables '())
             ((:values body body-invocation-arguments) (expand-with-macro/process-body body)))
        (when (eq body-invocation-arguments 'undefined)
          (simple-style-warning "You probably want to have at least one (-with-macro/body-) form in the body of a WITH-MACRO to invoke the user provided body...")
          (setf body-invocation-arguments nil))
        (bind ((lexically-transferred-arguments '())
               (body-lambda-arguments '())
               (call-with-fn/arguments args))
          (dolist (el body-invocation-arguments)
            (if (consp el)
                (bind (((original-name &optional new-name &key ignorable) el))
                  (unless new-name
                    (setf new-name `(quote ,original-name)))
                  (when (or (not (symbolp original-name))
                            (not (or (symbolp new-name)
                                     (and (consp new-name)
                                          (eq (first new-name) 'quote)
                                          (symbolp (second new-name))
                                          (not (cddr new-name))))))
                    (error "The arguments used to invoke (-with-macro/body- foo1 foo2) may only contain symbols, or (var-name-inside-macro-body var-name-visible-for-user-forms) pairs denoting variables that are \"transferred\" from the lexical scope of the with-macro into the lexical scope of the user provided body forms (implemented by renaming the fn's argument)."))
                  (when ignorable
                    (push new-name ignorable-variables))
                  (removef call-with-fn/arguments new-name)
                  (push new-name body-lambda-arguments)
                  (push original-name lexically-transferred-arguments))
                (progn
                  (push el lexically-transferred-arguments)
                  (push `(quote ,el) body-lambda-arguments))))
          (reversef lexically-transferred-arguments)
          (reversef body-lambda-arguments)
          (bind (((:values macro-args funcall-list rest-variable-name) (compute-arguments-for-function-bridge-macro
                                                                        args body-invocation-arguments
                                                                        (ensure-list (getf -options- :quoted-arguments)))))
            `(progn
               ,(when define-function?
                  `(defun ,call-with-fn/name (,fn ,@call-with-fn/arguments)
                     (declare (type function ,fn))
                     ,@(function-like-definer-declarations -options-)
                     (labels ((-with-macro/body- (,@lexically-transferred-arguments)
                                (funcall ,fn ,@lexically-transferred-arguments))
                              (-body- (&rest args)
                                (apply #'-with-macro/body- args)))
                       (declare (dynamic-extent #'-with-macro/body-))
                       (block ,name
                         ,@body))))
               ,(when define-macro?
                  `(defmacro ,name (,@(when (or args must-have-args?)
                                            (if flat? macro-args (list macro-args)))
                                    &body ,with-body)
                     `(,',call-with-fn/name
                       (named-lambda ,',(symbolicate name '#:-body) ,(list ,@body-lambda-arguments)
                         ,@,(when ignorable-variables
                             ``((declare (ignorable ,,@ignorable-variables))))
                         ,@,with-body)
                       ,@,@funcall-list
                       ,@,rest-variable-name))))))))))

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
