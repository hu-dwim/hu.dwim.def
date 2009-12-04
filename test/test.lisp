;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def.test)

(defsuite* (test :in root-suite))

(deftest test/function ()
  (is (equal '(progn
               (declaim (notinline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export 'foo))
                 (defun foo (bar baz unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (function ioed) foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/method ()
  (is (equal '(progn
               (declaim (notinline foo))
               (locally (declare (optimize (speed 0) (debug 3)))
                 (eval-when (:compile-toplevel :load-toplevel :execute)
                   (export 'foo))
                 (defmethod foo ((bar integer) (baz string) unused)
                   "documentation"
                   (declare (ignore unused))
                   (+ bar baz))))
             (macroexpand-1 '(def (method oed) foo ((bar integer) (baz string) unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/test ()
  (is (equal '(progn
               (locally
                   (deftest foo (bar baz unused)
                     "documentation"
                     (declare (ignore unused))
                     (+ bar baz))))
             (macroexpand-1 '(def test foo (bar baz unused)
                              "documentation"
                              (declare (ignore unused))
                              (+ bar baz))))))

(deftest test/constant ()
  (is (equal '(progn
               (eval-when (:compile-toplevel :load-toplevel :execute)
                 (defconstant +foo+ (hu.dwim.def::%reevaluate-constant '+foo+ 1 :test 'equal)
                   "documentation")))
             (macroexpand-1 '(def constant +foo+ 1 "documentation")))))

(deftest test/special-variable ()
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)
                 (setf +foo+ 1)))
             (macroexpand-1 '(def special-variable +foo+ 1 "documentation"))))
  (is (equal '(progn
               (progn
                 (setf (documentation '+foo+ 'variable) "documentation")
                 (defvar +foo+)
                 (makunbound '+foo+)))
             (macroexpand-1 '(def (special-variable :documentation "documentation") +foo+)))))

(defvar *with-foo/special*)

(defixture test/with-macro/fixture
  (handler-bind ((style-warning #'muffle-warning))
    (macrolet ((frob (&body body)
                 `(progn
                    ,@(mapcar (lambda (el)
                                `(finishes (eval ',el)))
                              body))))
      (finishes (frob
                 (def with-macro with-foo1 (foo)
                   (let ((*with-foo/special* foo))
                     (-body-)))
                 (def with-macro with-foo2 (foo bar)
                   (let* ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     ;; using this syntax, LOCAL is "transferred" into the lexical scope of the body
                     (-body- local)))
                 (def with-macro* with-foo3 (foo &key bar)
                   (let* ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     (-body- local)))
                 (def with-macro* with-foo4 (lexical-var-name foo &key bar)
                   (let* ((macro-local-var (* 2 foo))
                          (*with-foo/special* (+ macro-local-var bar)))
                     (-body- (macro-local-var lexical-var-name))))
                 (def with-macro* with-foo5 (new-var-name foo &rest args &key bar (keyword-defaulting (+ 2 2)) &allow-other-keys)
                   (let* ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     (-body- (local new-var-name))
                     (list* keyword-defaulting (remove-from-plist args :keyword-defaulting))))
                 (def with-macro* with-foo6 (arg1 arg2 arg2-new-name)
                   (let* ((arg1 (* 2 arg1))
                          (arg2 (+ 2 arg2 )))
                     (-body- arg1 (arg2 arg2-new-name))))

                 (def with-macro* with-foo7 (&key (keyword-defaulting *with-foo/special*) &allow-other-keys)
                   ;; *with-foo/special* must not be referenced while macroexpanding
                   ;; let's also test here when the new name (here 'x') is not coming from a local variable, but it's a quoted symbol
                   (-body- (keyword-defaulting 'x))))))))

(defsuite* test/with-macro ()
  (with-fixture test/with-macro/fixture
    (run-child-tests)))

(deftest (test/with-macro/1 :compile-before-run t) ()
  (with-foo1 42
    (is (= *with-foo/special* 42))))

(deftest (test/with-macro/2 :compile-before-run t) ()
  (with-foo2 2 6
    (is (= local 4))
    (is (= *with-foo/special* 10))))

(deftest (test/with-macro/3 :compile-before-run t) ()
  (with-foo3 (2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= local 4))    ; LOCAL is visible in the lexical scope
    (is (= *with-foo/special* 10))))

(deftest (test/with-macro/4 :compile-before-run t) ()
  (with-foo4 (zork 2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= zork 4))    ; LOCAL is visible in the lexical scope by the name given in VAR-NAME
    (is (= *with-foo/special* 10))))

(deftest (test/with-macro/5 :compile-before-run t) ()
  (is (equal '(4 :bar 6 :baz 42)
             (with-foo5 (zork 2 :bar 6 :baz 42)
               (is (= zork 4))
               (is (= *with-foo/special* 10))))))

(deftest (test/with-macro/6 :compile-before-run t) ()
  (bind ((x 2)
         (y 10))
    (with-foo6 (x y bar)
      (is (= arg1 4))
      (is (= bar 12)))))

(deftest (test/with-macro/7 :compile-before-run t) ()
  (bind ((*with-foo/special* 42))
    (with-foo7 ()
      (is (= x 42)))))
