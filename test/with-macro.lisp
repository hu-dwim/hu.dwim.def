;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def.test)

(defsuite* (test/with-macro :in test) ()
  (with-fixture test/with-macro/fixture
    (run-child-tests)))

(def special-variable *with-foo/special*)

(def fixture test/with-macro/fixture
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

(def definer with-macro-test (name &body body)
  `(def test (,name :compile-before-run t) ()
     (with-fixture test/with-macro/fixture
       ,@body)))

(def with-macro-test test/with-macro/1
  (with-foo1 42
    (is (= *with-foo/special* 42))))

(def with-macro-test test/with-macro/2
  (with-foo2 2 6
    (is (= local 4))
    (is (= *with-foo/special* 10))))

(def with-macro-test test/with-macro/3
  (with-foo3 (2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= local 4))    ; LOCAL is visible in the lexical scope
    (is (= *with-foo/special* 10))))

(def with-macro-test test/with-macro/4
  (with-foo4 (zork 2 :bar 6) ; there's a full arglist, because it's a with-macro*
    (is (= zork 4))    ; LOCAL is visible in the lexical scope by the name given in VAR-NAME
    (is (= *with-foo/special* 10))))

(def with-macro-test test/with-macro/5
  (is (equal '(4 :bar 6 :baz 42)
             (with-foo5 (zork 2 :bar 6 :baz 42)
               (is (= zork 4))
               (is (= *with-foo/special* 10))))))

(def with-macro-test test/with-macro/6
  (bind ((x 2)
         (y 10))
    (with-foo6 (x y bar)
      (is (= arg1 4))
      (is (= bar 12)))))

(def with-macro-test test/with-macro/7
  (bind ((*with-foo/special* 42))
    (with-foo7 ()
      (is (= x 42)))))
