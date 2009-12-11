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

(def test evaluate-with-macro-definition (definition)
  ;; mostly to have something to trace
  (finishes (eval definition)))

(def fixture test/with-macro/fixture
  (handler-bind ((style-warning #'muffle-warning))
    (macrolet ((frob (&body body)
                 `(progn
                    ,@(mapcar (lambda (el)
                                `(evaluate-with-macro-definition ',el))
                              body))))
      (finishes (frob
                 (def with-macro with-foo1 (arg)
                   (bind ((*with-foo/special* arg))
                     (-body-)))
                 (def with-macro with-foo2 (foo bar)
                   (bind ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     ;; using this syntax, LOCAL is "transferred" into the lexical scope of the body
                     (-body- local)))
                 (def with-macro* with-foo3 (foo &key bar)
                   (bind ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     (-body- local)))
                 (def (with-macro* :quoted-arguments lexical-var-name) with-foo4 (lexical-var-name foo &key bar)
                   (bind ((local-var (* 2 foo))
                          (*with-foo/special* (+ local-var bar)))
                     (-body- (local-var lexical-var-name))))
                 (def (with-macro* :quoted-arguments (new-var-name)) with-foo5 (new-var-name foo &rest args &key bar (keyword-defaulting (+ 2 2)) &allow-other-keys)
                   (bind ((local (* 2 foo))
                          (*with-foo/special* (+ local bar)))
                     (-body- (local new-var-name))
                     (list* keyword-defaulting (remove-from-plist args :keyword-defaulting))))
                 (def with-macro* with-foo6 (arg1 arg2 arg2-new-name)
                   (bind ((arg1 (* 2 arg1))
                          (arg2 (+ 2 arg2 )))
                     (-body- arg1 (arg2 arg2-new-name))))
                 (def with-macro* with-foo7 (some-variable)
                   ;; test the situation here when the new name is not coming from a local variable, but it's a quoted symbol
                   (-body- (some-variable 'constant-new-name)))
                 (def with-macro* with-foo8 (p1 p2
                                                &optional (o1 *with-foo/special* o1-provided?) (o2 *with-foo/special*)
                                                &key (k1 *with-foo/special* k1-provided?) (k2 *with-foo/special*)
                                                &allow-other-keys)
                   ;; *with-foo/special* must not be referenced anywhere while macroexpanding
                   ;; let's also test here when the new name (here 'x') is not coming from a local variable, but it's a quoted symbol
                   (macrolet ((argument-list ()
                                '(list p1 p2 o1 o1-provided? o2 k1 k1-provided? k2)))
                     (bind ((argument-list (argument-list)))
                       (declare (special argument-list))
                       (-body-))
                     (argument-list))))))))

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
  (with-foo4 (new-name 2 :bar 6)
    (is (= new-name 4))    ; LOCAL is visible in the lexical scope by the name given in the first required argument
    (is (= *with-foo/special* 10))))

(def with-macro-test test/with-macro/5
  (is (equal '(4 :bar 6 :baz 42)
             (with-foo5 (zork 2 :bar 6 :baz 42)
               (is (= zork 4))
               (is (= *with-foo/special* 10))))))

(def with-macro-test test/with-macro/6
  (bind ((x 2)
         (y 10))
    ;; the new name of arg2 (in this case 'bar) is implied to be a quoted macro argument due to it is being used as a new name in (-body- ...)
    (with-foo6 (x y bar)
      (is (= arg1 4))
      (is (= bar 12)))))

(def with-macro-test test/with-macro/7
    (with-foo7 (99)
      (is (= constant-new-name 99))))

(def with-macro-test test/with-macro/8
  (bind ((*with-foo/special* 'default))
    (bind ((expected-argument-list '(11 22 default nil default default nil default)))
      (is (equal (with-foo8 (11 22)
                   (is (equal (symbol-value 'argument-list) expected-argument-list)))
                 expected-argument-list)))
    (bind ((expected-argument-list '(11 22 :o1 t :o2 42 t default)))
      (is (equal (with-foo8 (11 22 :o1 :o2 :k1 42)
                   (is (equal (symbol-value 'argument-list) expected-argument-list)))
                 expected-argument-list)))
    (bind ((expected-argument-list '(11 22 :o1 t :o2 default nil 43)))
      (is (equal (with-foo8 (11 22 :o1 :o2 :k2 43)
                   (is (equal (symbol-value 'argument-list) expected-argument-list)))
                 expected-argument-list)))))
