;;; based on v1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (makunbound 'a)
 a)

(my-assert
 (makunbound 'x)
 x)

(my-assert
 (car '(a b c d e f g))
 a)

(my-assert
 (cdr '(a b c d e f g))
 (b c d e f g))

(my-assert
 (caar '((a) b c d e f g))
 a)

(my-assert
 (cadr '(a b c d e f g))
 b)

(my-assert
 (cdar '((a b) c d e f g))
 (b))

(my-assert
 (cddr '(a b c d e f g))
 (c d e f g))

(my-assert
 (caaar '(((a)) b c d e f g))
 a)

(my-assert
 (caadr '(a (b) c d e f g))
 b)

(my-assert
 (cadar '((a b) c d e f g))
 b)

(my-assert
 (caddr '(a b c d e f g))
 c)

(my-assert
 (cdaar '(((a b)) c d e f g))
 (b))

(my-assert
 (cdadr '(a (b c) d e f g))
 (c))

(my-assert
 (cddar '((a b c) d e f g))
 (c))

(my-assert
 (cdddr '(a b c d e f g))
 (d e f g))

(my-assert
 (caaaar '((((a))) b c d e f g))
 a)

(my-assert
 (caaadr '(a ((b)) c d e f g))
 b)

(my-assert
 (caadar '((a (b)) c d e f g))
 b)

(my-assert
 (caaddr '(a b (c) d e f g))
 c)

(my-assert
 (cadaar '(((a b)) c d e f g))
 b)

(my-assert
 (cadadr '(a (b c) d e f g))
 c)

(my-assert
 (caddar '((a b c) d e f g))
 c)

(my-assert
 (cadddr '(a b c d e f g))
 d)

(my-assert
 (cdaaar '((((a b))) c d e f g))
 (b))

(my-assert
 (cdaadr '(a ((b c)) d e f g))
 (c))

(my-assert
 (cdadar '((a (b c)) d e f g))
 (c))

(my-assert
 (cdaddr '(a b (c d) e f g))
 (d))

(my-assert
 (cddaar '(((a b c)) d e f g))
 (c))

(my-assert
 (cddadr '(a (b c d) e f g))
 (d))

(my-assert
 (cdddar '((a b c d) e f g))
 (d))

(my-assert
 (cddddr '(a b c d e f g))
 (e f g))

(my-assert
 (car '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	e f g))
 ((((1 2 3) 4) 5) (6 7)))

(my-assert
 (cdr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	e f g))
 ((((u v w) x) y) ((q w e) r) (a b c) e f g))

(my-assert
 (caar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	 e f g))
 (((1 2 3) 4) 5))

(my-assert
 (cadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	 e f g))
 (((u v w) x) y))

(my-assert
 (cdar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	 e f g))
 ((6 7)))

(my-assert
 (cddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	 e f g))
 (((q w e) r) (a b c) e f g))

(my-assert
 (caaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 ((1 2 3) 4))

(my-assert
 (caadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 ((u v w) x))

(my-assert
 (cadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 (6 7))

(my-assert
 (caddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 ((q w e) r))

(my-assert
 (cdaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 (5))

(my-assert
 (cdadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 (y))

(my-assert
 (cddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 nil)

(my-assert
 (cdddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b c)
	  e f g))
 ((a b c) e f g))

(my-assert
 (caaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (1 2 3))

(my-assert
 (caaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (u v w))

(my-assert
 (caadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 6)

(my-assert
 (caaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (q w e))

(my-assert
 (cadaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 5)

(my-assert
 (cadadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 y)

(my-assert
 (caddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 nil)

(my-assert
 (cadddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (a b c))

(my-assert
 (cdaaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (4))

(my-assert
 (cdaadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (x))

(my-assert
 (cdadar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (7))

(my-assert
 (cdaddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (r))

(my-assert
 (cddaar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 nil)

(my-assert
 (cddadr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 nil)

(my-assert
 (cdddar '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 nil)

(my-assert
 (cddddr '(((((1 2 3) 4) 5) (6 7)) (((u v w) x) y) ((q w e) r) (a b
								  c) e f g))
 (e f g))

(my-assert
 (car 'nil)
 nil)

(my-assert
 (cdr 'nil)
 nil)

(my-assert
 (caar 'nil)
 nil)

(my-assert
 (cadr 'nil)
 nil)

(my-assert
 (cdar 'nil)
 nil)

(my-assert
 (cddr 'nil)
 nil)

(my-assert
 (caaar 'nil)
 nil)

(my-assert
 (caadr 'nil)
 nil)

(my-assert
 (cadar 'nil)
 nil)

(my-assert
 (caddr 'nil)
 nil)

(my-assert
 (cdaar 'nil)
 nil)

(my-assert
 (cdadr 'nil)
 nil)

(my-assert
 (cddar 'nil)
 nil)

(my-assert
 (cdddr 'nil)
 nil)

(my-assert
 (caaaar 'nil)
 nil)

(my-assert
 (caaadr 'nil)
 nil)

(my-assert
 (caadar 'nil)
 nil)

(my-assert
 (caaddr 'nil)
 nil)

(my-assert
 (cadaar 'nil)
 nil)

(my-assert
 (cadadr 'nil)
 nil)

(my-assert
 (caddar 'nil)
 nil)

(my-assert
 (cadddr 'nil)
 nil)

(my-assert
 (cdaaar 'nil)
 nil)

(my-assert
 (cdaadr 'nil)
 nil)

(my-assert
 (cdadar 'nil)
 nil)

(my-assert
 (cdaddr 'nil)
 nil)

(my-assert
 (cddaar 'nil)
 nil)

(my-assert
 (cddadr 'nil)
 nil)

(my-assert
 (cdddar 'nil)
 nil)

(my-assert
 (cddddr 'nil)
 nil)

(my-assert
 (car '(a b c d e f g))
 a)

(my-assert
 (cdr '(a b c d e f g))
 (b c d e f g))

(my-assert
 (caar '(a b c d e f g))
 type-error)

(my-assert
 (cadr '(a b c d e f g))
 b)

(my-assert
 (cdar '(a b c d e f g))
 type-error)

(my-assert
 (cddr '(a b c d e f g))
 (c d e f g))

(my-assert
 (caaar '(a b c d e f g))
 type-error)

(my-assert
 (caadr '(a b c d e f g))
 type-error)

(my-assert
 (cadar '(a b c d e f g))
 type-error)

(my-assert
 (caddr '(a b c d e f g))
 c)

(my-assert
 (cdaar '(a b c d e f g))
 type-error)

(my-assert
 (cdadr '(a b c d e f g))
 type-error)

(my-assert
 (cddar '(a b c d e f g))
 type-error)

(my-assert
 (cdddr '(a b c d e f g))
 (d e f g))

(my-assert
 (caaaar '(a b c d e f g))
 type-error)

(my-assert
 (caaadr '(a b c d e f g))
 type-error)

(my-assert
 (caadar '(a b c d e f g))
 type-error)

(my-assert
 (caaddr '(a b c d e f g))
 type-error)

(my-assert
 (cadaar '(a b c d e f g))
 type-error)

(my-assert
 (cadadr '(a b c d e f g))
 type-error)

(my-assert
 (caddar '(a b c d e f g))
 type-error)

(my-assert
 (cadddr '(a b c d e f g))
 d)

(my-assert
 (cdaaar '(a b c d e f g))
 type-error)

(my-assert
 (cdaadr '(a b c d e f g))
 type-error)

(my-assert
 (cdadar '(a b c d e f g))
 type-error)

(my-assert
 (cdaddr '(a b c d e f g))
 type-error)

(my-assert
 (cddaar '(a b c d e f g))
 type-error)

(my-assert
 (cddadr '(a b c d e f g))
 type-error)

(my-assert
 (cdddar '(a b c d e f g))
 type-error)

(my-assert
 (cddddr '(a b c d e f g))
 (e f g))

(my-assert
 (car '(a))
 a)

(my-assert
 (cdr '(a))
 nil)

(my-assert
 (caar '(a))
 type-error)

(my-assert
 (cadr '(a))
 nil)

(my-assert
 (cdar '(a))
 type-error)

(my-assert
 (cddr '(a))
 nil)

(my-assert
 (caaar '(a))
 type-error)

(my-assert
 (caadr '(a))
 nil)

(my-assert
 (cadar '(a))
 type-error)

(my-assert
 (caddr '(a))
 nil)

(my-assert
 (cdaar '(a))
 type-error)

(my-assert
 (cdadr '(a))
 nil)

(my-assert
 (cddar '(a))
 type-error)

(my-assert
 (cdddr '(a))
 nil)

(my-assert
 (caaaar '(a))
 type-error)

(my-assert
 (caaadr '(a))
 nil)

(my-assert
 (caadar '(a))
 type-error)

(my-assert
 (caaddr '(a))
 nil)

(my-assert
 (cadaar '(a))
 type-error)

(my-assert
 (cadadr '(a))
 nil)

(my-assert
 (caddar '(a))
 type-error)

(my-assert
 (cadddr '(a))
 nil)

(my-assert
 (cdaaar '(a))
 type-error)

(my-assert
 (cdaadr '(a))
 nil)

(my-assert
 (cdadar '(a))
 type-error)

(my-assert
 (cdaddr '(a))
 nil)

(my-assert
 (cddaar '(a))
 type-error)

(my-assert
 (cddadr '(a))
 nil)

(my-assert
 (cdddar '(a))
 type-error)

(my-assert
 (cddddr '(a))
 nil)

(my-assert
 (cons 1 2)
 (1 . 2))

(my-assert
 (cons 'a 'b)
 (a . b))

(my-assert
 (cons 'a 'b 'c)
 program-error)

(my-assert
 (cons 'a)
 program-error)

(my-assert
 (cons)
 program-error)

(my-assert
 (cons 'a 'nil)
 (a))

(my-assert
 (cons 'nil 'a)
 (nil . a))

(my-assert
 (cons 'a (cons 'b (cons 'c 'nil)))
 (a b c))

(my-assert
 (cons 'a '(b c d))
 (a b c d))

(my-assert
 (tree-equal 1 1)
 t)

(my-assert
 (tree-equal 'word 'word)
 t)

(my-assert
 (tree-equal 'word1 'word2)
 nil)

(my-assert
 (tree-equal '(a b) '(a b))
 t)

(my-assert
 (tree-equal '(a (b c)) '((a b) c))
 nil)

(my-assert
 (tree-equal 5 (+ 2 3))
 t)

(my-assert
 (tree-equal '(a (b quote nil)) '(a (b)))
 nil)

(my-assert
 (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))))
 nil)

(my-assert
 (tree-equal 1 1 :test #'eq)
 t)

(my-assert
 (tree-equal 'word 'word :test #'eq)
 t)

(my-assert
 (tree-equal 'word1 'word2 :test #'eq)
 nil)

(my-assert
 (tree-equal '(a b) '(a b) :test #'eq)
 t)

(my-assert
 (tree-equal '(a (b c)) '((a b) c) :test #'eq)
 nil)

(my-assert
 (tree-equal 5 (+ 2 3) :test #'eq)
 t)

(my-assert
 (tree-equal '(a (b)) '(a (b)) :test #'eq)
 t)

(my-assert
 (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'eq)
 nil)

(my-assert
 (tree-equal 1 1 :test #'eql)
 t)

(my-assert
 (tree-equal 'word 'word :test #'eql)
 t)

(my-assert
 (tree-equal 'word1 'word2 :test #'eql)
 nil)

(my-assert
 (tree-equal '(a b) '(a b) :test #'eql)
 t)

(my-assert
 (tree-equal '(a (b c)) '((a b) c) :test #'eql)
 nil)

(my-assert
 (tree-equal 5 (+ 2 3) :test #'eql)
 t)

(my-assert
 (tree-equal '(a (b)) '(a (b)) :test #'eql)
 t)

(my-assert
 (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'eql)
 nil)

(my-assert
 (tree-equal 1 1 :test #'equal)
 t)

(my-assert
 (tree-equal 'word 'word :test #'equal)
 t)

(my-assert
 (tree-equal 'word1 'word2 :test #'equal)
 nil)

(my-assert
 (tree-equal '(a b) '(a b) :test #'equal)
 t)

(my-assert
 (tree-equal '(a (b c)) '((a b) c) :test #'equal)
 nil)

(my-assert
 (tree-equal 5 (+ 2 3) :test #'equal)
 t)

(my-assert
 (tree-equal '(a (b)) '(a (b)) :test #'equal)
 t)

(my-assert
 (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test #'equal)
 nil)

(my-assert
 (tree-equal 1 1 :test-not #'eq)
 nil)

(my-assert
 (tree-equal 'word 'word :test-not #'eq)
 nil)

(my-assert
 (tree-equal 'word1 'word2 :test-not #'eq)
 t)

(my-assert
 (tree-equal '(a b) '(a b) :test-not #'eq)
 nil)

(my-assert
 (tree-equal '(a (b c)) '((a b) c) :test-not #'eq)
 nil)

(my-assert
 (tree-equal 5 (+ 2 3) :test-not #'eq)
 nil)

(my-assert
 (tree-equal '(a (b)) '(a (b)) :test-not #'eq)
 nil)

(my-assert
 (tree-equal '(a (b . 1.0)) '(a (b #c(1.0 0.0))) :test-not #'eq)
 nil)

