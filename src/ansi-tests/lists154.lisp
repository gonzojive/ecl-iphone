;;; based on v1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (subst 'a 'b
	'(u b
	    (b)
	    c))
 (u a
    (a)
    c))

(my-assert
 (subst 'a 'b
	'(u b
	    (b)
	    c)
	:test-not
	#'(lambda (x y)
	    (if (atom y)
		(eql x y)
		t)))
 (a b
    (b . a)
    a . a))

(my-assert
 (subst 'a 'b
	'(u b
	    (b)
	    c)
	:test
	#'(lambda (x y)
	    (not (eql x y))))
 a)

(my-assert
 (subst 'a 'b
	'(u b
	    (b)
	    c)
	:test-not
	#'(lambda (x y)
	    (not (eql x y))))
 (u a
    (a)
    c))

(my-assert
 (subst 'a 'b
	'(u b
	    (b)
	    c)
	:test-not
	#'(lambda (x y)
	    (not (eql x y)))
	:key
	#'(lambda (u)
	    (if (listp u)
		(car u))))
 (u . a))

(my-assert
 (subst-if 'nummmer 'numberp
	   '((a (7 (v 6)))))
 ((a (nummmer (v nummmer)))))

(my-assert
 (subst-if-not 'nummmer 'numberp
	       '((a (7 (v 6)))))
 nummmer)

(my-assert
 (subst-if-not 'nummmer
	       #'(lambda (x)
		   (and (listp x)
			(numberp x)))
	       '((a (7 (v 6)))))
 nummmer)

(my-assert
 (subst-if-not 'nummmer
	       #'(lambda (x)
		   (or (listp x)
		       (numberp x)))
	       '((a (7 (v 6)))))
 ((nummmer (7 (nummmer 6)))))

(my-assert
 (nsubst 'a 'b
	 '(u b
	     (b)
	     c)
	 :test-not
	 #'(lambda (x y)
	     (if (atom y)
		 (eql x y)
		 t)))
 (a b
    (b . a)
    a . a))

(my-assert
 (nsubst 'a 'b
	 '(u b
	     (b)
	     c)
	 :test-not
	 #'(lambda (x y)
	     (not (eql x y))))
 (u a
    (a)
    c))

(my-assert
 (nsubst 'a 'b
	 '(u b
	     (b)
	     c)
	 :test
	 #'(lambda (x y)
	     (not (eql x y))))
 a)

(my-assert
 (nsubst-if 'oo 'numberp
	    '(a b c
		(3 (4)
		   0)))
 (a b c
    (oo (oo)
	oo)))

(my-assert
 (nsubst-if-not 'oo 'numberp
		'(a b c
		    (3 (4)
		       0)))
 oo)

(my-assert
 (nsubst-if-not 'oo
		#'(lambda (x)
		    (or (atom x)
			(numberp x)))
		'(a b c
		    (3 (4)
		       0)))
 oo)

(my-assert
 (nsubst-if-not 'oo
		#'(lambda (x)
		    (and (atom x)
			 (numberp x)))
		'(a b c
		    (3 (4)
		       0)))
 oo)

(my-assert
 (nsubst-if-not 'oo
		#'(lambda (x)
		    (or (list x)
			(numberp x)))
		'(a b c
		    (3 (4)
		       0)))
 (a b c
    (3 (4)
       0)))

(my-assert
 (nsubst-if-not 'oo
		#'(lambda (x)
		    (or (list x)
			(symbolp x)))
		'(a b c
		    (3 (4)
		       0)))
 (a b c
    (3 (4)
       0)))

(my-assert
 (sublis '((a . a1)
	   (b . b1))
	 '(a b))
 (a1 b1))

(my-assert
 (sublis '((a . a1)
	   (b . b1))
	 '(a b
	     (b . c)))
 (a1 b1
     (b1 . c)))

(my-assert
 (sublis '((a . a1)
	   (b . b1)
	   (nil . nil1))
	 '(a b
	     (b . c)))
 (a1 b1
     (b1 . c) .
     nil1))

(my-assert
 (sublis '((a . a1)
	   (b . b1)
	   (nil . nil1))
	 '(a b
	     (b c)))
 (a1 b1
     (b1 c . nil1) .
     nil1))

(my-assert
 (sublis '((a . a1)
	   (b . b1)
	   (nil . nil1))
	 '(a b
	     (b c))
	 :test-not 'eql)
 a1)

(my-assert
 (sublis '((a . a1)
	   (b . b1)
	   (nil . nil1))
	 '(a b
	     (b c))
	 :test-not
	 #'(lambda (x y)
	     (if (atom y)
		 (eql x y))))
 a1)

(my-assert
 (sublis '(((a) .
	    uu)
	   (a . ii))
	 '(i (a)
	     a))
 (i (ii)
    ii))

(my-assert
 (sublis '(((a) . uu) (a . ii))
	 '(i (a) a)
	 :key #'(lambda (x) (if (listp x) (car x))))
 (i ii . ii))				; key wird angewandt auf: x ein blatt des baumes

(my-assert
 (sublis '(((a) . uu) (a . ii))
	 '(i (a) a)
	 :test #'(lambda (x y) (if (listp y) (eql x (car y)))))
 #+(or xcl akcl lucid allegro ecls) (i ii . ii) ; x aus der aliste, y ein blatt des baumes
 #+(or clisp cmu sbcl)              (i (uu) uu) ; x ein blatt, y aus der aliste
 #-(or xcl clisp akcl cmu sbcl lucid allegro ecls) unknown)

(my-assert
 (nsublis '(((a) . uu) (a . ii))
	  '(i (a) a)
	  :key #'(lambda (x) (if (listp x) (car x))))
 (i ii . ii))				; key wird angewandt auf: x ein blatt des baumes

(my-assert
 (nsublis '(((a) . uu) (a . ii))
	  '(i (a) a)
	  :test #'(lambda (x y) (if (listp x) (equal x y))))
 (i uu . uu))

(my-assert
 (nsublis '(((a) . uu) (a . ii))
	  '(i (a) a)
	  :test #'(lambda (x y) (if (listp y) (equal x y))))
 (i uu . uu))

(my-assert
 (nsublis '(((a) . uu) (a . ii))
	  '(i (a) a)
	  :test #'(lambda (x y) (if (listp y) (eql x (car y)))))
 #+(or xcl akcl allegro ecls) (i ii . ii)	; x aus der aliste, y ein blatt des baumes
 #+(or clisp cmu sbcl lucid)  (i (uu) uu) ; x ein blatt, y aus der aliste
 #-(or xcl clisp akcl cmu sbcl lucid allegro ecls) unknown)

