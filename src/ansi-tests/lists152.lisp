;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (endp 'nil)
 t)

(my-assert
 (endp (cons 'a 'b))
 nil)

(my-assert
 (endp (append (list 'a 'b) 'c))
 nil)

(my-assert
 (endp (list 'a 'b 'c))
 nil)

(my-assert
 (endp (list 'a 'b 'c 'd))
 nil)

(my-assert
 (endp (append (list 'a 'b 'c)  'd))
 nil)

(my-assert
 (endp (list ''nil ''nil))
 nil)

(my-assert
 (list-length 'nil)
 0)

(my-assert
 (list-length (cons 'a 'b))
 #+xcl 1
 #-xcl type-error)

(my-assert
 (list-length (list 'a 'b 'c 'd))
 4)

(my-assert
 (list-length
  (list 'a (list 'b 'c) 'd))
 3)

(my-assert
 (let ((x (list 'a 'b 'c)))
   (rplacd (last x)
	   x)
   (list-length x))
 nil)

(my-assert
 (nth 0
      (list 'a 'b 'c 'd))
 a)

(my-assert
 (nth 1
      (list 'a 'b 'c 'd))
 b)

(my-assert
 (nth 3
      (list 'a 'b 'c 'd))
 d)

(my-assert
 (nth 5
      (list 'a 'b 'c 'd))
 nil)

(my-assert
 (nth -2
      (list 'a 'b 'c 'd))
 type-error)

(my-assert
 (nth 0 'nil)
 nil)

(my-assert
 (nth 2 'nil)
 nil)

(my-assert
 (first (list 1 2 3 4 5 6 7 8 9 10 11))
 1)

(my-assert
 (second (list 1 2 3 4 5 6 7 8 9 10 11))
 2)

(my-assert
 (third (list 1 2 3 4 5 6 7 8 9 10 11))
 3)

(my-assert
 (fourth (list 1 2 3 4 5 6 7 8 9 10 11))
 4)

(my-assert
 (fifth (list 1 2 3 4 5 6 7 8 9 10 11))
 5)

(my-assert
 (sixth (list 1 2 3 4 5 6 7 8 9 10 11))
 6)

(my-assert
 (seventh (list 1 2 3 4 5 6 7 8 9 10 11))
 7)

(my-assert
 (eighth (list 1 2 3 4 5 6 7 8 9 10 11))
 8)

(my-assert
 (ninth (list 1 2 3 4 5 6 7 8 9 10 11))
 9)

(my-assert
 (tenth (list 1 2 3 4 5 6 7 8 9 10 11))
 10)

(my-assert
 (first (list 1 2 3))
 1)

(my-assert
 (second (list 1 2 3))
 2)

(my-assert
 (third (list 1 2 3))
 3)

(my-assert
 (fourth (list 1 2 3))
 nil)

(my-assert
 (fifth (list 1 2 3))
 nil)

(my-assert
 (sixth (list 1 2 3))
 nil)

(my-assert
 (seventh (list 1 2 3))
 nil)

(my-assert
 (eighth (list 1 2 3))
 nil)

(my-assert
 (ninth (list 1 2 3))
 nil)

(my-assert
 (tenth (list 1 2 3))
 nil)

(my-assert
 (first 'nil)
 nil)

(my-assert
 (second 'nil)
 nil)

(my-assert
 (third 'nil)
 nil)

(my-assert
 (fourth 'nil)
 nil)

(my-assert
 (fifth 'nil)
 nil)

(my-assert
 (sixth 'nil)
 nil)

(my-assert
 (seventh 'nil)
 nil)

(my-assert
 (eighth 'nil)
 nil)

(my-assert
 (ninth 'nil)
 nil)

(my-assert
 (tenth 'nil)
 nil)

(my-assert
 (rest (list 1 2 3 4 5))
 (2 3 4 5))

(my-assert
 (rest 'nil)
 nil)

(my-assert
 (rest (cons 'a 'b))
 b)

(my-assert
 (rest (append (list 1 2 3) 4))
 (2 3 . 4))

(my-assert
 (nthcdr 0
	 (list 'a 'b 'c 'd))
 (a b c d))

(my-assert
 (nthcdr 1
	 (list 'a 'b 'c 'd))
 (b c d))

(my-assert
 (nthcdr 3
	 (list 'a 'b 'c 'd))
 (d))

(my-assert
 (nthcdr 5
	 (list 'a 'b 'c 'd))
 nil)

(my-assert
 (nthcdr -2
	 (list 'a 'b 'c 'd))
 type-error)

(my-assert
 (nthcdr 0 'nil)
 nil)

(my-assert
 (nthcdr 2 'nil)
 nil)

(my-assert
 (last (list 1 2 3 4 5))
 (5))

(my-assert
 (last 'nil)
 nil)

(my-assert
 (last (cons 'a 'b))
 (a . b))

(my-assert
 (last (append (list 1 2 3) 4))
 (3 . 4))

(my-assert
 (list 'a 'b 'c 'd)
 (a b c d))

(my-assert
 (list 'a)
 (a))

(my-assert
 (list (list 'a 'b)
       (list 'c 'd))
 ((a b)
  (c d)))

(my-assert
 (list 'a 'nil)
 (a nil))

(my-assert
 (list 'nil 'a)
 (nil a))

(my-assert
 (list 'nil 'nil)
 (nil nil))

(my-assert
 (list)
 nil)

(my-assert
 (list 3 4 'a
       (car (cons 'b 'c))
       (+ 6 -2))
 (3 4 a b 4))

(my-assert
 (list* 'a 'b 'c 'd)
 (a b c . d))

(my-assert
 (list* 'a)
 a)

(my-assert
 (list* (list 'a 'b)
	(list 'c 'd))
 ((a b)
  c d))

(my-assert
 (list* 'a 'nil)
 (a))

(my-assert
 (list* 'nil 'a)
 (nil . a))

(my-assert
 (list* 'nil 'nil)
 (nil))

(my-assert
 (list*)
 program-error)

(my-assert
 (list* 3 4 'a
	(car (cons 'b 'c))
	(+ 6 -2))
 (3 4 a b . 4))

(my-assert
 (list* 'a 'b 'c
	(list 'd 'e 'f))
 (a b c d e f))

(my-assert
 (list* x)
 unbound-variable)

(my-assert
 (list* 'nil)
 nil)

(my-assert
 (make-list 5)
 (nil nil nil nil nil))

(my-assert
 (make-list 5 :initial-element)
 program-error)

(my-assert
 (make-list 3 :initial-element 'rah)
 (rah rah rah))

(my-assert
 (make-list 0)
 nil)

(my-assert
 (make-list 0 :initial-element 'aaa)
 nil)

(my-assert
 (make-list 5 :initial-element 'nil)
 (nil nil nil nil nil))

(my-assert
 (make-list)
 program-error)

(my-assert
 (append (list 'a 'b 'c)
	 (list 'd 'e 'f)
	 'nil
	 (list 'g))
 (a b c d e f g))

(my-assert
 (append (list 'a 'b 'c)
	 'd)
 (a b c . d))

(my-assert
 (append 'a 'b)
 error)

(my-assert
 (append 'a 'nil)
 error)

(my-assert
 (append 'nil 'nil)
 nil)

(my-assert
 (append 'nil 'a)
 #+xcl error
 #-xcl a)

(my-assert
 (append 'nil
	 (list 'a 'b 'c))
 (a b c))

(my-assert
 (setq x
       (list 'a 'b 'c))
 (a b c))

(my-assert
 (setq y
       (list 'd 'e 'f))
 (d e f))

(my-assert
 (setq r
       (append x y))
 (a b c d e f))

(my-assert
 x
 (a b c))

(my-assert
 y
 (d e f))

(my-assert
 (eq (cdddr r)
     y)
 t)

(my-assert
 (copy-list (list 1 2 3 4 5))
 (1 2 3 4 5))

(my-assert
 (copy-list 'nil)
 nil)

(my-assert
 (copy-list (cons 'a 'b))
 (a . b))

(my-assert
 (copy-list (append (list 1 2 3) 4))
 (1 2 3 . 4))

(my-assert
 (setq l
       (list 1 2 3 4 5))
 (1 2 3 4 5))

(my-assert
 (eq l
     (copy-list l))
 nil)

(my-assert
 (eql l
      (copy-list l))
 nil)

(my-assert
 (equal l
	(copy-list l))
 t)

(my-assert
 (equalp l
	 (copy-list l))
 t)

(my-assert
 (copy-alist 'a)
 #-clisp error
 #+clisp a)

(my-assert
 (copy-alist 'nil)
 nil)

(my-assert
 (copy-alist 5)
 #-clisp error
 #+clisp 5)

(my-assert
 (copy-alist (list 'a 'b))
 #+(or xcl clisp allegro cmu sbcl) (a b)
 #+(or ecls gcl) error
 #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(my-assert
 (copy-alist (list (cons 1 'a)
		   (cons 2 'b)
		   (cons 3 'c)))
 ((1 . a)
  (2 . b)
  (3 . c)))

(my-assert
 (setq x
       (list (cons 1 'a)
	     (cons 2 'b)
	     (cons 3 'c)))
 ((1 . a)
  (2 . b)
  (3 . c)))

(my-assert
 (eq x
     (copy-alist x))
 nil)

(my-assert
 (eql x
      (copy-alist x))
 nil)

(my-assert
 (equal x
	(copy-alist x))
 t)

(my-assert
 (eq (cadr x)
     (cadr (copy-alist x)))
 nil)

(my-assert
 (eql (cadr x)
      (cadr (copy-alist x)))
 nil)

(my-assert
 (equal (cadr x)
	(cadr (copy-alist x)))
 t)

(my-assert
 (copy-alist (list (cons 1 2))
	     (list (cons a b)))
 error)

(my-assert
 (copy-alist (list (list 'a 'b)
		   'c
		   (list 'd 'e)))
 #+(or xcl clisp allegro cmu sbcl) ((a b) c (d e))
 #+(or gcl ecls) error
 #-(or xcl clisp gcl allegro cmu sbcl ecls) unknown)

(my-assert
 (copy-tree 'x)
 x)

(my-assert
 (copy-tree 5)
 5)

(my-assert
 (copy-tree (list 'a 'b))
 (a b))

(my-assert
 (copy-tree (list 'a 'b
		  (list 'c
			(list 'd)
			(list 'e 'f))
		  'g))
 (a b
    (c (d)
       (e f))
    g))

(my-assert
 (copy-tree (list (cons 1 'e)
		  (cons 2 'f)))
 ((1 . e)
  (2 . f)))

(my-assert
 (copy-tree #*001)
 #*001)

(my-assert
 (setq x
       (list 'a 'b
	     (list 'c 'd)
	     'e))
 (a b
    (c d)
    e))

(my-assert
 (eq x
     (copy-tree x))
 nil)

(my-assert
 (eql x
      (copy-tree x))
 nil)

(my-assert
 (equal x
	(copy-tree x))
 t)

(my-assert
 (eq (cdaddr x)
     (cdaddr (copy-tree x)))
 nil)

(my-assert
 (eql (cdaddr x)
      (cdaddr (copy-tree x)))
 nil)

(my-assert
 (equal (cdaddr x)
	(cdaddr (copy-tree x)))
 t)

(my-assert
 (revappend (list 'a 'b 'c)
	    (list 'd 'e 'f)
	    'nil
	    (list 'g))
 program-error)

(my-assert
 (revappend (list 'a 'b 'c)
	    'd)
 (c b a . d))

(my-assert
 (revappend 'a 'b)
 #-clisp type-error
 #+clisp b)

(my-assert
 (revappend 'a 'nil)
 #-clisp type-error
 #+clisp nil)

(my-assert
 (revappend 'nil 'nil)
 nil)

(my-assert
 (revappend 'nil 'a)
 a)

(my-assert
 (revappend 'nil
	    (list 'a 'b 'c))
 (a b c))

(my-assert
 (revappend (list 'a 'b 'c)
	    (list 'd 'e 'f))
 (c b a d e f))

(my-assert
 (revappend (list 'd 'e 'f)
	    (list 'a 'b 'c))
 (f e d a b c))

(my-assert
 (eql (revappend (list 'a 'b 'c)
		 (list 'd 'e 'f))
      (append (reverse (list 'a 'b 'c))
	      (list 'd 'e 'f)))
 nil)

(my-assert
 (equal (revappend (list 'a 'b 'c)
		   (list 'd 'e 'f))
	(append (reverse (list 'a 'b 'c))
		(list 'd 'e 'f)))
 t)

(my-assert
 (setq x
       (list 'a 'b 'c))
 (a b c))

(my-assert
 (setq y
       (list 'd 'e 'f))
 (d e f))

(my-assert
 (setq r
       (revappend x y))
 (c b a d e f))

(my-assert
 x
 (a b c))

(my-assert
 y
 (d e f))

(my-assert
 (eq (cdddr r)
     y)
 t)

(my-assert
 (setq x
       (list 'a 'b 'c)
       y
       (list 'd 'e 'f))
 (d e f))

(my-assert
 (nconc x y)
 (a b c d e f))

(my-assert
 x
 (a b c d e f))

(my-assert
 (eq (cdddr x)
     y)
 t)

(my-assert
 (setq x
       (list 'a 'b 'c)
       y
       (list 'd 'e 'f)
       z
       (list 'g 'h 'i))
 (g h i))

(my-assert
 (nconc)
 nil)

(my-assert
 (nconc x)
 (a b c))

(my-assert
 (nconc nil)
 nil)

(my-assert
 (nconc nil nil)
 nil)

(my-assert
 (nconc x nil)
 (a b c))

(my-assert
 (nconc nil nil nil nil)
 nil)

(my-assert
 (nconc nil nil x nil)
 (a b c))

(my-assert
 (nconc x nil y nil z nil)
 (a b c d e f g h i))

(my-assert
 x
 (a b c d e f g h i))

(my-assert
 y
 (d e f g h i))

(my-assert
 z
 (g h i))

(my-assert
 (eq (cdddr x)
     y)
 t)

(my-assert
 (eq (cdddr y)
     z)
 t)

(my-assert
 (nconc (list 1 2)
	'a)
 #+xcl error
 #+(or clisp akcl allegro cmu sbcl ecls) (1 2 . a)
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (nconc 'a)
 #+xcl error
 #+(or clisp akcl allegro cmu sbcl ecls) a
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (setq x
       (list 'a 'b 'c)
       y
       (list 'd 'e 'f))
 (d e f))

(my-assert
 (nreconc x y)
 (c b a d e f))

(my-assert
 x
 #+xcl was-destroyed			; wo kommt denn so was her?
 #+clisp (c b a d e f)
 #+(or akcl allegro cmu sbcl ecls) (a d e f)
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (tailp y x)
 t)

(my-assert
 (setq x
       (list 'a 'b 'c)
       y
       (list 'd 'e 'f)
       z
       (list 'g 'h 'i))
 (g h i))

(my-assert
 (nreconc)
 program-error)

(my-assert
 (nreconc x)
 program-error)

(my-assert
 (nreconc nil)
 program-error)

(my-assert
 (nreconc nil nil)
 nil)

(my-assert
 (nreconc x nil)
 (c b a))

(my-assert
 x
 #+xcl was-destroyed
 #+clisp (c b a)
 #+(or akcl allegro cmu sbcl ecls) (a)
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (nreconc nil nil nil nil)
 program-error)

(my-assert
 (nconc nil 'x)
 #+xcl error
 #+(or clisp akcl allegro cmu sbcl ecls) x
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (setq aa nil)
 nil)

(my-assert
 (push '1 aa)
 (1))

(my-assert
 (push '2 aa)
 (2 1))

(my-assert
 (push '2 aa)
 (2 2 1))

(my-assert
 (setq aa
       (list 'b 'a))
 (b a))

(my-assert
 (pushnew 'a aa)
 (b a))

(my-assert
 (pushnew 'c aa)
 (c b a))

(my-assert
 (setq xxx nil)
 nil)

(my-assert
 (pushnew 'c xxx :test 'equal)
 (c))

(my-assert
 (pushnew 'c xxx :test 'equal)
 (c))

(my-assert
 (pushnew (list 'c) xxx :test 'equal)
 ((c) c))

(my-assert
 xxx
 ((c) c))

(my-assert
 (setq xx (list nil
		'kkk))
 (nil kkk))

(my-assert
 (pushnew 'u (car xx))
 (u))

(my-assert
 (pushnew 'u
	  (car xx))
 (u))

(my-assert
 (pushnew 'v
	  (car xx))
 (v u))

(my-assert
 xx
 ((v u) kkk))

(my-assert
 (pushnew (list 'w)
	  (car xx))
 ((w)
  v u))

(my-assert
 (pushnew (list 'w)
	  (car xx))
 ((w)
  (w)
  v u))

(my-assert
 (pushnew (list 'w)
	  (car xx)
	  :test 'equal)
 ((w)
  (w)
  v u))

(my-assert
 (pushnew (list 'w)
	  (car xx)
	  :test-not 'equal)
 ((w)
  (w)
  v u))

(my-assert
 (setq aa (list 1 2 3))
 (1 2 3))

(my-assert
 (pop aa)
 1)

(my-assert
 aa
 (2 3))

(my-assert
 (pop aa)
 2)

(my-assert
 (pop aa)
 3)

(my-assert
 (pop aa)
 nil)

(my-assert
 (pop aa)
 nil)

(my-assert
 (butlast (list 'a 'b 'c))
 (a b))

(my-assert
 (butlast (list 'a 'b 'c)
	  2)
 (a))

(my-assert
 (nbutlast (list 'a 'b 'c 'd)
	   3)
 (a))

(my-assert
 (nbutlast (list 'a 'b 'c 'd)
	   1)
 (a b c))

(my-assert
 (nbutlast (list 'a 'b 'c 'd)
	   0)
 (a b c d))

(my-assert
 (nbutlast (list 'a 'b 'c 'd)
	   4)
 nil)

(my-assert
 (nbutlast (list 'a 'b 'c 'd)
	   6)
 nil)

