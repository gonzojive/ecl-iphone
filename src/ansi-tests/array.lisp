;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

;;erzeuge ein feld mit doppeltgenauen zahlen

(my-assert
 (setq da1
       (make-array
	(list 4 2 3)
	:initial-contents
	(list
	 (list (list 1.0d0 2.0d0 3.0d0)
	       (list 4.0d0 5.0d0 6.0d0))
	 (list (list 7.0d0 8.0d0 9.0d0)
	       (list 10.0d0 11.0d0 12.0d0))
	 (list (list 13.0d0 14.0d0 15.0d0)
	       (list 16.0d0 17.0d0 18.0d0))
	 (list (list 19.0d0 20.0d0 21.0d0)
	       (list 22.0d0 23.0d0 24.0d0)))
	:element-type
	(quote double-float)))
 #3a(((1.0d0 2.0d0 3.0d0)   (4.0d0 5.0d0 6.0d0))
     ((7.0d0 8.0d0 9.0d0)   (10.0d0 11.0d0 12.0d0))
     ((13.0d0 14.0d0 15.0d0)(16.0d0 17.0d0 18.0d0))
     ((19.0d0 20.0d0 21.0d0)(22.0d0 23.0d0 24.0d0))))

(my-assert
 (aref da1 0 0 0)
 1.0d0)

(my-assert
 (aref da1 0 0 1)
 2.0d0)

(my-assert
 (aref da1 0 0 2)
 3.0d0)

(my-assert
 (aref da1 0 1 0)
 4.0d0)

(my-assert
 (aref da1 0 1 1)
 5.0d0)

(my-assert
 (aref da1 0 1 2)
 6.0d0)

(my-assert
 (aref da1 1 0 0)
 7.0d0)

(my-assert
 (aref da1 1 0 1)
 8.0d0)

(my-assert
 (aref da1 1 0 2)
 9.0d0)

(my-assert
 (aref da1 1 1 0)
 10.0d0)

(my-assert
 (aref da1 1 1 1)
 11.0d0)

(my-assert
 (aref da1 1 1 2)
 12.0d0)

(my-assert
 (aref da1 2 0 0)
 13.0d0)

(my-assert
 (aref da1 2 0 1)
 14.0d0)

(my-assert
 (aref da1 2 0 2)
 15.0d0)

(my-assert
 (aref da1 2 1 0)
 16.0d0)

(my-assert
 (aref da1 2 1 1)
 17.0d0)

(my-assert
 (aref da1 2 1 2)
 18.0d0)

(my-assert
 (aref da1 3 0 0)
 19.0d0)

(my-assert
 (aref da1 3 0 1)
 20.0d0)

(my-assert
 (aref da1 3 0 2)
 21.0d0)

(my-assert
 (aref da1 3 1 0)
 22.0d0)

(my-assert
 (aref da1 3 1 1)
 23.0d0)

(my-assert
 (aref da1 3 1 1)
 23.0d0)

;;erzeuge ein feld mit einfachgenauen zahlen

(my-assert
 (setq fa1
       (make-array
	(list 4 2 3)
	:initial-contents
	(list
	 (list (list 1.0d0 2.0d0 3.0d0)
	       (list 4.0d0 5.0d0 6.0d0))
	 (list (list 7.0d0 8.0d0 9.0d0)
	       (list 10.0d0 11.0d0 12.0d0))
	 (list (list 13.0d0 14.0d0 15.0d0)
	       (list 16.0d0 17.0d0 18.0d0))
	 (list (list 19.0d0 20.0d0 21.0d0)
	       (list 22.0d0 23.0d0 24.0d0)))
	:element-type 'double-float))
 #3a(((1.0 2.0 3.0)(4.0 5.0 6.0))
     ((7.0 8.0 9.0)(10.0 11.0 12.0))
     ((13.0 14.0 15.0)(16.0 17.0 18.0))
     ((19.0 20.0 21.0)(22.0 23.0 24.0))))

(my-assert
 (aref fa1 0 0 0)
 1.0)

(my-assert
 (aref fa1 0 0 1)
 2.0)

(my-assert
 (aref fa1 0 0 2)
 3.0)

(my-assert
 (aref fa1 0 1 0)
 4.0)

(my-assert
 (aref fa1 0 1 1)
 5.0)

(my-assert
 (aref fa1 0 1 2)
 6.0)

(my-assert
 (aref fa1 1 0 0)
 7.0)

(my-assert
 (aref fa1 1 0 1)
 8.0)

(my-assert
 (aref fa1 1 0 2)
 9.0)

(my-assert
 (aref fa1 1 1 0)
 10.0)

(my-assert
 (aref fa1 1 1 1)
 11.0)

(my-assert
 (aref fa1 1 1 2)
 12.0)

(my-assert
 (aref fa1 2 0 0)   13.0)

(my-assert
 (aref fa1 2 0 1)
 14.0)

(my-assert
 (aref fa1 2 0 2)
 15.0)

(my-assert
 (aref fa1 2 1 0)   16.0)

(my-assert
 (aref fa1 2 1 1)
 17.0)

(my-assert
 (aref fa1 2 1 2)
 18.0)

(my-assert
 (aref fa1 3 0 0)
 19.0)

(my-assert
 (aref fa1 3 0 1)
 20.0)

(my-assert
 (aref fa1 3 0 2)
 21.0)

(my-assert
 (aref fa1 3 1 0)
 22.0)

(my-assert
 (aref fa1 3 1 1)
 23.0)

(my-assert
 (aref fa1 3 1 1)
 23.0)


;; limits fuer felder

(my-assert
 (let ((s (prin1-to-string array-rank-limit )))
   (or #+xcl (equal s "256")
       #+clisp (equal s "4294967296")
       #+clisp (equal s "65536")
       #+akcl (equal s "64")
       #+gcl (equal s "63")
       #+allegro (equal s "65536")
       #+(or cmu sbcl) (equal s "65529")
       #+ecls (equal s "64")
       #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string array-dimension-limit )))
   (or #+xcl (equal s "17920")
       #+akcl (equal s "16777216")
       #+gcl (equal s "2147483647")
       #+clisp (equal s (prin1-to-string (1+ most-positive-fixnum)))
       #+allegro (equal s "16777216")
       #+(or cmu sbcl) (equal s "536870911")
       #+ecls (equal s "16777216")
       #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string array-total-size-limit )))
   (or #+xcl (equal s "17920")
       #+akcl (equal s "16777216")
       #+clisp (equal s (prin1-to-string (1+ most-positive-fixnum)))
       #+allegro (equal s "16777216")
       #+(or cmu sbcl) (equal s "536870911")
       #+ecls (equal s "16777216")
       #-(or xcl clisp akcl allegro cmu sbcl ecls) "unknown"
       ) )
 t)

;;erzeuge einen einfachen (simple) vector

(my-assert
 (equalp (setq sv (vector (quote a) (quote b) (quote c) 1.0s0 3.7d0
			  4.1))
	 #(a b c 1.0s0 3.7d0 4.1))   t)

(my-assert
 (svref sv 0)   a)

(my-assert
 (svref sv 1)   b)

(my-assert
 (svref sv 2)   c)

(my-assert
 (svref sv 3)   1.0s0)

(my-assert
 (svref sv 4)   3.7d0)

;;pruefe setzen eines elements

(my-assert
 (setf (svref sv 0) (quote test))   test)

(my-assert
 (equalp sv #(test b c 1.0s0 3.7d0 4.1))   t)

;;test array-element-typ ... da2 nicht def.

(my-assert
 (array-element-type sv)   t)

(unintern 'sv)

(my-assert
 (array-element-type da1)
 #+(or xcl allegro cmu sbcl) double-float
 #+clisp t
 #+(or akcl ecls) long-float
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

;;test rang

(my-assert
 (array-rank da1)   3)

(my-assert
 (array-rank fa1)   3)

(unintern 'fa1)

;;test der einzelnen dimensionen

(my-assert
 (array-dimension da1 0)   4)

(my-assert
 (array-dimension da1 1)   2)

(my-assert
 (array-dimension da1 2)   3)

(my-assert
 (array-dimension da1 3)   error)

(unintern 'da1)
;;erzeuge ein 0-dim. feld (pseudoscalar) mit inhalt mod 5

(my-assert
 (progn
   (setq zero
	 (make-array (quote nil)
		     :element-type '(mod 5)))
   t)
 t)

(my-assert
 (array-rank zero)   0)

(my-assert
 (setf (aref zero) 4)   4)

(my-assert
 (setf (aref zero) 1.0)
 #+(or xcl clisp akcl allegro cmu sbcl ecls) type-error
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(unintern 'zero)

;;erzeuge ein 3-dim gen. feld

(my-assert
 (setq a1
       (make-array (list 4 2 3)
		   :initial-contents
		   (list
		    (list (list 'a 'b 'c)
			  (list 1 2 3))
		    (list (list 'd 'e 'f)
			  (list 3 1 2))
		    (list (list 'g 'h 'i)
			  (list 2 3 1))
		    (list (list 'j 'k 'l)
			  (list 0 0 0)))))
 #3a(((a b c)(1 2 3))
     ((d e f)(3 1 2))
     ((g h i)(2 3 1))
     ((j k l)(0 0 0))))

(my-assert
 (aref a1 0 0 0)   a)

(my-assert
 (aref a1 0 0 1)   b)

(my-assert
 (aref a1 0 0 2)   c)

(my-assert
 (aref a1 0 1 0)   1)

(my-assert
 (aref a1 0 1 1)   2)

(my-assert
 (aref a1 0 1 2)   3)

(my-assert
 (aref a1 1 0 0)   d)

(my-assert
 (aref a1 1 0 1)   e)

(my-assert
 (aref a1 1 0 2)   f)

(my-assert
 (aref a1 1 1 0)   3)

(my-assert
 (aref a1 1 1 1)   1)

(my-assert
 (aref a1 1 1 2)   2)

(my-assert
 (aref a1 2 0 0)   g)

(my-assert
 (aref a1 2 0 1)   h)

(my-assert
 (aref a1 2 0 2)   i)

(my-assert
 (aref a1 2 1 0)   2)

(my-assert
 (aref a1 2 1 1)   3)

(my-assert
 (aref a1 2 1 2)   1)

(my-assert
 (aref a1 3 0 0)   j)

(my-assert
 (aref a1 3 0 1)   k)

(my-assert
 (aref a1 3 0 2)   l)

(my-assert
 (aref a1 3 1 0)   0)

(my-assert
 (aref a1 3 1 1)   0)

(my-assert
 (aref a1 3 1 1)   0)

(unintern 'a1)

;;erzeuge ein 2-dim adj.feld, das ueberlagert wird

(my-assert
 (progn (setq m (make-array (list 4 4)
			    :adjustable t
			    :initial-contents
			    (list
			     (list 'alpha 'beta 'gamma 'delta)
			     (list 'epsilon 'zeta 'eta 'theta)
			     (list 'iota 'kappa 'lambda 'mu)
			     (list 'nu 'xi 'omicron 'pi))))
	t)
 t)

(my-assert
 (aref m 0 0)   alpha)

(my-assert
 (aref m 0 1)   beta)

(my-assert
 (aref m 0 2)   gamma)

(my-assert
 (aref m 0 3)   delta)

(my-assert
 (aref m 1 0)   epsilon)

(my-assert
 (aref m 1 1)   zeta)

(my-assert
 (aref m 1 2)   eta)

(my-assert
 (aref m 1 3)   theta)

(my-assert
 (aref m 2 0)   iota)

(my-assert
 (aref m 2 1)   kappa)

(my-assert
 (aref m 2 2)   lambda)

(my-assert
 (aref m 2 3)   mu)

(my-assert
 (aref m 3 0)   nu)

(my-assert
 (aref m 3 1)   xi)

(my-assert
 (aref m 3 2)   omicron)

(my-assert
 (aref m 3 3)   pi)

;;erzeuge ueberl. der zeilen

(my-assert
 (equalp (setq md0 (make-array 4 :displaced-to m))   #(alpha beta gamma
							     delta)) t)

(my-assert
 (equalp (setq md1 (make-array 4 :displaced-to m :displaced-index-offset4))
	 #(epsilon zeta eta theta)) t)


(my-assert
 (equalp (setq md2 (make-array 4 :displaced-to m :displaced-index-offset8))
	 #(iota kappa lambda mu)) t)


(unintern 'md0)
(unintern 'md1)
(unintern 'md2)


;;adjustiere feld m

(my-assert
 (progn (adjust-array m (quote (3 5)) :initial-element (quote baz))
	t)   t)

(my-assert
 (aref m 0 0)   alpha)

(my-assert
 (aref m 0 1)   beta)

(my-assert
 (aref m 0 2)   gamma)

(my-assert
 (aref m 0 3)   delta)

(my-assert
 (aref m 0 4)   baz)

(my-assert
 (aref m 1 0)   epsilon)

(my-assert
 (aref m 1 1)   zeta)

(my-assert
 (aref m 1 2)   eta)

(my-assert
 (aref m 1 3)   theta)

(my-assert
 (aref m 1 4)   baz)

(my-assert
 (aref m 2 0)   iota)

(my-assert
 (aref m 2 1)   kappa)

(my-assert
 (aref m 2 2)   lambda)

(unintern 'm)

;;teste zusammenspiel der schluesselworte

(my-assert
 (progn
   (setq dv (make-array 10 :element-type (quote double-float)
			:initial-contents(quote (0.0d0 1.0d0 2.0d0 3.0d0 4.0d0 5.0d0 6.0d0 7.0d0 8.0d0 9.0d0))))
   t)
 t)

#| *************************************************************************** ;


(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote ((1.0d0 2.0d0) (3.0d0 4.0d0 5.0d0)))))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote
					 ((1.0d0 2.0d0) (3.0d0 4.0d0) :displaced-to dv :displaced-index-offset
					  8))))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :initial-contents (quote ((1.0d0 2.0d0) (3.0d0 4.0d0))) :displaced-to
		      dv
		      :displaced-index-offset 8))   error

(setq dve (make-array (quote (2 2)) :element-type (quote double-float)

		      :displaced-to dv :displaced-index-offset 8))   error

***************************************************************************|#

(my-assert
 (aref dv 0)   0.0d0)

(my-assert
 (aref dv 1)   1.0d0)

(my-assert
 (aref dv 2)   2.0d0)

(my-assert
 (aref dv 3)   3.0d0)

(my-assert
 (aref dv 4)   4.0d0)

(my-assert
 (aref dv 5)   5.0d0)

(my-assert
 (aref dv 6)   6.0d0)

(my-assert
 (aref dv 7)   7.0d0)

(my-assert
 (aref dv 8)   8.0d0)

(my-assert
 (aref dv 9)   9.0d0)

(my-assert
 (setf (aref dv 5) -5.0d0)   -5.0d0)

(unintern 'dv)

;;definiere testfkt fuer indices

(my-assert
 (defun array-index-test (a &rest subs) (unless
					    (apply (function array-in-bounds-p) a subs)
					  (return-from array-index-test (quote error))) (=
					  (apply (function array-row-major-index) a subs) (apply (function +)
												 (maplist
												  (function (lambda (x y) (* (car x) (apply (function *) (cdr y)))))
												  subs
												  (array-dimensions a)))))   array-index-test)

(my-assert
 (array-index-test (make-array (quote (5 4 3 2 1))) 4 2 2 1 0)   t)

(my-assert
 (array-index-test (make-array (quote (5 4 3 2 1))) 3 4 2 1 2)   error)

;;test bitfelder

(my-assert
 (setq bvzero (make-array 100 :element-type (quote bit) :initial-element
			  0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(my-assert
 (setq bvone (make-array 100 :element-type (quote bit) :initial-element
			 1))
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(my-assert
 (setq bv3 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(my-assert
 (setq bv2 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(my-assert
 (setq bv1 (make-array 100 :element-type (quote bit) :initial-element
		       0))
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

;;setze bitfelder

(my-assert
 (dotimes (i 50 bv1) (setf (sbit bv1 (* i 2)) 1))
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(my-assert
 (dotimes (i 50 bv2) (setf (bit bv2 (* i 2)) 1))
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(my-assert
 (equalp bv1 bv2)   t)

(my-assert
 (dotimes (i 25 bv3)
   (setf (sbit bv3 (* i 4))
	 1))
 #*1000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000)

(my-assert
 (bit-and bv1 bv3)
 #*1000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000)

(my-assert
 (bit-ior bv1 bv3)
 #*1010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010)

(my-assert
 (bit-xor bv1 bv3)
 #*0010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010)

(my-assert
 (bit-eqv bv1 bv3)
 #*1101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101)

(my-assert
 (bit-nand bv1 bv3)
 #*0111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111)

(my-assert
 (bit-andc1 bv1 bv3)
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(my-assert
 (bit-andc2 bv1 bv3)
 #*0010001000100010001000100010001000100010001000100010001000100010001000100010001000100010001000100010)

(my-assert
 (bit-orc1 bv1 bv3)
 #*1101110111011101110111011101110111011101110111011101110111011101110111011101110111011101110111011101)

(my-assert
 (bit-orc2 bv1 bv3)
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(my-assert
 (bit-not bv1)
 #*0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101)

(my-assert
 (bit-not bvzero)
 #*1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111)

(my-assert
 (bit-not bvone)
 #*0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(unintern 'bv1)
(unintern 'bv2)
(unintern 'bv3)
(unintern 'bvzero)
(unintern 'bvone)

;;teste operationen mit fillpointern

(my-assert
 (make-array (quote (3 4 5)) :fill-pointer t)   error)

(my-assert
 (equalp (make-array 5 :fill-pointer 5)
	 #+(or xcl cmu sbcl)
	 #(0 0 0 0 0)
	 #-(or xcl cmu sbcl)
	 #(nil nil nil nil nil))
 t)

(my-assert
 (make-array 5 :fill-pointer -5)   error)

;;allgem. vector mit fillpointer

(my-assert
 (progn (setq vmf (make-array 5 :fill-pointer 0)) t)   t)

(my-assert
 (fill-pointer vmf)   0)

(my-assert
 (vector-push (quote a) vmf)   0)

(my-assert
 (fill-pointer vmf)   1)

(my-assert
 (vector-push (quote b) vmf)   1)

(my-assert
 (vector-push (quote c) vmf)   2)

(my-assert
 (vector-push (quote d) vmf)   3)

(my-assert
 (vector-push (quote e) vmf)   4)

(my-assert
 (vector-push (quote voll) vmf)   nil)

(my-assert
 (vector-pop vmf)   e)

(my-assert
 (vector-pop vmf)   d)

(my-assert
 (vector-pop vmf)   c)

(my-assert
 (vector-pop vmf)   b)

(my-assert
 (vector-pop vmf)   a)

(my-assert
 (vector-pop vmf)   error)

;;adjustabler allgem. vector mit fillpointer

(unintern 'vmf)

(my-assert
 (progn (setq vmfa (make-array 5 :fill-pointer 0 :adjustable t)) t)
 t)

(my-assert
 (fill-pointer vmfa)   0)

(my-assert
 (vector-push-extend (quote a) vmfa)   0)

(my-assert
 (fill-pointer vmfa)   1)

(my-assert
 (vector-push-extend (quote b) vmfa)   1)

(my-assert
 (vector-push-extend (quote c) vmfa)   2)

(my-assert
 (vector-push-extend (quote d) vmfa)   3)

(my-assert
 (vector-push-extend (quote e) vmfa)   4)

(my-assert
 (vector-push-extend (quote voll) vmfa)   5)

(my-assert
 (vector-pop vmfa)   voll)

(my-assert
 (vector-pop vmfa)   e)

(my-assert
 (vector-pop vmfa)   d)

(my-assert
 (vector-pop vmfa)   c)

(my-assert
 (vector-pop vmfa)   b)

(my-assert
 (vector-pop vmfa)   a)

;;doppeltgen. vector mit fillpointer

(unintern 'vmfa)

(my-assert
 (progn
   (setq vmfd (make-array 5 :fill-pointer 0 :element-type (quote double-float)))
   t)   t)

(my-assert
 (fill-pointer vmfd)   0)

(my-assert
 (vector-push 0.0d0 vmfd)   0)

(my-assert
 (fill-pointer vmfd)   1)

(my-assert
 (vector-push 1.0d0 vmfd)   1)

(my-assert
 (vector-push 2.0d0 vmfd)   2)

(my-assert
 (vector-push 3.0d0 vmfd)   3)

(my-assert
 (vector-push 4.0d0 vmfd)   4)

(my-assert
 (vector-push 5.0d0 vmfd)   nil)

(my-assert
 (vector-pop vmfd)   4.0d0)

(my-assert
 (vector-pop vmfd)   3.0d0)

(my-assert
 (vector-pop vmfd)   2.0d0)

(my-assert
 (vector-pop vmfd)   1.0d0)

(my-assert
 (vector-pop vmfd)   0.0d0)

(my-assert
 (vector-pop vmfd)   error)

;;doppeltgen. adjust. vector mit fillpointer

(unintern 'vmfd)

(my-assert
 (progn (setq vmfad
	      (make-array 5 :fill-pointer 0 :element-type (quote double-float) :adjustable
			  t))
	t)   t)

(my-assert
 (fill-pointer vmfad)   0)

(my-assert
 (vector-push-extend 0.0d0 vmfad)   0)

(my-assert
 (fill-pointer vmfad)   1)

(my-assert
 (vector-push-extend 1.0d0 vmfad)   1)

(my-assert
 (vector-push-extend 2.0d0 vmfad)   2)

(my-assert
 (vector-push-extend 3.0d0 vmfad)   3)

(my-assert
 (vector-push-extend 4.0d0 vmfad)   4)

(my-assert
 (vector-push-extend 5.0d0 vmfad)   5)

(my-assert
 (vector-pop vmfad)   5.0d0)

(my-assert
 (vector-pop vmfad)   4.0d0)

(my-assert
 (vector-pop vmfad)   3.0d0)

(my-assert
 (vector-pop vmfad)   2.0d0)

(my-assert
 (vector-pop vmfad)   1.0d0)

(my-assert
 (vector-pop vmfad)   0.0d0)

(my-assert
 (vector-push-extend 5.0s0 vmfad)
 #+(or xcl gcl allegro cmu sbcl) error
 #+(or clisp ecls (and akcl (not gcl))) 0
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(unintern 'vmfad)
