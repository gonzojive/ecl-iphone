;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (ACONS 'A 'B NIL)
 ((A . B)))

(my-assert
 (ACONS 'A 'B
	'((C . D)))
 ((A . B)
  (C . D)))

(my-assert
 (PAIRLIS '(A B C)
	  '(1 2))
 #+XCL
 ((B . 2)
  (A . 1))
 #-XCL
 ERROR)

(my-assert
 (PAIRLIS '(A B C)
	  '(1 2 3))
 #+(or XCL CLISP ALLEGRO cmu sbcl ecls)
 ((C . 3)
  (B . 2)
  (A . 1))
 #+AKCL ((A . 1) (B . 2) (C . 3))
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(my-assert
 (ASSOC 'A
	'((B C)
	  (A U)
	  (A I)))
 (A U))

(my-assert
 (ASSOC 'A
	'((B C)
	  ((A)
	   U)
	  (A I)))
 (A I))

(my-assert
 (ASSOC 'A
	'((B C)
	  ((A)
	   U)
	  (A I))
	:KEY
	#'(LAMBDA (X)
		  (IF (LISTP X)
		      (CAR X))))
 ((A)
  U))

(my-assert
 (ASSOC 'A
	'((B C)
	  A
	  ((A)
	   U)
	  (A I))
	:KEY
	#'(LAMBDA (X)
		  (IF (LISTP X)
		      (CAR X))))
 #-(or GCL ALLEGRO cmu sbcl)
 ((A) U)
 #+(or GCL ALLEGRO cmu sbcl)
 TYPE-ERROR)

(my-assert
 (ASSOC 'A
	'((B C)
	  A
	  ((A)
	   U)
	  (A I))
	:KEY
	#'(LAMBDA (X)
		  (IF (ATOM X)
		      X)))
 #-(or GCL ALLEGRO cmu sbcl) (A I)
 #+(or GCL ALLEGRO cmu sbcl)
 TYPE-ERROR)

(my-assert
 (ASSOC 'A
	'((B C)
	  A
	  ((A)
	   U)
	  (A I))
	:TEST
	#'(LAMBDA (X Y)
		  (IF (LISTP Y)
		      (EQL (CAR Y)
			   X))))
 #-(or GCL ALLEGRO cmu sbcl) ((A) U)
 #+(or GCL ALLEGRO cmu sbcl)
 TYPE-ERROR)

(my-assert
 (ASSOC 'A
	'((B C)
	  A
	  ((A)
	   U)
	  (A I))
	:TEST
	#'(LAMBDA (X Y)
		  (IF (ATOM Y)
		      (EQL Y X))))
 #-(or GCL ALLEGRO cmu sbcl) (A I)
 #+(or GCL ALLEGRO cmu sbcl) ERROR)

(my-assert
 (ASSOC 'A
	'((B C)
	  A
	  ((A)
	   U)
	  (A I))
	:TEST-NOT
	#'(LAMBDA (X Y)
		  (IF (ATOM Y)
		      (EQL Y X))))
 #-ALLEGRO (B C)
 #+ALLEGRO ERROR)

(my-assert
 (ASSOC-IF 'NUMBERP
	   '((A . 3)
	     (3 . A)))
 (3 . A))

(my-assert
 (ASSOC-IF 'SYMBOLP
	   '((A . 3)
	     (3 . A)))
 (A . 3))

(my-assert
 (ASSOC-IF-NOT 'SYMBOLP
	       '((A . 3)
		 (3 . A)))
 (3 . A))

(my-assert
 (ASSOC-IF-NOT 'NUMBERP
	       '((A . 3)
		 (3 . A)))
 (A . 3))

(my-assert
 (RASSOC 'A
	 '((1 . B)
	   (2 . A)))
 (2 . A))

(my-assert
 (RASSOC-IF 'SYMBOLP
	    '((1 . B)
	      (2 . A)))
 (1 . B))

(my-assert
 (RASSOC-IF 'SYMBOLP
	    '((1 . 3)
	      (2 . A)))
 (2 . A))

(my-assert
 (RASSOC-IF-NOT 'SYMBOLP
		'((1 . 3)
		  (2 . A)))
 (1 . 3))

