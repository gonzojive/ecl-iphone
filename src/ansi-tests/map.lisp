;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (setf a-vector (make-array 10))
 #+(or XCL cmu sbcl) #(0 0 0 0 0 0 0 0 0 0)
 #+(or CLISP AKCL ALLEGRO ecls) #(NIL NIL NIL NIL NIL NIL NIL NIL NIL NIL)
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(my-assert
 (do ((i 0 (1+ i))
      (n (length a-vector)))
     ((= i n))
   (when (null (aref a-vector i))
     (setf (aref a-vector i) 0)))
 nil)

(my-assert
 (setq liste '(a b c d))
 (a b c d))

(my-assert
 (setq x 'anfangswert-von-x)
 anfangswert-von-x)

(my-assert
 (do ((x liste (cdr x))
      (oldx x x))
     ((null x))
   (print oldx) (print x))
 nil)

(my-assert
 (defun list-reverse(list)
   (do ((x list (cdr x))
	(y '() (cons (car x) y)))
       ((endp x) y)))
 list-reverse)

(my-assert
 (list-reverse '(a b c d))
 (d c b a))

(my-assert
 (setq foo '(a b c d))
 (a b c d))

(my-assert
 (setq bar '(1 2 3 4))
 (1 2 3 4))

(my-assert
 (defun fkt(a b) (cons a b))
 fkt)

;; mapcar

(my-assert
 (mapcar #'abs '(3 -4 2 -5 -6))
 (3 4 2 5 6))

(my-assert
 (mapcar #'cons '(a b c) '(1 2 3))
 ((a . 1) (b . 2) (c . 3)))


(my-assert
 (mapcar #'fkt foo bar)
 ((a . 1)(b . 2)(c . 3)(d . 4)))

(my-assert
 (do ((x foo (cdr x))
      (y bar (cdr y))
      (z '() (cons (fkt (car x) (car y)) z)))
     ((or (null x) (null y))
      (nreverse z)))
 ((a . 1)(b . 2)(c . 3)(d . 4)))

;; dolist
(my-assert
 (let    ((l '(1 2 3))
	  (r 0))
   (dolist (x l r)
     (setf r (+ r  x)) ))
 6)


;; dolist
(my-assert
 (let ((l '(1 2 3)))
   (dolist (x l)(if (> 0 x)(incf x)(return 10))))
 10)

(my-assert
 (let ((l '(1 2 3)))
   (dolist (x l )(incf x)))
 nil)

;; dotimes

(my-assert
 (let ((s 0))
   (dotimes (i (+ 1 9)s)(setf s (+ s i))))
 45)


(my-assert
 (dolist (x '(a b c d)) (prin1 x) (princ " "))
 nil)

(my-assert
 (defun palindromep (string &optional
			    (start 0)
			    (end (length string)))
   (dotimes (k (floor (- end start) 2) t)
     (unless (char-equal (char string (+ start k))
			 (char string (- end k 1)))
       (return nil))))
 palindromep)

(my-assert
 (palindromep "Able was I ere I saw Elba")
 t)

(my-assert
 (palindromep "einnegermitgazellezagtimregennie")
 t)

(my-assert
 (palindromep "eisgekuehlter bommerlunder")
 nil)

(my-assert
 (palindromep (remove-if-not #'alpha-char-p
			     "A man, a plan, a canal -- Panama"))
 t)

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
 ((A) (B) (C)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
							       (1 2 3)))
 ((A 1) (B 2) (C 3)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
							       (1 2)))
 ((A 1) (B 2)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (C)) (QUOTE (1
								  2)))
 ((C 1)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (C)) (QUOTE (1
								    2)) (U V W))
 ERROR)

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (C)) (QUOTE (1
								    2))
	 (QUOTE (U V W)))
 ((C 1)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y))) (QUOTE (A B C)) (QUOTE
								 (1 2 3))
	 (QUOTE (U V W)))
 ((A 1) (B 2) (C 3)))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								   (1 2 3))
	 (QUOTE (U V W)))
 ((A 1 U) (B 2 V) (C 3 W)))

;; mapc
(my-assert
 (mapc #'abs '(3 -4 2 -5 -6))
 (3 -4 2 -5 -6))

(my-assert
 (MAPC (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								 (1 2 3))
       (QUOTE (U I V)))
 (A B C))

(my-assert
 (MAPCAR (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								   (1 2 3))
	 (QUOTE (U I V)))
 ((A 1 U) (B 2 I) (C 3 V)))

(my-assert
 (mapl #'(lambda (x y)(cons x y))'(a b c d)'(1 2 3 4))
 (a b c d))

(my-assert
 (MAPL (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								 (1 2 3))
       (QUOTE (U I V)))
 (A B C))

;; maplist

(my-assert
 (maplist #'(lambda (x)(cons 'foo x))'(a b c d))
 ((foo a b c d)(foo b c d)(foo c d)(foo d)))


(my-assert
 (maplist #'(lambda (x) (if (member (car x)(cdr x)) 0 1))
	  '(a b a c d b c))
 (0 0 1 0 1 1 1))


(my-assert
 (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C))
	  (QUOTE (1 2 3)) (QUOTE (U I V)))
 (((A B C) (1 2 3) (U I V)) ((B C) (2 3) (I V)) ((C) (3) (V))))

(my-assert
 (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C))
	  (QUOTE (1 2 3)) (QUOTE (U I)))
 (((A B C) (1 2 3) (U I)) ((B C) (2 3) (I))))

(my-assert
 (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								    (1 2))
	  (QUOTE (U I V)))
 (((A B C) (1 2) (U I V)) ((B C) (2) (I V))))

(my-assert
 (MAPLIST (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B)) (QUOTE
								  (1 2 3))
	  (QUOTE (U I V)))
 (((A B) (1 2 3) (U I V)) ((B) (2 3) (I V))))

;; mapcon

(my-assert
 (mapcon #'(lambda (x)(and (oddp (car x))(list (car x))))'(5 4 3 2 1))
 (5 3 1))


(my-assert
 (MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B)) (QUOTE
								 (1 2 3))
	 (QUOTE (U I V)))
 ((A B) (1 2 3) (U I V) (B) (2 3) (I V)))

(my-assert
 (MAPCON (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								   (1 2 3))
	 (QUOTE (U I V)))
 ((A B C) (1 2 3) (U I V) (B C) (2 3) (I V) (C) (3) (V)))

;; mapcan

(my-assert
 (mapcan #'(lambda (x)(and (numberp x)(list x)))'(a 1 b c 3 4 d 5))
 (1 3 4 5))

(my-assert
 (MAPCAN (FUNCTION (LAMBDA (X Y Z) (LIST X Y Z))) (QUOTE (A B C)) (QUOTE
								   (1 2 3))
	 (QUOTE (U I V)))
 (A 1 U B 2 I C 3 V))

(my-assert
 (MAPCAN (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
							       (1 2 3)))
 (A 1 B 2 C 3))

(my-assert
 (MAPCAN (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
 (A B C))

(my-assert
 (MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
 ((A B C) (B C) (C)))

(my-assert
 (MAPCON (FUNCTION (LAMBDA (X Y) (LIST X Y))) (QUOTE (A B C)) (QUOTE
							       (1 2)))
 ((A B C) (1 2) (B C) (2)))

(my-assert
 (MAPCON (FUNCTION (LAMBDA (X) (LIST X))) (QUOTE (A B C)))
 ((A B C) (B C) (C)))

