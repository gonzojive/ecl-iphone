;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)
;;;
;;; testfile nach steele-beispielen
;;;

;; 7.3


(my-assert
 (let ((f '+))
   (apply f '(1 2)))
 3)

(my-assert
 (let    ((f #'-))
   (apply f '(1 2)))
 -1)

(my-assert
 (apply #'max 3 5 '(2 7 3))
 7)

(my-assert
 (apply 'cons '((+ 2 3) 4))
 ((+ 2 3) . 4))


(my-assert
 (apply #'+ '())
 0)

(my-assert
 (apply #'(lambda (&key a b)(list a b)) '(:b 3))
 (nil 3))


(my-assert
 (funcall '+ 2 3)
 5)

(my-assert
 (let    ((c (symbol-function '+)))
   (funcall c 1 2 3 4))
 10)


;;abschnitt 7.4

;; progn
(my-assert
 (progn 1 2 3)
 3)

(my-assert
 (progn (+ 2 1) 2)
 2)

(my-assert
 (progn 1 2 (values  2 3))
 2)

(my-assert
 (progn)
 nil)


;; prog1
(my-assert
 (prog1 1 2 3)
 1)

(my-assert
 (prog1 3 (+ 1 2) 2)
 3)

(my-assert
 (prog1 (values  2 3) 1 2 )
 2)

(my-assert
 (let ((x '(a b c)))
   (prog1 (car x)(rplaca x 'foo)))
 a)

;; prog2

(my-assert
 (prog2 1 2 3)
 2)

(my-assert
 (prog2  (+ 1 2) 2 3)
 2)

(my-assert
 (prog2 1 (values  2 3) 4)
 2)

;; 7.5

;; let
(setf a 0)

(my-assert
 (let ((a 1)(b 2) c)
   (declare (integer a b))
   (list a b c))
 (1 2 nil))


(my-assert
 (let ((a 1)
       (b a))
   (declare (integer a b))
   (list a b))
 (1 0))

(my-assert
 (let (x239)
   (declare (special x239))
   (symbol-value 'x239))
 nil)

;; let*
(my-assert
 (let* ((a 1)(b 2) c )
   (declare (integer a b))
   (list a b c))
 (1 2 nil))


(my-assert
 (let* ((a 1)(b a))
   (declare (integer a b))
   (list a b))
 (1 1))

;; compiler-let (?)


;; progv

(my-assert
 (progv
     '(a b c)
     '(1 2 3)

   (+ a b c))
 6)

(unintern 'a)
(unintern 'b)
(unintern 'c)

(my-assert
 (progv
     '(a b c)
     '(1 2)

   (list a b c))
 error)

(my-assert
 (let ((v '(a b c))
       (val '(3 2 1)))
   (progv v val (mapcar #'eval v)))
 (3 2 1))


;; flet

(my-assert
 (flet ((plus (a b)(+ a b))
	(minus (a b)(- a b)))
   (list (plus 1 2)(minus 1 2)))
 (3 -1))


(my-assert
 (list (flet ( (+ (a b)(- a b)))(+ 3 2))(+ 3 2))
 (1 5))

(my-assert
 (flet ((+ (a b)(+ (+ a b a) b)))(+ 3 2))
 10)

;; labels
(my-assert
 (labels ((queue (l)(if (car l)(queue (cdr l))'ende)))(queue '(1 2 3)))
 ende)

(my-assert
 (labels ((+ (a b)(* a (+ a a b))))(+ 1 2 3))
 error)

;; macrolet ?


;; 7.6

;; if

(my-assert
 (let ((a t)(b nil))(list (if a 1 2)(if b 1 2)(if a 1)(if b 1)))
 (1 2 1 nil))


;; when
(my-assert
 (let ((a t)(b nil))(list (when a 1 2)(when b 1 2)(when a 1)))
 (2 nil 1))


;; unless
(my-assert
 (let ((a t)(b nil))(list (unless a 1 2)(unless b 1 2)(unless a 1)))
 (nil 2 nil))


;; cond
(my-assert
 (let ((a t)(b 10)(c nil))
   (list (cond (a 1)(t 'end))(cond (b)(t 'end))(cond (c 1)(t 'end))))
 (1 10 end))


;; case
(my-assert
 (case (+  1 2)
   (1 -1)
   (2 -2)
   (3 -3))
 -3)

(my-assert
 (case (+  1 2)
   (1 -1)
   (2 -2))
 nil)


;; (case (+  1 2)
;;       (1 -1)
;;       (2 -2)
;;       (1 -1)
;;       (3 -3))
;; error


(my-assert
 (case (+  1 2)
   ((1 3) -1)
   (2 -2)
   (otherwise 100))
 -1)


;;
;; (case (+  1 2)
;;       ((1 3) -1)
;;       ((2 1) -2)
;;       (t 100))
;; error          ;weil ein key nur einmal erscheinen darf!
;;



;; typecase

(my-assert
 (typecase (+  1 2)
   (list -2)
   (null -3)
   (integer -1))
 -1)

;; 7.7

;; block

(my-assert
 (block blocktest (if t (return 0) ) 1)
 error)

(my-assert
 (block blocktest (if t (return-from blocktest 0) ) 1)
 0)


(my-assert
 (block blocktest (if nil (return-from blocktest 0) ) 1)
 1)


(my-assert
 (block blocktest (catch 'catcher
		    (if t (throw 'catcher 0) ) 1))
 0)


;; 7.8

;; 7.8.1

;; loop

(my-assert
 (let ((i 10))
   (loop (if (< (decf i) 1)(return i))))
 0)


(my-assert
 (let ((i 10))
   (catch 'catcher
     (loop (if (< (decf i) 1)(return i)))))
 0)

;; 7.8.2
;; do,do*

(setf a 0)

(my-assert
 (do ((a 1 (+ a 1))(b a))
     ((> a 9) (list b c))
   (setf c (+ a b)))
 (0 9))

(my-assert
 (do* ((a 1 (+ a 1))(b a))
     ((> a 9) b)
   )
 1)

(my-assert
 (let ((a 0))
   (do* ((a 1 (+ a 1))(b a))
       ((> a 9) a)
     (declare (integer a b)))
   a)
 0)



;; 7.8.3


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
   (dotimes (i (+ 1 9)s)
     (setf s (+ s i))))
 45)


;; 7.8.4


;; mapcar

(my-assert
 (mapcar #'abs '(3 -4 2 -5 -6))
 (3 4 2 5 6))

(my-assert
 (mapcar #'cons '(a b c) '(1 2 3))
 ((a . 1) (b . 2) (c . 3)))


;; maplist

(my-assert
 (maplist #'(lambda (x)(cons 'foo x))'(a b c d))
 ((foo a b c d)(foo b c d)(foo c d)(foo d)))


(my-assert
 (maplist #'(lambda (x) (if (member (car x)(cdr x)) 0 1))
	  '(a b a c d b c))
 (0 0 1 0 1 1 1))


;; mapc
(my-assert
 (mapc #'abs '(3 -4 2 -5 -6))
 (3 -4 2 -5 -6))

;; mapc

(my-assert
 (mapl #'(lambda (x y)(cons x y))'(a b c d)'(1 2 3 4))
 (a b c d))

;; mapcan

(my-assert
 (mapcan #'(lambda (x)(and (numberp x)(list x)))'(a 1 b c 3 4 d 5))
 (1 3 4 5))

;; mapcon

(my-assert
 (mapcon #'(lambda (x)(and (oddp (car x))(list (car x))))'(5 4 3 2 1))
 (5 3 1))

;; 7.8.5

;; tagbody
(my-assert
 (let ((a 0))
   (tagbody (if nil (go tag0) (go tag1))
	    (this will never be reached)
	    tag0
	    (setf a 1)
	    tag1
	    (setf a 2))
   a)
 2)

(my-assert
 (let ((a 0))
   (tagbody (if t (go tag0) (go tag1))
	    (this will never be reached)
	    tag0
	    (setf a 1)
	    )
   a)
 ;;  cmucl compiles on the fly and therefore signals an error
 #-(or cmu sbcl) 1
 #+(or cmu sbcl) error)



;; prog*

(my-assert
 (let ((z '(1 0)))
   (prog* ((y z)(x (car y)))
	  (return x)))
 1)

(my-assert
 (prog  (a (b 1))
	(if a (go tag0) (go tag1))
	(this will never be reached)
	tag0
	(setf a 1)
	(this will never be reached)
	tag1
	(setf a 2))
 nil)



(my-assert
 (prog  (a (b 1))
	(if a (return nil) (go tag1))
	(this will never be reached)
	tag0
	(return (list a 1))
	tag1
	(setf a 2)
	(go tag0))
 (2 1))


;; 7.9

;; multiple-value-bind
(my-assert
 (defun adder (x y)(values (+ 1 x)(+ 1 y) ) )
 adder)


(my-assert
 (multiple-value-bind (a b)(adder 1 2)(+ a b))
 5)

(my-assert
 (defun adder (x y)(values-list (list  (+ 1 x)(+ 1 y))))
 adder)


(my-assert
 (multiple-value-bind (a b)(adder 1 2)(+ a b))
 5)


(my-assert
 (multiple-value-list (floor -3 4))
 (-1 1))


(my-assert
 (multiple-value-call #'+ (floor 5 3)(floor 19 4))
 10)

(my-assert
 (multiple-value-bind (c d)
     (multiple-value-prog1 (floor -3 4) (+ 1 2))
   (list c d))
 (-1 1))


(my-assert
 (multiple-value-bind (x)(floor 5 3)(list x))
 (1))


(my-assert
 (multiple-value-bind (x y)(floor 5 3)(list x y))
 (1 2))


(my-assert
 (multiple-value-bind (x y z)(floor 5 3)(list x y z))
 (1 2 nil))




(my-assert
 (multiple-value-setq
     (a b)
   (values 10 20))
 10)

(my-assert
 b
 20)

(unintern 'a)
(unintern 'b)
;; 7.10

;; catch/throw/unwind-protect

