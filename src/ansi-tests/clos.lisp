;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)


#-(OR CMU SBCL)
(my-assert
 (use-package "CLOS")
 T)

#+SBCL
(my-assert
 (use-package "SB-PCL")
 T)

(my-assert
 (unintern '<C1>)
 T)

(my-assert
 (progn
   (defclass <C1> ()
     ((x :initform 0 :accessor x-val :reader get-x :writer set-x :initarg :x)
      (y :initform 1 :accessor y-val :reader get-y :writer set-y :initarg :y)))
   ())
 NIL)

(my-assert
 (progn
   (defclass <C2> (<C1>)
     ((z :initform 0 :accessor z-val :reader get-z :writer set-z :initarg :z)))
   ())
 NIL)

(my-assert
 (defparameter a (make-instance (find-class '<C1>) :x 10))
 A)

(my-assert
 (x-val a)
 10)

(my-assert
 (y-val a)
 1)

(my-assert
 (setf (x-val a) 20)
 20)

(my-assert
 (x-val a)
 20)

(my-assert
 (get-x a)
 20)

(my-assert
 (set-x 10 a)
 10)

(my-assert
 (x-val a)
 10)

(my-assert
 (defparameter b (make-instance (find-class '<C2>) :x 10 :y 20 :z 30))
 B)

(my-assert
 (x-val b)
 10)

(my-assert
 (y-val b)
 20)

(my-assert
 (z-val b)
 30)

(my-assert
 (progn
   (defgeneric f (x y)
     (:method ((x t) (y t))
	      (list x y)))
   (defmethod f ((i integer) (j number))
     (+ i j))
   (defmethod f ((s1 string) (s2 string))
     (concatenate 'string s1 s2))
   ())
 NIL)

(my-assert
 (f t t)
 (T T))

(my-assert
 (f 2 3)
 5)

(my-assert
 (f 2 3.0)
 5.0)

(my-assert
 (f 2.0 3)
 (2.0 3))

(my-assert
 (f "ab" "cd")
 "abcd")

(my-assert
 (f 1 "abc")
 (1 "abc"))

(my-assert
 (progn
   (defgeneric f (x y)
     (:method ((x t) (y t))
	      (list x y))
     (:method ((i number) (j integer))
	      (list (call-next-method) (- i j)))
     (:method ((i integer) (j number))
	      (list (call-next-method) (+ i j))))
   ())
 NIL)

(my-assert
 (f 'x 'y)
 (X Y))

(my-assert
 (f 1 2)
 (((1 2) -1) 3))

(my-assert
 (f 1 2.0)
 ((1 2.0) 3.0))

(my-assert
 (f 1.0 2)
 ((1.0 2) -1.0))

(my-assert
 (progn
   (defgeneric g (x)
     (:method ((x null))
	      (cons 'null (call-next-method)))
     (:method ((x list))
	      (if (next-method-p)
		  (cons 'list (call-next-method))
		  '(list$)))
     (:method ((x symbol))
	      (if (next-method-p)
		  (cons 'symbol (call-next-method))
		  '(symbol$))))
   ())
 NIL)

(my-assert
 (g 'x)
 (SYMBOL$))

(my-assert
 (g '(x))
 (LIST$))

(my-assert
 (g '())
 (NULL SYMBOL LIST$)
 "Class precedence list for NULL:

null, symbol, list, sequence, t")

(my-assert
 (defvar hl)
 HL)

(my-assert
 (progn
   (defgeneric hgen (x)
     (:method ((x integer))
	      (setf hl (cons 'i-primary-1 hl))
	      (call-next-method)
	      (setf hl (cons 'i-primary-2 hl)))
     (:method :before ((x integer))
	      (setf hl (cons 'i-before hl)))
     (:method :after ((x integer))
	      (setf hl (cons 'i-after hl)))
     (:method :around ((x integer))
	      (setf hl (cons 'i-around-1 hl))
	      (call-next-method)
	      (setf hl (cons 'i-around-2 hl)))
     (:method ((x number))
	      (setf hl (cons 'n-primary-1 hl))
	      (call-next-method)
	      (setf hl (cons 'n-primary-2 hl)))
     (:method :before ((x number))
	      (setf hl (cons 'n-before hl)))
     (:method :after ((x number))
	      (setf hl (cons 'n-after hl)))
     (:method :around ((x number))
	      (setf hl (cons 'n-around-1 hl))
	      (call-next-method)
	      (setf hl (cons 'n-around-2 hl)))
     (:method ((x t))
	      (setf hl (cons 'innermost hl))))
   (defun h (x)
     (setf hl '()) (hgen x) (reverse hl))
   )
 H)

(my-assert
 (h 'abc)
 (INNERMOST))

(my-assert
 (h 3.14)
 (N-AROUND-1 N-BEFORE N-PRIMARY-1 INNERMOST N-PRIMARY-2 N-AFTER N-AROUND-2))

(my-assert
 (h 3)
 (I-AROUND-1 N-AROUND-1 I-BEFORE N-BEFORE I-PRIMARY-1 N-PRIMARY-1 INNERMOST
	     N-PRIMARY-2 I-PRIMARY-2 N-AFTER I-AFTER N-AROUND-2 I-AROUND-2
	     ))

(my-assert
 (unintern '<C1>)
 T)

(my-assert
 (progn
   (defclass <C1> ()
     ((x :initform 0 :accessor x-val :initarg :x)
      (y :initform 1 :accessor y-val :initarg :y)))
   ())
 NIL)

(my-assert
 (defparameter a (make-instance (find-class '<C1>) :x 10))
 A)

(my-assert
 (defparameter b (make-instance (find-class '<C1>) :y 20 :x 10))
 B)

(my-assert
 (defparameter c (make-instance (find-class '<C1>)))
 C)

(my-assert
 (x-val a)
 10)

(my-assert
 (y-val a)
 1)

(my-assert
 (x-val b)
 10)

(my-assert
 (y-val b)
 20)

(my-assert
 (x-val c)
 0)

(my-assert
 (y-val c)
 1)

(my-assert
 (unintern '<C1>)
 T)

(my-assert
 (progn
   (defclass <C1> ()
     ((x :initform 0 :accessor x-val :initarg :x)
      (y :initform 1 :accessor y-val :initarg :y)))
   (defmethod initialize-instance :after ((instance <C1>) &rest initvalues)
     (if (= (x-val instance) 0)
	 (setf (x-val instance) (y-val instance))))
   ())
 NIL)

(my-assert
 (x-val (make-instance (find-class '<C1>)))
 1)

(my-assert
 (x-val (make-instance (find-class '<C1>) :x 10))
 10)

(my-assert
 (x-val (make-instance (find-class '<C1>) :y 20))
 20)

(my-assert
 (x-val (make-instance (find-class '<C1>) :x 10 :y 20))
 10)

(my-assert
 (unintern '<C1>)
 T)

(my-assert
 (eq (class-of ())               (find-class 'null))
 T)

(my-assert
 (eq (class-of t)                (find-class 'symbol))
 T)

(my-assert
 (eq (class-of 10)
     (find-class #+(or ALLEGRO cmu sbcl) 'fixnum
		 #-(or ALLEGRO cmu sbcl) 'integer))
 T)

(my-assert
 (eq (class-of 10.0)
     (find-class #+(or ALLEGRO cmu sbcl) 'single-float
		 #-(or ALLEGRO cmu sbcl) 'float))
 T)

(my-assert
 (eq (class-of '(a b))
     (find-class 'cons))
 T)

(my-assert
 (eq (class-of "abc")
     (find-class #+(OR CMU SBCL) 'simple-string
		 #-(OR CMU SBCL) 'string))
 T)

(my-assert
 (eq (class-of '#(1 2))
     (find-class #+(OR CMU SBCL) 'simple-vector
		 #-(OR CMU SBCL) 'vector))
 T)

(my-assert
 (eq (class-of #'car)
     (find-class 'function))
 T)

(my-assert
 (eq (class-of #'make-instance)
     (find-class 'standard-generic-function))
 T)

(my-assert
 (eq (class-of '#2a((a) (b)))
     (find-class #+(OR CMU SBCL) 'simple-array
		 #-(OR CMU SBCL) 'array))
 T)

(my-assert
 (eq (class-of *standard-input*)
     (find-class 'stream))
 NIL)

(my-assert
 (eq (class-of (lambda (x) x))
     (find-class 'function))
 T
 "lambda should return a function.

a function is:
function n. 1. an object representing code, which can
be called with zero or more arguments, and which produces
zero or more values. 2. an object of type function.

So class-of should return a function. Not?")

(my-assert
 (eq (class-of (find-class 't))
     (find-class 'built-in-class))
 T)

(my-assert
 (typep "abc" (find-class 't))
 T)

(my-assert
 (typep "abc" (find-class 'array))
 T)

(my-assert
 (typep "abc" (find-class 'vector))
 T)

(my-assert
 (typep "abc" (find-class 'string))
 T)

(my-assert
 (typep "abc" (find-class 'integer))
 NIL)

(my-assert
 (typep 3 (find-class 't))
 T)

(my-assert
 (typep 3 (find-class 'number))
 T)

(my-assert
 (typep 3 (find-class 'float))
 NIL)

(my-assert
 (typep 3 (find-class 'integer))
 T)

(my-assert
 (typep 3 (find-class 'string))
 NIL)

(my-assert
 (typep *standard-input* (find-class 'stream))
 T)

#+(or clisp allegro cmu sbcl)
(my-assert
 #+CLISP
 (defun subclassp (class1 class2)
   (clos::subclassp class1 class2)
   )
 #+ALLEGRO
 (defun subclassp (class1 class2)
   (finalize-inheritance class1)
   (not (null (member class2 (class-precedence-list class1))))
   )
 #+CMU
 (defun subclassp (class1 class2)
   (not (null (member (car (pcl:class-precedence-list class2))
		      (pcl:class-precedence-list class1)
		      ) )    )     )
 #+sbcl
 (defun subclassp (class1 class2)
   (not (null (member (car (sb-pcl:class-precedence-list class2))
		      (sb-pcl:class-precedence-list class1)
		      ) )    )     )
 #+(or CLISP ALLEGRO cmu sbcl) SUBCLASSP)

(my-assert
 (subclassp (find-class 'number)
	    (find-class 't))
 T)

(my-assert
 (subclassp (find-class 'integer)
	    (find-class 'number))
 T)

(my-assert
 (subclassp (find-class 'float)
	    (find-class 'number))
 T)
