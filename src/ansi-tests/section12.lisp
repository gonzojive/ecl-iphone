;;; 12: numbers -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 12.1.4.1.1

;;;; Combining rationals with floats.
;;; This example assumes an implementation in which
;;; (float-radix 0.5) is 2 (as in IEEE) or 16 (as in IBM/360),
;;; or else some other implementation in which 1/2 has an exact
;;;  representation in floating point.
(my-assert
 (+ 1/2 0.5)
 1.0)


(my-assert
 (- 1/2 0.5d0)
 0.0d0)


(my-assert
 (+ 0.5 -0.5 1/2)
 0.5)

;;;; Comparing rationals with floats.
;;; This example assumes an implementation in which the default float
;;; format is IEEE single-float, IEEE double-float, or some other format
;;; in which 5/7 is rounded upwards by FLOAT.

(my-assert
 (< 5/7 (float 5/7))
 t)


(my-assert
 (< 5/7 (rational (float 5/7)))
 t)


(my-assert
 (< (float 5/7) (float 5/7))
 nil)

;;; 12.1.5.3.1


(my-assert
 #c(1.0 1.0)
 #C(1.0 1.0))

(my-assert
 #c(0.0 0.0)
 #C(0.0 0.0))

(my-assert
 #c(1.0 1)
 #C(1.0 1.0))

(my-assert
 #c(0.0 0)
 #C(0.0 0.0))

(my-assert
 #c(1 1)
 #C(1 1))

(my-assert
 #c(0 0)
 0)

(my-assert
 (typep #c(1 1) '(complex (eql 1)))
 t)


(my-assert
 (typep #c(0 0) '(complex (eql 0)))
 nil)

;;; number


(my-assert
 (subtypep 'real  'number)
 t)


(my-assert
 (subtypep 'complex 'number)
 t)


(my-assert
 (subtypep 'rational 'real)
 t)


(my-assert
 (subtypep 'float 'real)
 t)


;;; float


(my-assert
 (subtypep 'short-float 'float)
 t)


(my-assert
 (subtypep 'single-float 'float)
 t)


(my-assert
 (subtypep 'double-float 'float)
 t)


(my-assert
 (subtypep 'long-float  'float)
 t)

;;; rational


(my-assert
 (subtypep 'integer 'rational)
 t)


(my-assert
 (subtypep 'ratio 'rational)
 t)

;;; integer


(my-assert
 (subtypep 'fixnum 'integer)
 t)


(my-assert
 (subtypep 'bignum 'integer)
 t)
;;; fixnum

(my-assert
 (subtypep '(signed-byte 16) 'fixnum)
 t)
;;; = /= < > <= >=

(my-assert
 (= 3 3)
 t)

(my-assert
 (/= 3 3)
 nil)

(my-assert
 (= 3 5)
 nil)

(my-assert
 (/= 3 5)
 t)

(my-assert
 (= 3 3 3 3)
 t)

(my-assert
 (/= 3 3 3 3)
 nil)

(my-assert
 (= 3 3 5 3)
 nil)

(my-assert
 (/= 3 3 5 3)
 nil)

(my-assert
 (= 3 6 5 2)
 nil)

(my-assert
 (/= 3 6 5 2)
 t)

(my-assert
 (= 3 2 3)
 nil)

(my-assert
 (/= 3 2 3)
 nil)

(my-assert
 (< 3 5)
 t)

(my-assert
 (<= 3 5)
 t)

(my-assert
 (< 3 -5)
 nil)

(my-assert
 (<= 3 -5)
 nil)

(my-assert
 (< 3 3)
 nil)

(my-assert
 (<= 3 3)
 t)

(my-assert
 (< 0 3 4 6 7)
 t)

(my-assert
 (<= 0 3 4 6 7)
 t)

(my-assert
 (< 0 3 4 4 6)
 nil)

(my-assert
 (<= 0 3 4 4 6)
 t)

(my-assert
 (> 4 3)
 t)

(my-assert
 (>= 4 3)
 t)

(my-assert
 (> 4 3 2 1 0)
 t)

(my-assert
 (>= 4 3 2 1 0)
 t)

(my-assert
 (> 4 3 3 2 0)
 nil)

(my-assert
 (>= 4 3 3 2 0)
 t)

(my-assert
 (> 4 3 1 2 0)
 nil)

(my-assert
 (>= 4 3 1 2 0)
 nil)

(my-assert
 (= 3)
 t)

(my-assert
 (/= 3)
 t)

(my-assert
 (< 3)
 t)

(my-assert
 (<= 3)
 t)

(my-assert
 (= 3.0 #c(3.0 0.0))
 t)

(my-assert
 (/= 3.0 #c(3.0 1.0))
 t)

(my-assert
 (= 3 3.0)
 t)

(my-assert
 (= 3.0s0 3.0d0)
 t)

(my-assert
 (= 0.0 -0.0)
 t)

(my-assert
 (= 5/2 2.5)
 t)
(my-assert
 (> 0.0 -0.0)
 nil)
(my-assert
 (= 0 -0.0)
 t)


;;; min max


(my-assert
 (max 3)
 3 )


(my-assert
 (min 3)
 3)


(my-assert
 (max 6 12)
 12 )


(my-assert
 (min 6 12)
 6)


(my-assert
 (max -6 -12)
 -6 )


(my-assert
 (min -6 -12)
 -12)


(my-assert
 (max 1 3 2 -7)
 3 )


(my-assert
 (min 1 3 2 -7)
 -7)


(my-assert
 (max -2 3 0 7)
 7 )


(my-assert
 (min -2 3 0 7)
 -2)


(my-assert
 (max 5.0 2)
 5.0 )


(my-assert
 (min 5.0 2)
 #+(or cmu sbcl clisp ecls) 2
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  2 OR  2.0


(my-assert
 (max 3.0 7 1)
 #+(or cmu sbcl clisp ecls) 7
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  7 OR  7.0


(my-assert
 (min 3.0 7 1)
 #+(or cmu sbcl clisp ecls) 1
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; 1 OR  1.0


(my-assert
 (max 1.0s0 7.0d0)
 7.0d0)


(my-assert
 (min 1.0s0 7.0d0)
 #+(or cmu sbcl ecls) 1.0		;hmm in fact an error?
 #+clisp 1.0s0
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  1.0s0 OR  1.0d0


(my-assert
 (max 3 1 1.0s0 1.0d0)
 #+(or cmu sbcl clisp ecls) 3
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  3 OR 3.0d0


(my-assert
 (min 3 1 1.0s0 1.0d0)
 #+(or cmu sbcl clisp ecls) 1
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  1 OR  1.0s0  OR  1.0d0

;;; plusp minusp


(my-assert
 (minusp -1)
 t)


(my-assert
 (plusp 0)
 nil)


(my-assert
 (plusp least-positive-single-float)
 t)


(my-assert
 (plusp least-positive-double-float)
 t)


(my-assert
 (minusp least-positive-single-float)
 nil)


(my-assert
 (minusp least-positive-double-float)
 nil)


(my-assert
 (plusp least-negative-single-float)
 nil)


(my-assert
 (plusp least-negative-double-float)
 nil)


(my-assert
 (minusp least-negative-single-float)
 t)


(my-assert
 (minusp least-negative-double-float)
 t)


(my-assert
 (minusp 0)
 nil)


(my-assert
 (minusp -0.0)
 nil)


(my-assert
 (minusp +0.0)
 nil)


(my-assert
 (plusp 0)
 nil)


(my-assert
 (plusp -0.0)
 nil)


(my-assert
 (plusp +0.0)
 nil)


;;; zerop


(my-assert
 (zerop 0)
 t)


(my-assert
 (zerop 0.0)
 t)


(my-assert
 (zerop +0.0)
 t)


(my-assert
 (zerop -0.0)
 t)


(my-assert
 (zerop -1)
 nil)


(my-assert
 (zerop 1)
 nil)


(my-assert
 (zerop 0/100)
 t)


(my-assert
 (zerop #c(0 0.0))
 t)

;;; random-state-p


(my-assert
 (random-state-p *random-state*)
 t)


(my-assert
 (random-state-p (make-random-state))
 t)


(my-assert
 (random-state-p 'test-function)
 nil)

;;; number-p


(my-assert
 (numberp 12)
 t)


(my-assert
 (numberp (expt 2 130))
 t)


(my-assert
 (numberp #c(5/3 7.2))
 t)


(my-assert
 (numberp nil)
 nil)


(my-assert
 (numberp (cons 1 2))
 nil)

;;; most-positive-fixnum



(my-assert
 (>= most-positive-fixnum (- (expt 2 15) 1))
 t)


(my-assert
 (>= most-positive-fixnum array-dimension-limit)
 t
 "
most-positive-fixnum is that fixnum closest in value
to positive infinity provided by the implementation, and
greater than or equal to both 2^15 - 1 and array-dimension-limit.
")


(my-assert
 (<= most-negative-fixnum (- (expt 2 15)))
 t)

;;; most-positive bla bla


(my-assert
 (plusp MOST-POSITIVE-SHORT-FLOAT)
 t)


(my-assert
 (plusp  LEAST-POSITIVE-SHORT-FLOAT)
 t)


(my-assert
 (plusp  LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT)
 t)


(my-assert
 (plusp  MOST-POSITIVE-DOUBLE-FLOAT)
 t)


(my-assert
 (plusp LEAST-POSITIVE-DOUBLE-FLOAT)
 t)


(my-assert
 (plusp  LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT)
 t)


(my-assert
 (plusp MOST-POSITIVE-LONG-FLOAT)
 t)


(my-assert
 (plusp LEAST-POSITIVE-LONG-FLOAT)
 t)


(my-assert
 (plusp LEAST-POSITIVE-NORMALIZED-LONG-FLOAT)
 t)


(my-assert
 (plusp  MOST-POSITIVE-SINGLE-FLOAT)
 t)


(my-assert
 (plusp LEAST-POSITIVE-SINGLE-FLOAT)
 t)


(my-assert
 (plusp  LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT)
 t)


(my-assert
 (minusp MOST-NEGATIVE-SHORT-FLOAT)
 t)


(my-assert
 (minusp  LEAST-NEGATIVE-SHORT-FLOAT)
 t)


(my-assert
 (minusp LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT)
 t)


(my-assert
 (minusp  MOST-NEGATIVE-SINGLE-FLOAT)
 t)


(my-assert
 (minusp LEAST-NEGATIVE-SINGLE-FLOAT)
 t)


(my-assert
 (minusp  LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT)
 t)


(my-assert
 (minusp MOST-NEGATIVE-DOUBLE-FLOAT)
 t)


(my-assert
 (minusp  LEAST-NEGATIVE-DOUBLE-FLOAT)
 t)


(my-assert
 (minusp LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT)
 t)


(my-assert
 (minusp  MOST-NEGATIVE-LONG-FLOAT)
 t)


(my-assert
 (minusp LEAST-NEGATIVE-LONG-FLOAT)
 t)


(my-assert
 (minusp  LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT)
 t)

;;; epsilons



(my-assert
 (not (= (float 1 short-float-epsilon)
	 (+ (float 1 short-float-epsilon) short-float-epsilon)))
 t
 "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated: 

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(my-assert
 (not (= (float 1 single-float-epsilon)
	 (+ (float 1 single-float-epsilon) single-float-epsilon)))
 t
 "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated: 

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(my-assert
 (not (= (float 1 double-float-epsilon)
	 (+ (float 1 double-float-epsilon) double-float-epsilon)))
 t
 "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated: 

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(my-assert
 (not (= (float 1 long-float-epsilon )
	 (+ (float 1 long-float-epsilon ) long-float-epsilon )))
 t
 "The value of each of the constants short-float-epsilon,
single-float-epsilon, double-float-epsilon, and
long-float-epsilon is the smallest positive float
<EPSILON> of the given format, such that the following
expression is true when evaluated: 

(not (= (float 1 <EPSILON>) (+ (float 1 <EPSILON>) <EPSILON>))
     ")


(my-assert
 (not (= (float 1 short-float-negative-epsilon)
	 (- (float 1 short-float-negative-epsilon)
	    short-float-negative-epsilon)))
 t
 "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated: 

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")


(my-assert
 (not (= (float 1 single-float-negative-epsilon)
	 (- (float 1 single-float-negative-epsilon)
	    single-float-negative-epsilon)))
 t
 "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated: 

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")


(my-assert
 (not (= (float 1 double-float-negative-epsilon)
	 (- (float 1 double-float-negative-epsilon)
	    double-float-negative-epsilon)))
 t
 "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated: 

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")



(my-assert
 (not (= (float 1 long-float-negative-epsilon)
	 (- (float 1 long-float-negative-epsilon)
	    long-float-negative-epsilon)))
 t
 "The value of each of the constants short-float-negative-epsilon,
single-float-negative-epsilon,
double-float-negative-epsilon, and long-float-negative-epsilon
is the smallest positive float <EPSILON> of
the given format, such that the following expression
is true when evaluated: 

(not (= (float 1 <EPSILON>) (- (float 1 <EPSILON>) <EPSILON>))) ")
