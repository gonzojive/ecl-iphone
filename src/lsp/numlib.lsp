;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                           number routines

(in-package "SYSTEM")

(c-declaim (si::c-export-fname isqrt abs phase signum cis asin acos
			       asinh acosh atanh rational
			       ffloor fceiling ftruncate fround
			       logtest byte byte-size byte-position
			       ldb ldb-test mask-field dpb deposit-field))

(defconstant imag-one #C(0.0 1.0))

(defun isqrt (i)
       (unless (and (integerp i) (>= i 0))
               (error "~S is not a non-negative integer." i))
       (if (zerop i)
           0
           (let ((n (integer-length i)))
                (do ((x (ash 1 (ceiling n 2)))
                     (y))
                    (nil)
                    (setq y (floor i x))
                    (when (<= x y)
                          (return x))
                    (setq x (floor (+ x y) 2))))))

(defun abs (z)
  (if (complexp z)
      ;; Compute sqrt(x*x + y*y) carefully to prevent overflow.
      ;; Assume |x| >= |y|. Then sqrt(x*x + y*y) = |x|*sqrt(1 +(y/x)^2).
      (let* ((x (abs (realpart z)))
	     (y (abs (imagpart z))))
	;; Swap x and y so that |x| >= |y|.
	(if (< x y)
	    (rotatef x y))
	(if (zerop x)
	    x
	    (let ((r (/ y x)))
	      (* x (sqrt (+ 1 (* r r)))))))
      (if (minusp z)
	  (- z)
	  z)))

(defun phase (x)
       (atan (imagpart x) (realpart x)))

(defun signum (x) (if (zerop x) x (/ x (abs x))))

(defun cis (x) (exp (* imag-one x)))

(defun asin (x)
  ;; (* #C(0.0 -1.0) (log (+ (* imag-one x) (sqrt (- 1.0 (* x x))))))
  (let ((c (log (+ (* imag-one x)
		   (sqrt (- 1.0 (* x x)))))))
    (if (and (complexp c) (zerop (realpart c)))
	(imagpart c)
	(* #C(0.0 -1.0) c))))

(defun acos (x)
  ;; (* #C(0.0 -1.0) (log (+ x (* imag-one (sqrt (- 1.0 (* x x)))))))
  (let ((c (log (+ x (* imag-one
			(sqrt (- 1.0 (* x x))))))))
    (if (and (complexp c) (zerop (realpart c)))
	(imagpart c)
	(* #C(0.0 -1.0) c))))

;;; (defun sinh (x) (/ (- (exp x) (exp (- x))) 2.0))
;;; version by Raymond Toy <toy@rtp.ericsson.se>
#+nil
(defun sinh (z)
  (if (complexp z)
      ;; For complex Z, compute the real and imaginary parts
      ;; separately to get better precision.
      (let ((x (realpart z))
	    (y (imagpart z)))
	(complex (* (sinh x) (cos y))
		 (* (cosh x) (sin y))))
      (let ((limit #.(expt (* double-float-epsilon 45/2) 1/5)))
	(if (< (- limit) z limit)
	    ;; For this region, write use the fact that sinh z =
	    ;; z*exp(z)*[(1 - exp(-2z))/(2z)].  Then use the first
	    ;; 4 terms in the Taylor series expansion of
	    ;; (1-exp(-2z))/2/z.  series expansion of (1 -
	    ;; exp(2*x)).  This is needed because there is severe
	    ;; roundoff error calculating (1 - exp(-2z)) for z near
	    ;; 0.
	    (* z (exp z)
	       (- 1 (* z
		       (- 1 (* z
			       (- 2/3 (* z
					 (- 1/3 (* 2/15 z)))))))))
	    (let ((e (exp z)))
	      (/ (- e (/ e)) 2.0))))))

;;; (defun cosh (x) (/ (+ (exp x) (exp (- x))) 2.0))
;;; version by Raymond Toy <toy@rtp.ericsson.se>
#+nil
(defun cosh (z)
  (if (complexp z)
      ;; For complex Z, compute the real and imaginary parts
      ;; separately to get better precision.
      (let ((x (realpart z))
	    (y (imagpart z)))
	(complex (* (cosh x) (cos y))
		 (* (sinh x) (sin y))))
      ;; For real Z, there's no chance of round-off error, so
      ;; direct evaluation is ok.
      (let ((e (exp z)))
	(/ (+ e (/ e)) 2.0))))

#+nil
(defun tanh (x) (/ (sinh x) (cosh x)))

(defun asinh (x) (log (+ x (sqrt (+ 1.0 (* x x))))))
(defun acosh (x)
  ;; CLtL1: (log (+ x (* (1+ x) (sqrt (/ (1- x) (1+ x))))))
  (* 2 (log (+ (sqrt (/ (1+ x) 2)) (sqrt (/ (1- x) 2))))))

(defun atanh (x)
  (/ (- (log (1+ x)) (log (- 1 x))) 2))	; CLtL2

(defun rational (x)
  (etypecase x
    (FLOAT	  
      (multiple-value-bind (i e s) (integer-decode-float x)
			   (if (>= s 0)
			       (* i (expt (float-radix x) e))
			     (- (* i (expt (float-radix x) e))))))
    (RATIONAL x)))


(setf (symbol-function 'rationalize) (symbol-function 'rational))

(defun ffloor (x &optional (y 1.0s0))
       (multiple-value-bind (i r) (floor (float x) (float y))
        (values (float i r) r)))

(defun fceiling (x &optional (y 1.0s0))
       (multiple-value-bind (i r) (ceiling (float x) (float y))
        (values (float i r) r)))

(defun ftruncate (x &optional (y 1.0s0))
       (multiple-value-bind (i r) (truncate (float x) (float y))
        (values (float i r) r)))

(defun fround (x &optional (y 1.0s0))
       (multiple-value-bind (i r) (round (float x) (float y))
        (values (float i r) r)))

(defun logtest (x y) (not (zerop (logand x y))))


(defun byte (size position)
  (cons size position))

(defun byte-size (bytespec)
  (car bytespec))

(defun byte-position (bytespec)
  (cdr bytespec))

(defun ldb (bytespec integer)
  (logandc2 (ash integer (- (byte-position bytespec)))
            (- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  (not (zerop (ldb bytespec integer))))

(defun mask-field (bytespec integer)
  (ash (ldb bytespec integer) (byte-position bytespec)))

(defun dpb (newbyte bytespec integer)
  (logxor integer
          (mask-field bytespec integer)
          (ash (logandc2 newbyte
                         (- (ash 1 (byte-size bytespec))))
               (byte-position bytespec))))

(defun deposit-field (newbyte bytespec integer)
  (dpb (ash newbyte (- (byte-position bytespec))) bytespec integer))
