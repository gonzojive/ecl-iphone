;;; section 5 -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 5.1.1.1.1

(my-assert
 (let ((ref2 (list '())))
   (push (progn (princ "1") 'ref1)
	 (car (progn (princ "2") ref2))))
 (REF1))

#+nil
(my-assert
 (let (x)
   (push (setq x (list 'a))
	 (car (setq x (list 'b))))
   x)
 (((A) . B)))				;possible bug in hyperspec


;;; apply
(my-assert
 (setq f '+)
 +)

(my-assert
 (apply f '(1 2))
 3)

(my-assert
 (progn
   (setq f #'-)
   t)
 t)

(my-assert
 (apply f '(1 2))
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
 (defparameter *some-list* '(a b c))
 *SOME-LIST*)

(my-assert
 (defun strange-test (&rest x) (eq x *some-list*))
 STRANGE-TEST)

(my-assert
 (apply #'strange-test *some-list*)
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in )

;;(my-assert
;;(defun foo (size &rest keys &key double &allow-other-keys)
;;   (let ((v (apply #'make-array size :allow-other-keys t keys)))
;;     (if double (concatenate (type-of v) v v) v)))
;;FOO)

;;(my-assert
;;(foo 4 :initial-contents '(a b c d) :double t)
;;#(A B C D A B C D))

;;; defun

(my-assert
 (defun recur (x)
   (when (> x 0)
     (recur (1- x))))
 RECUR )

(my-assert
 (defun ex (a b &optional c (d 66) &rest keys &key test (start 0))
   (list a b c d keys test start))
 EX )

(my-assert
 (ex 1 2)
 (1 2 NIL 66 NIL NIL 0))

(my-assert
 (ex 1 2 3 4 :test 'equal :start 50)
 (1 2 3 4 (:TEST EQUAL :START 50) EQUAL 50))

(my-assert
 (ex :test 1 :start 2)
 (:TEST 1 :START 2 NIL NIL 0))

;; This function assumes its callers have checked the types of the
;; arguments, and authorizes the compiler to build in that assumption.
(my-assert
 (defun discriminant (a b c)
   (declare (number a b c))
   "Compute the discriminant for a quadratic equation."
   (- (* b b) (* 4 a c)))
 DISCRIMINANT)

(my-assert
 (discriminant 1 2/3 -2)
 76/9)

;; This function assumes its callers have not checked the types of the
;; arguments, and performs explicit type checks before making any assumptions.

(my-assert
 (defun careful-discriminant (a b c)
   "Compute the discriminant for a quadratic equation."
   (check-type a number)
   (check-type b number)
   (check-type c number)
   (locally (declare (number a b c))
	    (- (* b b) (* 4 a c))))
 CAREFUL-DISCRIMINANT)

(my-assert
 (careful-discriminant 1 2/3 -2)
 76/9)

;;; fboundp

(my-assert
 (fboundp 'car)
 t)

(my-assert
 (fboundp 'nth-value)
 #+(or cmu sbcl clisp) t
 #-(or cmu sbcl clisp) nil)

(my-assert
 (fboundp 'with-open-file)
 t)

(my-assert
 (fboundp 'unwind-protect)
 t)

(my-assert
 (defun my-function (x) x)
 MY-FUNCTION)

(my-assert
 (fboundp 'my-function)
 t)

(my-assert
 (let ((saved-definition (symbol-function 'my-function)))
   (unwind-protect (progn (fmakunbound 'my-function)
			  (fboundp 'my-function))
     (setf (symbol-function 'my-function) saved-definition)))
 nil)

(my-assert
 (fboundp 'my-function)
 t)

(my-assert
 (defmacro my-macro (x) `',x)
 MY-MACRO)

(my-assert
 (fboundp 'my-macro)
 t)

(my-assert
 (fmakunbound 'my-function)
 MY-FUNCTION)

(my-assert
 (fboundp 'my-function)
 nil)

(my-assert
 (flet ((my-function (x) x))
   (fboundp 'my-function))
 nil)

;;; fmakunbound

(my-assert
 (defun add-some (x) (+ x 19))
 ADD-SOME)

(my-assert
 (fboundp 'add-some)
 t)

(my-assert
 (flet ((add-some (x) (+ x 37)))
   (fmakunbound 'add-some)
   (add-some 1))
 38)

(my-assert
 (fboundp 'add-some)
 nil)

;;; macroletjes

(my-assert
 (flet ((flet1 (n) (+ n n)))
   (flet ((flet1 (n) (+ 2 (flet1 n))))
     (flet1 2)))
 6)

(my-assert
 (defun dummy-function () 'top-level)
 DUMMY-FUNCTION )

(my-assert
 (funcall #'dummy-function)
 TOP-LEVEL )

(my-assert
 (flet ((dummy-function () 'shadow))
   (funcall #'dummy-function))
 SHADOW )

(my-assert
 (eq (funcall #'dummy-function) (funcall 'dummy-function))
 t )

(my-assert
 (flet ((dummy-function () 'shadow))
   (eq (funcall #'dummy-function)
       (funcall 'dummy-function)))
 nil)

(my-assert
 (defun recursive-times (k n)
   (labels ((temp (n)
		  (if (zerop n) 0 (+ k (temp (1- n))))))
     (temp n)))
 RECURSIVE-TIMES)

(my-assert
 (recursive-times 2 3)
 6)

(my-assert
 (defmacro mlets (x &environment env)
   (let ((form `(babbit ,x)))
     (macroexpand form env)))
 MLETS)

(my-assert
 (macrolet ((babbit (z) `(+ ,z ,z))) (mlets 5))
 10)

(my-assert
 (flet ((safesqrt (x) (sqrt (abs x))))
   ;; The safesqrt function is used in two places.
   (safesqrt (apply #'+ (map 'list #'safesqrt '(1 2 3 4 5 6)))))
 3.2911735)

(my-assert
 (defun integer-power (n k)
   (declare (integer n))
   (declare (type (integer 0 *) k))
   (labels ((expt0 (x k a)
		   (declare (integer x a) (type (integer 0 *) k))
		   (cond ((zerop k) a)
			 ((evenp k) (expt1 (* x x) (floor k 2) a))
			 (t (expt0 (* x x) (floor k 2) (* x a)))))
	    (expt1 (x k a)
		   (declare (integer x a) (type (integer 0 *) k))
		   (cond ((evenp k) (expt1 (* x x) (floor k 2) a))
			 (t (expt0 (* x x) (floor k 2) (* x a))))))
     (expt0 n k 1)))
 INTEGER-POWER)

(my-assert
 (defun example (y l)
   (flet ((attach (x)
		  (setq l (append l (list x)))))
     (declare (inline attach))
     (dolist (x y)
       (unless (null (cdr x))
	 (attach x)))
     l))
 EXAMPLE)

(my-assert
 (example '((a apple apricot) (b banana) (c cherry) (d) (e))
          '((1) (2) (3) (4 2) (5) (6 3 2)))
 ((1) (2) (3) (4 2) (5) (6 3 2) (A APPLE APRICOT) (B BANANA) (C CHERRY)))


;;; funcall

(my-assert
 (funcall #'+ 1 2 3)
 6)

(my-assert
 (funcall 'car '(1 2 3))
 1)

(my-assert
 (funcall 'position 1 '(1 2 3 2 1) :start 1)
 4)

(my-assert
 (cons 1 2)
 (1 . 2))

(my-assert
 (flet ((cons (x y) `(kons ,x ,y)))
   (let ((cons (symbol-function '+)))
     (funcall #'cons
              (funcall 'cons 1 2)
              (funcall cons 1 2))))
 (KONS (1 . 2) 3))

;;; functionp

(my-assert
 (functionp 'append)
 nil)

(my-assert
 (functionp #'append)
 t)

(my-assert
 (functionp (symbol-function 'append))
 t)

(my-assert
 (flet ((f () 1)) (functionp #'f))
 t)

(my-assert
 (functionp (compile nil '(lambda () 259)))
 t)

(my-assert
 (functionp nil)
 nil)

(my-assert
 (functionp 12)
 nil)

(my-assert
 (functionp '(lambda (x) (* x x)))
 nil)

(my-assert
 (functionp #'(lambda (x) (* x x)))
 t)

;;; compiled-function-p


(my-assert
 (defun f (x) x)
 F)

(my-assert
 (compiled-function-p #'f)
 #+(or cmu sbcl ecls) t
 #+clisp nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
;;  false OR true

(my-assert
 (compiled-function-p 'f)
 nil)

(my-assert
 (compile 'f)
 F)

(my-assert
 (compiled-function-p #'f)
 t)

(my-assert
 (compiled-function-p 'f)
 nil)

(my-assert
 (compiled-function-p (compile nil '(lambda (x) x)))
 t)

(my-assert
 (compiled-function-p #'(lambda (x) x))
 #+(or cmu sbcl ecls) t
 #+clisp nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  false OR true

(my-assert
 (compiled-function-p '(lambda (x) x))
 nil)

;;; CALL-ARGUMENTS-LIMIT

(my-assert
 (>= CALL-ARGUMENTS-LIMIT  50)
 t)

;;; LAMBDA-LIST-KEYWORDS

(my-assert
 (not (member '&ALLOW-OTHER-KEYS LAMBDA-LIST-KEYWORDS))
 nil)

(my-assert
 (not (member '&AUX LAMBDA-LIST-KEYWORDS))
 nil)

(my-assert
 (not (member '&ENVIRONMENT LAMBDA-LIST-KEYWORDS))
 nil)

(my-assert
 (not (member '&OPTIONAL LAMBDA-LIST-KEYWORDS))
 nil)

(my-assert
 (not (member '&REST LAMBDA-LIST-KEYWORDS))
 nil)

(my-assert
 (not (member '&WHOLE LAMBDA-LIST-KEYWORDS))
 nil)

;;; LAMBDA-PARAMETERS-LIMIT

(my-assert
 (>= LAMBDA-PARAMETERS-LIMIT 50)
 t)

;;; defconstant

(my-assert
 (defconstant this-is-a-constant 'never-changing "for a test")
 THIS-IS-A-CONSTANT)

(my-assert
 this-is-a-constant
 NEVER-CHANGING)

(my-assert
 (documentation 'this-is-a-constant 'variable)
 "for a test")

(my-assert
 (constantp 'this-is-a-constant)
 t)

;;; defparameter

(my-assert
 (defparameter *p* 1)
 *P*)

(my-assert
 *p*
 1)

(my-assert
 (constantp '*p*)
 nil)

(my-assert
 (setq *p* 2)
 2)

(my-assert
 (defparameter *p* 3)
 *P*)

(my-assert
 *p*
 3)

(unintern '*V*)

(my-assert
 (defvar *v* 1)
 *V*)

(my-assert
 *v*
 1)

(my-assert
 (constantp '*v*)
 nil)

(my-assert
 (setq *v* 2)
 2)

(my-assert
 (defvar *v* 3)
 *V*)

(my-assert
 *v*
 2)

(my-assert
 (defun foo ()
   (let ((*p* 'p) (*v* 'v))
     (bar)))
 FOO)

(my-assert
 (defun bar () (list *p* *v*))
 BAR)

(my-assert
 (foo)
 (P V))

;;; destructuring-bind

(my-assert
 (defun iota (n) (loop for i from 1 to n collect i)) ;helper
 IOTA)

(my-assert
 (destructuring-bind ((a &optional (b 'bee)) one two three)
     `((alpha) ,@(iota 3))
   (list a b three two one))
 (ALPHA BEE 3 2 1))

;;; let & let*

					;(let ((a 'top))

					;  (my-assert
					;   (defun dummy-function () a)
					;   DUMMY-FUNCTION)

					;  (my-assert
					;   (let ((a 'inside) (b a))
					;     (format nil "~S ~S ~S" a b (dummy-function)))
					;   "INSIDE TOP TOP" )

					;  (my-assert
					;   (let* ((a 'inside) (b a))
					;     (format nil "~S ~S ~S" a b (dummy-function)))
					;   "INSIDE INSIDE TOP" ))

					;(setf a 'top)

					;(my-assert
					; (let ((a 'inside) (b a))
					;   (declare (special a))
					;   (format nil "~S ~S ~S" a b (dummy-function)))
					; "INSIDE TOP INSIDE")

;;; progv

(my-assert
 (let ((*x* 3))
   (progv '(*x*) '(4)
     (list *x* (symbol-value '*x*))))
 (3 4))

(my-assert
 (setq *x* 1)
 1)

(my-assert
 (progv '(*x*) '(2) *x*)
 2)

(my-assert
 *x*
 1)


;;; setq
(my-assert
 (setq a 1 b 2 c 3)
 3)

(my-assert
 a
 1)

(my-assert
 b
 2)

(my-assert
 c
 3)

;; Use of SETQ to update values by sequential assignment.
(my-assert
 (setq a (1+ b) b (1+ a) c (+ a b))
 7)

(my-assert
 a
 3)

(my-assert
 b
 4)

(my-assert
 c
 7)

;; This illustrates the use of SETQ on a symbol macro.
(my-assert
 (let ((x (list 10 20 30)))
   (symbol-macrolet ((y (car x)) (z (cadr x)))
     (setq y (1+ z) z (1+ y))
     (list x y z)))
 ((21 22 30) 21 22))

;;; psetq
(my-assert
 (psetq a 1 b 2 c 3)
 NIL)

(my-assert
 a
 1)

(my-assert
 b
 2)

(my-assert
 c
 3)

;; Use of PSETQ to update values by parallel assignment.
;; The effect here is very different than if SETQ had been used.
(my-assert
 (psetq a (1+ b) b (1+ a) c (+ a b))
 NIL)

(my-assert
 a
 3)

(my-assert
 b
 2)

(my-assert
 c
 3)

;; Use of PSETQ on a symbol macro.
(my-assert
 (let ((x (list 10 20 30)))
   (symbol-macrolet ((y (car x)) (z (cadr x)))
     (psetq y (1+ z) z (1+ y))
     (list x y z)))
 ((21 11 30) 21 11))

;; Use of parallel assignment to swap values of A and B.
(my-assert
 (multiple-value-bind (n h)
     (let ((a 1) (b 2))
       (psetq a b  b a)
       (values a b))
   (list n h))
 (2 1))


;;; block

(my-assert
 (block empty)
 NIL)

(my-assert
 (multiple-value-bind (n h)
     (block whocares (values 1 2) (values 3 4))
   (list n h))
 (3 4))

(my-assert
 (let ((x 1))
   (block stop (setq x 2) (return-from stop) (setq x 3))
   x)
 2)

(my-assert
 (multiple-value-bind (n h)
     (block early
       (return-from early (values 1 2))
       (values 3 4))
   (list n h))
 (1  2))

(my-assert
 (block outer (block inner (return-from outer 1)) 2)
 1)

(my-assert
 (block twin (block twin (return-from twin 1)) 2)
 2)

;; Contrast behavior of this example with corresponding example of CATCH.
(my-assert
 (block b
   (flet ((b1 () (return-from b 1)))
     (block b (b1) (print 'unreachable))
     2))
 1)

;; catch

(my-assert
 (catch 'dummy-tag 1 2 (throw 'dummy-tag 3) 4)
 3)

(my-assert
 (catch 'dummy-tag 1 2 3 4)
 4)

(my-assert
 (defun throw-back (tag) (throw tag t))
 THROW-BACK)

(my-assert
 (catch 'dummy-tag (throw-back 'dummy-tag) 2)
 T)

;; Contrast behavior of this example with corresponding example of BLOCK.
(my-assert
 (catch 'c
   (flet ((c1 () (throw 'c 1)))
     (catch 'c (c1) (print 'unreachable))
     2))
 2)

;;; go
(my-assert
 (tagbody
  (setq val 2)
  (go lp)
  (incf val 3)
  lp (incf val 4))
 NIL)

(my-assert
 val
 6 )

;;; return-from
(my-assert
 (block alpha (return-from alpha) 1)
 NIL)

(my-assert
 (block alpha (return-from alpha 1) 2)
 1)

(my-assert
 (multiple-value-bind (n h)
     (block alpha (return-from alpha (values 1 2)) 3)
   (list n h))
 (1  2))

(my-assert
 (let ((a 0))
   (dotimes (i 10) (incf a) (when (oddp i) (return)))
   a)
 2)

(my-assert
 (defun temp (x)
   (if x (return-from temp 'dummy))
   44)
 TEMP)

(my-assert
 (temp nil)
 44)

(my-assert
 (temp t)
 DUMMY)

(my-assert
 (block out
   (flet ((exit (n) (return-from out n)))
     (block out (exit 1)))
   2)
 1)

(my-assert
 (block nil
   (unwind-protect (return-from nil 1)
     (return-from nil 2)))
 2)

;;; return

(my-assert
 (block nil (return) 1)
 NIL)

(my-assert
 (block nil (return 1) 2)
 1)

(my-assert
 (multiple-value-bind (n h)
     (block nil (return (values 1 2)) 3)
   (list n h))
 (1 2))

(my-assert
 (block nil (block alpha (return 1) 2))
 1)

(my-assert
 (block alpha (block nil (return 1)) 2)
 2)

(my-assert
 (block nil (block nil (return 1) 2))
 1)

;;; tagbody

(my-assert
 (let (val)
   (tagbody
    (setq val 1)
    (go point-a)
    (incf val 16)
    point-c
    (incf val 04)
    (go point-b)
    (incf val 32)
    point-a
    (incf val 02)
    (go point-c)
    (incf val 64)
    point-b
    (incf val 08))
   val)
 15)

(my-assert
 (defun f1 (flag)
   (let ((n 1))
     (tagbody
      (setq n (f2 flag #'(lambda () (go out))))
      out
      (prin1 n))))
 F1)

(my-assert
 (defun f2 (flag escape)
   (if flag (funcall escape) 2))
 F2)

(my-assert
 (f1 nil)
 NIL)

(my-assert
 (f1 t)
 NIL)

;;; trow

(my-assert
 (multiple-value-bind (n h)
     (catch 'result
       (setq i 0 j 0)
       (loop (incf j 3) (incf i)
	 (if (= i 3) (throw 'result (values i j)))))
   (list n h))
 (3 9))


(my-assert
 (catch nil
   (unwind-protect (throw nil 1)
     (throw nil 2)))
 2)

;;; unwind-protect

(my-assert
 (defun dummy-function (x)
   (setq state 'running)
   (unless (numberp x) (throw 'abort 'not-a-number))
   (setq state (1+ x)))
 DUMMY-FUNCTION)

(my-assert
 (catch 'abort (dummy-function 1))
 2)

(my-assert
 state
 2)

(my-assert
 (catch 'abort (dummy-function 'trash))
 NOT-A-NUMBER)

(my-assert
 state
 RUNNING)

(my-assert
 (catch 'abort (unwind-protect (dummy-function 'trash)
		 (setq state 'aborted)))
 NOT-A-NUMBER)

(my-assert
 state
 ABORTED)

;;; not

(my-assert
 (not nil)
 T)

(my-assert
 (not '())
 T)

(my-assert
 (not (integerp 'sss))
 T)

(my-assert
 (not (integerp 1))
 NIL)

(my-assert
 (not 3.7)
 NIL)

(my-assert
 (not 'apple)
 NIL)

;;; eq

(my-assert
 (eq 'a 'b)
 nil)

(my-assert
 (eq 'a 'a)
 t)

(my-assert
 (eq 3 3)
 #+(or cmu sbcl clisp ecls) t
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR false

(my-assert
 (eq 3 3.0)
 nil)

(my-assert
 (eq 3.0 3.0)
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(my-assert
 (eq #c(3 -4) #c(3 -4))
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(my-assert
 (eq #c(3 -4.0) #c(3 -4))
 nil)

(my-assert
 (eq (cons 'a 'b) (cons 'a 'c))
 nil)

(my-assert
 (eq (cons 'a 'b) (cons 'a 'b))
 nil)

(my-assert
 (eq '(a . b) '(a . b))
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(my-assert
 (progn (setq x (cons 'a 'b)) (eq x x))
 T)

(my-assert
 (progn (setq x '(a . b)) (eq x x))
 T)

(my-assert
 (eq #\A #\A)
 #+(or cmu sbcl clisp ecls) t
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(my-assert
 (let ((x "Foo")) (eq x x))
 T)

(my-assert
 (eq "Foo" "Foo")
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  true OR  false

(my-assert
 (eq "Foo" (copy-seq "Foo"))
 nil)

(my-assert
 (eq "FOO" "foo")
 nil)

(my-assert
 (eq "string-seq" (copy-seq "string-seq"))
 nil)

(my-assert
 (let ((x 5)) (eq x x))
 #+(or cmu sbcl clisp ecls) t
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR false

;;; eql

(my-assert
 (eql 'a 'b)
 nil)

(my-assert
 (eql 'a 'a)
 t)

(my-assert
 (eql 3 3)
 t)

(my-assert
 (eql 3 3.0)
 nil)

(my-assert
 (eql 3.0 3.0)
 t)

(my-assert
 (eql #c(3 -4) #c(3 -4))
 t)

(my-assert
 (eql #c(3 -4.0) #c(3 -4))
 nil)

(my-assert
 (eql (cons 'a 'b) (cons 'a 'c))
 nil)

(my-assert
 (eql (cons 'a 'b) (cons 'a 'b))
 nil)

(my-assert
 (eql '(a . b) '(a . b))
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(my-assert
 (progn (setq x (cons 'a 'b)) (eql x x))
 t)

(my-assert
 (progn (setq x '(a . b)) (eql x x))
 t)

(my-assert
 (eql #\A #\A)
 t)

(my-assert
 (eql "Foo" "Foo")
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; true OR  false

(my-assert
 (eql "Foo" (copy-seq "Foo"))
 nil)

(my-assert
 (eql "FOO" "foo")
 nil)

;;; equal

(my-assert
 (equal 'a 'b)
 nil)

(my-assert
 (equal 'a 'a)
 T)

(my-assert
 (equal 3 3)
 t)

(my-assert
 (equal 3 3.0)
 nil)

(my-assert
 (equal 3.0 3.0)
 t)

(my-assert
 (equal #c(3 -4) #c(3 -4))
 t)

(my-assert
 (equal #c(3 -4.0) #c(3 -4))
 nil)

(my-assert
 (equal (cons 'a 'b) (cons 'a 'c))
 nil)

(my-assert
 (equal (cons 'a 'b) (cons 'a 'b))
 t)

(my-assert
 (equal #\A #\A)
 t)

(my-assert
 (equal #\A #\a)
 nil)

(my-assert
 (equal "Foo" "Foo")
 t)

(my-assert
 (equal "Foo" (copy-seq "Foo"))
 t)

(my-assert
 (equal "FOO" "foo")
 nil)

(my-assert
 (equal "This-string" "This-string")
 t)

(my-assert
 (equal "This-string" "this-string")
 nil)

;;; equalp

(my-assert
 (equalp 'a 'b)
 nil)

(my-assert
 (equalp 'a 'a)
 t)

(my-assert
 (equalp 3 3)
 t)

(my-assert
 (equalp 3 3.0)
 t)

(my-assert
 (equalp 3.0 3.0)
 t)

(my-assert
 (equalp #c(3 -4) #c(3 -4))
 t)

(my-assert
 (equalp #c(3 -4.0) #c(3 -4))
 t)

(my-assert
 (equalp (cons 'a 'b) (cons 'a 'c))
 nil)

(my-assert
 (equalp (cons 'a 'b) (cons 'a 'b))
 t)

(my-assert
 (equalp #\A #\A)
 t)

(my-assert
 (equalp #\A #\a)
 t)

(my-assert
 (equalp "Foo" "Foo")
 t)

(my-assert
 (equalp "Foo" (copy-seq "Foo"))
 t)

(my-assert
 (equalp "FOO" "foo")
 t)

(my-assert
 (setq array1 (make-array 6 :element-type 'integer
			  :initial-contents '(1 1 1 3 5 7)))
 #(1 1 1 3 5 7))

(my-assert
 (setq array2 (make-array 8 :element-type 'integer
			  :initial-contents '(1 1 1 3 5 7 2 6)
			  :fill-pointer 6))
 #(1 1 1 3 5 7))

(my-assert
 (equalp array1 array2)
 t)

(my-assert
 (setq vector1 (vector 1 1 1 3 5 7))
 #(1 1 1 3 5 7))

(my-assert
 (equalp array1 vector1)
 t )

;; hashtables etc?

;;; identity

(my-assert
 (identity 101)
 101)

(my-assert
 (mapcan #'identity (list (list 1 2 3) '(4 5 6)))
 (1 2 3 4 5 6))

;;; complement

(my-assert
 (funcall (complement #'zerop) 1)
 t)

(my-assert
 (funcall (complement #'characterp) #\A)
 nil)

(my-assert
 (funcall (complement #'member) 'a '(a b c))
 nil)

(my-assert
 (funcall (complement #'member) 'd '(a b c))
 t)


;;; constantly

(my-assert
 (mapcar (constantly 3) '(a b c d))
 (3 3 3 3))

(my-assert
 (defmacro with-vars (vars &body forms)
   `((lambda ,vars ,@forms) ,@(mapcar (constantly nil) vars)))
 WITH-VARS)

(my-assert
 (multiple-value-bind (n h)
     (macroexpand '(with-vars (a b)
			      (setq a 3 b (* a a))
			      (list a b)))
   (list n h))
 (((LAMBDA (A B) (SETQ A 3 B (* A A)) (LIST A B)) NIL NIL) t))

;;; every en co

(my-assert
 (every #'characterp "abc")
 t)

(my-assert
 (some #'= '(1 2 3 4 5) '(5 4 3 2 1))
 t)

(my-assert
 (notevery #'< '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
 nil)

(my-assert
 (notany #'> '(1 2 3 4) '(5 6 7 8) '(9 10 11 12))
 t)

;;; and

(my-assert
 (setq temp1 1 temp2 1 temp3 1)
 1 )

(my-assert
 (and (incf temp1) (incf temp2) (incf temp3))
 2 )

(my-assert
 (and (eql 2 temp1) (eql 2 temp2) (eql 2 temp3))
 t)

(my-assert
 (decf temp3)
 1 )

(my-assert
 (and (decf temp1) (decf temp2) (eq temp3 'nil) (decf temp3))
 NIL )

(my-assert
 (and (eql temp1 temp2) (eql temp2 temp3))
 t)

(my-assert
 (and)
 T )

;;; cond

(my-assert
 (defun select-options ()
   (cond ((= a 1) (setq a 2))
	 ((= a 2) (setq a 3))
	 ((and (= a 3) (floor a 2)))
	 (t (floor a 3))))
 SELECT-OPTIONS)

(my-assert
 (setq a 1)
 1)

(my-assert
 (select-options)
 2)

(my-assert
 a
 2)

(my-assert
 (select-options)
 3)

(my-assert
 a
 3)

(my-assert
 (select-options)
 1)

(my-assert
 (setq a 5)
 5)

(my-assert
 (multiple-value-bind (n h)
     (select-options)
   (list n h))
 (1 2))

;;; or

(my-assert
 (or)
 NIL )

(my-assert
 (setq temp0 nil temp1 10 temp2 20 temp3 30)
 30)

(my-assert
 (or temp0 temp1 (setq temp2 37))
 10)

(my-assert
 temp2
 20)

(my-assert
 (or (incf temp1) (incf temp2) (incf temp3))
 11)

(my-assert
 temp1
 11)

(my-assert
 temp2
 20)

(my-assert
 temp3
 30)

(my-assert
 (or (values) temp1)
 11)

(my-assert
 (or (values temp1 temp2) temp3)
 11)

(my-assert
 (multiple-value-bind (n h)
     (or temp0 (values temp1 temp2))
   (list n h))
 (11 20))

(my-assert
 (multiple-value-bind (n h)
     (or (values temp0 temp1) (values temp2 temp3))
   (list n h))
 (20 30))

;;; when

(my-assert
 (when t 'hello)
 HELLO)

(my-assert
 (unless t 'hello)
 NIL)

(my-assert
 (when nil 'hello)
 NIL)

(my-assert
 (unless nil 'hello)
 HELLO)

(my-assert
 (when t)
 NIL)

(my-assert
 (unless nil)
 NIL)

(my-assert
 (when t (prin1 1) (prin1 2) (prin1 3))
 3)

(my-assert
 (unless t (prin1 1) (prin1 2) (prin1 3))
 NIL)

(my-assert
 (when nil (prin1 1) (prin1 2) (prin1 3))
 NIL)

(my-assert
 (unless nil (prin1 1) (prin1 2) (prin1 3))
 3)

(my-assert
 (let ((x 3))
   (list (when (oddp x) (incf x) (list x))
	 (when (oddp x) (incf x) (list x))
	 (unless (oddp x) (incf x) (list x))
	 (unless (oddp x) (incf x) (list x))
	 (if (oddp x) (incf x) (list x))
         (if (oddp x) (incf x) (list x))
         (if (not (oddp x)) (incf x) (list x))
         (if (not (oddp x)) (incf x) (list x))))
 ((4) NIL (5) NIL 6 (6) 7 (7)))

;;; multiple-value-bind

(my-assert
 (multiple-value-bind (f r)
     (floor 130 11)
   (list f r))
 (11 9))

;;; multiple-value-call

(my-assert
 (multiple-value-call #'list 1 '/ (values 2 3) '/ (values) '/ (floor 2.5))
 (1 / 2 3 / / 2 0.5))

(my-assert
 (+ (floor 5 3) (floor 19 4))
 5)

(my-assert
 (multiple-value-call #'+ (floor 5 3) (floor 19 4))
 10)

;;; multiple-value-list

(my-assert
 (multiple-value-list (floor -3 4))
 (-1 1))

;;; multiple-value-prog1

(my-assert
 (setq temp '(1 2 3))
 (1 2 3))

(my-assert
 (multiple-value-bind (n h j)
     (multiple-value-prog1
      (values-list temp)
      (setq temp nil)
      (values-list temp))
   (list n h j))
 (1 2 3))

;;; multiple-value-setq

(my-assert
 (multiple-value-setq (quotient remainder) (truncate 3.2 2))
 1)

(my-assert
 quotient
 1)

(my-assert
 remainder
 1.2)

(my-assert
 (multiple-value-setq (a b c) (values 1 2))
 1)

(my-assert
 a
 1)

(my-assert
 b
 2)

(my-assert
 c
 NIL)

(my-assert
 (multiple-value-setq (a b) (values 4 5 6))
 4)

(my-assert
 a
 4)

(my-assert
 b
 5)

;;; values

(my-assert
 (values 1)
 1)

(my-assert
 (multiple-value-bind (n h)
     (values 1 2)
   (list n h))
 (1 2))

(my-assert
 (multiple-value-bind (n h j)
     (values 1 2 3)
   (list n h j))
 (1 2 3))

(my-assert
 (multiple-value-bind (n h j)
     (values (values 1 2 3) 4 5)
   (list n h j))
 (1 4 5))

(my-assert
 (defun polar (x y)
   (values (sqrt (+ (* x x) (* y y))) (atan y x)))
 POLAR)

(my-assert
 (multiple-value-bind (r theta) (polar 3.0 4.0)
   (vector r theta))
 #(5.0 0.9272952))

;;; values-list

(my-assert
 (values-list '(1))
 1)

(my-assert
 (multiple-value-bind (n h)
     (values-list '(1 2))
   (list n h))
 (1 2))

(my-assert
 (multiple-value-bind (n h j)
     (values-list '(1 2 3))
   (list n h j))
 (1 2 3))

;;; multiple-values-limit

(my-assert
 (>= MULTIPLE-VALUES-LIMIT 20)
 T)


;;; nth-value

(my-assert
 (nth-value 0 (values 'a 'b))
 A)

(my-assert
 (nth-value 1 (values 'a 'b))
 B)

(my-assert
 (nth-value 2 (values 'a 'b))
 NIL)

(my-assert
 (multiple-value-bind (n h j)
     (let* ((x 83927472397238947423879243432432432)
	    (y 32423489732)
	    (a (nth-value 1 (floor x y)))
	    (b (mod x y)))
       (values a b (= a b)))
   (list n h j))
 (3332987528 3332987528 t))

;;; prog

(my-assert
 (setq a 1)
 1)

(my-assert
 (prog ((a 2) (b a)) (return (if (= a b) '= '/=)))
 /=)

(my-assert
 (prog* ((a 2) (b a)) (return (if (= a b) '= '/=)))
 =)

(my-assert
 (prog () 'no-return-value)
 NIL)

;;; prog1

(my-assert
 (setq temp 1)
 1)

(my-assert
 (prog1 temp (print temp) (incf temp) (print temp))
 1)

(my-assert
 (prog1 temp (setq temp nil))
 2)

(my-assert
 temp
 NIL)

(my-assert
 (prog1 (values 1 2 3) 4)
 1 )

(my-assert
 (setq temp (list 'a 'b 'c))
 (A B C))

(my-assert
 (prog1 (car temp) (setf (car temp) 'alpha))
 A)

(my-assert
 temp
 (ALPHA B C))

(my-assert
 (multiple-value-bind (n h)
     (flet ((swap-symbol-values (x y)
				(setf (symbol-value x)
				      (prog1 (symbol-value y)
					(setf (symbol-value y) (symbol-value x))))))
       (let ((*foo* 1) (*bar* 2))
	 (declare (special *foo* *bar*))
	 (swap-symbol-values '*foo* '*bar*)
	 (values *foo* *bar*)))
   (list n h))
 (2 1))

(my-assert
 (setq temp 1)
 1)

(my-assert
 (prog2 (incf temp) (incf temp) (incf temp))
 3)

(my-assert
 temp
 4)

(my-assert
 (prog2 1 (values 2 3 4) 5)
 2)

;;; progn

(my-assert
 (progn)
 NIL)

(my-assert
 (progn 1 2 3)
 3)

(my-assert
 (multiple-value-bind (n h j)
     (progn (values 1 2 3))
   (list n h j))
 (1 2 3))

(my-assert
 (setq a 1)
 1)

(my-assert
 (if a
     (progn (setq a nil) 'here)
     (progn (setq a t) 'there))
 HERE)

(my-assert
 a
 NIL)

;;; define-modify-macro

(my-assert
 (define-modify-macro appendf (&rest args)
   append "Append onto list")
 APPENDF)

(my-assert
 (setq x '(a b c) y x)
 (A B C))

(my-assert
 (appendf x '(d e f) '(1 2 3))
 (A B C D E F 1 2 3))

(my-assert
 x
 (A B C D E F 1 2 3))

(my-assert
 y
 (A B C))

;;; defsetf

(my-assert
 (defun middleguy (x) (nth (truncate (1- (list-length x)) 2) x))
 MIDDLEGUY)

(my-assert
 (defun set-middleguy (x v)
   (unless (null x)
     (rplaca (nthcdr (truncate (1- (list-length x)) 2) x) v))
   v)
 SET-MIDDLEGUY)

(my-assert
 (defsetf middleguy set-middleguy)
 MIDDLEGUY)

(my-assert
 (setq a (list 'a 'b 'c 'd)
       b (list 'x)
       c (list 1 2 3 (list 4 5 6) 7 8 9))
 (1 2 3 (4 5 6) 7 8 9))

(my-assert
 (setf (middleguy a) 3)
 3)

(my-assert
 (setf (middleguy b) 7)
 7)

(my-assert
 (setf (middleguy (middleguy c)) 'middleguy-symbol)
 MIDDLEGUY-SYMBOL)

(my-assert
 a
 (A 3 C D))

(my-assert
 b
 (7))

(my-assert
 c
 (1 2 3 (4 MIDDLEGUY-SYMBOL 6) 7 8 9))

(my-assert
 (defsetf subseq (sequence start &optional end) (new-sequence)
   `(progn (replace ,sequence ,new-sequence
                    :start1 ,start :end1 ,end)
           ,new-sequence))
 SUBSEQ)

(unintern '*XY*)

(my-assert
 (defvar *xy* (make-array '(10 10) :initial-element NIL))
 *XY*)

(defun xy (&key ((:x x) 0) ((:y y) 0))
  (aref *xy* x y))

(defun set-xy (new-value &key ((:x x) 0) ((:y y) 0))
   (setf (aref *xy* x y) new-value))

(defsetf xy (&key ((:x x) 0) ((:y y) 0)) (store)
  `(set-xy ,store :x ,x :y ,y))

(my-assert
 (progn
   (get-setf-expansion '(xy :x a :y b))
   t)
 t)

(my-assert
 (xy :x 1)
 NIL)

(my-assert
 (setf (xy :x 1) 1)
 1)

(my-assert
 (xy :x 1)
 1)

(my-assert
 (setf (xy :x 1 :y 2) 3)
 3)

(my-assert
 (setf (xy :y 5 :x 9) 14)
 14)

(my-assert
 (xy :y 0 :x 1)
 1)

(my-assert
 (xy :x 1 :y 2)
 3)

;;; define-setf-expander

(my-assert
 (defun lastguy (x) (car (last x)))
 LASTGUY)

(my-assert
 (define-setf-expander lastguy (x &environment env)
   "Set the last element in a list to the given value."
   (multiple-value-bind (dummies vals newval setter getter)
       (get-setf-expansion x env)
     (let ((store (gensym)))
       (values dummies
	       vals
	       `(,store)
	       `(progn (rplaca (last ,getter) ,store) ,store)
	       `(lastguy ,getter)))))
 LASTGUY)

(my-assert
 (setq a (list 'a 'b 'c 'd)
       b (list 'x)
       c (list 1 2 3 (list 4 5 6)))
 (1 2 3 (4 5 6)))

(my-assert
 (setf (lastguy a) 3)
 3)

(my-assert
 (setf (lastguy b) 7)
 7)

(my-assert
 (setf (lastguy (lastguy c)) 'lastguy-symbol)
 LASTGUY-SYMBOL)

(my-assert
 a
 (A B C 3))

(my-assert
 b
 (7))

(my-assert
 c
 (1 2 3 (4 5 LASTGUY-SYMBOL)))

;;; setf

(my-assert
 (setq x (cons 'a 'b) y (list 1 2 3))
 (1 2 3) )

(my-assert
 (setf (car x) 'x (cadr y) (car x) (cdr x) y)
 (1 X 3) )

(my-assert
 x
 (X 1 X 3) )

(my-assert
 y
 (1 X 3) )

(my-assert
 (setq x (cons 'a 'b) y (list 1 2 3))
 (1 2 3) )

(my-assert
 (psetf (car x) 'x (cadr y) (car x) (cdr x) y)
 NIL )

(my-assert
 x
 (X 1 A 3) )

(my-assert
 y
 (1 A 3) )

;;; shiftf

(my-assert
 (setq x (list 1 2 3) y 'trash)
 TRASH)

(my-assert
 (shiftf y x (cdr x) '(hi there))
 TRASH)

(my-assert
 x
 (2 3))

(my-assert
 y
 (1 HI THERE))

(my-assert
 (setq x (list 'a 'b 'c))
 (A B C))

(my-assert
 (shiftf (cadr x) 'z)
 B)

(my-assert
 x
 (A Z C))

(my-assert
 (shiftf (cadr x) (cddr x) 'q)
 Z)

(my-assert
 x
 (A (C) . Q))

(my-assert
 (setq n 0)
 0)

(my-assert
 (setq x (list 'a 'b 'c 'd))
 (A B C D))

(my-assert
 (shiftf (nth (setq n (+ n 1)) x) 'z)
 B)

(my-assert
 x
 (A Z C D))

;;; rotatef

(my-assert
 (let ((n 0)
       (x (list 'a 'b 'c 'd 'e 'f 'g)))
   (rotatef (nth (incf n) x)
	    (nth (incf n) x)
	    (nth (incf n) x))
   x)
 (A C D B E F G))



























