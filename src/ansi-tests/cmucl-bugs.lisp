;;; -*- mode: lisp -*-
(in-package :cl-user)

;; Your fd-stream-read-n-bytes (in 17e) crashes when reading from a
;; pipe and it didn't get the requested byte-count (it should re-read
;; because pipe-reads may be interrupted). You have done some changes
;; in from 17c to 17e (I think) but it dosen't work yet. Here is a old
;; patched version that works for us.


;;An alist with SETF and a function name causes
;;an error whenever it's used:

(my-assert
 (defparameter foo '((setf . sqrt)))
 FOO)


(my-assert
 foo
 ((SETF . SQRT)))


(my-assert
 (setq foo '((zut . 4)))
 ((ZUT . 4)))


(my-assert
 foo
 ((ZUT . 4)))


(my-assert
 (setq foo '((setf . 3)))
 ((SETF . 3)))


(my-assert
 '(setq . 2)
 (setq . 2))

(unintern 'foo)

;;


(my-assert
 (* 10000000000000000000000000000000000000000
    10000000000000000000000000000000000000000)

 100000000000000000000000000000000000000000000000000000000000000000000000000000000)


(my-assert
 (time (+ 2 2))
 4)

;; cltl2 p 727


(my-assert
 (let ((stack (copy-list '(a b c d e f))))
   (loop for item = (length stack) then (pop stack) while stack
     collect item))
 (6 A B C D E))

;; p 737

(my-assert
 (loop with ( a b c) (float integer float)
   return (list a b c))
 (0.0 0 0.0))


(my-assert
 (loop with ( a b c) float
   return (list a b c))
 (0.0 0.0 0.0))


;; printing arrays


(my-assert
 (make-array '(22) :element-type 'single-float :initial-element 0.0)
 #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))


(my-assert
 (make-array '(2 2))
 #-clisp
 #2A((0 0) (0 0))
 #+clisp
 #2A((NIL NIL) (NIL NIL)))


(my-assert
 (make-array '(2 2) :element-type 'single-float :initial-element 0.0)
 #2A((0.0 0.0) (0.0 0.0)))

;; without pretty-print?

(my-assert
 (make-array '(22) :element-type 'single-float :initial-element 0.0)
 #(0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0 0.0))


(my-assert
 (make-array '(2 2))
 #-clisp
 #2A((0 0) (0 0))
 #+clisp
 #2A((NIL NIL) (NIL NIL)))

(my-assert
 (make-array '(2 2) :element-type 'single-float :initial-element 0.0)
 #2A((0.0 0.0) (0.0 0.0)))

;; bignums


(my-assert
 (defun factorial (n &optional (i 1))
   (if (plusp n) (factorial (1- n) (* i n)) i))
 FACTORIAL)


(my-assert
 (/ (factorial 100) (factorial 99))
 100)


(my-assert
 (/ (factorial 1000) (factorial 999))
 1000)

(unintern 'factorial)

(my-assert
 1e-37
 10.0e-38)


(my-assert
 1L-38
 10.0L-39)


(my-assert
 (flet ((print-result (x)
	  (format nil "~&x is ~F (a ~S)." x (type-of x))))
   (print-result "non-number"))
 "x is non-number (a (SIMPLE-BASE-STRING 10))."
 "Notice that ~3,2F does work.")

(my-assert
 (defun sigmoid (x)
   (/ 1 (1+ (exp (- x)))))
 SIGMOID)


(my-assert
 (compile 'sigmoid)			; in CMU Common Lisp 17f
 SIGMOID)

#-clisp
(my-assert
 (sigmoid 100)
 1.0)


(unintern 'sigmoid)

(my-assert
 (setq X (copy-list '((1 2) (1 2 3) (3))))
 ((1 2) (1 2 3) (3)))


(my-assert
 (remove-duplicates X :test #'subsetp)
 ((1 2 3) (3)))


(my-assert
 (delete-duplicates X :test #'subsetp)
 ((1 2 3) (3)))


(unintern 'X)

(my-assert
 (progn
   (run-program "/bin/date" '() :output t :error :stream)
   t)
 t)
;; #<process 780 :EXITED>



(my-assert
 (- 0.0 #C( 1.0 1.0))
 #C(-1.0 -1.0))


(my-assert
 (- #C(.5 .866) 0.0)
 #C(0.5 0.866))



(my-assert
 (/ 2.0 #C(-1.0 -1.0))
 #C(-1.0 1.0))


(my-assert
 (* 2.0 #C(-1.0 -1.0))
 #C(-2.0 -2.0))



(my-assert
 (with-open-file
  (foo "/tmp/foocl"
       :direction :output
       :element-type
       (list 'signed-byte (1+ (integer-length
			       most-positive-fixnum))))
  (write-byte 17 foo)
  (write-byte -17 foo)
  (write-byte 4517 foo)
  (write-byte -1217 foo))
 -1217)


(my-assert
 (with-open-file
  (foo "/tmp/foocl"
       :direction :input
       :element-type
       (list 'signed-byte (1+ (integer-length
			       most-positive-fixnum))))
  (list (read-byte foo)
	(read-byte foo)
	(read-byte foo)
	(read-byte foo)))
 (17 -17 4517 -1217))


(my-assert
 (unless (ignore-errors (error "grr"))
   (print "hi"))
 "hi")


(my-assert
 (setf (elt '(a b c d) 2) 'x)
 x)


(my-assert
 (acos 1.00001)
 #+(or cmu sbcl)
 #C(0.0 0.004475168)
 #+clisp
 #C(0 0.0044751023)
 #-(or clisp cmu sbcl)
 fill-this-in)


(my-assert
 (parse-namestring (make-pathname :defaults "tst"))
 #p"tst")


(my-assert
 (string< "abcd" "012abcz" :start2 3 :end2 6)
 NIL)


(my-assert
 (string> "abcd" "012abcd" :start2 3 :end2 5)
 2)


(my-assert
 (defun (setf foo) () t)
 (setf foo))


(my-assert
 (compile '(setf foo))
 (setf foo))


(my-assert
 (typep '(setf cons)
	'generic-function)
 NIL)


(my-assert
 (make-sequence '(vector float) 4  :initial-element 0.0)
 #(0.0 0.0 0.0 0.0))


(my-assert
 (typep (complex 0.0d0) '(complex double-float))
 t
 "complex returns a number whose real part is realpart
and whose imaginary part is imagpart.

If realpart is a rational and imagpart is the rational
number zero, the result of complex is realpart, a rational.
Otherwise, the result is a complex.

If either realpart or imagpart is a float, the non-float
is converted to a float before the complex is created. If
imagpart is not supplied, the imaginary part is a zero of
 the same type as realpart; i.e., (coerce 0 (type-of
realpart)) is effectively used.

the second parameter is not supplied, the first is
a double-float, so actually this is (complex 0.0d0 0.0d0)
these are not rationals, so we get a complex number back.
")


;; From: Gary Bunting <gbunting@cantor.une.edu.au>
(my-assert
 (setf xx (expt 3 32))
 1853020188851841)

(my-assert
 (* xx xx)
 3433683820292512484657849089281)


#|					;
(defun bugged (x)
  (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
	      (if y
		  (let ((a (pop y)))
		    (f a)))))

;;;; (f x) <<<
;;;; Error in function COMMON-LISP::ASSERT-ERROR:
;;;; The assertion (EQ (C::LAMBDA-TAIL-SET C::CALLER)
;;;;               (C::LAMBDA-TAIL-SET (C::LAMBDA-HOME C::CALLEE)))
;;;; failed.

;;; However this works ok.
    (f x nil)))
|#
(my-assert
 (defun bugged (x)
   (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
	       (if y
		   (let ((a (pop y)))
		     (f a)))))
     (f x)))
 BUGGED)

(my-assert
 (bugged (list (list)))
 NIL)

(unintern 'bugged)

(my-assert
 (defun tst()
   (with-open-file
    (stream "does-not-exist" :if-does-not-exist nil)
    (unless stream
      'abacab)))
 TST)

(my-assert
 (tst)
 abacab)

(unintern 'tst)

(my-assert
 (defun f (a b)
   (declare (type (single-float  0.0 0.5) a)
	    (type (single-float  0.0 0.2) b)
	    (optimize (debug 0) (safety 0) (speed 3)))
   (expt a b))
 F)

(my-assert
 (progn
   (compile 'f)
   t)
 t)

;;; deltax^2 == deltat

;;; from Paul Werkowski

(my-assert
 (progn
   (compile-file "compile-bug5.lisp")
   :ok)
 :ok)

(my-assert
 (progn
   (compile-file "compile-bug6.lisp")
   :ok)
 :ok)

(my-assert
 (progn
   (defclass cl1 ()())
   (defclass cl2 (cl1 missing)())
   (defclass cl4 ()())

   (defmethod foo ((c cl2))
     c)
   ;; method specializing on class with fwd reference
   ;; ok so far

   ;; then this dies

   (defmethod foo ((c cl4))
     c)   ;; add a new method to gf #'foo
   t)
 T)

(my-assert
 (progn
   (defmethod foo ((f function))
     f)
   (defun zzz (x)
     x)
   (foo #'zzz)   ;; this is supposed to work.
   t)
 t)

(unintern 'zzz)

#+(or sbcl cmu)
(my-assert
 (progn
   (compile-file "compile-bug1.lisp")
   :ok)
 :ok)



;;; From: William Harold Newman <william.newman@airmail.net>

(my-assert
 (equalp #\a 'a)
 nil)

(defun my-sxhash (x)
  (declare (type double-float x))
  (sxhash x))

(my-assert
 (eq (my-sxhash 1.2d0)
     (sxhash 1.2d0))
 T)

(my-assert
 (progn
   (compile 'my-sxhash)
   (eq (my-sxhash 1.2d0)
       (sxhash 1.2d0)))
 T)


;;; From: Raymond Toy <toy@rtp.ericsson.se>

(defun tst2 (x n)
  (declare (type (integer -134217728 134217728) x)
           (type (integer -4 4) n)
           (optimize (speed 3) (safety 0)))
  (ash x n))

(my-assert
 (compile 'tst2)
 tst2)

;; From pvaneynd:

(my-assert
 (exp 1)
 2.7182817)

(my-assert
 (macrolet ((foobar (a b)
		    `(+ ,a ,b)))
   (foobar 2 4))
 6)

(my-assert
 (macrolet ((foobar (a b)
		    `(+ ,a ,b)))
   (foobar 2 4 5 6))
 program-error)


;;; From: Marco Antoniotti <marcoxa@parades.rm.cnr.it>

(my-assert
 (progn
   (defclass ccc () ())
   (setf (find-class 'ccc1) (find-class 'ccc))
   :ok)
 :ok)

(my-assert
 (progn
   (defmethod zut ((c ccc1)) 123)
   :ok)
 :ok)


;;; From: Fred Gilham <gilham@snapdragon.csl.sri.com>


(my-assert
 (progn
   (compile-file "compile-bug2.lisp")
   :ok)
 :ok)

;;; From: lyle@cogni.iaf.cnrs-gif.fr (Lyle Borg-Graham)

(defun foo ()
  (loop for x from 1.0 to 10.0
    maximize x into max single-float))

(my-assert
 (compile 'foo)
 foo)

;;; From: Timothy Miller <tsm@cs.brown.edu>

#+(or cmu sbcl)
(my-assert
 (> 2 single-float-positive-infinity)
 NIL)

;;; From: "Fernando D. Mato Mira" <matomira@iname.com>

(defun prolog-length (p)
  (let ((x (length (car p))))
    (reduce #'(lambda (v1 v2)
		(declare (ignore v1))
		(setq x (+ x (length v2))))
	    p)))

(my-assert
 (compile 'prolog-length)
 prolog-length)

(my-assert
 (prolog-length (list (list 1 2)
		      (list 3)))
 3)

(my-assert
 (progn
   (compile-file "compile-bug3.lisp")
   :ok)
 :ok)

(my-assert
 (progn
   (compile-file "compile-bug4.lisp")
   :ok)
 :ok)

(my-assert
 (progn
   (compile-file "compile-bug4nt.lisp")
   :ok)
 :ok)

(my-assert
 (prolog-length (list (list 1 2)
		      (list 3)))
 3)

;;; From: Sam Steingold <sds@gnu.org>
#+UNIX
(my-assert
 (let ((z (make-concatenated-stream
	   (make-string-input-stream "abc")
	   (open "/etc/hosts"))))
   (read-line z)
   (concatenated-stream-streams z)
   :ok)
 :ok)


;;; From: Hannu Koivisto <azure@iki.fi>

(my-assert
 (case t)
 nil)

;;; From: Raymond Toy <toy@rtp.ericsson.se>

(my-assert
 (progn
  (with-open-file (file "/tmp/foobar"
			:direction :output
			:if-exists :supersede)
		  (princ #\F file))
  (with-open-file (file "/tmp/foobar"
			:direction :input)
		  (let ((c (peek-char nil file nil 'eof t)))
		    (list c (read file)
			  (peek-char nil file nil 'eof t)))))
 (#\F F EOF))

;;; From Barry Margolin:

#+cmu
(my-assert
 (> (length
     (pcl:generic-function-lambda-list
      (ensure-generic-function 'change-class)))
    2)
 T
 "change-class (instance t) (new-class symbol) &rest initargs")

;;; From the clisp CHANGES file:





