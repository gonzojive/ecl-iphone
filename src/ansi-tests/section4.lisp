;;; types -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;coerce
(my-assert
 (coerce '(a b c) 'vector)
 #(A B C))

(my-assert
 (coerce '(a b c) 'list)
 (A B C))

(my-assert
 (coerce '(#\A #\B #\C) 'string)
 "ABC")

(my-assert
 (coerce #(a b c) 'vector)
 #(A B C))

(my-assert
 (coerce #(a b c) 'list)
 (A B C))

(my-assert
 (coerce #(#\A #\B #\C) 'string)
 "ABC")

(my-assert
 (coerce "ABC" 'vector)
 #(#\A #\B #\C))

(my-assert
 (coerce "ABC" 'list)
 (#\A #\B #\C))

(my-assert
 (coerce "ABC" 'string)
 "ABC")

(my-assert
 (coerce '(a b c) '(vector * 3))
 #(A B C))

(my-assert
 (coerce '(a b c) 'list)
 (A B C))

(my-assert
 (coerce '(#\A #\B #\C) '(string 3))
 "ABC")

(my-assert
 (coerce #(a b c) '(vector * 3))
 #(A B C))

(my-assert
 (coerce #(a b c) 'list)
 (A B C))

(my-assert
 (coerce #(#\A #\B #\C) '(string 3))
 "ABC")

(my-assert
 (coerce "ABC" '(vector * 3))
 #(#\A #\B #\C))

(my-assert
 (coerce "ABC" 'list)
 (#\A #\B #\C))

(my-assert
 (coerce "ABC" '(string 3))
 "ABC")

(my-assert
 (coerce 'a 'character)
 #\A)

(my-assert
 (coerce 4.56 'complex)
 #C(4.56 0.0))

(my-assert
 (coerce 4.5s0 'complex)
 #C(4.5s0 0.0s0))

(my-assert
 (coerce 7/2 'complex)
 7/2)

(my-assert
 (coerce 0 'short-float)
 0.0s0)

(my-assert
 (coerce 3.5L0 'float)
 3.5L0)

(my-assert
 (coerce 7/2 'float)
 3.5)

(my-assert
 (coerce (cons 1 2) t)
 (1 . 2))

(my-assert
 (coerce '(a b c) '(vector * 4))
 type-error)

(my-assert
 (coerce #(a b c) '(vector * 4))
 type-error)

(my-assert
 (coerce '(a b c) '(vector * 2))
 type-error)

(my-assert
 (coerce #(a b c) '(vector * 2))
 type-error)

(my-assert
 (coerce "foo" '(string 2))
 type-error)

(my-assert
 (coerce #(#\a #\b #\c) '(string 2))
 type-error)

(my-assert
 (coerce '(0 1) '(simple-bit-vector 3))
 type-error)

;; subtypep
(my-assert
 (multiple-value-bind (a b)
     (subtypep 'compiled-function 'function)
   (list a b))
 (T T)
 "Type COMPILED-FUNCTION 

Supertypes:

compiled-function, function, t
...
")

(my-assert
 (multiple-value-bind (a b)
     (subtypep 'null 'list)
   (list a b))
 (T T))

(my-assert
 (multiple-value-bind (a b)
     (subtypep 'null 'symbol)
   (list a b))
 (T T))

(my-assert
 (multiple-value-bind (a b)
     (subtypep 'integer 'string)
   (list a b))
 (nil #-clisp T
      #+clisp nil))

(my-assert
 (multiple-value-bind (a b)
     (subtypep '(satisfies dummy) nil)
   (list a b))
 (nil #-clisp t
      #+clisp nil))

(my-assert
 (multiple-value-bind (a b)
     (subtypep '(integer 1 3) '(integer 1 4))
   (list a b))
 (T T))

(my-assert
 (multiple-value-bind (a b)
     (subtypep '(member) 'nil)
   (list a b))
 (T T))					;   true, true ;or false, false

(my-assert
 (multiple-value-bind (a b)
     (subtypep 'nil '(member))
   (list a b))
 (T T))					; true, true ;or false, false

;;; type-of

(my-assert
 (type-of 'a)
 SYMBOL          )

(my-assert
 (type-of '(1 . 2))
 CONS)
					; OR=>  (CONS FIXNUM FIXNUM)

(my-assert
 (type-of #c(0 1))
 #-cmu COMPLEX
 #+cmu (COMPLEX BIT))
					; OR=>  (COMPLEX INTEGER)

(my-assert
 (defstruct temp-struct x y z)
 TEMP-STRUCT)

(my-assert
 (type-of (make-temp-struct))
 TEMP-STRUCT)

(my-assert
 (type-of "abc")
 #+(or cmu sbcl clisp)
 (SIMPLE-BASE-STRING 3)
 #-(or cmu sbcl clisp)
 STRING)
					; OR=>  (STRING 3)

(my-assert
 (multiple-value-bind (a b)
     (subtypep (type-of "abc") 'string)
   (list a b))
 (T T))

(my-assert
 (type-of (expt 2 40))
 BIGNUM)
					; OR=>  INTEGER
					; OR=>  (INTEGER 1099511627776 1099511627776)
					; OR=>  SYSTEM::TWO-WORD-BIGNUM
					; OR=>  FIXNUM

(my-assert
 (multiple-value-bind (a b)
     (subtypep (type-of 112312) 'integer)
   (list a b))
 (T T))

(my-assert
 (defvar *foo* (make-array 5 :element-type t))
 *FOO*)

(my-assert
 (class-name (class-of *foo*))
 #+(or cmu sbcl) SIMPLE-VECTOR
 #-(or cmu sbcl) VECTOR)

(my-assert
 (type-of *foo*)
 #+(or cmu sbcl clisp)
 (SIMPLE-VECTOR 5)
 #-(or cmu sbcl clisp)
 VECTOR)
					; OR=>  (VECTOR T 5)

;;; typep

(my-assert
 (typep 12 'integer)
 T)

(my-assert
 (typep (1+ most-positive-fixnum) 'fixnum)
 nil)

(my-assert
 (typep nil t)
 t)

(my-assert
 (typep nil nil)
 nil)

(my-assert
 (typep 1 '(mod 2))
 t )

(my-assert
 (typep #c(1 1) '(complex (eql 1)))
 t )

;; To understand this next example, you might need to refer to
;; Section 12.1.5.3 (Rule of Canonical Representation for Complex Rationals).
(my-assert
 (typep #c(0 0) '(complex (eql 0)))
 nil
 "(upgraded-complex-part-type '(eql 0)) -> RATIONAL
a subtype of REAL. So it should work.

12.1.5.3:
also #C(5 0) is eql to 5
     #C(5.0 0.0) is not eql to 5.0
CMUCL bombs here because of the eql. We give two
replacement tests below:
")

(my-assert
 (typep #c(1 1) 'complex)
 T
 "Because #C(1 1) remains an complex")

(my-assert
 (typep #c(3/2 0) 'complex)
 NIL
 "Because #C(3/2 0) is eql to 3/2")

(my-assert
 (typep #c(1 0) 'complex)
 NIL
 "Because #c(0 0) is eql to 0")

(my-assert
 (typep #c(0.0 0.0) 'complex)
 T
 "Because #c(0.0 0.0) remains a complex")

;;; type-error-datum
;;(my-assert
;;(progn
;;  (defun fix-digits (condition)
;;    (check-type condition type-error)
;;    (let* ((digits '(zero one two three four
;;			  five six seven eight nine))
;;	   (val (position (type-error-datum condition) digits)))
;;      (if (and val (subtypep 'number (type-error-expected-type condition)))
;;	  (store-value 7))))

;;  (defun foo (x)
;;    (handler-bind ((type-error #'fix-digits))
;;		  (check-type x number)
;;		  (+ x 3)))
;;  t)
;;t)

;;(my-assert
;;(foo 'seven)
;;10)







