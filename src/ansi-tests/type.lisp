;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (typep (quote a) (quote symbol))
 t)

(my-assert
 (typep (quote nil) (quote symbol))
 t)

(my-assert
 (typep (quote (nil)) (quote symbol))
 nil)

(my-assert
 (typep 3 (quote integer))
 t)

(my-assert
 (typep 3 (quote (integer 0 4)))
 t)

(my-assert
 (typep 3 (quote (integer 0 3)))
 t)

(my-assert
 (typep 3 (quote (integer 0 2)))
 nil)

(my-assert
 (typep 3 (quote (float 0.0 2.0)))
 nil)

(my-assert
 (typep 3 (quote (float 0.0 2.0)))
 nil)

(my-assert
 (typep 3 (quote (float 0.0 4.0)))
 nil)

(my-assert
 (typep 3.2 (quote (float 0.0 4.0)))
 t)

(my-assert
 (typep 3.2 (quote (float 0.0 3.2)))
 t)

(my-assert
 (typep 3.2 (quote (float 0.0 (3.2))))
 nil)

(my-assert
 (typep 3.2 (quote (short-float 0.0s0 3.2s0)))
 #+(or allegro cmu sbcl) t
 #-(or allegro cmu sbcl) nil)

(my-assert
 (typep 3.2 (quote (single-float 0.0f0 3.2f0)))
 t)

(my-assert
 (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
 nil)

(my-assert
 (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
 nil)

(my-assert
 (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
 nil)

(my-assert
 (typep 3.2s0 (quote (double-float 0.0d0 3.2d0)))
 nil)

(my-assert
 (typep 3.2 (quote (double-float 0.0d0 3.2d0)))
 nil)

(my-assert
 (typep 3.2 (quote (float 0.0 3.2)))
 t)

(my-assert
 (typep 3.2s0 (quote (float 0.0s0 3.2s0)))
 t)

(my-assert
 (typep 2.0s0 (quote (short-float 0.0s0 3.0s0)))
 t)

(my-assert
 (typep 2.0s0 (quote (single-float 0.0f0 3.0f0)))
 #+(or allegro cmu sbcl) t
 #-(or allegro cmu sbcl) nil)

(my-assert
 (typep 2.0 (quote (single-float 0.0f0 3.0f0)))
 t)

(my-assert
 (typep 2.0d0 (quote (double-float 0.0d0 3.0d0)))
 t)

(my-assert
 (typep 3.0d0 (quote (double-float 0.0d0 3.0d0)))
 t)

(my-assert
 (typep 3.0d0 (quote (double-float 0.0d0 (3.0d0))))
 nil)

(my-assert
 (typep 4 (quote (mod 4)))
 nil)

(my-assert
 (typep 4 (quote (mod 5)))
 t)

(my-assert
 (typep 4 (quote (rational 2 5)))
 t)

(my-assert
 (typep 4 (quote (rational 2 7/2)))
 nil)

(my-assert
 (typep 4 (quote (rational 2 9/2)))
 t)

(my-assert
 (typep 4 (quote (rational 2 4)))
 t)

(my-assert
 (typep 4/3 (quote (rational 2 4)))
 nil)

(my-assert
 (typep 2 (quote (rational 2 4)))
 t)

(my-assert
 (typep "abcd" (quote string))
 t)

(my-assert
 (typep "abcd" (quote (string 4)))
 t)

(my-assert
 (typep "abcd" (quote (string 43)))
 nil)

(my-assert
 (typep '#(2 3) (quote (complex integer)))
 nil)

(my-assert
 (typep '#(2 3) (quote complex))
 nil)

(my-assert
 (typep #c(2 3) (quote complex))
 t)

(my-assert
 (typep #c(2 3) (quote (complex integer)))
 t)

(my-assert
 (typep #c(2 3) (quote (complex float)))
 nil)

(my-assert
 (typep #c(2 3) (quote (complex symbol)))
 #+(or cmu sbcl) error
 #-(or cmu sbcl) nil)

(my-assert
 (typep '#(a b c d) (quote vector))
 t)

(my-assert
 (typep '#(a b c d) (quote (vector * 4)))
 t)

#|
;;
;; hängt von (upgraded-array-element-type 'symbol) ab!
(typep '#(a b c d) (quote (vector symbol 4)))
nil
|#

(my-assert
 (typep (quote a) (quote (symbol cons)))
 error)

(my-assert
 (typep (quote a) (quote (or cons symbol)))
 t)

(my-assert
 (typep (quote a) (quote (or cons number)))
 nil)

(my-assert
 (typep (quote a) (quote (or atom number)))
 t)

(my-assert
 (typep (quote a) (quote (and atom number)))
 nil)

(my-assert
 (typep (quote 2) (quote (and atom number)))
 t)

(my-assert
 (typep (quote 2) (quote (member 1 2 3)))
 t)

(my-assert
 (typep (quote 2) (quote (member 1 3)))
 nil)

(my-assert
 (typep (quote 2) (quote (not (member 1 3))))
 t)

(my-assert
 (typep (quote 2) (quote (not (member 1 2 3))))
 nil)

(my-assert
 (typep 2 (quote (and number (not symbol))))
 t)

(my-assert
 (typep 2 (quote (and string (not symbol))))
 nil)

(my-assert
 (typep 2 (quote (or string (not symbol))))
 t)

(my-assert
 (typep (quote cons) (quote function))
 nil)

(my-assert
 (typep (quote cons) (quote (satisfies functionp)))
 nil)

(my-assert
 (typep (quote cons) (quote (satisfies not)))
 nil)

(my-assert
 (typep (quote nil) (quote (satisfies not)))
 t)

(my-assert
 (typep (quote nil) nil)
 nil)

(my-assert
 (typep (quote t) nil)
 nil)

(my-assert
 (subtypep (quote cons) t)
 t)

(my-assert
 (subtypep nil (quote cons))
 t)

(my-assert
 (subtypep (quote cons) (quote list))
 t)

(my-assert
 (subtypep (quote cons) (quote (or atom cons)))
 t)

(my-assert
 (subtypep (quote cons) (quote (and atom cons)))
 nil)

(my-assert
 (subtypep (quote cons) (quote (not atom)))
 #-(or clisp akcl allegro) t
 #+(or clisp akcl allegro) nil
 "Type atom: is equivalent to (not cons)")

(my-assert
 (subtypep (quote list) (quote (not atom)))
 nil)

(my-assert
 (subtypep (quote (integer 1 5)) (quote (integer 0 7)))
 t)

(my-assert
 (subtypep (quote (integer 1 5)) (quote (integer 0 (5))))
 nil)

(my-assert
 (subtypep (quote (integer 1 5)) (quote (integer 0 5)))
 t)

(my-assert
 (subtypep (quote (integer 1 5)) (quote (mod 5)))
 nil)

(my-assert
 (subtypep (quote (integer 1 (5))) (quote (mod 5)))
 t)

(my-assert
 (subtypep '(or (integer 1 (5) float))
	   '(or float (mod 5)))
 #+(or xcl clisp ecls) t
 #+(or allegro cmu sbcl) error
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (subtypep '(or (integer 1 (5)) float)
	   '(or float (mod 5)))
 t)

(my-assert
 (subtypep '(and number (float 1.0 (5.0)))
	   '(or float (mod 5)))
 t)

(my-assert
 (subtypep '(and number (not (float 1.0 (5.0))))
	   '(or float (mod 5)))
 nil)


(my-assert
 (subtypep '(and float (not (float 1.0 (5.0))))
	   '(or float (mod 5)))
 t
 "a float that is not in [1-5[ is a subtype of float")

(my-assert
 (subtypep '(and float (not (float 1.0 (5.0))))
	   '(or (float * 1.0) (float * 5.0)))
 nil)

(my-assert
 (subtypep '(satisfies consp)
	   'list)
 nil)

(my-assert
 (subtypep (quote simple-string) (quote array))
 t)

(my-assert
 (deftype mod1 (n) `(and number (float 0.0 (,n))))
 mod1)

(my-assert
 (typep 4.1 (quote (mod1 5.0)))
 t)

(my-assert
 (typep 4.1 (quote (mod1 4.1)))
 nil)

(my-assert
 (subtypep (quote (float 2.3 6.7)) (quote (mod1 6.8)))
 t)

(my-assert
 (subtypep (quote (float 2.3 6.7)) (quote (mod1 6.7)))
 nil)

(my-assert
 (defun beliebiger-test (a) (member a (quote (u i v x))))
 beliebiger-test)

(my-assert
 (not (null (typep (quote u) (quote (satisfies beliebiger-test)))))
 t)

(my-assert
 (typep (quote a) (quote (satisfies beliebiger-test)))
 nil)

;; This looks like asking a bit _too_ much
;; of the type system [pve]
(my-assert
 (subtypep (quote (member u i)) (quote (satisfies beliebiger-test)))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)

(my-assert
 (subtypep (quote (or (member u i))) (quote (satisfies beliebiger-test)))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)


(my-assert
 (subtypep (quote (or (member u i a))) (quote (satisfies beliebiger-test)))
 nil)

(my-assert
 (subtypep (quote (satisfies beliebiger-test))
	   (quote (member u i v x y)))
 nil)

(my-assert
 (deftype beliebiger-typ nil (quote (satisfies beliebiger-test)))
 beliebiger-typ)

(my-assert
 (not (null (typep (quote u) (quote beliebiger-typ))))
 t)

(my-assert
 (typep (quote a) (quote beliebiger-typ))
 nil)

(my-assert
 (subtypep (quote (member u i)) (quote beliebiger-typ))
 #-(or cmu sbcl) t
 #+(or cmu sbcl) nil)


(my-assert
 (subtypep (quote beliebiger-typ) (quote (member u i v x y)))
 nil)

(my-assert
 (subtypep nil 'fixnum) t)

(my-assert
 (subtypep 'short-float 'float ) t)

(my-assert
 (subtypep 'single-float 'float ) t)

(my-assert
 (subtypep 'double-float 'float ) t)

(my-assert
 (subtypep 'long-float 'float ) t)

(my-assert
 (subtypep 'null 'symbol) t)

(my-assert
 (subtypep 'null 'list) t)

(my-assert
 (subtypep 'cons 'list) t)

(my-assert
 (subtypep 'string 'vector) t)

(my-assert
 (subtypep 'bit-vector 'vector) t)
(my-assert
 (subtypep 'vector 'array) t)

(my-assert
 (subtypep 'simple-array 'array) t)

(my-assert
 (subtypep 'simple-vector 'simple-array) t)

(my-assert
 (subtypep 'simple-vector 'vector) t)

(my-assert
 (subtypep 'simple-string 'simple-array) t)

(my-assert
 (subtypep 'simple-bit-vector 'simple-array) t)

(my-assert
 (subtypep 'simple-string 'string) t)

(my-assert
 (subtypep 'simple-string 'vector) t)

(my-assert
 (subtypep 'simple-string 'simple-vector) nil)

(my-assert
 (subtypep 'simple-bit-vector 'bit-vector) t)

(my-assert
 (subtypep 'bit-vector 'vector) t)

(my-assert
 (subtypep 'simple-bit-vector 'simple-vector) nil)

(my-assert
 (subtypep 'unsigned-byte 'integer) t)

(my-assert
 (subtypep 'signed-byte 'integer) t)
