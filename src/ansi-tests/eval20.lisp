;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

;; testen abschitt 20


;;  eval

(my-assert
 (eval (list 'cdr
	     '(car (list (cons 'a 'b) 'c))))
 b)

(my-assert
 (makunbound 'x)
 x)

(my-assert
 (eval 'x)
 UNBOUND-VARIABLE)

(my-assert
 (setf x 3)
 3)

(my-assert
 (eval 'x)
 3)

;; constantp

(my-assert
 (constantp 2)
 T)

(my-assert
 (constantp #\r)
 T)

(my-assert
 (constantp "max")
 T)

(my-assert
 (constantp '#(110))
 T)

(my-assert
 (constantp :max)
 T)

(my-assert
 (constantp T)
 T)

(my-assert
 (constantp NIL)
 T)

(my-assert
 (constantp 'PI)
 #-CLISP T
 #+CLISP NIL)

(my-assert
 (constantp '(quote foo))
 T)

