;;; 2.1.4.5.1 examples of mutiple escape characters -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(my-assert
 (eq 'abc 'ABC)
 T)

(my-assert
 (eq 'abc '|ABC|)
 T)

(my-assert
 (eq 'abc 'a|B|c)
 T)

(my-assert
 (eq 'abc '|abc|)
 nil)

;;; 2.1.4.6.1
(my-assert
 (eq 'abc '\A\B\C)
 T)

(my-assert
 (eq 'abc 'a\Bc)
 T)

(my-assert
 (eq 'abc '\ABC)
 T)

(my-assert
 (eq 'abc '\abc)
 nil)

;;; 2.1.4.7.1
(my-assert
 (length '(this-that))
 1)

(my-assert
 (length '(this - that))
 3)

(my-assert
 (length '(a
           b))
 2)

(my-assert
 (+ 34)
 34)

(my-assert
 (+ 3 4)
 7)

;;; 2.4.1

(my-assert
 (cons 'this-one 'that-one)
 (this-one . that-one))


;;; 2.4.3.1

(my-assert
 'foo
 FOO)

(my-assert
 ''foo
 (QUOTE FOO))

(my-assert
 (car ''foo)
 QUOTE)

;;; 2.4.4.1

(my-assert
 (+ 3					; three
    4)
 7)

;;; 2.4.8.7

(my-assert
 #B1101
 13 )

(my-assert
 #b101/11
 5/3)

;;; 2.4.8.8
(my-assert
 #o37/15
 31/13)

(my-assert
 #o777
 511)

(my-assert
 #o105
 69)

;;; 2.4.8.9
(my-assert
 #xF00
 3840             )

(my-assert
 #x105
 261 )

;;; 2.4.8.10
(my-assert
 #2r11010101
 213)

(my-assert
 #b11010101
 213)

(my-assert
 #b+11010101
 213)

(my-assert
 #o325
 213)

(my-assert
 #xD5
 213)

(my-assert
 #16r+D5
 213)

(my-assert
 #o-300
 -192)

(my-assert
 #3r-21010
 -192)

(my-assert
 #25R-7H
 -192)

(my-assert
 #xACCEDED
 181202413)


