;;; Based on 1.1.1.1 -*- mode: lisp -*-
;;; testen von backquote
(in-package :cl-user)

(my-assert
 (setf x (list 'a 'b 'c))
 (a b c))

(my-assert
 `(x ,x ,@x foo ,(cadr  x) bar ,(cdr x) baz ,@(cdr x) ,. x)
 (X (A B C) A B C FOO B BAR (B C) BAZ B C A B C))

(my-assert
 (read-from-string "`,@x")
 ERROR)

(my-assert
 `(,x . ,x)				; = (append (list x) x)
 ((a b c) a b c))


(my-assert
 (read-from-string "`(,x . ,@x)")
 ERROR)


(my-assert
 (read-from-string ",x")
 ERROR)

(my-assert
 `#(1 2 3 4)
 #(1 2 3 4))

