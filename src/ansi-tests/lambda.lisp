;;; based on 1.1.1.1 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (makunbound 'b) b)

(my-assert
 (makunbound 'e) e)

(my-assert
 (setq z 2) 2)

(my-assert
 ((lambda (z) (declare (special z)) (list z (symbol-value 'z))) 3)
 (3 3))

(my-assert
 (makunbound 'z) z)

(my-assert
 ((lambda (a b) (+ a (* b 3))) 4 5)
 19)

(my-assert
 ((lambda (a &optional (b 2)) (+ a (* b 3))) 4 5)
 19)

(my-assert
 ((lambda (a &optional (b 2)) (+ a (* b 3))) 4)
 10)

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)))
 (2 nil 3 nil nil))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)
 (6 t 3 nil nil))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)
 (6 t 3 t nil))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3
  8)
 (6 t 3 t (8)))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3
  8 9 10 11)
 (6 t 3 t (8 9 10 11)))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2)
 (1 2 nil nil))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6)
 (1 2 6 nil))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8)
 (1 2 nil 8))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6 :d 8)
 (1 2 6 8))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8 :c 6)
 (1 2 6 8))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) :a 1 :d 8 :c 6)
 (:a 1 6 8))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) :a :b :c :d)
 (:a :b :d nil))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  1)
 (1 3 nil 1 nil))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  1 2)
 (1 2 nil 1 nil))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  :c 7)
 (:c 7 nil :c nil))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  1 6 :c 7)
 (1 6 7 1 (:c 7)))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  1 6 :d 8)
 (1 6 nil 8 (:d 8)))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a)) (list a b c d x))
  1 6 :d 8 :c
  9 :d 10)
 (1 6 9 8 (:d 8 :c 9 :d 10)))

(my-assert
 ((lambda (x &aux (a 3) (b 4)) (+ x (* a b))) 2)
 14)

(my-assert
 ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
	  (+ x y a (* b (car z)) c (* d u) v))
  3 4 5 2 7 :c 6 :d 8)
 program-error)

(my-assert
 ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
	  (+ x y a (* b (car z)) c (* d u) v))
  3 4 5 2 7 :c 6)
 program-error)

(my-assert
 ((lambda (x &aux c) (cons x c)) (quote a))
 (a))

(my-assert
 ((lambda (x &rest y z) (list x y z)) 1 2 3)
 error)

(my-assert
 ((lambda (5 a b) (list a b)) 1 2)
 error)

(my-assert
 ((lambda ((length (quote (a b))) c) (list c)) 1)
 error)

(my-assert
 ((lamda (x &key :y :z) (list x y z)) 1 :y 2 :z 3)
 error)

(my-assert
 ((lambda (x y) (list x y z)) 1 2)
 unbound-variable)

(my-assert
 ((lambda (x y) (list x y z)) 1 2 3)
 error)

(my-assert
 ((lambda (&optional) (list a b c)) 1)
 error)

(my-assert
 ((lambda (&optional (a)) (list a)) 1)
 (1))

(my-assert
 ((lambda (&optional (a b)) (list a b)) 1)
 unbound-variable)

(my-assert
 ((lambda (&optional (a 3 b)) (list a b)) 1)
 (1 t))

(my-assert
 ((lambda (&optional (a 3)) (list a)) 1)
 (1))

(my-assert
 ((lambda (&optional (a 3 b 4)) (list a b)) 1)
 #+xcl (1 t)
 #-xcl error)

(my-assert
 ((lambda (x) (list x y)) 1 2)
 error)

(my-assert
 ((lambda (x) (list x)) 1 2)
 error)

(my-assert
 ((lambda (#\a) (list a)) 1)
 error)

(my-assert
 ((lambda (#*10) (list 1 2 3)))
 error)

(my-assert
 ((lambda (x y) ((lambda (a b) (list a b)) (quote u) (quote v))) 5 6)
 (u v))

(my-assert
 ((lambda (x y) (list x y)) 1)
 error)

(my-assert
 ((lambda (x &rest y &optional (z 5)) (list x y z)) 1 3)
 error)

(my-assert
 ((lambda (x &x) (list x)) 7)
 error)

(my-assert
 ((lambda (x &aux) (list x)) 6)
 (6))

(my-assert
 ((lambda (x &aux y) (list x y)) 6)
 (6 nil))

(my-assert
 ((lambda (x &aux (y)) (list x y)) 6)
 (6 nil))

(my-assert
 ((lambda (x &rest) (list x)) 2)
 error)

(my-assert
 ((lambda (x &key) (list x)) 3)
 (3))

(my-assert
 ((lambda (x &key y) (list x)) 3)
 (3))

(my-assert
 ((lambda (x &key y) (list x)) 3 :y)
 error)

(my-assert
 ((lambda (x &key y) (list x)) :\3)
 (:\3))

(my-assert
 ((lambda nil (list 1 2 3)))
 (1 2 3))

(my-assert
 ((lambda nil (list 1 2 3)) 4 5)
 error)

(my-assert
 ((lambda (list 1 2 3)))
 error)

(my-assert
 ((lambda (x)))
 error)

(my-assert
 ((lambda (&aux &key &rest &optional)))
 error)

(my-assert
 ((lambda (a b &key c d &allow-other-keys) (list a b c d e f)) 1 2 :c
  6 :d 8 :e 5
  :f 7)
 error)

(my-assert
 ((lambda (x &allow-other-keys) (list x y)) 2 :y 3)
 error)

(my-assert
 ((lambda))
 error)

