;;; based on v1.4 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (setf li1 '(a (b) ((c) (d)) )  vec1 '#(0 1 2 3))
 #(0 1 2 3))

(my-assert
 (setf pa 'old)
 old)

(my-assert
 (psetf pa 'new pao pa)
 nil)

(my-assert
 pa
 new)

(my-assert
 pao
 old)

(my-assert
 (setf (nth 1 li1) (quote uu))
 uu)

(my-assert
 (eval (quote li1))
 (a uu ((c) (d))))

(my-assert
 (setf (elt li1 1) (quote oo))
 oo)

(my-assert
 (setf (elt vec1 1) (quote oo))
 oo)

(my-assert
 (eval (quote li1))
 (a oo ((c) (d))))

(my-assert
 (eval (quote vec1))
 #(0 oo 2 3))

(my-assert
 (setf (rest li1) (quote ((ww))))
 ((ww)))

(my-assert
 (eval (quote li1))
 (a (ww)))

(my-assert
 (setf (first li1) (quote aa))
 aa)

(my-assert
 (first li1)
 aa)

(my-assert
 (setf (second li1) (quote bb))
 bb)

(my-assert
 (eval (quote li1))
 (aa bb))

(my-assert
 (setf (third li1) (quote bb))
 type-error)

(my-assert
 (eval (quote li1))
 (aa bb))


(my-assert
 (setf (rest li1) (quote (2 3 4 5 6 7 8 9 10)))
 (2 3 4 5 6 7 8 9 10))

(my-assert
 (setf (second li1) 22)
 22)

(my-assert
 (eval (quote li1))
 (aa 22 3 4 5 6 7 8 9 10))

(my-assert
 (setf (third li1) (quote 33))
 33)

(my-assert
 (setf (fourth li1) (quote 44))
 44)

(my-assert
 (setf (fifth li1) (quote 55))
 55)

(my-assert
 (setf (sixth li1) (quote 66))
 66)

(my-assert
 (setf (seventh li1) (quote 77))
 77)

(my-assert
 (setf (eighth li1) (quote 88))
 88)

(my-assert
 (setf (ninth li1) (quote 99))
 99)

(my-assert
 (setf (tenth li1) (quote 1010))
 1010)

(my-assert
 (eval (quote li1))
 (aa 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (first li1) (quote (((a)))))
 (((a))))

(my-assert
 (setf (caaar li1) (quote uu))
 uu)

(my-assert
 (caaar li1)
 uu)

(my-assert
 (car li1)
 ((uu)))

(my-assert
 (setf (caar li1) (quote oo))
 oo)

(my-assert
 (eval (quote li1))
 ((oo) 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (car li1) (quote ii))
 ii)

(my-assert
 (eval (quote li1))
 (ii 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (cdddr li1) (quote pp))
 pp)

(my-assert
 (eval (quote li1))
 (ii 22 33 . pp))

(my-assert
 (setf (caddr li1) (quote 333))
 333)

(my-assert
 (eval (quote li1))
 (ii 22 333 . pp))

(my-assert
 (setf (svref vec1 2) (quote kk))
 kk)

(my-assert
 (eval (quote vec1))
 #(0 oo kk 3))

(my-assert
 (setf (get (quote a) (quote b)) (quote uu))
 uu)

(my-assert
 (get (quote a) (quote b))
 uu)

(my-assert
 (setf (getf (cadr (setq xx (quote (aaa (i1 v1 i2 v2))))) (quote i2))

       (quote v222))
 v222)

(my-assert
 (eval (quote xx))
 (aaa (i1 v1 i2 v222)))

(my-assert
 (getf (cadr xx) (quote i2))
 v222)

(my-assert
 (getf (cadr xx) (quote i1))
 v1)

(my-assert
 (setf (documentation (quote beispiel) (quote typ1)) "doc 1")
 "doc 1")

(my-assert
 (setf (documentation (quote beispiel) (quote typ2)) "doc 2")
 "doc 2")

(my-assert
 (documentation (quote beispiel) (quote typ2))
 #+xcl (typ2 . "doc 2")
 #-xcl "doc 2")

(my-assert
 (setf (documentation (quote beispiel) (quote typ2)) "doc 3")
 "doc 3")

(my-assert
 (documentation (quote beispiel) (quote typ2))
 #+xcl (typ2 . "doc 3")
 #-xcl "doc 3")

(my-assert
 (symbol-plist 'beispiel)
 #+xcl (documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
 #+clisp (system::documentation-strings (typ2 "doc 3" typ1 "doc 1"))
 #+allegro (excl::%documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
 #+(or cmu ecls) nil
 #-(or xcl clisp allegro cmu ecls) unknown)

(my-assert
 (setf (symbol-value (quote xx)) (quote voelligneu))
 voelligneu)

(my-assert
 (eval (quote xx))
 voelligneu)

(my-assert
 (progn
   (setf (symbol-function (quote ff))
	 (coerce (quote (lambda (x) (print x) (quote hello))) (quote function)))
   nil)
 nil)

(my-assert
 (ff 5)
 hello)

(my-assert
 (defun xx nil 'a)
 xx)

(my-assert
 (progn (setf (symbol-function 'xx1) (symbol-function 'xx)) nil)
 nil)

(my-assert
 (xx1)
 a)

(my-assert
 (setq l '(a 1 c d))
 (a 1 c d))

(my-assert
 (setf (the integer (cadr l)) 100)
 100)

(my-assert
 l
 (a 100 c d))

(my-assert
 (progn (setf a (make-hash-table)) t)
 t)

(my-assert
 (setf (gethash 'color a) 'brown)
 brown)

(my-assert
 (gethash 'color a)
 brown)

(my-assert
 (defstruct schiff masse)
 schiff)

(my-assert
 (progn (setf s1 (make-schiff)) nil)
 nil)

(my-assert
 (setf (schiff-masse s1) 500)
 500)

(my-assert
 (schiff-masse s1)
 500)

(my-assert
 (defmacro setf-test (v) `(svref ,v 3))
 setf-test)

(my-assert
 (progn (setf (macro-function 'setf-test1) (macro-function 'setf-test)) nil)
 nil)

(my-assert
 (setf (setf-test vec1) 'oho)
 oho)

(my-assert
 (eval 'vec1)
 #(0 oo kk oho))

(my-assert
 (setf (setf-test1 vec1) 'hihi)
 hihi)

(my-assert
 (eval 'vec1)
 #(0 oo kk hihi))

;;  (setf (displace ?? (svref vec1 3)) "aha")
;;  aha

;;  (eval 'vec1)
;;  #(0 oo kk aha)

(my-assert
 (progn (setf a (make-array '(4 3))) nil)
 nil)

(my-assert
 (aref a 2 2)
 #+(or xcl cmu) 0
 #+(or clisp akcl allegro ecls) nil
 #-(or xcl clisp akcl allegro cmu ecls) unknown)

(my-assert
 (setf (apply #'aref a '(2 2)) 'xxxx)
 xxxx)

(my-assert
 (aref a 2 2)
 xxxx)

(my-assert
 (setf (aref '#(a b c) 1) (quote ii))
 ii)

(my-assert
 (setf b #*101010)
 #*101010)

(my-assert
 (bit b 2)
 1)

(my-assert
 (setf (bit b 2) 0)
 0)

(my-assert
 (bit b 2)
 0)

(my-assert
 (setf (sbit b 2) 1)
 1)

(my-assert
 (sbit b 2)
 1)

(my-assert
 (progn (setf a (make-array 5 :fill-pointer t)) t)
 t)

(my-assert
 (fill-pointer a)
 5)

(my-assert
 (setf (fill-pointer a) 3)
 3)

(my-assert
 (fill-pointer a)
 3)

(my-assert
 (let ((str (copy-seq "hose")))
   str)
 "hose")

(my-assert
 (let ((str (copy-seq "hose")))
   (setf (char str 0) #\d))
 #\d)

(my-assert
  (let ((str (copy-seq "hose")))
   (setf (char str 0) #\d)
   str)
 "dose")

(my-assert
 (let ((str (copy-seq "hose")))
   (setf (char str 0) #\d)
   (setf str "aaaxxxccc"))
 "aaaxxxccc")

(my-assert
 (let ((str (copy-seq "hose")))
   (setf (char str 0) #\d)
   (setf str (copy-seq "aaaxxxccc"))
   (setf (subseq str 3 6) "bbb"))
 "bbb")

(my-assert
 (let ((str (copy-seq "hose")))
   (setf (char str 0) #\d)
   (setf str (copy-seq "aaaxxxccc"))
   (setf (subseq str 3 6) "bbb")
   str)
 "aaabbbccc")

(my-assert
 (setq x (list 'a 'b 'c))
 (a b c))

(my-assert
 (shiftf (cadr x) 'z)
 b)

(my-assert
 x
 (a z c))

(my-assert
 (shiftf (cadr x) (cddr x) 'q)
 z)

(my-assert
 x
 (a (c) . q))

(my-assert
 (progn
   (defun ad (x) (values (car x) (cdr x)))
   (defsetf ad (x) (a b) `(setf (values (car ,x) (cdr ,x)) (values ,a ,b)))
   (setq x (cons 1 2) y 3 z 4 w 5 v 6 u 7))
 7)

(my-assert
 (rotatef (ad x) (values y z) (values w v u))
 nil)

(my-assert
 x
 (3 . 4))

(my-assert
 (list y z w v u)
 (5 6 1 2 nil))

(my-assert 
 (multiple-value-list
  (shiftf (ad x)
	  (values y z w)
	  (values v u)
	  (floor 89 10)))
 (3 4)
 "(ad x) ->  3 and  4)
 (y -> 5 z -> 6 w -> 1)
 (v -> 2 u-> nil)
 (floor 89 10) -> 8 and 9

so after shifting we expect:
x -> (5 . 6)
(y -> 2 z-> nil w -> nil)
(v -> 8 u -> 9)

and we return 3 and 4")

(my-assert
 x
 (5 . 6)
 "check the shiftf result")

(my-assert
 (list y z w v u)
 (2 nil nil 8 9)
 "check the shiftf result")

(my-assert
 (progn (defsetf subseq (sequence start &optional end) (new-sequence)
	  `(progn (replace ,sequence ,new-sequence
			   :start1 ,start :end1 ,end)
		  ,new-sequence)) t)
 t)

(my-assert
 (let (s)
   (setf s (copy-seq "asdfg")
	 (subseq s 1 3) "xy"))
 "xy")

(my-assert
  (let (s)
   (setf s (copy-seq "asdfg")
	 (subseq s 1 3) "xy")
   s)
 "axyfg")

