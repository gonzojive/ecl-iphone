;;; section 14 conses -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))
;;; cons

(my-assert
 (cons 1 2)
 (1 . 2))

(my-assert
 (cons 1 nil)
 (1))

(my-assert
 (cons nil 2)
 (NIL . 2))

(my-assert
 (cons nil nil)
 (NIL))

(my-assert
 (cons 1 (cons 2 (cons 3 (cons 4 nil))))
 (1 2 3 4))

(my-assert
 (cons 'a 'b)
 (A . B))

(my-assert
 (cons 'a (cons 'b (cons 'c '())))
 (A B C))

(my-assert
 (cons 'a (list 'b 'c 'd))
 (A B C D))


;;; consp

(my-assert
 (consp nil)
 nil)

(my-assert
 (consp (cons 1 2))
 t)

(my-assert
 (consp '())
 nil)

(my-assert
 (consp 'nil)
 nil)

;;; atom

(my-assert
 (atom 'sss)
 t)

(my-assert
 (atom (cons 1 2))
 nil)

(my-assert
 (atom nil)
 t)

(my-assert
 (atom '())
 t)

(my-assert
 (atom 3)
 t)

;;; rplaca

(my-assert
 (defparameter *some-list* (list* 'one 'two 'three 'four))
 *some-list*)

(my-assert
 *some-list*
 (ONE TWO THREE . FOUR))

(my-assert
 (rplaca *some-list* 'uno)
 (UNO TWO THREE . FOUR))

(my-assert
 *some-list*
 (UNO TWO THREE . FOUR))

(my-assert
 (rplacd (last *some-list*) (list 'IV))
 (THREE IV))

(my-assert
 *some-list*
 (UNO TWO THREE IV))

;;; copy-tree

(my-assert
 (let* ((object (list (cons 1 "one")
		      (cons 2 (list 'a 'b 'c))))
	(object-too object)
	(copy-as-list (copy-list object))
	(copy-as-alist (copy-alist object))
	(copy-as-tree (copy-tree object)))
   (list 
    (eq object object-too) ;; T
    (eq copy-as-tree object) ;; NIL
    (eql copy-as-tree object) ;; NIL
    (equal copy-as-tree object) ;; T
    )
   )
 (t nil nil t))

(my-assert
  (let* ((object (list (cons 1 "one")
		       (cons 2 (list 'a 'b 'c)))))
    
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1)))
  (ONE . 1))

(my-assert
   (let* ((object (list (cons 1 "one")
		       (cons 2 (list 'a 'b 'c)))))
    
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    object)
   ((ONE . 1) ("two" "a" B C)))

(my-assert
  (let* ((object (list (cons 1 "one")
		      (cons 2 (list 'a 'b 'c))))
	 (object-too object))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    object-too)
 ((ONE . 1) ("two" "a" B C)))

(my-assert
   (let* ((object (list (cons 1 "one")
		      (cons 2 (list 'a 'b 'c))))
	  (copy-as-list (copy-list object)))
     (setf (first (cdr (second object))) "a"
	   (car (second object)) "two"
	   (car object) (cons 'one  1))
     copy-as-list)
   ((1 . "one") ("two" "a" B C)))

(my-assert
    (let* ((object (list (cons 1 "one")
		      (cons 2 (list 'a 'b 'c))))
		(copy-as-alist (copy-alist object)))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    copy-as-alist)
 ((1 . "one") (2 "a" B C)))

(my-assert
 (let* ((object (list (cons 1 "one")
		      (cons 2 (list 'a 'b 'c))))
	(copy-as-tree (copy-tree object)))
    (setf (first (cdr (second object))) "a"
	  (car (second object)) "two"
	  (car object) (cons 'one  1))
    copy-as-tree)
 ((1 . "one") (2 A B C)) )

;;; sublis

(my-assert
 (sublis (list (cons 'x  100)
	       (cons 'z 'zprime))
	 (append (list 'plus 'x
		       (list 'minus 'g 'z 'x 'p)
		       4)
		 'x))
 (PLUS 100 (MINUS G ZPRIME 100 P) 4 . 100))

(my-assert
 (sublis (list (cons (list '+ 'x 'y)
		     (list '- 'x 'y))
	       (cons (list '- 'x 'y)
		     (list '+ 'x 'y)))
         (list '*
	       (list '/
		     (list '+ 'x 'y)
		     (list '+ 'x 'p))
	       (list '- 'x 'y))
         :test #'equal)
 (* (/ (- X Y) (+ X P)) (+ X Y)))

(my-assert
 (let ((tree1 (list 1
		    (list 1 2)
		    (list (list 1 2 3))
		    (list (list (list 1 2 3 4))))))
   tree1)
 (1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(my-assert
  (let ((tree1 (list 1
		    (list 1 2)
		    (list (list 1 2 3))
		    (list (list (list 1 2 3 4))))))
    (sublis (list (cons 3 "three"))
	    tree1))
 (1 (1 2) ((1 2 "three")) (((1 2 "three" 4)))))

(my-assert
 (let ((tree1 (list 1
		    (list 1 2)
		    (list (list 1 2 3))
		    (list (list (list 1 2 3 4))))))
   (sublis (list (cons t  "string"))
	   (sublis (list (cons 1 "") (cons 4 44))
		   tree1)
	   :key #'stringp))
 ("string" ("string" 2) (("string" 2 3)) ((("string" 2 3 44)))))

(my-assert
   (let ((tree1 (list 1
		    (list 1 2)
		    (list (list 1 2 3))
		    (list (list (list 1 2 3 4))))))
    (sublis (list (cons 3 "three"))
	    tree1)
    (sublis (list (cons t  "string"))
	    (sublis (list (cons 1 "") (cons 4 44))
		    tree1)
	    :key #'stringp)
    tree1)
 (1 (1 2) ((1 2 3)) (((1 2 3 4)))))

(my-assert
 (let ((tree2 (list "one" (list "one" "two")
		   (list (list "one" "Two" "three")))))
   tree2)
 ("one" ("one" "two") (("one" "Two" "three"))) )

(my-assert
 (let ((tree2 (list (string "one")
		    (list (string "one")
				(string "two"))
		   (list (list (string "one")
			       (string "Two")
			       (string "three")
			       )))))
   (sublis (list (cons (copy-seq "two")
		       2))
	   tree2))
 ("one" ("one" "two") (("one" "Two" "three"))))

(my-assert
  (let ((tree2 (list (string "one")
		     (list (string "one")
			   (string "two")
			   )
		   (list (list (string "one")
			       (string "Two")
			       (string "three")
			       )))))
   (sublis (list (cons (string "two")
		       2))
	   tree2)
   tree2)
 ("one" ("one" "two") (("one" "Two" "three"))) )

(my-assert
  (let ((tree2 (list "one" (list "one" "two")
		   (list (list "one" "Two" "three")))))
    (sublis (list (cons "two" 2)) tree2 :test 'equal))
 ("one" ("one" 2) (("one" "Two" "three"))) )

(my-assert
 (let ((tree1 (list 1
		    (list 1 2)
		    (list (list 1 2 3))
		    (list (list (list 1 2 3 4))))))
   (nsublis (list (cons t '(quote temp)))
	    tree1
	    :key #'(lambda (x) (or (atom x) (< (list-length x) 3)))))
 ((QUOTE TEMP) (QUOTE TEMP) QUOTE TEMP) )

;;; subst

(my-assert
 (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
   tree1)
 (1 (1 2) (1 2 3) (1 2 3 4)))

(my-assert
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst "two" 2 tree1))
 (1 (1 "two") (1 "two" 3) (1 "two" 3 4)))

(my-assert
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst "five" 5 tree1))
 (1 (1 2) (1 2 3) (1 2 3 4)))

(my-assert
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (eq tree1 (subst "five" 5 tree1)))
  #+(or sbcl cmu sbcl clisp) T
  #+ecls nil
  #-(or sbcl cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (subst 'tempest 'hurricane
	(list 'shakespeare 'wrote (list 'the 'hurricane)))
 (SHAKESPEARE WROTE (THE TEMPEST)))

(my-assert
 (subst 'foo 'nil (list 'shakespeare 'wrote (list 'twelfth 'night)))
 (SHAKESPEARE WROTE (TWELFTH NIGHT . FOO) . FOO))

(my-assert
 (subst (cons 'a 'cons)
	(cons 'old 'pair)
	(list (cons 'old 'spice)
	      (append (list (cons 'old 'shoes) 'old)
		      'pair)
	      (cons 'old 'pair))
	:test #'equal)
 ((OLD . SPICE) ((OLD . SHOES) A . CONS) (A . CONS)))

(my-assert
 (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
   (subst-if 5 #'listp tree1))
 5)

(subst-if-not (list 'x)
	      #'consp
	      (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4)))

(my-assert
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
    (subst-if-not (list 'x) #'consp tree1))
					;(1 (X))
 ((X) ((X) (X) X) ((X) (X) (X) X) ((X) (X) (X) (X) X) X))

(my-assert
 (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
   (subst-if-not (list 'x) #'consp tree1)
   tree1)
 (1 (1 2) (1 2 3) (1 2 3 4)))

(my-assert
 (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
   (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y)))))
 (1 (1 2) X X))

(my-assert
  (let  ((tree1 (list 1 (list 1 2) (list 1 2 3) (list 1 2 3 4))))
   (nsubst 'x 3 tree1 :key #'(lambda (y) (and (listp y) (third y))))
   tree1)
 (1 (1 2) X X))

;;; tree-equal

(my-assert
 (let ((tree1 (list 1 (list 1 2)))
       (tree2 (list 1 (list 1 2))))
   tree2)
 (1 (1 2)))

(my-assert
 (let ((tree1 (list 1 (list 1 2)))
       (tree2 (list 1 (list 1 2)))) 
   (tree-equal tree1 tree2))
 t)

(my-assert
 (let ((tree1 (list 1 (list 1 2)))
       (tree2 (list 1 (list 1 2)))) 
   (eql tree1 tree2))
 nil)

(my-assert 
 (let ((tree1 (list ''a (list ''b ''c)))
       (tree2 (list ''a (list ''b ''c))))
   tree2)
 ('a ('b 'c)) )

(my-assert
  (let ((tree1 (list ''a (list ''b ''c)))
       (tree2 (list ''a (list ''b ''c))))
    (tree-equal tree1 tree2 :test 'eq))
 t)

;;; copy-list

(my-assert
 (let ((lst (list 1 (list 2 3))))
   lst)
 (1 (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst))
    slst)
 (1 (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
    clst)
 (1 (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (eq slst lst))
 t)

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (eq clst lst))
 nil)

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (equal clst lst))
 t)

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one"))
 ("one" (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")
   slst)
 ("one" (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")  
   clst)
 (1 (2 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")  
   (setf (caadr lst) "two"))
 "two")

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")  
   (setf (caadr lst) "two")
   lst)
 ("one" ("two" 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")  
   (setf (caadr lst) "two")
   slst)
 ("one" ("two" 3)))

(my-assert
 (let* ((lst (list 1 (list 2 3)))
	(slst lst)
	(clst (copy-list lst)))
   (rplaca lst "one")  
   (setf (caadr lst) "two")
   clst)
 (1 ("two" 3)))

;;; list list*

(my-assert
 (list 1)
 (1))

(my-assert
 (list* 1)
 1)

(my-assert
 (let (( a 1))
   a)
 1)

(my-assert
 (let (( a 1))
   (list a 2))
 (1 2))

(my-assert
 (let (( a 1))
   (list 'a '2))
 (A 2))

(my-assert
 (let (( a 1))
   (list 'a 2))
 (A 2))

(my-assert
  (let (( a 1))
    (list* a 2))
 (1 . 2))

(my-assert
 (list)
 NIL)

(my-assert
 (let ((a (list 1 2)))
   a)
 (1 2))

(my-assert
 (let ((a (list 1 2)))
   (eq a (list* a)))
 t)

(my-assert
 (let ((a (list 1 2)))
   (list 3 4 'a (car (cons 'b 'c)) (+ 6 -2)))
 (3 4 A B 4))

(my-assert
 (let ((a (list 1 2)))
   (list* 'a 'b 'c 'd))
 (A B C . D))

(my-assert
 (cons 'a (cons 'b (cons 'c 'd)))
 (A B C . D))

(my-assert
 (list* 'a 'b 'c (list 'd 'e 'f))
 (A B C D E F))

;;; list-length

(my-assert
 (list-length (list 'a 'b 'c 'd))
 4)

(my-assert
 (list-length (list 'a (list 'b 'c) 'd))
 3)

(my-assert
 (list-length '())
 0)

(my-assert
 (list-length nil)
 0)

(my-assert
 (defun circular-list (&rest elements)
   (let ((cycle (copy-list elements)))
     (nconc cycle cycle)))
 CIRCULAR-LIST)

(my-assert
 (list-length (circular-list 'a 'b))
 NIL)

(my-assert
 (list-length (circular-list 'a))
 NIL)

(my-assert
 (list-length (circular-list))
 0)

;;; listp

(my-assert
 (listp nil)
 t)

(my-assert
 (listp (cons 1 2))
 t)

(my-assert
 (listp (make-array 6))
 nil)

(my-assert
 (listp t)
 nil)

;;; make-list

(my-assert
 (make-list 5)
 (NIL NIL NIL NIL NIL))

(my-assert
 (make-list 3 :initial-element 'rah)
 (RAH RAH RAH))

(my-assert
 (make-list 2 :initial-element (list 1 2 3))
 ((1 2 3) (1 2 3)))

(my-assert
 (make-list 0)
 NIL)					;i.e.,  ())

(my-assert
 (make-list 0 :initial-element 'new-element)
 NIL )

;;; push

(my-assert
 (let ((llst (list nil)))
   llst)
 (NIL))

(my-assert
 (let ((llst (list nil)))
   (push 1 (car llst)))
 (1))

(my-assert
 (let ((llst (list nil)))
   (push 1 (car llst))
   llst)
 ((1)))

(my-assert
 (let ((llst (list nil)))
   (push 1 (car llst))
   (push 1 (car llst)))
 (1 1))

(my-assert
 (let ((llst (list nil)))
   (push 1 (car llst))
   (push 1 (car llst))
   llst)
 ((1 1)))

(my-assert
 (let ((x (list 'a
		 (list 'b 'c)
		 'd)))
   x)
 (A (B C) D))

(my-assert
 (let ((x (list 'a
		(list 'b 'c)
		'd)))
   (push 5 (cadr x)))
 (5 B C)  )

(my-assert
 (let ((x (list 'a
		(list 'b 'c)
		'd)))
   (push 5 (cadr x))
   x)
 (A (5 B C) D))

;;; pop

(my-assert
 (let ((stack (list 'a 'b 'c)))
   stack)
 (A B C))

(my-assert
  (let ((stack (list 'a 'b 'c)))
    (pop stack))
 A)

(my-assert
  (let ((stack (list 'a 'b 'c)))
    (pop stack)
    stack)
 (B C))

(my-assert
 (let ((llst (list (list 1 2 3 4))))
   llst)
 ((1 2 3 4)))

(my-assert
  (let ((llst (list (list 1 2 3 4))))
    (pop (car llst)))
 1)

(my-assert
  (let ((llst (list (list 1 2 3 4))))
    (pop (car llst))
    llst)
 ((2 3 4)))

;;; nth

(my-assert
 (nth 0 (list 'foo 'bar 'baz))
 FOO)

(my-assert
 (nth 1 (list 'foo 'bar 'baz))
 BAR)

(my-assert
 (nth 3 (list 'foo 'bar 'baz))
 NIL)

(my-assert
 (let ((0-to-3 (list 0 1 2 3)))
   0-to-3)
 (0 1 2 3))

(my-assert
  (let ((0-to-3 (list 0 1 2 3)))
    (setf (nth 2 0-to-3) "two"))
 "two")

(my-assert
  (let ((0-to-3 (list 0 1 2 3)))
    (setf (nth 2 0-to-3) "two")
    0-to-3)
 (0 1 "two" 3))

;;; endp

(my-assert
 (endp nil)
 t)

(my-assert
 (endp (list 1 2))
 nil)

(my-assert
 (endp (cddr (list 1 2)))
 t)

;;; null

(my-assert
 (null '())
 T)

(my-assert
 (null nil)
 T)

(my-assert
 (null t)
 NIL)

(my-assert
 (null 1)
 NIL)

;;; nconc

(my-assert
 (nconc)
 NIL)

(my-assert
 (setq x (list 'a 'b 'c))
 (A B C))

(my-assert
 (setq y (list 'd 'e 'f))
 (D E F))

(my-assert
 (nconc x y)
 (A B C D E F))

(my-assert
 x
 (A B C D E F))

(my-assert
 (setq foo (list 'a 'b 'c 'd 'e)
       bar (list 'f 'g 'h 'i 'j)
       baz (list 'k 'l 'm))
 (K L M))

(my-assert
 (setq foo (nconc foo bar baz))
 (A B C D E F G H I J K L M))

(my-assert
 foo
 (A B C D E F G H I J K L M))

(my-assert
 bar
 (F G H I J K L M))

(my-assert
 baz
 (K L M))

(my-assert
 (setq foo (list 'a 'b 'c 'd 'e)
       bar (list 'f 'g 'h 'i 'j)
       baz (list 'k 'l 'm))
 (K L M))

(my-assert
 (setq foo (nconc nil foo bar nil baz))
 (A B C D E F G H I J K L M) )

(my-assert
 foo
 (A B C D E F G H I J K L M))

(my-assert
 bar
 (F G H I J K L M))

(my-assert
 baz
 (K L M))

;;; append

(my-assert
 (append (list 'a 'b 'c)
	 (list 'd 'e 'f)
	 '()
	 (list 'g))
 (A B C D E F G))

(my-assert
 (append (list 'a 'b 'c)
	 'd)
 (A B C . D))

(my-assert
 (setq lst (list 'a 'b 'c))
 (A B C))

(my-assert
 (append lst (list 'd))
 (A B C D))

(my-assert
 lst
 (A B C))

(my-assert
 (append)
 NIL)

(my-assert
 (append 'a)
 A)

;;; revappend

(my-assert
 (let ((list-1 (list 1 2 3))
       (list-2 (list 'a 'b 'c)))
   (list (revappend list-1 list-2)
	 (equal list-1 (list 1 2 3))
	 (equal list-2 (list 'a 'b 'c))))
 ((3 2 1 A B C)  T  T))


(my-assert
 (revappend (list 1 2 3) '())
 (3 2 1))

(my-assert
 (revappend (list 1 2 3)
	    (cons 'a 'b))
 (3 2 1 A . B))

(my-assert
 (revappend '()
	    (list 'a 'b 'c))
 (A B C))

(my-assert
 (revappend (list 1 2 3)
	    'a)
 (3 2 1 . A))

(my-assert
 (revappend '() 'a)
 A )					;degenerate case)

(my-assert
 (let ((list-1 (copy-list (list 1 2 3)))
       (list-2 (list 'a 'b 'c)))
   (list (nreconc list-1 list-2)
	 (equal list-1 (list 1 2 3))
	 (equal list-2 (list 'a 'b 'c))))
 ((3 2 1 A B C) NIL T))


;;; butlast

(my-assert
 (setq lst (list 1 2 3 4 5 6 7 8 9))
 (1 2 3 4 5 6 7 8 9))

(my-assert
 (butlast lst)
 (1 2 3 4 5 6 7 8))

(my-assert
 (butlast lst 5)
 (1 2 3 4))

(my-assert
 (butlast lst (+ 5 5))
 NIL)

(my-assert
 lst
 (1 2 3 4 5 6 7 8 9))

(my-assert
 (nbutlast lst 3)
 (1 2 3 4 5 6))

(my-assert
 lst
 (1 2 3 4 5 6))

(my-assert
 (nbutlast lst 99)
 NIL)

(my-assert
 lst
 (1 2 3 4 5 6))

(my-assert
 (butlast (list 'a 'b 'c 'd))
 (A B C))

(my-assert
 (butlast (list (list 'a 'b)
		(list 'c 'd)))
 ((A B)))

(my-assert
 (butlast (list 'a))
 NIL)

(my-assert
 (butlast nil)
 NIL)

(my-assert
 (setq foo (list 'a 'b 'c 'd))
 (A B C D))

(my-assert
 (nbutlast foo)
 (A B C))

(my-assert
 foo
 (A B C))

(my-assert
 (nbutlast (list 'a))
 NIL)

(my-assert
 (nbutlast '())
 NIL)

;;; last

(my-assert
 (last nil)
 NIL)

(my-assert
 (last (list 1 2 3))
 (3))

(my-assert
 (last (append (list 1 2)
	       3))
 (2 . 3))

(my-assert
 (setq x (list 'a 'b 'c 'd))
 (A B C D))

(my-assert
 (last x)
 (D))

(my-assert
 (progn
   (rplacd (last x) (list 'e 'f))
   t)
 t)

(my-assert
 x
 (A B C D E F))

(my-assert
 (last x)
 (F))

(my-assert
 (last (list 'a 'b 'c))
 (C))

(my-assert
 (last (list 'a 'b 'c) 0)
 ())

(my-assert
 (last (list 'a 'b 'c) 1)
 (C))

(my-assert
 (last (list 'a 'b 'c) 2)
 (B C))

(my-assert
 (last (list 'a 'b 'c) 3)
 (A B C))

(my-assert
 (last (list 'a 'b 'c) 4)
 (A B C))

(my-assert
 (last (cons 'a 'b) 0)
 B)

(my-assert
 (last (cons 'a 'b) 1)
 (A . B))

(my-assert
 (last (cons 'a 'b) 2)
 (A . B))

;;; nthcdr

(my-assert
 (nthcdr 0 '())
 NIL)

(my-assert
 (nthcdr 3 '())
 NIL)

(my-assert
 (nthcdr 0 (list 'a 'b 'c))
 (A B C))

(my-assert
 (nthcdr 2 (list 'a 'b 'c))
 (C))

(my-assert
 (nthcdr 4 (list 'a 'b 'c))
 ())

(my-assert
 (nthcdr 1 (cons 0 1))
 1)

(my-assert
 (locally (declare (optimize (safety 3)))
	  (nthcdr 3 (cons 0 1)))
 TYPE-ERROR)

;;; rest

(my-assert
 (rest (list 1 2))
 (2))

(my-assert
 (rest (cons 1 2))
 2)

(my-assert
 (rest (list 1))
 NIL)

(my-assert
 (setq *cons* (cons 1 2))
 (1 . 2))

(my-assert
 (setf (rest *cons*) "two")
 "two")

(my-assert
 *cons*
 (1 . "two"))

;;; member

(my-assert
 (member 2 (list 1 2 3))
 (2 3))

(my-assert
 (member 2 (list (cons 1 2)
		 (cons 3 4))
	 :test-not #'=
	 :key #'cdr)
 ((3 . 4)))

(my-assert
 (member 'e (list 'a 'b 'c 'd))
 NIL)

(my-assert
 (member-if #'listp (list 'a 'b nil 'c 'd))
 (NIL C D))

(my-assert
 (member-if #'numberp (list 'a #\Space 5/3 'foo))
 (5/3 FOO))

(my-assert
 (member-if-not #'zerop
		(append (list 3 6 9 11)
			12)
		:key #'(lambda (x) (mod x 3)))
 (11 . 12))

;;; mapc and co

(my-assert
 (mapcar #'car (list (list 1 'a)
		     (list 2 'b)
		     (list 3 'c)))
 (1 2 3) )

(my-assert
 (mapcar #'abs (list 3 -4 2 -5 -6))
 (3 4 2 5 6))

(my-assert
 (mapcar #'cons (list 'a 'b 'c) (list 1 2 3))
 ((A . 1) (B . 2) (C . 3)))

(my-assert
 (maplist #'append (list 1 2 3 4) (list 1 2) (list 1 2 3))
 ((1 2 3 4 1 2 1 2 3) (2 3 4 2 2 3)))

(my-assert
 (maplist #'(lambda (x) (cons 'foo x)) (list 'a 'b 'c 'd))
 ((FOO A B C D) (FOO B C D) (FOO C D) (FOO D)))

(my-assert
 (maplist #'(lambda (x) (if (member (car x) (cdr x)) 0 1))
	  (list 'a 'b 'a 'c 'd 'b 'c))
 (0 0 1 0 1 1 1))

(my-assert
 (setq dummy nil)
 NIL )

(my-assert
 (mapc #'(lambda (&rest x) (setq dummy (append dummy x)))
       (list 1 2 3 4)
       (list 'a 'b 'c 'd 'e)
       (list 'x 'y 'z))
 (1 2 3 4) )

(my-assert
 dummy
 (1 A X 2 B Y 3 C Z)                   )

(my-assert
 (setq dummy nil)
 NIL )

(my-assert
 (mapl #'(lambda (x) (push x dummy))
       (list 1 2 3 4))
 (1 2 3 4) )

(my-assert
 dummy
 ((4) (3 4) (2 3 4) (1 2 3 4)) )

(my-assert
 (mapcan #'(lambda (x y) (if (null x) nil (list x y)))
	 (list nil nil nil 'd 'e)
	 (list 1 2 3 4 5 6))
 (D 4 E 5) )

(my-assert
 (mapcan #'(lambda (x) (and (numberp x) (list x)))
	 (list 'a 1 'b 'c 3 4 'd 5))
 (1 3 4 5))

(my-assert
 (mapcon #'list (list 1 2 3 4))
 ((1 2 3 4) (2 3 4) (3 4) (4)) )


;;; acons

(my-assert
 (setq alist '())
 NIL)

(my-assert
 (acons 1 "one" alist)
 ((1 . "one")))

(my-assert
 alist
 NIL)

(my-assert
 (setq alist (acons 1 "one" (acons 2 "two" alist)))
 ((1 . "one") (2 . "two")))

(my-assert
 (assoc 1 alist)
 (1 . "one"))

(my-assert
 (setq alist (acons 1 "uno" alist))
 ((1 . "uno") (1 . "one") (2 . "two")))

(my-assert
 (assoc 1 alist)
 (1 . "uno"))

;;; assoc

(my-assert
 (setq values
       (list (cons 'x 100)
	     (cons 'y 200)
	     (cons 'z 50)))
 ((X . 100) (Y . 200) (Z . 50)))

(my-assert
 (assoc 'y values)
 (Y . 200))

(my-assert
 (rplacd (assoc 'y values) 201)
 (Y . 201))

(my-assert
 (assoc 'y values)
 (Y . 201))

(my-assert
 (setq alist
       (list (cons 1 "one")
	     (cons 2 "two")
	     (cons 3 "three")))
 ((1 . "one") (2 . "two") (3 . "three")))

(my-assert
 (assoc 2 alist)
 (2 . "two"))

(my-assert
 (assoc-if #'evenp alist)
 (2 . "two"))

(my-assert
 (assoc-if-not #'(lambda(x) (< x 3)) alist)
 (3 . "three"))

(my-assert
 (setq alist (list (cons "one" 1)
		   (cons "two" 2)))
 (("one" . 1) ("two" . 2)))

(my-assert
 (assoc "one" alist)
 NIL)

(my-assert
 (assoc "one" alist :test #'equalp)
 ("one" . 1))

(my-assert
 (assoc "two" alist :key #'(lambda(x) (char x 2)))
 NIL )

(my-assert
 (assoc #\o alist :key #'(lambda(x) (char x 2)))
 ("two" . 2))

(my-assert
 (assoc 'r (list (cons 'a 'b)
		 (cons 'c 'd)
		 (cons 'r 'x)
		 (cons 's 'y)
		 (cons 'r 'z)))
 (R . X))

(my-assert
 (assoc 'goo (list (cons 'foo 'bar)
		   (cons 'zoo 'goo)))
 NIL)

(my-assert
 (assoc '2 (list (list 1 'a 'b 'c)
		 (list 2 'b 'c 'd)
		 (list -7 'x 'y 'z)))
 (2 B C D))

(my-assert
 (setq alist (list (cons "one" 1)
		   (cons "2" 2)
		   (cons "three" 3)))
 (("one" . 1) ("2" . 2) ("three" . 3)))

(my-assert
 (assoc-if-not #'alpha-char-p alist
	       :key #'(lambda (x) (char x 0)))
 ("2" . 2))

;;; copy-alist

(my-assert
 (progn
   (defparameter *alist* (acons 1 "one" (acons 2 "two" '())))
   t)
 t)

(my-assert
 *alist*
 ((1 . "one") (2 . "two")))

(my-assert
 (progn
   (defparameter *list-copy* (copy-list *alist*))
   t)
 t)

(my-assert
 *list-copy*
 ((1 . "one") (2 . "two")))

(my-assert
 (progn
   (defparameter *alist-copy* (copy-alist *alist*))
   t)
 t)

(my-assert
 *alist-copy*
 ((1 . "one") (2 . "two")))

(my-assert
 (setf (cdr (assoc 2 *alist-copy*)) "deux")
 "deux")

(my-assert
 *alist-copy*
 ((1 . "one") (2 . "deux")))

(my-assert
 *alist*
 ((1 . "one") (2 . "two")))

(my-assert
 (setf (cdr (assoc 1 *list-copy*)) "uno")
 "uno")

(my-assert
 *list-copy*
 ((1 . "uno") (2 . "two")))

(my-assert
 *alist*
 ((1 . "uno") (2 . "two")))

;;; pairlis

(my-assert
 (setq keys (list 1 2 3)
       data (list "one" "two" "three")
       alist (list (cons 4 "four")))
 ((4 . "four")))

(my-assert
 (pairlis keys data)
 ((3 . "three") (2 . "two") (1 . "one")))

(my-assert
 (pairlis keys data alist)
 ((3 . "three") (2 . "two") (1 . "one") (4 . "four")))

(my-assert
 alist
 ((4 . "four")))

;;; rassoc

(my-assert
 (setq alist (list (cons 1 "one")
		   (cons 2 "two")
		   (cons 3 3)))
 ((1 . "one") (2 . "two") (3 . 3)))

(my-assert
 (rassoc 3 alist)
 (3 . 3))

(my-assert
 (rassoc "two" alist)
 NIL)

(my-assert
 (rassoc "two" alist :test 'equal)
 (2 . "two"))

(my-assert
 (rassoc 1 alist :key #'(lambda (x) (if (numberp x) (/ x 3))))
 (3 . 3))

(my-assert
 (rassoc 'a
	 (list (cons 'a 'b)
	       (cons 'b 'c)
	       (cons 'c 'a)
	       (cons 'z 'a)))
 (C . A))

(my-assert
 (rassoc-if #'stringp alist)
 (1 . "one"))

(my-assert
 (rassoc-if-not #'vectorp alist)
 (3 . 3))

;;; get-properties

(my-assert
 (setq x '())
 NIL)

(my-assert
 (setq *indicator-list* (list 'prop1 'prop2))
 (PROP1 PROP2))

(my-assert
 (getf x 'prop1)
 NIL)

(my-assert
 (setf (getf x 'prop1) 'val1)
 VAL1)

(my-assert
 (eq (getf x 'prop1) 'val1)
 t)

(my-assert
 (multiple-value-bind (a b c)
     (get-properties x *indicator-list*)
   (list a b c))
 (PROP1  VAL1 (PROP1 VAL1)))

(my-assert
 x
 (PROP1 VAL1))

;;; getf

(my-assert
 (setq x '())
 NIL)

(my-assert
 (getf x 'prop1)
 NIL)

(my-assert
 (getf x 'prop1 7)
 7)

(my-assert
 (getf x 'prop1)
 NIL)

(my-assert
 (setf (getf x 'prop1) 'val1)
 VAL1)

(my-assert
 (eq (getf x 'prop1) 'val1)
 t)

(my-assert
 (getf x 'prop1)
 VAL1)

(my-assert
 (getf x 'prop1 7)
 VAL1)

(my-assert
 x
 (PROP1 VAL1))

;;; remf

(my-assert
 (setq x (cons () ()))
 (NIL))

(my-assert
 (setf (getf (car x) 'prop1) 'val1)
 VAL1)

(my-assert
 (remf (car x) 'prop1)
 t)

(my-assert
 (remf (car x) 'prop1)
 nil)

;;; intersection

(my-assert
 (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" (string #\B) "C" "d"))
       (list2 (list 1 4 5 'b 'c 'd "a" (string #\B) "c" "D")))
   (intersection list1 list2))
 #-clisp
 (C B 4 1 1)
 #+clisp
 (1 1 4 B C))

(my-assert
 (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" (string #\B) "C" "d"))
       (list2 (list 1 4 5 'b 'c 'd "a" (string #\B) "c" "D")))
   (intersection list1 list2 :test 'equal))
 #-clisp
 ("B" C B 4 1 1)
 #+clisp
 (1 1 4 B C "B"))

(my-assert
 (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
       (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
   (intersection list1 list2 :test #'equalp))
  #-clisp
  ("d" "C" "B" "A" C B 4 1 1)
  #+clisp
  (1 1 4 B C "A" "B" "C" "d"))

(my-assert
 (let ((list1 (list 1 1 2 3 4 'a 'b 'c "A" "B" "C" "d"))
       (list2 (list 1 4 5 'b 'c 'd "a" "B" "c" "D")))
   (nintersection list1 list2))
 #-clisp
 (C B 4 1 1)
 #+clisp
 (1 1 4 B C))
					;(1 1 4 B C))

(my-assert
  (let ((list1 (copy-list (list (cons 1 2)
				 (cons 2 3)
				 (cons 3 4)
			      (cons 4 5))))
	(list2 (copy-list (list (cons 1 3)
			      (cons 2 4)
			      (cons 3 6)
			      (cons 4 8)))))
    (nintersection list1 list2 :key #'cdr))
 #+(or sbcl cmu) ((3 . 4) (2 . 3))
 #-(or sbcl cmu) ((2 . 3) (3 . 4)) )

;;; adjoin

(my-assert
 (setq slist '())
 NIL )

(my-assert
 (adjoin 'a slist)
 (A) )

(my-assert
 slist
 NIL )

(my-assert
 (setq slist (adjoin (list 'test-item
			   '1)
		     slist))
 ((TEST-ITEM 1)) )

(my-assert
 (adjoin (list 'test-item 1)
	 slist)
 ((TEST-ITEM 1) (TEST-ITEM 1)) )

(my-assert
 (adjoin (list 'test-item 1)
	 slist
	 :test 'equal)
 ((TEST-ITEM 1)) )

(my-assert
 (adjoin (list 'new-test-item 1)
	 slist
	 :key #'cadr)
 ((TEST-ITEM 1)) )

(my-assert
 (adjoin (list 'new-test-item 1)
	 slist)
 ((NEW-TEST-ITEM 1) (TEST-ITEM 1)) )

;;; pushnew

(my-assert
 (setq x (list 'a (list 'b 'c) 'd))
 (A (B C) D))

(my-assert
 (pushnew 5 (cadr x))
 (5 B C)   )

(my-assert
 x
 (A (5 B C) D))

(my-assert
 (pushnew 'b (cadr x))
 (5 B C)  )

(my-assert
 x
 (A (5 B C) D))

(my-assert
 (setq lst (list (list 1)
		 (list 1 2)
		 (list 1 2 3)))
 ((1) (1 2) (1 2 3)))

(my-assert
 (pushnew (list 2) lst)
 ((2) (1) (1 2) (1 2 3)))

(my-assert
 (pushnew (list 1) lst)
 ((1) (2) (1) (1 2) (1 2 3)))

(my-assert
 (pushnew (list 1) lst :test 'equal)
 ((1) (2) (1) (1 2) (1 2 3)))

(my-assert
 (pushnew (list 1) lst :key #'car)
 ((1) (2) (1) (1 2) (1 2 3)) )

;;; set-difference

(my-assert
 (let ((lst1 (mapcar #'string (list #\A #\b #\C #\d)))
       (lst2 (mapcar #'string (list #\a #\B #\C #\d))))
      (set-difference lst1 lst2))
 #-clisp
 ("d" "C" "b" "A")
 #+clisp
 ("A" "b" "C" "d"))

(my-assert
 (let ((lst1 (list "A" "b" "C" "d"))
       (lst2 (list "a" "B" "C" "d")))
    (set-difference lst1 lst2 :test 'equal))
 #-clisp
 ("b" "A")
 #+clisp
 ("A" "b"))

(my-assert
  (let ((lst1 (list "A" "b" "C" "d"))
       (lst2 (list "a" "B" "C" "d")))
    (set-difference lst1 lst2 :test #'equalp))
 NIL )

(my-assert
  (let ((lst1 (list "A" "b" "C" "d"))
       (lst2 (list "a" "B" "C" "d")))
    (nset-difference lst1 lst2 :test #'string=))
 #+(or sbcl cmu)
 ("b" "A")
 #-(or sbcl cmu)
 ("A" "b"))

(my-assert
 (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-difference lst1 lst2 :test #'string= :key #'cdr))
 #+(or sbcl cmu)
 (("e" . "f") ("c" . "d"))
 #-(or sbcl cmu)
 (("c" . "d") ("e" . "f")))

(my-assert
  (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-difference lst1 lst2 :test #'string= :key #'cdr)
   lst1)
 #+(or sbcl cmu) (("a" . "b") ("c" . "d"))
 #-(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f")) )


(my-assert
  (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-difference lst1 lst2 :test #'string= :key #'cdr)
   lst2)
 (("c" . "a") ("e" . "b") ("d" . "a")) )

;; Remove all flavor names that contain "c" or "w".
(my-assert
 (set-difference (list
		  "strawberry" "chocolate" "banana"
		  "lemon" "pistachio" "rhubarb")
		 '(#\c #\w)
		 :test #'(lambda (s c) (find c s)))
 #+(or sbcl cmu) ("rhubarb" "lemon" "banana")
 #+clisp ("banana" "lemon" "rhubarb")
 #-(or sbcl cmu sbcl clisp) ("banana" "rhubarb" "lemon"))
;;One possible ordering.)

;;; set-exclusive-or

(my-assert
 (let ((lst1 (list 1 (string #\a) (string #\b)))
       (lst2 (list 1 (string #\A) (string #\b))))
   (set-exclusive-or lst1 lst2))
  #-clisp
  ("b" "A" "b" "a")
  #+clisp
  ("a" "b" "A" "b"))

(my-assert
 (let ((lst1 (list 1 (string #\a) (string #\b)))
       (lst2 (list 1 (string #\A) (string #\b))))
    (set-exclusive-or lst1 lst2 :test #'equal))
 ("A" "a"))

(my-assert
 (let ((lst1 (list 1 (string #\a) (string #\b)))
       (lst2 (list 1 (string #\A) (string #\b))))
    (set-exclusive-or lst1 lst2 :test 'equalp))
 NIL )

(my-assert
  (let ((lst1 (list 1 (string #\a) (string #\b)))
	(lst2 (list 1 (string #\A) (string #\b))))
    (nset-exclusive-or lst1 lst2))
 ("a" "b" "A" "b") )


(my-assert
 (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr))
 (("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(my-assert
  (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr)
   lst1)
 #-(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f"))
 #+(or sbcl cmu) (("a" . "b") ("c" . "d") ("e" . "f") ("c" . "a") ("d" . "a")))

(my-assert
  (let ((lst1 (list (cons "a" "b")
		   (cons "c" "d")
		   (cons "e" "f")))
       (lst2 (list (cons "c" "a")
		   (cons "e" "b")
		   (cons "d" "a"))))
   (nset-exclusive-or lst1 lst2 :test #'string= :key #'cdr)
   lst2)
 (("c" . "a") ("d" . "a")))

;;; subsetp

(my-assert
 (setq cosmos (list 1 "a" (list 1 2)))
 (1 "a" (1 2)))

(my-assert
 (subsetp (list 1) cosmos)
 t)

(my-assert
 (subsetp (list (list 1 2)) cosmos)
 nil)

(my-assert
 (subsetp (list (list 1 2)) cosmos :test 'equal)
 t)

(my-assert
 (subsetp (list 1 "A") cosmos :test #'equalp)
 t)

(my-assert
 (subsetp (list (list 1) (list 2))
	  (list (list 1) (list 2)))
 nil)

(my-assert
 (subsetp (list (list 1) (list 2))
	  (list (list 1) (list 2)) :key #'car)
 t)

;;; union

(my-assert
 (union (list 'a 'b 'c) (list 'f 'a 'd))
 #+(or sbcl cmu) (C B F A D)
 #+(or clisp ecls) (B C F A D)
 #-(or sbcl cmu sbcl clisp ecls) fill-this-in)

;; (A B C F D) OR  (B C F A D) OR  (D F A B C)

(my-assert
 (union (list (list 'x 5)
	      (list 'y 6))
	(list (list 'z 2)
	      (list 'x 4))
	:key #'car)
 #+(or sbcl cmu sbcl clisp ecls) ((Y 6) (Z 2) (X 4))
 #-(or sbcl cmu sbcl clisp ecls) fill-this-in)
;;  ((X 5) (Y 6) (Z 2)) OR  ((X 4) (Y 6) (Z 2))

(my-assert
 (let ((lst1 (list 1 2 (list 1 2) "a" "b"))
       (lst2 (list 2 3 (list 2 3) "B" "C")))
   (nunion lst1 lst2))
 #+(or sbcl cmu)
 ("b" "a" (1 2) 1 2 3 (2 3) "B" "C")
 #+(or clisp ecls)
 (1 (1 2) "a" "b" 2 3 (2 3) "B" "C")
 #-(or sbcl cmu sbcl clisp ecls)
 fill-this-in)

;;  (1 (1 2) "a" "b" 2 3 (2 3) "B" "C")  OR  (1 2 (1 2) "a" "b" "C" "B" (2 3) 3)











