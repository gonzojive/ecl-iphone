;;; 3.1.2.1.1.4 -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(if (boundp 'x2q) (makunbound 'x2q) 'ok)

(my-assert
 (let ((x2q 1))				;Binds a special variable X
   (declare (special x2q))
   (let ((x2q 2))			;Binds a lexical variable X
     (+ x2q				;Reads a lexical variable X
	(locally (declare (special x2q))
                 x2q))))		;Reads a special variable X
 3)

(if (boundp 'x3q) (makunbound 'x3q) 'ok)

(my-assert
 (progn
   (defun two-funs (x3q)
     (list (function (lambda () x3q))
	   (function (lambda (y) (setq x3q y)))))
   (setq funs (two-funs 6))
   T)
 T)

(my-assert
 (funcall (car funs))
 6)

(my-assert
 (funcall (cadr funs) 43)
 43)

(my-assert
 (funcall (car funs))
 43)

;;; 3.1.5
(my-assert
 (progn
   (defun contorted-example (f g x)
     (if (= x 0)
	 (funcall f)
	 (block here
	   (+ 5 (contorted-example g
				   #'(lambda () (return-from here 4))
				   (- x 1))))))
   t)
 T)

(my-assert
 (contorted-example nil nil 2)
 4)


(my-assert
 (progn
   (defun contorted-example (f g x)
     (if (= x 0)
	 (funcall g)
	 (block here
	   (+ 5 (contorted-example g
				   #'(lambda () (return-from here 4))
				   (- x 1))))))
   t)
 T)

(my-assert
 (contorted-example nil nil 2)
 9)

;;; 3.1.6

(my-assert
 (progn
   (defun invalid-example ()
     (let ((y (block here #'(lambda (z) (return-from here z)))))
       (if (numberp y) y (funcall y 5))))
   T)
 T)

(my-assert
 (invalid-example)
 CONTROL-ERROR)

(my-assert
 (progn
   (defun fun1 (x)
     (catch 'trap (+ 3 (fun2 x))))
   (defun fun2 (y)
     (catch 'trap (* 5 (fun3 y))))
   (defun fun3 (z)
     (throw 'trap z))
   T)
 T)

(my-assert
 (fun1 7)
 10)

;;; 3.3.4.1

(unintern 'x)

(my-assert
 (let ((x 1))
   (declare (special x))
   (let ((x 2))
     (let ((old-x x)
           (x 3))
       (declare (special x))
       (list old-x x))))
 (2 3)
 "The first declare is only valid in it's
block. The (let ((x 2)) is a new block,
where x is not special anymore.")

(if (boundp 'x) (makunbound 'x) 'ok)

(my-assert
 (let ((x4q  1))			;[1]
   (declare (special x4q))		;[2]
   (let ((x4q 2))			;[3]
     (dotimes (i x4q x4q)		;[4]
       (declare (special x4q)))))	;[5]
 1)


(if (boundp 'x) (makunbound 'x) 'ok)

;;; 3.4.1.4.1.1


(my-assert
 ((lambda (&key x) x) :x 1 :y 2 :allow-other-keys t)
 1)

(my-assert
 ((lambda (&key x &allow-other-keys) x) :x 1 :y 2)
 1)

(my-assert
 ((lambda (&key) t) :allow-other-keys nil)
 T)

(my-assert
 ((lambda (&key x) x)
  :x 1 :y 2 :allow-other-keys t :allow-other-keys nil)
 1)

(my-assert
 ((lambda (&key x) x)                   ;This call is not valid
  :x 1 :y 2 :allow-other-keys nil :allow-other-keys t)
 PROGRAM-ERROR
 "See 3.5.1.4:
If this situation occurs in a safe call, an error of type
program-error must be signaled; and in an unsafe call the
situation has undefined consequences. ");; from 3.5.1.4

;;; 3.4.1.6


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
 (2 NIL 3 NIL NIL))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6)
 (6 T 3 NIL NIL))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3)
 (6 T 3 T NIL))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x)) 6 3 8)
 (6 T 3 T (8)))

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x) (list a b c d x))
  6 3 8 9 10 11)
 (6 t 3 t (8 9 10 11)))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2)
 (1 2 NIL NIL))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :c 6)
 (1 2 6 NIL))

(my-assert
 ((lambda (a b &key c d) (list a b c d)) 1 2 :d 8)
 (1 2 NIL 8))

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
 (:a :b :d NIL))

(my-assert
 ((lambda (a b &key ((:sea c)) d) (list a b c d)) 1 2 :sea 6)
 (1 2 6 NIL))

(my-assert
 ((lambda (a b &key ((c c)) d) (list a b c d)) 1 2 'c 6)
 (1 2 6 NIL)
 "3.4.1.4: ...
If the notation ((keyword-name var) init-form) is used,
then the keyword name used to match arguments to
parameters is keyword-name, which may
be a symbol in any package. ...
")

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) 1)
 (1 3 NIL 1 ()) )

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) 1 2)
 (1 2 NIL 1 ()))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) :c 7)
 (:c 7 NIL :c ()))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) 1 6 :c 7)
 (1 6 7 1 (:c 7)))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) 1 6 :d 8)
 (1 6 NIL 8 (:d 8)))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x)) 1 6 :d 8 :c 9 :d 10)
 (1 6 9 8 (:d 8 :c 9 :d 10)))

;;;; eval function
					;(let ((form2p5 nil)
					;      (a2p5 nil))

					;  (my-assert
					;   (setq form2p5 '(1+ a2p5) a2p5 999)
					;   999)

					;  (my-assert
					;   (eval form2p5)
					;   1000)

					;  (my-assert
					;   (eval 'form2p5)
					;   (1+ A2p5))

					;  (my-assert
					;   (let ((a2p5 '(this would break if eval used local value)))
					;     (eval form2p5))
					;   1000))

;;; quote
(my-assert
 (let ((a 1)) 
   a)
 1)

(my-assert
 (let ((a 1)) 
 (quote (setq a 3)))
 (SETQ A 3))

(my-assert
 (let ((a 1)) 
 (quote (setq a 3)))
 a)
 1)

(my-assert
 (let ((a 1)) 
 (quote (setq a 3)))
 'a)
 A)

(my-assert
 (let ((a 1)) 
 (quote (setq a 3)))
 ''a)
 (QUOTE A) )

(my-assert
 (let ((a 1)) 
 (quote (setq a 3)))
 '''a)
 (QUOTE (QUOTE A)))

(my-assert
 (let ((a 43))
   a)
 43)

(my-assert
 (let ((a 43))
  (list a (cons a 3)))
 (43 (43 . 3)))

(my-assert
 (let ((a 43))
  (list a (cons a 3))
  (list (quote a) (quote (cons a 3))))
 (A (CONS A 3)) )


(my-assert
 1
 1)

(my-assert
 '1
 1)

(my-assert
 '"foo"
 "foo")

(my-assert
 (car '(a b))
 A)

(my-assert
 '(car '(a b))
 (CAR (QUOTE (A B))))

(my-assert
 #(car '(a b))
 #(CAR (QUOTE (A B))))

(my-assert
 '#(car '(a b))
 #(CAR (QUOTE (A B))))

;;; define-compiler-macro
(my-assert
 (defun square (x) (expt x 2))
 SQUARE)

(my-assert
 (define-compiler-macro square (&whole form arg)
   (if (atom arg)
       `(expt ,arg 2)
       (case (car arg)
	 (square (if (= (length arg) 2)
		     `(expt ,(nth 1 arg) 4)
		     form))
	 (expt   (if (= (length arg) 3)
		     (if (numberp (nth 2 arg))
			 `(expt ,(nth 1 arg) ,(* 2 (nth 2 arg)))
			 `(expt ,(nth 1 arg) (* 2 ,(nth 2 arg))))
		     form))
	 (otherwise `(expt ,arg 2)))))
 SQUARE)

(my-assert
 (square (square 3))
 81)

(my-assert
 (macroexpand '(square x))
 (SQUARE X))				;  f

(if (boundp 'x) (makunbound 'x) 'ok)

(my-assert
 (funcall (compiler-macro-function 'square) '(square x) nil)
 (EXPT X 2))

(my-assert
 (funcall (compiler-macro-function 'square) '(square (square x)) nil)
 (EXPT X 4))

(my-assert
 (funcall (compiler-macro-function 'square) '(funcall #'square x) nil)
 (EXPT X 2)
 "define-compiler-macro:
... but if the car of the actual form is the symbol funcall,
then the destructuring of the arguments
is actually performed using its cddr instead")

;;; defmacro
(my-assert
 (defmacro mac1 (a b) "Mac1 multiplies and adds"
   `(+ ,a (* ,b 3)))
 MAC1 )

(my-assert
 (mac1 4 5)
 19 )

(my-assert
 (documentation 'mac1 'function)
 "Mac1 multiplies and adds" )

(my-assert
 (defmacro mac2 (&optional (a 2 b) (c 3 d) &rest x)
   `'(,a ,b ,c ,d ,x))
 MAC2 )

(my-assert
 (mac2 6)
 (6 T 3 NIL NIL) )

(my-assert
 (mac2 6 3 8)
 (6 T 3 T (8)) )

(my-assert
 (defmacro mac3 (&whole r a &optional (b 3) &rest x &key c (d a))
   `'(,r ,a ,b ,c ,d ,x))
 MAC3 )

(my-assert
 (mac3 1 6 :d 8 :c 9 )
 ((MAC3 1 6 :D 8 :C 9 ) 1 6 9 8 (:D 8 :C 9)) )

;;; part II
(my-assert
 (progn
   (defmacro dm1a (&whole x) `',x)
   t)
 t)

(my-assert
 (macroexpand '(dm1a))
 (QUOTE (DM1A)))

(my-assert
 (macroexpand '(dm1a a))
 ERROR)

(my-assert
 (progn
   (defmacro dm1b (&whole x a &optional b) `'(,x ,a ,b))
   t)
 t)

(my-assert
 (macroexpand '(dm1b))
 ERROR)

(my-assert
 (macroexpand '(dm1b q))
 (QUOTE ((DM1B Q) Q NIL)))

(my-assert
 (macroexpand '(dm1b q r))
 (QUOTE ((DM1B Q R) Q R)))

(my-assert
 (macroexpand '(dm1b q r s))
 ERROR)

(my-assert
 (progn
   (defmacro dm2a (&whole form a b) `'(form ,form a ,a b ,b))
   t)
 t)

(my-assert
 (macroexpand '(dm2a x y))
 (QUOTE (FORM (DM2A X Y) A X B Y)))

(my-assert
 (dm2a x y)
 (FORM (DM2A X Y) A X B Y))

(my-assert
 (progn
   (defmacro dm2b (&whole form a (&whole b (c . d) &optional (e 5))
			  &body f &environment env)
     ``(,',form ,,a ,',b ,',(macroexpand c env) ,',d ,',e ,',f))
   t)
 t)

					;Note that because backquote is involved, implementations may differ
					;slightly in the nature (though not the functionality) of the expansion.

					;(my-assert
					;(macroexpand '(dm2b x1 (((incf x2) x3 x4)) x5 x6))
					;#+(or cmu sbcl sbcl) `((DM2B X1 (((INCF X2) X3 X4)) X5 X6) ,X1 (((INCF X2) X3 X4))
					;	(LET* ((#:G411 (+ X2 1)))
					;	      (SETQ X2 #:G411))
					;	(X3 X4) 5 (X5 X6))
					;#-(or cmu sbcl sbcl) (LIST* '(DM2B X1 (((INCF X2) X3 X4))
					;                   X5 X6)
					;            X1
					;            '((((INCF X2) X3 X4)) (SETQ X2 (+ X2 1)) (X3 X4) 5 (X5 X6))))

(my-assert
 (let ((x1 5))
   (macrolet ((segundo (x) `(cadr ,x)))
     (dm2b x1 (((segundo x2) x3 x4)) x5 x6)))
 ((DM2B X1 (((SEGUNDO X2) X3 X4)) X5 X6)
  5 (((SEGUNDO X2) X3 X4)) (CADR X2) (X3 X4) 5 (X5 X6)))

;;; macrofunction

(my-assert
 (defmacro macfun (x) '(macro-function 'macfun))
 MACFUN )

(my-assert
 (not (macro-function 'macfun))
 nil)

(my-assert
 (macrolet ((foo (&environment env)
		 (if (macro-function 'bar env)
		     ''yes
		     ''no)))
   (list (foo)
	 (macrolet ((bar () :beep))
	   (foo))))
 (NO YES))

;;; macroexpand

(my-assert
 (defmacro alpha (x y) `(beta ,x ,y))
 ALPHA)

(my-assert
 (defmacro beta (x y) `(gamma ,x ,y))
 BETA)

(my-assert
 (defmacro delta (x y) `(gamma ,x ,y))
 DELTA)

(my-assert
 (defmacro expand (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand form env)
     `(values ',expansion ',expanded-p)))
 EXPAND)

(my-assert
 (defmacro expand-1 (form &environment env)
   (multiple-value-bind (expansion expanded-p)
       (macroexpand-1 form env)
     `(values ',expansion ',expanded-p)))
 EXPAND-1)

;; Simple examples involving just the global environment
(my-assert
 (multiple-value-bind (a b)
     (macroexpand-1 '(alpha a b))
   (list a b))
 ((BETA A B) T))

(my-assert
 (multiple-value-bind (a b)
     (expand-1 (alpha a b))
   (list a b))
 ((BETA A B) T))

(my-assert
 (multiple-value-bind (a b)
     (macroexpand '(alpha a b))
   (list a b))
 ((GAMMA A B) T))

(my-assert
 (multiple-value-bind (a b)
     (expand (alpha a b))
   (list a b))
 ((GAMMA A B) T))

(my-assert
 (multiple-value-bind (a b)
     (macroexpand-1 'not-a-macro)
   (list a b))
 (NOT-A-MACRO nil))

(my-assert
 (multiple-value-bind (a b)
     (expand-1 not-a-macro)
   (list a b))
 (NOT-A-MACRO nil) )

(my-assert
 (multiple-value-bind (a b)
     (macroexpand '(not-a-macro a b))
   (list a b))
 ((NOT-A-MACRO A B) nil))

(my-assert
 (multiple-value-bind (a b)
     (expand (not-a-macro a b))
   (list a b))
 ((NOT-A-MACRO A B) nil))

;; Examples involving lexical environments

(my-assert
 (multiple-value-bind (n h)
     (macrolet ((alpha (x y) `(delta ,x ,y)))
       (macroexpand-1 '(alpha a b)))
   (list n h))
 ((BETA A B) T))

(my-assert
 (multiple-value-bind (n h)
     (macrolet ((alpha (x y) `(delta ,x ,y)))
       (expand-1 (alpha a b)))
   (list n h))
 ((DELTA A B) T))

(my-assert
 (multiple-value-bind (n h)
     (macrolet ((alpha (x y) `(delta ,x ,y)))
       (macroexpand '(alpha a b)))
   (list n h))
 ((GAMMA A B) T))

(my-assert
 (multiple-value-bind (n h)
     (macrolet ((alpha (x y) `(delta ,x ,y)))
       (expand (alpha a b)))
   (list n h))
 ((GAMMA A B) T))


(my-assert
 (multiple-value-bind (n h)
     (macrolet ((beta (x y) `(epsilon ,x ,y)))
       (expand (alpha a b)))
   (list n h))
 ((EPSILON A B) T))

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a (first x)))
	 (expand a)))
   (list n h))
 error
 "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a-new (first x)))
	 (expand a-new)))
   (list n h))
 ((FIRST X) T))

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a (first x)))
	 (macroexpand 'a)))
   (list n h))
 error
 "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a-new (first x)))
	 (macroexpand 'a-new)))
   (list n h))
 (a-new nil))

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b (alpha x y)))
       (expand-1 b))
   (list n h))
 error
 "B has been declared special, thus SYMBOL-MACROLET may not bind it")

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b-new (alpha x y)))
       (expand-1 b-new))
   (list n h))
 ((ALPHA X Y)  T))

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b (alpha x y)))
       (expand b))
   (list n h))
 error
 "B has been declared special, thus SYMBOL-MACROLET may not bind it")

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b-new (alpha x y)))
       (expand b-new))
   (list n h))
 ((GAMMA X Y) T))

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b (alpha x y))
		       (a b))
       (expand-1 a))
   (list n h))
 error
 "A and B have been declared special, thus SYMBOL-MACROLET may not bind them")

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b-new (alpha x y))
		       (a-new b-new))
       (expand-1 a-new))
   (list n h))
 (B-NEW T))

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b (alpha x y))
		       (a b))
       (expand a))
   (list n h))
 error
 "A and B have been declared special, thus SYMBOL-MACROLET may not bind them")

(my-assert
 (multiple-value-bind (n h)
     (symbol-macrolet ((b-new (alpha x y))
		       (a-new b-new))
       (expand a-new))
   (list n h))
 ((GAMMA X Y) T))

;; Examples of shadowing behavior
(my-assert
 (multiple-value-bind (n h)
     (flet ((beta (x y) (+ x y)))
       (expand (alpha a b)))
   (list n h))
 ((BETA A B) T))

(my-assert
 (multiple-value-bind (n h)
     (macrolet ((alpha (x y) `(delta ,x ,y)))
       (flet ((alpha (x y) (+ x y)))
	 (expand (alpha a b))))
   (list n h))
 ((ALPHA A B) nil))

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a (first x)))
	 (let ((a x))
	   (expand a))))
   (list n h))
 error
 "A has been declared special, thus SYMBOL-MACROLET may not bind it")

(my-assert
 (multiple-value-bind (n h)
     (let ((x (list 1 2 3)))
       (symbol-macrolet ((a-new (first x)))
	 (let ((a-new x))
	   (expand a-new))))
   (list n h))
 (a-new nil))

;;; define-symbol-macro
(my-assert
 (defvar *things* (list 'alpha 'beta 'gamma))
 *THINGS*)

(my-assert
 (fboundp 'define-symbol-macro)
 T
 "The macro DEFINE-SYMBOL-MACRO should exist")

(my-assert
 (define-symbol-macro thing1 (first *things*))
 THING1)

(my-assert
 (define-symbol-macro thing2 (second *things*))
 THING2)

(my-assert
 (define-symbol-macro thing3 (third *things*))
 THING3)

(my-assert
 thing1
 ALPHA)

(my-assert
 (setq thing1 'ONE)
 ONE)

(my-assert
 *things*
 (ONE BETA GAMMA))

(my-assert
 (multiple-value-setq (thing2 thing3) (values 'two 'three))
 TWO)

(my-assert
 thing3
 THREE)

(my-assert
 *things*
 (ONE TWO THREE))

(my-assert
 (list thing2 (let ((thing2 2)) thing2))
 (TWO 2))

;;; *macrexpand-hook*

(my-assert
 (defun hook (expander form env)
   (format t "Now expanding: ~S~%" form)
   (funcall expander form env))
 HOOK )

(my-assert
 (defmacro machook (x y) `(/ (+ ,x ,y) 2))
 MACHOOK )

(my-assert
 (macroexpand '(machook 1 2))
 (/ (+ 1 2) 2))				; true

(my-assert
 (let ((*macroexpand-hook* #'hook)) (macroexpand '(machook 1 2)))
 (/ (+ 1 2) 2))				; true

;;; special opperator

(my-assert
 (special-operator-p 'if)
 T)

(my-assert
 (special-operator-p 'car)
 nil)

(my-assert
 (special-operator-p 'one)
 nil)


(my-assert
 (special-operator-p 'block)
 T)

(my-assert
 (special-operator-p 'let*)
 T)

(my-assert
 (special-operator-p 'return-from)
 T)

(my-assert
 (special-operator-p 'catch)
 T)

(my-assert
 (special-operator-p 'load-time-value)
 T)

(my-assert
 (special-operator-p 'setq)
 T)

(my-assert
 (special-operator-p 'eval-when)
 T)

(my-assert
 (special-operator-p 'locally)
 T
 "locally is a special operator")

(my-assert
 (special-operator-p 'symbol-macrolet)
 T)

(my-assert
 (special-operator-p 'flet)
 T)

(my-assert
 (special-operator-p 'macrolet)
 T)

(my-assert
 (special-operator-p 'tagbody)
 T)

(my-assert
 (special-operator-p 'function)
 T)

(my-assert
 (special-operator-p 'multiple-value-call)
 T)

(my-assert
 (special-operator-p 'the)
 T)

(my-assert
 (special-operator-p 'go)
 T)

(my-assert
 (special-operator-p 'multiple-value-prog1)
 T)

(my-assert
 (special-operator-p 'throw)
 T)

(my-assert
 (special-operator-p 'progn)
 T)

(my-assert
 (special-operator-p 'unwind-protect)
 T)

(my-assert
 (special-operator-p 'labels)
 T)

(my-assert
 (special-operator-p 'progv)
 T)

(my-assert
 (special-operator-p 'let)
 T)

(my-assert
 (special-operator-p 'quote)
 T)

;;; constantp

(my-assert
 (constantp 1)
 T)

(my-assert
 (constantp 'temp)
 nil)

(my-assert
 (constantp ''temp)
 t)

(my-assert
 (defconstant this-is-a-constant 'never-changing)
 THIS-IS-A-CONSTANT )

(my-assert
 (constantp 'this-is-a-constant)
 t)

(my-assert
 (constantp "temp")
 t)

(my-assert
 (let ((a 6))
   a)
 6 )

(my-assert
 (let ((a 6))
 (constantp a))
 t)

(my-assert
 (constantp (values 37 Pi 'foo))
 #+(or cmu sbcl sbcl clisp ecls) t
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)


(my-assert
 (constantp '(sin pi))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(car '(x)))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(eql x x))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(typep x 'nil))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(typep x 't))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(values this-is-a-constant))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(values 'x 'y))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)

(my-assert
 (constantp '(let ((a '(a b c))) (+ (length a) 6)))
 #+(or cmu sbcl sbcl clisp ecls) nil
 #-(or cmu sbcl sbcl clisp ecls) FILL-THIS-IN)









