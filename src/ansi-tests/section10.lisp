;;; section 10: symbols -*- mode: lisp -*-
(in-package :cl-user)

;;; symbolp

(my-assert
 (symbolp 'elephant)
 t)


(my-assert
 (symbolp 12)
 nil)


(my-assert
 (symbolp nil)
 t)


(my-assert
 (symbolp '())
 t)


(my-assert
 (symbolp :test)
 t)


(my-assert
 (symbolp "hello")
 nil)

;;; keywordp

(my-assert
 (keywordp 'elephant)
 nil)


(my-assert
 (keywordp 12)
 nil)


(my-assert
 (keywordp :test)
 t)


(my-assert
 (keywordp ':test)
 t)


(my-assert
 (keywordp nil)
 nil)


(my-assert
 (keywordp :nil)
 t)


(my-assert
 (keywordp '(:test))
 nil)


(my-assert
 (keywordp "hello")
 nil)


(my-assert
 (keywordp ":hello")
 nil)


(my-assert
 (keywordp '&optional)
 nil)

;;; make-symbol


(my-assert
 (setq temp-string "temp")
 "temp")


(my-assert
 (progn
   (setq temp-symbol (make-symbol temp-string))
   t)
 t)


(my-assert
 (symbol-name temp-symbol)
 "temp")


(my-assert
 (eq (symbol-name temp-symbol) temp-string)
 #+(or cmu sbcl clisp ecls) t
 #-(or cmu sbcl clisp ecls) fill-this-in)


(my-assert
 (multiple-value-bind (a b)
     (find-symbol "temp")
   (list a b))
 ( NIL NIL))


(my-assert
 (eq (make-symbol temp-string) (make-symbol temp-string))
 nil)

;;; copy-symbol


(my-assert
 (setq fred 'fred-smith)
 FRED-SMITH)


(my-assert
 (setf (symbol-value fred) 3)
 3)


(my-assert
 (progn
   (setq fred-clone-1a (copy-symbol fred nil))
   t)
 t)


(my-assert
 (progn
   (setq fred-clone-1b (copy-symbol fred nil))
   t)
 t)


(my-assert
 (progn
   (setq fred-clone-2a (copy-symbol fred t))
   t)
 t)


(my-assert
 (progn
   (setq fred-clone-2b (copy-symbol fred t))
   t)
 t)


(my-assert
 (eq fred fred-clone-1a)
 nil)


(my-assert
 (eq fred-clone-1a fred-clone-1b)
 nil)


(my-assert
 (eq fred-clone-2a fred-clone-2b)
 nil)


(my-assert
 (eq fred-clone-1a fred-clone-2a)
 nil)


(my-assert
 (symbol-value fred)
 3)


(my-assert
 (boundp fred-clone-1a)
 nil)


(my-assert
 (symbol-value fred-clone-2a)
 3)


(my-assert
 (setf (symbol-value fred-clone-2a) 4)
 4)


(my-assert
 (symbol-value fred)
 3)


(my-assert
 (symbol-value fred-clone-2a)
 4)


(my-assert
 (symbol-value fred-clone-2b)
 3)


(my-assert
 (boundp fred-clone-1a)
 nil)


(my-assert
 (progn
   (setf (symbol-function fred) #'(lambda (x) x))
   t)
 t)


(my-assert
 (fboundp fred)
 t)


(my-assert
 (fboundp fred-clone-1a)
 nil)


(my-assert
 (fboundp fred-clone-2a)
 nil)

;;; symbol-function


(my-assert
 (progn
   (symbol-function 'car)
   t)
 t)

(my-assert
 (symbol-function 'twice)
 UNDEFINED-FUNCTION)


(my-assert
 (defun twice (n) (* n 2))
 TWICE)


(my-assert
 (progn
   (symbol-function 'twice)
   t)
 t)


(my-assert
 (list (twice 3)
       (funcall (function twice) 3)
       (funcall (symbol-function 'twice) 3))
 (6 6 6))


(my-assert
 (flet ((twice (x) (list x x)))
   (list (twice 3)
	 (funcall (function twice) 3)
	 (funcall (symbol-function 'twice) 3)))
 ((3 3) (3 3) 6)   )


(my-assert
 (progn
   (setf (symbol-function 'twice) #'(lambda (x) (list x x)))
   t)
 t)


(my-assert
 (list (twice 3)
       (funcall (function twice) 3)
       (funcall (symbol-function 'twice) 3))
 ((3 3) (3 3) (3 3)))


(my-assert
 (fboundp 'defun)
 t)


(my-assert
 (progn
   (symbol-function 'defun)
   t)
 t)


(my-assert
 (progn
   (functionp (symbol-function 'defun))
   t)
 t)


(my-assert
 (defun symbol-function-or-nil (symbol)
   (if (and (fboundp symbol)
	    (not (macro-function symbol))
	    (not (special-operator-p symbol)))
       (symbol-function symbol)
       nil))
 SYMBOL-FUNCTION-OR-NIL)


(my-assert
 (progn
   (symbol-function-or-nil 'car)
   t)
 t)


(my-assert
 (symbol-function-or-nil 'defun)
 NIL)

;;; symbol-name


(my-assert
 (symbol-name 'temp)
 "TEMP" )


(my-assert
 (symbol-name :start)
 "START")

;;; symbol-package


(my-assert
 (progn
   (in-package "CL-USER")
   t)
 t)



(my-assert
 (progn
   (symbol-package 'car)
   t)
 t)



(my-assert
 (progn
   (symbol-package 'bus)
   t)
 t)


(my-assert
 (progn
   (symbol-package :optional)
   t)
 t)


;; Gensyms are uninterned, so have no home package.

(my-assert
 (symbol-package (gensym))
 NIL)


(if (find-package "PK2")
    (delete-package
     (find-package "PK2")))

(if (find-package "PK1")
    (delete-package
     (find-package "PK1")))

(my-assert
 (find-package "PK1")
 nil)

(my-assert
 (progn
   (make-package 'pk1)
   t)
 t)


(my-assert
 (multiple-value-bind (a b)
     (intern "SAMPLE1" "PK1")
   (list a b))
 (PK1::SAMPLE1 :internal))


(my-assert
 (export (find-symbol "SAMPLE1" "PK1") "PK1")
 T)


(my-assert
 (progn
   (make-package 'pk2 :use '(pk1))
   t)
 t)


(my-assert
 (multiple-value-bind (a b)
     (find-symbol "SAMPLE1" "PK2")
   (list a b))
 (PK1:SAMPLE1 :INHERITED))


(my-assert
 (progn
   (symbol-package 'pk1::sample1)
   t)
 t)


(my-assert
 (progn
   (symbol-package 'pk2::sample1)
   t)
 t)


(my-assert
 (progn
   (symbol-package 'pk1::sample2)
   t)
 t)


(my-assert
 (progn
   (symbol-package 'pk2::sample2)
   t)
 t)

;; The next several forms create a scenario in which a symbol
;; is not really uninterned, but is "apparently uninterned",
;; and so SYMBOL-PACKAGE still returns NIL.

(my-assert
 (setq s3 'pk1::sample3)
 PK1::SAMPLE3)


(my-assert
 (import s3 'pk2)
 T)


(my-assert
 (unintern s3 'pk1)
 T)


(my-assert
 (symbol-package s3)
 NIL)


(my-assert
 (eq s3 'pk2::sample3)
 T)

;;; symbol-plist


(setq sym (gensym))

(my-assert
 (symbol-plist sym)
 ())


(my-assert
 (setf (get sym 'prop1) 'val1)
 VAL1)


(my-assert
 (symbol-plist sym)
 (PROP1 VAL1))


(my-assert
 (setf (get sym 'prop2) 'val2)
 VAL2)


(my-assert
 (symbol-plist sym)
 (PROP2 VAL2 PROP1 VAL1))


(my-assert
 (setf (symbol-plist sym) (list 'prop3 'val3))
 (PROP3 VAL3))


(my-assert
 (symbol-plist sym)
 (PROP3 VAL3))

;;; setf


(my-assert
 (setf (symbol-value 'a) 1)
 1)


(my-assert
 (symbol-value 'a)
 1)

;; SYMBOL-VALUE can see dynamic variables.

(my-assert
 (let ((a 2))
   (declare (special a))
   (symbol-value 'a))
 2)


(my-assert
 (let ((a 2))
   (declare (special a))
   (setq a 3)
   (symbol-value 'a))
 3)


(my-assert
 (let ((a 2))
   (setf (symbol-value 'a) 3)
   t)
 t)


					;(my-assert
					;a
					;3)


					;(my-assert
					;(symbol-value 'a)
					;3)


(my-assert
 (multiple-value-bind (h j)
     (let ((a 4))
       (declare (special a))
       (let ((b (symbol-value 'a)))
	 (setf (symbol-value 'a) 5)
	 (values a b)))
   (list h j))
 (5 4))


					;(my-assert
					;a
					;3)


(my-assert
 (symbol-value :any-keyword)
 :ANY-KEYWORD)


(my-assert
 (symbol-value 'nil)
 NIL)


(my-assert
 (symbol-value '())
 NIL)

;; The precision of this next one is implementation-dependent.

(my-assert
 (symbol-value 'pi)
 #-clisp
 3.141592653589793d0
 #+clisp
 3.1415926535897932385L0)

;;; get


(my-assert
 (defun make-person (first-name last-name)
   (let ((person (gensym "PERSON")))
     (setf (get person 'first-name) first-name)
     (setf (get person 'last-name) last-name)
     person))
 MAKE-PERSON)


(my-assert
 (defvar *john* (make-person "John" "Dow"))
 *JOHN*)


(my-assert
 (progn
   *john*
   t)
 t)


(my-assert
 (defvar *sally* (make-person "Sally" "Jones"))
 *SALLY*)


(my-assert
 (get *john* 'first-name)
 "John")


(my-assert
 (get *sally* 'last-name)
 "Jones")


(my-assert
 (defun marry (man woman married-name)
   (setf (get man 'wife) woman)
   (setf (get woman 'husband) man)
   (setf (get man 'last-name) married-name)
   (setf (get woman 'last-name) married-name)
   married-name)
 MARRY)


(my-assert
 (marry *john* *sally* "Dow-Jones")
 "Dow-Jones")


(my-assert
 (get *john* 'last-name)
 "Dow-Jones")


(my-assert
 (get (get *john* 'wife) 'first-name)
 "Sally")


(my-assert
 (progn
   (symbol-plist *john*)
   t)
 t)


(my-assert
 (defmacro age (person &optional (default ''thirty-something))
   `(get ,person 'age ,default))
 AGE)


(my-assert
 (age *john*)
 THIRTY-SOMETHING)


(my-assert
 (age *john* 20)
 20)


(my-assert
 (setf (age *john*) 25)
 25)


(my-assert
 (age *john*)
 25)


(my-assert
 (age *john* 20)
 25)

;;; remprop


(my-assert
 (progn
   (setq test (make-symbol "PSEUDO-PI"))
   t)
 t)


(my-assert
 (symbol-plist test)
 ())


(my-assert
 (setf (get test 'constant) t)
 T)


(my-assert
 (setf (get test 'approximation) 3.14)
 3.14)


(my-assert
 (setf (get test 'error-range) 'noticeable)
 NOTICEABLE)


(my-assert
 (symbol-plist test)
 (ERROR-RANGE NOTICEABLE APPROXIMATION 3.14 CONSTANT T))


(my-assert
 (setf (get test 'approximation) nil)
 NIL)


(my-assert
 (symbol-plist test)
 (ERROR-RANGE NOTICEABLE APPROXIMATION NIL CONSTANT T))


(my-assert
 (get test 'approximation)
 NIL)


(my-assert
 (not (remprop test 'approximation))
 nil)


(my-assert
 (get test 'approximation)
 NIL)


(my-assert
 (symbol-plist test)
 (ERROR-RANGE NOTICEABLE CONSTANT T))


(my-assert
 (remprop test 'approximation)
 NIL)


(my-assert
 (symbol-plist test)
 (ERROR-RANGE NOTICEABLE CONSTANT T))


(my-assert
 (not (remprop test 'error-range))
 nil)


(my-assert
 (setf (get test 'approximation) 3)
 3)


(my-assert
 (symbol-plist test)
 (APPROXIMATION 3 CONSTANT T))


;;; boundp


(my-assert
 (setq x 1)
 1)


(my-assert
 (boundp 'x)
 t)


(my-assert
 (makunbound 'x)
 X)


(my-assert
 (boundp 'x)
 nil)



(my-assert
 (let ((x 2)) (declare (special x)) (boundp 'x))
 t)

;;; mkunbound


(my-assert
 (setf (symbol-value 'a) 1)
 1)


(my-assert
 (boundp 'a)
 t)


(my-assert
 a
 1)


(my-assert
 (makunbound 'a)
 A)


(my-assert
 (boundp 'a)
 nil)

;;; set


(my-assert
 (setf (symbol-value 'n) 1)
 1)


(my-assert
 (set 'n 2)
 2)


(my-assert
 (symbol-value 'n)
 2)


					;(my-assert
					;(let ((n 3))
					;  (declare (special n))
					;  (setq n (+ n 1))
					;  (setf (symbol-value 'n) (* n 10))
					;  (set 'n (+ (symbol-value 'n) n))
					;   n)
					;80)


					;(my-assert
					;n
					;2)


					;(my-assert
					;(let ((n 3))
					;  (setq n (+ n 1))
					;  (setf (symbol-value 'n) (* n 10))
					;  (set 'n (+ (symbol-value 'n) n))
					;  n)
					;4)


					;(my-assert
					;n
					;44)


(my-assert
 (defvar *n* 2)
 *N*)


(my-assert
 (let ((*n* 3))
   (setq *n* (+ *n* 1))
   (setf (symbol-value '*n*) (* *n* 10))
   (set '*n* (+ (symbol-value '*n*) *n*))
   *n*)
 80)


(my-assert
 *n*
 2)


(my-assert
 (defvar *even-count* 0)
 *EVEN-COUNT*)


(my-assert
 (defvar *odd-count* 0)
 *ODD-COUNT*)


(my-assert
 (defun tally-list (list)
   (dolist (element list)
     (set (if (evenp element) '*even-count* '*odd-count*)
	  (+ element (if (evenp element) *even-count* *odd-count*)))))
 tally-list)


(my-assert
 (tally-list '(1 9 4 3 2 7))
 NIL)


(my-assert
 *even-count*
 6)



(my-assert
 *odd-count*
 20)

