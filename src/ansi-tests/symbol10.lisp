;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (progn (in-package :cl-user) nil)
 nil
 "in-package expects a 'string designator'
this is or a character, a symbol or a string.")
;; test der neuen valuezelle

;;; 1. ungebundenes symbol


(my-assert
 (defun testvar (var)
   (list (boundp var)			; gebunden
         (if (boundp var)
	     (symbol-value var)
	     nil)			; wert/nil
         (constantp var)		; konstante
         #+xcl
	 (eq (sys::%p-get-cdr var 0)
	     sys::%cdr-specsym)		; specvar
         #+clisp
	 (and (sys::special-variable-p var)
	      (not (constantp var)))	; specvar
         #+allegro
	 (and (not (constantp var))
	      (eval `(let ((,var (list nil)))
		       (and (boundp ',var)
			    (eq (symbol-value ',var)
				,var)))))
         #+cmu	 
	 (eq (ext:info variable kind var)
	     ':special);; like clisp
	 #+ecls
	 (si::specialp var)
         #+sbcl	 
	 (eq (sb-int::info variable kind var)
	     ':special);; like clisp
         (and (fboundp var) t)		; funktion. eigenschaft
         (and (fboundp var) (macro-function var) t) ; macro?
         (and (fboundp var)
	      (special-operator-p var)
	      t)			; spezialform?
         #-clisp
	 (and (symbol-plist var) t)	; p-liste?
         #+clisp
	 (and (or (get var 'i1)
		  (get var 'i2)
		  (get var 'i3))
	      t)			; p-liste?
         (get var 'i1)			; i1
         (get var 'i2)			; i2
         (get var 'i3)			; i3
	 )  )
 testvar)

(my-assert
 (defun clrvar (var)
   #+xcl
   (subr 84				;sys::%p-set-cdr-content
	 var 0 (sys::%p-get-content 'sys::%void-value 0) 0)
   #-xcl
   (progn (makunbound var) (fmakunbound var)
	  (setf (symbol-plist var) '()))
   #+allegro
   (setf (excl::symbol-bit var 'excl::.globally-special.) nil)
   #+cmu
   (setf (ext:info variable kind var) ':global)
   #+sbcl
   (setf (sb-int::info variable kind var) ':global)
   var)
 clrvar)

#+(or xcl clisp allegro cmu sbcl)
(my-assert
 (progn (setf (symbol-function 'setf-get)
	      (symbol-function #+xcl 'sys::setf-get
			       #+clisp 'sys::%put
			       #+allegro 'excl::.inv-get
			       #+(or cmu sbcl) 'cl::%put)) t)
 t)

;;; begin breitentest

(my-assert
 (clrvar 'v1)
 v1)

;;;; value - umbinden - macro - umbinden - props - umbinden

;;; value

(my-assert
 (testvar 'v1)
 ;; geb val konst svar func mac spec plist i1  i2  i3
 (nil nil nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (setq v1 'val)
 val)

(my-assert
 (testvar 'v1)
 ;; geb val konst svar func mac spec plist i1  i2  i3
 (t   val nil   nil  nil  nil nil  nil   nil nil nil))

;;; umbinden

(my-assert
 (makunbound 'v1)
 v1)

(my-assert
 (testvar 'v1)
 ;; geb val konst svar func mac spec plist i1  i2  i3
 (nil nil nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (setq v1 'val2)
 val2)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val2 nil   nil  nil  nil nil  nil   nil nil nil))

;;; macro

(my-assert
 (defmacro v1 (x) (list 'quote x))
 v1)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val2 nil   nil  t    t   nil  nil   nil nil nil))

;;; umbinden

(my-assert
 (fmakunbound 'v1)
 v1)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val2 nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (defmacro v1 (x) (list 'quote (list x x)))
 v1)

(my-assert
 (v1 33)
 (33 33))

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val2 nil   nil  t    t   nil  nil   nil nil nil))

(my-assert
 (makunbound 'v1)
 v1)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    t   nil  nil   nil nil nil))

(my-assert
 (setq v1 'val3)
 val3)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val3 nil   nil  t    t   nil  nil   nil nil nil))

;;; props

(my-assert
 (setf-get 'v1 'i1 11)
 11)

(my-assert
 (setf-get 'v1 'i2 22)
 22)

(my-assert
 (setf-get 'v1 'i3 33)
 33)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val3 nil   nil  t    t   nil  t     11  22  33))

;;; umbinden

(my-assert
 (not (null (remprop 'v1 'i2)))
 t)
(my-assert
 (not (null (remprop 'v1 'i1)))
 t)
(my-assert
 (not (null (remprop 'v1 'i3)))
 t)
(my-assert
 (fmakunbound 'v1)
 v1)
(my-assert
 (makunbound 'v1)
 v1)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (setf-get 'v1 'i1 99)
 99)
(my-assert
 (defmacro v1 (x) (list 'quote (list x x x)))
 v1)
(my-assert
 (v1 a)
 (a a a))
(my-assert
 (setq v1 'val4)
 val4)

(my-assert
 (testvar 'v1)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   val4 nil   nil  t    t   nil  t     99  nil nil))

;;; --- ende test1 -----

(my-assert
 (clrvar 'v2)
 v2)

;;; specvar - props - rebind - function

(my-assert
 (defvar v2 'v2a)
 v2)

(my-assert
 (testvar 'v2)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   v2a  nil   t    nil  nil nil  nil   nil nil nil))

(my-assert
 (setf-get 'v2 'i3 33)
 33)
(my-assert
 (setf-get 'v2 'i2 22)
 22)
(my-assert
 (setf-get 'v2 'i1 11)
 11)

(my-assert
 (testvar 'v2)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   v2a  nil   t    nil  nil nil  t     11  22  33))

;;; rebind

(my-assert
 (makunbound 'v2)
 v2)
(my-assert
 (not (null (remprop 'v2 'i1)))
 t)
(my-assert
 (not (null (remprop 'v2 'i2)))
 t)
(my-assert
 (not (null (remprop 'v2 'i3)))
 t)

(my-assert
 (testvar 'v2)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 #+xcl
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
 #-xcl
 (nil nil  nil   t    nil  nil nil  nil   nil nil nil))

(my-assert
 (defvar v2 'v2b)
 v2)
(my-assert
 (setf-get 'v2 'i1 111)
 111)
(my-assert
 (setf-get 'v2 'i2 222)
 222)
(my-assert
 (setf-get 'v2 'i3 333)
 333)

(my-assert
 (testvar 'v2)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   v2b  nil   t    nil  nil nil  t     111 222 333))

;;; function

(my-assert
 (defun v2 (x) (list x x))
 v2)
(my-assert
 (v2 44)
 (44 44))

(my-assert
 (testvar 'v2)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   v2b  nil   t    t    nil nil  t     111 222 333 ))


(my-assert
 (clrvar 'v3)
 v3)

;;;;; function - con - rebind - prop

;;; function

(my-assert
 (defun v3 (x y) (list x y))
 v3)

(my-assert
 (testvar 'v3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

;;; constant

(my-assert
 (defconstant v3 99)
 v3)

(my-assert
 v3
 99)
(my-assert
 (v3 'a 'b)
 (a b))

(my-assert
 (testvar 'v3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t    99  t     nil  t    nil nil  nil   nil nil nil))

;;; rebind

(my-assert
 (makunbound 'v3)
 #+(or xcl allegro cmu sbcl) v3
 #+(or clisp ecls) error)

(my-assert
 (fmakunbound 'v3)
 v3)

#+xcl
(my-assert
 (testvar 'v3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (defconstant v3 999)
 v3)

(my-assert
 (defun v3 (x) (list x x))
 v3)

(my-assert
 (v3 'c)
 (c c))

(my-assert
 v3
 999)

(my-assert
 (testvar 'v3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   999  t     nil  t    nil nil  nil   nil nil nil))

;;;defparameter

(my-assert
 (defparameter var33)
 error)

(my-assert
 (defparameter var3 99)
 var3)

(my-assert
 var3
 99)

(my-assert
 (testvar 'var3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t    99  nil   t    nil  nil nil  nil   nil nil nil))

;;; rebind

(my-assert
 (makunbound 'var3)
 var3)

(my-assert
 (testvar 'var3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 #+xcl
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil)
 #-xcl
 (nil nil  nil   t    nil  nil nil  nil   nil nil nil))

;;; props

(my-assert
 (setf-get 'v3 'i2 222)
 222)

(my-assert
 (setf-get 'v3 'i1 111)
 111)

(my-assert
 (testvar 'v3)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   999  t     nil  t    nil nil  t     111 222 nil))


(my-assert
 (clrvar 'v4)
 v4)

;;;;  function - rebind - prop - rebind - specvar

(my-assert
 (defun v4 (x) x)
 v4)

(my-assert
 (v4 55)
 55)

(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

;;; rebind

(my-assert
 (fmakunbound 'v4)
 v4)

(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (defun v4 (x) (list x))
 v4)

(my-assert
 (v4 88)
 (88))

(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    nil nil  nil   nil nil nil))

(my-assert
 (setf-get 'v4 'i1 11)
 11)

(my-assert
 (setf-get 'v4 'i2 22)
 22)

(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    nil nil  t     11  22  nil))

;;; rebind

(my-assert
 (fmakunbound 'v4)
 v4)
(my-assert
 (not (null (remprop 'v4 'i1)))
 t)
(my-assert
 (not (null (remprop 'v4 'i2)))
 t)
(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (defun v4 (x) (list x x x))
 v4)

(my-assert
 (v4 44)
 (44 44 44))

(my-assert
 (setf-get 'v4 'i2 222)
 222)

(my-assert
 (setf-get 'v4 'i3 333)
 333)

(my-assert
 (testvar 'v4)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    nil nil  t     nil 222 333))

(my-assert
 (defvar v4 'v4-value)
 v4)

(my-assert
 (testvar 'v4)
 ;; geb val     konst svar func mac spec plist i1  i2  i3
 (t  v4-value nil   t    t    nil nil  t     nil 222 333))

(my-assert
 (clrvar 'v5)
 v5)

;;;;; prop - rebind - con - rebind - fun

(my-assert
 (setf-get 'v5 'i1 1)
 1)
(my-assert
 (setf-get 'v5 'i2 2)
 2)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  t     1   2  nil))

;;; rebind

(my-assert
 (not (null (remprop 'v5 'i1)))
 t)
(my-assert
 (not (null (remprop 'v5 'i2)))
 t)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

(my-assert
 (setf-get 'v5 'i1 11)
 11)
(my-assert
 (setf-get 'v5 'i2 22)
 22)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  t     11  22  nil))

;;; con

(my-assert
 (defconstant v5 '123)
 v5)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   123  t     nil  nil  nil nil  t     11  22  nil))

;;; rebind

(my-assert
 (makunbound 'v5)
 #+(or xcl allegro cmu sbcl) v5
 #+(or clisp ecls) error)

(my-assert
 (not (null (remprop 'v5 'i2)))
 t)

(my-assert
 (not (null (remprop 'v5 'i1)))
 t)

#+xcl
(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  nil   nil nil nil))

;;; das ging schief !!

(my-assert
 (defconstant v5 321)
 v5)

(my-assert
 (setf-get 'v5 'i3 333)
 333)

(my-assert
 (setf-get 'v5 'i2 222)
 222)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   321  t     nil  nil  nil nil  t     nil 222 333))

(my-assert
 (defun v5 (x) x)
 v5)

(my-assert
 (v5 666)
 666)

(my-assert
 (testvar 'v5)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   321  t     nil  t    nil nil  t     nil 222 333))

(my-assert
 (clrvar 'v6)
 v6)

;;;;; prop mac con

(my-assert
 (setf-get 'v6 'i1 1)
 1)

(my-assert
 (setf-get 'v6 'i3 3)
 3)

(my-assert
 (testvar 'v6)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  nil  nil nil  t     1   nil 3))

(my-assert
 (defmacro v6 (x) (list 'quote x))
 v6)

(my-assert
 (v6 a)
 a)

(my-assert
 (testvar 'v6)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (nil nil  nil   nil  t    t   nil  t     1   nil 3))

(my-assert
 (defconstant v6 234)
 v6)

(my-assert
 (testvar 'v6)
 ;; geb val  konst svar func mac spec plist i1  i2  i3
 (t   234  t     nil  t    t   nil  t     1   nil 3))


;;  aufraeumen
(mapc #'unintern '(v1 v2 v3 v4 v5 v6))

