;;; based on v1.7 -*- mode: lisp -*-
;; ****************************************************************************
;; *                      kurztest        xcl                                 *
;; ****************************************************************************
;; kap. 1 einfuehrung
;; ----------------------------------------------------------------------------
;; kap. 2  datentypen
;; ----------------------------------------------------------------------------
;; kap. 3  gueltigkeitsbereiche
;; ----------------------------------------------------------------------------
;; kap. 4  typspezifier
;; ----------------------------------------------------------------------------
;;
;; deftype, coerce, type-of
;;
;; kap. 5  programmstrukturen
;; ----------------------------------------------------------------------------
;;
;; lambda-listen
(in-package :cl-user)

(my-assert
 ((lambda (a b)
    (+ a (* b 3)))
  4 5)
 19)

(my-assert
 ((lambda (a &optional (b 2))
    (+ a (* b 3)))
  4 5)
 19)

(my-assert
 ((lambda (&optional (a 2 b) (c 3 d) &rest x)
    (list a b c d x)))
 (2 nil 3 nil nil))

(my-assert
 ((lambda (a b &key c d)
    (list a b c d))
  1 2)
 (1 2 nil nil))

(my-assert
 ((lambda (a &optional (b 3) &rest x &key c (d a))
    (list a b c d x))
  1)
 (1 3 nil 1 nil))

(my-assert
 ((lambda (x &aux (a 3) (b 4))
    (+ x (* a b)))
  2)
 14)

(my-assert
 ((lambda (x y &optional a b &rest z &key c (d y) &aux (u 3) (v 4))
    (+ x y a (* b (car z)) c (* d u) v))
  3 4 5 2 7 :c 6 :d 8)
 error)

(my-assert
 ((lambda (x y)
    ((lambda (a b)
       (list a b))
     'u 'v))
  5 6)
 (u v))

(my-assert
 ((lambda (x &allow-other-keys)
    (list x y))
  2 :y 3)
 error)

(my-assert
 lambda-list-keywords
 #+xcl (&optional &rest &key &allow-other-keys &aux &body &whole system::&environment)
 #+clisp (&optional &rest &key &allow-other-keys &aux &body &whole &environment)
 #+(or akcl ecls)
 (&optional &rest &key &allow-other-keys &aux &whole &environment &body)
 #+(or allegro cmu sbcl)
 (&optional &rest &key &aux &body &whole &allow-other-keys &environment)
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (let ((s (prin1-to-string lambda-parameters-limit )))
   (or #+xcl (equal s "128")
       #+clisp (equal s "65536")
       #+clisp (equal s "4294967296")
       #+akcl (equal s "64")
       #+allegro (equal s "16384")
       #+(or cmu sbcl sbcl) (equal s "536870911")
       ) )
 t)

;; defvar, defconstant, defparameter, eval-when

;; kap 6 praedikate
;; ----------------------------------------------------------------------------

(my-assert
 (typep 'nil 'null)
 t)

(my-assert
 (typep (list 'a 'b 'c) 'null)
 nil)

(my-assert
 (typep 'abc 'symbol)
 t)

(my-assert
 (typep 4 'atom)
 t)

(my-assert
 (typep 55 'cons)
 nil)

(my-assert
 (typep (list 'a (list 'b 'c)) 'list)
 t)

(my-assert
 (typep 5/8 'number)
 t)

(my-assert
 (typep -800 'integer)
 t)

(my-assert
 (typep 5/7 'rational)
 t)

(my-assert
 (typep 2.718 'float)
 t)

(my-assert
 (typep #c(1.23 3.56) 'float)
 nil)

(my-assert
 (typep #\a 'character)
 t)

(my-assert
 (typep "abc" 'string)
 t)

(my-assert
 (typep '#(1 2 3) 'string)
 nil)

(my-assert
 (typep '#(a b c) 'bit-vector)
 nil)

(my-assert
 (typep '#(a b c) 'vector)
 t)

(my-assert
 (typep "abc" 'vector)
 t)

(my-assert
 (typep '#(1 2 3 4) 'simple-vector)
 t)

(my-assert
 (typep 3 'simple-vector)
 nil)

(my-assert
 (typep "a b cd" 'simple-string)
 t)

(my-assert
 (typep 'abc 'simple-string)
 nil)

(my-assert
 (typep #*1101 'simple-bit-vector)
 t)

(my-assert
 (typep '#(1 0 0 1) 'simple-bit-vector)
 nil)

(my-assert
 (typep '#2a((a b)(c d)) 'array)
 t)

(my-assert
 (setq x 7)
 7)

(my-assert
 (typep x 'compiled-function)
 nil)

(my-assert
 (typep x 'common)
 error)

(unintern 'x)

(my-assert
 (subtypep 'character 'number)
 nil)

(my-assert
 (subtypep 'number 'character)
 nil)

(my-assert
 (subtypep 'string 'number)
 nil)

(my-assert
 (subtypep 'complex 'number)
 t)

(my-assert
 (subtypep 'float 'number)
 t)

(my-assert
 (subtypep 'fixnum 'number)
 t)

(my-assert
 (subtypep 'rational 'number)
 t)

(my-assert
 (subtypep 'float 'complex)
 nil)

(my-assert
 (subtypep 'integer 'rational)
 t)

(my-assert
 (subtypep 'number 'vector)
 nil)

(my-assert
 (subtypep 'vector 'array)
 t)

(my-assert
 (subtypep 'number 'array)
 nil)

(my-assert
 (null 'nil)
 t)

(my-assert
 (symbolp *standard-input*)
 nil)

(my-assert
 (symbolp 'car)
 t)

(my-assert
 (atom 'abc)
 t)

(my-assert
 (consp (acons 'x 'y 'a))
 #+xcl error
 #+(or clisp akcl allegro cmu sbcl sbcl ecls) t
 #-(or xcl clisp akcl allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 (listp (list (append (cons 'a 'b) 'c)))
 t)

(my-assert
 (listp 'a)
 nil)

(my-assert
 (listp nil)
 t)

(my-assert
 (listp '(a b c))
 t)

(my-assert
 (numberp #*101)
 nil)

(my-assert
 (numberp -5)
 t)

(my-assert
 (integerp 5)
 t)

(my-assert
 (integerp #\+)
 nil)

(my-assert
 (rationalp 0)
 t)

(my-assert
 (floatp -5)
 nil)

(my-assert
 (floatp (read-from-string "1.0e30"))
 t)

(my-assert
 (floatp 123.4)
 t)

(my-assert
 (complexp 1/2)
 nil)

(my-assert
 (complexp #c(2 3))
 t)

(my-assert
 (characterp #\1)
 t)

(my-assert
 (stringp "abc")
 t)

(my-assert
 (stringp :+*/-)
 nil)

(my-assert
 (bit-vector-p (read-from-string "#5*01110"))
 t)

(my-assert
 (vectorp "abc")
 t)

(my-assert
 (simple-vector-p #*101)
 nil)

(my-assert
 (simple-string-p "abc")
 t)

(my-assert
 (simple-string-p :+*/-)
 nil)

(my-assert
 (simple-bit-vector-p #*101)
 t)

(my-assert
 (arrayp (read-from-string "#7(2 4 3)"))
 t)

(my-assert
 (arrayp '(read-from-string "#1a 5.77"))
 nil)

(my-assert
 (packagep (read-from-string "#5*01110"))
 nil)

(my-assert
 (packagep *package*)
 t)

(my-assert
 (functionp 'atom)
 #-(or cltl2 clisp) t
 #+(or cltl2 clisp) nil)

(my-assert
 (compiled-function-p 'do)
 nil)

;; commonp

(my-assert
 (eq (list 1 2 3 4 5)
     (copy-list (list 1 2 3 4 5)))
 nil)

(my-assert
 (setq x (list (cons 1 'a) (cons 2 'b) (cons 3 'c)) )
 ((1 . a) (2 . b) (3 . c)))

(my-assert
 (eq (cadr x) (cadr (copy-alist x)))
 nil)

(unintern 'x)

(my-assert
 (eq #\a #\a)
 t)

(my-assert
 (booleanp (eq "Foo" "Foo"))
 t)

(my-assert
 (eq "Foo" (copy-seq "Foo"))
 nil)

(my-assert
 (eql #c(3.0 -4.0) #c(3 -4))
 nil)

(my-assert
 (eql (cons 'a 'b) (cons 'a 'c))
 nil)

(my-assert
 (equal (list 1 2 3 4 5) (copy-list (list 1 2 3 4 5)))
 t)

(my-assert
 (equal x (copy-alist x))
 t)

(my-assert
 (equal 3 3)
 t)

(my-assert
 (equal 3 3.0)
 nil)

(my-assert
 (equal 3.0 3.0)
 t)

(my-assert
 (equal #c(3 -4) #c(3 -4))
 t)

(my-assert
 (equalp (list 1 2 3 4 5) (copy-list (list 1 2 3 4 5)))
 t)

(my-assert
 (equalp "            foo" "            FOO")
 t)

(my-assert
 (equalp "            fou" "            FOO")
 nil)

(my-assert
 (not 1)
 nil)

(my-assert
 (not nil)
 t)

(my-assert
 (and (eq 1 2) (eq 2 3) (eq 3 4) (eq 4 4))
 nil)

(my-assert
 (and (eq 1 2) (eq 3 3) (eq 3 4) (eq 4 4))
 nil)

(my-assert
 (or (eq 2 2) (eq 3 3) (eq 3 4) (eq 4 4))
 t)

(my-assert
 (or (eq 1 2) (eq 2 3) (eq 3 4) (eq 4 5))
 nil)

;; kap 7 kontrollstructuren
;; ----------------------------------------------------------------------------

;;  quote, function, symbol-value, symbol-function, boundp, fboundp,
;;  special-form-p, setq, psetq, set, makunbound, fmakunbound,

(my-assert
 (setq li1 (list 'a (list 'b)
		 (list (list 'c)
		       (list 'd))))
 (a (b) ((c) (d))))

(my-assert
 (setq vec1 (vector 0 1 2 3))
 #(0 1 2 3))

(my-assert
 (setf (nth 1 li1) 'uu)
 uu)

(my-assert
 (eval 'li1)
 (a uu ((c) (d))))

(my-assert
 (setf (elt li1 1) 'oo)
 oo)

(my-assert
 (setf (elt vec1 1) 'oo)
 oo)

(my-assert
 (eval 'li1)
 (a oo ((c) (d))))

(my-assert
 (eval 'vec1)
 #(0 oo 2 3))

(my-assert
 (setf (rest li1) '((ww)))
 ((ww)))

(my-assert
 (eval 'li1)
 (a (ww)))

(my-assert
 (setf (first li1) 'aa)
 aa)

(my-assert
 (first li1)
 aa)

(my-assert
 (setf (second li1) 'bb)
 bb)

(my-assert
 (eval 'li1)
 (aa bb))

(my-assert
 (setf (rest li1) (list 2 3 4 5 6 7 8 9 10))
 (2 3 4 5 6 7 8 9 10))

(my-assert
 (setf (second li1) 22)
 22)

(my-assert
 (eval 'li1)
 (aa 22 3 4 5 6 7 8 9 10))

(my-assert
 (setf (third li1) '33)
 33)

(my-assert
 (setf (fourth li1) '44)
 44)

(my-assert
 (setf (fifth li1) '55)
 55)

(my-assert
 (setf (sixth li1) '66)
 66)

(my-assert
 (setf (seventh li1) '77)
 77)

(my-assert
 (setf (eighth li1) '88)
 88)

(my-assert
 (setf (ninth li1) '99)
 99)

(my-assert
 (setf (tenth li1) '1010)
 1010)

(my-assert
 (eval 'li1)
 (aa 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (first li1) '(((a))))
 (((a))))

(my-assert
 (setf (caaar li1) 'uu)
 uu)

(my-assert
 (caaar li1)
 uu)

(my-assert
 (car li1)
 ((uu)))

(my-assert
 (setf (caar li1) 'oo)
 oo)

(my-assert
 (eval 'li1)
 ((oo) 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (car li1) 'ii)
 ii)

(my-assert
 (eval 'li1)
 (ii 22 33 44 55 66 77 88 99 1010))

(my-assert
 (setf (cdddr li1) 'pp)
 pp)

(my-assert
 (eval 'li1)
 (ii 22 33 . pp))

(my-assert
 (setf (caddr li1) '333)
 333)

(my-assert
 (eval 'li1)
 (ii 22 333 . pp))

(my-assert
 (setf (svref vec1 2) 'kk)
 kk)

(my-assert
 (eval 'vec1)
 #(0 oo kk 3))

(unintern 'vec1)
(unintern 'li1)

(my-assert
 (setf (get 'a 'b) 'uu)
 uu)

(my-assert
 (get 'a 'b)
 uu)

(my-assert
 (setf (getf
	(cadr
	 (setq xx
	       (list 'aaa
		     (list 'i1 'v1 'i2 'v2))))
	'i2)
       'v222)
 v222)

(my-assert
 (eval 'xx)
 (aaa (i1 v1 i2 v222)))

(my-assert
 (getf (cadr xx) 'i2)
 v222)

(my-assert
 (getf (cadr xx) 'i1)
 v1)

(unintern 'xx)

(my-assert
 (setf (documentation 'beispiel 'typ1) "doc 1")
 "doc 1")

(my-assert
 (setf (documentation 'beispiel 'typ2) "doc 2")
 "doc 2")

(my-assert
 (documentation 'beispiel 'typ2)
 #+xcl (typ2 . "doc 2")
 #-xcl "doc 2")

(my-assert
 (setf (documentation 'beispiel 'typ2) "doc 3")
 "doc 3")

(my-assert
 (documentation 'beispiel 'typ2)
 #+xcl (typ2 . "doc 3")
 #-xcl "doc 3")

(my-assert
 (symbol-plist 'beispiel)
 #+xcl (documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
 #+clisp (system::documentation-strings (typ2 "doc 3" typ1 "doc 1"))
 #+allegro (excl::%documentation ((typ2 . "doc 3") (typ1 . "doc 1")))
 #+(or cmu sbcl ecls) nil
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 (setf (symbol-value 'xx) 'voelligneu)
 voelligneu)

(my-assert
 (eval 'xx)
 voelligneu)

(unintern 'xx)

;; psetf, shiftf, rotatef, define-modify-macro, defsetf, define-setf-method,
;; get-setf-method, get-setf-method-multiple-value, apply, funcall, progn,
;; prog1, prog2,

(my-assert
 (let ((x (list 'a 'b 'c)))
   (rplacd (last x) x)
   (list-length x))
 nil)

;; let*, compiler-let, progv, flet, labels, macrolet, if, when, unless, cond,
;; case, typecase, block, loop, do, do*, dolist, dotimes,

(my-assert
 (mapcar (function (lambda (x) (list x))) (list 'a 'b 'c))
 ((a) (b) (c)))

(my-assert
 (mapc (function
	(lambda (x y z)
	  (list x y z)))
       (list 'a 'b 'c)
       (list 1 2 3)
       (list 'u 'i 'v))
 (a b c))

(my-assert
 (mapl (function (lambda (x y z) (list x y z))) (list 'a 'b 'c) (list 1 2 3)
       (list 'u 'i 'v))
 (a b c))

(my-assert
 (maplist (lambda (x y z) (list x y z))
	  (list 'a 'b 'c)
	  (list 1 2 3)
	  (list 'u 'i 'v))
 (((a b c) (1 2 3) (u i v)) ((b c) (2 3) (i v)) ((c) (3) (v))))

(my-assert
 (mapcon (lambda (x y z) (list x y z))
	 (list 'a 'b)
	 (list 1 2 3)
	 (list 'u 'i 'v))
 ((a b) (1 2 3) (u i v) (b) (2 3) (i v)))

(my-assert
 (mapcan (lambda (x y z) (list x y z))
	 (list 'a 'b 'c)
	 (list 1 2 3)
	 (list 'u 'i 'v))
 (a 1 u b 2 i c 3 v))

;; tagbody, go, multiple-value-list, multiple-value-call, multiple-value-prog1,
;; multiple-value-bind, multiple-value-setq, values, values-list, catch,

;; unwind-protect, throw,

;; kap 8 macros
;; ----------------------------------------------------------------------------

;; macro-function, defmacro, macroexpand, macroexpand-1,

;; kap 9 declarationen
;; ----------------------------------------------------------------------------

;; declare, locally, proclaim, the,

;; kap 10 symbole
;; ----------------------------------------------------------------------------

;; get, remprop, symbol-plist, getf, remf, get-properties, symbol-name,

;; make-symbol, copy-symbol, gensym, gentemp, symbol-package,

(my-assert
 (keywordp 36)
 nil)

(my-assert
 (keywordp :rename)
 t)

;; kap 11 pakete
;; ----------------------------------------------------------------------------

;; find-package, in-package, list-all-packages, make-package, package-name,
;; package-nicknames, package-shadowing-symbols, package-use-list,
;; package-used-by-list, rename-package, unuse-package, use-package, intern,
;; unintern, find-symbol, export, unexport, import, shadowing-import, shadow,
;; find-all-symbols, do-symbols, do-external-symbols, do-all-symbols,
;; provide, require,

;; kap 12 zahlen
;; ----------------------------------------------------------------------------

(my-assert
 (zerop -456)
 nil)

(my-assert
 (zerop 0)
 t)

(my-assert
 (plusp 3)
 t)

(my-assert
 (plusp 3453786543987565)
 t)

(my-assert
 (minusp -456)
 t)

(my-assert
 (oddp -1)
 t)

(my-assert
 (oddp 0)
 nil)

(my-assert
 (evenp -456)
 t)

(my-assert
 (evenp -345)
 nil)

(my-assert
 (= 5/2 2.5)
 t)

(my-assert
 (/= 3.0 3)
 nil)

(my-assert
 (/= 3.0 #c(3.0 1.0))
 t)

(my-assert
 (< 3.0 3)
 nil)

(my-assert
 (< 3 3.0 3 #c(3.0 0.0))
 #+(or allegro cmu sbcl sbcl) nil
 #-(or allegro cmu sbcl sbcl) error)

(my-assert
 (< -5 -4 -2 0 4 5)
 t)

(my-assert
 (> 8 7 6 5 4)
 t)

(my-assert
 (> 3 3.0 3 #c(3.0 0.0))
 #+(or allegro cmu sbcl sbcl) nil
 #-(or allegro cmu sbcl sbcl) error)

(my-assert
 (<= 3.0 3)
 t)

(my-assert
 (<= 3 3)
 t)

(my-assert
 (<= 1 3 3 2 5)
 nil)

(my-assert
 (<= 5/2 2.5)
 t)

(my-assert
 (>= -5 -4 -2 0 4 5)
 nil)

(my-assert
 (max 1 3 2 -7)
 3)

;; min,

(my-assert
 (+ 1 1/2 0.5 #c(3.0 5.5))
 #c(5.0 5.5))

(my-assert
 (- 3 0 3 5 -6)
 1)

(my-assert
 (- #c(0 6) 1/4 0.5 7)
 #c(-7.75 6.0))

(my-assert
 (* 7 6 5 4 3 2 1)
 5040)

(my-assert
 (* 2 2 2.0 2)
 16.0)

(my-assert
 (/ -8)
 -1/8)

(my-assert
 (/ 4 2)
 2)

(my-assert
 (1+ 0)
 1)

(my-assert
 (1+ #c(0 1))
 #c(1 1))

(my-assert
 (1- 5.0)
 4.0)

;; incf, decf,

(my-assert
 (conjugate #c(3/5 4/5))
 #c(3/5 -4/5))

(my-assert
 (gcd 91 -49)
 7)

(my-assert
 (lcm 14 35)
 70)

(my-assert
 (prin1-to-string (exp 1) )
 "2.7182817")				; "2.718282"

(my-assert
 (expt #c(0 1) 2)
 -1)

(my-assert
 (prin1-to-string (expt 2 #c(0 1)) )
 "#C(0.7692389 0.63896126)")		; "#C(0.7692389 0.6389612)"

(my-assert
 (prin1-to-string (log -3 10) )
 "#C(0.47712126 1.3643764)")		; "#C(0.4771213 1.364376)"

(my-assert
 (log 3 0)
 #+(or xcl cmu sbcl sbcl) 0
 #+allegro 0.0
 #-(or xcl allegro cmu sbcl sbcl) error)

(my-assert
 (sqrt 9)
 3.0)

(my-assert
 (sqrt -9.0)
 #c(0.0 3.0))

(my-assert
 (isqrt 9)
 3)

(my-assert
 (isqrt 26)
 5)

(my-assert
 (abs 6)
 6)

(my-assert
 (abs -6)
 6)

;; phase,

(my-assert
 (signum 0)
 0)

(my-assert
 (signum -4)
 -1)

(my-assert
 (signum 4)
 1)

(my-assert
 (prin1-to-string (sin (* 8 (/ pi 2))) )
 #+xcl "-4.576950980887866D-17"
 #+clisp "2.0066230454737344098L-19"
 #+akcl "-4.898425415289509E-16"
 #+(or allegro cmu sbcl sbcl ecls) "-4.898425415289509d-16"
 #-(or xcl clisp akcl allegro cmu sbcl  ecls) unknown)

(my-assert
 (prin1-to-string (sin (expt 10 3)) )
 "0.82687956")				; "0.8268796"

(my-assert
 (cos 0)
 1.0)

(my-assert
 (prin1-to-string (cos (/ pi 2)) )
 #+xcl "5.721188726109832D-18"
 #+clisp "-2.5082788076048218878L-20"
 #+akcl "6.1230317691118863E-17"
 #+(or allegro cmu sbcl sbcl ecls) "6.123031769111886d-17"
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string (tan 1) )
 "1.5574077")				; "1.557408"

(my-assert
 (prin1-to-string (tan (/ pi 2)) )
 #+xcl "1.747888503373944D17"
 #+clisp "-3.986797629004264116L19"
 #+akcl "1.6331778728383844E16"
 #+ecls "1.6331778728383844d16"
 #+(or allegro cmu sbcl sbcl) "1.6331778728383844d+16"
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string (cis -1) )
 "#C(0.5403023 -0.84147096)")		; "#C(0.5403023 -0.8414709)"

(my-assert
 (cis 2.5)
 #c(-0.8011436 0.5984721))

(my-assert
 (prin1-to-string (asin -1) )
 "-1.5707964")				; "-1.570796"

(my-assert
 (asin 0)
 0.0)

(my-assert
 (asin 2)
 #+(or cmu sbcl sbcll)
 #c(1.5707964 -1.3169578)
 #-(or cmu sbcl sbcll)
 #c(1.5707964 -1.316958))

(my-assert
 (prin1-to-string (acos 0) )
 "1.5707964")				; "1.570796"

(my-assert
 (prin1-to-string (acos -1) )
 "3.1415927")				; "3.141593"

(my-assert
 (prin1-to-string (acos 2) )
 #+xcl
 "#C(0.0 1.316958)"
 #+clisp
 "#C(0 1.316958)"
 #+allegro
 "#c(0.0 1.316958)"
 #+(or cmu sbcl sbcl ecls)
 "#C(0.0 1.3169578)"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (acos 1.00001)
 #+ganz-korrekt
 #c(0.0 0.0044721322)
 #+xcl
 #c(0.0 0.004475157)
 #+clisp-korrekt
 #c(0.0 0.0044751678)	; da schon 1.00001 gerundet wurde
 #+clisp
 #c(0.0 0.0044751023)	; i * ln(x+sqrt(x^2-1))
 #+clisp-anders
 #c(0.0 0.0044752206)	; i * ln(x+sqrt((x-1)*(x+1)))
 #+allegro
 #c(0.0 0.004475168)
 #+(or cmu sbcl sbcll)
 #c(0.0 0.0044751678)
 #-(or xcl clisp allegro cmu sbcl sbcl)
 #c(0.0 0.0044721322))

(my-assert
 (atan 1)
 #+(or xcl allegro cmu sbcl sbcl ecls) 0.7853982
 #+clisp 0.7853981
 #-(or xcl allegro clisp cmu sbcl sbcl ecls) unknown)

(my-assert
 (prin1-to-string pi )
 #+xcl "3.141592653589793D0"
 #+clisp "3.1415926535897932385L0"
 #+(or allegro cmu sbcl sbcl ecls) "3.141592653589793d0"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (sinh 0)
 0.0)

(my-assert
 (prin1-to-string (sinh #c(5.0 -9.6)) )
 #+(or cmu sbcl sbcll)
 "#C(-73.06699 12.936809)"
 #-(or cmu sbcl sbcll)
 "#C(-73.06699 12.93681)")

(my-assert
 (cosh 0)
 1.0)

(my-assert
 (prin1-to-string (cosh 1) )
 #+(or cmu sbcl sbcll) "1.5430807"	; round-off error
 #-(or cmu sbcl sbcll) "1.5430806")	; "1.543081"

(my-assert
 (tanh 50)
 1.0)

(my-assert
 (prin1-to-string (tanh 0.00753) )
 #-allegro "0.0075298576"
 #+allegro "0.0075298795")		; "0.007529857"

(my-assert
 (prin1-to-string (asinh 0.5) )
 #-(or allegro cmu sbcl sbcl) "0.48121184"
 #+(or allegro cmu sbcl sbcl) "0.4812118") ; "0.4812118"

(my-assert
 (prin1-to-string (asinh 3/7) )
 #-(or clisp allegro cmu sbcl sbcl) "0.4164308"
 #+clisp "0.4164307"			; rundungsfehler
 #+(or allegro cmu sbcl sbcl) "0.41643077")

(my-assert
 (acosh 0)
 #c(0 1.5707964))

(my-assert
 (acosh 1)
 0)

(my-assert
 (acosh -1)
 #c(0 3.1415927))

(my-assert
 (prin1-to-string (atanh 0.5) )
 "0.54930615")				; "0.5493061"

(my-assert
 (prin1-to-string (atanh 3/7) )
 #-(or clisp allegro cmu sbcl sbcl) "0.4581454"
 #+clisp "0.4581453"			; rundungsfehler
 #+(or allegro cmu sbcl sbcl) "0.45814538")

(my-assert
 (= (sin (* #c(0 1) 5)) (* #c(0 1) (sinh 5)))
 t)

(my-assert
 (= (cos (* #c(0 1) 5)) (cosh 5))
 t)

(my-assert
 (= (tan (* #c(0 1) 5)) (* #c(0 1) (tanh 5)))
 t)

(my-assert
 (= (sinh (* #c(0 1) 5)) (* #c(0 1) (sin 5)))
 t)

(my-assert
 (= (cosh (* #c(0 1) 5)) (cos 5))
 t)

(my-assert
 (float 1)
 1.0)

(my-assert
 (float 0.5)
 0.5)

(my-assert
 (rational 2)
 2)

(my-assert
 (rational 2.0)
 2)

(my-assert
 (rational 2.5)
 5/2)

(my-assert
 (rationalize 2.5)
 5/2)

(my-assert
 (rationalize 7/3)
 7/3)

(my-assert
 (rationalize pi)
 #+xcl 28296953155597409/9007199254740992
 #+clisp 8717442233/2774848045
 #+ecls 884279719003555/281474976710656
 #+(or allegro cmu sbcl sbcl) 245850922/78256779
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 (numerator 5/2)
 5)

(my-assert
 (numerator (/ 8 -6))
 -4)

(my-assert
 (denominator 5/2)
 2)

(my-assert
 (denominator (/ 8 -6))
 3)

(my-assert
 (gcd (numerator 7/9) (denominator 7/9))
 1)

(my-assert
 (floor 2.6)
 2)

(my-assert
 (floor 2.5)
 2)

(my-assert
 (ceiling 2.6)
 3)

(my-assert
 (ceiling 2.5)
 3)

(my-assert
 (ceiling 2.4)
 3)

(my-assert
 (truncate 2.6)
 2)

(my-assert
 (truncate 2.5)
 2)

(my-assert
 (truncate 2.4)
 2)

(my-assert
 (round 2.6)
 3)

(my-assert
 (round 2.5)
 2)

(my-assert
 (round 2.4)
 2)

(my-assert
 (mod 13 4)
 1)

(my-assert
 (mod -13 4)
 3)

(my-assert
 (prin1-to-string (rem 13.4 1) )
 #-(or clisp allegro cmu sbcl sbcl) "0.4" ;
 #+xcl "0.3999996"
 #+(or clisp allegro cmu sbcl sbcl) "0.39999962") ; rundungsfehler

(my-assert
 (ffloor 2.6)
 2)

(my-assert
 (ffloor 2.5)
 2)

(my-assert
 (ffloor 2.4)
 2)

(my-assert
 (fceiling -0.3)
 0)

(my-assert
 (fceiling -0.7)
 0)

(my-assert
 (fceiling -2.4)
 -2)

(my-assert
 (ftruncate 2.5)
 2.0)

(my-assert
 (ftruncate 2.4)
 2.0)

(my-assert
 (fround -0.7)
 -1.0)

(my-assert
 (fround -2.4)
 -2.0)

(my-assert
 (decode-float 35.0)
 0.546875)

(my-assert
 (decode-float 3.5s0)
 0.875s0)

(my-assert
 (scale-float 2.5 5)
 80.0)

(my-assert
 (scale-float 0.7541 2)
 3.0164)

(my-assert
 (float-radix 2.5)
 2)

(my-assert
 (float-radix 3.5d0)
 2)

;; float-digits, float-precision, float-sign, integer-decode-float,

(my-assert
 (complex 1/4 7.3)
 #c(0.25 7.3))

(my-assert
 (complex 1 0)
 1)

(my-assert
 (realpart 5)
 5)

(my-assert
 (realpart #c(1.4 0.0))
 1.4)

(my-assert
 (imagpart 5)
 0)

(my-assert
 (imagpart #c(1.4 0.0))
 0.0)

;; logand, logandc1, logandc2, logeqv, logior, lognand, lognor, lognot,
;; logorc1, logorc2, logtest, logxor, logbitp, ash,

(my-assert
 (logcount 13)
 3)

(my-assert
 (logcount -13)
 2)

(my-assert
 (integer-length 0)
 0)

(my-assert
 (integer-length 1)
 1)

;; byte, byte-position, byte-size, ldb, ldb-test, mask-field, dpb, deposit-field,

;; random,

#+xcl
(my-assert
 (random-state-p
  (eval (read-from-string "(sys::%set-type-pointer sys::%type-random-state 1)")))
 t)

;; make-random-state,

(my-assert
 boole-clr
 0)

(my-assert
 boole-set
 #+(or xcl allegro cmu sbcl sbcl) 1
 #+(or clisp ecls) 15
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-1
 #+(or xcl allegro cmu sbcl sbcl) 2
 #+clisp 10
 #+ecls 3
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-2
 #+(or xcl allegro cmu sbcl sbcl) 3
 #+clisp 12
 #+ecls 5
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-c1
 #+(or xcl allegro cmu sbcl sbcl) 4
 #+clisp 5
 #+ecls 12
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-c2
 #+(or xcl allegro cmu sbcl sbcl) 5
 #+clisp 3
 #+ecls 10
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-and
 #+(or xcl allegro cmu sbcl sbcl) 6
 #+clisp 8
 #+ecls 1
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-ior
 #+(or xcl allegro cmu sbcl sbcl) 7
 #+clisp 14
 #+ecls 7
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-xor
 #+(or xcl allegro cmu sbcl sbcl) 8
 #+(or clisp ecls) 6
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-eqv
 #+(or xcl allegro cmu sbcl sbcl) 9
 #+(or clisp ecls) 9
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-nand
 #+(or xcl allegro cmu sbcl sbcl) 10
 #+clisp 7
 #+ecls 14
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-nor
 #+(or xcl allegro cmu sbcl sbcl) 11
 #+clisp 1
 #+ecls 8
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 boole-andc1
 #+(or xcl allegro cmu sbcl sbcl) 12
 #+(or clisp ecls) 4
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-andc2
 #+(or xcl allegro cmu sbcl sbcl) 13
 #+(or clisp ecls) 2
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-orc1
 #+(or xcl allegro cmu sbcl sbcl) 14
 #+(or clisp ecls) 13
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 boole-orc2
 #+(or xcl allegro cmu sbcl sbcl) 15
 #+(or clisp ecls) 11
 #-(or xcl clisp allegro cmu sbcl sbcl ecls) unknown)

(my-assert
 (let ((s (prin1-to-string most-positive-fixnum )))
   (or #+(or xcl clisp) (equal s "16777215")
       #+clisp (equal s "33554431")
       #+clisp (equal s "67108863")
       #+clisp (equal s "4294967295")
       #+(or allegro cmu sbcl sbcl) (equal s "536870911")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string most-negative-fixnum )))
   (or #+(or xcl clisp) (equal s "-16777216")
       #+clisp (equal s "-33554432")
       #+clisp (equal s "-67108864")
       #+clisp (equal s "-4294967296")
       #+(or allegro cmu sbcl) (equal s "-536870912")
       ) )
 t)

(my-assert
 (prin1-to-string most-positive-short-float )
 #+xcl "1.701S38"
 #+clisp "1.7014s38"
 #+allegro "3.4028232e+38"
 #+(or cmu sbcl sbcll) "3.4028235e+38"
 #+ecls "3.4028235e38"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string least-positive-short-float )
 #+xcl "2.939S-39"
 #+clisp "2.93874s-39"
 #+(or allegro cmu sbcl sbcl) "1.4012985e-45"
 #+ecls "1.401298E-45"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string least-negative-short-float )
 #+xcl "-2.939S-39"
 #+clisp "-2.93874s-39"
 #+(or allegro cmu sbcl sbcl) "-1.4012985e-45"
 #+ecls "-1.401298E-45"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string most-negative-short-float )
 #+xcl "-1.701S38"
 #+clisp "-1.7014s38"
 #+allegro "-3.4028232e+38"
 #+(or cmu sbcl sbcll) "-3.4028235e+38"
 #+ecls "-3.402823E38"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (let ((s (prin1-to-string most-positive-single-float )))
   (or #+xcl (equal s "1.701411E38")
       #+clisp (equal s "1.7014117E38")
       #+clisp (equal s "3.4028235E38")
       #+allegro (equal s "3.4028232e+38")
       #+(or cmu sbcl sbcll) (equal s "3.4028235e+38")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string least-positive-single-float )))
   (or #+(or xcl clisp) (equal s "2.938736E-39")
       #+clisp (equal s "1.1754944E-38")
       #+(or allegro cmu sbcl sbcl) (equal s "1.4012985e-45")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string least-negative-single-float )))
   (or #+(or xcl clisp) (equal s "-2.938736E-39")
       #+clisp (equal s "-1.1754944E-38")
       #+(or allegro cmu sbcl sbcl) (equal s "-1.4012985e-45")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string most-negative-single-float )))
   (or #+xcl (equal s "-1.701411E38")
       #+clisp (equal s "-1.7014117E38")
       #+clisp (equal s "-3.4028235E38")
       #+allegro (equal s "-3.4028232e+38")
       #+(or cmu sbcl sbcll) (equal s "-3.4028235e+38")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string most-positive-double-float )))
   (or #+xcl (equal s "1.701411834604692D38")
       #+clisp (equal s "8.988465674311579d307")
       #+clisp (equal s "1.7976931348623157d308")
       #+allegro (equal s "4.494232837155787d+307")
       #+(or cmu sbcl sbcll) (equal s "1.7976931348623157d+308")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string least-positive-double-float )))
   (or #+xcl (equal s "2.938735877055719D-39")
       #+clisp (equal s "5.562684646268004d-309")
       #+clisp (equal s "2.2250738585072014d-308")
       #+allegro (equal s "4.9406564584124657d-324")
       #+(or cmu sbcl sbcll) (equal s "4.940656458412465d-324")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string least-negative-double-float )))
   (or #+xcl (equal s "-2.938735877055719D-39")
       #+clisp (equal s "-5.562684646268004d-309")
       #+clisp (equal s "-2.2250738585072014d-308")
       #+allegro (equal s "-4.9406564584124657d-324")
       #+(or cmu sbcl sbcll) (equal s "-4.940656458412465d-324")
       ) )
 t)

(my-assert
 (let ((s (prin1-to-string most-negative-double-float )))
   (or #+xcl (equal s "-1.701411834604692D38")
       #+clisp (equal s "-8.988465674311579d307")
       #+clisp (equal s "-1.7976931348623157d308")
       #+allegro (equal s "-4.494232837155787d+307")
       #+(or cmu sbcl sbcll) (equal s "-1.7976931348623157d+308")
       ) )
 t)

(my-assert
 (prin1-to-string most-positive-long-float )
 #+xcl "1.701411834604692D38"
 #+clisp "8.8080652584198167656L646456992"
 #+allegro "4.494232837155787d+307"
 #+(or cmu sbcl sbcll) "1.7976931348623157d+308"
 #+ecls "1.797693134862316d308"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string least-positive-long-float )
 #+xcl "2.938735877055719D-39"
 #+clisp "5.676615526003731344L-646456994"
 #+allegro "4.9406564584124657d-324"
 #+(or cmu sbcl ecls) "4.940656458412465d-324"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string least-negative-long-float )
 #+xcl "-2.938735877055719D-39"
 #+clisp "-5.676615526003731344L-646456994"
 #+allegro "-4.9406564584124657d-324"
 #+(or cmu sbcl ecls) "-4.940656458412465d-324"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string most-negative-long-float )
 #+xcl "-1.701411834604692D38"
 #+clisp "-8.8080652584198167656L646456992"
 #+allegro "-4.494232837155787d+307"
 #+(or cmu sbcl sbcll) "-1.7976931348623157d+308"
 #+ecls "-1.797693134862316d308"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string short-float-epsilon )
 #+xcl "1.526S-5"
 #+clisp "7.6295s-6"
 #+allegro "1.1920929e-7"
 #+(or cmu sbcl sbcll) "5.960465e-8"
 #+ecls "6.258487E-8"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string single-float-epsilon )
 #+xcl "5.960464E-8"
 #+clisp "5.960465E-8"
 #+allegro "1.1920929e-7"
 #+(or cmu sbcl sbcll) "5.960465e-8"
 #+ecls "6.258487E-8"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string double-float-epsilon )
 #+xcl "1.387778780781446D-17"
 #+(or clisp cmu sbcl sbcl) "1.1102230246251568d-16"
 #+allegro "2.220446049250313d-16"
 #+ecls "1.165734175856414d-16"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string long-float-epsilon )
 #+xcl "1.387778780781446D-17"
 #+clisp "5.4210108624275221706L-20"
 #+allegro "2.220446049250313d-16"
 #+(or cmu sbcl sbcll) "1.1102230246251568d-16"
 #+ecls "1.165734175856414d-16"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string short-float-negative-epsilon )
 #+xcl "1.526S-5"
 #+clisp "3.81476s-6"
 #+allegro "1.1920929e-7"
 #+(or cmu sbcl sbcll) "2.9802325e-8"
 #+ecls "3.129244E-8"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string single-float-negative-epsilon )
 #+xcl "5.960464E-8"
 #+clisp "2.9802326E-8"
 #+allegro "1.1920929e-7"
 #+(or cmu sbcl sbcll) "2.9802325e-8"
 #+ecls "3.129244E-8"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string double-float-negative-epsilon )
 #+xcl "1.387778780781446D-17"
 #+(or clisp cmu sbcl sbcl) "5.551115123125784d-17"
 #+allegro "2.220446049250313d-16"
 #+ecls "5.828670879282072d-17"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (prin1-to-string long-float-negative-epsilon )
 #+xcl "1.387778780781446D-17"
 #+clisp "2.7105054312137610853L-20"
 #+allegro "2.220446049250313d-16"
 #+(or cmu sbcl sbcll) "5.551115123125784d-17"
 #+ecls "5.828670879282072d-17"
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (/ 1 0)
 error)

(my-assert
 (/ 1 0.0s0)
 error)

(my-assert
 (/ 1 0.0f0)
 error)

(my-assert
 (/ 1 0.0d0)
 error)

(my-assert
 (/ 1 0.0l0)
 error)

(my-assert
 (expt 10.0s0 1000)
 error)

(my-assert
 (expt 10.0f0 1000)
 error)

(my-assert
 (expt 10.0d0 1000)
 error)

(my-assert
 (expt 10.0l0 1000000000)
 error)

;; kap 13 zeichen
;; ----------------------------------------------------------------------------

(my-assert
 (standard-char-p #\a)
 t)

(my-assert
 (standard-char-p 1)
 error)

(my-assert
 (graphic-char-p #\a)
 t)

(my-assert
 (graphic-char-p 1)
 error)

(my-assert
 (characterp
    #\a)
 t)

(my-assert
 (characterp
    #\1)
 t)

(my-assert
 (alpha-char-p #\a)
 t)

(my-assert
 (alpha-char-p #\$)
 nil)

(my-assert
 (upper-case-p #\a)
 nil)

(my-assert
 (lower-case-p #\A)
 nil)

(my-assert
 (both-case-p #\a)
 t)

(my-assert
 (both-case-p #\$)
 nil)

(my-assert
 (digit-char-p #\a)
 nil)

(my-assert
 (digit-char-p #\5)
 5)

(my-assert
 (alphanumericp #\a)
 t)

(my-assert
 (alphanumericp #\$)
 nil)

(my-assert
 (char= #\d #\d)
 t)

(my-assert
 (char/= #\d #\d)
 nil)

(my-assert
 (char< #\z #\0)
 nil)

;; char>, char>=, char<=,

(my-assert
 (char-equal #\d #\d)
 t)

(my-assert
 (char-not-equal #\d #\d)
 nil)

(my-assert
 (char-lessp #\d #\x)
 t)

(my-assert
 (char-lessp #\d #\d)
 nil)

(my-assert
 (char-not-greaterp #\d #\d)
 t)

(my-assert
 (char-greaterp #\e #\d)
 t)

(my-assert
 (char-not-lessp #\e #\d)
 t)

;; char-code, code-char, character,

(my-assert
 (char-upcase #\a)
 #\a)

(my-assert
 (char-upcase #\=)
 #\=)

(my-assert
 (char= (char-downcase (char-upcase #\x)) #\x)
 t)

(my-assert
 (char-downcase #\a)
 #\a)

(my-assert
 (char= (char-upcase (char-downcase #\X)) #\X)
 t)

(my-assert
 (digit-char 7)
 #\7)

(my-assert
 (digit-char 12)
 nil)

;; char-int, int-char, char-name, name-char,

(my-assert
 char-code-limit
 #+xcl 128
 #+(or (and clisp (not unicode)) akcl sbcl cmu ecls) 256
 #+(or (and clisp unicode) allegro) 655366
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

;; kap 14 sequenzen
;; ----------------------------------------------------------------------------

(my-assert
 (elt (symbol-name 'abc) 0)
 #\a)

(my-assert
 (subseq (list 'a 'b 'c 'd 'e) 2)
 (c d e))

(my-assert
 (copy-seq '#(a b c))
 #(a b c))

(my-assert
 (copy-seq (list (list 'a 'b) 'c (list 'd 'e)))
 ((a b) c (d e)))

(my-assert
 (length #(a b c d e f))
 6)

(my-assert
 (length (list 'a 'b 'c 'd 'e 'f))
 6)

(my-assert
 (nreverse (list 'a
		 (list 'b
		       (list 'c)
		       'd)))
 ((b (c) d) a))

(my-assert
 (reverse (list 1 2 3 4))
 (4 3 2 1))

(my-assert
 (make-sequence 'vector 4 :initial-element 'o)
 #(o o o o))

(my-assert
 (make-sequence 'list 4 :initial-element 'o)
 (o o o o))

(my-assert
 (concatenate 'list (list 'a 'b 'c) (list 1 2))
 (a b c 1 2))

(my-assert
 (map 'list 'list
      (list #\a #\b #\c)
      (list #\1 #\2 #\3))
 ((#\a #\1) (#\b #\2) (#\c #\3)))

(my-assert
 (map 'list 'list (list 'a 'b 'c) (list 1 2 3))
 ((a 1) (b 2) (c 3)))

(my-assert
 (some 'null (list 'a 'b nil 't 'e))
 t)

(my-assert
 (every 'atom (list 'a 8 #(a b)))
 t)

(my-assert
 (notany 'eq
	 (list 'a 'b 'c 'd 'e 4)
	 (list 'i 'j 'k 'l 'm 4))
 nil)					;? t

(my-assert
 (notevery 'eq '#(u)
	   (list 'a 'x 'u))
 t)

(my-assert
 (reduce 'list '(a) :from-end nil :initial-value nil)

 (nil a))

(my-assert
 (reduce 'list
	 (list 'a 'b 'c 'd)
	 :from-end nil
	 :initial-value 'iii)
 ((((iii a) b) c) d))

(my-assert
 (reduce 'list (list 'a 'b 'c 'd) :from-end t)
 (a (b (c d))))

(my-assert
 (fill '#(a b c d) 'i :start 1 :end 3)
 #(a i i d))

(my-assert
 (replace '#(a b c d) '#(i j) :start1 1)
 #(a i j d))

(my-assert
 (remove 'numberp '#(y a 4 a c 9 a d 2 3)
	 :count 1 :from-end t)
 #(y a 4 a c 9 a d 2 3))

(my-assert
 (remove 'a
	 (list 'a 1 'b 'a '2 'a)
	 :start 1)
 (a 1 b 2))

(my-assert
 (remove-duplicates (list 'a 'b 'c 'a 'd 'a)
		    :start 1)
 (a b c d a))

(my-assert
 (remove-if 'numberp '#(y a 4 a c 9 a d 2 3))
 #(y a a c a d))

(my-assert
 (remove-if-not 'numberp #(y a 4 a c 9 a d 2 3))
 #(4 9 2 3))

(my-assert
 (remove-if-not 'numberp #(y a 4 a c 9 a d 2 3)
		:count 2 :from-end nil)
 #(4 a c 9 a d 2 3))

(my-assert
 (delete '(a) (list (list 'a 'b) (list 'c 'd) (list 'a))
	 :test 'equal)
 ((a b) (c d)))

(my-assert
 (delete-if (lambda (x) (eq (car x) 'a))
	    (list (list 'a 'b)
		  (list 'c 'd)
		  (list 'a)))
 ((c d)))

(my-assert
 (delete-if-not 'numberp (list 'a 3 'b 4))
 (3 4))

;; delete-duplicates,

(my-assert
 (nsubstitute 'new (list 1 'old)
	      (list (list 0 'old) (list 1 'old) (list 2 'old))
	      :test-not 'equal
	      :from-end t)
 (new (1 old) new))

(my-assert
 (nsubstitute 'new 'old (list 0 'old 1 'old 2 'old) :end 2)
 (0 new 1 old 2 old))

(my-assert
 (nsubstitute-if 'new 'numberp (list 0 'a 1 'b 2 'c 3 'd)
		 :count 2
		 :end 5)
 (new a new b 2 c 3 d))

(my-assert
 (nsubstitute-if-not 'new 'numberp
		     (list 0 'a 1 'b 2 'c 3 'd)
		     :count 2
		     :from-end t)
 (0 a 1 b 2 new 3 new))

(my-assert
 (substitute 'new (list 2 'old)
	     (list (list 1 'old) (list 2 'old) (list 3 'old) (list 4 'old))
	     :test 'equal
	     :start 3)
 ((1 old) (2 old) (3 old) (4 old)))

(my-assert
 (substitute-if 'new 'numberp
		(list 'a 1 'b 2 'd 3))
 (a new b new d new))

(my-assert
 (substitute-if-not 'new 'numberp (list 'a 1 'b 2 'd 3)
		    :count 2
		    :from-end t)
 (a 1 new 2 new 3))

(my-assert
 (find '0 (list (list 0 'a) (list 1 'a) (list 2 'a) (list 0 'b))
       :test '=
       :from-end t
       :key 'car
       :start 1)
 (0 b))

(my-assert
 (find-if 'numberp (list (list 'a 0) (list 'b 1) (list 'c 2))
	  :key 'cadr
	  :start 3)
 nil)

;; find-if-not,

(my-assert
 (position 'a (list (list 0 'a) (list 1 'b) (list 2 'a) (list 3 'b))
	   :test #'(lambda (x y) (eq x (cadr y)))
	   :start 1)
 2)

(my-assert
 (position 'a
	   (list (list 0 'a) (list 1 'b) (list 2 'a) (list 3 'b))
	   :key 'cadr)

 0)

(my-assert
 (position-if 'numberp
	      (list (list 0 'x) (list 1 7.0) (list 2 8))
	      :from-end t
	      :start 1
	      :key 'cadr)
 2)

;; position-if-not,

(my-assert
 (count '(a)
	(list 'a (list 'a) 'a (list 'a) 'a 'b)
	:test-not 'equal
	:key (lambda (x)
	       (when (atom x)
		 (list x))))
 3)

(my-assert
 (count-if-not 'numberp '#(a 3 b 5 7 c d) :start 2 :end 5)
 1)

;; count-if-not,

(my-assert
 (mismatch (list 'a 'b 'c 3 4 5)
	   (list 'a 'b 'x 3 4 'b)
	   :start1 1
	   :start2 5
	   :end1 2
	   :test-not 'eq)
 1)

(my-assert
 (mismatch (list 'a 'b 'c 3 4 5)
	   (list 'u 'b 'x 3 4 5)
	   :from-end t)
 #+xcl 2
 #-xcl 3)

(my-assert
 (search "ABCD" "0ABIABJBCBC"
	 :end1 3
	 :start1 1
	 :start2 0
	 :from-end t)
 9)

(my-assert
 (search (list #\A #\B #\C #\D)
	 "0ABIABJBCBC"
	 :end1 2
	 :start2 0
	 :from-end t)
 4)

(my-assert
 (search (list 'a 'b 'c 'd)
	 (list 0 'a 'b 'i 'a 'b 'j 'b 'c 'b 'c)
	 :end1 2
	 :start2 2)
 4)

(my-assert
 (sort (list (list 'u 3) (list 'i 1)
	     (list 'a 7) (list 'k 3)
	     (list 'c 4) (list 'b 6))
       '<
       :key 'cadr)
 ((i 1) (u 3) (k 3) (c 4) (b 6) (a 7)))

(my-assert
 (stable-sort (list (list 'b 4) (list 'a 3)
		    (list 'a 2) (list 'b 1)
		    (list 'c 9) (list 'b 2))
	      'string<
	      :key  'car)
 ((a 3) (a 2) (b 4) (b 1) (b 2) (c 9)))

(my-assert
 (merge 'list
	(list 5 1 4 4 7)
	(list 2 3 5 6 8 9)
	'<)
 (2 3 5 1 4 4 5 6 7 8 9))		;? error

(my-assert
 (merge 'list
	(list 1 4 4 7)
	(list 2 3 5 6 8 9)
	'<)
 (1 2 3 4 4 5 6 7 8 9))			;? error

;; kap 15 listen
;; ----------------------------------------------------------------------------

(my-assert
 (car (list 'a 'b 'c 'd 'e 'f 'g))
 a)

(my-assert
 (cdr (list 'a 'b 'c 'd 'e 'f 'g))
 (b c d e f g))

(my-assert
 (cadr (list 'a 'b 'c 'd 'e 'f 'g))
 b)

(my-assert
 (cddr (list 'a 'b 'c 'd 'e 'f 'g))
 (c d e f g))

(my-assert
 (caddr (list 'a 'b 'c 'd 'e 'f 'g))
 c)

(my-assert
 (cdddr (list 'a 'b 'c 'd 'e 'f 'g))
 (d e f g))

(my-assert
 (cadddr (list 'a 'b 'c 'd 'e 'f 'g))
 d)

(my-assert
 (cddddr (list 'a 'b 'c 'd 'e 'f 'g))
 (e f g))

(my-assert
 (caadr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 ((u v w) x))

(my-assert
 (cadar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (6 7))

(my-assert
 (cdaar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (5))

(my-assert
 (cdadr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (y))

(my-assert
 (cddar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 nil)

(my-assert
 (caaaar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (1 2 3))

(my-assert
 (caadar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 6)

(my-assert
 (caaddr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (q w e))

(my-assert
 (cadaar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 5)

(my-assert
 (cadadr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 y)

(my-assert
 (caddar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 nil)

(my-assert
 (cadddr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (a b c))

(my-assert
 (cdaaar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (4))

(my-assert
 (cdaadr
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (x))

(my-assert
 (cdadar
  (list (list (list (list (list 1 2 3)
			  4)
		    5)
	      (list 6 7))
	(list (list (list 'u 'v 'w)
		    'x)
	      'y)
	(list (list 'q 'w 'e)
	      'r)
	(list 'a 'b 'c)
	'e 'f 'g))
 (7))

(my-assert
 (cons 1 2)
 (1 . 2))

(my-assert
 (cons 'a (cons 'b (cons 'c 'nil)))
 (a b c))

(my-assert
 (cons 'a (list 'b 'c 'd))
 (a b c d))

(my-assert
 (tree-equal 5 (+ 2 3) :test (function eql))
 t)

(my-assert
 (endp 'nil)
 t)

(my-assert
 (endp (cons 'a 'b))
 nil)

(my-assert
 (list-length (list 'a 'b 'c 'd))
 4)

(my-assert
 (let ((x (list 'a 'b 'c))) (rplacd (last x) x)

      (list-length x))
 nil)

(my-assert
 (nth 0 (list 'a 'b 'c 'd))
 a)

(my-assert
 (first (list 1 2 3 4 5 6 7 8 9 10 11))
 1)

(my-assert
 (second (list 1 2 3 4 5 6 7 8 9 10 11))
 2)

(my-assert
 (third (list 1 2 3 4 5 6 7 8 9 10 11))
 3)

(my-assert
 (fourth (list 1 2 3 4 5 6 7 8 9 10 11))
 4)

(my-assert
 (fifth (list 1 2 3 4 5 6 7 8 9 10 11))
 5)

(my-assert
 (sixth (list 1 2 3 4 5 6 7 8 9 10 11))
 6)

(my-assert
 (seventh (list 1 2 3 4 5 6 7 8 9 10 11))
 7)

(my-assert
 (eighth (list 1 2 3 4 5 6 7 8 9 10 11))
 8)

(my-assert
 (ninth (list 1 2 3 4 5 6 7 8 9 10 11))
 9)

(my-assert
 (tenth (list 1 2 3 4 5 6 7 8 9 10 11))
 10)

(my-assert
 (rest (cons 'a 'b))
 b)

(my-assert
 (nthcdr 1 (list 'a 'b 'c 'd))
 (b c d))

(my-assert
 (last (list 1 2 3 4 5))
 (5))

(my-assert
 (last (append (list 1 2 3) 4))
 (3 . 4))

(my-assert
 (list 'a 'b 'c 'd)
 (a b c d))

(my-assert
 (list* 'a 'b 'c 'd)
 (a b c . d))

(my-assert
 (make-list 4 :initial-element 'o)
 (o o o o))

(my-assert
 (make-list 3 :initial-element 'rah)
 (rah rah rah))

(my-assert
 (append (list 'a 'b 'c)
	 (list 'd 'e 'f) 'nil '(g))
 (a b c d e f g))

(my-assert
 (copy-list (list 1 2 3 4 5))
 (1 2 3 4 5))

(my-assert
 (copy-list (append (list 1 2 3) 4))
 (1 2 3 . 4))

(my-assert
 (copy-alist (list 'a 'b))
 (a b))

(my-assert
 (copy-alist (list (cons 1 'a) (cons 2 'b) (cons 3 'c)))
 ((1 . a) (2 . b) (3 . c)))

(my-assert
 (copy-alist (list (list 'a 'b) 'c (list 'd 'e)))
 ((a b) c (d e)))

(my-assert
 (copy-tree (list 'a 'b
		  (list 'c
			(list 'd)
			(list 'e 'f))
		  'g))
 (a b (c (d) (e f)) g))

(my-assert
 (revappend (list 'a 'b 'c) (list 'd 'e 'f))
 (c b a d e f))

(my-assert
 (revappend (list 'a 'b 'c) 'i)
 (c b a . i))				;? error

(my-assert
 (nreconc (list 'a 'b 'c) (list 'i 'j))
 (c b a i j))

;; nreconc

(my-assert
 (setq aa nil)
 nil)

(my-assert
 (push '1 aa)
 (1))

(my-assert
 (push '2 aa)
 (2 1))

(my-assert
 (pop aa)
 2)

(my-assert
 (pop aa)
 1)

(my-assert
 (pop aa)
 nil)

(my-assert
 (setq aa (list 'b 'a))
 (b a))

(my-assert
 (pushnew 'a aa)
 (b a))

(my-assert
 (pushnew 'c aa)
 (c b a))

(my-assert
 (pushnew 'u (car (setq xx (list nil 'kkk))))
 (u))

(my-assert
 (pushnew 'u (car xx))
 (u))

(my-assert
 (pushnew 'v (car xx))
 (v u))

(my-assert
 (eval 'xx)
 ((v u) kkk))

(my-assert
 (butlast (list 'a 'b 'c) 2)
 (a))

(my-assert
 (nbutlast (list 'a 'b 'c 'd) 6)
 nil)

(my-assert
 (nbutlast (list 'a 'b 'c 'd) 1)
 (a b c))

(my-assert
 (ldiff (setq xx (list 'a 'b 'c 'd 'e))
	(cddr xx))
 (a b))

(my-assert
 (ldiff (setq xx (append (list 'a 'b 'c 'd)
			 'e))
	(cddr xx))
 (a b))

(unintern 'xx)

(my-assert
 (ldiff (append (list 'a 'b 'c 'd)
		'e)
	'e)
 (a b c d))

;; rplaca, rplacd

(my-assert
 (nsubst 'a 'b
	 (list 'u 'b (list 'b) 'c)
	 :test-not (lambda (x y)
		     (not (eql x y))))
 (u a (a) c))

(my-assert
 (nsubst-if 'oo
	    'numberp
	    (list 'a 'b 'c (list 3 (list 4) 0)))
 (a b c (oo (oo) oo)))

(my-assert
 (nsubst-if-not 'oo
		#'(lambda (x)
		    (or (list x)
			(symbolp x)))
		(list 'a 'b 'c (list 3 (list 4) 0)))
 (a b c (3 (4) 0)))

(my-assert
 (subst 'a 'b (list 'u 'b (list 'b) 'c)
	:test-not (lambda (x y)
		    (not (eql x y)))
	:key (lambda (u)
	       (when (listp u)
		 (car u))))
 (u . a))

(my-assert
 (subst-if 'nummmer
	   'numberp
	   (list (list 'a (list 7 (list 'v 6)))))

 ((a (nummmer (v nummmer)))))

(my-assert
 (subst-if-not 'nummmer
	       #'(lambda (x)
		   (or (listp x)
		       (numberp x)))
	       (list (list 'a (list 7 (list 'v 6)))))
 ((nummmer (7 (nummmer 6)))))

(my-assert
 (nsublis (list (cons (list 'a) 'uu)
		(cons 'a 'ii))
	  (list 'i (list 'a) 'a)
	  :test
	  (lambda (x y)
	    (when (listp y)
	      (eql x (car y)))))
 #+(or xcl allegro ecls) (i ii . ii)	; x aus der aliste, y ein blatt des baumes
 #+(or clisp cmu sbcl lucid) (i (uu) uu) ; x ein blatt, y aus der aliste
 #-(or xcl clisp cmu sbcl lucid allegro ecls) unknown)

(my-assert
 (SUBLIS (QUOTE (((A) . UU) (A . II)))
	 (QUOTE (I (A) A))
	 :TEST (LAMBDA (X Y)
		       (IF (LISTP Y) (EQL X (CAR Y)))))
 #+nil

 (sublis (list (list (cons (list 'a) 'uu) (cons 'a 'ii)))
	 (list 'i (list 'a) 'a)
	 :test (lambda (x y)
		 (when (listp y)
		   (eql x (car y)))))
 #+(or xcl allegro lucid ecls) (i ii . ii)	; x aus der aliste, y ein blatt des baumes
 #+(or clisp cmu sbcl sbcl) (i (uu) uu) ; x ein blatt, y aus der aliste
 #-(or xcl clisp cmu sbcl lucid allegro ecls) unknown)

(my-assert
 (member 'A
	 (list (list 'A)
	       (list 'B)
	       (list 'A)
	       (list 'C))
	 :key
	 'car)
 ((a) (b) (a) (c)))

(my-assert
 (member-if 'numberp
	    (list (list 'a)
		  (list 'b)
		  (list 3)
		  (list 'c))
	    :key 'car)

 ((3) (c)))

(my-assert
 (member-if-not 'numberp
		(list (list 8)
		      (list 'a)
		      (list 'b)
		      (list 3)
		      (list 'c))
		:key 'car)
 ((a) (b) (3) (c)))

(my-assert
 (tailp (cddr (setq xx (list 'u 'i 'a 'b))) xx)
 t)

(unintern 'xx)

(my-assert
 (tailp 'd (append (list 'a 'b 'c) 'd))
 t)

(my-assert
 (adjoin 'a
	 (list (list 'a)
	       'b
	       'c)
	 :test 'equal)
 (a (a) b c))

(my-assert
 (nunion (list 'a 'b 'c 'd)
	 (list 'u 'i 'b 'a))
 #+xcl (a b c d u i)
 #+(or ecls clisp) (c d u i b a)
 #+(or allegro cmu sbcl) (d c u i b a)
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (union (list 'a 'b 'c 'd)
	(list 'a 'd 'i 'v))
 #+xcl (v i a b c d)
 #+(or ecls clisp) (b c a d i v)
 #+(or allegro cmu sbcl) (c b a d i v)
 #-(or xcl clisp allegro cmu sbcl ecls) unknown)

(my-assert
 (intersection (list (list 'a 1)
		     (list 'a 2)
		     (list 'a 3))
	       (list (list 'a 4)
		     (list 'a 2)
		     (list 'b 6)
		     (list 'c 7))
	       :test 'equal)
 ((a 2)))

(my-assert
 (nintersection (list 'a 'b 'c 'd)
		(list 'c 'd 'e 'f 'g)
		:test-not (quote eql))
 #-(or allegro cmu sbcl sbcl) (a b c d)
 #+(or allegro cmu sbcl sbcl) (d c b a))

(my-assert
 (nset-difference (list 'a 'b 'c 'd)
		  (list 'i 'j 'c))
 #-(or allegro cmu sbcl sbcl) (a b d)
 #+(or allegro cmu sbcl sbcl) (d b a))

(my-assert
 (nset-exclusive-or (list 'a 'b 'c)
		    (list 'i 'a 'd 'c))
 (b i d))

(my-assert
 (set-difference (list 'anton 'berta 'auto 'berlin)
		 (list 'amerilla)
		 :test (lambda (x y)
			 (eql (elt (symbol-name x) 0)
			      (elt (symbol-name y) 0))))
 #+(or xcl allegro cmu sbcl sbcl) (berlin berta)
 #+(or clisp akcl ecls) (berta berlin)
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (set-exclusive-or (list 'anton 'anna 'emil)
		   (list 'berta 'auto 'august)
		   :test (lambda (x y)
			   (eql (elt (symbol-name x) 0)
				(elt (symbol-name y) 0))))
 #-(or allegro cmu sbcl) (emil berta)
 #+(or allegro cmu sbcl) (berta emil))

(my-assert
 (subsetp (list 'a 'b) (list 'b 'a 'u 'i 'c 'd))
 t)

(my-assert
 (acons 'a 'b (list (cons 'c 'd)))
 ((a . b) (c . d)))

(my-assert
 (acons 'a 'b nil)
 ((a . b)))

(my-assert
 (assoc 'a (list (list 'b 'c)
		 'a
		 (list (list 'a) 'u)
		 (list 'a 'i))
	:test-not (lambda (x y)
		    (when (atom y)
		      (eql y x))))
 #+allegro error
 #-allegro (b c))

(my-assert
 (assoc-if 'symbolp
	   (list (cons 'a 3)
		 (cons 3 'a)))
 (a . 3))

(my-assert
 (assoc-if-not 'numberp (list (cons 'a 3)
			      (cons 3 'a)))
 (a . 3))

(my-assert
 (pairlis (list 'a 'b 'c) (list 1 2 3))
 ((c . 3) (b . 2) (a . 1)))

(my-assert
 (rassoc 'a (list (cons 1 'b) (cons 2 'a)))
 (2 . a))

(my-assert
 (rassoc-if 'symbolp
	    (list (cons 1 3) (cons 2 'a)))
 (2 . a))

(my-assert
 (rassoc-if-not 'symbolp
		(list (cons 1 3) (cons 2 'a)))
 (1 . 3))

;; kap 16 hash-tabellen
;; ----------------------------------------------------------------------------

(my-assert
 (hash-table-p (make-hash-table :test #'eql
				:rehash-size 2
				:size 20))
 t)

(my-assert
 (hash-table-p (make-hash-table :test #'eql
				:rehash-size 1.1
				:size 20))
 t)
;; clrhash, gethash, hash-table-count, maphash, remhash, sxhash,

;; <hs>/body/mac_with-hash_ble-iterator.html
(my-assert
 (defun test-hash-table-iterator (hash-table)
   (let ((all-entries '())
	 (generated-entries '())
	 (unique (list nil)))
     (maphash #'(lambda (key value)
		  (push (list key value)
			all-entries))
	      hash-table)
     (with-hash-table-iterator
      (generator-fn hash-table)
      (loop
	(multiple-value-bind (more? key value)
	    (generator-fn)
	  (unless more? (return))
	  (unless (eql value (gethash key hash-table unique))
	    (error "Key ~S not found for value ~S" key value))
	  (push (list key value) generated-entries))))
     (unless (= (length all-entries)
		(length generated-entries)
		(length (union all-entries
			       generated-entries
			       :key #'car
			       :test (hash-table-test hash-table))))
       (error "Generated entries and Maphash entries don't correspond"))
     t))
 test-hash-table-iterator)

(my-assert
 (let ((tab (make-hash-table :test #'equal)))
   (setf (gethash "Richard" tab) "Gabriel")
   (setf (gethash "Bruno" tab) "Haible")
   (setf (gethash "Michael" tab) "Stoll")
   (setf (gethash "Linus" tab) "Torvalds")
   (setf (gethash "Richard" tab) "Stallman")
   (test-hash-table-iterator tab)
   )
 t)

;; kap 17 felder
;; ----------------------------------------------------------------------------

;; make-array, vector, aref, svref, array-element-type, array-rank,
;; array-dimension, array-dimensions, array-total-size, array-in-bounds-p,
;; array-row-major-index, adjustable-array-p,

;; array-rank-limit, array-dimension-limit, array-total-size-limit,


;; bit, sbit, bit-and, bit-andc1, bit-andc2, bit-eqv, bit-ior, bit-nand,
;;; bit-nor, bit-not, bit-orc1, bit-orc2, bit-xor,

;; array-has-fill-pointer-p, fill-pointer, vector-pop, vector-push,
;; vector-push-extend, adjust-array,

;; kap 18 strings
;; ----------------------------------------------------------------------------

;; char, schar, string=, string-equal, string/=, string<, string<=, string>,

;; string>=, string-greaterp, string-lessp, string-not-equal,
;; string-not-greaterp, string-not-lessp, make-string, string-left-trim,

;; string-right-trim, string-trim, string-upcase, string-capitalize,

;; string-downcase, nstring-capitalize, nstring-downcase, nstring-upcase,
;; string,

;; kap 19 strukturen
;; ----------------------------------------------------------------------------

;; defstruct,

(my-assert
 (defstruct (ice-cream-factory
	     (:constructor make-factory)
	     (:constructor fabricate-factory
			   (&key (capacity 5)
				 location
				 (local-flavors
				  (case location
				    ((hawaii) '(pineapple macadamia guava))
				    ((massachusetts) '(lobster baked-bean))
				    ((california) '(ginger lotus avocado bean-sprout garlic))
				    ((texas) '(jalapeno barbecue))))
				 (flavors
				  (subseq (append local-flavors
						  '(vanilla chocolate strawberry pistachio
							    maple-walnut peppermint))
					  0 capacity)))))
   (capacity 3)
   (flavors '(vanilla chocolate strawberry mango)))
 ice-cream-factory)

(my-assert
 (let ((houston (fabricate-factory :capacity 4 :location 'texas)))
   (ice-cream-factory-flavors houston))
 (jalapeno barbecue vanilla chocolate))

(my-assert
 (let ((cambridge (fabricate-factory :location 'massachusetts)))
   (ice-cream-factory-flavors cambridge))
 (lobster baked-bean vanilla chocolate strawberry))

(my-assert
 (let ((seattle (fabricate-factory :local-flavors '(salmon))))
   (ice-cream-factory-flavors seattle))
 (salmon vanilla chocolate strawberry pistachio))

(my-assert
 (let ((wheaton (fabricate-factory :capacity 4 :location 'illinois)))
   (ice-cream-factory-flavors wheaton))
 (vanilla chocolate strawberry pistachio))

(my-assert
 (let ((pittsburgh (fabricate-factory :capacity 4)))
   (ice-cream-factory-flavors pittsburgh))
 (vanilla chocolate strawberry pistachio))

(my-assert
 (let ((cleveland (make-factory :capacity 4)))
   (ice-cream-factory-flavors cleveland))
 (vanilla chocolate strawberry mango))

;; kap 20 eval
;; ----------------------------------------------------------------------------

;; eval, evalhook, *evalhook*, applyhook, *applyhook*,

(my-assert
 (constantp -5)
 t)

(my-assert
 (constantp (read-from-string "1.0e30"))
 t)

;; kap 21 streams
;; ----------------------------------------------------------------------------

;; make-synonym-stream, make-broadcast-stream, make-concatenated-stream,
;; make-two-way-stream, make-echo-stream, make-string-input-stream,
;; make-string-output-stream, get-output-stream-string, with-input-from-string,
;; with-open-stream, with-output-to-string,

(my-assert
 (streamp *standard-input*)
 t)

(my-assert
 (input-stream-p *terminal-io*)
 t)

;; output-stream-p, stream-element-type, close,

;; kap 22 ein- und ausgabe
;; ----------------------------------------------------------------------------

(my-assert
 (readtablep *readtable*)
 t)

(my-assert
 (readtablep 'progn)
 nil)

;; copy-readtable, read, *read-base*, read-byte, read-char, read-char-no-hang,

;; *read-default-float-format*, read-delimited-list, read-from-string, read-line,
;; read-preserving-whitespace, *read-suppress*, *readtable*, unread-char,

;; get-dispatch-macro-character, get-macro-character,
;; set-dispatch-macro-character, set-macro-character, set-syntax-from-char,
;; make-dispatch-macro-character,

(my-assert
 (get-dispatch-macro-character #\# #\0)
 nil)

;; pprint, prin1, prin1-to-string, princ, princ-to-string, print, *print-array*,
;; *print-base*, *print-case*, *print-circle*, *print-escape*, *print-gensym*,

;; *print-length*, *print-level*, *print-pretty*, *print-radix*,

;; peek-char, listen, clear-input, clear-output, parse-integer,

;; write, write-byte, write-char, write-line, write-string, write-to-string,
;; y-or-n-p, yes-or-no-p,

;; terpri, finish-output, force-output, format, fresh-line,

;; kap 23 file-interface
;; ----------------------------------------------------------------------------

;; pathname, truename, parse-namestring, merge-pathnames,
;; *default-pathname-defaults*, make-pathname, pathnamep, pathname-device,
;; pathname-directory, pathname-host, pathname-name, pathname-type,
;; pathname-version, namestring, file-namestring, directory-namestring,

;; host-namestring, enough-namestring, user-homedir-pathname, open,
;; with-open-file, rename-file, delete-file, probe-file, file-write-date,

;; file-author, file-length, file-position, load, *load-verbose*, directory

;; kap 24 fehler
;; ----------------------------------------------------------------------------

;; cerror, error, *break-on-warnings*, warn, break, check-type, assert, etypecase,
;; ecase, ctypecase, ccase

;; kap 25 erweiterungen
;; ----------------------------------------------------------------------------

;; compile, disassemble, compile-file, documentation, trace, untrace, step, time,
;; describe, inspect, room, ed, dribble, apropos, apropos-list,
;; get-decoded-time, get-internal-real-time, get-internal-run-time,
;; get-universal-time, decode-universal-time, encode-universal-time,

;; internal-time-units-per-second, sleep, lisp-implementation-type,
;; lisp-implementation-version, machine-instance, machine-type, machine-version,

;; software-type, software-version, short-site-name, long-site-name, *features*,
;; identity

;; kap i systeminterne praedikate
;; ----------------------------------------------------------------------------
;; ? (sequencep (type-specifier-p (bit-array-p
;; ? (adjustable-vector-with-fill-pointer-p (alistp (declaration-specifier-p


(my-assert
 #-sbcl
 (sys::fixnump 10)			;?
 #+sbcl
 (sb-kernel:fixnump 10)			;?
 t)					;?

;; kap ii systeminterne atome
;; ----------------------------------------------------------------------------

;; case-every, comment, cond-every, displace, return, return-from, access, boole,
;; call-arguments-limit, defun, errset, *errset*, *macroexpand-hook*, *package*,
;; *random-state*, *save-old-definition-when-redefined*,
