;; -*- lisp -*-
(defun safe-mapcar (f list)
  (mapcar #'(lambda (x) (with-ignored-errors (funcall f x))) list))
safe-mapcar
(defun safe-mapcar2 (f list list2)
  (mapcar #'(lambda (x y) (with-ignored-errors (funcall f x y))) list list2))
safe-mapcar2

;;
;; CONS
;;
(cons 'a 'b)
(a . b)
(cons 'a nil)
(a)
(cons nil 'a)
(nil . a)

;;
;; CONSP, LISTP, ENDP, NULL
;;
(mapcar #'(lambda (f)
	    (safe-mapcar f '(a nil (a) (a . b) (a b))))
	'(consp listp endp null))
((nil nil t t t)
 (nil t t t t)
 (error t nil nil nil)
 (nil t nil nil nil))

;;
;; LIST, LIST*
;;
(list)			; List with no arg outputs nil
	nil
(list 'a)		; List with one arg makes a list
	(a)
(list 'a 'b)		; Elements are added to the list
	(a b)
(list 'a nil)
	(a nil)
(list nil nil)
	(nil nil)
(list 'a '(b c))	; except for trailing lists, which are appended
	(a (b c))
(list 'a 'b '(c))
	(a b (c))

(list*)			; list* requires at least one arg
	error
(list* 'a)		; list* outputs its single arg, no matter what it is
	a
(let ((a (cons 'a 'b))) (eq (list* a) a))
	t
(list* 'a 'b)		; the last arg is made the CDR of the last cons
	(a . b)
(list* 'a nil)
	(a)
(list* nil nil)
	(nil)
(list* 'a '(b c))	; except for trailing lists, which are appended
	(a b c)
(list* 'a 'b '(c))
	(a b c)

;;
;; MAKE-LIST
;;
(safe-mapcar #'make-list '(-1 0 1 2 3 4))
(error nil (nil) (nil nil) (nil nil nil) (nil nil nil nil))

(safe-mapcar2 #'(lambda (x y) (make-list x :initial-element y))
	      '(-1 0 1 2 3 4)
	      '(a b a b a b))
(error nil (a) (b b) (a a a) (b b b b))

;;
;; REST, CAR, CDR, CAAR, etc
;;
#+nil ; This was used to generate the trees below
(defun make-tree (depth left-index)
  (if (<= depth 0)
    (values left-index (1+ left-index))
    (multiple-value-bind (left right-index)
	(make-tree (1- depth) left-index)
      (multiple-value-bind (right right-index)
	  (make-tree (1- depth) right-index)
	(values (cons left right)
		(1+ right-index))))))
(defparameter a '(nil
		  1
		  (1 . 2)
		  ((1 . 2) 3 . 4)
		  (((1 . 2) 3 . 4) (5 . 6) 7 . 8)
		  ((((1 . 2) 3 . 4) (5 . 6) 7 . 8) ((9 . 10) 11 . 12) (13 . 14) 15 . 16)))
a

; Test CAR and friends as selectors
(mapcar #'(lambda (f) (safe-mapcar f a))
	'(car cdr rest caar cadr cdar cddr
	  caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))
((nil error 1 (1 . 2) ((1 . 2) 3 . 4) (((1 . 2) 3 . 4) (5 . 6) 7 . 8))
 (nil error 2 (3 . 4) ((5 . 6) 7 . 8) (((9 . 10) 11 . 12) (13 . 14) 15 . 16))
 (nil error 2 (3 . 4) ((5 . 6) 7 . 8) (((9 . 10) 11 . 12) (13 . 14) 15 . 16))
 (nil error error 1 (1 . 2) ((1 . 2) 3 . 4))
 (nil error error 3 (5 . 6) ((9 . 10) 11 . 12))
 (nil error error 2 (3 . 4) ((5 . 6) 7 . 8))
 (nil error error 4 (7 . 8) ((13 . 14) 15 . 16))
 (nil error error error 1 (1 . 2))
 (nil error error error 5 (9 . 10))
 (nil error error error 3 (5 . 6))
 (nil error error error 7 (13 . 14))
 (nil error error error 2 (3 . 4))
 (nil error error error 6 (11 . 12))
 (nil error error error 4 (7 . 8))
 (nil error error error 8 (15 . 16))
 (nil error error error error 1)
 (nil error error error error 9)
 (nil error error error error 5)
 (nil error error error error 13)
 (nil error error error error 3)
 (nil error error error error 11)
 (nil error error error error 7)
 (nil error error error error 15)
 (nil error error error error 2)
 (nil error error error error 10)
 (nil error error error error 6)
 (nil error error error error 14)
 (nil error error error error 4)
 (nil error error error error 12)
 (nil error error error error 8)
 (nil error error error error 16))

; Test CAR and friends as accessors for SETF
(mapcar #'(lambda (f)
	    (eval `(safe-mapcar #'(lambda (tree)
				    (setf (,f tree) ',f)
				    (funcall ',f tree))
		    a)))
	'(caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
	  cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr
	  caaar caadr cadar caddr cdaar cdadr cddar cdddr
	  caar cadr cdar cddr
	  car cdr rest))
((error error error error error caaaar) (error error error error error caaadr)
 (error error error error error caadar) (error error error error error caaddr)
 (error error error error error cadaar) (error error error error error cadadr)
 (error error error error error caddar) (error error error error error cadddr)
 (error error error error error cdaaar) (error error error error error cdaadr)
 (error error error error error cdadar) (error error error error error cdaddr)
 (error error error error error cddaar) (error error error error error cddadr)
 (error error error error error cdddar) (error error error error error cddddr)
 (error error error error caaar caaar) (error error error error caadr caadr)
 (error error error error cadar cadar) (error error error error caddr caddr)
 (error error error error cdaar cdaar) (error error error error cdadr cdadr)
 (error error error error cddar cddar) (error error error error cdddr cdddr)
 (error error error caar caar caar) (error error error cadr cadr cadr)
 (error error error cdar cdar cdar) (error error error cddr cddr cddr)
 (error error car car car car) (error error cdr cdr cdr cdr)
 (error error rest rest rest rest))

;;
;; RPLACA, RPLACD
;;
; They both fail with non-cons objects
(rplaca 'a 'b)
error
(rplacd 'a 'b)
error
(rplaca nil 'a)
error
(rplaca nil 'b)
error

; Now test proper work
(rplaca (cons 'a 'b) 'c)
(c . b)
(rplacd (cons 'a 'b) 'c)
(a . c)

;;
;; FIRST, SECOND, etc
;;
(let ((all '(nil
	     (1)
	     (1 2)
	     (1 2 3)
	     (1 2 3 4)
	     (1 2 3 4 5)
	     (1 2 3 4 5 6)
	     (1 2 3 4 5 6 7)
	     (1 2 3 4 5 6 7 8)
	     (1 2 3 4 5 6 7 8 9)
	     (1 2 3 4 5 6 7 8 9 10)
	     (1 2 3 4 5 6 7 8 9 10 11))))
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (z)
			  (with-ignored-errors
			      (funcall (symbol-function x) z)))
		      all))
	  '(first second third fourth fifth sixth seventh eighth ninth tenth)))
((nil 1 1 1 1 1 1 1 1 1 1 1)
 (nil nil 2 2 2 2 2 2 2 2 2 2)
 (nil nil nil 3 3 3 3 3 3 3 3 3)
 (nil nil nil nil 4 4 4 4 4 4 4 4)
 (nil nil nil nil nil 5 5 5 5 5 5 5)
 (nil nil nil nil nil nil 6 6 6 6 6 6)
 (nil nil nil nil nil nil nil 7 7 7 7 7)
 (nil nil nil nil nil nil nil nil 8 8 8 8)
 (nil nil nil nil nil nil nil nil nil 9 9 9)
 (nil nil nil nil nil nil nil nil nil nil 10 10))

(let ((all '((1 . b)
	     (1 2 . b)
	     (1 2 3 . b)
	     (1 2 3 4 . b)
	     (1 2 3 4 5 . b)
	     (1 2 3 4 5 6 . b)
	     (1 2 3 4 5 6 7 . b)
	     (1 2 3 4 5 6 7 8 . b)
	     (1 2 3 4 5 6 7 8 9 . b)
	     (1 2 3 4 5 6 7 8 9 10 . b)
	     (1 2 3 4 5 6 7 8 9 10 11 . b))))
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (z)
			  (with-ignored-errors
			      (funcall (symbol-function x) z)))
		      all))
	  '(first second third fourth fifth sixth seventh eighth ninth tenth)))
((1 1 1 1 1 1 1 1 1 1 1)
 (error 2 2 2 2 2 2 2 2 2 2)
 (error error 3 3 3 3 3 3 3 3 3)
 (error error error 4 4 4 4 4 4 4 4)
 (error error error error 5 5 5 5 5 5 5)
 (error error error error error 6 6 6 6 6 6)
 (error error error error error error 7 7 7 7 7)
 (error error error error error error error 8 8 8 8)
 (error error error error error error error error 9 9 9)
 (error error error error error error error error error 10 10))

;;
;; LAST, BUTLAST, NBUTLAST
;;
(mapcar #'(lambda (f)
	    (safe-mapcar #'(lambda (x) (funcall f (copy-list x)))
			 '(nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)
			   a (1 . 2) (1 2 . 3) (1 2 3 . 4) (1 2 3 4 . 5))))
	'(last butlast nbutlast))
((nil (1) (2) (3) (4) (5) error (1 . 2) (2 . 3) (3 . 4) (4 . 5))
 (nil nil (1) (1 2) (1 2 3) (1 2 3 4) error nil (1) (1 2) (1 2 3))
 (nil nil (1) (1 2) (1 2 3) (1 2 3 4) error nil (1) (1 2) (1 2 3)))

(mapcar #'(lambda (f)
	    (safe-mapcar #'(lambda (x) (funcall f x 0))
			 '(nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)
			   a (1 . 2) (1 2 . 3) (1 2 3 . 4) (1 2 3 4 . 5))))
	'(last butlast nbutlast))
((nil nil nil nil nil nil error 2 3 4 5)
 (nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5) error (1) (1 2) (1 2 3) (1 2 3 4))
 (nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5) error (1) (1 2) (1 2 3) (1 2 3 4)))

(mapcar #'(lambda (f)
	    (safe-mapcar #'(lambda (x) (funcall f (copy-list x) 3))
			 '(nil (1) (1 2) (1 2 3) (1 2 3 4) (1 2 3 4 5)
			   a (1 . 2) (1 2 . 3) (1 2 3 . 4) (1 2 3 4 . 5))))
	'(last butlast nbutlast))
((nil (1) (1 2) (1 2 3) (2 3 4) (3 4 5) error (1 . 2) (1 2 . 3) (1 2 3 . 4) (2 3 4 . 5))
 (nil nil nil nil (1) (1 2) error nil nil nil (1))
 (nil nil nil nil (1) (1 2) error nil nil nil (1))))

(let ((a (list 1 2 3)))
  (rplacd (last a) a)
  (safe-mapcar2 #'funcall '(last butlast nbutlast) (list a a a)))
(error error error)

;;
;; NTH
;;
(let ((all '(nil
	     (1)
	     (1 2)
	     (1 2 3)
	     (1 2 3 4)
	     (1 2 3 4 5)
	     (1 2 3 4 5 6)
	     (1 2 3 4 5 6 7)
	     (1 2 3 4 5 6 7 8)
	     (1 2 3 4 5 6 7 8 9)
	     (1 2 3 4 5 6 7 8 9 10)
	     (1 2 3 4 5 6 7 8 9 10 11))))
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (z)
			  (with-ignored-errors
			      (nth x z)))
		      all))
	  '(-1 0 1 2 3 4 5 6 7 8 9 10 11 12)))
((error error error error error error error error error error error error)
 (nil 1 1 1 1 1 1 1 1 1 1 1)
 (nil nil 2 2 2 2 2 2 2 2 2 2)
 (nil nil nil 3 3 3 3 3 3 3 3 3)
 (nil nil nil nil 4 4 4 4 4 4 4 4)
 (nil nil nil nil nil 5 5 5 5 5 5 5)
 (nil nil nil nil nil nil 6 6 6 6 6 6)
 (nil nil nil nil nil nil nil 7 7 7 7 7)
 (nil nil nil nil nil nil nil nil 8 8 8 8)
 (nil nil nil nil nil nil nil nil nil 9 9 9)
 (nil nil nil nil nil nil nil nil nil nil 10 10)
 (nil nil nil nil nil nil nil nil nil nil nil 11)
 (nil nil nil nil nil nil nil nil nil nil nil nil)
 (nil nil nil nil nil nil nil nil nil nil nil nil))

(let ((all '((1 . b)
	     (1 2 . b)
	     (1 2 3 . b)
	     (1 2 3 4 . b)
	     (1 2 3 4 5 . b)
	     (1 2 3 4 5 6 . b)
	     (1 2 3 4 5 6 7 . b)
	     (1 2 3 4 5 6 7 8 . b)
	     (1 2 3 4 5 6 7 8 9 . b)
	     (1 2 3 4 5 6 7 8 9 10 . b)
	     (1 2 3 4 5 6 7 8 9 10 11 . b))))
  (mapcar #'(lambda (x)
	      (mapcar #'(lambda (z)
			  (with-ignored-errors
			      (nth x z)))
		      all))
	  '(-1 0 1 2 3 4 5 6 7 8 9 10 11 12)))
((error error error error error error error error error error error)
 (1 1 1 1 1 1 1 1 1 1 1)
 (error 2 2 2 2 2 2 2 2 2 2)
 (error error 3 3 3 3 3 3 3 3 3)
 (error error error 4 4 4 4 4 4 4 4)
 (error error error error 5 5 5 5 5 5 5)
 (error error error error error 6 6 6 6 6 6)
 (error error error error error error 7 7 7 7 7)
 (error error error error error error error 8 8 8 8)
 (error error error error error error error error 9 9 9)
 (error error error error error error error error error 10 10)
 (error error error error error error error error error error 11)
 (error error error error error error error error error error error)
 (error error error error error error error error error error error))

;;
;; LIST-LENGTH
;;
(safe-mapcar #'list-length '(1 a t
			     nil (1) (1 . 2)
			     (1 2 3 4) (1 2 3 . 4)
			     (1 2 nil) (1 2 nil . 3)))
(error error error 0 1 error 4 error 3 error)

(let ((a (list 1 2 3)))
  (rplaca (last a) a)
  (list-length a))
3

(let ((a (list 1 2 3)))
  (rplacd (last a) a)
  (list-length a))
nil

;;
;; COPY-LIST
;;
(defun check-fresh-list (a b)
  (cond ((and (null a) (null b))
	 t)
	((and (atom a) (atom b))
	 t)
	((and (not (eq a b))
	      (eq (car a) (car b)))
	 (check-fresh-list (cdr a) (cdr b)))))
check-fresh-list

(let* ((a '(nil (1) (1 2) (1 2 nil) (1 . 2) (1 2 . 3)))
       (b (safe-mapcar #'copy-list a)))
  (safe-mapcar2 #'check-fresh-list b a))
(t t t t t t)

#+ecls
(let ((a (list 1 2 3)))
  (rplacd (last a) a)
  (copy-list a))
#+ecls
error

