;;; section 15: arrays -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(my-assert
 (subtypep 'simple-array 'array)
 t)

;;; make-array

(my-assert
 (make-array 4 :initial-element nil)
 #(NIL NIL NIL NIL))

(my-assert
 (make-array '(2 4)
	     :element-type '(unsigned-byte 2)
	     :initial-contents '((0 1 2 3) (3 2 1 0)))
 #2A((0 1 2 3) (3 2 1 0)))

(my-assert
 (make-array 6
	     :element-type 'character
	     :initial-element #\a
	     :fill-pointer 3)
 "aaa")

(my-assert
 (progn
   (setq a (make-array '(4 3)))
   t)
 t)

(my-assert
 (dotimes (i 4)
   (dotimes (j 3)
     (setf (aref a i j) (list i 'x j '= (* i j)))))
 NIL)

(my-assert
 (progn
   (setq b (make-array 8 :displaced-to a
		       :displaced-index-offset 2))
   t)
 t)

(my-assert
 (let ((a '()))
   (dotimes (i 8)
     (setq a (append a (list i (aref b i)))))
   a)
 (0 (0 X 2 = 0)
    1 (1 X 0 = 0)
    2 (1 X 1 = 1)
    3 (1 X 2 = 2)
    4 (2 X 0 = 0)
    5 (2 X 1 = 2)
    6 (2 X 2 = 4)
    7 (3 X 0 = 0)))

(my-assert
 (progn
   (setq a1 (make-array 50))
   t)
 t)

(my-assert
 (progn
   (setq b1 (make-array 20 :displaced-to a1 :displaced-index-offset 10))
   t)
 t)

(my-assert
 (length b1)
 20)

(my-assert
 (progn
   (setq a2 (make-array 50 :fill-pointer 10))
   t)
 t)

(my-assert
 (progn
   (setq b2 (make-array 20 :displaced-to a2 :displaced-index-offset 10))
   t)
 t)

(my-assert
 (length a2)
 10)

(my-assert
 (length b2)
 20)

(my-assert
 (progn
   (setq a3 (make-array 50 :fill-pointer 10))
   t)
 t)

(my-assert
 (progn
   (setq b3 (make-array 20 :displaced-to a3 :displaced-index-offset 10
			:fill-pointer 5))
   t)
 t)

(my-assert
 (length a3)
 10)

(my-assert
 (length b3)
 5)


;;; adjust-array

(my-assert
 (adjustable-array-p
  (setq ada (adjust-array
	     (make-array '(2 3)
			 :adjustable t
			 :initial-contents '((a b c) (1 2 3)))
	     '(4 6))))
 T )

(my-assert
 (array-dimensions ada)
 (4 6) )

(my-assert
 (aref ada 1 1)
 2 )

(my-assert
 (setq beta (make-array '(2 3) :adjustable t))
 #+(or cmu sbcl) #2A((0 0 0) (0 0 0))
 #-(or cmu sbcl) #2A((NIL NIL NIL) (NIL NIL NIL)))

(my-assert
 (adjust-array beta '(4 6) :displaced-to ada)
 #+(or cmu sbcl) #2A((A B C 0 0 0)
		     (1 2 3 0 0 0)
		     (0 0 0 0 0 0)
		     (0 0 0 0 0 0))
 #-(or cmu sbcl) #2A((A B C NIL NIL NIL)
		     (1 2 3 NIL NIL NIL)
		     (NIL NIL NIL NIL NIL NIL)
		     (NIL NIL NIL NIL NIL NIL)))

(my-assert
 (array-dimensions beta)
 (4 6))

(my-assert
 (aref beta 1 1)
 2 )

(my-assert
 (let ((m
	(make-array '(4 4)
		    :adjustable t
		    :initial-contents
		    '(( alpha     beta      gamma     delta )
		      ( epsilon   zeta      eta       theta )
		      ( iota      kappa     lambda    mu    )
		      ( nu        xi        omicron   pi    )))))
   m)
 #2A(( alpha     beta      gamma     delta )
     ( epsilon   zeta      eta       theta )
     ( iota      kappa     lambda    mu    )
     ( nu        xi        omicron   pi    )))

(my-assert
  (let ((m
	(make-array '(4 4)
		    :adjustable t
		    :initial-contents
		    '(( alpha     beta      gamma     delta )
		      ( epsilon   zeta      eta       theta )
		      ( iota      kappa     lambda    mu    )
		      ( nu        xi        omicron   pi    )))))
    (adjust-array m '(3 5) :initial-element 'baz))
 #2A(( alpha     beta      gamma     delta     baz )
     ( epsilon   zeta      eta       theta     baz )
     ( iota      kappa     lambda    mu        baz )))

;;; adjustable-array-p

(my-assert
 (adjustable-array-p
  (make-array 5
	      :element-type 'character
	      :adjustable t
	      :fill-pointer 3))
 t)

;;; aref

(my-assert
 (aref (setq alpha (make-array 4)) 3)
 #+(or cmu sbcl) 0
 #+(or clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (setf (aref alpha 3) 'sirens)
 SIRENS)

(my-assert
 (aref alpha 3)
 SIRENS)

(my-assert
 (aref (setq beta (make-array '(2 4)
			      :element-type '(unsigned-byte 2)
			      :initial-contents '((0 1 2 3) (3 2 1 0))))
       1 2)
 1)

(my-assert
 (setq gamma '(0 2))
 (0 2))

(my-assert
 (apply #'aref beta gamma)
 2)

(my-assert
 (setf (apply #'aref beta gamma) 3)
 3)

(my-assert
 (apply #'aref beta gamma)
 3)

(my-assert
 (aref beta 0 2)
 3)

;;; array-dimension

(my-assert
 (array-dimension (make-array 4) 0)
 4)

(my-assert
 (array-dimension (make-array '(2 3)) 1)
 3)

;;; array-dimensions

(my-assert
 (array-dimensions (make-array 4))
 (4))

(my-assert
 (array-dimensions (make-array '(2 3)))
 (2 3))

(my-assert
 (array-dimensions (make-array 4 :fill-pointer 2))
 (4))

;;; array-element-type

(my-assert
 (array-element-type (make-array 4))
 T)

(my-assert
 (array-element-type (make-array 12 :element-type '(unsigned-byte 8)))
 #+(or cmu sbcl clisp) (unsigned-byte 8)
 #+ecls fixnum
 #-(or cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (array-element-type (make-array 12 :element-type '(unsigned-byte 5)))
 #+(or cmu sbcl clisp) (unsigned-byte 8)
 #+ecls fixnum
 #-(or cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (array-element-type (make-array 5 :element-type '(mod 5)))
 #+(or cmu sbcl clisp) (UNSIGNED-BYTE 4)
 #+ecls fixnum
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; (mod 5), (mod 8), fixnum, t, or any other type of which (mod 5) is a subtype.

;;; array-has-fill-pointer

(my-assert
 (array-has-fill-pointer-p (make-array 4))
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (array-has-fill-pointer-p (make-array '(2 3)))
 nil)

(my-assert
 (array-has-fill-pointer-p
  (make-array 8
	      :fill-pointer 2
	      :initial-element 'filler))
 t)

;;; array-displacement

(my-assert
 (progn
   (setq a1 (make-array 5))
   t)
 t)

(my-assert
 (progn
   (setq a2 (make-array 4 :displaced-to a1
                        :displaced-index-offset 1))
   t)
 t)

(my-assert
 (progn
   (multiple-value-bind (a b)
       (array-displacement a2)
     (list a b))
   t)
 t)

(my-assert
 (progn
   (setq a3 (make-array 2 :displaced-to a2
			:displaced-index-offset 2))
   t)
 t)

(my-assert
 (progn
   (array-displacement a3)
   t)
 t)

;;; array-in-bounds

(my-assert
 (progn
   (setq a (make-array '(7 11) :element-type 'string-char))
   t)
 t)

(my-assert
 (array-in-bounds-p a 0  0)
 t)

(my-assert
 (array-in-bounds-p a 6 10)
 t)

(my-assert
 (array-in-bounds-p a 0 -1)
 nil)

(my-assert
 (array-in-bounds-p a 0 11)
 nil)

(my-assert
 (array-in-bounds-p a 7  0)
 nil)

;;; array-rank

(my-assert
 (array-rank (make-array '()))
 0)

(my-assert
 (array-rank (make-array 4))
 1)

(my-assert
 (array-rank (make-array '(4)))
 1)

(my-assert
 (array-rank (make-array '(2 3)))
 2)

;;; array-row-major-index

(my-assert
 (progn
   (setq a (make-array '(4 7) :element-type '(unsigned-byte 8)))
   t)
 t)

(my-assert
 (array-row-major-index a 1 2)
 9)

(my-assert
 (array-row-major-index
  (make-array '(2 3 4)
	      :element-type '(unsigned-byte 8)
	      :displaced-to a
	      :displaced-index-offset 4)
  0 2 1)
 9)

;;; array-total-size

(my-assert
 (array-total-size (make-array 4))
 4)

(my-assert
 (array-total-size (make-array 4 :fill-pointer 2))
 4)

(my-assert
 (array-total-size (make-array 0))
 0)

(my-assert
 (array-total-size (make-array '(4 2)))
 8)

(my-assert
 (array-total-size (make-array '(4 0)))
 0)

(my-assert
 (array-total-size (make-array '()))
 1)

;;; arrayp

(my-assert
 (arrayp (make-array '(2 3 4) :adjustable t))
 t)

(my-assert
 (arrayp (make-array 6))
 t)

(my-assert
 (arrayp #*1011)
 t)

(my-assert
 (arrayp "hi")
 t)

(my-assert
 (arrayp 'hi)
 nil)

(my-assert
 (arrayp 12)
 nil)

;;; fill-pointer

(my-assert
 (setq a (make-array 8 :fill-pointer 4))
 #+(or cmu sbcl) #(0 0 0 0)
 #-(or cmu sbcl) #(NIL NIL NIL NIL))

(my-assert
 (fill-pointer a)
 4)

(my-assert
 (dotimes (i (length a)) (setf (aref a i) (* i i)))
 NIL)

(my-assert
 a
 #(0 1 4 9))

(my-assert
 (setf (fill-pointer a) 3)
 3)

(my-assert
 (fill-pointer a)
 3)

(my-assert
 a
 #(0 1 4))

(my-assert
 (setf (fill-pointer a) 8)
 8)

(my-assert
 a
 #+(or cmu sbcl) #(0 1 4 9 0 0 0 0)
 #-(or cmu sbcl) #(0 1 4 9 NIL NIL NIL NIL))

(my-assert
 (>= ARRAY-DIMENSION-LIMIT 1024)
 t)

(my-assert
 (>= ARRAY-RANK-LIMIT  8)
 t)

(my-assert
 (>= ARRAY-TOTAL-SIZE-LIMIT 1024)
 t)

;;; simple-vector-p

(my-assert
 (simple-vector-p (make-array 6))
 t)

(my-assert
 (simple-vector-p "aaaaaa")
 nil)

(my-assert
 (simple-vector-p (make-array 6 :fill-pointer t))
 nil)

;;; svref

(my-assert
 (simple-vector-p (setq v (vector 1 2 'sirens)))
 t)

(my-assert
 (svref v 0)
 1)

(my-assert
 (svref v 2)
 SIRENS)

(my-assert
 (setf (svref v 1) 'newcomer)
 NEWCOMER               )

(my-assert
 v
 #(1 NEWCOMER SIRENS))

;;; vector

(my-assert
 (arrayp (setq v (vector 1 2 'sirens)))
 t)

(my-assert
 (vectorp v)
 t)

(my-assert
 (simple-vector-p v)
 t         )

(my-assert
 (length v)
 3)

;;; vector-pop

(my-assert
 (vector-push (setq fable (list 'fable))
	      (setq fa (make-array 8
				   :fill-pointer 2
				   :initial-element 'sisyphus)))
 2 )

(my-assert
 (fill-pointer fa)
 3 )

(my-assert
 (eq (vector-pop fa) fable)
 t)

(my-assert
 (vector-pop fa)
 SISYPHUS )

(my-assert
 (fill-pointer fa)
 1 )

;;; vector-push

(my-assert
 (vector-push (setq fable (list 'fable))
	      (setq fa (make-array 8
				   :fill-pointer 2
				   :initial-element 'first-one)))
 2)

(my-assert
 (fill-pointer fa)
 3 )

(my-assert
 (eq (aref fa 2) fable)
 t)

(my-assert
 (vector-push-extend #\X
		     (setq aa
			   (make-array 5
				       :element-type 'character
				       :adjustable t
				       :fill-pointer 3)))
 3)

(my-assert
 (fill-pointer aa)
 4 )

(my-assert
 (vector-push-extend #\Y aa 4)
 4 )

(my-assert
 (>= (array-total-size aa) 5)
 t)

(my-assert
 (vector-push-extend #\Z aa 4)
 5 )

(my-assert
 (>= (array-total-size aa) 9)
 t)

;;; vectorp

(my-assert
 (vectorp "aaaaaa")
 t)

(my-assert
 (vectorp (make-array 6 :fill-pointer t))
 t)

(my-assert
 (vectorp (make-array '(2 3 4)))
 nil)

(my-assert
 (vectorp #*11)
 t)

(my-assert
 (vectorp #b11)
 nil)

;;; bit

(my-assert
 (bit (setq ba (make-array 8
			   :element-type 'bit
			   :initial-element 1))
      3)
 1)

(my-assert
 (setf (bit ba 3) 0)
 0)

(my-assert
 (bit ba 3)
 0)

(my-assert
 (sbit ba 5)
 1)

(my-assert
 (setf (sbit ba 5) 1)
 1)

(my-assert
 (sbit ba 5)
 1)

;;; bit-and etc

(my-assert
 (bit-and (setq ba #*11101010) #*01101011)
 #*01101010)

(my-assert
 (bit-and #*1100 #*1010)
 #*1000      )

(my-assert
 (bit-andc1 #*1100 #*1010)
 #*0010)

(my-assert
 (setq rba (bit-andc2 ba #*00110011 t))
 #*11001000)

(my-assert
 (eq rba ba)
 t)

(my-assert
 (bit-not (setq ba #*11101010))
 #*00010101)

(my-assert
 (setq rba (bit-not ba
		    (setq tba (make-array 8
					  :element-type 'bit))))
 #*00010101)

(my-assert
 (equal rba tba)
 t)

(my-assert
 (bit-xor #*1100 #*1010)
 #*0110)

;;; bit-vector-p

(my-assert
 (bit-vector-p (make-array 6
			   :element-type 'bit
			   :fill-pointer t))
 t)

(my-assert
 (bit-vector-p #*)
 t)

(my-assert
 (bit-vector-p (make-array 6))
 nil)

;;; simple-bit-vector

(my-assert
 (simple-bit-vector-p (make-array 6))
 nil)

(my-assert
 (simple-bit-vector-p #*)
 t)







