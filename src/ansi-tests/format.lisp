;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

;; ****************************************************************************
;; *      Rosenmueller            format.tst                                  *
;; ****************************************************************************

;;  ~< ------------------------------------------------------------------------
(my-assert
 (format nil "~10<foo~;bar~>")
 "foo    bar")

(my-assert
 (format nil "~10:<foo~;bar~>")
 "  foo  bar")

(my-assert
 (format nil "~10@<foo~;bar~>")
 "foo  bar  ")

(my-assert
 (format nil "~10:@<foo~;bar~>")
 #+(or XCL CLISP ALLEGRO) "  foo bar "
 #+(or AKCL cmu sbcl ecls) " foo bar  "
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl ecls) UNKNOWN)

(my-assert
 (format nil "~10<foobar~>")
 "    foobar")

(my-assert
 (format nil "~10:<foobar~>")
 "    foobar")

(my-assert
 (format nil "~10@<foobar~>")
 "foobar    ")

(my-assert
 (format nil "~10:@<foobar~>")
 "  foobar  ")

;;  ~< ~s ~^ ---------------------------------------------------------------------
(my-assert
 (format nil "~15<~S~>" 'foo)
 "            foo")

(my-assert
 (format nil "~15<~S~;~^~S~>" 'foo)
 "            foo")

(my-assert
 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo)
 "            foo")

(my-assert
 (format nil "~15<~S~;~^~S~>" 'foo 'bar)
 "foo         bar")

(my-assert
 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar)
 "foo         bar")

(my-assert
 (format nil "~15<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
 "foo   bar   baz")

(my-assert
 (format nil "~12<~S~;~^~S~;~^~S~>" 'foo 'bar 'baz)
 #+(or CLISP ALLEGRO) "foo  bar baz"
 #+(OR CMU SBCL) "foo bar  baz"
 #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (progn
   (setq liste '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
			 gggggggg
			 hhhhh iiii j kk lll mmmm nnnnnn oooooooooo ppppppppppppppp qqqqqqq
			 rrrrrrrrrrrr
			 s ttt uuuuuuuuu vvvvvvv wwwwwwwwww xxxxx yyyyyy zzzzzzzz)) ;26
   T)
 T)

(my-assert
 (format nil "~%;; ~<~%;; ~1:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"
	 ;; 2!
	 'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
	 'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
	 'qqqqqqq
	 'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
	 'zzzzzzzz)
 #+XCL
 "
;;  AAAAAAA  BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV   WWWWWWWWWW   XXXXX
"
 ;; 23456789;123456789;123456789;123456789;123456789;123456789;123456789;12
 #-XCL
 "
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
")

(my-assert
 (format nil "~%;; ~<~%;; ~1,50:; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~; ~s~;~
 ~s~; ~s~; ~s~; ~s~;~>~%"                               ; 2!
	 'aaaaaaa 'bbbbbb 'cccccccccccc 'dddddddddddddd 'eeee 'fffffffff 'gggggggg
	 'hhhhh 'iiii 'j 'kk 'lll 'mmmm 'nnnnnn 'oooooooooo 'ppppppppppppppp
	 'qqqqqqq
	 'rrrrrrrrrrrr 's 'ttt 'uuuuuuuuu 'vvvvvvv 'wwwwwwwwww 'xxxxx 'yyyyyy
	 'zzzzzzzz)
 #+XCL
 "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII JKK LLL
;;  MMMM    NNNNNN   OOOOOOOOOO   PPPPPPPPPPPPPPP
;;  QQQQQQQ  RRRRRRRRRRRR  S TTTUUUUUUUUU VVVVVVV
;;  WWWWWWWWWW                              XXXXX
"
 ;; 23456789;123456789;123456789;123456789;123456789;
 #-XCL
 "
;; 
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG HHHHH IIII JKK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ RRRRRRRRRRRR S TTTUUUUUUUUU VVVVVVV WWWWWWWWWW XXXXX
")

#-sbcl
(my-assert
 (defun format-blocksatz (stream parts prefix &optional line-length start-p end-p)
   (if (null stream)
       (let ((stream (make-string-output-stream)))
	 (format-blocksatz stream parts prefix line-length start-p end-p)
	 (get-output-stream-string stream)
	 )
       (unless (endp parts)
	 (setq line-length (or line-length #|(sys::line-length stream)|# 72))
	 (when start-p (format stream prefix))
	 (loop
	   ;;  Hier ist parts /= NIL
	   (let ((pos (#+CLISP sys::line-position
			       #+ALLEGRO excl::charpos
			       #+(OR CMU SBCL) cl::charpos stream))
		 (parts-now '()))
	     (let ((pos-now pos))
	       (loop
		 (when (endp parts) (return))
		 (let* ((part (first parts))
			(part-length (length part)))
		   (unless (null parts-now)
		     (when (> (+ pos-now part-length) line-length)
		       (return)
		       ) )
		   (pop parts)
		   (push part parts-now)
		   (incf pos-now part-length)
		   ) ) )
	     ;;  Hier ist parts-now /= NIL
	     (apply #'format
		    stream
		    (if (and (endp parts) (not end-p))
			(apply #'concatenate 'string
			       (make-list (length parts-now) :initial-element "~A")
			       )
			(concatenate 'string
				     "~"
				     (write-to-string (max 0 (- line-length pos))
						      :radix nil :base 10
						      )
				     (if (= (length parts-now) 1) "@" "")
				     "<"
				     (apply #'concatenate 'string
					    "~A"
					    (make-list (1- (length parts-now)) :initial-element "~;~A")
					    )
				     "~>"
				     ) )
		    (nreverse parts-now)
		    ) )
	   (when (endp parts) (return))
	   (format stream prefix)
	   ) ) ) )
 FORMAT-BLOCKSATZ)

#-sbcl
(my-assert
 (format-blocksatz nil
		   (mapcar #'(lambda (x) (format nil " ~A" x))
			   '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
				     gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
				     ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
				     wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
			   )
		   "~%;; "
		   nil t nil
		   )
 #+(or CLISP ALLEGRO)
 "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE FFFFFFFFF GGGGGGGG
;;  HHHHH  IIII  J KK LLL MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR   S  TTT  UUUUUUUUU  VVVVVVV  WWWWWWWWWW  XXXXX  YYYYYY
;;  ZZZZZZZZ"
 #+(OR CMU SBCL)
 "
;;  AAAAAAA BBBBBB CCCCCCCCCCCC DDDDDDDDDDDDDD EEEE  FFFFFFFFF  GGGGGGGG
;;  HHHHH IIII J KK LLL MMMM NNNNNN OOOOOOOOOO  PPPPPPPPPPPPPPP  QQQQQQQ
;;  RRRRRRRRRRRR  S  TTT  UUUUUUUUU  VVVVVVV  WWWWWWWWWW  XXXXX   YYYYYY
;;  ZZZZZZZZ"
 #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)
;; 123456789;123456789;123456789;123456789;123456789;123456789;123456789;12

#-sbcl
(my-assert
 (format-blocksatz nil
		   (mapcar #'(lambda (x) (format nil " ~A" x))
			   '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
				     gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
				     ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
				     wwwwwwwwww xxxxx yyyyyy zzzzzzzz)
			   )
		   "~%;; "
		   50 t t
		   )
 #+(or CLISP ALLEGRO)
 "
;;  AAAAAAA   BBBBBB  CCCCCCCCCCCC  DDDDDDDDDDDDDD
;;  EEEE  FFFFFFFFF  GGGGGGGG  HHHHH IIII J KK LLL
;;  MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR    S    TTT   UUUUUUUUU   VVVVVVV
;;  WWWWWWWWWW      XXXXX      YYYYYY     ZZZZZZZZ"
 #+(OR CMU SBCL)
 "
;;  AAAAAAA  BBBBBB  CCCCCCCCCCCC   DDDDDDDDDDDDDD
;;  EEEE FFFFFFFFF GGGGGGGG HHHHH IIII  J  KK  LLL
;;  MMMM NNNNNN OOOOOOOOOO PPPPPPPPPPPPPPP QQQQQQQ
;;  RRRRRRRRRRRR   S   TTT    UUUUUUUUU    VVVVVVV
;;  WWWWWWWWWW     XXXXX      YYYYYY      ZZZZZZZZ"
 #-(or CLISP ALLEGRO cmu sbcl) UNKNOWN)
;; 123456789;123456789;123456789;123456789;123456789;

;;; unklare Bedeutung (Fehler in Sprachbeschreibung?)
;;; (format nil "~%;; ~{~<~%;; ~1:; ~s~>~^,~}.~%" liste) ""
;;; (format nil "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%" liste) ""

(my-assert
 (format nil "~%;; ~{~<~%;; ~1,50:; ~s~>~^,~}.~%"
	 '(aaaaaaa bbbbbb cccccccccccc dddddddddddddd eeee fffffffff
		   gggggggg hhhhh iiii j kk lll mmmm nnnnnn oooooooooo
		   ppppppppppppppp qqqqqqq rrrrrrrrrrrr s ttt uuuuuuuuu vvvvvvv
		   wwwwwwwwww xxxxx yyyyyy zzzzzzzz))
 "
;;  AAAAAAA, BBBBBB, CCCCCCCCCCCC, DDDDDDDDDDDDDD,
;;  EEEE, FFFFFFFFF, GGGGGGGG, HHHHH, IIII, J, KK,
;;  LLL, MMMM, NNNNNN, OOOOOOOOOO,
;;  PPPPPPPPPPPPPPP, QQQQQQQ, RRRRRRRRRRRR, S,
;;  TTT, UUUUUUUUU, VVVVVVV, WWWWWWWWWW, XXXXX,
;;  YYYYYY, ZZZZZZZZ.
")

;;  ~f ------------------------------------------------------------------------
;;  Format F

(my-assert
 (DEFUN FOO (X)
   (FORMAT NIL "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F" X X X X
	   X X))
 FOO)

(my-assert
 (FOO 3.14159)
 ;;        "  3.14| 31.42|  3.14|3.1416|3.14|3.141590116672995328"
 "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")

(my-assert
 (FOO -3.14159)
 ;;        " -3.14|-31.42| -3.14|-3.142|-3.14|-3.141590116672995328"
 " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")

(my-assert
 (FOO 100.0)
 "100.00|******|100.00| 100.0|100.00|100.0")

(my-assert
 (FOO 1234.0)
 "1234.00|******|??????|1234.0|1234.00|1234.0")

(my-assert
 (FOO 0.006)
 "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(my-assert
 (format nil "~5,2,-13f" 1.1e13)
 " 1.10")

(my-assert
 (format nil "~9,0,6f" 3.14159)
 " 3141590.")

(my-assert
 (FORMAT NIL "~5D" (QUOTE A))
 "    A"
 "ANSI CL is not clear here whether the width is ignored or not,
but it makes more sense to print non-numeric arguments properly alighned")

(my-assert
 (FORMAT NIL "~5,3F" (QUOTE A))
 "    A"
 "ANSI CL is not clear here whether the width is ignored or not,
but it makes more sense to print non-numeric arguments properly alighned")

(my-assert
 (FORMAT NIL "~5,3F" #C(1.2 0.3))
 "#C(1.2 0.3)")

(my-assert
 (FORMAT NIL "~5,3F" 2/3)
 "0.667")

;;  ~e ----------------------------- ------------------------------------------
;;  Format E

(my-assert
 (defun foo (x)
   (format nil
	   "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~9,3,2,-2,'%@e|~9,2E"
	   x x x x))
 FOO)

(my-assert
 (foo 3.14159)
 "  3.14E+0| 31.42$-01|+.003E+03|  3.14E+0")

(my-assert
 (foo -3.14159)
 " -3.14E+0|-31.42$-01|-.003E+03| -3.14E+0")

(my-assert
 (foo 1100.0)
 "  1.10E+3| 11.00$+02|+.001E+06|  1.10E+3")

(my-assert
 (foo 1100.0L0)
 #+XCL "  1.10D+3| 11.00$+02|+.001D+06|  1.10D+3"
 #+(or CLISP AKCL) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
 #+(or ALLEGRO cmu sbcl) "  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (foo 1.1E13)
 "*********| 11.00$+12|+.001E+16| 1.10E+13")

;;  ERROR beim read der zahl (foo 1.1L120)

(my-assert
 (FORMAT NIL "_~10,4E_" 1.2)
 "_ 1.2000E+0_")

(my-assert
 (format nil "~9,2,1E" 0.0314159)
 "  3.14E-2")

;;  ~% ~d ~e (v) --------------------------------------------------------------
(my-assert
 (let (x)
   (dotimes (k 13 x)
     (setq x (cons (format nil "~%Scale factor ~2D: |~13,6,2,VE|"
			   (- k 5) (- k 5) 3.14159) x))))
 (
  "
Scale factor  7: | 3141590.E-06|" "
Scale factor  6: | 314159.0E-05|" "
Scale factor  5: | 31415.90E-04|" "
Scale factor  4: | 3141.590E-03|" "
Scale factor  3: | 314.1590E-02|" "
Scale factor  2: | 31.41590E-01|" "
Scale factor  1: | 3.141590E+00|" "
Scale factor  0: | 0.314159E+01|" "
Scale factor -1: | 0.031416E+02|" "
Scale factor -2: | 0.003142E+03|" "
Scale factor -3: | 0.000314E+04|" "
Scale factor -4: | 0.000031E+05|" "
Scale factor -5: | 0.000003E+06|"))


;;  ~g ------------------------------------------------------------------------
(my-assert
 (defun foo (x)
   (format nil "~9,2,1,,'*G|~9,3,2,3,'?,,'$G|~9,3,2,0,'%G|~9,2G"
	   x x x x))
 foo)

(my-assert
 (foo 0.0314159)
 "  3.14E-2|314.2$-04|0.314E-01|  3.14E-2")

(my-assert
 (foo 0.314159)
 "  0.31   |0.314    |0.314    | 0.31    ")

(my-assert
 (foo 3.14159)
 "   3.1   | 3.14    | 3.14    |  3.1    ")

(my-assert
 (foo 31.4159)
 "   31.   | 31.4    | 31.4    |  31.    ")

(my-assert
 (foo 314.159)
 "  3.14E+2| 314.    | 314.    |  3.14E+2")

(my-assert
 (foo 3141.59)
 "  3.14E+3|314.2$+01|0.314E+04|  3.14E+3")

(my-assert
 (foo 3141.59L0)
 #+XCL "  3.14D+3|314.2$+01|0.314D+04|  3.14D+3"
 #+(or CLISP AKCL) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
 #+(or ALLEGRO cmu sbcl) "  3.14d+3|314.2$+01|0.314d+04|  3.14d+3"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (foo 3.14E12)
 "*********|314.0$+10|0.314E+13| 3.14E+12")

;; (foo 3.14L120 und L1200) fehler in numerik

;;  ~a ------------------------------------------------------------------------

(my-assert
 (FORMAT NIL "foo")
 "foo")

(my-assert
 (FORMAT NIL "format-a:--~a--ende" (QUOTE AB\c))
 "format-a:--ABc--ende")

(my-assert
 (SETQ Y "elephant")
 "elephant")

(my-assert
 (FORMAT NIL "Look at the ~A!" Y)
 "Look at the elephant!")

(my-assert
 (FORMAT NIL "format-%:--~%--1-newline-*")
 "format-%:--
--1-newline-*")

(my-assert
 (FORMAT NIL "format-%:--~3%--3-newline-*")
 "format-%:--


--3-newline-*")

(my-assert
 (FORMAT NIL "format-a:--~5a--ende-*" (QUOTE AB\c))
 "format-a:--ABc  --ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2a--ende-*" (QUOTE AB\c))
 "format-a:--ABc  --ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2,3a--ende-*" (QUOTE AB\c))
 "format-a:--ABc   --ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2,3,'*a--ende-*" (QUOTE AB\c))
 "format-a:--ABc***--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~@a--ende-*" (QUOTE AB\c))
 "format-a:--ABc--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5@a--ende-*" (QUOTE AB\c))
 "format-a:--  ABc--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2@a--ende-*" (QUOTE AB\c))
 "format-a:--  ABc--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2,3@a--ende-*" (QUOTE AB\c))
 "format-a:--   ABc--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~5,2,3,'*@a--ende-*" (QUOTE AB\c))
 "format-a:--***ABc--ende-*")

(my-assert
 (FORMAT NIL "format-a:--~:a--ende-*" (QUOTE (AB\c NIL XYZ)))
 "format-a:--(ABc NIL XYZ)--ende-*")

(my-assert
 (FORMAT NIL "format-s:--~s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c --ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c  --ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2,3s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c   --ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|   --ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2,3,'*s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c***--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|***--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~@s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5@s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:-- AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2@s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--  AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2,3@s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--   AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--   |ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~5,2,3,'*@s--ende-*" (QUOTE AB\c))
 #+XCL "format-s:--***AB\\c--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--***|ABc|--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (FORMAT NIL "format-s:--~:s--ende-*" (QUOTE (AB\c NIL XYZ)))
 #+XCL "format-s:--(AB\\c NIL XYZ)--ende-*"
 #+(or CLISP AKCL ALLEGRO cmu sbcl) "format-s:--(|ABc| NIL XYZ)--ende-*"
 #-(or XCL CLISP AKCL ALLEGRO cmu sbcl) UNKNOWN)

(my-assert
 (SETQ X 5)
 5)

(my-assert
 (FORMAT NIL "The answer is ~D." X)
 "The answer is 5.")

(my-assert
 (FORMAT NIL "The answer is ~3D." X)
 "The answer is   5.")

(my-assert
 (FORMAT NIL "The answer is ~3,'0D." X)
 "The answer is 005.")

(my-assert
 (FORMAT NIL "The answer is ~:D." (EXPT 47 X))
 "The answer is 229,345,007.")

(my-assert
 (FORMAT NIL "decimal:~d, width=5:~5d-*" 10 10)
 "decimal:10, width=5:   10-*")

(my-assert
 (FORMAT NIL "format-d:--~d--ende-*" 123)
 "format-d:--123--ende-*")

(my-assert
 (FORMAT NIL "format-d:--~10d--ende-*" 123)
 "format-d:--       123--ende-*")

(my-assert
 (FORMAT NIL "format-d:--~10,'?d--ende-*" 123)
 "format-d:--???????123--ende-*")

(my-assert
 (FORMAT NIL "format-d:--~@d--ende-*" 123)
 "format-d:--+123--ende-*")

(my-assert
 (FORMAT NIL "format-d:--~10@d--ende-*" 123)
 "format-d:--      +123--ende-*")

(my-assert
 (FORMAT NIL "format-d:--~10,'?@d--ende-*" 123)
 "format-d:--??????+123--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~b--ende-*" 123)
 "format-b:--1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10b--ende-*" 123)
 "format-b:--   1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10,'?b--ende-*" 123)
 "format-b:--???1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~:b--ende-*" 123)
 "format-b:--1,111,011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10:b--ende-*" 123)
 "format-b:-- 1,111,011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10,'?:b--ende-*" 123)
 "format-b:--?1,111,011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10,'?,'.:b--ende-*" 123)
 "format-b:--?1.111.011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~@b--ende-*" 123)
 "format-b:--+1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10@b--ende-*" 123)
 "format-b:--  +1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~10,'?@b--ende-*" 123)
 "format-b:--??+1111011--ende-*")

(my-assert
 (FORMAT NIL "format-b:--~:@b--ende-*" 123)
 "format-b:--+1,111,011--ende-*")

(my-assert
 (FORMAT NIL "format-o:--~o--ende-*" 123)
 "format-o:--173--ende-*")

(my-assert
 (FORMAT NIL "format-o:--~10o--ende-*" 123)
 "format-o:--       173--ende-*")

(my-assert
 (FORMAT NIL "format-o:--~10,'?o--ende-*" 123)
 "format-o:--???????173--ende-*")

(my-assert
 (FORMAT NIL "format-o:--~@o--ende-*" 123)
 "format-o:--+173--ende-*")

(my-assert
 (FORMAT NIL "format-o:--~10@o--ende-*" 123)
 "format-o:--      +173--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~x--ende-*" 123)
 "format-x:--7B--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~10x--ende-*" 123)
 "format-x:--        7B--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~10,'?x--ende-*" 123)
 "format-x:--????????7B--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~10:x--ende-*" 123)
 "format-x:--        7B--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~@x--ende-*" 123)
 "format-x:--+7B--ende-*")

(my-assert
 (FORMAT NIL "format-x:--~10@x--ende-*" 123)
 "format-x:--       +7B--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~20r--ende-*" 123)
 "format-r:--63--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~20,10r--ende-*" 123)
 "format-r:--        63--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~20@r--ende-*" 123)
 "format-r:--+63--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~r--ende-*" 9)
 "format-r:--nine--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~:r--ende-*" 9)
 "format-r:--ninth--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~@r--ende-*" 9)
 "format-r:--IX--ende-*")

(my-assert
 (FORMAT NIL "format-r:--~:@r--ende-*" 9)
 "format-r:--VIIII--ende-*")

(my-assert
 (FORMAT NIL "format-p:--~d  object~p-*" 1 1)
 "format-p:--1  object-*")

(my-assert
 (FORMAT NIL "format-p:--~d  object~p-*" 2 2)
 "format-p:--2  objects-*")

(my-assert
 (FORMAT NIL "format-p:--~d  bab~@p-*" 1 1)
 "format-p:--1  baby-*")

(my-assert
 (FORMAT NIL "format-p:--~d  bab~@p-*" 2 2)
 "format-p:--2  babies-*")

(my-assert
 (FORMAT NIL "format-p:--~d  object~:p-*" 1)
 "format-p:--1  object-*")

(my-assert
 (FORMAT NIL "format-p:--~d  object~:p-*" 2)
 "format-p:--2  objects-*")

(my-assert
 (FORMAT NIL "format-p:--~d  bab~:@p-*" 1)
 "format-p:--1  baby-*")

(my-assert
 (FORMAT NIL "format-&:--~%~&--1-newline-*")
 "format-&:--
--1-newline-*")

(my-assert
 (FORMAT NIL "format-&:--~%~3&--3-newline-*")
 "format-&:--


--3-newline-*")

(my-assert
 (FORMAT NIL "format-tilde:--~~--1-tilde-*")
 "format-tilde:--~--1-tilde-*")

(my-assert
 (FORMAT NIL "format-tilde:--~3~--3-tilden-*")
 "format-tilde:--~~~--3-tilden-*")

(my-assert
 (FORMAT NIL "format-|:--~|--1-ff-*")
 "format-|:----1-ff-*")

(my-assert
 (FORMAT NIL "format-|:--~2|--2-ff-*")
 "format-|:----2-ff-*")

(my-assert
 (FORMAT NIL
	 "format-<nl>:~
                         gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*")
 "format-<nl>:gl. zeile gl. angeschlossen trotz 2*<tab> und sp-*")

(my-assert
 (FORMAT NIL "format-<nl>:~@
                         neue Zeile Anfang trotz <tab> + sp-*")
 "format-<nl>:
neue Zeile Anfang trotz <tab> + sp-*")

(my-assert
 (FORMAT NIL "format-<nl>:~:
	gleiche Zeile aber ein tab vor Anfang-*")
 "format-<nl>:	gleiche Zeile aber ein tab vor Anfang-*")

(my-assert
 (FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++~s+++~s+++" (QUOTE
								(A B)) 2)
 "format-?:***1***+++A+++B+++***2***-*")

(my-assert
 (FORMAT NIL "format-?:***~a***~?***~a***-*" 1 "+++++++++++++" NIL 2)
 "format-?:***1***+++++++++++++***2***-*")

(my-assert
 (FORMAT NIL "~(AAAAAAAA BBBBBB ccccccc dddddddd~)")
 "aaaaaaaa bbbbbb ccccccc dddddddd")

(my-assert
 (FORMAT NIL "~:(AAAAAAAA BBBBBB ccccccc dddddddd~)")
 "Aaaaaaaa Bbbbbb Ccccccc Dddddddd")

(my-assert
 (FORMAT NIL "~@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
 "Aaaaaaaa bbbbbb ccccccc dddddddd")

(my-assert
 (FORMAT NIL "~:@(AAAAAAAA BBBBBB ccccccc dddddddd~)")
 "AAAAAAAA BBBBBB CCCCCCC DDDDDDDD")

(my-assert
 (FORMAT NIL "++~{-=~s=-~}++" (QUOTE (1 2 3)))
 "++-=1=--=2=--=3=-++")

(my-assert
 (FORMAT NIL "++~2{-=~s=-~}++" (QUOTE (1 2 3)))
 "++-=1=--=2=-++")

(my-assert
 (FORMAT NIL "++~@{-=~s=-~}++" 1 2 3)
 "++-=1=--=2=--=3=-++")

(my-assert
 (FORMAT NIL "++~:{-=~s=~s=-~}++" (QUOTE ((1 2) (3 4 5) (6 7))))
 "++-=1=2=--=3=4=--=6=7=-++")

(my-assert
 (FORMAT NIL "++~:@{-=~s=~s=-~}++" (QUOTE (1 2)) (QUOTE (3 4 5)) (QUOTE
								  (6 7)))
 "++-=1=2=--=3=4=--=6=7=-++")

(my-assert
 (FORMAT NIL "~{abc~:}")
 #+XCL "abc"
 #-XCL ERROR)

(my-assert
 (FORMAT NIL "~{~:}" "xyz")
 #+XCL "xyz"
 #-XCL ERROR)

(my-assert
 (FORMAT NIL "~1{~:}" "-~s-" (QUOTE (1 2)) 3)
 "-1-")

(my-assert
 (FORMAT NIL "123456789012345678901234567890
~10,4txx~10,4ty~10,4tzzz~10,4tende")
 #+XCL
 "123456789012345678901234567890
         xx  y   zzz ende"
 #-XCL
 "123456789012345678901234567890
          xx  y   zzz ende")

(my-assert
 (FORMAT NIL "123456789012345678901234567890
~3,4@txx~3,4@ty~3,4@tzzz~3,4@tende")
 #+XCL
 "123456789012345678901234567890
   xx      y   zzz     ende"
 #-XCL
 "123456789012345678901234567890
    xx      y   zzz     ende")

(my-assert
 (FORMAT NIL "-~a-~a-~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-3-4-")

(my-assert
 (FORMAT NIL "-~a-~a-~*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-4-5-")

(my-assert
 (FORMAT NIL "-~a-~a-~3*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-6-7-")

(my-assert
 (FORMAT NIL "-~a-~a-~:*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-2-3-")

(my-assert
 (FORMAT NIL "-~a-~a-~2:*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-1-2-")

(my-assert
 (FORMAT NIL "-~a-~a-~@*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-1-2-")

(my-assert
 (FORMAT NIL "-~a-~a-~6@*~a-~a-" 1 2 3 4 5 6 7 8 9)
 "-1-2-7-8-")

(my-assert
 (FORMAT NIL "~[aa~;bb~;cc~]" 1)
 "bb")

(my-assert
 (FORMAT NIL "~[aa~;bb~;cc~]" 10)
 "")

(my-assert
 (FORMAT NIL "~2[aa~;bb~;cc~]" 10)
 "cc")

(my-assert
 (FORMAT NIL "~@[aaa~]" NIL 10)
 "")

(my-assert
 (FORMAT NIL "~@[aaa~]" 20 10)
 "aaa")

(my-assert
 (FORMAT NIL "~@[aaa~d~]" NIL 10)
 "")

(my-assert
 (FORMAT NIL "~@[aaa~d~]" 20 10)
 "aaa20")

(my-assert
 (FORMAT NIL "~@[aaa~d~]bbb~d" NIL 10 30)
 "bbb10")

(my-assert
 (FORMAT NIL "~@[aaa~d~]bbb~d" 20 10 30)
 "aaa20bbb10")

(my-assert
 (FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" NIL 10 20)
 "-nil--ende10")

(my-assert
 (FORMAT NIL "~:[-nil-~;-true-~d~]-ende~d" T 10 20)
 "-true-10-ende20")

(my-assert
 (FORMAT NIL "Start test, newline:~%freshline:~&")
 "Start test, newline:
freshline:
")

(my-assert
 (FORMAT NIL "decimal pad with period:~10,vd-*" #\. 12)
 "decimal pad with period:........12-*")

(my-assert
 (FORMAT NIL "char normal:~c, as ~%# would read:~%~@c, human read:~:c-*"
	 #\SPACE
	 #\SPACE #\SPACE)
 #+(or XCL cmu sbcl CLISP) "char normal: , as 
# would read:
#\\Space, human read:Space-*"
 #+(or AKCL LUCID)    "char normal:Space, as 
# would read:
#\\Space, human read:Space-*"
 #+ALLEGRO            "char normal: , as 
# would read:
#\\space, human read:space-*"
 #-(or XCL cmu sbcl CLISP AKCL LUCID ALLEGRO) UNKNOWN)

(my-assert
 (FORMAT NIL
	 "cardinal:~r, roman new:~@r, roman-old:~:@r~
                <same line I hope>~@
                new line but at beginning~:
   same line, but spaced out~@
        new line and over two tabs-*" 4 4 4)
 "cardinal:four, roman new:IV, roman-old:IIII<same line I hope>
new line but at beginning   same line, but spaced out
new line and over two tabs-*")

(my-assert
 (SETQ N 3)
 3)

(my-assert
 (FORMAT NIL "~D item~:P found." N)
 "3 items found.")

(my-assert
 (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
 "three dogs are here.")

(my-assert
 (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
 "three dogs are here.")

(my-assert
 (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
 "Here are three puppies.")

(my-assert
 (SETQ N 1)
 1)

(my-assert
 (FORMAT NIL "~D item~:P found." N)
 "1 item found.")

(my-assert
 (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
 "one dog is here.")

(my-assert
 (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
 "one dog is here.")

(my-assert
 (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
 "Here is one puppy.")

(my-assert
 (SETQ N 0)
 0)

(my-assert
 (FORMAT NIL "~D item~:P found." N)
 "0 items found.")

(my-assert
 (FORMAT NIL "~R dog~:[s are~; is~] here." N (= N 1))
 "zero dogs are here.")

(my-assert
 (FORMAT NIL "~R dog~:*~[s are~; is~:;s are~] here." N)
 "zero dogs are here.")

(my-assert
 (FORMAT NIL "Here ~[are~;is~:;are~] ~:*~R pupp~:@p." N)
 "Here are zero puppies.")

(my-assert
 (FORMAT NIL "~D tr~:@p/~D win~:P" 7 1)
 "7 tries/1 win")

(my-assert
 (FORMAT NIL "~D tr~:@p/~D win~:P" 1 0)
 "1 try/0 wins")

(my-assert
 (FORMAT NIL "~D tr~:@p/~D win~:P" 1 3)
 "1 try/3 wins")

(my-assert
 (DEFUN TYPE-CLASH-ERROR (FN NARGS ARGNUM RIGHT-TYPE WRONG-TYPE) (FORMAT
								  NIL
								  "~&~S requires itts ~:[~:R~;~*~] ~
           argument to be of type ~S,~%but it was called ~
           with an argument of type ~S.-*" FN (EQL NARGS 1) ARGNUM
								  RIGHT-TYPE
								  WRONG-TYPE))
 TYPE-CLASH-ERROR)

(my-assert
 (TYPE-CLASH-ERROR (QUOTE AREF) NIL 2 (QUOTE INTEGER) (QUOTE VECTOR))
 "AREF requires itts second argument to be of type INTEGER,
but it was called with an argument of type VECTOR.-*")

(my-assert
 (TYPE-CLASH-ERROR (QUOTE CAR) 1 1 (QUOTE LIST) (QUOTE SHORT-FLOAT))
 "CAR requires itts  argument to be of type LIST,
but it was called with an argument of type SHORT-FLOAT.-*")

(my-assert
 (FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE ("Foo" 5)) 7)
 "<Foo 5> 7")

(my-assert
 (FORMAT NIL "~? ~D" "<~A ~D>" (QUOTE (" Foo" 5 14)) 7)
 "< Foo 5> 7")

(my-assert
 (FORMAT NIL "~@? ~d" "<~A ~D>" "Foo" 5 7)
 "<Foo 5> 7")

(my-assert
 (FORMAT NIL "~@? ~D" "<~A ~D>" "Foo" 5 14 7)
 "<Foo 5> 14")

(my-assert
 (FORMAT NIL "~@R ~(~@R~)" 14 14)
 "XIV xiv")

(my-assert
 (DEFUN F (N) (FORMAT NIL "~@(~R~) error~:P detected." N))
 F)

(my-assert
 (F 0)
 "Zero errors detected.")

(my-assert
 (F 1)
 "One error detected.")

(my-assert
 (F 23)
 "Twenty-three errors detected.")

(my-assert
 (SETQ *PRINT-LEVEL* NIL *PRINT-LENGTH* 5)
 5)

(my-assert
 (FORMAT NIL "~@[ print level = ~D~]~@[ print length = ~D~]" *PRINT-LEVEL*

	 *PRINT-LENGTH*)
 " print length = 5")

(my-assert
 (SETQ *PRINT-LENGTH* NIL)
 NIL)

(my-assert
 (SETQ FOO
       "Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~].")
 "Items:~#[none~; ~s~; ~S and ~S~
          ~:;~@{~#[~; and~] ~S~^,~}~].")

(my-assert
 (FORMAT NIL FOO)
 "Items:none.")

(my-assert
 (FORMAT NIL FOO (QUOTE FOO))
 "Items: FOO.")

(my-assert
 (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR))
 "Items: FOO and BAR.")

(my-assert
 (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ))
 "Items: FOO, BAR, and BAZ.")

(my-assert
 (FORMAT NIL FOO (QUOTE FOO) (QUOTE BAR) (QUOTE BAZ) (QUOTE QUUX))
 "Items: FOO, BAR, BAZ, and QUUX.")

(my-assert
 (FORMAT NIL "The winners are:~{ ~S~}." (QUOTE (FRED HARRY JILL)))
 "The winners are: FRED HARRY JILL.")

(my-assert
 (FORMAT NIL "Pairs:~{ <~S,~S>~}." (QUOTE (A 1 B 2 C 3)))
 "Pairs: <A,1> <B,2> <C,3>.")

(my-assert
 (FORMAT NIL "Pairs:~:{ <~S,~S>~}." (QUOTE ((A 1) (B 2) (C 3))))
 "Pairs: <A,1> <B,2> <C,3>.")

(my-assert
 (FORMAT NIL "Pairs:~@{ <~S,~S>~}." (QUOTE A) 1 (QUOTE B) 2 (QUOTE C)
	 3)
 "Pairs: <A,1> <B,2> <C,3>.")

(my-assert
 (FORMAT NIL "Pairs:~:@{ <~S,~S>~}." (QUOTE (A 1)) (QUOTE (B 2)) (QUOTE
								  (C 3)))
 "Pairs: <A,1> <B,2> <C,3>.")

(my-assert
 (SETQ DONESTR "done.~^ ~D warning~:P.~^ ~D error~:P.")
 "done.~^ ~D warning~:P.~^ ~D error~:P.")

(my-assert
 (FORMAT NIL DONESTR)
 "done.")

(my-assert
 (FORMAT NIL DONESTR 3)
 "done. 3 warnings.")

(my-assert
 (FORMAT NIL DONESTR 1 5)
 "done. 1 warning. 5 errors.")

(my-assert
 (SETQ TELLSTR "~@(~@[~R~]~^ ~A.~)")
 "~@(~@[~R~]~^ ~A.~)")

(my-assert
 (FORMAT NIL TELLSTR 23)
 "Twenty-three")

(my-assert
 (FORMAT NIL TELLSTR NIL "losers")
 " Losers.")

(my-assert
 (FORMAT NIL TELLSTR 23 "losers")
 "Twenty-three losers.")

(my-assert
 (FORMAT NIL "**~c**" #\SPACE)
 #+(or XCL cmu sbcl CLISP ALLEGRO) "** **"
 #+(or AKCL LUCID)            "**Space**"
 #-(or XCL cmu sbcl CLISP AKCL LUCID ALLEGRO) UNKNOWN)

(my-assert
 (FORMAT NIL "**~:c**" #\SPACE)
 "**Space**")

(my-assert
 (FORMAT NIL "**~:@c**" #\SPACE)
 "**Space**")

(my-assert
 (FORMAT NIL "**~@c**" #\SPACE)
 "**#\\Space**")

(my-assert
 (FORMAT NIL "**~c**" #\A)
 "**A**")

(my-assert
 (FORMAT NIL "**~:c**" #\A)
 "**A**")

(my-assert
 (FORMAT NIL "**~:@c**" #\A)
 "**A**")

(my-assert
 (FORMAT NIL "**~@c**" #\A)
 "**#\\A**")

#+XCL
(my-assert
 (FORMAT NIL "**~c**" (CODE-CHAR 26))
 "****")

#+clisp
(my-assert
 (FORMAT NIL "**~c**" (CODE-CHAR 27))
 "****")

#+XCL
(my-assert
 (FORMAT NIL "**~:c**" (CODE-CHAR 26))
 "**Z**")

#+clisp
(my-assert
 (FORMAT NIL "**~:c**" (CODE-CHAR 27))
 "**Escape**")

#+XCL
(my-assert
 (FORMAT NIL "**~:@c**" (CODE-CHAR 26))
 "**^Z**")

#+clisp
(my-assert
 (FORMAT NIL "**~:@c**" (CODE-CHAR 27))
 "**Escape**")

#+XCL
(my-assert
 (FORMAT NIL "**~@c**" (CODE-CHAR 26))
 "**#\\**")

#+clisp
(my-assert
 (FORMAT NIL "**~@c**" (CODE-CHAR 27))
 "**#\\Escape**")

(my-assert
 (progn (fmakunbound 'foo)
	(makunbound 'liste)
	t)
 T)

