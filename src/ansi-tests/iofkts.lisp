;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

;; ****************************************************************************
;; *      test der i/o-funktionen                                             *
;; ****************************************************************************

#+xcl
(my-assert
 (progn (in-package (quote sys)) t)
 t)

;; --- let test ---------------------------------------------------------------
;;  ewiger compiler-fehler
;;

(my-assert
 (progn (setq bs (make-broadcast-stream)) t)
 t)

#+xcl
(my-assert
 *cur-broadcast-stream*
 nil)

(my-assert
 (print 123. bs)
 123.)

#+xcl
(my-assert
 *cur-broadcast-stream*
 nil)

;; -------------------------------------------------------------------------------
;;  unread test mit structure-stream
;;

(my-assert
 (setq str1 "test 123456")   "test 123456")

(my-assert
 (progn (setq s1 (make-two-way-stream (make-string-input-stream str1)
				      *standard-output*)) t)
 t)

(my-assert
 (read s1)   test)

(my-assert
 (read-char s1)   #\1)

(my-assert
 (read-char s1)   #\2)

(my-assert
 (unread-char #\2 s1)   nil)

(my-assert
 (read-char s1)   #\2)

(my-assert
 (read-char s1)   #\3)

(my-assert
 (read-char s1)   #\4)

(my-assert
 (unread-char #\a s1)   error
 "I just read #\4 I cannot put #\a back")

(my-assert
 (read-char s1)   #\5 "The last unread should have failed, we're
out of sync")

(my-assert
 (read-char s1)   #\6 "still out of sync?")

(my-assert
 (close s1)   t)

(my-assert
 str1   "test 123456")


;; -------------------------------------------------------------------------------

(my-assert
 (multiple-value-list (parse-integer "abc"))
 error)

(my-assert
 (multiple-value-list (parse-integer "  abc  "))
 error)

(my-assert
 (multiple-value-list (parse-integer "123"))
 (123 3))

(my-assert
 (multiple-value-list (parse-integer "  123  "))
 #-(or cmu sbcl)
 (123 7)
 #+(or cmu sbcl)
 (123 5))

(my-assert
 (multiple-value-list (parse-integer "123 t"))
 error)

(my-assert
 (multiple-value-list (parse-integer "  123   t  "))
 error)

(my-assert
 (multiple-value-list (parse-integer " ( 12 ) 43   t  "))
 error)

(my-assert
 (multiple-value-list (parse-integer "  abc  " :junk-allowed t))
 (nil 2))

(my-assert
 (multiple-value-list (parse-integer "123" :junk-allowed t))
 (123 3))

(my-assert
 (multiple-value-list (parse-integer "  123  " :junk-allowed t))
 (123 #+xcl 7
      #+(or clisp akcl allegro cmu sbcl ecls) 5
      #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(my-assert
 (multiple-value-list (parse-integer "123 t" :junk-allowed t))
 (123 #+xcl 4
      #+(or clisp akcl allegro cmu sbcl ecls) 3
      #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(my-assert
 (multiple-value-list (parse-integer "  123   t  " :junk-allowed t))
 (123 #+xcl 8
      #+(or clisp akcl allegro cmu sbcl ecls) 5
      #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown))

(my-assert
 (multiple-value-list (parse-integer " ( 12 ) 43   t  " :junk-allowed
				     t))
 (nil 1))

(my-assert
 (setq a "q w e 1 2 r 4 d : :;;;")
 "q w e 1 2 r 4 d : :;;;")

(my-assert
 (setq b "1 2 3 4 5 6 7")
 "1 2 3 4 5 6 7")

(my-assert
 (setq c "1.3 4.223")
 "1.3 4.223")

(my-assert
 (setq d "q w e r t z")
 "q w e r t z")

(my-assert
 (multiple-value-list (parse-integer a))
 error)

(my-assert
 (multiple-value-list (parse-integer b))
 error)

(my-assert
 (multiple-value-list (parse-integer c))
 error)

(my-assert
 (multiple-value-list (parse-integer d))
 error)

(my-assert
 (multiple-value-list (parse-integer a :start 4 :end 6))
 error)

(my-assert
 (multiple-value-list (parse-integer b :start 2 :end 3))
 (2 3))

(my-assert
 (multiple-value-list (parse-integer c :start 1))
 error)

(my-assert
 (multiple-value-list (parse-integer d :start 6))
 error)

(my-assert
 (multiple-value-list (parse-integer a :end 4))
 error)

(my-assert
 (multiple-value-list (parse-integer b :end 3))
 error)

(my-assert
 (multiple-value-list (parse-integer c :end 3))
 error)

(my-assert
 (multiple-value-list (parse-integer d :end 1))
 error)

(my-assert
 (multiple-value-list (parse-integer a :radix 1))
 error)

(my-assert
 (multiple-value-list (parse-integer b :radix 10))
 error)

(my-assert
 (multiple-value-list (parse-integer c :radix 20))
 error)

(my-assert
 (multiple-value-list (parse-integer d :radix 40))
 error)

(my-assert
 (multiple-value-list (parse-integer a :junk-allowed t))
 (nil 0))

(my-assert
 (multiple-value-list (parse-integer b :junk-allowed t))
 (1 #+xcl 2
    #+(or clisp akcl allegro cmu sbcl ecls) 1
    #-(or xcl clisp akcl allegro cmu sbcl) unknown))

(my-assert
 (multiple-value-list (parse-integer c :junk-allowed t))
 (1 1))

(my-assert
 (multiple-value-list (parse-integer d :junk-allowed t))
 (nil 0))

(my-assert
 (stream-element-type #+xcl stdin
		      #-xcl *terminal-io*)
 character)

(my-assert
 (progn (setq a (make-string-input-stream "aaa bbb")) t)
 t)

(my-assert
 (read a)
 aaa)

#+xcl
(my-assert
 (b-clear-input a)
 nil)

(my-assert
 (read a)
 #+xcl error
 #-xcl bbb)

(my-assert
 (progn (setq a (make-string-output-stream))
	(setq b (make-string-output-stream))
	(setq c (make-broadcast-stream a b)) t)
 t)

(my-assert
 (print "xxx" c)
 "xxx")

(my-assert
 (clear-output c)
 nil)

(my-assert
 (finish-output c)
 #+xcl t
 #-xcl nil)

(my-assert
 (get-output-stream-string a)
 "
\"xxx\" ")

(my-assert
 (get-output-stream-string b)
 "
\"xxx\" ")

(my-assert
 (print "yyy" c)
 "yyy")

(my-assert
 (clear-output c)
 nil)

(my-assert
 (finish-output c)
 #+xcl t
 #-xcl nil)

(my-assert
 (print "zzz" a)
 "zzz")

(my-assert
 (clear-output a)
 nil)

(my-assert
 (finish-output a)
 #+xcl t
 #-xcl nil)

(my-assert
 (get-output-stream-string a)
 #+xcl ""
 #-xcl "
\"yyy\" 
\"zzz\" ")

(my-assert
 (get-output-stream-string b)
 "
\"yyy\" ")

(my-assert
 (progn (setq a (make-string-input-stream "123")) t)
 t)

(my-assert
 (listen a)
 t)

(my-assert
 (read a)
 123)

(my-assert
 (listen a)
 nil)

(my-assert
 *print-case*
 :upcase)

(my-assert
 *print-gensym*
 t)

(my-assert
 *print-level*
 nil)

(my-assert
 *print-length*
 nil)

(my-assert
 *print-array*
 t)

(my-assert
 *print-escape*
 t)

(my-assert
 *print-pretty*
 nil)

(my-assert
 *print-circle*
 nil)

(my-assert
 *print-base*
 10)

(my-assert
 *print-radix*
 nil)

(my-assert
 (setq string1 "Das ist ein Test mit Print ")
 "Das ist ein Test mit Print ")

(my-assert
 (prin1-to-string string1)
 "\"das ist ein test mit print \"")

(my-assert
 (princ-to-string string1)
 "Das ist ein Test mit Print ")

(my-assert
 (progn (setq a (make-string-input-stream "123")) t)
 t)

(my-assert
 (read-char-no-hang a)
 #\1)

(my-assert
 (read a)
 23)

(my-assert
 (read-char-no-hang a)
 error)

(my-assert
 (read-char-no-hang a nil "EOF")
 "EOF")

(my-assert
 (progn (setq a (make-string-input-stream "1   2   ;32  abA"))
	(setq b (make-string-input-stream " 1 2 3 A x y z
a b c")) t)
 t)

(my-assert
 (read-delimited-list #\A b)
 (1 2 3))

(my-assert
 (setq c (multiple-value-list (read-line b)))
 (" x y z" nil))

(my-assert
 (length c)
 2)

(my-assert
 (multiple-value-list (read-line b))
 ("a b c" t))

(my-assert
 (multiple-value-list (read-line b))
 error)

(my-assert
 (multiple-value-list (read-line b nil "EOF"))
 ("EOF" t)
 "read-line &optional input-stream eof-error-p eof-value recursive-p

=> line, missing-newline-p
")

(my-assert
 (peek-char nil a)
 #\1)

(my-assert
 (read-char a)
 #\1)

(my-assert
 (peek-char t a)
 #\2)

(my-assert
 (read-char a)
 #\2)

(my-assert
 (peek-char t a)
 #\;)

(my-assert
 (read-char a)
 #\;)

(my-assert
 (peek-char #\A a)
 #\A)

(my-assert
 (read-char a)
 #\A)

(my-assert
 (peek-char nil a)
 error)

(my-assert
 (peek-char nil a nil "EOF")
 "EOF")

(my-assert
 (setq a (quote
	  ((berlin (dresden frankfurt bonn muenchen)) (mueller (karl luise dieter
								     aldo)))))
 ((berlin (dresden frankfurt bonn muenchen)) (mueller (karl luise dieter
							    aldo))))

(my-assert
 (progn (setq aa (make-string-input-stream "berlin d mueller :r")) t)
 t)

(my-assert
 (defun ask (&optional (res nil))
   "  (terpri)(terpri)(terpri)
  (print '(*** Eingabe des  Keywortes ***))
  (print '(- mit :r reset))
  (terpri)" (setq x (read aa)) "  (print x)" (cond
					      ((equal x (quote :r)) (cons "--- reset ---" res))
					      (t (cons (cadr (assoc x a)) (ask res)))))
 ask)

(my-assert
 (ask)
 ((dresden frankfurt bonn muenchen) nil (karl luise dieter aldo) "--- reset ---"))

(my-assert
 (setq string1 "Das ist ein Teststring")
 "Das ist ein Teststring")

(my-assert
 (setq string2 "Auch das 1 2 3 ist ein Teststring")
 "Auch das 1 2 3 ist ein Teststring")

(my-assert
 (multiple-value-list (read-from-string string1))
 (das 4))

(my-assert
 (multiple-value-list (read-from-string string2))
 (auch 5))

(my-assert
 (multiple-value-list (read-from-string string1 t nil :start 2))
 (s 4))

(my-assert
 (multiple-value-list
  (read-from-string string1 t nil :start 2 :preserve-whitespace t))
 (s 3))

(my-assert
 (multiple-value-list (read-from-string string2 t nil :start 5))
 (das 9))

(my-assert
 (multiple-value-list (read-from-string string2 t nil :start 5 :end
					6))
 (d 6))

(my-assert
 (multiple-value-list (read-from-string string1 t nil :start 4 :end
					3))
 error)

(my-assert
 (multiple-value-list (read-from-string string1 t nil :end 0))
 error)

(my-assert
 (multiple-value-list (read-from-string string1 t nil :start -2 :end
					0))
 error)

(my-assert
 (multiple-value-list (read-from-string string1 t nil :end 2))
 (da 2))

(my-assert
 *read-suppress*
 nil)

(my-assert
 (standard-char-p (quote a))
 error)

(my-assert
 (standard-char-p (quote #\backspace))
 #+xcl t
 #-xcl nil)

(my-assert
 (standard-char-p (quote #\tab))
 #+xcl t
 #-xcl nil)

(my-assert
 (standard-char-p (quote #\newline))
 t)

(my-assert
 (standard-char-p (quote #\page))
 #+xcl t
 #-xcl nil)

(my-assert
 (standard-char-p (quote #\return))
 #+xcl t
 #-xcl nil)

#-(or cmu sbcl sbcl)
(my-assert
 (string-char-p (quote a))
 error)

(my-assert
 (characterp (quote
	      #\space))
 t)

(my-assert
 (characterp (quote
	      #\newline))
 t)

(my-assert
 (characterp (quote
				#\backspace))
 t)

(my-assert
 (characterp (quote
	      #\a))
 t)

(my-assert
 (characterp (quote
	      #\8))
 t)

(my-assert
 (characterp (quote
	      #\-))
 t)

(my-assert
 (characterp (quote
				#\n))
 t)

(my-assert
 (characterp (quote
				#\())
 t)

(my-assert
 (stringp "das ist einer der Teststrings")
 t)

(my-assert
 (stringp (quote (das ist natuerlich falsch)))
 nil)

(my-assert
 (stringp "das ist die eine Haelfte" "und das die andere")
 error)

(my-assert
 (setq j 0)
 0)

(my-assert
 (with-input-from-string (s "animal crackers" :start 6) (read s))
 crackers)

(my-assert
 (with-input-from-string (s "animal crackers" :index j :start 6) (read s))
 crackers)

(my-assert
 j
 15)

(my-assert
 (with-input-from-string (s "animal crackers" :index j :start 7) (read s))
 crackers)

(my-assert
 j
 15)

(my-assert
 (with-input-from-string (s "animal crackers" :index j :start 2) (read s))
 imal)

(my-assert
 j
 7)

(my-assert
 (with-input-from-string (s "animal crackers" :index j :start 0 :end 6) (read s))
 animal)

(my-assert
 j
 6)

(my-assert
 (with-input-from-string (s "animal crackers"
			    :index j
			    :start 0 :end 12)
			 (read s))
 animal)

(my-assert
 j
 7)

(my-assert
 (with-input-from-string (s "animal crackers" :index j :start -1) (read s))
 error)

(my-assert
 j
 7)

(my-assert
 (with-input-from-string (s "animal crackers"
			    :index j
			    :start 6 :end 20)
			 (read s))
 #+xcl
 crackers
 #+(or clisp akcl allegro sbcl cmu ecls)
 error
 #-(or xcl clisp akcl allegro sbcl cmu ecls)
 unknown)

(my-assert
 j
 #+xcl
 20
 #+(or clisp akcl allegro sbcl cmu ecls)
 7
 #-(or xcl clisp akcl allegro sbcl cmu ecls)
 unknown)

(my-assert
 (setq a "Das ist wieder einmal einer der SUUPERTESTstrings.")
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (progn (setq b (make-string-output-stream)) t)
 t)

(my-assert
 (write-string a b)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (write-string a b :start 10)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (write-string a b :start 80)
 #+xcl "Das ist wieder einmal einer der SUUPERTESTstrings."
 #-xcl error)

(my-assert
 (write-string a b :end 5)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (write-string a b :end -2)
 error)

(my-assert
 (write-string a b :end 100)
 #+(or sbcl cmu xcl)
 "Das ist wieder einmal einer der SUUPERTESTstrings."
 #-(or sbcl cmu xcl)
 error)

(my-assert
 (write-string a b :start 5 :end 20)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (write-string a b :start 10 :end 5)
 #+xcl "Das ist wieder einmal einer der SUUPERTESTstrings."
 #-xcl error)

(my-assert
 (get-output-stream-string b)
 #+(or sbcl cmu xcl)
 "Das ist wieder einmal einer der SUUPERTESTstrings.eder einmal einer der SUUPERTESTstrings.Das iDas ist wieder einmal einer der SUUPERTESTstrings.st wieder einma"
 #+(or clisp akcl ecls)
 "Das ist wieder einmal einer der SUUPERTESTstrings.eder einmal einer der SUUPERTESTstrings.Das ist wieder einma"
 #-(or xcl clisp akcl sbcl cmu ecls)
 unknown)

(my-assert
 (write-string a b)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (length (get-output-stream-string b))
 50)

(my-assert
 (write-line a b)
 "Das ist wieder einmal einer der SUUPERTESTstrings.")

(my-assert
 (length (get-output-stream-string b))
 51)

(my-assert
 (with-output-to-string (s) (print (quote xxx) s))
 "
XXX ")

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   a)
 "")

(my-assert
  (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
    (with-output-to-string (s a) (princ 123 s)))
 123)

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
    a)
 "123")

(my-assert
  (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s)))
 4567)

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s))
   a)
 "1234567")

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s))
   (with-output-to-string (s a)
			  (princ 890 s)))
 890)

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s))
   (with-output-to-string (s a)
			  (princ 890 s))
   a)
 "1234567890")

(my-assert
 (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s))
   (with-output-to-string (s a)
			  (princ 890 s))
   (with-output-to-string (s a)
			  (princ (quote a) s)))
 error
 "All 10 characters are up. This should fail")

(my-assert
  (let ((a (make-array 10
		      :element-type 'character
		      :fill-pointer 0)))
   (with-output-to-string (s a) (princ 123 s))
   (with-output-to-string (s a) (princ 4567 s))
   (with-output-to-string (s a)
			  (princ 890 s))
   (ignore-errors
    (with-output-to-string (s a)
			   (princ (quote a) s)))
   a)
 "1234567890")

(my-assert
 (setq a
       (make-array 10 :element-type 'character
		   :fill-pointer 0
		   :adjustable t))
 "")

(my-assert
 (with-output-to-string (s a) (princ 123 s))
 123)

(my-assert
 a
 "123")

(my-assert
 (with-output-to-string (s a) (princ 4567 s))
 4567)

(my-assert
 a
 "1234567")

(my-assert
 (with-output-to-string (s a) (princ 890 s))
 890)

(my-assert
 a
 "1234567890")

(my-assert
 (with-output-to-string (s a) (princ (quote abcde) s))
 abcde)

(my-assert
 a
 "1234567890ABCDE")

(my-assert
 (with-output-to-string (s a) (princ (quote fghi) s))
 fghi)

(my-assert
 a
 "1234567890ABCDEFGHI")

(makunbound 'bs)
(makunbound 'a)
(makunbound 'b)
(makunbound 'c)
(makunbound 'd)
(makunbound 'aa)
(makunbound 'string1)
(makunbound 'string2)
(makunbound 'x)
(makunbound 'j)
(makunbound 's1)
(makunbound 'str1)

