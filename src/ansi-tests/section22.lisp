;;; section 22: printer -*- mode: lisp -*-
(in-package :cl-user)


;;; from : Raymond Toy <toy@rtp.ericsson.se>
(my-assert
 (format nil "~V,,,'-A" 10 "abc")
 "abc-------")
					; 0123456789

(my-assert
 (format nil "foo")
 "foo")

(my-assert
 (setq x 5)
 5)

(my-assert
 (format nil "The answer is ~D." x)
 "The answer is 5.")

(my-assert
 (format nil "The answer is ~3D." x)
 "The answer is   5.")

(my-assert
 (format nil "The answer is ~3,'0D." x)
 "The answer is 005.")

(my-assert
 (format nil "The answer is ~:D." (expt 47 x))
 "The answer is 229,345,007.")

(my-assert
 (setq y "elephant")
 "elephant")

(my-assert
 (format nil "Look at the ~A!" y)
 "Look at the elephant!")

(my-assert
 (setq n 3)
 3)

(my-assert
 (format nil "~D item~:P found." n)
 "3 items found.")

(my-assert
 (format nil "~R dog~:[s are~; is~] here." n (= n 1))
 "three dogs are here.")

(my-assert
 (format nil "~R dog~:*~[s are~; is~:;s are~] here." n)
 "three dogs are here.")

(my-assert
 (format nil "Here ~[are~;is~:;are~] ~:*~R pupp~:@P." n)
 "Here are three puppies.")

(my-assert
 (defun foo (x)
   (format nil "~6,2F|~6,2,1,'*F|~6,2,,'?F|~6F|~,2F|~F"
           x x x x x x))
 FOO)

(my-assert
 (foo 3.14159)
 "  3.14| 31.42|  3.14|3.1416|3.14|3.14159")

(my-assert
 (foo -3.14159)
 " -3.14|-31.42| -3.14|-3.142|-3.14|-3.14159")

(my-assert
 (foo 100.0)
 "100.00|******|100.00| 100.0|100.00|100.0")

(my-assert
 (foo 1234.0)
 "1234.00|******|??????|1234.0|1234.00|1234.0")

(my-assert
 (foo 0.006)
 "  0.01|  0.06|  0.01| 0.006|0.01|0.006")

(my-assert
 (defun foo (x)
   (format nil
           "~9,2,1,,'*E|~10,3,2,2,'?,,'$E|~
            ~9,3,2,-2,'%@E|~9,2E"
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
 #-(or cmu sbcl) "  1.10L+3| 11.00$+02|+.001L+06|  1.10L+3"
 #+(or cmu sbcl) "  1.10d+3| 11.00$+02|+.001d+06|  1.10d+3")

(my-assert
 (foo 1.1E13)
 "*********| 11.00$+12|+.001E+16| 1.10E+13")

(my-assert
 (foo 1.1L120)
 #-(or cmu sbcl) "*********|??????????|%%%%%%%%%|1.10L+120"
 #+(or cmu sbcl) "*********|??????????|%%%%%%%%%|1.10d+120")

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
 #-(or cmu sbcl) "  3.14L+3|314.2$+01|0.314L+04|  3.14L+3"
 #+(or cmu sbcl) "  3.14d+3|314.2$+01|0.314d+04|  3.14d+3")

(my-assert
 (foo 3.14E12)
 "*********|314.0$+10|0.314E+13| 3.14E+12")

(my-assert
 (foo 3.14L120)
 #-(or cmu sbcl) "*********|?????????|%%%%%%%%%|3.14L+120"
 #+(or cmu sbcl) "*********|?????????|%%%%%%%%%|3.14d+120")

(my-assert
 (format nil "~10<foo~;bar~>")
 "foo    bar")

(my-assert
 (format nil "~10:<foo~;bar~>")
 "  foo  bar")

(my-assert
 (format nil "~10<foobar~>")
 "    foobar")

(my-assert
 (format nil "~10:<foobar~>")
 "    foobar")

(my-assert
 (format nil "~10:@<foo~;bar~>")
 #+(or sbcl cmu ecls)
 " foo bar  "
 #+clisp
 "  foo bar "
 #-(or sbcl cmu clisp ecls)
 fill-this-in)
 
(my-assert
 (format nil "~10@<foobar~>")
 "foobar    ")

(my-assert
 (format nil "~10:@<foobar~>")
 "  foobar  ")

(my-assert
 (FORMAT NIL "Written to ~A." #P"foo.bin")
 "Written to foo.bin.")

