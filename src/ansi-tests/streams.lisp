;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

#+xcl
(my-assert
 (progn (in-package 'sys) t)
 t)

#-(or akcl allegro)
(my-assert
 (prin1-to-string (make-broadcast-stream))
 #+xcl "#<%TYPE-STRUCTURE-STREAM NIL>"
 #+clisp "#<BROADCAST-STREAM>"
 #+(or cmu sbcl) "#<Broadcast Stream>"
 #+ecls "#<broadcast stream 8>"
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (progn (setq s1 (open "d1.plc" :direction :output))
	(setq s2 (open "d2.plc" :direction :output))
	(setq s3 (open "d3.plc" :direction :output))
	(setq b1 (make-broadcast-stream s1 s2 s3 *standard-output*)) t)
 t)

(my-assert
 (print "test broadcast satz 1" b1)
 "test broadcast satz 1")

(my-assert
 (print "test broadcast satz 2" b1)
 "test broadcast satz 2")

(my-assert
 (print "test broadcast satz 3" b1)
 "test broadcast satz 3")

(my-assert
 (close s1)
 t)

(my-assert
 (close s2)
 t)

(my-assert
 (close s3)
 t)

(my-assert
 (progn (setq s (open "d1.plc")) t)
 t)

(my-assert
 (read s)
 "test broadcast satz 1")

(my-assert
 (read s)
 "test broadcast satz 2")

(my-assert
 (read s)
 "test broadcast satz 3")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d2.plc")) t)
 t)

(my-assert
 (read s)
 "test broadcast satz 1")

(my-assert
 (read s)
 "test broadcast satz 2")

(my-assert
 (read s)
 "test broadcast satz 3")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d3.plc")) t)
 t)

(my-assert
 (read s)
 "test broadcast satz 1")

(my-assert
 (read s)
 "test broadcast satz 2")

(my-assert
 (read s)
 "test broadcast satz 3")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t0.plc" :direction :output)) t)
 t)

(my-assert
 (print (quote read1) s)
 read1)

(my-assert
 (print (quote read2) s)
 read2)

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq inptw (open "t0.plc"))
	(setq s1 (open "d1.plc" :direction :output))
	(setq s2 (open "d2.plc" :direction :output))
	(setq sy (make-synonym-stream (quote s2)))
	(setq s3 (open "d3.plc" :direction :output))
	(setq tw (make-two-way-stream inptw s3))
	(setq s4 (open "d4.plc" :direction :output))
	(setq ec (make-echo-stream inptw s4))
	(setq s5 (open "d5.plc" :direction :output))
	(setq s6 (open "d6.plc" :direction :output))
	(setq b1 (make-broadcast-stream s5 s6))
	(setq s7 (open "d7.plc" :direction :output))
	(setq b2 (make-broadcast-stream s1 sy tw ec b1 s7)) t)
 t)

(my-assert
 (print "w to b2 1.satz" b2)
 "w to b2 1.satz")

(my-assert
 (print "w to sy" sy)
 "w to sy")

(my-assert
 (print "w to b2 2.satz" b2)
 "w to b2 2.satz")

(my-assert
 (print "w to tw" tw)
 "w to tw")

(my-assert
 (print "w to b2 3.satz" b2)
 "w to b2 3.satz")

(my-assert
 (print "w to ec" ec)
 "w to ec")

(my-assert
 (print "w to b2 4.satz" b2)
 "w to b2 4.satz")

(my-assert
 (print "w to b1" b1)
 "w to b1")

(my-assert
 (print "w to b2 5.satz" b2)
 "w to b2 5.satz")

(my-assert
 (print "w to s7" s7)
 "w to s7")

(my-assert
 (print "w to b2 6.satz" b2)
 "w to b2 6.satz")

(my-assert
 (read tw)
 read1)

(my-assert
 (read ec)
 read2)

(my-assert
 (print "w to b2 7.satz" b2)
 "w to b2 7.satz")

(my-assert
 (print "w to b2 8.satz" b2)
 "w to b2 8.satz")

(my-assert
 (close inptw)
 t)

(my-assert
 (close s1)
 t)

(my-assert
 (close s2)
 t)

(my-assert
 (close s3)
 t)

(my-assert
 (close s4)
 t)

(my-assert
 (close s5)
 t)

(my-assert
 (close s6)
 t)

(my-assert
 (close s7)
 t)

(my-assert
 (progn (setq s (open "d1.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d2.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to sy")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d3.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to tw")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d4.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to ec")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 read2)

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d5.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b1")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d6.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b1")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "d7.plc")) t)
 t)

(my-assert
 (read s)
 "w to b2 1.satz")

(my-assert
 (read s)
 "w to b2 2.satz")

(my-assert
 (read s)
 "w to b2 3.satz")

(my-assert
 (read s)
 "w to b2 4.satz")

(my-assert
 (read s)
 "w to b2 5.satz")

(my-assert
 (read s)
 "w to s7")

(my-assert
 (read s)
 "w to b2 6.satz")

(my-assert
 (read s)
 "w to b2 7.satz")

(my-assert
 (read s)
 "w to b2 8.satz")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t1.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t1" s)
 "1.satz t1")

(my-assert
 (print "2.satz t1" s)
 "2.satz t1")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t2.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t2" s)
 "1.satz t2")

(my-assert
 (print "2.satz t2" s)
 "2.satz t2")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t3.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t3" s)
 "1.satz t3")

(my-assert
 (print "2.satz t3" s)
 "2.satz t3")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t4.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t4" s)
 "1.satz t4")

(my-assert
 (print "2.satz t4" s)
 "2.satz t4")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t5.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t5" s)
 "1.satz t5")

(my-assert
 (print "2.satz t5" s)
 "2.satz t5")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t6.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t6" s)
 "1.satz t6")

(my-assert
 (print "2.satz t6" s)
 "2.satz t6")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t7.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t7" s)
 "1.satz t7")

(my-assert
 (print "2.satz t7" s)
 "2.satz t7")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t8.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t8" s)
 "1.satz t8")

(my-assert
 (print "2.satz t8" s)
 "2.satz t8")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t9.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t9" s)
 "1.satz t9")

(my-assert
 (print "2.satz t9" s)
 "2.satz t9")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s (open "t10.plc" :direction :output)) t)
 t)

(my-assert
 (print "1.satz t10" s)
 "1.satz t10")

(my-assert
 (print "2.satz t10" s)
 "2.satz t10")

(my-assert
 (close s)
 t)

(my-assert
 (progn (setq s1 (open "t1.plc")) (setq s2 (open "t2.plc"))
	(setq s3 (open "t3.plc")) (setq s4 (open "t4.plc")) (setq s5 (open
								      "t5.plc"))
	(setq c1 (make-concatenated-stream s1 s2 s3))
	(setq c2 (make-concatenated-stream s4 s5)) t)
 t)

(my-assert
 (read c1)
 "1.satz t1")

(my-assert
 (read c2)
 "1.satz t4")

(my-assert
 (read c1)
 "2.satz t1")

(my-assert
 (read c1)
 "1.satz t2")

(my-assert
 (read c2)
 "2.satz t4")

(my-assert
 (read c2)
 "1.satz t5")

(my-assert
 (read c1)
 "2.satz t2")

(my-assert
 (read c1)
 "1.satz t3")

(my-assert
 (read c1)
 "2.satz t3")

(my-assert
 (read c2)
 "2.satz t5")

(my-assert
 (close s1)
 t)

(my-assert
 (close s2)
 t)

(my-assert
 (close s3)
 t)

(my-assert
 (close s4)
 t)

(my-assert
 (close s5)
 t)

(my-assert
 (progn (setq s1 (open "t1.plc")) (setq s2 (open "t2.plc"))
	(setq s3 (open "t3.plc")) (setq s4 (open "t4.plc")) (setq s5 (open
								      "t5.plc"))
	(setq s6 (open "t6.plc")) (setq s7 (open "t7.plc")) (setq s8 (open
								      "t8.plc"))
	(setq s9 (open "t9.plc")) (setq s10 (open "t10.plc"))
	(setq c1 (make-concatenated-stream s1 s2))
	(setq c2 (make-concatenated-stream s3))
	(setq c3 (make-concatenated-stream c1 c2 s4))
	(setq c4 (make-concatenated-stream s5 s6 s7 s8 s9 s10)) t)
 t)

(my-assert
 (read c4)
 "1.satz t5")

(my-assert
 (read c3)
 "1.satz t1")

(my-assert
 (read c4)
 "2.satz t5")

(my-assert
 (read c4)
 "1.satz t6")

(my-assert
 (read c3)
 "2.satz t1")

(my-assert
 (read c3)
 "1.satz t2")

(my-assert
 (read c4)
 "2.satz t6")

(my-assert
 (read c4)
 "1.satz t7")

(my-assert
 (read c4)
 "2.satz t7")

(my-assert
 (read c3)
 "2.satz t2")

(my-assert
 (read c3)
 "1.satz t3")

(my-assert
 (read c3)
 "2.satz t3")

(my-assert
 (read c4)
 "1.satz t8")

(my-assert
 (read c4)
 "2.satz t8")

(my-assert
 (read c4)
 "1.satz t9")

(my-assert
 (read c4)
 "2.satz t9")

(my-assert
 (read c3)
 "1.satz t4")

(my-assert
 (read c3)
 "2.satz t4")

(my-assert
 (read c4)
 "1.satz t10")

(my-assert
 (read c4)
 "2.satz t10")

(my-assert
 (close s1)
 t)

(my-assert
 (close s2)
 t)

(my-assert
 (close s3)
 t)

(my-assert
 (close s4)
 t)

(my-assert
 (close s5)
 t)

(my-assert
 (close s6)
 t)

(my-assert
 (close s7)
 t)

(my-assert
 (close s8)
 t)

(my-assert
 (close s9)
 t)

(my-assert
 (close s10)
 t)

(my-assert
 (setq str1 "test 123456")
 "test 123456")

(my-assert
 (progn (setq s1 (make-string-input-stream str1)) t)
 t)

(my-assert
 (read s1)
 test)

(my-assert
 (read-char s1)
 #\1)

(my-assert
 (read-char s1)
 #\2)

(my-assert
 (unread-char #\2 s1)
 nil)

(my-assert
 (read-char s1)
 #\2)

(my-assert
 (read-char s1)
 #\3)

(my-assert
 (read-char s1)
 #\4)

(my-assert
 (unread-char #\a s1)
 error
 "We previously read #\4 from S1, we are not allowed to
put #\a back in!")

(my-assert
 (read-char s1)
 #\5
 "The previous unread-char should have failed, so
we expect to see #\5 here. If the unread-char worked
we will (wrongly!) see #\4 or #\a")

(my-assert
 (read-char s1)
 #\6
 "Likewise the unread-char should have failed")

(my-assert
 (close s1)
 t)

(my-assert
 str1
 "test 123456")

(my-assert
 (multiple-value-list (read-from-string "012345 789"))
 (12345 7))

(my-assert
 (multiple-value-list (read-from-string "012345 789" t nil
					:preserve-whitespace t))
 (12345 6))

(my-assert
 (multiple-value-list (read-from-string "012345 789" t nil :end 4))
 (123 4))

(my-assert
 (multiple-value-list (read-from-string "012345 789" t nil :start 2))
 (2345 7))

(my-assert
 (progn (setq strgstream (make-string-input-stream "0123456789" 5 8))
	t)
 t)

(my-assert
 (read strgstream)
 567)

(my-assert
 (progn (setq strgstream
	      (make-string-input-stream "wenn alles gut geht ist das ein stream 012"))
	t)
 t)

(my-assert
 (read strgstream)
 wenn)

(my-assert
 (read strgstream)
 alles)

(my-assert
 (read strgstream)
 gut)

(my-assert
 (read strgstream)
 geht)

(my-assert
 (read strgstream)
 ist)

(my-assert
 (read strgstream)
 das)

(my-assert
 (read strgstream)
 ein)

(my-assert
 (read strgstream)
 stream)

(my-assert
 (read strgstream)
 12)

(my-assert
 (progn (setq strgstream (make-string-output-stream)) t)
 t)

(my-assert
 (princ "das " strgstream)
 "das ")

(my-assert
 (princ "ist " strgstream)
 "ist ")

(my-assert
 (princ "ein " strgstream)
 "ein ")

(my-assert
 (princ "string " strgstream)
 "string ")

(my-assert
 (princ "output " strgstream)
 "output ")

(my-assert
 (princ "stream " strgstream)
 "stream ")

(my-assert
 (get-output-stream-string strgstream)
 "das ist ein string output stream ")

(my-assert
 (get-output-stream-string strgstream)
 "")

(my-assert
 (princ "das ist ein neuer string output stream" strgstream)
 "das ist ein neuer string output stream")

(my-assert
 (get-output-stream-string strgstream)
 "das ist ein neuer string output stream")

(my-assert
 (setq *print-length* 50)
 50)

(my-assert
 (write-to-string 123456789)
 "123456789")

(my-assert
 "(write-to-string '#1=(123456789 . #1#))"
 "(write-to-string '#1=(123456789 . #1#))")

(my-assert
 (prin1-to-string "abc")
 "\"abc\"")

(my-assert
 (princ-to-string "abc")
 "abc")

(my-assert
 (progn (setq os (make-string-output-stream)) t)
 t)

(my-assert
 (setq s50 "123456789A123456789B123456789C123456789D12345678
E")
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (setq s49 "123456789A123456789B123456789C123456789D1234567
*")
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s49 os)
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (princ "A" os)
 "A")

(my-assert
 (princ "B" os)
 "B")

(my-assert
 (princ "C" os)
 "C")

(my-assert
 (length (princ (get-output-stream-string os)))
 402)

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s50 os)
 "123456789A123456789B123456789C123456789D12345678
E")

(my-assert
 (princ s49 os)
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (princ s49 os)
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (princ s49 os)
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (princ s49 os)
 "123456789A123456789B123456789C123456789D1234567
*")

(my-assert
 (length (princ (get-output-stream-string os)))
 496)

(my-assert
 (progn (setq os (open "d0.plc" :direction :output))
	(setq os1 (open "d1.plc" :direction :output))
	(setq is (open "t0.plc" :direction :output)) t)
 t)

(my-assert
 (princ "'(a b #.(print \"1.zwischenwert\" os1) c d)" is)
 "'(a b #.(print \"1.zwischenwert\" os1) c d)")

(my-assert
 (princ "'(a b #.(prin1-to-string \"2.zwischenwert\") c d)" is)
 "'(a b #.(prin1-to-string \"2.zwischenwert\") c d)")

(my-assert
 (princ "'(a b #.(format nil  \"3.zwischenwert\") c d)" is)
 "'(a b #.(format nil  \"3.zwischenwert\") c d)")

(my-assert
 (close is)
 t)

(my-assert
 (progn (setq is (open "t0.plc")) (setq es (make-echo-stream is os))
	t)
 t)

(my-assert
 (print "ausgabe os1" os1)
 "ausgabe os1")

(my-assert
 (read es)
 (quote (a b "1.zwischenwert" c d)))

(my-assert
 (print "ausgabe os1" os1)
 "ausgabe os1")

(my-assert
 (read es)
 (quote (a b "\"2.zwischenwert\"" c d)))

(my-assert
 (print "ausgabe os1" os1)
 "ausgabe os1")

(my-assert
 (read es)
 (quote (a b "3.zwischenwert" c d)))

(my-assert
 (print "ausgabe os1" os1)
 "ausgabe os1")

(my-assert
 (close is)
 t)

(my-assert
 (close os)
 t)

(my-assert
 (progn (setq is (open "d0.plc")) t)
 t)

(my-assert
 (read is)
 (quote (a b "1.zwischenwert" c d)))

(my-assert
 (read is)
 (quote (a b "\"2.zwischenwert\"" c d)))

(my-assert
 (read is)
 (quote (a b "3.zwischenwert" c d)))

(my-assert
 (close is)
 t)

(my-assert
 (close os1)
 t)

(my-assert
 (progn (setq is (open "d1.plc")) t)
 t)

(my-assert
 (read is)
 "ausgabe os1")

(my-assert
 (read is)
 "1.zwischenwert")

(my-assert
 (read is)
 "ausgabe os1")

(my-assert
 (read is)
 "ausgabe os1")

(my-assert
 (read is)
 "ausgabe os1")

(my-assert
 (read is)
 "1.zwischenwert")

(my-assert
 (close is)
 t)

(my-assert
 (progn (mapc #'delete-file (directory "*.plc")) t)
 t)

(my-assert
 (progn
   (makunbound 's)
   (makunbound 's1)
   (makunbound 's2)
   (makunbound 's3)
   (makunbound 's4)
   (makunbound 's5)
   (makunbound 's6)
   (makunbound 's7)
   (makunbound 's8)
   (makunbound 's9)
   (makunbound 's10)
   (makunbound 'b1)
   (makunbound 'b2)
   (makunbound 'c1)
   (makunbound 'c2)
   (makunbound 'c3)
   (makunbound 'c4)
   (makunbound 'inptw)
   (makunbound 'sy)
   (makunbound 'tw)
   (makunbound 'ec)
   (makunbound 'str1)
   (makunbound 'strgstream)
   (makunbound 'os)
   (makunbound 'os1)
   (makunbound 'is)
   (makunbound 'es)
   (makunbound 's50)
   (makunbound 's49)
   (setq *print-length* nil)
   t)
 t)

