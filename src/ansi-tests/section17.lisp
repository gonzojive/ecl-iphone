;;; section 17: sequences -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; 17.2.1.1

(my-assert
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equal)
 (foo bar "BAR" "foo" "bar"))

(my-assert
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'equalp)
 (foo bar "BAR" "bar"))

(my-assert
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string-equal)
 (bar "BAR" "bar"))

(my-assert
 (remove "FOO" '(foo bar "FOO" "BAR" "foo" "bar") :test #'string=)
 (BAR "BAR" "foo" "bar"))

(my-assert
 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'eql)
 (1))

(my-assert
 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test-not #'=)
 (1 1.0 #C(1.0 0.0)))

(my-assert
 (remove 1 '(1 1.0 #C(1.0 0.0) 2 2.0 #C(2.0 0.0)) :test (complement #'=))
 (1 1.0 #C(1.0 0.0)))

(my-assert
 (count 1 '((one 1) (uno 1) (two 2) (dos 2)) :key #'cadr)
 2)

(my-assert
 (count 2.0 '(1 2 3) :test #'eql :key #'float)
 1)

(my-assert
 (count "FOO" (list (make-pathname :name "FOO" :type "X")
                    (make-pathname :name "FOO" :type "Y"))
	:key #'pathname-name
	:test #'equal)
 2)

;;; 17.2.2.1

(my-assert
 (count-if #'zerop '(1 #C(0.0 0.0) 0 0.0d0 0.0s0 3))
 4)

(my-assert
 (remove-if-not #'symbolp '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
 (A B C D E F))

(my-assert
 (remove-if (complement #'symbolp) '(0 1 2 3 4 5 6 7 8 9 A B C D E F))
 (A B C D E F))

(my-assert
 (count-if #'zerop '("foo" "" "bar" "" "" "baz" "quux") :key #'length)
 3)

;;; copy-seq

(my-assert
 (let ((str "a string"))
   str)
 "a string")

(my-assert
 (let ((str "a string"))
   (equalp str (copy-seq str)))
 t)

(my-assert
 (let ((str "a string"))
   (eql str (copy-seq str)))
 nil)

;;; elt

(my-assert
 (let ((str "a string"))
   (setq str (copy-seq "0123456789")))
 "0123456789")

(my-assert
 (let ((str "a string"))
   (setq str (copy-seq "0123456789"))
   (elt str 6))
 #\6)

(my-assert
 (let ((str "a string"))
   (setq str (copy-seq "0123456789"))
   (setf (elt str 0) #\#))
 #\#)

(my-assert
 (let ((str "a string"))
   (setq str (copy-seq "0123456789"))
   (setf (elt str 0) #\#)
   str)
 "#123456789")

;;; fill

(my-assert
 (fill (list 0 1 2 3 4 5) '(444))
 ((444) (444) (444) (444) (444) (444)))

(my-assert
 (fill (copy-seq "01234") #\e :start 3)
 "012ee")

(my-assert
 (setq x (vector 'a 'b 'c 'd 'e))
 #(A B C D E))

(my-assert
 (fill x 'z :start 1 :end 3)
 #(A Z Z D E))

(my-assert
 x
 #(A Z Z D E))

(my-assert
 (fill x 'p)
 #(P P P P P))

(my-assert
 x
 #(P P P P P))

;;; make-sequence

(my-assert
 (make-sequence 'list 0)
 ())

(my-assert
 (make-sequence 'string 26 :initial-element #\.)
 "..........................")

(my-assert
 (make-sequence '(vector double-float) 2
		:initial-element 1d0)
 #(1.0d0 1.0d0))

(my-assert
 (make-sequence '(vector * 2) 3)
 TYPE-ERROR)

(my-assert
 (make-sequence '(vector * 4) 3)
 TYPE-ERROR)

;;; subseq

(my-assert
 (let ((str (copy-seq "012345")))
   str)
 "012345")

(my-assert
  (let ((str (copy-seq "012345")))
    (subseq str 2))
 "2345")

(my-assert
  (let ((str (copy-seq "012345")))
    (subseq str 3 5))
 "34")

(my-assert
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc"))
 "abc")

(my-assert
 (let ((str (copy-seq "012345")))
   (setf (subseq str 4) "abc")
   str)
 "0123ab")

(my-assert
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc")
    (setf (subseq str 0 2) "A"))
 "A")

(my-assert
  (let ((str (copy-seq "012345")))
    (setf (subseq str 4) "abc")
    (setf (subseq str 0 2) "A")
    str)
 "A123ab")

;;; map

(my-assert
 (map 'string #'(lambda (x y)
		  (char "01234567890ABCDEF" (mod (+ x y) 16)))
      '(1 2 3 4)
      '(10 9 8 7))
 "AAAA")

(my-assert
 (let ((seq (map 'list #'copy-seq
		 '("lower" "UPPER" "" "123"))))
   seq)
 ("lower" "UPPER" "" "123"))

(my-assert
  (let ((seq (map 'list #'copy-seq
		 '("lower" "UPPER" "" "123"))))
    (map nil #'nstring-upcase seq))
 NIL)

(my-assert
  (let ((seq (map 'list #'copy-seq
		 '("lower" "UPPER" "" "123"))))
    (map nil #'nstring-upcase seq)
    seq)
 ("LOWER" "UPPER" "" "123"))

(my-assert
 (map 'list #'- '(1 2 3 4))
 (-1 -2 -3 -4))

(my-assert
 (map 'string
      #'(lambda (x) (if (oddp x) #\1 #\0))
      '(1 2 3 4))
 "1010")

(my-assert
 (map '(vector * 4) #'cons "abc" "de")
 TYPE-ERROR)

;;; map-into

(my-assert
 (setq a (list 1 2 3 4) b (list 10 10 10 10))
 (10 10 10 10))

(my-assert
 (map-into a #'+ a b)
 (11 12 13 14))

(my-assert
 a
 (11 12 13 14))

(my-assert
 b
 (10 10 10 10))

(my-assert
 (setq k '(one two three))
 (ONE TWO THREE))

(my-assert
 (map-into a #'cons k a)
 ((ONE . 11) (TWO . 12) (THREE . 13) 14))

;;; reduce

(my-assert
 (reduce #'* '(1 2 3 4 5))
 120)

(my-assert
 (reduce #'append '((1) (2)) :initial-value '(i n i t))
 (I N I T 1 2))

(my-assert
 (reduce #'append '((1) (2)) :from-end t
	 :initial-value '(i n i t))
 (1 2 I N I T))

(my-assert
 (reduce #'- '(1 2 3 4))
 -8)

(my-assert
 (reduce #'- '(1 2 3 4) :from-end t)
 -2)

(my-assert
 (reduce #'+ '())
 0)

(my-assert
 (reduce #'+ '(3))
 3)

(my-assert
 (reduce #'+ '(foo))
 FOO)

(my-assert
 (reduce #'list '(1 2 3 4))
 (((1 2) 3) 4))

(my-assert
 (reduce #'list '(1 2 3 4) :from-end t)
 (1 (2 (3 4))))

(my-assert
 (reduce #'list '(1 2 3 4) :initial-value 'foo)
 ((((foo 1) 2) 3) 4))

(my-assert
 (reduce #'list '(1 2 3 4)
	 :from-end t :initial-value 'foo)
 (1 (2 (3 (4 foo)))))

;;; count

(my-assert
 (count #\a "how many A's are there in here?")
 2)

(my-assert
 (count-if-not #'oddp '((1) (2) (3) (4)) :key #'car)
 2)

(my-assert
 (count-if #'upper-case-p "The Crying of Lot 49" :start 4)
 2)

;; length

(my-assert
 (length "abc")
 3)

(my-assert
 (setq str (make-array '(3) :element-type 'character
		       :initial-contents "abc"
		       :fill-pointer t))
 "abc")

(my-assert
 (length str)
 3)

(my-assert
 (setf (fill-pointer str) 2)
 2)

(my-assert
 (length str)
 2)

;;; reverse

(my-assert
 (setq str "abc")
 "abc")

(my-assert
 (reverse str)
 "cba")

(my-assert
 str
 "abc")

(my-assert
 (setq str (copy-seq str))
 "abc")

(my-assert
 (nreverse str)
 "cba")

(my-assert
 str
 #+(or cmu sbcl clisp ecls) "cba"
 #-(or cmu sbcl clisp ecls) fill-this-in)

(my-assert
 (let ((l (list 1 2 3)))
   l)
 (1 2 3))

(my-assert
 (let ((l (list 1 2 3)))
   (nreverse l))
 (3 2 1))

(my-assert
 (let ((l (list 1 2 3)))
   (nreverse l)
   l)
 #+(or cmu sbcl ecls) (1)
 #+clisp (3 2 1)
 #-(or cmu sbcl clisp ecls) fill-this-in)

;;; sort

(my-assert
 (setq tester (copy-seq "lkjashd"))
 "lkjashd")

(my-assert
 (sort tester #'char-lessp)
 "adhjkls")

(my-assert
 (setq tester (list '(1 2 3) '(4 5 6) '(7 8 9)))
 ((1 2 3) (4 5 6) (7 8 9)))

(my-assert
 (sort tester #'> :key #'car)
 ((7 8 9) (4 5 6) (1 2 3)))

(my-assert
 (setq tester (list 1 2 3 4 5 6 7 8 9 0))
 (1 2 3 4 5 6 7 8 9 0))

(my-assert
 (stable-sort tester #'(lambda (x y) (and (oddp x) (evenp y))))
 (1 3 5 7 9 2 4 6 8 0))

(my-assert
 (sort (setq committee-data
             (vector (list (list "JonL" "White") "Iteration")
                     (list (list "Dick" "Waters") "Iteration")
                     (list (list "Dick" "Gabriel") "Objects")
                     (list (list "Kent" "Pitman") "Conditions")
                     (list (list "Gregor" "Kiczales") "Objects")
                     (list (list "David" "Moon") "Objects")
                     (list (list "Kathy" "Chapman") "Editorial")
                     (list (list "Larry" "Masinter") "Cleanup")
                     (list (list "Sandra" "Loosemore") "Compiler")))
       #'string-lessp :key #'cadar)
 #((("Kathy" "Chapman") "Editorial")
   (("Dick" "Gabriel") "Objects")
   (("Gregor" "Kiczales") "Objects")
   (("Sandra" "Loosemore") "Compiler")
   (("Larry" "Masinter") "Cleanup")
   (("David" "Moon") "Objects")
   (("Kent" "Pitman") "Conditions")
   (("Dick" "Waters") "Iteration")
   (("JonL" "White") "Iteration")))

;; Note that individual alphabetical order within `committees'
;; is preserved.

(my-assert
 (setq committee-data
       (stable-sort committee-data #'string-lessp :key #'cadr))
 #((("Larry" "Masinter") "Cleanup")
   (("Sandra" "Loosemore") "Compiler")
   (("Kent" "Pitman") "Conditions")
   (("Kathy" "Chapman") "Editorial")
   (("Dick" "Waters") "Iteration")
   (("JonL" "White") "Iteration")
   (("Dick" "Gabriel") "Objects")
   (("Gregor" "Kiczales") "Objects")
   (("David" "Moon") "Objects")))

;;; find

(my-assert
 (find #\d "here are some letters that can be looked at" :test #'char>)
 #\Space)

(my-assert
 (find-if #'oddp '(1 2 3 4 5) :end 3 :from-end t)
 3)

(my-assert
 (find-if-not #'complexp
	      '#(3.5 2 #C(1.0 0.0) #C(0.0 1.0))
	      :start 2)
 NIL)


;;; position

(my-assert
 (position #\a "baobab" :from-end t)
 4)

(my-assert
 (position-if #'oddp '((1) (2) (3) (4)) :start 1 :key #'car)
 2)

(my-assert
 (position 595 '())
 NIL)

(my-assert
 (position-if-not #'integerp '(1 2 3 4 5.0))
 4)

;;; search

(my-assert
 (search "dog" "it's a dog's life")
 7)

(my-assert
 (search '(0 1) '(2 4 6 1 3 5) :key #'oddp)
 2)

;;; mismatch

(my-assert
 (mismatch "abcd" "ABCDE" :test #'char-equal)
 4)

(my-assert
 (mismatch '(3 2 1 1 2 3) '(1 2 3) :from-end t)
 3)

(my-assert
 (mismatch '(1 2 3) '(2 3 4) :test-not #'eq :key #'oddp)
 NIL)

(my-assert
 (mismatch '(1 2 3 4 5 6) '(3 4 5 6 7) :start1 2 :end2 4)
 NIL)

;;; replace

(my-assert
 (replace (copy-seq "abcdefghij")
	  "0123456789" :start1 4 :end1 7 :start2 4)
 "abcd456hij")

(my-assert
 (let ((lst (copy-seq "012345678")))
   lst)
 "012345678")

(my-assert
  (let ((lst (copy-seq "012345678")))
    (replace lst lst :start1 2 :start2 0))
 "010123456")

(my-assert
  (let ((lst (copy-seq "012345678")))
    (replace lst lst :start1 2 :start2 0)
    lst)
 "010123456")

;;; substitute

(my-assert
 (substitute #\. #\SPACE "0 2 4 6")
 "0.2.4.6")

(my-assert
 (substitute 9 4 '(1 2 4 1 3 4 5))
 (1 2 9 1 3 9 5))

(my-assert
 (substitute 9 4 '(1 2 4 1 3 4 5) :count 1)
 (1 2 9 1 3 4 5))

(my-assert
 (substitute 9 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
 (1 2 4 1 3 9 5))

(my-assert
 (substitute 9 3 '(1 2 4 1 3 4 5) :test #'>)
 (9 9 4 9 3 4 5))

(my-assert
 (substitute-if 0 #'evenp '((1) (2) (3) (4)) :start 2 :key #'car)
 ((1) (2) (3) 0))

(my-assert
 (substitute-if 9 #'oddp '(1 2 4 1 3 4 5))
 (9 2 4 9 9 4 9))

(my-assert
 (substitute-if 9 #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
 (1 2 4 1 3 9 5))

(my-assert
 (setq some-things (list 'a 'car 'b 'cdr 'c))
 (A CAR B CDR C))

(my-assert
 (nsubstitute-if "function was here" #'fboundp some-things
                 :count 1 :from-end t)
 (A CAR B "function was here" C))

(my-assert
 some-things
 (A CAR B "function was here" C))

(my-assert
 (setq alpha-tester (copy-seq "ab "))
 "ab ")

(my-assert
 (nsubstitute-if-not #\z #'alpha-char-p alpha-tester)
 "abz")

(my-assert
 alpha-tester
 "abz")

;;; concatenate

(my-assert
 (concatenate 'string "all" " " "together" " " "now")
 "all together now")

(my-assert
 (concatenate 'list "ABC" '(d e f) #(1 2 3) #*1011)
 (#\A #\B #\C D E F 1 2 3 1 0 1 1))

(my-assert
 (concatenate 'list)
 NIL)

(my-assert
 (concatenate '(vector * 2) "a" "bc")
 TYPE-ERROR)

;;; merge

(my-assert
 (setq test1 (list 1 3 4 6 7))
 (1 3 4 6 7))

(my-assert
 (setq test2 (list 2 5 8))
 (2 5 8))

(my-assert
 (merge 'list test1 test2 #'<)
 (1 2 3 4 5 6 7 8))

(my-assert
 (setq test1 (copy-seq "BOY"))
 "BOY")

(my-assert
 (setq test2 (copy-seq "nosy"))
 "nosy")

(my-assert
 (merge 'string test1 test2 #'char-lessp)
 "BnOosYy")

(my-assert
 (setq test1 (vector '(red . 1) '(blue . 4)))
 #((RED . 1) (BLUE . 4)))

(my-assert
 (setq test2 (vector '(yellow . 2) '(green . 7)))
 #((YELLOW . 2) (GREEN . 7)))

(my-assert
 (merge 'vector test1 test2 #'< :key #'cdr)
 #((RED . 1) (YELLOW . 2) (BLUE . 4) (GREEN . 7)))

(my-assert
 (merge '(vector * 4) '(1 5) '(2 4 6) #'<)
 TYPE-ERROR)


;;; remove

(my-assert
 (remove 4 '(1 3 4 5 9))
 (1 3 5 9))

(my-assert
 (remove 4 '(1 2 4 1 3 4 5))
 (1 2 1 3 5))

(my-assert
 (remove 4 '(1 2 4 1 3 4 5) :count 1)
 (1 2 1 3 4 5))

(my-assert
 (remove 4 '(1 2 4 1 3 4 5) :count 1 :from-end t)
 (1 2 4 1 3 5))

(my-assert
 (remove 3 '(1 2 4 1 3 4 5) :test #'>)
 (4 3 4 5))

(my-assert
 (setq lst '(list of four elements))
 (LIST OF FOUR ELEMENTS))

(my-assert
 (setq lst2 (copy-seq lst))
 (LIST OF FOUR ELEMENTS))

(my-assert
 (setq lst3 (delete 'four lst))
 (LIST OF ELEMENTS))

(my-assert
 (equal lst lst2)
 nil)

(my-assert
 (remove-if #'oddp '(1 2 4 1 3 4 5))
 (2 4 4))

(my-assert
 (remove-if #'evenp '(1 2 4 1 3 4 5) :count 1 :from-end t)
 (1 2 4 1 3 5))

(my-assert
 (remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9) :count 2 :from-end t)
 (1 2 3 4 5 6 8))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete 4 tester)
 (1 2 1 3 5))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete 4 tester :count 1)
 (1 2 1 3 4 5))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete 4 tester :count 1 :from-end t)
 (1 2 4 1 3 5))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete 3 tester :test #'>)
 (4 3 4 5))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete-if #'oddp tester)
 (2 4 4))

(my-assert
 (setq tester (list 1 2 4 1 3 4 5))
 (1 2 4 1 3 4 5))

(my-assert
 (delete-if #'evenp tester :count 1 :from-end t)
 (1 2 4 1 3 5))

(my-assert
 (setq tester (list 1 2 3 4 5 6))
 (1 2 3 4 5 6))

(my-assert
 (delete-if #'evenp tester)
 (1 3 5))

(my-assert
 tester
 #+(or cmu sbcl clisp ecls) (1 3 5)
 #-(or cmu sbcl clisp ecls)  fill-this-in)

(my-assert
 (setq foo (list 'a 'b 'c))
 (A B C))

(my-assert
 (setq bar (cdr foo))
 (B C))

(my-assert
 (setq foo (delete 'b foo))
 (A C))

(my-assert
 bar
 #+(or cmu sbcl clisp ecls) (B C)
 #-(or cmu sbcl clisp ecls)  fill-this-in)
					; ((C))) or ...

(my-assert
 (eq (cdr foo) (car bar))
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)
					; T or ...


;;; remove-duplicates

(my-assert
 (remove-duplicates "aBcDAbCd" :test #'char-equal :from-end t)
 "aBcD")

(my-assert
 (remove-duplicates '(a b c b d d e))
 (A C B D E))

(my-assert
 (remove-duplicates '(a b c b d d e) :from-end t)
 (A B C D E))

(my-assert
 (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
		    :test #'char-equal :key #'cadr)
 ((BAR #\%) (BAZ #\A)))

(my-assert
 (remove-duplicates '((foo #\a) (bar #\%) (baz #\A))
		    :test #'char-equal :key #'cadr :from-end t)
 ((FOO #\a) (BAR #\%)))

(my-assert
 (setq tester (list 0 1 2 3 4 5 6))
 (0 1 2 3 4 5 6))

(my-assert
 (delete-duplicates tester :key #'oddp :start 1 :end 6)
 (0 4 5 6))

