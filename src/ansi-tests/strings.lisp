;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (char  "abcdef-dg1ndh" 0)
 #\a)

(my-assert
 (char  "abcdef-dg1ndh" 1)
 #\b)

(my-assert
 (char  "abcdef-dg1ndh" 6)
 #\-)

(my-assert
 (char  "abcdef-dg1ndh" 20)
 error)

(my-assert
 (char  "abcdef-dg1ndh")
 program-error)

(my-assert
 (char  "abcdef-dg1ndh" -3)
 error)

(my-assert
 (char)
 program-error)

(my-assert
 (char 2)
 program-error)

(my-assert
 (char  "abcde" 2 4)
 error)

(my-assert
 (char 'a 0)
 #+xcl #\a
 #-xcl error)

(my-assert
 (char 'anna 0)
 #+xcl #\a
 #-xcl error)

(my-assert
 (schar 'a 0)
 #+xcl #\a
 #-xcl error)

(my-assert
 (schar 'anna 0)
 #+xcl #\a
 #-xcl error)

(my-assert
 (schar  "abcdef-dg1ndh" 0)
 #\a)

(my-assert
 (schar  "abcdef-dg1ndh" 1)
 #\b)

(my-assert
 (schar  "abcdef-dg1ndh" 6)
 #\-)

(my-assert
 (schar  "abcdef-dg1ndh" 20)
 error)

(my-assert
 (schar  "abcdef-dg1ndh")
 program-error)

(my-assert
 (schar  "abcdef-dg1ndh" -3)
 error)

(my-assert
 (schar 2)
 program-error)

(my-assert
 (schar 2 2)
 error)

(my-assert
 (schar  "abcde" 2 4)
 program-error)

(my-assert
 (string=  "foo" "foo")
 t)

(my-assert
 (string=  "foo" "Foo")
 nil)

(my-assert
 (string=  "foo" "FOO")
 nil)

(my-assert
 (string=  "foo" "bar")
 nil)

(my-assert
 (string=  "together" "frog" :start1 1 :end1 3 :start2 2)
 t)

(my-assert
 (string=  "abcdef" "defghi" :start1 3 :end2 3)
 t)

(my-assert
 (string=  "abcdefghi" "uvdefmgnj" :start1 3 :end1 6 :start2 2 :end2
	   5)
 t)

(my-assert
 (string=  "abcdefg" "abcdefg" :end2 4)
 nil)

(my-assert
 (string=  "abcdef" "abcdef" :start1 1 :end1 4 :start2 4 :end2 1)
 error)

(my-assert
 (string-equal  "foo" "foo")
 t)

(my-assert
 (string-equal  "foo" "Foo")
 t)

(my-assert
 (string-equal  "foo" "FOO")
 t)

(my-assert
 (string-equal  "foo" "bar")
 nil)

(my-assert
 (string-equal  "absDEfg-HijM1#r" "udEFG-hIfvd" :start1 3 :end1 10 :start2
		1 :end2
		8)
 t)

(my-assert
 (string-equal  "ABCdefg" "abcDEFG")
 t)

(my-assert
 (string-equal  "ABCdefg" "abcDEFG" :start1 3)
 nil)

(my-assert
 (string-equal  "AbCdEf" "aBcDeF" :start1 5 :end1 3)
 error)

(my-assert
 (string<  "" "abcdefgh")
 0)

(my-assert
 (string<  "a" "abcdefgh")
 1)

(my-assert
 (string<  "abc" "abcdefgh")
 3)

(my-assert
 (string<  "cabc" "abcdefgh")
 nil)

(my-assert
 (string<  "abcdefgh" "abcdefgh")
 nil)

(my-assert
 (string<  "xyzabc" "abcdefgh")
 nil)

(my-assert
 (string<  "abc" "xyzabcdefgh")
 0)

(my-assert
 (string<  "abcdefgh" "abcdefgh" :end1 4)
 4)

(my-assert
 (string<  "xyzabc" "abcdefgh" :start1 3)
 6)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string<  "abcdefgh" "")
 nil)

(my-assert
 (string<  "abcdefgh" "a")
 nil)

(my-assert
 (string<  "abcdefgh" "abc")
 nil)

(my-assert
 (string<  "abcdefgh" "cabc")
 0)

(my-assert
 (string<  "abcdefgh" "xyzabc")
 0)

(my-assert
 (string<  "xyzabcdefgh" "abc")
 nil)

(my-assert
 (string<  "abcdefgh" "abcdefgh" :end2 4)
 nil)

(my-assert
 (string<  "xyzabc" "abcdefgh" :start2 3)
 nil)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string<  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string<  "abcdef" "bcdefgh")
 0)

(my-assert
 (string<  "abcdef" "abcdefgh" :start2 2)
 0)

(my-assert
 (string<  "abcdef" "bngdabcdef" :start2 9 :end2 5)
 error)

(my-assert
 (string>  "" "abcdefgh")
 nil)

(my-assert
 (string>  "a" "abcdefgh")
 nil)

(my-assert
 (string>  "abc" "abcdefgh")
 nil)

(my-assert
 (string>  "cabc" "abcdefgh")
 0)

(my-assert
 (string>  "abcdefgh" "abcdefgh")
 nil)

(my-assert
 (string>  "xyzabc" "abcdefgh")
 0)

(my-assert
 (string>  "abc" "xyzabcdefgh")
 nil)

(my-assert
 (string>  "abcdefgh" "abcdefgh" :end1 4)
 nil)

(my-assert
 (string>  "xyzabc" "abcdefgh" :start1 3)
 nil)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string>  "abcdefgh" "")
 0)

(my-assert
 (string>  "abcdefgh" "a")
 1)

(my-assert
 (string>  "abcdefgh" "abc")
 3)

(my-assert
 (string>  "abcdefgh" "cabc")
 nil)

(my-assert
 (string>  "abcdefgh" "xyzabc")
 nil)

(my-assert
 (string>  "xyzabcdefgh" "abc")
 0)

(my-assert
 (string>  "abcdefgh" "abcdefgh" :end2 4)
 4)

(my-assert
 (string>  "xyzabc" "abcdefgh" :start2 3)
 0)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string>  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string>  "abcde" "bc")
 nil)

(my-assert
 (string>  "bcdef" "abcde")
 0)

(my-assert
 (string>  "bcdef" "abcdef")
 0)

(my-assert
 (string>  "abcdefghij" "abcdefgh" :start1 1)
 1)

(my-assert
 (string>  "ghijkl" "xyzabcd" :start2 6 :end2 4)
 error)

(my-assert
 (string<  "" "abcdefgh")
 0)

(my-assert
 (string<=  "a" "abcdefgh")
 1)

(my-assert
 (string<=  "abc" "abcdefgh")
 3)

(my-assert
 (string<=  "aaabce" "aaabcdefgh")
 nil)

(my-assert
 (string<=  "cabc" "abcdefgh")
 nil)

(my-assert
 (string<=  "abcdefgh" "abcdefgh")
 8)

(my-assert
 (string<=  "xyzabc" "abcdefgh")
 nil)

(my-assert
 (string<=  "abc" "xyzabcdefgh")
 0)

(my-assert
 (string<=  "abcdefgh" "abcdefgh" :end1 4)
 4)

(my-assert
 (string<=  "xyzabc" "abcdefgh" :start1 3)
 6)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string<=  "abcdefgh" "")
 nil)

(my-assert
 (string<=  "abcdefgh" "a")
 nil)

(my-assert
 (string<=  "abcdefgh" "abc")
 nil)

(my-assert
 (string<=  "abcdefgh" "cabc")
 0)

(my-assert
 (string<=  "abcdefgh" "xyzabc")
 0)

(my-assert
 (string<=  "xyzabcdefgh" "abc")
 nil)

(my-assert
 (string<=  "abcdefgh" "abcdefgh" :end2 4)
 nil)

(my-assert
 (string<=  "xyzabc" "abcdefgh" :start2 3)
 nil)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string<=  "abc" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string<=  "abcdef" "bcdefgh")
 0)

(my-assert
 (string<=  "abcdef" "abcdefgh" :start2 2)
 0)

(my-assert
 (string<=  "abcdef" "bngdabcdef" :start2 9 :end2 5)
 error)


(my-assert
 (string>= "" "abcdefgh")
 nil)

(my-assert
 (string>= "a" "abcdefgh")
 nil)

(my-assert
 (string>= "abc" "abcdefgh")
 nil)

(my-assert
 (string>= "cabc" "abcdefgh")
 0)

(my-assert
 (string>= "abcdefgh" "abcdefgh")
 8)

(my-assert
 (string>= "xyzabc" "abcdefgh")
 0)

(my-assert
 (string>= "abc" "xyzabcdefgh")
 nil)

(my-assert
 (string>= "abcdefgh" "abcdefgh" :end1 4)
 nil)

(my-assert
 (string>= "xyzabc" "abcdefgh" :start1 3)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string>= "abcdefgh" "")
 0)

(my-assert
 (string>= "abcdefgh" "a")
 1)

(my-assert
 (string>= "abcdefgh" "abc")
 3)

(my-assert
 (string>= "abcdefgh" "cabc")
 nil)

(my-assert
 (string>= "abcdefgh" "xyzabc")
 nil)

(my-assert
 (string>= "xyzabcdefgh" "abc")
 0)

(my-assert
 (string>= "abcdefgh" "abcdefgh" :end2 4)
 4)

(my-assert
 (string>= "xyzabc" "abcdefgh" :start2 3)
 0)

(my-assert
 (string>= "xyzabc" "abcdefgh" :start1 3)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string>= "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string>= "bcdef" "abcdef")
 0)

(my-assert
 (string>= "abcdefghij" "abcdefgh" :start1 1)
 1)

(my-assert
 (string>= "ghijkl" "xyzabcd" :start2 6 :end2 4)
 error)

(my-assert
 (string/= "" "abcdefgh")
 0)

(my-assert
 (string/= "a" "abcdefgh")
 1)

(my-assert
 (string/= "abc" "abcdefgh")
 3)

(my-assert
 (string/= "cabc" "abcdefgh")
 0)

(my-assert
 (string/= "abcdefgh" "abcdefgh")
 nil)

(my-assert
 (string/= "xyzabc" "abcdefgh")
 0)

(my-assert
 (string/= "abc" "xyzabcdefgh")
 0)

(my-assert
 (string/= "abcdefgh" "abcdefgh" :end1 4)
 4)

(my-assert
 (string/= "xyzabc" "abcdefgh" :start1 3)
 6)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string/= "abcdefgh" "")
 0)

(my-assert
 (string/= "abcdefgh" "a")
 1)

(my-assert
 (string/= "abcdefgh" "abc")
 3)

(my-assert
 (string/= "abcdefgh" "cabc")
 0)

(my-assert
 (string/= "abcdefgh" "xyzabc")
 0)

(my-assert
 (string/= "xyzabcdefgh" "abc")
 0)

(my-assert
 (string/= "abcdefgh" "abcdefgh" :end2 4)
 4)

(my-assert
 (string/= "xyzabc" "abcdefgh" :start2 3)
 0)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string/= "abc" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string/= "abcdefghi" "uvdefmgnj" :start1 3 :end1 6 :start2 2 :end2 5)
 nil)

(my-assert
 (string/= "abcdefg" "abcdefg" :end2 4)
 4)

(my-assert
 (string/= "abcdef" "abcdef" :start1 1 :end1 4 :start2 4 :end2 1)
 error)

(my-assert
 (string-lessp "" "abcDEFgh")
 0)

(my-assert
 (string-lessp "a" "Abcdefgh")
 1)

(my-assert
 (string-lessp "abc" "aBcDEfgh")
 3)

(my-assert
 (string-lessp "cABc" "aBCDefgh")
 nil)

(my-assert
 (string-lessp "abCDeFgh" "abCDEfgh")
 nil)

(my-assert
 (string-lessp "xyzAbc" "ABcCDfgh")
 nil)

(my-assert
 (string-lessp "aBC" "xYZAbcdEfgh")
 0)

(my-assert
 (string-lessp "abcDEfgh" "abcDEfgh" :end1 4)
 4)

(my-assert
 (string-lessp "XYZabc" "ABcdefgh" :start1 3)
 6)

(my-assert
 (string-lessp "aBc" "xyZABcdefgh" :start2 3)
 3)

(my-assert
 (string-lessp "abc" "xyzabCDEcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-lessp "abc" "xyzABcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string-lessp "abcdefgh" "")
 nil)

(my-assert
 (string-lessp "Abcdefgh" "a")
 nil)

(my-assert
 (string-lessp "ABCdefgh" "abc")
 nil)

(my-assert
 (string-lessp "ABCdefgh" "cabc")
 0)

(my-assert
 (string-lessp "abcdefgh" "xyzABC")
 0)

(my-assert
 (string-lessp "xyzABCdefgh" "abc")
 nil)

(my-assert
 (string-lessp "abcdEFgh" "abcdeFGh" :end2 4)
 nil)

(my-assert
 (string-lessp "xyzaBC" "abCDefgh" :start2 3)
 nil)

(my-assert
 (string-lessp "ABC" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string-lessp "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-lessp "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string-lessp "aBCDef" "bcdefgh")
 0)

(my-assert
 (string-lessp "aBCDef" "abcdefgh" :start2 2)
 0)

(my-assert
 (string-lessp "aBCDef" "bngdabcdef" :start2 9 :end2 5)
 error)

(my-assert
 (string-greaterp "" "abcdefgh")
 nil)

(my-assert
 (string-greaterp "A" "abcdefgh")
 nil)

(my-assert
 (string-greaterp "ABc" "abcdefgh")
 nil)

(my-assert
 (string-greaterp "CAbc" "abcdefgh")
 0)

(my-assert
 (string-greaterp "abcdefgh" "abcDEFgh")
 nil)

(my-assert
 (string-greaterp "xyzabc" "abCDEfgh")
 0)

(my-assert
 (string-greaterp "ABC" "xyzabcdefgh")
 nil)

(my-assert
 (string-greaterp "ABCdefgh" "abcdefgh" :end1 4)
 nil)

(my-assert
 (string-greaterp "xyzaBc" "ABCdefgh" :start1 3)
 nil)

(my-assert
 (string-greaterp "abc" "xyzABcdefgh" :start2 3)
 nil)

(my-assert
 (string-greaterp "abc" "xyzABcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string-greaterp "abc" "xyZAbcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string-greaterp "abcdefgh" "")
 0)

(my-assert
 (string-greaterp "Abcdefgh" "a")
 1)

(my-assert
 (string-greaterp "ABCdefgh" "abc")
 3)

(my-assert
 (string-greaterp "ABCdefgh" "cabc")
 nil)

(my-assert
 (string-greaterp "ABCdefgh" "xyzabc")
 nil)

(my-assert
 (string-greaterp "xyzabcdefgh" "Abc")
 0)

(my-assert
 (string-greaterp "abcdefgh" "aBCDefgh" :end2 4)
 4)

(my-assert
 (string-greaterp "xyzabc" "abcdEFgh" :start2 3)
 0)

(my-assert
 (string-greaterp "ABC" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string-greaterp "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string-greaterp "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string-greaterp "bCDEf" "abcde")
 0)

(my-assert
 (string-greaterp "bcDEF" "abcdef")
 0)

(my-assert
 (string-greaterp "abCDEfghij" "abcdefgh" :start1 1)
 1)

(my-assert
 (string-greaterp "ghijKl" "xyzabcd" :start2 6 :end2 4)
 error)

(my-assert
 (string-not-greaterp  "" "abcdefgh")
 0)

(my-assert
 (string-not-greaterp  "A" "abcdefgh")
 1)

(my-assert
 (string-not-greaterp  "aBC" "abcdefgh")
 3)

(my-assert
 (string-not-greaterp  "CABc" "abcdefgh")
 nil)

(my-assert
 (string-not-greaterp  "abcDEFgh" "abcdefgh")
 8)

(my-assert
 (string-not-greaterp  "xyzabc" "ABcdefgh")
 nil)

(my-assert
 (string-not-greaterp  "abc" "xyzABcdefgh")
 0)

(my-assert
 (string-not-greaterp  "ABCDEFgh" "abcdefgh" :end1 4)
 4)

(my-assert
 (string-not-greaterp  "xyzabc" "aBCDefgh" :start1 3)
 6)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string-not-greaterp  "abcdefgh" "")
 nil)

(my-assert
 (string-not-greaterp  "Abcdefgh" "a")
 nil)

(my-assert
 (string-not-greaterp  "ABCdefgh" "abc")
 nil)

(my-assert
 (string-not-greaterp  "ABCdefgh" "cabc")
 0)

(my-assert
 (string-not-greaterp  "ABCdefgh" "xyzabc")
 0)

(my-assert
 (string-not-greaterp  "xyzABCdefgh" "abc")
 nil)

(my-assert
 (string-not-greaterp  "abcdeFgh" "abcdefgh" :end2 4)
 nil)

(my-assert
 (string-not-greaterp  "xyzABC" "abcdefgh" :start2 3)
 nil)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-not-greaterp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 nil)

(my-assert
 (string-not-greaterp  "abcDEF" "bcdefgh")
 0)

(my-assert
 (string-not-greaterp  "abcDEF" "abcdefgh" :start2 2)
 0)

(my-assert
 (string-not-greaterp  "abcdef" "bngDAbcdef" :start2 9 :end2 5)
 error)

(my-assert
 (string-not-lessp  "" "abcdefgh")
 nil)

(my-assert
 (string-not-lessp  "a" "Abcdefgh")
 nil)

(my-assert
 (string-not-lessp  "ABC" "abcdefgh")
 nil)

(my-assert
 (string-not-lessp  "CABc" "abcdefgh")
 0)

(my-assert
 (string-not-lessp  "ABCdefgh" "abcdefgh")
 8)

(my-assert
 (string-not-lessp  "xyzABC" "abcdefgh")
 0)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh")
 nil)

(my-assert
 (string-not-lessp  "ABCdefgh" "abcdefgh" :end1 4)
 nil)

(my-assert
 (string-not-lessp  "xyzABC" "abcdefgh" :start1 3)
 nil)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string-not-lessp  "abcdefgh" "")
 0)

(my-assert
 (string-not-lessp  "Abcdefgh" "a")
 1)

(my-assert
 (string-not-lessp  "ABCdefgh" "abc")
 3)

(my-assert
 (string-not-lessp  "abCDEfgh" "cabc")
 nil)

(my-assert
 (string-not-lessp  "aBCdefgh" "xyzabc")
 nil)

(my-assert
 (string-not-lessp  "xyzABcdefgh" "abc")
 0)

(my-assert
 (string-not-lessp  "abCDEfgh" "abcdefgh" :end2 4)
 4)

(my-assert
 (string-not-lessp  "xyzABc" "abcdefgh" :start2 3)
 0)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3)
 nil)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 nil)

(my-assert
 (string-not-lessp  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string-not-lessp  "bCDef" "abcdef")
 0)

(my-assert
 (string-not-lessp  "ABCdefghij" "abcdefgh" :start1 1)
 1)

(my-assert
 (string-not-lessp  "ghIjkl" "xyzabcd" :start2 6 :end2 4)
 error)

(my-assert
 (string-not-equal  "" "abcdefgh")
 0)

(my-assert
 (string-not-equal  "A" "abcdefgh")
 1)

(my-assert
 (string-not-equal  "ABc" "abcdefgh")
 3)

(my-assert
 (string-not-equal  "cABc" "abcdefgh")
 0)

(my-assert
 (string-not-equal  "ABCdefgh" "abcdefgh")
 nil)

(my-assert
 (string-not-equal  "xyzABc" "abcdefgh")
 0)

(my-assert
 (string-not-equal  "ABC" "xyzabcdefgh")
 0)

(my-assert
 (string-not-equal  "ABCdefgh" "abcdefgh" :end1 4)
 4)

(my-assert
 (string-not-equal  "xyzaBC" "abcdefgh" :start1 3)
 6)

(my-assert
 (string-not-equal  "ABC" "xyzabcdefgh" :start2 3)
 3)

(my-assert
 (string-not-equal  "ABC" "xyzabcdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-not-equal  "ABC" "xyzabcdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string-not-equal  "abcdefgh" "")
 0)

(my-assert
 (string-not-equal  "Abcdefgh" "a")
 1)

(my-assert
 (string-not-equal  "aBCdefgh" "abc")
 3)

(my-assert
 (string-not-equal  "abcdefgh" "cABc")
 0)

(my-assert
 (string-not-equal  "abcdefgh" "xyzAbc")
 0)

(my-assert
 (string-not-equal  "xyzabcdefgh" "ABC")
 0)

(my-assert
 (string-not-equal  "abcdefgh" "abcDEFgh" :end2 4)
 4)

(my-assert
 (string-not-equal  "xyzabc" "aBCDefgh" :start2 3)
 0)

(my-assert
 (string-not-equal  "abc" "xyzABCdefgh" :start2 3)
 3)

(my-assert
 (string-not-equal  "abc" "xyzABCdefgh" :start2 3 :end2 8)
 3)

(my-assert
 (string-not-equal  "abc" "xyzABCdefgh" :start2 3 :end2 5)
 2)

(my-assert
 (string/=  "abcdefghi" "uvdEFmgnj" :start1 3 :end1 6 :start2 2 :end2 5)
 4)

(my-assert
 (string/=  "abcdefg" "abcDEfg" :end2 4)
 3)

(my-assert
 (string/=  "abcdef" "abCDef" :start1 1 :end1 4 :start2 4 :end2 1)
 error)

(my-assert
 (string-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
 "garbanzo beans")

(my-assert
 (string-trim   " (*)" " ( *three(siily) words* ) ")
 "three(siily) words")

(my-assert
 (string-trim   (quote a) "ababa")
 error)

(my-assert
 (string-trim   (quote (a)) "ababa")
 #+xcl error
 #+(or clisp gcl allegro cmu ecls) "ababa"
 #-(or xcl clisp gcl allegro cmu ecls) unknown)

(my-assert
 (string-trim   "a" "ababa")
 "bab")

(my-assert
 (string-trim   "c e" "    ceabceabce    c")
 "abceab")

(my-assert
 (string-trim   (quote (#\a)) "abcd")
 "bcd")

(my-assert
 (string-trim   (quote (#\a)) "xyzabcd")
 "xyzabcd")

(my-assert
 (string-trim   (quote (#\a)) "abcda")
 "bcd")

(my-assert
 (string-left-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
 "garbanzo beans
   ")

(my-assert
 (string-left-trim   " (*)" " ( *three(siily) words* ) ")
 "three(siily) words* ) ")

(my-assert
 (string-left-trim   (quote a) "ababa")
 error)

(my-assert
 (string-left-trim   (quote (a)) "ababa")
 #+xcl error
 #+(or clisp gcl allegro cmu ecls) "ababa"
 #-(or xcl clisp gcl allegro cmu ecls) unknown)

(my-assert
 (string-left-trim   "a" "ababa")
 "baba")

(my-assert
 (string-left-trim   "c e" "    ceabceabce    c")
 "abceabce    c")

(my-assert
 (string-left-trim   (quote (#\a)) "abcd")
 "bcd")

(my-assert
 (string-left-trim   (quote (#\a)) "xyzabcd")
 "xyzabcd")

(my-assert
 (string-left-trim   (quote (#\a)) "abcda")
 "bcda")

(my-assert
 (string-right-trim   (quote (#\space #\tab #\newline)) " garbanzo beans
   ")
 " garbanzo beans")

(my-assert
 (string-right-trim   " (*)" " ( *three(siily) words* ) ")
 " ( *three(siily) words")

(my-assert
 (string-right-trim   (quote a) "ababa")
 error)

(my-assert
 (string-right-trim   (quote (a)) "ababa")
 #+xcl error
 #+(or clisp gcl allegro cmu ecls) "ababa"
 #-(or xcl clisp gcl allegro cmu ecls) unknown)

(my-assert
 (string-right-trim   "a" "ababa")
 "abab")

(my-assert
 (string-right-trim   "c e" "    ceabceabce    c")
 "    ceabceab")

(my-assert
 (string-right-trim   (quote (#\a)) "abcd")
 "abcd")

(my-assert
 (string-right-trim   (quote (#\a)) "xyzabcd")
 "xyzabcd")

(my-assert
 (string-right-trim   (quote (#\a)) "abcda")
 "abcd")

(my-assert
 (string-upcase  "abCD efGh-ij")
 "ABCD EFGH-IJ")

(my-assert
 (string-upcase  "abCD efGh-ij" :start 5)
 "abCD EFGH-IJ")

(my-assert
 (string-upcase  "abCD efGh-ij" :end 5)
 "ABCD efGh-ij")

(my-assert
 (string-upcase  "abCD efGh-ij" :start 1 :end 6)
 "aBCD EfGh-ij")

(my-assert
 (string-upcase  "abCD efGh-ij" :start 6 :end 1)
 error)

(my-assert
 (string-upcase  "abCD efGh-ij" :start 3 :end 3)
 "abCD efGh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij")
 "abcd efgh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij" :start 3)
 "abCd efgh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij" :end 3)
 "abcD efGh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij" :start 3 :end 3)
 "abCD efGh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij" :start 1 :end 6)
 "abcd efGh-ij")

(my-assert
 (string-downcase  "abCD efGh-ij" :start 6 :end 1)
 error)

(my-assert
 (string-capitalize  "abcd def g hi")
 "Abcd Def G Hi")

(my-assert
 (string-capitalize  "abCd dEf G hi")
 "Abcd Def G Hi")

(my-assert
 (string-capitalize  "Abcd Def G Hi")
 "Abcd Def G Hi")

(my-assert
 (string-capitalize  "abcd def g hi" :start 6)
 "abcd dEf G Hi")

(my-assert
 (string-capitalize  "abcd def g hi" :end 6)
 "Abcd Def g hi")

(my-assert
 (string-capitalize  "abcd def g hi" :start 2 :end 10)
 "abCd Def G hi")

(my-assert
 (string-capitalize  "abcd def g hi" :start 10 :end 2)
 error)

(my-assert
 (string-capitalize  "don't")
 "Don'T")

(my-assert
 (string-capitalize  "DON'T")
 "Don'T")

(my-assert
 (string-capitalize  "34a 5BC")
 "34a 5bc")

(my-assert
 (string  1)
 error)

(my-assert
 (string  (quote a))
 "A")

(my-assert
 (string  #\a)
 "a")

(my-assert
 (string  "abc")
 "abc")

(my-assert
 (nstring-upcase
  (copy-seq "abCD efGh-ij"))
 "ABCD EFGH-IJ")

(my-assert
 (nstring-upcase
  (copy-seq "abCD efGh-ij")
  :start 5)
 "abCD EFGH-IJ")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ij")
		  :end 5)
 "ABCD efGh-ij")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ij")
		  :start6 :end 1)
 error)

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ij")
		  :start 3 :end 3)
 "abCD efGh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCD efGh-ij"))
 "abcd efgh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCD efGh-ij")
		    :start 3)
 "abCd efgh-ij")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ij")
		  :start 1 :end 6)
 "aBCD EfGh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCD efGh-ij")
		    :end 3)
 "abcD efGh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCd efGh-ij")
		    :start 3 :end 3)
 "abCd efGh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCd efGh-ij")
		    :start 1 :end 6)
 "abcd efGh-ij")

(my-assert
 (nstring-downcase  (copy-seq "abCD efGh-ij")
		    :start 6 :end 1)
 error)

(my-assert
 (nstring-downcase  (copy-seq "abCD efGh-ij")
		    :start nil :end nil)
 #+(or xcl akcl) "abcd efgh-ij"
 #-(or xcl akcl) error)

(my-assert
 (nstring-upcase  (copy-seq "abDC efGh-oj"))
 "ABDC EFGH-OJ")

(my-assert
 (nstring-upcase (copy-seq "abCD efGh-ij")
		 :start 1 :end 6)
 "aBCD EfGh-ij")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-fg")
		  :start 1 :end 6)
 "aBCD EfGh-fg")

(my-assert
 (nstring-upcase (copy-seq "abCD efGh-ef")
		 :start 3 :end 3)
 "abCD efGh-ef")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ef")
		  :start 3 :end 3)
 "abCD efGh-ef")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ef")
		  :start 3 :end 3)
 "abCD efGh-ef")

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ef")
		  :start 3 :end 1)
 error)

(my-assert
 (nstring-upcase  (copy-seq "abCD efGh-ef")
		  :start nil :end nil)
 #+(or xcl akcl) "ABCD EFGH-EF"
 #-(or xcl akcl) error)

(my-assert
 (nstring-downcase  (copy-seq "saBG efGh-ef"))
 "sabg efgh-ef")

(my-assert
 (nstring-downcase  (copy-seq "dfGV efGh-ef")
		    :start 1 :end 6)
 "dfgv efGh-ef")

(my-assert
 (nstring-downcase  (copy-seq "fgCD efGf-ef")
		    :start 1 :end 3)
 "fgcD efGf-ef")

(my-assert
 (nstring-downcase  (copy-seq "dfCF edFg-fg")
		    :start nil :end nil)
 #+(or xcl akcl) "dfcf edfg-fg"
 #-(or xcl akcl) error)

(my-assert
 (nstring-downcase  (copy-seq "fgHG edgf-fg")
		    :start 5 :end 1)
 error)

(my-assert
 (nstring-downcase  (copy-seq "scDF edFG-ef")
		    :start 1)
 "scdf edfg-ef")

(my-assert
 (nstring-downcase  (copy-seq "fgHG edFG-ef")
		    :end 4)
 "fghg edFG-ef")

(my-assert
 (nstring-capitalize  (copy-seq "fg hgf fgh"))
 "Fg Hgf Fgh")

(my-assert
 (let ((x (copy-seq "ABCDEF")))
   (nstring-downcase x)
   x)
 "abcdef")

