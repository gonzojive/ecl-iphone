;;; section 16: strings -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; simple-string-p

(my-assert
 (simple-string-p "aaaaaa")
 t)

(my-assert
 (simple-string-p (make-array 6
			      :element-type 'character
			      :fill-pointer t))
 nil)

;;; char

(my-assert
 (setq my-simple-string (make-string 6 :initial-element #\A))
 "AAAAAA")

(my-assert
 (schar my-simple-string 4)
 #\A)

(my-assert
 (setf (schar my-simple-string 4) #\B)
 #\B)

(my-assert
 my-simple-string
 "AAAABA")

(my-assert
 (setq my-filled-string
       (make-array 6 :element-type 'character
		   :fill-pointer 5
		   :initial-contents my-simple-string))
 "AAAAB")

(my-assert
 (char my-filled-string 4)
 #\B)

(my-assert
 (char my-filled-string 5)
 #\A
 "char: ...

char ignores fill pointers when accessing elements. ")

(my-assert
 (setf (char my-filled-string 3) #\C)
 #\C)

(my-assert
 (setf (char my-filled-string 5) #\D)
 #\D
 "char: ...

char ignores fill pointers when accessing elements. ")

(my-assert
 (setf (fill-pointer my-filled-string) 6)
 6)

(my-assert
 my-filled-string
 "AAACBD")

;;; string

(my-assert
 (string "already a string")
 "already a string")

(my-assert
 (string 'elm)
 "ELM")

(my-assert
 (string #\c)
 "c")

;;; string-upcase

(my-assert
 (string-upcase "abcde")
 "ABCDE")

(my-assert
 (string-upcase "Dr. Livingston, I presume?")
 "DR. LIVINGSTON, I PRESUME?")

(my-assert
 (string-upcase "Dr. Livingston, I presume?" :start 6 :end 10)
 "Dr. LiVINGston, I presume?")

(my-assert
 (string-downcase "Dr. Livingston, I presume?")
 "dr. livingston, i presume?")

(my-assert
 (string-capitalize "elm 13c arthur;fig don't")
 "Elm 13c Arthur;Fig Don'T")

(my-assert
 (string-capitalize " hello ")
 " Hello ")

(my-assert
 (string-capitalize "occlUDeD cASEmenTs FOreSTAll iNADVertent DEFenestraTION")
 "Occluded Casements Forestall Inadvertent Defenestration")

(my-assert
 (string-capitalize 'kludgy-hash-search)
 "Kludgy-Hash-Search")

(my-assert
 (string-capitalize "DON'T!")
 "Don'T!")				;not "Don't!"

(my-assert
 (string-capitalize "pipe 13a, foo16c")
 "Pipe 13a, Foo16c")

(my-assert
 (setq str (copy-seq "0123ABCD890a"))
 "0123ABCD890a")

(my-assert
 (nstring-downcase str :start 5 :end 7)
 "0123AbcD890a")

(my-assert
 str
 "0123AbcD890a")

;;; string-trim

(my-assert
 (string-trim "abc" "abcaakaaakabcaaa")
 "kaaak")

(my-assert
 (string-trim '(#\Space #\Tab #\Newline) " garbanzo beans
        ")
 "garbanzo beans")

(my-assert
 (string-trim " (*)" " ( *three (silly) words* ) ")
 "three (silly) words")

(my-assert
 (string-left-trim "abc" "labcabcabc")
 "labcabcabc")

(my-assert
 (string-left-trim " (*)" " ( *three (silly) words* ) ")
 "three (silly) words* ) ")

(my-assert
 (string-right-trim " (*)" " ( *three (silly) words* ) ")
 " ( *three (silly) words")

;;; string=

(my-assert
 (string= "foo" "foo")
 t)

(my-assert
 (string= "foo" "Foo")
 nil)

(my-assert
 (string= "foo" "bar")
 nil)

(my-assert
 (string= "together" "frog" :start1 1 :end1 3 :start2 2)
 t)

(my-assert
 (string-equal "foo" "Foo")
 t)

(my-assert
 (string= "abcd" "01234abcd9012" :start2 5 :end2 9)
 t)

(my-assert
 (string< "aaaa" "aaab")
 3)

(my-assert
 (string>= "aaaaa" "aaaa")
 4)

(my-assert
 (string-not-greaterp "Abcde" "abcdE")
 5)

(my-assert
 (string-lessp "012AAAA789" "01aaab6" :start1 3 :end1 7
	       :start2 2 :end2 6)
 6)

(my-assert
 (string-not-equal "AAAA" "aaaA")
 nil)

;;; stringp

(my-assert
 (stringp "aaaaaa")
 t)

(my-assert
 (stringp #\a)
 nil)

;;; make-string

(my-assert
 (make-string 10 :initial-element #\5)
 "5555555555")

(my-assert
 (length (make-string 10))
 10)







