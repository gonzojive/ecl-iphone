;;; 13 characters -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(my-assert
 (subtypep 'base-char 'character)
 T)


(my-assert
 (subtypep 'standard-char 'base-char)
 t)


(my-assert
 (subtypep 'extended-char 'character)
 t
 "Type EXTENDED-CHAR

Supertypes:

extended-char, character, t")

;;; char= etc


(my-assert
 (char= #\d #\d)
 t)


(my-assert
 (char= #\A #\a)
 nil)


(my-assert
 (char= #\d #\x)
 nil)


(my-assert
 (char= #\d #\D)
 nil)


(my-assert
 (char/= #\d #\d)
 nil)


(my-assert
 (char/= #\d #\x)
 t)


(my-assert
 (char/= #\d #\D)
 t)


(my-assert
 (char= #\d #\d #\d #\d)
 t)


(my-assert
 (char/= #\d #\d #\d #\d)
 nil)


(my-assert
 (char= #\d #\d #\x #\d)
 nil)


(my-assert
 (char/= #\d #\d #\x #\d)
 nil)


(my-assert
 (char= #\d #\y #\x #\c)
 nil)


(my-assert
 (char/= #\d #\y #\x #\c)
 t)


(my-assert
 (char= #\d #\c #\d)
 nil)


(my-assert
 (char/= #\d #\c #\d)
 nil)


(my-assert
 (char< #\d #\x)
 t)


(my-assert
 (char<= #\d #\x)
 t)


(my-assert
 (char< #\d #\d)
 nil)


(my-assert
 (char<= #\d #\d)
 t)


(my-assert
 (char< #\a #\e #\y #\z)
 t)


(my-assert
 (char<= #\a #\e #\y #\z)
 t)


(my-assert
 (char< #\a #\e #\e #\y)
 nil)


(my-assert
 (char<= #\a #\e #\e #\y)
 t)


(my-assert
 (char> #\e #\d)
 t)


(my-assert
 (char>= #\e #\d)
 t)


(my-assert
 (char> #\d #\c #\b #\a)
 t)


(my-assert
 (char>= #\d #\c #\b #\a)
 t)


(my-assert
 (char> #\d #\d #\c #\a)
 nil)


(my-assert
 (char>= #\d #\d #\c #\a)
 t)


(my-assert
 (char> #\e #\d #\b #\c #\a)
 nil)


(my-assert
 (char>= #\e #\d #\b #\c #\a)
 nil)


(my-assert
 (char> #\z #\A)
 #+(or cmu sbcl clisp ecls) T
 #-(or cmu sbcl clisp ecls) fill-this-in)


(my-assert
 (char> #\Z #\a)
 #+(or cmu sbcl clisp ecls) nil
 #-(or cmu sbcl clisp ecls) fill-this-in)


(my-assert
 (char-equal #\A #\a)
 t)


(my-assert
 (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char-lessp)
 (#\A #\a #\b #\B #\c #\C))


(my-assert
 (stable-sort (list #\b #\A #\B #\a #\c #\C) #'char<)
 #+(or cmu sbcl clisp ecls) (#\A #\B #\C #\a #\b #\c)
 #-(or cmu sbcl clisp ecls) fill-this-in)
					;  (#\A #\B #\C #\a #\b #\c) ;Implementation A
					;  (#\a #\b #\c #\A #\B #\C) ;Implementation B
					;  (#\a #\A #\b #\B #\c #\C) ;Implementation C
					;  (#\A #\a #\B #\b #\C #\c) ;Implementation D
					;  (#\A #\B #\a #\b #\C #\c) ;Implementation E

;;; character


(my-assert
 (character #\a)
 #\a)


(my-assert
 (character "a")
 #\a)

(my-assert
 (character 'a)
 #\A)


(my-assert
 (character '\a)
 #\a)


(my-assert
 (character 65.0)
 TYPE-ERROR)


(my-assert
 (character 'apple)
 TYPE-ERROR)


;;; alpha-char-p


(my-assert
 (alpha-char-p #\a)
 t)


(my-assert
 (alpha-char-p #\5)
 nil)


(my-assert
 (alpha-char-p #\Newline)
 nil)

;;; alphanumericp


(my-assert
 (alphanumericp #\Z)
 t)


(my-assert
 (alphanumericp #\9)
 t)


(my-assert
 (alphanumericp #\Newline)
 nil)


(my-assert
 (alphanumericp #\#)
 nil)

;;; digit-char


(my-assert
 (digit-char 0)
 #\0)


(my-assert
 (digit-char 10 11)
 #\A)


(my-assert
 (digit-char 10 10)
 nil)


(my-assert
 (digit-char 7)
 #\7)


(my-assert
 (digit-char 12)
 nil)


(my-assert
 (digit-char 12 16)
 #\C)


(my-assert
 (digit-char 6 2)
 nil)


(my-assert
 (digit-char 1 2)
 #\1)

;;; digit-char-p


(my-assert
 (digit-char-p #\5)
 5)


(my-assert
 (digit-char-p #\5 2)
 nil)


(my-assert
 (digit-char-p #\A)
 nil)


(my-assert
 (digit-char-p #\a)
 nil)


(my-assert
 (digit-char-p #\A 11)
 10)


(my-assert
 (digit-char-p #\a 11)
 10)


(my-assert
 (mapcar #'(lambda (radix)
	     (map 'list #'(lambda (x) (digit-char-p x radix))
		  "059AaFGZ"))
	 '(2 8 10 16 36))
 ((0 NIL NIL NIL NIL NIL NIL NIL)
  (0 5 NIL NIL NIL NIL NIL NIL)
  (0 5 9 NIL NIL NIL NIL NIL)
  (0 5 9 10 10 15 NIL NIL)
  (0 5 9 10 10 15 16 35)))

;;; graphic-char


(my-assert
 (graphic-char-p #\G)
 t)


(my-assert
 (graphic-char-p #\#)
 t)


(my-assert
 (graphic-char-p #\Space)
 t)


(my-assert
 (graphic-char-p #\Newline)
 nil)

;;; standard-char-p


(my-assert
 (standard-char-p #\Space)
 t)


(my-assert
 (standard-char-p #\~)
 t)

;;; char-upcase


(my-assert
 (char-upcase #\a)
 #\A)


(my-assert
 (char-upcase #\A)
 #\A)


(my-assert
 (char-downcase #\a)
 #\a)


(my-assert
 (char-downcase #\A)
 #\a)


(my-assert
 (char-upcase #\9)
 #\9)


(my-assert
 (char-downcase #\9)
 #\9)


(my-assert
 (char-upcase #\@)
 #\@)


(my-assert
 (char-downcase #\@)
 #\@)

;; Note that this next example might run for a very long time in
;; some implementations if CHAR-CODE-LIMIT happens to be very large
;; for that implementation.

(my-assert
 (dotimes (code char-code-limit)
   (let ((char (code-char code)))
     (when char
       (unless (cond ((upper-case-p char)
		      (char= (char-upcase
			      (char-downcase char)) char))
		     ((lower-case-p char)
		      (char= (char-downcase
			      (char-upcase char)) char))
		     (t (and (char= (char-upcase
				     (char-downcase char)) char)
			     (char= (char-downcase
				     (char-upcase char)) char))))
	 (return char)))))
 NIL)

;;; upper-case-p

(my-assert
 (upper-case-p #\A)
 t)


(my-assert
 (upper-case-p #\a)
 nil)


(my-assert
 (both-case-p #\a)
 t)


(my-assert
 (both-case-p #\5)
 nil)


(my-assert
 (lower-case-p #\5)
 nil)


(my-assert
 (upper-case-p #\5)
 nil)

;;; char-code-limit


(my-assert
 (>= char-code-limit 96)
 t)

;;; char-name


(my-assert
 (char-name #\ )
 "Space")


(my-assert
 (char-name #\Space)
 "Space")


(my-assert
 (char-name #\Page)
 "Page")


(my-assert
 (char-name #\a)
 #+(or cmu sbcl ecls) nil
 #+clisp "LATIN_SMALL_LETTER_A"
 #-(or cmu sbcl clisp ecls) fill-this-in)
;; NIL OR "LOWERCASE-a" OR  "Small-A" OR  "LA01"


(my-assert
 (char-name #\A)
 #+(or cmu sbcl ecls) nil
 #+clisp "LATIN_CAPITAL_LETTER_A"
 #-(or cmu sbcl clisp ecls) fill-this-in)
;;  NIL OR "UPPERCASE-A" OR  "Capital-A" OR  "LA02"

;; Even though its CHAR-NAME can vary, #\A prints as #\A

(my-assert
 (prin1-to-string (read-from-string (format nil "#\\~A" (or (char-name #\A) "A"))))
 "#\\A")

;;; name-char


(my-assert
 (name-char 'space)
 #\Space)


(my-assert
 (name-char "space")
 #\Space)


(my-assert
 (name-char "Space")
 #\Space)



(my-assert
 (let ((x (char-name #\a)))
   (or (not x) (eql (name-char x) #\a)))
 t)















