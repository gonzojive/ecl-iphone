;;; based on v1.4 -*- mode: lisp -*-
(in-package :cl-user)


(my-assert
 char-code-limit
 #+xcl 128
 #+(or (and clisp (not unicode)) akcl sbcl cmu ecls) 256
 #+(or (and clisp unicode) allegro) 65536
 #-(or xcl clisp akcl allegro cmu sbcl ecls) unknown)

(my-assert
 (standard-char-p #\a)
 t)

(my-assert
 (standard-char-p #\$)
 t)

(my-assert
 (standard-char-p #\.)
 t)

(my-assert
 (standard-char-p #\A)
 t)

(my-assert
 (standard-char-p 1)
 type-error)

(my-assert
 (standard-char-p #\\)
 t)

(my-assert
 (standard-char-p #\5)
 t)

(my-assert
 (standard-char-p #\))
 t)

(my-assert
 (standard-char-p #\%)
 t)

(my-assert
 (standard-char-p #\Backspace)
 #+xcl t
 #-xcl nil)

(my-assert
 (standard-char-p #\Page)
 #+xcl t
 #-xcl nil)

(my-assert
 (standard-char-p #\Return)
 #+xcl t
 #-xcl nil)

(my-assert
 (graphic-char-p #\a)
 t)

(my-assert
 (graphic-char-p #\$)
 t)

(my-assert
 (graphic-char-p #\.)
 t)

(my-assert
 (graphic-char-p #\A)
 t)

(my-assert
 (graphic-char-p 1)
 type-error)

(my-assert
 (graphic-char-p #\\)
 t)

(my-assert
 (graphic-char-p #\5)
 t)

(my-assert
 (graphic-char-p #\))
 t)

(my-assert
 (graphic-char-p #\%)
 t)

(my-assert
 (graphic-char-p #\Backspace)
 nil)

(my-assert
 (graphic-char-p #\Page)
 nil)

(my-assert
 (graphic-char-p #\Return)
 nil)

(my-assert
 (characterp
  #\a)
 t)

(my-assert
 (characterp
    #\$)
 t)

(my-assert
 (characterp
    #\.)
 t)

(my-assert
 (characterp
    #\A)
 t)

(my-assert
 (characterp
    #\\)
 t)

(my-assert
 (characterp
    #\5)
 t)

(my-assert
 (characterp
    #\))
 t)

(my-assert
 (characterp
    #\%)
 t)

(my-assert
 (characterp
    #\Backspace)
 t)

(my-assert
 (characterp
    #\Page)
 t)

(my-assert
 (characterp
    #\Return)
 t)

(my-assert
 (alpha-char-p #\a)
 t)

(my-assert
 (alpha-char-p #\$)
 nil)

(my-assert
 (alpha-char-p #\.)
 nil)

(my-assert
 (alpha-char-p #\A)
 t)

(my-assert
 (alpha-char-p 1)
 type-error)

(my-assert
 (alpha-char-p #\\)
 nil)

(my-assert
 (alpha-char-p #\5)
 nil)

(my-assert
 (alpha-char-p #\))
 nil)

(my-assert
 (alpha-char-p #\%)
 nil)

(my-assert
 (alpha-char-p #\Backspace)
 nil)

(my-assert
 (alpha-char-p #\Page)
 nil)

(my-assert
 (alpha-char-p #\Return)
 nil)

(my-assert
 (upper-case-p #\a)
 nil)

(my-assert
 (upper-case-p #\$)
 nil)

(my-assert
 (upper-case-p #\.)
 nil)

(my-assert
 (upper-case-p #\A)
 t)

(my-assert
 (upper-case-p 1)
 type-error)

(my-assert
 (upper-case-p #\\)
 nil)

(my-assert
 (upper-case-p #\5)
 nil)

(my-assert
 (upper-case-p #\))
 nil)

(my-assert
 (upper-case-p #\%)
 nil)

(my-assert
 (upper-case-p #\Backspace)
 nil)

(my-assert
 (upper-case-p #\Page)
 nil)

(my-assert
 (upper-case-p #\Return)
 nil)

(my-assert
 (lower-case-p #\a)
 t)

(my-assert
 (lower-case-p #\$)
 nil)

(my-assert
 (lower-case-p #\.)
 nil)

(my-assert
 (lower-case-p #\A)
 nil)

(my-assert
 (lower-case-p 1)
 type-error)

(my-assert
 (lower-case-p #\\)
 nil)

(my-assert
 (lower-case-p #\5)
 nil)

(my-assert
 (lower-case-p #\))
 nil)

(my-assert
 (lower-case-p #\%)
 nil)

(my-assert
 (lower-case-p #\Backspace)
 nil)

(my-assert
 (lower-case-p #\Page)
 nil)

(my-assert
 (lower-case-p #\Return)
 nil)

(my-assert
 (both-case-p #\a)
 t)

(my-assert
 (both-case-p #\$)
 nil)

(my-assert
 (both-case-p #\.)
 nil)

(my-assert
 (both-case-p #\A)
 t)

(my-assert
 (both-case-p 1)
 type-error)

(my-assert
 (both-case-p #\\)
 nil)

(my-assert
 (both-case-p #\5)
 nil)

(my-assert
 (both-case-p #\))
 nil)

(my-assert
 (both-case-p #\%)
 nil)

(my-assert
 (both-case-p #\Backspace)
 nil)

(my-assert
 (both-case-p #\Page)
 nil)

(my-assert
 (both-case-p #\Return)
 nil)

(my-assert
 (digit-char-p #\a)
 nil)

(my-assert
 (digit-char-p #\$)
 nil)

(my-assert
 (digit-char-p #\.)
 nil)

(my-assert
 (digit-char-p #\A)
 nil)

(my-assert
 (digit-char-p 1)
 type-error)

(my-assert
 (digit-char-p #\\)
 nil)

(my-assert
 (digit-char-p #\5)
 5)

(my-assert
 (digit-char-p #\))
 nil)

(my-assert
 (digit-char-p #\%)
 nil)

(my-assert
 (digit-char-p #\Backspace)
 nil)

(my-assert
 (digit-char-p #\Page)
 nil)

(my-assert
 (digit-char-p #\Return)
 nil)

(my-assert
 (digit-char-p #\5 4)
 nil)

(my-assert
 (digit-char-p #\5 8)
 5)

(my-assert
 (digit-char-p #\E 16)
 14)

(my-assert
 (digit-char-p #\R 35)
 27)

(my-assert
 (digit-char-p #\5 4)
 nil)

(my-assert
 (digit-char-p #\5 5)
 nil)

(my-assert
 (digit-char-p #\5 6)
 5)

(my-assert
 (digit-char-p #\1 2)
 1)

(my-assert
 (alphanumericp #\a)
 t)

(my-assert
 (alphanumericp #\$)
 nil)

(my-assert
 (alphanumericp #\.)
 nil)

(my-assert
 (alphanumericp #\A)
 t)

(my-assert
 (alphanumericp 1)
 type-error)

(my-assert
 (alphanumericp #\\)
 nil)

(my-assert
 (alphanumericp #\5)
 t)

(my-assert
 (alphanumericp #\))
 nil)

(my-assert
 (alphanumericp #\%)
 nil)

(my-assert
 (alphanumericp #\Backspace)
 nil)

(my-assert
 (alphanumericp #\Page)
 nil)

(my-assert
 (alphanumericp #\Return)
 nil)

(my-assert
 (alphanumericp #\5 4)
 error)

(my-assert
 (alphanumericp #\5 8)
 error)

(my-assert
 (alphanumericp #\E 16)
 error)

(my-assert
 (alphanumericp #\R 35)
 error)

(my-assert
 (char= #\d #\d)
 t)

(my-assert
 (char/= #\d #\d)
 nil)

(my-assert
 (char= #\d #\x)
 nil)

(my-assert
 (char/= #\d #\x)
 t)

(my-assert
 (char= #\d #\D)
 nil)

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
 (char> #\d #\d #\b #\a)
 nil)

(my-assert
 (char>= #\d #\d #\b #\a)
 t)

(my-assert
 (char> #\e #\d #\b #\c #\a)
 nil)

(my-assert
 (char>= #\e #\d #\b #\c #\a)
 nil)

(my-assert
 (char> #\z #\A)
 t)

(my-assert
 (char> #\Z #\a)
 nil)

(my-assert
 (char< #\9 #\a)
 t)

(my-assert
 (char> #\9 #\a)
 nil)

(my-assert
 (char> #\z #\0)
 t)

(my-assert
 (char< #\z #\0)
 nil)

(my-assert
 (char-equal #\d #\d)
 t)

(my-assert
 (char-not-equal #\d #\d)
 nil)

(my-assert
 (char-equal #\d #\x)
 nil)

(my-assert
 (char-not-equal #\d #\x)
 t)

(my-assert
 (char-equal #\d #\D)
 t)

(my-assert
 (char-not-equal #\d #\D)
 nil)

(my-assert
 (char-equal #\d #\d #\d #\d)
 t)

(my-assert
 (char-not-equal #\d #\d #\d #\d)
 nil)

(my-assert
 (char-equal #\d #\d #\x #\d)
 nil)

(my-assert
 (char-not-equal #\d #\d #\x #\d)
 nil)

(my-assert
 (char-equal #\d #\y #\x #\c)
 nil)

(my-assert
 (char-not-equal #\d #\y #\x #\c)
 t)

(my-assert
 (char-equal #\d #\c #\d)
 nil)

(my-assert
 (char-not-equal #\d #\c #\d)
 nil)

(my-assert
 (char-lessp #\d #\x)
 t)

(my-assert
 (char-not-greaterp #\d #\x)
 t)

(my-assert
 (char-lessp #\d #\d)
 nil)

(my-assert
 (char-not-greaterp #\d #\d)
 t)

(my-assert
 (char-lessp #\a #\e #\y #\z)
 t)

(my-assert
 (char-not-greaterp #\a #\e #\y #\z)
 t)

(my-assert
 (char-lessp #\a #\e #\e #\y)
 nil)

(my-assert
 (char-not-greaterp #\a #\e #\e #\y)
 t)

(my-assert
 (char-greaterp #\e #\d)
 t)

(my-assert
 (char-not-lessp #\e #\d)
 t)

(my-assert
 (char-greaterp #\d #\c #\b #\a)
 t)

(my-assert
 (char-not-lessp #\d #\c #\b #\a)
 t)

(my-assert
 (char-greaterp #\d #\d #\b #\a)
 nil)

(my-assert
 (char-not-lessp #\d #\d #\b #\a)
 t)

(my-assert
 (char-greaterp #\e #\d #\b #\c #\a)
 nil)

(my-assert
 (char-not-lessp #\e #\d #\b #\c #\a)
 nil)

(my-assert
 (char-greaterp #\z #\A)
 t)

(my-assert
 (char-greaterp #\Z #\a)
 t)

(my-assert
 (char-lessp #\9 #\a)
 t)

(my-assert
 (char-greaterp #\9 #\a)
 nil)

(my-assert
 (char-greaterp #\z #\0)
 t)

(my-assert
 (char-lessp #\z #\0)
 nil)

(my-assert
 (char-equal #\A #\a)
 t)

(my-assert
 (char-upcase #\a)
 #\A)

(my-assert
 (char-upcase #\A)
 #\A)

(my-assert
 (char-upcase #\5)
 #\5)

(my-assert
 (char-upcase #\;)
 #\;)

(my-assert
 (char-upcase #\=)
 #\=)

(my-assert
 (char= (char-downcase (char-upcase #\x)) #\x)
 t)

(my-assert
 (char-downcase #\A)
 #\a)

(my-assert
 (char-downcase #\a)
 #\a)

(my-assert
 (char-downcase #\%)
 #\%)

(my-assert
 (char-downcase #\+)
 #\+)

(my-assert
 (char-downcase #\-)
 #\-)

(my-assert
 (char= (char-upcase (char-downcase #\X)) #\X)
 t)

(my-assert
 (digit-char 7)
 #\7)

(my-assert
 (digit-char 12)
 nil)

(my-assert
 (digit-char 'a)
 error)

(my-assert
 (digit-char 12 16)
 #\C)

(my-assert
 (digit-char 6 2)
 nil)

(my-assert
 (digit-char 1 2)
 #\1)

;; evan though char-*-bit are not in the ANSI CL standard,
;; they may be present as an extension

;; (my-assert
;;  char-control-bit
;;  error)

;; (my-assert
;;  char-meta-bit
;;  error)

;; (my-assert
;;  char-super-bit
;;  error)

;; (my-assert
;;  char-hyper-bit
;;  error)

(my-assert
 (char-name #\Space)
 "Space")

(my-assert
 (char-name #\Newline)
 #-cmu
 "Newline"
 #+cmu
 "Linefeed")
