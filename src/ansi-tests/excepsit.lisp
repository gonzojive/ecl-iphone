;;; based on 1.6 -*- mode: lisp -*-
(in-package :cl-user)

;;; Test "Exceptional situations" as specified by CLHS

;; NB: CLHS section 1.4.2 implies that we have to verify only those
;; specifications which have the wording "an error is signalled" or
;; "an error should be signalled".

#-(or cmu sbcl);; XXXX
(my-assert
 (abort)
 control-error)

(my-assert
 (acos 'x)
 type-error)

(my-assert
 (acosh 'x)
 type-error)

(my-assert
 (progn
   (defgeneric foo01 (x))
   (defmethod foo01 ((x number)) t)
   (let ((m (find-method #'foo01 nil (list (find-class 'number)))))
     (remove-method #'foo01 m)
     (defgeneric foo01 (x y))
     (add-method #'foo01 m)
     ) )
 error)

#-CLISP
(my-assert
 ;; documented behaviour of ADD-METHOD
 (progn
   (defgeneric foo02 (x))
   (defmethod foo02 ((x number)) t)
   (let ((m (find-method #'foo02 nil (list (find-class 'number)))))
     ; wrong, not? (remove-method #'foo02 m)
     (defgeneric foo03 (x))
     (add-method #'foo03 m)
     ) )
 error
 "add-method:...

If method is a method object of another generic function,
an error of type error is signaled. ")

(my-assert
 (let ((a (make-array 5 :adjustable t)))
   (adjust-array a 4 :fill-pointer 1)
   )
 error)

(my-assert
 (adjustable-array-p '(x))
 type-error)

(my-assert
 (alpha-char-p 33)
 type-error)

(my-assert
 (alphanumericp 33)
 type-error)

(my-assert
 (array-dimensions '(x))
 type-error)

(my-assert
 (array-displacement '(x))
 type-error)

(my-assert
 (array-element-type '(x))
 type-error)

(my-assert
 (array-has-fill-pointer-p '(x))
 type-error)

(my-assert
 (array-rank '(x))
 type-error)

(my-assert
 (array-total-size '(x))
 type-error)

(my-assert
 (ash 3/4 2)
 type-error)

(my-assert
 (ash 3 4.0)
 type-error)

(my-assert
 (asin 'x)
 type-error)

(my-assert
 (asinh 'x)
 type-error)

(my-assert
 (atan 'x)
 type-error)

(my-assert
 (atan #c(0 0.4) 3.4)
 type-error)

(my-assert
 (atan -4 #c(3 4))
 type-error)

(my-assert
 (atanh 'x)
 type-error)

(my-assert
 (boole 'x 3 4)
 type-error)

(my-assert
 (boole boole-and 3/4 -7)
 type-error)

(my-assert
 (boole boole-set 5 #c(-3 4))
 type-error)

(my-assert
 (both-case-p 33)
 type-error)

(my-assert
 (boundp 47)
 type-error)

(my-assert
 (butlast '(a b c) -1)
 type-error)

(my-assert
 (butlast '#(a b c))
 type-error)

(my-assert
 (car 'x)
 type-error)

(my-assert
 (cdr '#(a b c))
 type-error)

(my-assert
 (cdadar '((x y)))
 type-error)

(my-assert
 (progn
   (defgeneric foo04 (x))
   (defmethod foo04 ((x real)) 'ok)
   (defmethod foo04 ((x integer)) (call-next-method (sqrt x)))
   (foo04 -1))
 error
 "(sqrt -1) is not a real...")

(my-assert
 (progn
   (defgeneric foo041 (x))
   (defmethod foo041 ((x real)) 'ok)
   (defmethod foo041 ((x integer)) (call-next-method (sqrt x)))
   (foo041 2))
 error
 "CLHS:  When providing arguments to CALL-NEXT-METHOD, the following
 rule must be satisfied or an error of type ERROR should be signaled:
 the ordered set of applicable methods for a changed set of arguments
 for CALL-NEXT-METHOD must be the same as the ordered set of applicable
 methods for the original arguments to the generic function.")

(my-assert
 (ccase 'x)
 type-error)

(my-assert
 (char-code 33)
 type-error)

(my-assert
 (char-downcase 33)
 type-error)

(my-assert
 (char-equal)
 program-error)

(my-assert
 (char-greaterp)
 program-error)

(my-assert
 (char-lessp)
 program-error)

(my-assert
 (char-name 33)
 type-error)

(my-assert
 (char-not-equal)
 program-error)

(my-assert
 (char-not-greaterp)
 program-error)

(my-assert
 (char-not-lessp)
 program-error)

(my-assert
 (char-upcase 33)
 type-error)

(my-assert
 (char/=)
 program-error)

(my-assert
 (char<)
 program-error)

(my-assert
 (char<=)
 program-error)

(my-assert
 (char=)
 program-error)

(my-assert
 (char>)
 program-error)

(my-assert
 (char>=)
 program-error)

(my-assert
 (character "abc")
 type-error)

(my-assert
 (character "")
 type-error)

(my-assert
 (character 33)
 type-error)

(my-assert
 (clear-input '*terminal-io*)
 type-error)

(my-assert
 (clear-output '*terminal-io*)
 type-error)

(my-assert
 (coerce '(a b c) '(vector * 4))
 type-error)

(my-assert
 (coerce '#(a b c) '(vector * 4))
 type-error)

(my-assert
 (coerce '(a b c) '(vector * 2))
 type-error)

(my-assert
 (coerce '#(a b c) '(vector * 2))
 type-error)

(my-assert
 (coerce "foo" '(string 2))
 type-error)

(my-assert
 (coerce '#(#\a #\b #\c) '(string 2))
 type-error)

(my-assert
 (coerce '(0 1) '(simple-bit-vector 3))
 type-error)

(my-assert
 (coerce nil 'nil)
 type-error)

(my-assert
 (coerce '#:nonexistent 'function)
 error)

(my-assert
 (coerce 'and 'function)
 error)

(my-assert
 (compile-file "/tmp/12836123.lsp")
 file-error)

(my-assert
 (concatenate 'symbol)
 error)

(my-assert
 (concatenate '(string 3) "ab" "cd")
 type-error)

#-CLISP
(my-assert
 (copy-pprint-dispatch 'x)
 type-error)

(my-assert
 (copy-seq 'x)
 type-error)

(my-assert
 (copy-symbol #\x)
 type-error)

(my-assert
 (cos 'x)
 type-error)

(my-assert
 (cosh 'x)
 type-error)

(my-assert
 (count #\x 'x)
 type-error)

(my-assert
 (let ((x nil)) (ctypecase x))
 type-error)

(my-assert
 (decode-float 2/3)
 type-error)

(my-assert
 (defclass foo05 () (a b a))
 program-error
 "defclass:
...
If there are any duplicate slot names,
an error of type program-error is signaled. ")

(my-assert
 (defclass foo06 () (a b) (:default-initargs x a x b))
 program-error
 "defclass:
...
If an initialization argument name appears more
than once in :default-initargs class option, an
error of typeprogram-error is signaled. ")

(my-assert
 (defclass foo07 () ((a :allocation :class :allocation :class)))
 program-error
 "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(my-assert
 (defclass foo08 () ((a :initform 42 :initform 42)))
 program-error
 "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(my-assert
 (defclass foo09 () ((a :type real :type real)))
 program-error
 "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(my-assert
 (defclass foo10 () ((a :documentation "bla" :documentation "blabla")))
 program-error
 "defclass:
...
If any of the following slot options appears more than once in a
single slot description, an error of type program-error is
signaled: :allocation, :initform, :type, :documentation.")

(my-assert
 (defgeneric if (x))
 program-error
 "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(my-assert
 (progn
   (defmacro foo11 (x) x)
   (defgeneric foo11 (x)))
 program-error
 "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(my-assert
 (progn
   (defun foo12 (x) x)
   (defgeneric foo12 (x)))
 program-error
 "defgeneric:
...
If function-name names an ordinary function,
a macro, or a special operator, an error of type
program-error is signaled.")

(my-assert
 (defgeneric foo13 (x y &rest l)
   (:method (x y))
   )
 error
 "")

(my-assert
 (defgeneric foo14 (x)
   (:documentation "bla")
   (:documentation "blabla")
   )
 program-error)

(my-assert
 (defgeneric foo15 (x)
   (:my-option t))
 program-error)

;;  define-method-combination is too complicated

(my-assert
 (progn
   (defvar foo16)
   (define-symbol-macro foo16 t))
 program-error)

(my-assert
 (defmethod if (x) nil)
 error)

(my-assert
 (progn
   (defmacro foo17 (x) x)
   (defmethod foo17 (x) nil))
 error)

(my-assert
 (progn
   (defun foo18 (x) x)
   (defmethod foo18 (x) nil))
 error)

(my-assert
 (progn
   (defgeneric foo19 (x))
   (defmethod foo19 (x y) nil))
 error)

(my-assert
 (progn
   (defpackage "FOO20")
   (defpackage "FOO21" (:nicknames "FOO20")))
 package-error)

(my-assert
 (defpackage "FOO22" (:size 20) (:size 20))
 program-error)

(my-assert
 (defpackage "FOO23" (:documentation "bla") (:documentation "blabla"))
 program-error)

(my-assert
 (defpackage "FOO24" (:my-option t))
 program-error)

(my-assert
 (defpackage "FOO25" (:shadow "IF") (:intern "IF"))
 program-error)

(my-assert
 (defpackage "FOO26" (:shadow "IF") (:import-from "USER" "IF"))
 program-error)

(my-assert
 (defpackage "FOO27" (:shadow "IF") (:shadowing-import-from "USER" "IF"))
 program-error)

(my-assert
 (defpackage "FOO28" (:intern "IF") (:import-from "USER" "IF"))
 program-error)

(my-assert
 (defpackage "FOO29" (:intern "IF") (:shadowing-import-from "USER" "IF"))
 program-error)

(my-assert
 (defpackage "FOO30" (:import-from "USER" "IF") (:shadowing-import-from "USER" "IF"))
 program-error)

(my-assert
 (defpackage "FOO31" (:export "IF") (:intern "IF"))
 program-error)

#-sbcl
(my-assert
 (defstruct foo32 a system::a)
 program-error)

#-sbcl
(my-assert
 (progn
   (defstruct foo33 a)
   (defstruct (foo34 (:include foo33)) system::a))
 program-error)

(my-assert
 (delete #\x 'x)
 type-error)

(my-assert
 (delete-duplicates 'abba)
 type-error)

;; deleting a non-existing file can be successful!
;; the results are not easily predictable across implementations
;;(my-assert
;; (progn
;;   (with-open-file (s "/tmp/foo35.tmp" :direction :output))
;;   (delete-file "/tmp/foo35.tmp/bar"))
;; nil or file-error??)

(my-assert
 (destructuring-bind (a) '(1 2) a)
 error)

;;  directory - no way to make a directory search fail

#-CLISP
(my-assert
 ;; documented behaviour of DISASSEMBLE
 (disassemble #x123456)
 type-error)

;;  dribble - no way to force a file-error

(my-assert
 (ecase 'x)
 type-error)

(my-assert
 (elt 'x 0)
 type-error)

(my-assert
 (elt "abc" 4)
 type-error)

(my-assert
 (elt '(a b c) 4)
 type-error)

(my-assert
 (elt '#(a b c) 4)
 type-error)

(my-assert
 (elt (make-array 3 :fill-pointer 3 :adjustable t) 4)
 type-error)

(my-assert
 (endp 'x)
 type-error)

(my-assert
 (ensure-directories-exist "/*/")
 file-error)

(my-assert
 (error 42)
 type-error)

(my-assert
 (let ((x nil)) (etypecase x))
 type-error)

(my-assert
 (every '(lambda (x) x) nil)
 type-error)

(my-assert
 (every #'identity 'x)
 type-error)

(my-assert
 (fboundp '(psetf aref))
 type-error)

(my-assert
 (fdefinition '(psetf aref))
 type-error)

(my-assert
 (fdefinition '#:nonexistent)
 undefined-function)

(my-assert
 (file-author "*")
 file-error)

(my-assert
 (file-length *terminal-io*)
 type-error)

(my-assert
 (with-open-file (s "/tmp/foo35.tmp" :direction :output)
		 (file-position s 0.0))
 error)

(my-assert
 (with-open-file (s "/tmp/foo35.tmp" :direction :output)
		 (file-position s -1))
 error)

(my-assert
 (with-open-file (s "/tmp/foo35.tmp" :direction :input)
		 (file-position s (+ (file-length s) 1000)))
 error)

(my-assert
 (not (delete-file "/tmp/foo35.tmp"))
 nil)

(my-assert
 (file-write-date "*")
 file-error)

(my-assert
 (fill 'x #\x)
 type-error)

(my-assert
 (fill (make-list 3) 'x :start nil)
 type-error)

(my-assert
 (fill (make-list 3) 'x :start -1)
 type-error)

(my-assert
 (fill (make-list 3) 'x :start 1 :end -1)
 type-error)

(my-assert
 (fill-pointer "abc")
 type-error)

(my-assert
 (find #\x 'x)
 type-error)

(my-assert
 (find-class '#:nonexistent t)
 error)

(my-assert
 (progn
   (defgeneric foo36 (x y))
   (find-method #'foo36 nil (list (find-class 'number))))
 error)

(my-assert
 (progn
   (defgeneric foo37 (x))
   (find-method #'foo37 nil (list (find-class 'number))))
 error)

(my-assert
 (finish-output '*terminal-io*)
 type-error)

(my-assert
 (float-digits 2/3)
 type-error)

(my-assert
 (float-precision 2/3)
 type-error)

(my-assert
 (float-radix 2/3)
 type-error)

(my-assert
 (float-sign 2/3)
 type-error)

(my-assert
 (float-sign -4.5 2/3)
 type-error)

(my-assert
 (fmakunbound '(psetf aref))
 type-error)

(my-assert
 (force-output '*terminal-io*)
 type-error)

(my-assert
 (funcall 'foo38)
 undefined-function)

(my-assert
 (funcall 'and)
 undefined-function)

(my-assert
 (gcd 4 3/4)
 type-error)

(my-assert
 (gensym #\x)
 type-error)

(my-assert
 (gentemp 't)
 type-error)

(my-assert
 (gentemp "X" 24)
 type-error)

(my-assert
 (get "a" 'x)
 type-error)

(my-assert
 (get-dispatch-macro-character #\0 #\#)
 error)

(my-assert
 (graphic-char-p 33)
 type-error)

(my-assert
 (hash-table-rehash-size *readtable*)
 type-error)

(my-assert
 (hash-table-rehash-threshold *package*)
 type-error)

(my-assert
 (hash-table-size *random-state*)
 type-error)

(my-assert
 (hash-table-test '#(a b c))
 type-error)

(my-assert
 (imagpart #\c)
 type-error)

(my-assert
 #-CLISP
 (in-package "FOO39")
 #+CLISP
 (common-lisp:in-package "FOO39")
 package-error)

(my-assert
 (input-stream-p (pathname "abc"))
 type-error)

(my-assert
 (integer-decode-float 2/3)
 type-error)

(my-assert
 (integer-length 0.0)
 type-error)

(my-assert
 (interactive-stream-p (pathname "abc"))
 type-error)

(my-assert
 (invoke-restart 'foo40)
 control-error)

(my-assert
 (invoke-restart-interactively 'foo41)
 control-error)

(my-assert
 (isqrt -1)
 type-error)

(my-assert
 (isqrt #c(3 4))
 type-error)

(my-assert
 (last '(a b c) -1)
 type-error)

(my-assert
 (lcm 4/7 8)
 type-error)

(my-assert
 (length 'x)
 type-error)

(my-assert
 (list-length 'x)
 type-error)

(my-assert
 (list-length '(x . y))
 type-error)

(my-assert
 (load "/tmp/128347234.lsp")
 file-error)

(my-assert
 (load "*.lsp")
 file-error)

(my-assert
 (load-logical-pathname-translations "FOO41")
 error)

(my-assert
 (logand -3 2.3)
 type-error)

(my-assert
 (logbitp -1 5)
 type-error)

(my-assert
 (logbitp 2 3/7)
 type-error)

(my-assert
 (logcount #*01010011)
 type-error)

(my-assert
 (logical-pathname '#(#\A #\B))
 type-error)

(my-assert
 (logical-pathname-translations '#(#\A #\B))
 type-error)

(my-assert
 (lower-case-p 33)
 type-error)

(my-assert
 (make-broadcast-stream (make-string-input-stream "abc"))
 type-error)

(my-assert
 (make-concatenated-stream (make-string-output-stream))
 type-error)

(my-assert
 (progn
   (defclass foo42 () ())
   (make-instance 'foo42 :x 1))
 error)

(my-assert
 (make-list -1)
 type-error)

(my-assert
 (progn
   (defstruct foo43)
   (make-load-form (make-foo43)))
 error)

(my-assert
 (make-random-state 'x)
 type-error)

(my-assert
 (make-sequence 'x 5)
 type-error)

(my-assert
 (make-sequence 'sequence 5)
 type-error)

(my-assert
 (make-sequence '(string 3) 4)
 type-error)

(my-assert
 (make-symbol 'x)
 type-error)

(my-assert
 (make-synonym-stream *terminal-io*)
 type-error)

(my-assert
 (make-two-way-stream (make-string-input-stream "abc") (make-string-input-stream "def"))
 type-error)

(my-assert
 (make-two-way-stream (make-string-output-stream) (make-string-output-stream))
 type-error)

(my-assert
 (makunbound "xx")
 type-error)

(my-assert
 (map 'x #'identity "abc")
 type-error)

(my-assert
 (map '(string 3) #'identity "ab")
 type-error)

(my-assert
 (max 3 #c(4 0.0))
 type-error)

(my-assert
 (merge '(vector * 5) '(3 1) '(2 4) #'<)
 type-error)

(my-assert
 (min 3 #c(4 0.0))
 type-error)

(my-assert
 (minusp #c(4 -3/4))
 type-error)

(my-assert
 (muffle-warning)
 control-error)

(my-assert
 (name-char '#(#\N #\u #\l))
 type-error)

(my-assert
 (nbutlast '(a b c) -1)
 type-error)

(my-assert
 (nbutlast '#(a b c))
 type-error)

(my-assert
 (no-applicable-method #'cons)
 error)

(my-assert
 (no-next-method #'print-object (find-method #'print-object nil (list (find-class 'standard-object) (find-class 't))))
 error)

(my-assert
 (notany '(lambda (x) x) nil)
 type-error)

(my-assert
 (notany #'identity 'x)
 type-error)

(my-assert
 (notevery '(lambda (x) x) nil)
 type-error)

(my-assert
 (notevery #'identity 'x)
 type-error)

(my-assert
 (nthcdr 2 '(a . b))
 type-error)

(my-assert
 (oddp 3.5)
 type-error)

#+UNIX
(my-assert
 (progn (open "/etc/passwd" :direction :input :if-exists :error) (/ 0))
 division-by-zero)

#+UNIX
(my-assert
 (progn (open "/etc/nonexistent" :direction :input :if-exists :error) (/ 0))
 file-error)

(my-assert
 (open "/tmp/foo44nonexistent" :direction :input :if-does-not-exist :error)
 file-error)

(my-assert
 (open "/tmp/*" :direction :input)
 file-error)

#+UNIX
(my-assert
 (open "/etc/mtab" :direction :input :external-format 'mtab-entries)
 error)

(my-assert
 (open-stream-p (pathname "foo45"))
 type-error)

(my-assert
 (output-stream-p (pathname "foo46"))
 type-error)

(my-assert
 (package-name 47)
 type-error)

(my-assert
 (package-nicknames (pathname "foo47"))
 type-error)

(my-assert
 (package-shadowing-symbols (vector 'a 'b 'c))
 type-error)

(my-assert
 (package-use-list (list 'a 'b 'c))
 type-error)

(my-assert
 (package-used-by-list (list 'a 'b 'c))
 type-error)

(my-assert
 (parse-integer "x-y")
 error)

(my-assert
 (parse-namestring (coerce (list #\f #\o #\o (code-char 0) #\4 #\8) 'string))
 parse-error)

(my-assert
 (parse-namestring "foo48:a" (logical-pathname "foo49:"))
 error)

(my-assert
 (pathname-match-p 34 "*")
 type-error)

(my-assert
 (pathname-match-p "x" 34)
 type-error)

(my-assert
 (peek-char nil (make-string-input-stream "") t)
 end-of-file)

(my-assert
 (peek-char #\space (make-string-input-stream "") t)
 end-of-file)

#|					; It's not clear why peek-char should signal an error, where read-char and
;;  read-line don't. Kent Pitman says: "Sounds like a mess."
(peek-char nil (make-string-input-stream "") nil nil t)
end-of-file
|#

(my-assert
 (phase 'x)
 type-error)

(my-assert
 (plusp #c(0 4.2))
 type-error)

#-CLISP
(my-assert
 (pprint-dispatch nil t)
 type-error)

#-CLISP
(my-assert
 (pprint-exit-if-list-exhausted)
 error)

#-CLISP
(my-assert
 (pprint-indent nil 2)
 error)

#-CLISP
(my-assert
 (let ((x (make-string-output-stream)))
   (pprint-logical-block (x nil :prefix 24)))
 type-error)

#-CLISP
(my-assert
 (let ((x (make-string-output-stream)))
   (pprint-logical-block (x nil :prefix "a" :per-line-prefix "b")))
 error)

#-CLISP
(my-assert
 (pprint-newline :fresh)
 type-error)

#-CLISP
(my-assert
 (pprint-pop)
 error)

(my-assert
 (pprint-tab :paragraph 0 1)
 error)

(my-assert
 (let ((*print-readably* t)) (print-unreadable-object (nil *standard-output*)))
 print-not-readable)

(my-assert
 (probe-file "*")
 file-error)

(my-assert
 (provide 25)
 type-error)

(my-assert
 (random -2.3)
 type-error)

(my-assert
 (rational #c(2.4 -0.3))
 type-error)

(my-assert
 (rationalize #c(2.4 -0.3))
 type-error)

(my-assert
 (read (make-string-input-stream "((a b)") nil)
 end-of-file)

(my-assert
 (read (make-string-input-stream " ") t)
 end-of-file)

(my-assert
 (read-byte (pathname "foo50"))
 type-error)

(my-assert
 (read-byte (make-string-input-stream "abc"))
 error)

(my-assert
 (let ((filename "/tmp/foo51.bin"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input
		      :element-type '(unsigned-byte 8))
		   (read-byte s t)))
 end-of-file)

(my-assert
 (not (delete-file "/tmp/foo51.bin"))
 nil)

(my-assert
 (let ((filename "/tmp/foo52.txt"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input)
		   (read-char s t)))
 end-of-file)

(my-assert
 (not (delete-file "/tmp/foo52.txt"))
 nil)

(my-assert
 (let ((filename "/tmp/foo53.txt"))
   (with-open-file (s filename :direction :output
		      :if-exists :overwrite
		      :if-does-not-exist :create))
   (with-open-file (s filename :direction :input)
		   (read-char-no-hang s t)))
 end-of-file)

(my-assert
 (not (delete-file "/tmp/foo53.txt"))
 nil)

(my-assert
 (read-from-string "((a b))" nil nil :end 6)
 end-of-file)

(my-assert
 (read-from-string " () () " t nil :start 3 :end 4)
 end-of-file)

(my-assert
 (read-line (make-string-input-stream "") t)
 end-of-file)

(my-assert
 (read-sequence (list 1 2 3) (make-string-input-stream "") :start nil)
 type-error)

(my-assert
 (read-sequence (list 1 2 3) (make-string-input-stream "") :end -1)
 type-error)

(my-assert
 (readtable-case nil)
 type-error)

(my-assert
 (setf (readtable-case *readtable*) ':unknown)
 type-error)

(my-assert
 (realpart #\c)
 type-error)

(my-assert
 (progn
   (defclass foo54 () ())
   (reinitialize-instance (make-instance 'foo54) :dummy 0))
 error)

(my-assert
 (remove #\x 'x)
 type-error)

(my-assert
 (remove-duplicates 'abba)
 type-error)

(my-assert
 (remprop 55 'abc)
 type-error)

(my-assert
 (rplaca nil 5)
 type-error)

(my-assert
 (rplacd nil 5)
 type-error)

(my-assert
 (scale-float 2/3 -1)
 type-error)

(my-assert
 (scale-float 3.4 1.0)
 type-error)

(my-assert
 (set-dispatch-macro-character #\0 #\# #'(lambda (s c n) (loop)))
 error)

(my-assert
 (set-pprint-dispatch '(vector * 2) nil #c(3 4))
 error)

(my-assert
 (sin 'x)
 type-error)

(my-assert
 (sinh 'x)
 type-error)

(my-assert
 (sleep -1)
 type-error)

(my-assert
 (progn
   (defclass foo55 () (a))
   (slot-boundp (make-instance 'foo55) ':a))
 error)

(my-assert
 (progn
   (defclass foo56 () (a))
   (slot-makunbound (make-instance 'foo56) ':a))
 error)

(my-assert
 (slot-missing (find-class 't) nil ':a 'setf)
 error)

(my-assert
 (slot-unbound (find-class 't) nil ':a)
 unbound-slot)

(my-assert
 (progn
   (defclass foo57 () (a))
   (slot-value (make-instance 'foo57) ':a))
 error)

(my-assert
 (some '(lambda (x) x) nil)
 type-error)

(my-assert
 (some #'identity 'x)
 type-error)

(my-assert
 (special-operator-p '(and x y))
 type-error)

(my-assert
 (special-operator-p '(setf aref))
 type-error)

(my-assert
 (sqrt 'x)
 type-error)

(my-assert
 (standard-char-p 33)
 type-error)

(my-assert
 (stream-element-type '*terminal-io)
 type-error)

(my-assert
 (string 33)
 type-error)

(my-assert
 (symbol-function 33)
 type-error)

(my-assert
 (symbol-function ':compile)
 undefined-function)

(my-assert
 (symbol-macrolet ((t true)))
 program-error)

(my-assert
 (symbol-macrolet ((*print-pretty* (stream-print-pretty *standard-output*))))
 program-error)

(my-assert
 (symbol-macrolet ((foo58 t)) (declare (special foo58)))
 program-error)

(my-assert
 (symbol-name '(setf foo59))
 type-error)

(my-assert
 (symbol-package '(setf foo59))
 type-error)

(my-assert
 (symbol-plist '(setf foo59))
 type-error)

(my-assert
 (symbol-value '(setf foo59))
 type-error)

(my-assert
 (symbol-value '#:nonexistent)
 unbound-variable)

(my-assert
 (tan 'x)
 type-error)

(my-assert
 (tanh 'x)
 type-error)

(my-assert
 (throw '#:nonexistent nil)
 control-error)

(my-assert
 (translate-logical-pathname (make-broadcast-stream))
 type-error)

(my-assert
 (translate-logical-pathname (logical-pathname "foo61:"))
 file-error)

#-CLISP
(my-assert
 ;; clisp explicitly allows symbols as pathnames
 (translate-pathname 'x "x" "y")
 type-error)

#-CLISP
(my-assert
 ;; clisp explicitly allows symbols as pathnames
 (translate-pathname "a" '* '*)
 type-error)

(my-assert
 (translate-pathname "x" "y" "z")
 error)

(my-assert
 (truename "/tmp/foo62nonexistent")
 file-error)

(my-assert
 (truename "/tmp/*/x")
 file-error)

(my-assert
 (typep nil 'values)
 error)

(my-assert
 (typep #'cons '(values t))
 error)

(my-assert
 (typep #'cons '(function (t t) list))
 error)

(my-assert
 (unexport ':foo63)
 package-error)

(my-assert
 (progn
   (defpackage "FOO64" (:export "XYZ"))
   (defpackage "FOO65" (:export "XYZ"))
   (defpackage "FOO66" (:use "FOO64" "FOO65") (:shadow "XYZ"))
   (unintern (find-symbol "XYZ" (find-package "FOO66")) (find-package "FOO66")))
 error)

;;  update-instance-for-different-class too complicated

;;  update-instance-for-redefined-class too complicated

(my-assert
 (upper-case-p 33)
 type-error)

(my-assert
 (values-list '(a b . c))
 type-error)

(my-assert
 (vector-pop "foo67")
 type-error)

(my-assert
 (vector-pop (make-array 10 :fill-pointer 0))
 error)

(my-assert
 (vector-push 'x (make-array 10))
 error)

(my-assert
 (let ((a (make-array 5
		      :fill-pointer 0
		      :adjustable nil)))
   (if (adjustable-array-p a)
       'error
       (dotimes (i 100) (vector-push-extend 'x a))))
 error)

(my-assert
 (warn (make-condition 'error))
 type-error)

(my-assert
 (warn (make-condition 'warning) "x")
 type-error)

(my-assert
 (warn 'error)
 type-error)

(my-assert
 (wild-pathname-p #\x)
 type-error)

(my-assert
 (write-byte 1 (pathname "foo67"))
 type-error)

(my-assert
 (write-byte 1 (make-string-output-stream))
 error)

(my-assert
 (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :start nil)
 type-error)

(my-assert
 (write-sequence '(#\1 #\2 #\3) (make-string-output-stream) :end -1)
 type-error)

(my-assert
 (zerop 'x)
 type-error)

;;  section 2.3.1.1
(my-assert
 (read-from-string "-35/000")
 reader-error)				; not division-by-zero!

(my-assert
 (read-from-string "31e300")
 reader-error)				; not floating-point-overflow!

