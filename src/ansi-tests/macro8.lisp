;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

;; testen der macrofunktionen kapitel 8
;; ------------------------------------


;; 8.1
;; macro-function | defmacro


(my-assert
 (and (macro-function 'push) T)
 T)

(my-assert
 (and (macro-function 'member) T)
 NIL)

(my-assert
 (defmacro arithmetic-if (test neg-form zero-form pos-form)
   (let ((var (gensym)))
     `(let ((,var ,test))
	(cond ((< ,var 0) ,neg-form)
	      ((= ,var 0) ,zero-form)
	      (T ,pos-form)))))
 arithmetic-if)


(my-assert
 (and (macro-function 'arithmetic-if) T)
 T)

(my-assert
 (setf x 8)
 8)

(my-assert
 (arithmetic-if (- x 4)(- x)(LIST "ZERO") x)
 8)


(my-assert
 (setf x 4)
 4)

(my-assert
 (arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
 ("ZERO"))


(my-assert
 (setf x 3)
 3)

(my-assert
 (arithmetic-if (- x 4)(- x)(LIST "ZERO")x)
 -3)



(my-assert
 (defmacro arithmetic-if (test neg-form &optional zero-form pos-form)
   (let ((var (gensym)))
     `(let ((,var ,test))
	(cond ((< ,var 0) ,neg-form)
	      ((= ,var 0) ,zero-form)
	      (T ,pos-form)))))
 arithmetic-if)


(my-assert
 (setf x 8)
 8)

(my-assert
 (arithmetic-if (- x 4)(- x))
 nil)


(my-assert
 (setf x 4)
 4)

(my-assert
 (arithmetic-if (- x 4)(- x))
 NIL)


(my-assert
 (setf x 3)
 3)

(my-assert
 (arithmetic-if (- x 4)(- x))
 -3)

(my-assert
 (defmacro halibut ((mouth eye1 eye2)
		    ((fin1 length1)(fin2 length2))
		    tail)
   `(list ,mouth ,eye1 ,eye2 ,fin1 ,length1 ,fin2 ,length2 ,tail))
 halibut)

(my-assert
 (setf m 'red-mouth
       eyes '(left-eye . right-eye)
       f1 '(1 2 3 4 5)
       f2 '(6 7 8 9 0)
       my-favorite-tail '(list of all parts of tail))
 (list of all parts of tail))



(my-assert
 (halibut (m (car eyes)(cdr eyes))
	  ((f1 (length f1))(f2 (length f2)))
	  my-favorite-tail)
 (RED-MOUTH LEFT-EYE RIGHT-EYE (1 2 3 4 5) 5 (6 7 8 9 0) 5
	    (LIST OF ALL PARTS OF TAIL)))

;; 8.2
;;  macroexpand | macroexpand-1


(my-assert
 (ecase 'otherwise
   (otherwise 4))
 4
 "This is bad style, but perfectly legal!!")

;; Issue MACRO-FUNCTION-ENVIRONMENT:YES
(my-assert
 (macrolet ((foo (&environment env)
		 (if (macro-function 'bar env)
		     ''yes
		     ''no)))
   (list (foo)
         (macrolet ((bar () :beep))
	   (foo))))
 (no yes))
