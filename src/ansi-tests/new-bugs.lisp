;;;  -*- mode: lisp -*-
(proclaim '(special log))
(in-package :cl-user)


;; From: Gary Bunting <gbunting@cantor.une.edu.au>

(my-assert
 (setf xx (expt 3 32))
 1853020188851841)

(my-assert
 (* xx xx)
 3433683820292512484657849089281)

;; paul

(my-assert
 (defun bugged (x)
   (labels ((f (y &optional trouble)	;  <<< or &key or &rest ..
	       (if y
		   (let ((a (pop y)))
		     (f a)))))))
 BUGGED)

(my-assert
 (defun tst ()
   (labels
       ((eff (&key trouble)
	     (eff)
	     ))
     ;;(eff :trouble nil)  ;<< this works
     (eff);; << this causes assert failure
     ))
 tst)


