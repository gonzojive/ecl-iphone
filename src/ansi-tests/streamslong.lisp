;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (read-from-string "123")
 123)

(my-assert
 (prin1-to-string 123)
 "123")

(my-assert
 (let ((*a*
	(make-array 10. :element-type 'character
		    :fill-pointer 0)))
   (format *a* "XXX"))
 nil)

(my-assert
 (let ((*a*
	(make-array 10. :element-type 'character
		    :fill-pointer 0)))
   (format *a* "XXX")   
   *a*)
 "XXX")

#+xcl
(my-assert
 (sys::check-stream-system)
 #+xcl t)

