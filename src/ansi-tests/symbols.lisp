;;; based on v1.2 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (symbol-name (quote xyz))
 "XYZ")

(my-assert
 (let ((*gensym-counter* 32))
   (gensym)
   (prin1-to-string (gensym "FOO-")))
 "#:FOO-33")

(my-assert
 (let ((*gensym-counter* 32))
   (gensym)
   (prin1-to-string (gensym "garbage-")))
 #+xcl "#:|garbage|-33"
 #+(or clisp akcl allegro cmu sbcl ecls) "#:|garbage-33|"
 #-(or xcl clisp akcl allegro cmu sbcl ecls) UNKNOWN)

