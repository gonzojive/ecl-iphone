;;; section 18 hash tables -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(my-assert
 (defvar *counters* (make-hash-table))
 *COUNTERS*)

(my-assert
 (multiple-value-bind (a b)
     (gethash 'foo *counters*)
   (list a b))
 (NIL nil))

(my-assert
 (multiple-value-bind (a b)
     (gethash 'foo *counters* 0)
   (list a b))
 (0 nil))

(my-assert				; XXX
 (defmacro how-many (obj) `(values (gethash ,obj *counters* 0)))
 HOW-MANY)

(my-assert				; XXX
 (defun count-it (obj) (incf (how-many obj)))
 COUNT-IT)

(dolist (x '(bar foo foo bar bar baz)) (count-it x))

(my-assert
 (how-many 'foo)
 2)

(my-assert
 (how-many 'bar)
 3)

(my-assert
 (how-many 'quux)
 0)

