;;; section 7: objects -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

;;; function-keywords
(my-assert
 (progn
   (defmethod gf1 ((a integer) &optional (b 2)
		   &key (c 3) ((:dee d) 4) e ((eff f)))
     (list a b c d e f))
   t)
 T)

(my-assert
 (eq (find-method #'gf1 '() (list (find-class 'integer)))  'nil)
 nil)					; XXX

(my-assert
 (multiple-value-list
  (function-keywords (find-method #'gf1 '()
                                  (list (find-class 'integer)))))
 ((:C :DEE :E EFF) nil))

(my-assert
 (eq (defmethod gf2 ((a integer))
       (list a b c d e f)) 'nil)
 nil)					; XXX

(my-assert
 (multiple-value-list
  (function-keywords (find-method #'gf2 '() (list (find-class 'integer)))))
 (() nil))

(my-assert
 (progn
   (defmethod gf3 ((a integer) &key b c d &allow-other-keys)
     (list a b c d e f))
   t)
 t)

(my-assert
 (multiple-value-list
  (function-keywords (find-method #'gf3 '() (list (find-class 'integer)))))
 ((:B :C :D) t))

;;; if only i knew more about clos

