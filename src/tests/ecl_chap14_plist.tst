;; -*- lisp -*-
(defun safe-mapcar (f list)
  (mapcar #'(lambda (x) (with-ignored-errors (funcall f x))) list))
safe-mapcar

;;
;; ACONS
;;
(acons 'a 'b nil)
((a . b))

(acons 'a 'b '((c . d)))
((a . b) (c . d))

(acons 'a 'b 1)
#+ecl ((a . b) . 1) #-ecl error

;;
;; PAIRLIS
;;
(safe-mapcar #'(lambda (x) (apply #'pairlis x))
	     '(((a b c) (1 2 3)                  )
	       ((a b c) (1 2 3) ((d . 4) (e . 5)))
	       ((a b c) (1 2)                    )
	       ((a b c) (1 2)   ((d . 4) (e . 5)))
	       ('a      1                        )
	       ('a      1       ((d . 4) (e . 5)))))
(((c . 3) (b . 2) (a . 1))
 ((c . 3) (b . 2) (a . 1) (d . 4) (e . 5))
 error
 error
 error
 error)

;;
;; GETF
;;

; Empty lists are good property lists
(getf nil 'a)
nil

; Getf uses #'eq to compare
(getf '("A" 1 "B" 2) (string-upcase 'a))
nil

(mapcar #'(lambda (x) (getf '(a 1 b 2 c 3 d 4) x)) '(a b c d e f g))
(1 2 3 4 nil nil nil)

(mapcar #'(lambda (x) (getf '(a 1 b 2 c 3 d 4) x x)) '(a b c d e f g))
(1 2 3 4 e f g)

(let ((a nil))
  (dolist (i '(a b c d))
    (setf (getf a i) i))
  a)
(d d c c b b a a)

(let ((a '(a 1 b 2 c 3 d 4)))
  (dolist (i '(d c a b e b f a))
    (setf (getf a i) i))
  a)
(f f e e a a b b c c d d)

; Default value in setf-get is ignored but evaluated
(let ((a '(a 1 b 2 c 3))
      (b nil))
  (setf (getf a 'b 'default) 'b)
  (setf (getf a 'a (push 1 b)) 'a)
  (setf (getf a 'd (push 2 b)) 'd)
  (cons a b))
((d d a a b b c 3)
 2 1)

; Leading properties take peference over duplicates
(mapcar #'(lambda (x) (getf '(a 1 b 2 b 3 c 4 a 5 c 6) x)) '(a e f b c))
(1 nil nil 2 4)

(let ((a '(a 1 b 2 b 3 a 4 c 6)))
  (dolist (i '(d e c b a))
    (setf (getf a i) i))
  a)
(e e d d a a b b b 3 a 4 c c)

; Failure due to non list
(getf 'a 'c)
error

; Failure due to odd length
(getf '(a 1 b) 'c)
error

; Failure due to dotted list
(getf '(a 1 b . 2) 'c)
error
(getf '(a 1 . b) 'c)
error

; Failure due to circular lists. Only ECL passes this.
#+ecl
(let ((a '(a 1 b 2)))
  (nconc a a)
  (getf a 'c))
#+ecl
error

#+ecl
(let ((a '(a 1 b 2)))
  (nconc a a)
  (setf (getf a 'c) 3)
  a)
#+ecl
error

;;
;; REMF
;;

; Empty lists are good property lists
(let ((a nil)) (remf a 'a))
nil

; Getf uses #'eq to compare
(let ((a '("A" 1 "B" 2))) (remf a (string-upcase 'a)))
nil

; Try several operations with duplicates.
(let ((a '(a 1 b 2 c 3 d 4 a 5)))
  (mapcar #'(lambda (x) (let ((flag (remf a x))) (cons flag (copy-list a)))) '(a e f a c g)))
((t b 2 c 3 d 4 a 5) (nil b 2 c 3 d 4 a 5) (nil b 2 c 3 d 4 a 5) (t b 2 c 3 d 4) (t b 2 d 4) (nil b 2 d 4))

; Failure due to non list
(let ((a 'c)) (remf a 'c))
error

; Failure due to odd length
(let ((a '(a 1 b))) (remf a 'b))
error

; Failure due to dotted list
(let ((a '(a 1 b . 2))) (remf a 'b))
error
(let ((a '(a 1 . b))) (remf a 'b))
error

; Failure due to circular lists. Only ECL passes this.
#+ecl
(let ((a '(a 1 b 2)))
  (nconc a a)
  (remf a 'c))
#+ecl
error

