;;; section 18 hash tables -*- mode: lisp -*-
(in-package :cl-user)

(proclaim '(special log))

(my-assert
 (progn
   (setq a (make-hash-table))
   t)
 t)
;; #<HASH-TABLE EQL 0/120 32536573>

(my-assert
 (setf (gethash 'color a) 'brown)
 BROWN)

(my-assert
 (setf (gethash 'name a) 'fred)
 FRED)

(my-assert
 (multiple-value-bind (a b)
     (gethash 'color a)
   (list a b))
 (BROWN t))

(my-assert
 (multiple-value-bind (a b)
     (gethash 'name a)
   (list a b))
 ( FRED t))

(my-assert
 (multiple-value-bind (a b)
     (gethash 'pointy a)
   (list a b))
 ( NIL nil))

;;;make-hash-table

(my-assert
 (progn
   (setq table (make-hash-table))
   t)
 t)
;;  #<HASH-TABLE EQL 0/120 46142754>

(my-assert
 (setf (gethash "one" table) 1)
 1)

(my-assert
 (multiple-value-bind (a b)
     (gethash "one" table)
   (list a b))
 (  NIL nil))

(my-assert
 (progn
   (setq table (make-hash-table :test 'equal))
   t)
 t)
					;  #<HASH-TABLE EQUAL 0/139 46145547>

(my-assert
 (setf (gethash "one" table) 1)
 1)

(my-assert
 (multiple-value-bind (a b)
     (gethash "one" table)
   (list a b))
 (  1 T))

(my-assert
 (progn
   (make-hash-table :rehash-size 1.5 :rehash-threshold 0.7)
   t)
 t)
					;  #<HASH-TABLE EQL 0/120 46156620>

;;; hash-table-p

(my-assert
 (progn
   (setq table (make-hash-table))
   t)
 t)
					; #<HASH-TABLE EQL 0/120 32511220>

(my-assert
 (hash-table-p table)
 t)

(my-assert
 (hash-table-p 37)
 nil)

(my-assert
 (hash-table-p '((a . 1) (b . 2)))
 nil)

;; hash-table-count

(my-assert
 (progn
   (setq table (make-hash-table))
   t)
 t)
					;  #<HASH-TABLE EQL 0/120 32115135>

(my-assert
 (hash-table-count table)
 0)

(my-assert
 (setf (gethash 57 table) "fifty-seven")
 "fifty-seven")

(my-assert
 (hash-table-count table)
 1)

(my-assert
 (dotimes (i 100) (setf (gethash i table) i))
 NIL)

(my-assert
 (hash-table-count table)
 100)

;;; hash-table-rehash-size

(my-assert
 (progn (setq table (make-hash-table :size 100 :rehash-size 1.4))
	t)
 t)
					;  #<HASH-TABLE EQL 0/100 2556371>

(my-assert
 (hash-table-rehash-size table)
 #-clisp 1.4
 #+clisp 1.4s0)

;;; HASH-TABLE-REHASH-THRESHOLD

(my-assert
 (progn
   (setq table (make-hash-table :size 100 :rehash-threshold 0.5))
   t)
 t)
					;  #<HASH-TABLE EQL 0/100 2562446>

(my-assert
 (hash-table-rehash-threshold table)
 #-clisp 0.5
 #+clisp 0.75s0)


;;; get-hash

(my-assert
 (progn
   (setq table (make-hash-table))
   t)
 t)


(my-assert
 (multiple-value-bind (a b)
     (gethash 1 table)
   (list a b))
 (NIL nil))

(my-assert
 (multiple-value-bind (a b)
     (gethash 1 table 2)
   (list a b))
 (2 nil))

(my-assert
 (setf (gethash 1 table) "one")
 "one")

(my-assert
 (setf (gethash 2 table "two") "two")
 "two")

(my-assert
 (multiple-value-bind (a b)
     (gethash 1 table)
   (list a b))
 ("one" t))

(my-assert
 (multiple-value-bind (a b)
     (gethash 2 table)
   (list a b))
 ("two" t))

(my-assert
 (multiple-value-bind (a b)
     (gethash nil table)
   (list a b))
 (NIL nil))

(my-assert
 (setf (gethash nil table) nil)
 NIL)

(my-assert
 (multiple-value-bind (a b)
     (gethash nil table)
   (list a b))
 (NIL t))

(unintern '*counters*)

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

;;; remhash

(setq table (make-hash-table))

(my-assert
 (setf (gethash 100 table) "C")
 "C")
(my-assert
 (multiple-value-bind (a b)
     (gethash 100 table)
   (list a b))
 ("C" t))

(my-assert
 (remhash 100 table)
 t)

(my-assert
 (multiple-value-bind (a b)
     (gethash 100 table)
   (list a b))
 (NIL nil))

(my-assert
 (remhash 100 table)
 nil)

;;; maphash

(setq table (make-hash-table))

(my-assert
 (dotimes (i 10) (setf (gethash i table) i))
 NIL)

(my-assert
 (let ((sum-of-squares 0))
   (maphash #'(lambda (key val)
		(let ((square (* val val)))
		  (incf sum-of-squares square)
		  (setf (gethash key table) square)))
	    table)
   sum-of-squares)
 285)

(my-assert
 (hash-table-count table)
 10)

(my-assert
 (maphash #'(lambda (key val)
	      (when (oddp val) (remhash key table)))
	  table)
 NIL)

(my-assert
 (hash-table-count table)
 5)

(my-assert
 (let ((a nil))
   (maphash #'(lambda (k v) (setq a (cons  (list k v) a ))) table)
   a)
 #-clisp
 ((8 64) (6 36) (4 16) (2 4) (0 0))
 #+clisp
 ((0 0) (2 4) (4 16) (6 36) (8 64)))

;;; with-hash-table-iterator

(my-assert
 (defun test-hash-table-iterator (hash-table)
   (let ((all-entries '())
	 (generated-entries '())
	 (unique (list nil)))
     (maphash #'(lambda (key value) (push (list key value) all-entries))
	      hash-table)
     (with-hash-table-iterator (generator-fn hash-table)
			       (loop
				 (multiple-value-bind (more? key value) (generator-fn)
				   (unless more? (return))
				   (unless (eql value (gethash key hash-table unique))
				     (error "Key ~S not found for value ~S" key value))
				   (push (list key value) generated-entries))))
     (unless (= (length all-entries)
		(length generated-entries)
		(length (union all-entries generated-entries
			       :key #'car :test (hash-table-test hash-table))))
       (error "Generated entries and Maphash entries don't correspond"))
     t))
 test-hash-table-iterator)

(my-assert
 (test-hash-table-iterator table)
 t)

;;; clrhash

(setq table (make-hash-table))

(my-assert
 (dotimes (i 100) (setf (gethash i table) (format nil "~R" i)))
 NIL)

(my-assert
 (hash-table-count table)
 100)

(my-assert
 (multiple-value-bind (a b)
     (gethash 57 table)
   (list a b))
 ("fifty-seven" t))

(clrhash table)

(my-assert
 (hash-table-count table)
 0)

(my-assert
 (multiple-value-bind (a b)
     (gethash 57 table)
   (list a b))
 ( NIL nil))

;;; sxhash

(my-assert
 (= (sxhash (list 'list "ab")) (sxhash (list 'list "ab")))
 t)


(my-assert
 (= (sxhash "a") (sxhash (make-string 1 :initial-element #\a)))
 t)


(my-assert
 (let ((r (make-random-state)))
   (= (sxhash r) (sxhash (make-random-state r))))
 t)









