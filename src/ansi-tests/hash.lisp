;;; based on v1.3 -*- mode: lisp -*-
(in-package :cl-user)

(my-assert
 (progn (in-package (quote sys)) t)      t)

(my-assert
 (make-hash-table :test (quote eq) :size 20)
 #s(hash-table test eq size 20 %%size 64 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :test (quote eql) :size 2)
 #s(hash-table test eql size 2 %%size 4 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :test (quote equal) :size 2)
 #s(hash-table test equal size 2 %%size 4 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (progn (make-hash-table :test (function eq) :size 2) t) t)

(my-assert
 (progn (make-hash-table :test (function eql) :size 2)t) t)

(my-assert
 (make-hash-table :size nil)
 error)

(my-assert
 (make-hash-table :size -3)
 error)

(my-assert
 (make-hash-table :size 2.0)
 error)

(my-assert
 (make-hash-table :size 2 :rehash-size 1.5)
 #s(hash-table test eql size 2 %%size 4 rehash-size 1.5 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :size 2 :rehash-size -1.5)
 error)

(my-assert
 (make-hash-table :size 2 :rehash-size 0.5)
 error)

(my-assert
 (make-hash-table :size 2 :rehash-size 1.0)
 #s(hash-table test eql size 2 %%size 4 rehash-size 1.0 %%rehash-size 4
	       rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :size 2 :rehash-size 5)
 #s(hash-table test eql size 2 %%size 4 rehash-size 5 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 2 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :size 4 :rehash-size 5.0)
 #s(hash-table test eql size 4 %%size 8 rehash-size 5.0 %%rehash-size 8
	       rehash-threshold 13 %%rehash-threshold 3 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element)))

(my-assert
 (make-hash-table :size 2 :rehash-size nil)
 error)

(my-assert
 (make-hash-table :size 2 :rehash-threshold nil)
 error)

(my-assert
 (make-hash-table :%%size 3)
 #s(hash-table test eql size 16 %%size 3 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element)))

(my-assert
 (setq tab (make-hash-table))
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

(my-assert
 (setf-gethash (quote hallo) tab (quote wiegwhts))
 wiegwhts)

(my-assert
 (setf-gethash (quote uhu) tab (quote kauz))
 kauz)

(my-assert
 (gethash (quote uhu) tab)
 kauz)

(my-assert
 (gethash uhu tab)
 error)

(my-assert
 (make-hash-table)
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

(my-assert
 (setq tab nil)
 nil)

(my-assert
 (setf-gethash (quote uhu) tab (quote kaus))
 error)

(my-assert
 (gethash (quote uhu) tab)
 error)

(my-assert
 (gethash (quote otto) tab)
 error)

(my-assert
 (setq tab (make-hash-table))
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

(my-assert
 (setf-gethash (quote uhu) tab (quote kaus))
 kaus)

(my-assert
 (gethash (quote uhu) tab)
 kaus)

(my-assert
 (gethash (quote otto) tab)
 nil)

(my-assert
 (setf-gethash (quote uhu) tab (quote kauz))
 kauz)

(my-assert
 (setf-gethash tab)
 error)

(my-assert
 (remhash (quote uhu) tab)
 t)

(my-assert
 tab
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

(my-assert
 (clrhash tab9)
 error)

(my-assert
 (clrhash tab)
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

(my-assert
 (hash-table-count tab)
 0)

(my-assert
 (setf-gethash (quote klak) tab (quote klase))
 klase)

(my-assert
 (setf-gethash (quote kunze) tab (quote riese))
 riese)

(my-assert
 (hash-table-p tab)
 t)

(my-assert
 (hash-table-count tab)
 2)

(my-assert
 (remhash (quote kunze) tab)
 t)

(my-assert
 (setf-gethash (quote wald) tab (quote khjgsfgjhdf))
 khjgsfgjhdf)

(my-assert
 (gethash)
 error)

(my-assert
 (remhash)
 error)

(my-assert
 (clrhash tab)
 #s(hash-table test eql size 16 %%size 32 rehash-size 2.0 %%rehash-size 2
	       rehash-threshold 13 %%rehash-threshold 13 %%count 0 %%hash-vektor
	       #(%%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element %%empty-element %%empty-element
				 %%empty-element %%empty-element %%empty-element)))

