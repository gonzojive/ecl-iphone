;;;;  Copyright (c) 1995, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                        list manipulating routines

(in-package "SYSTEM")

(eval-when (compile) (proclaim '(optimize (safety 0) (space 3))))

(defun union (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (consp x) (error "UNION not passed a list"))
    (unless (apply #'member1 (car x) list2 rest)
      (if last
	  (progn (rplacd last (cons (car x) nil))
		 (setq last (cdr last)))
	  (progn (setq first (cons (car x) nil))
		 (setq last first))))))

(defun nunion (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last list2))
       (or first list2))
    (unless (consp x) (error "NUNION not passed a list"))
    (unless (apply #'member1 (car x) list2 rest)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun intersection (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (ans))
      ((null x)
       (nreverse ans)) ; optional nreverse: not required by CLtL
    (unless (consp x) (error "INTERSECTION not passed a list"))
    (when (apply #'member1 (car x) list2 rest)
        (push (car x) ans))))

(defun nintersection (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (unless (consp x) (error "NINTERSECTION not passed a list"))
    (when (apply #'member1 (car x) list2 rest)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun set-difference (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (ans))
      ((null x) (nreverse ans))
    (unless (consp x) (error "SET-DIFFERENCE not passed a list"))
    (unless (apply #'member1 (car x) list2 rest)
      (push (car x) ans))))

(defun nset-difference (list1 list2 &rest rest)
  (do ((x list1 (cdr x))
       (first) (last))
      ((null x)
       (when last (rplacd last nil))
       first)
    (unless (consp x) (error "NSET-DIFFERENCE not passed a list"))
    (unless (apply #'member1 (car x) list2 rest)
      (if last
	  (rplacd last x)
	  (setq first x))
      (setq last x))))

(defun set-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (nconc (apply #'set-difference list1 list2 rest)
         (apply #'set-difference list2 list1 rest)))

(defun nset-exclusive-or (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (nconc (apply #'set-difference list1 list2 rest)
	 (apply #'nset-difference list2 list1 rest)))

(defun subsetp (list1 list2 &rest rest &key test test-not key)
  (declare (ignore test test-not key))
  (do ((l list1 (cdr l)))
      ((null l) t)
    (unless (consp l) (error "SUBSETP not passed a list"))
    (unless (apply #'member1 (car l) list2 rest)
      (return nil))))
