;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPWT  Output routines.

(in-package "COMPILER")

(defvar *wt-string-size* 0)

;;; Each lisp compiled file consists on code and a data section. Whenever an
;;; #'in-package toplevel form is found, a read-time evaluated expression is
;;; inserted in the data section which changes the current package for the
;;; rest of it. This way it is possible to save some space by writing the
;;; symbol's package only when it does not belong to the current package.

(defun wt-comment (message &optional symbol)
  (if symbol
    (progn
      (terpri *compiler-output1*)
      (princ "/*	" *compiler-output1*)
      (princ message *compiler-output1*))
    (if (symbol-package message)
	(progn
	  (format *compiler-output1* "~50T/*  ")
	  (setq symbol message))
	;; useless to show gensym's
	(return-from wt-comment)))
  (let* ((s (symbol-name symbol))
         (l (1- (length s)))
         c)
    (declare (string s) (fixnum l) (character c))
    (dotimes (n l)
      (setq c (schar s n))
      (princ c *compiler-output1*)
      (when (and (char= c #\*) (char= (schar s (1+ n)) #\/))
        (princ #\\ *compiler-output1*)))
    (princ (schar s l) *compiler-output1*))
  (format *compiler-output1* "~70T*/")
  nil
  )

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    ((or LONG-FLOAT SHORT-FLOAT)
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (if (consp form)
    (let ((fun (get (car form) 'wt)))
      (if fun
        (apply fun (cdr form))
        (cmperr "The location ~s is undefined." form)))
    (princ form *compiler-output2*))
  nil)

;;; This routine converts lisp data into C-strings. We have to take
;;; care of escaping special characteres with backslashes.
;;;
;;; FIXME! With some compilers (GCC, for instance), four backslashes
;;; at the end of a line are turned into a single backslash!!!!
;;; Current fix is ugly.
(defun wt-filtered-data (string)
  (let ((N (length string)))
    (incf *wt-string-size* (1+ N)) ; 1+ accounts for newline
    (terpri *compiler-output-data*)
    (dotimes (i N)
      (let ((x (aref string i)))
	(cond
	  ((or (< (char-code x) 32)
	       (> (char-code x) 127))
	   (case x
	     ; We avoid a trailing backslash+newline because some preprocessors
	     ; remove them.
	     (#\Newline
	      (if (and (> i 0) (char= (aref string (1- i)) #\\))
		(princ "\\n" *compiler-output-data*)
		(princ #\Newline *compiler-output-data*)))
	     (#\Tab (princ "\\t" *compiler-output-data*))
	     (t (format *compiler-output-data* "\\~3,'0o" (char-code x)))))
	  ((char= x #\\)
	   (princ "\\\\" *compiler-output-data*))
	  ((char= x #\")
	   (princ "\\\"" *compiler-output-data*))
	  (t (princ x *compiler-output-data*)))))
    string))

;;; This routine outputs some data into the C file data section. The objects
;;; which are output include all symbols and all constants. To avoid
;;; superfluous package names when printing symbols, we bind *package* to the
;;; package of the last in-package form before printing anything.
(defvar *compiler-package* (find-package "CL"))

(defun wt-data (expr)
  (let ((*print-radix* nil)
        (*print-base* 10)
        (*print-circle* t)
        (*print-pretty* nil)
        (*print-level* nil)
        (*print-length* nil)
        (*print-case* :downcase)
        (*print-gensym* t)
        (*print-array* t)
	(*read-default-float-format* 'single-float)
	;(*package* *compiler-package*)
	;(sys::*print-package* *compiler-package*)
	(sys::*print-package* (symbol-package 'nil))
        (sys::*print-structure* t))
    (wt-filtered-data
     (typecase expr
       (COMPILED-FUNCTION
	(prin1-to-string (sys:compiled-function-name expr)))
       (PACKAGE
	(format nil "~%#.(find-package ~S)" (package-name expr)))
       (t (prin1-to-string expr))))
    nil))

(defun wt-data-begin ()
  (setq *wt-string-size* 0)
  (princ #\" *compiler-output-data*)
  (wt-filtered-data (format nil "#!0 ~s" "CL"))
  nil)

(defun wt-data-end ()
  (princ #\" *compiler-output-data*))

(defun wt-data-package-operation (form)
  (ecase (car form)
    (si::select-package
     (let ((output (t1ordinary form)))
       (cmp-eval form)
       (let ((package-name (cadr form)))
	 (setq *compiler-package* (si::select-package package-name)))
       output))
	 ;#+nil(wt-filtered-data (format nil "#!0 ~s" (string package-name)))))
    (si::%defpackage
     (let ((output (t1ordinary `(eval ',form))))
       (wt-filtered-data (format nil "#!1 ~s" (second form)))
       (cmp-eval form)
       output))))