;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
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

(defun wt-label (label)
  (when (cdr label) (wt-nl1 "L" (car label) ":;")))

(defun wt-filtered-comment (text stream single-line)
  (declare (string text))
  (if single-line
      (progn
	(terpri stream)
	(princ "/*	" stream))
      (format stream "~50T/*  "))
  (let* ((l (1- (length text))))
    (declare (fixnum l))
    (dotimes (n l)
      (let ((c (schar text n)))
	(princ c stream)
	(when (and (char= c #\*) (char= (schar text (1+ n)) #\/))
	  (princ #\\ stream))))
    (princ (schar text l) stream))
  (format stream "~70T*/")
  )

(defun wt-comment (message &optional extra (single-line extra))
  (when (and (symbolp message) (not (symbol-package message)))
    ;; useless to show gensym's
    (return-from wt-comment))
  (wt-filtered-comment (format nil "~A~A" message (or extra ""))
		       *compiler-output1*
		       single-line))

(defun wt1 (form)
  (typecase form
    ((or STRING INTEGER CHARACTER)
     (princ form *compiler-output1*))
    ((or DOUBLE-FLOAT SINGLE-FLOAT)
     (format *compiler-output1* "~10,,,,,,'eG" form))
    (LONG-FLOAT
     (format *compiler-output1* "~,,,,,,'eEl" form))
    (VAR (wt-var form))
    (t (wt-loc form)))
  nil)

(defun wt-h1 (form)
  (if (consp form)
      (let ((fun (get-sysprop (car form) 'wt-loc)))
	(if fun
	    (let ((*compiler-output1* *compiler-output2*))
	      (apply fun (cdr form)))
	    (cmperr "The location ~s is undefined." form)))
      (princ form *compiler-output2*))
  nil)

;;; This routine converts lisp data into C-strings. We have to take
;;; care of escaping special characteres with backslashes. We also have
;;; to split long lines using  the fact that multiple strings are joined
;;; together by the compiler.
;;;
(defun wt-filtered-data (string stream)
  (let ((N (length string))
	(wt-data-column 80))
    (incf *wt-string-size* (1+ N)) ; 1+ accounts for a blank space
    (format stream "~%\"")
    (dotimes (i N)
      (decf wt-data-column)
      (when (< wt-data-column 0)
	(format stream "\"~% \"")
	(setq wt-data-column 79))
      (let ((x (aref string i)))
	(cond
	  ((or (< (char-code x) 32)
	       (> (char-code x) 127))
	   (case x
	     ; We avoid a trailing backslash+newline because some preprocessors
	     ; remove them.
	     (#\Newline (princ "\\n" stream))
	     (#\Tab (princ "\\t" stream))
	     (t (format stream "\\~3,'0o" (char-code x)))))
	  ((char= x #\\)
	   (princ "\\\\" stream))
	  ((char= x #\")
	   (princ "\\\"" stream))
	  (t (princ x stream)))))
    (princ " \"" stream)
    string))

;;; ======================================================================
;;;
;;; DATA FILES
;;;

(defun data-permanent-storage-size ()
  (length *permanent-objects*))

(defun data-temporary-storage-size ()
  (length *temporary-objects*))

(defun data-size ()
  (+ (data-permanent-storage-size)
     (data-temporary-storage-size)))

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
      (with-open-file (s filename :direction :input)
	(setf *permanent-objects* (read s)
	      *temporary-objects* (read s)))
      (setf *permanent-objects* (make-array 128 :adjustable t :fill-pointer 0)
	    *temporary-objects* (make-array 128 :adjustable t :fill-pointer 0))))

(defun data-get-all-objects ()
  ;; We collect all objects that are to be externalized, but filter out
  ;; those which will be created by a lisp form.
  (loop for i in (nconc (map 'list #'first *permanent-objects*)
			(map 'list #'first *temporary-objects*))
	collect (if (gethash i *load-objects*)
		    0
		    i)))

(defun data-dump (stream &key as-lisp-file init-name &aux must-close)
  (etypecase stream
    (null (return-from data-dump))
    ((or pathname string)
     (setf stream (open stream :direction :output :if-does-not-exist :create
			:if-exists :supersede)
	   must-close stream))
    (stream))
  (let ((*print-radix* nil)
	(*print-base* 10)
	(*print-circle* t)
	(*print-pretty* nil)
	(*print-level* nil)
	(*print-length* nil)
	(*print-case* :downcase)
	(*print-gensym* t)
	(*print-array* t)
	(*print-readably* t)
	(*read-default-float-format* 'single-float)
	(sys::*print-package* (find-package "CL"))
	(sys::*print-structure* t)
	(output nil))
    (cond (as-lisp-file
	   (print *permanent-objects* stream)
	   (print *temporary-objects* stream))
	  (*compiler-constants*
	   (format stream "~%#define compiler_data_text NULL~%#define compiler_data_text_size 0~%")
	   (setf output (concatenate 'vector (data-get-all-objects))))
	  ((plusp (data-size))
	   (wt-data-begin stream)
	   (wt-filtered-data
	    (subseq (prin1-to-string (data-get-all-objects)) 1)
	    stream)
	   (wt-data-end stream)))
    (when must-close
      (close must-close))
    (data-init)
    output))

(defun wt-data-begin (stream)
  (setq *wt-string-size* 0)
  (setq *wt-data-column* 80)
  (princ "static const char compiler_data_text[] = " stream)
  nil)

(defun wt-data-end (stream)
  (princ #\; stream)
  (format stream "~%#define compiler_data_text_size ~D~%" *wt-string-size*)
  (setf *wt-string-size* 0))

(defun data-empty-loc ()
  (add-object 0 :duplicate t :permanent t))

(defun add-load-form (object location)
  (when (clos::need-to-make-load-form-p object)
    (if (not (eq *compiler-phase* 't1))
	(cmperr "Unable to internalize complex object ~A in ~a phase" object *compiler-phase*)
	(multiple-value-bind (make-form init-form) (make-load-form object)
	  (setf (gethash object *load-objects*) location)
	  (when make-form
	    (push (make-c1form* 'MAKE-FORM :args location (c1expr make-form)) *make-forms*))
	  (when init-form
	    (push (make-c1form* 'INIT-FORM :args location (c1expr init-form)) *make-forms*))))))

(defun add-object (object &key (duplicate nil)
		   (permanent (or (symbolp object) *permanent-data*)))
  ;; FIXME! Currently we have two data vectors and, when compiling
  ;; files, it may happen that a constant is duplicated and stored
  ;; both in VV and VVtemp. This would not be a problem if the
  ;; constant were readable, but due to using MAKE-LOAD-FORM we may
  ;; end up having two non-EQ objects created for the same value.
  (let* ((test (if *compiler-constants* 'eq 'equal))
	 (array (if permanent *permanent-objects* *temporary-objects*))
	 (vv (if permanent 'VV 'VV-temp))
	 (x (or (and (not permanent)
		     (find object *permanent-objects* :test test
			   :key #'first))
		(find object array :test test :key #'first)))
	 (next-ndx (length array))
	 found)
    (cond ((and x duplicate)
	   (setq x (list vv next-ndx))
	   (vector-push-extend (list object x next-ndx) array)
	   x)
	  (x
	   (second x))
	  ((and (not duplicate)
		(symbolp object)
		(multiple-value-setq (found x) (si::mangle-name object)))
	   x)
	  (t
	   (setq x (list vv next-ndx))
	   (vector-push-extend (list object x next-ndx) array)
	   (unless *compiler-constants*
	     (add-load-form object x))
	   x))))

(defun add-symbol (symbol)
  (add-object symbol :duplicate nil :permanent t))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list
  ;; *OBJECTS* contains elements in decreasing order!!!
  (let ((x (search keywords *permanent-objects*
		   :test #'(lambda (k record) (eq k (first record))))))
    (if x
	(progn
	  (cmpnote "~@<Reusing keywords lists for ~_~A~@:>" keywords)
	  (second (elt *permanent-objects* x)))
	(prog1
	    (add-object (pop keywords) :duplicate t :permanent t)
	  (dolist (k keywords)
	    (add-object k :duplicate t :permanent t))))))
