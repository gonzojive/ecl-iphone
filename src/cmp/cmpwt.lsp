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

(defun wt-comment (message &optional extra)
  (if extra
    ;; Message is a prefix string for EXTRA. All fits in a single line.
    (progn
      (terpri *compiler-output1*)
      (princ "/*	" *compiler-output1*)
      (princ message *compiler-output1*))
    ;; Message is a symbol.
    (if (or (not (symbolp message)) (symbol-package message))
	(progn
	  (format *compiler-output1* "~50T/*  ")
	  (setq extra message))
	;; useless to show gensym's
	(return-from wt-comment)))
  (let* ((s (if (symbolp extra) (symbol-name extra) (format nil "~A" extra)))
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

(defun data-init (&optional filename)
  (if (and filename (probe-file filename))
    (with-open-file (s filename :direction :input)
      (setf *objects* (read s)
	    *next-vv* (read s)))
    (setf *objects* '()
	  *next-vv* -1)))

(defun data-size ()
  (1+ *next-vv*))

(defun data-dump (stream &optional as-lisp-file &aux must-close)
  (etypecase stream
    (null (return-from data-dump))
    ((or pathname string)
     (setf stream (open stream :direction :output
			:if-does-not-exist :create
			:if-exists :overwrite)))
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
	(*read-default-float-format* 'single-float)
	(sys::*print-package* (find-package "CL"))
	(sys::*print-structure* t))
    (if as-lisp-file
	(progn
	  (print *objects* stream)
	  (print *next-vv* stream))
	(unless (zerop (data-size))
	  (wt-data-begin stream)
	  (wt-filtered-data
	   (subseq (prin1-to-string (nreverse (mapcar #'car *objects*))) 1)
	   stream)
	  (wt-data-end stream))))
  (when must-close
    (close stream))
  (setf *objects* nil
	*next-vv* -1))

(defun wt-data (expr stream)
  (wt-filtered-data
   (typecase expr
     (FUNCTION
      (prin1-to-string (sys:compiled-function-name expr)))
     (PACKAGE
      (format nil "~%#.(find-package ~S)" (package-name expr)))
     (t (prin1-to-string expr)))
   stream))

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
  (let ((x `(VV ,(incf *next-vv*))))
    (push (list 0 x *next-vv*) *objects*)
    x))

(defun add-object (object &optional (duplicate nil))
  (when (typep object '(or function package))
    (error "Object ~S cannot be externalized" object))
  (let ((x (assoc object *objects* :test 'equal))
	(found nil))
    (cond ((and x duplicate)
	   (setq found `(VV ,(incf *next-vv*)))
	   (push (list object found *next-vv* (- (1+ (third x)))) *objects*)
	   found)
	  (x
	   (second x))
	  ((and (not duplicate)
		(symbolp object)
		(multiple-value-setq (found x) (si::mangle-name object)))
	   x)
	  (t (setq x `(VV ,(incf *next-vv*)))
	     (push (list object x *next-vv*) *objects*)
	     x))))

(defun add-symbol (symbol)
  (add-object symbol))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  ;; We search for keyword lists that are similar. However, the list
  ;; *OBJECTS* contains elements in decreasing order!!!
  (let ((x (search (reverse keywords) *objects*
		   :test #'(lambda (k rec) (eq k (first rec))))))
    (if x
	(progn
	  (cmpnote "Reusing keywords lists for ~S" keywords)
	  (second (elt *objects* (+ x (length keywords) -1))))
	(let ((x (add-object (first keywords) t)))
	  (dolist (k (rest keywords) x)
	    (add-object k t))))))
