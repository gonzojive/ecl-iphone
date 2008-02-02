;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPUTIL  --  Miscellaneous Functions.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defvar *c1form-level* 0)
(defun print-c1forms (form)
  (cond ((consp form)
	 (let ((*c1form-level* (1+ *c1form-level*)))
	   (mapc #'print-c1forms form)))
	((c1form-p form)
	 (format t "~% ~D > ~A, parent ~A" *c1form-level* form (c1form-parent form))
	 (print-c1forms (c1form-args form))
	 form
	 )))

(defun print-ref (ref-object stream)
  (let ((name (ref-name ref-object)))
    (if name
	(format stream "#<a ~A: ~A>" (type-of ref-object) name)
	(format stream "#<a ~A>" (type-of ref-object)))))

(defun print-var (var-object stream)
  (format stream "#<a VAR: ~A KIND: ~A>" (var-name var-object) (var-kind var-object)))

(defun cmperr (string &rest args &aux (*print-case* :upcase))
  (print-current-form)
  (format t "~&;;; Error: ")
  (apply #'format t string args)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun check-args-number (operator args &optional (min 0) (max nil))
  (let ((l (length args)))
    (when (< l min)
      (too-few-args operator min l))
    (when (and max (> l max))
      (too-many-args operator max l))))

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; ~S requires at most ~R argument~:p, ~
          but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          upper-bound
          n)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun too-few-args (name lower-bound n &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; ~S requires at least ~R argument~:p, ~
          but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          lower-bound
          n)
  (incf *error-count*)
  (throw *cmperr-tag* '*cmperr-tag*))

(defun cmpwarn (string &rest args &aux (*print-case* :upcase))
  (unless *suppress-compiler-warnings*
    (print-current-form)
    (format t "~&;;; Warning: ")
    (apply #'format t string args)
    (terpri))
  nil)

(defun cmpnote (string &rest args &aux (*print-case* :upcase))
  (unless *suppress-compiler-notes* 
    (format t "~&;;; Note: ")
    (apply #'format t string args)
    (terpri))
  nil)

(defun print-current-form ()
  (unless *suppress-compiler-notes*
    (let ((*print-length* 2)
	  (*print-level* 2))
      (format t "~&;;; Compiling ~s.~%" *current-form*)))
  nil)

(defun print-emitting (f)
  (let* ((name (fun-name f)))
    (unless name
      (setf name (fun-description f)))
    (when (and name (not *suppress-compiler-notes*))
      (format t "~&;;; Emitting code for ~s.~%" name))))

(defun undefined-variable (sym &aux (*print-case* :upcase))
  (print-current-form)
  (format t
          "~&;;; The variable ~s is undefined.~
           ~%;;; The compiler will assume this variable is a global.~%"
          sym)
  nil)

(defun baboon (&aux (*print-case* :upcase))
  (print-current-form)
  (error "~&;;; A bug was found in the compiler.  Contact worm@arrakis.es.~%")
  (format
   t "~&;;; A bug was found in the compiler.  Contact worm@arrakis.es.~%")
  (incf *error-count*)
  (break)
;  (throw *cmperr-tag* '*cmperr-tag*) DEBUG
)

(defmacro with-cmp-protection (main-form error-form)
  `(let* #+nil
     ((sys::*ihs-base* sys::*ihs-top*)
      (sys::*ihs-top* (sys::ihs-top 'cmp-toplevel-eval))
      (*break-enable* *compiler-break-enable*)
      (sys::*break-hidden-packages*
       (cons (find-package 'compiler)
	     sys::*break-hidden-packages*))
      (throw-flag t))
     ((*break-enable* *compiler-break-enable*)
      (throw-flag t))
     (unwind-protect
	 (multiple-value-prog1 ,main-form
	   (setf throw-flag nil))
       (when throw-flag ,error-form))))

(defun cmp-eval (form)
  (with-cmp-protection (eval form)
    (let ((*print-case* :upcase))
      (print-current-form)
      (format t "~&;;; The form ~s was not evaluated successfully.~
                 ~%;;; You are recommended to compile again.~%"
	      form))))

(defun cmp-macroexpand (form &optional (env *cmp-env*))
  (with-cmp-protection (macroexpand form env)
    (let ((*print-case* :upcase))
      (print-current-form)
      (format t "~&;;; The macro form ~S was not expanded successfully.~
                 ~%;;; You are recommended to compile again.~%" form))))

(defun cmp-expand-macro (fd form &optional (env *cmp-env*))
  (with-cmp-protection
    (let ((new-form (funcall *macroexpand-hook* fd form env)))
      (values new-form (not (eql new-form form))))
    (let ((*print-case* :upcase))
      (print-current-form)
      (format t "~&;;; The macro form ~S was not expanded successfully.~
                 ~%;;; You are recommended to compile again.~%" form))))

(defun si::compiler-clear-compiler-properties (symbol)
  #-:CCL
  ;(sys::unlink-symbol symbol)
  (rem-sysprop symbol 't1)
  (rem-sysprop symbol 't2)
  (rem-sysprop symbol 't3)
  (rem-sysprop symbol 'c1)
  (rem-sysprop symbol 'c2)
  (rem-sysprop symbol 'c1conditional)
  (rem-sysprop symbol ':inline-always)
  (rem-sysprop symbol ':inline-unsafe)
  (rem-sysprop symbol ':inline-safe)
  (rem-sysprop symbol 'lfun))
  
(defun lisp-to-c-name (obj)
  "Translate Lisp object prin1 representation to valid C identifier name"
  (and obj 
       (map 'string 
            #'(lambda (c)
                (let ((cc (char-code c)))
                  (if (or (<= #.(char-code #\a) cc #.(char-code #\z))
                          (<= #.(char-code #\0) cc #.(char-code #\9)))
                      c #\_)))
            (string-downcase (prin1-to-string obj)))))

(defun proper-list-p (x &optional test)
  (and (listp x)
       (handler-case (list-length x) (type-error (c) nil))
       (or (null test) (every test x))))
