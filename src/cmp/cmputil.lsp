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

(define-condition compiler-message (condition)
  ((file :initarg :file :initform *compile-file-pathname*
	 :accessor compiler-message-file)
   (position :initarg :file :initform *compile-file-position*
	     :accessor compiler-message-file-position)
   (form :initarg :form :initform *current-form* :accessor compiler-message-form)))

(define-condition compiler-note (compiler-message simple-condition warning) ())

(define-condition compiler-warning (compiler-message simple-condition style-warning) ())

(define-condition compiler-error (compiler-message simple-error) ())

(define-condition compiler-fatal-error (compiler-error) ())

(define-condition compiler-internal-error (compiler-fatal-error) ())

(define-condition compiler-undefined-variable (compiler-message warning)
  ((variable :initarg :name :initform nil))
  (:report
   (lambda (condition stream)
     (format stream "Variable ~A was undefined. Compiler assumes it is a global."
	     (slot-value condition 'variable)))))

(defun handle-fatal-error (c)
  (push c *compiler-conditions*)
  (abort))

(defun print-compiler-message (c stream &optional (header "Error"))
  (let ((position (compiler-message-file-position c)))
    (if position
	(let ((*print-length* 3)
	      (*print-level* 2))
	  (format stream "~&;;; ~A: in file ~A, position ~D, and form ~%;;;   ~A~%~@<;;; ~@;~A~:>"
		  header (compiler-message-file c)
		  position (compiler-message-form c) c))
	(format stream "~&~@<;;; ~@;~A: ~A~:>" header c))))

(defun handle-note (c)
  (unless *suppress-compiler-notes*
    (print-compiler-message c t "Note")))

(defun handle-warning (c)
  (push c *compiler-conditions*)
  (unless *suppress-compiler-warnings*
    (print-compiler-message c t "Warning")))

(defun handle-error (c)
  (push c *compiler-conditions*)
  (print-compiler-message c t)
  (invoke-restart (find-restart-never-fail 'abort-form c)))

(defmacro with-compiler-env ((error-flag) &body body)
  `(with-lock (+load-compile-lock+)
     (restart-case
	 (handler-bind ((compiler-note #'handle-note)
			(compiler-warning #'handle-warning)
			(compiler-error #'handle-error)
			(compiler-fatal-error #'handle-fatal-error))
	   (let ,+init-env-form+
	     (setf ,error-flag nil)
	     ,@body))
       (abort (c) (setf ,error-flag t))
       (abort-form (c) (setf ,error-flag t)))))

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

(defun cmpprogress (&rest args)
  (when *compile-verbose*
    (apply #'format t args)))

(defun cmperr (string &rest args)
  (signal 'compiler-error
	  :format-control string
	  :format-arguments args))

(defun check-args-number (operator args &optional (min 0) (max nil))
  (let ((l (length args)))
    (when (< l min)
      (too-few-args operator min l))
    (when (and max (> l max))
      (too-many-args operator max l))))

(defun too-many-args (name upper-bound n &aux (*print-case* :upcase))
  (cmperr "~S requires at most ~R argument~:p, but ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          upper-bound
          n))

(defun too-few-args (name lower-bound n)
  (cmperr "~S requires at least ~R argument~:p, but only ~R ~:*~[were~;was~:;were~] supplied.~%"
          name
          lower-bound
          n))

(defun do-cmpwarn (&rest args)
  (declare (si::c-local))
  (let ((condition (apply #'make-condition args)))
    (restart-case (signal condition)
      (muffle-warning ()
	:REPORT "Skip warning"
	(return-from do-cmpwarn nil)))))

(defun cmpwarn (string &rest args)
  (do-cmpwarn 'compiler-warning
    :format-control string
    :format-arguments args))

(defun cmpnote (string &rest args)
  (do-cmpwarn 'compiler-note
    :format-control string
    :format-arguments args))

(defun print-current-form ()
  (when *compile-verbose*
    (let ((*print-length* 2)
	  (*print-level* 2))
      (format t "~&;;; Compiling ~s.~%" *current-form*)))
  nil)

(defun print-emitting (f)
  (let* ((name (fun-name f)))
    (unless name
      (setf name (fun-description f)))
    (when (and name *compile-verbose*)
      (format t "~&;;; Emitting code for ~s.~%" name))))

(defun undefined-variable (sym)
  (signal 'compiler-undefined-variable :name sym))
  
(defun baboon (&aux (*print-case* :upcase))
  (signal 'compiler-internal-error
	  :format-control "A bug was found in the compiler.  Contact jjgarcia@users.sourceforge.net"
	  :format-arguments nil))
  
(defmacro with-cmp-protection (main-form error-form)
  `(let* ((si::*break-enable* *compiler-break-enable*)
          (throw-flag t))
     (unwind-protect
	 (multiple-value-prog1 ,main-form
			       (setf throw-flag nil))
       (when throw-flag ,error-form))))

(defun cmp-eval (form)
  (with-cmp-protection (eval form)
    (cmperr "~&;;; The form ~s was not evaluated successfully.~
             ~%;;; You are recommended to compile again.~%"
	    form)))
  
(defun cmp-macroexpand (form &optional (env *cmp-env*))
  (with-cmp-protection (macroexpand form env)
    (cmperr "~&;;; The macro form ~S was not expanded successfully.~
             ~%;;; You are recommended to compile again.~%" form)))
  
(defun cmp-expand-macro (fd form &optional (env *cmp-env*))
  (with-cmp-protection
    (let ((new-form (funcall *macroexpand-hook* fd form env)))
      (values new-form (not (eql new-form form))))
    (cmperr "~&;;; The macro form ~S was not expanded successfully.~
             ~%;;; You are recommended to compile again.~%" form)))

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
