;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;                    Exporting external symbols of LISP package

(eval-when (eval compile load)
  (si::select-package "SI"))

;;; ----------------------------------------------------------------------
;;;
(*make-special '*dump-defun-definitions*)
(setq *dump-defun-definitions* nil)
(*make-special '*dump-defmacro-definitions*)
(setq *dump-defmacro-definitions* *dump-defun-definitions*)

;; This is needed only when bootstrapping ECL using ECL-MIN
(eval-when (eval)
  (si::fset 'defun
	  #'(ext::lambda-block defun (def env)
	      (let* ((name (second def))
		     (function `#'(ext::lambda-block ,@(cdr def))))
		(when *dump-defun-definitions*
		  (print function)
		  (setq function `(si::bc-disassemble ,function)))
		`(si::fset ',name ,function)))
	  t)
 (si::fset 'in-package
 	  #'(ext::lambda-block in-package (def env)
	      `(eval-when (eval compile load)
		(si::select-package ,(string (second def)))))
 	  t)
)

;;
;; This is also needed for booting ECL. In particular it is required in
;; defmacro.lsp.
;;
(let ((f #'(ext::lambda-block do/do*-expand (whole env)
           (let (do/do* control test result vl step let psetq body)
	     (setq do/do* (first whole) body (rest whole))
	     (if (eq do/do* 'do)
		 (setq let 'LET psetq 'PSETQ)
		 (setq let 'LET* psetq 'SETQ))
	     (when (endp body)
	       (simple-program-error "Syntax error in DO/DO* body:~%~A" whole))
	     (setq control (first body) body (rest body))
	     (when (endp body)
	       (simple-program-error "Syntax error in DO/DO* body:~%~A" whole))
	     (setq test (first body) body (rest body))
	     (when (endp test)
	       (simple-program-error "Syntax error in DO/DO* body:~%~A" whole))
	     (setq result (rest test) test (first test))
	     (dolist (c control)
	       (when (symbolp c) (setq c (list c)))
	       (case (length c)
		 ((1 2)
		  (setq vl (cons c vl)))
		 ((3)
		  (setq vl (cons (butlast c) vl)
			step (list* (third c) (first c) step)))
		 (t
		  (simple-program-error "Syntax error in DO/DO* body:~%~A" whole))))
	     (multiple-value-bind (declarations real-body doc)
		 (process-declarations body nil)
	       `(BLOCK NIL
		 (,let ,(nreverse vl)
		   (declare ,@declarations)
		   (sys::while ,test
			       ,@real-body
			       ,@(when step (list (cons psetq (nreverse step)))))
		   ,@(or result '(nil)))))))))
  (si::fset 'do f t)
  (si::fset 'do* f t))

(defun eval-feature (x)
  (declare (si::c-local))
  (cond ((symbolp x)
         (member x *features*
                 :test #'(lambda (a b)
                           (or (eql a b)
			       (and (symbolp a) (symbolp b)
				    (string-equal (symbol-name a)
						  (symbol-name b)))))))
	((atom x) (error "~ is not allowed as a feature" x))
        ((eq (car x) 'AND)
         (dolist (x (cdr x) t) (when (not (eval-feature x)) (return nil))))
        ((eq (car x) 'OR)
         (dolist (x (cdr x) nil) (when (eval-feature x) (return t))))
        ((eq (car x) 'NOT)
	 (not (eval-feature (second x))))
	(t (error "~S is not a feature expression." x))))

;;; Revised by G. Attardi
(defun check-no-infix (stream subchar arg)
  (declare (si::c-local))
  (when arg
    (error "Reading from ~S: no number should appear between # and ~A"
	   stream subchar)))

(defun sharp-+-reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (and (not *read-suppress*) (eval-feature feature))
	(read stream t nil t)
	(let ((*read-suppress* t)) (read stream t nil t) (values)))))

(set-dispatch-macro-character #\# #\+ 'sharp-+-reader)
(set-dispatch-macro-character #\# #\+ 'sharp-+-reader
                              (sys::standard-readtable))

(defun sharp---reader (stream subchar arg)
  (check-no-infix stream subchar arg)
  (let ((feature (read stream t nil t)))
    (if (or *read-suppress* (eval-feature feature))
	(let ((*read-suppress* t)) (read stream t nil t) (values))
	(read stream t nil t))))

(set-dispatch-macro-character #\# #\- 'sharp---reader)
(set-dispatch-macro-character #\# #\- 'sharp---reader
                              (sys::standard-readtable))
