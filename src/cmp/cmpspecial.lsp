;;;;  CMPSPECIAL  Miscellaneous special forms.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun c1quote (args)
  (check-args-number 'QUOTE args 1 1)
  (c1constant-value (car args) t))

(defun c1eval-when (args)
  (check-args-number 'EVAL-WHEN args 1)
  (dolist (situation (car args) (c1nil))
    (case situation
      ((EVAL :EXECUTE) (return-from c1eval-when (c1progn (cdr args))))
      ((LOAD COMPILE :LOAD-TOPLEVEL :COMPILE-TOPLEVEL))
      (otherwise
       (cmperr "The situation ~s is illegal." situation)))))

(defun c1declare (args)
  (cmperr "The declaration ~s was found in a bad place." (cons 'DECLARE args)))

(defun c1the (args)
  (check-args-number 'THE args 2 2)
  (let* ((form (c1expr (second args)))
	 (the-type (type-filter (first args)))
	 type)
    (cond ((and (consp the-type) (eq (first the-type) 'VALUES))
	   (cmpwarn "Ignoring THE form with type ~A" the-type))
	  ((not (setf type (type-and the-type (c1form-primary-type form))))
	   (cmpwarn "Type mismatch was found in ~s." (cons 'THE args)))
	  (t
	   (setf (c1form-type form) type)))
    form))

(defun c1compiler-let (args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'COMPILER-LET 1 0))
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (setq args (progv symbols values (c1progn (cdr args))))
  (make-c1form 'COMPILER-LET args symbols values args))

(defun c2compiler-let (symbols values body)
  (progv symbols values (c2expr body)))

(defun c1function (args &aux fd)
  (check-args-number 'FUNCTION args 1 1)
  (let ((fun (car args)))
    (cond ((si::valid-function-name-p fun)
	   (let ((funob (local-function-ref fun t)))
	     (if funob
		 (let* ((vars (list (fun-var funob))))
		   (incf (var-ref (fun-var funob)))
		   (make-c1form* 'VAR :referred-vars vars
				 :local-referred vars
				 :args (first vars)))
		 (make-c1form* 'FUNCTION
			       :sp-change (not (and (symbolp fun)
						    (get-sysprop fun 'NO-SP-CHANGE)))
			       :args 'GLOBAL nil fun))))
          ((and (consp fun) (member (car fun) '(LAMBDA EXT::LAMBDA-BLOCK)))
           (cmpck (endp (cdr fun))
                  "The lambda expression ~s is illegal." fun)
           (let* ((name (and (eq (first fun) 'EXT::LAMBDA-BLOCK)
			    (first (setf fun (rest fun)))))
		  (fun (c1compile-function (rest fun) :name name :global nil))
		  (lambda-form (fun-lambda fun)))
	     (make-c1form 'FUNCTION lambda-form 'CLOSURE lambda-form fun)))
	  (t (cmperr "The function ~s is illegal." fun)))))

(defun c2function (kind funob fun)
  (case kind
    (GLOBAL
     (unwind-exit (list 'FDEFINITION fun)))
    (CLOSURE
     (new-local 0 fun)	; 0 was *level*
     (unwind-exit `(MAKE-CCLOSURE ,fun)))))

;;; Mechanism for sharing code.
(defun new-local (level fun)
  ;; returns the previous function or NIL.
  (declare (type fun fun))
  (let ((previous (dolist (old *local-funs*)
		    (when (and (= *env* (fun-env old))
			       ;; closures must be embedded in env of
			       ;; same size
			       (similar (fun-lambda fun) (fun-lambda old)))
		      (return old)))))
    (if previous
	(progn
          (if (eq (fun-closure fun) 'CLOSURE)
	      (cmpnote "Sharing code for closure")
	      (cmpnote "Sharing code for local function ~A" (fun-name fun)))
	  (setf (fun-cfun fun) (fun-cfun previous)
		(fun-lambda fun) nil)
	  previous)
        (progn
          (setf (fun-level fun) (if (fun-ref-ccb fun) 0 level)
                (fun-env fun) *env*
		*local-funs* (cons fun *local-funs*))
	  NIL))))

(defun wt-fdefinition (fun-name)
  (let ((vv (add-object fun-name)))
    (if (and (symbolp fun-name)
	     (or (not (safe-compile))
		 (and (eql (symbol-package fun-name) (find-package "CL"))
		      (fboundp fun-name) (functionp (fdefinition fun-name)))))
	(wt "(" vv "->symbol.gfdef)")
	(wt "ecl_fdefinition(" vv ")"))))

(defun wt-make-closure (fun &aux (cfun (fun-cfun fun)))
  (declare (type fun fun))
  (let* ((closure (fun-closure fun))
	 (minarg (fun-minarg fun))
	 (maxarg (fun-maxarg fun))
	 (narg (if (= minarg maxarg) maxarg nil)))
    (cond ((eq closure 'CLOSURE)
	   (wt "cl_make_cclosure_va((void*)" cfun ",env" *env-lvl* ",Cblock)"))
	  ((eq closure 'LEXICAL)
	   (baboon))
	  (narg ; empty environment fixed number of args
	   (wt "cl_make_cfun((void*)" cfun ",Cnil,Cblock," narg ")"))
	  (t ; empty environment variable number of args
	   (wt "cl_make_cfun_va((void*)" cfun ",Cnil,Cblock)")))))


;;; ----------------------------------------------------------------------

(put-sysprop 'quote 'c1special 'c1quote)
(put-sysprop 'function 'c1special 'c1function)
(put-sysprop 'function 'c2 'c2function)
(put-sysprop 'the 'c1special 'c1the)
(put-sysprop 'eval-when 'c1special 'c1eval-when)
(put-sysprop 'declare 'c1special 'c1declare)
(put-sysprop 'compiler-let 'c1special 'c1compiler-let)
(put-sysprop 'compiler-let 'c2 'c2compiler-let)

(put-sysprop 'fdefinition 'wt-loc 'wt-fdefinition)
(put-sysprop 'make-cclosure 'wt-loc 'wt-make-closure)
