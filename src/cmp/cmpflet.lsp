;;;;  CMPFLET  Flet, Labels, and Macrolet.

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

(defun c1labels (args) (c1labels/flet 'LABELS args))

(defun c1flet (args) (c1labels/flet 'FLET args))

(defun c1labels/flet (origin args)
  (check-args-number origin args 1)
  (let ((new-funs *funs*)
	(defs '())
	(local-funs '())
	(fnames '())
	body-c1form)
    ;; On a first round, we extract the definitions of the functions,
    ;; and build empty function objects that record the references to
    ;; this functions in the processed body. In the end
    ;;	DEFS = ( { ( fun-object  function-body ) }* ).
    (dolist (def (car args))
      (cmpck (or (endp def)
		 (not (si::valid-function-name-p (car def)))
		 (endp (cdr def)))
	     "The local function definition ~s is illegal." def)
      (cmpck (member (car def) fnames)
	     "The function ~s was already defined." (car def))
      (push (car def) fnames)
      (let* ((name (car def))
	     (var (make-var :name name :kind :object))
	     (fun (make-fun :name name :var var)))
	(push (list (fun-name fun) 'FUNCTION fun) new-funs)
	(push (cons fun (cdr def)) defs)))

    ;; Now we compile the functions, either in an empty environment
    ;; in which there are no new functions
    (let ((*funs* (if (eq origin 'FLET) *funs* new-funs)))
      (dolist (def (nreverse defs))
	(let ((fun (first def)))
	  ;; The closure type will be fixed later on by COMPUTE-...
	  (push (c1compile-function (rest def) :fun fun :CB/LB 'LB)
		local-funs))))

    ;; When we are in a LABELs form, we have to propagate the external
    ;; variables from one function to the other functions that use it.
    (dolist (f1 local-funs)
      (let ((vars (fun-referred-vars f1)))
	(dolist (f2 local-funs)
	  (when (and (not (eq f1 f2))
		     (member f1 (fun-referred-funs f2)))
	    (add-referred-variables-to-function f2 vars)))))

    ;; Now we can compile the body itself.
    (let ((*vars* *vars*)
          (*funs* new-funs))
      (multiple-value-bind (body ss ts is other-decl)
	  (c1body (rest args) t)
	(c1declare-specials ss)
	(check-vdecl nil ts is)
	(setq body-c1form (c1decl-body other-decl body))))

    ;; Keep only functions that have been referenced at least once.
    ;; It is not possible to look at FUN-REF before because functions
    ;; in a LABELS can reference each other.
    (setf local-funs (remove-if-not #'plusp local-funs :key #'fun-ref))

    (if local-funs
	(make-c1form* 'LOCALS :type (c1form-type body-c1form)
		      :args local-funs body-c1form (eq origin 'LABELS))
	body-c1form)))

(defun fun-referred-local-vars (fun)
  (remove-if #'(lambda (v) (member (var-kind v) '(SPECIAL GLOBAL REPLACED)))
	     (fun-referred-vars fun)))

(defun compute-fun-closure-type (fun)
  (labels
      ((closure-type (fun &aux (lambda-form (fun-lambda fun)))
	 (let ((vars (fun-referred-local-vars fun))
	       (funs (remove fun (fun-referred-funs fun) :test #'child-p))
	       (closure nil))
	   ;; it will have a full closure if it refers external non-global variables
	   (dolist (var vars)
	     ;; ...across CB
	     (if (ref-ref-ccb var)
		 (setf closure 'CLOSURE)
		 (unless closure (setf closure 'LEXICAL))))
	   ;; ...or if it directly calls a function
	   (dolist (f funs)
	     ;; .. which has a full closure
	     (when (not (child-p f fun))
	       (case (fun-closure fun)
		 (CLOSURE (setf closure 'CLOSURE))
		 (LEXICAL (unless closure (setf closure 'LEXICAL))))))
	   ;; ...or the function itself is referred across CB
	   (when closure
	     (when (or (fun-ref-ccb fun)
		       (and (fun-var fun)
			    (plusp (var-ref (fun-var fun)))))
	       (setf closure 'CLOSURE)))
	   closure))
       (child-p (presumed-parent fun)
	 (let ((real-parent (fun-parent fun)))
	   (when real-parent
	     (or (eq real-parent presumed-parent)
		 (child-p real-parent presumed-parent))))))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (closure-type fun))
	  (old-type (fun-closure fun)))
;;       (format t "~%CLOSURE-TYPE: ~A ~A -> ~A, ~A" (fun-name fun)
;;       	      old-type new-type (fun-parent fun))
;;       (print (fun-referred-vars fun))
      ;; Same type
      (when (eq new-type old-type)
	(return-from compute-fun-closure-type nil))
      ;; {lexical,closure} -> no closure!
      ;; closure -> {lexical, no closure}
      (when (or (and (not new-type) old-type)
		(eq old-type 'CLOSURE))
	(baboon))
      (setf (fun-closure fun) new-type)
      ;; All external, non-global variables become of type closure
      (when (eq new-type 'CLOSURE)
	(dolist (var (fun-referred-local-vars fun))
	  (setf (var-ref-clb var) nil
		(var-ref-ccb var) t
		(var-kind var) 'CLOSURE
		(var-loc var) 'OBJECT))
	(dolist (f (fun-referred-funs fun))
	  (setf (fun-ref-ccb f) t)))
      ;; If the status of some of the children changes, we have
      ;; to recompute the closure type.
      (do ((finish nil t)
	   (recompute nil))
	(finish
	 (when recompute (compute-fun-closure-type fun)))
	(dolist (f (fun-child-funs fun))
	  (when (compute-fun-closure-type f)
	    (setf recompute t finish nil))))
      t)))

(defun c2locals (funs body labels ;; labels is T when deriving from labels
		      &aux block-p
		      (*env* *env*)
		      (*env-lvl* *env-lvl*) env-grows)
  ;; create location for each function which is returned,
  ;; either in lexical:
  (dolist (fun funs)
    (let* ((var (fun-var fun)))
      (when (plusp (var-ref var)) ; the function is returned
        (unless (member (var-kind var) '(LEXICAL CLOSURE))
          (setf (var-loc var) (next-lcl))
          (unless block-p
            (setq block-p t) (wt-nl "{ "))
          (wt "cl_object " var ";"))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))))
  ;; or in closure environment:
  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "volatile cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))
  ;; bind such locations:
  ;; - first create binding (because of possible circularities)
  (dolist (fun funs)
    (let* ((var (fun-var fun)))
      (when (plusp (var-ref var))
	(bind nil var))))
  ;; create the functions:
  (mapc #'new-local funs)
  ;; - then assign to it
  (dolist (fun funs)
    (let* ((var (fun-var fun)))
      (when (plusp (var-ref var))
	(set-var (list 'MAKE-CCLOSURE fun) var))))

  (c2expr body)
  (when block-p (wt-nl "}")))

(defun c1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (c1declare-specials ss)
    (check-vdecl nil ts is)
    (c1decl-body other-decl body)))

(defun c1macrolet (args &aux (*funs* *funs*))
  (check-args-number 'MACROLET args 1)
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The macro definition ~s is illegal." def)
    (push (list (car def)
		'MACRO
		(si::make-lambda (car def)
				 (cdr (sys::expand-defmacro (car def) (second def) (cddr def)))))
          *funs*))
  (c1locally (cdr args)))

(defun c1symbol-macrolet (args &aux (*vars* *vars*))
  (check-args-number 'SYMBOL-MACROLET args 1)
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The symbol-macro definition ~s is illegal." def)
    (push def *vars*))
  (c1locally (cdr args)))

(defun local-function-ref (fname &optional build-object &aux (ccb nil) (clb nil))
  (dolist (fun *funs*)
    (cond ((eq fun 'CB)
           (setq ccb t))
	  ((eq fun 'LB)
           (setq clb t))
	  ((and (consp fun)
                (equal fname (first fun))
                (eq (second fun) 'MACRO))
           ;; a macro
	   (when build-object
	     (cmperr "The name of a macro ~A was found in a call to FUNCTION."
		     fname))
	   (return nil))
          ((and (consp fun)
                (same-fname-p (first fun) fname)
                (eq (second fun) 'FUNCTION))
            ;; it is a function definition -- extract the actual function record
            (let ((fun (third fun)))
	      (incf (fun-ref fun))
	      (cond (build-object
		     (setf (fun-ref-ccb fun) t))
		    (*current-function*
		     (push fun (fun-referred-funs *current-function*))))
	      ;; we introduce a variable to hold the funob
	      (let ((var (fun-var fun)))
	        (cond (ccb (when build-object
			     (setf (var-ref-ccb var) t
				   (var-kind var) 'CLOSURE))
			   (setf (fun-ref-ccb fun) t))
		      (clb (when build-object 
			     (setf (var-ref-clb var) t
				   (var-kind var) 'LEXICAL)))))
	       (return fun))))))

(defun sch-local-fun (fname)
  ;; Returns fun-ob for the local function (not locat macro) named FNAME,
  ;; if any.  Otherwise, returns FNAME itself.
  (dolist (fun *funs* fname)
    (when (and (not (eq fun 'CB))
               (not (consp fun))
               (same-fname-p (fun-name fun) fname))
          (return fun))))

(defun sch-local-macro (fname)
  (dolist (fun *funs*)
    (when (and (consp fun)
               (eq (first fun) fname))
          (return (third fun)))))

(defun c2call-local (fun args &optional narg)
  (declare (type fun fun))
  (unless (c2try-tail-recursive-call fun args)
    (let ((*inline-blocks* 0))
      (unwind-exit (list 'CALL-NORMAL fun (coerce-locs (inline-args args))))
      (close-inline-blocks))))

;;; ----------------------------------------------------------------------

(put-sysprop 'FLET 'C1SPECIAL 'c1flet)
(put-sysprop 'LABELS 'C1SPECIAL 'c1labels)
(put-sysprop 'LOCALLY 'C1SPECIAL 'c1locally)
(put-sysprop 'MACROLET 'C1SPECIAL 'c1macrolet)
(put-sysprop 'SYMBOL-MACROLET 'C1SPECIAL 'c1symbol-macrolet)

(put-sysprop 'LOCALS 'c2 'c2locals)	; replaces both c2flet and c2lables
;;; c2macrolet is not defined, because MACROLET is replaced by PROGN
;;; during Pass 1.
(put-sysprop 'CALL-LOCAL 'C2 'c2call-local)
