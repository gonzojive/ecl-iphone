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
  (let ((*funs* *funs*)
	(old-funs *funs*)
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
      (let ((fun (make-fun :name (car def))))
	(push fun *funs*)
	(push (cons fun (cdr def)) defs)))

    ;; Now we can compile the body and the function themselves. Notice
    ;; that, whe compiling FLET, we must empty *fun* so that the functions
    ;; do not see each other.
    (multiple-value-bind (body ss ts is other-decl)
	(c1body (rest args) t)
      (let ((*vars* *vars*))
	(c1add-globals ss)
	(check-vdecl nil ts is)
	(setq body-c1form (c1decl-body other-decl body))))

    (when (eq origin 'FLET)
      (setf *funs* old-funs))
    (dolist (def (nreverse defs))
      (let ((fun (first def)))
	(push (c1compile-function (rest def) :fun fun
				  :CB/LB (if (fun-ref-ccb fun) 'CB 'LB))
	      local-funs)))

    ;; Keep only functions that have been referenced at least once.
    ;; It is not possible to look at FUN-REF before because functions
    ;; in a LABELS can reference each other.
    (setf local-funs (remove-if-not #'plusp local-funs :key #'fun-ref))

    (if local-funs
	(make-c1form* 'LOCALS :type (c1form-type body-c1form)
		      :local-vars (remove nil (mapcar #'fun-var local-funs))
		      :args local-funs body-c1form (eq origin 'LABELS))
	body-c1form)))

(defun compute-fun-closure-type (fun)
  (labels
      ((closure-type (fun &aux (lambda-form (fun-lambda fun)))
	 (let ((vars (fun-referred-vars fun))
	       (funs (remove fun (fun-referred-funs fun) :test #'child-p)))
	   ;; it will have a full closure if it refers external variables
	   (unless (or vars funs)
	     (return-from closure-type nil))
	   (dolist (var vars)
	     ;; ...across CB
	     (when (ref-ref-ccb var)
	       (return-from closure-type 'CLOSURE)))
	   ;; ...or the function itself is referred across CB
	   (when (fun-ref-ccb fun)
	     (return-from closure-type 'CLOSURE))
	   ;; or if it directly calls a function
	   (dolist (f funs 'LEXICAL)
	     ;; .. which has a full closure
	     (when (and (not (child-p f fun))
			(eq (fun-closure fun) 'CLOSURE))
	       (return 'CLOSURE)))))
       (child-p (presumed-parent fun)
	 (let ((real-parent (fun-parent fun)))
	   (when real-parent
	     (or (eq real-parent presumed-parent)
		 (child-p real-parent presumed-parent))))))
    ;; This recursive algorithm is guaranteed to stop when functions
    ;; do not change.
    (let ((new-type (closure-type fun))
	  (old-type (fun-closure fun)))
      ;; Same type
      (when (eq new-type old-type)
	(return-from compute-fun-closure-type nil))
      ;; {lexical,closure} -> no closure!
      ;; closure -> {lexical, no closure}
      (when (or (and (not new-type) old-type)
		(eq old-type 'CLOSURE))
	(baboon))
      (setf (fun-closure fun) new-type)
      (when (eq new-type 'CLOSURE)
	(dolist (var (fun-referred-vars fun))
	  (unless (or (ref-ref-ccb var)
		      (member (var-kind var) '(GLOBAL SPECIAL REPLACED CLOSURE)))
	    (setf (var-ref-clb var) nil
		  (var-ref-ccb var) t
		  (var-kind var) 'CLOSURE
		  (var-loc var) 'OBJECT))))
      (dolist (f (fun-child-funs fun))
	(compute-fun-closure-type f))
      t)))

(defun c2locals (funs body labels ;; labels is T when deriving from labels
		      &aux block-p
		      (level *level*)
		      (*env* *env*)
		      (*env-lvl* *env-lvl*) env-grows)
  ;; create location for each function which is returned,
  ;; either in lexical:
  (dolist (fun funs)
    (let* ((var (fun-var fun)))
      (when (and var (plusp (var-ref var))) ; the function is returned
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
      (when (and var (plusp (var-ref var)))
	(when labels
	  (incf (fun-env fun)))		; var is included in the closure env
	(bind nil var))))
  ;; - then assign to it
  (dolist (fun funs)
    (let* ((var (fun-var fun)))
      (when (and var (plusp (var-ref var)))
	(set-var (list 'MAKE-CCLOSURE fun) var))))
  ;; We need to introduce a new lex vector when lexical variables
  ;; are present in body and it is the outermost FLET or LABELS
  ;; (nested FLETS/LABELS can use a single lex).
  (when (plusp *lex*)
    (incf level))
  ;; create the functions:
  (dolist (fun funs)
    (let* ((previous (new-local level fun)))
      (when previous
	(format t "~%> ~A" previous)
	(setf (fun-level fun) (fun-level previous)
	      (fun-env fun) (fun-env previous)))))

  (c2expr body)
  (when block-p (wt-nl "}")))

(defun c1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (c1add-globals ss)
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
    (cond ((eq fun 'CB) (setq ccb t))
	  ((eq fun 'LB) (setq clb t))
	  ((and (consp fun) (equal fname (first fun))) ; macro
	   (when build-object
	     (cmperr "The name of a macro ~A was found in a call to FUNCTION."
		     fname))
	   (return nil))
          ((and (fun-p fun) (same-fname-p (fun-name fun) fname))
	   (incf (fun-ref fun))
	   (if build-object
	       (setf (fun-ref-ccb fun) t)
	       (push fun (fun-referred-funs *current-function*)))
	   ;; we introduce a variable to hold the funob
	   (let ((var (or (fun-var fun)
			  (setf (fun-var fun)
				(make-var :name fname :kind :OBJECT)))))
	     (cond (ccb (setf (var-ref-ccb var) t
			      (var-kind var) 'CLOSURE)
			(setf (fun-ref-ccb fun) t))
		   (clb (setf (var-ref-clb var) t
			      (var-kind var) 'LEXICAL))))
	   (return fun)))))

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
  (multiple-value-bind (*unwind-exit* args narg)
      (maybe-push-args args)
    (when narg
      (c2call-local fun args narg)
      (wt-nl "}")
      (return-from c2call-local)))
  (unless (c2try-tail-recursive-call fun args)
    (let ((*inline-blocks* 0))
      (unwind-exit
       (if (eq args 'ARGS-PUSHED)
	   (list 'CALL-ARGS-PUSHED fun narg)
	   (list 'CALL-NORMAL fun (coerce-locs (inline-args args)))))
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
