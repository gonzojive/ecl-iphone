;;;;  CMPBIND  Variable Binding.
;;;;
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;  This file is part of ECoLisp, herein referred to as ECL.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it under
;;;;    the terms of the GNU LIBRARY GENERAL PUBLIC LICENSE as published by
;;;;    the Free Software Foundation; either version 2 of the License, or
;;;;    (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;; bind must be called for each variable in a lambda or let, once the value
;;; to be bound has been placed in loc.
;;; bind takes care of setting var-loc.

(defun bind (loc var)
  ;; loc can be either (LCL n), 'VA-ARGS, (KEYVARS n), (CAR n),
  ;; a constant, or (VAR var) from a let binding. ; ccb
  (declare (type var var))
  (case (var-kind var)
    (CLOSURE
     (let ((var-loc (var-loc var)))
       (unless (sys:fixnump var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-env))
	 (setf (var-loc var) var-loc))
       (when (zerop var-loc) (wt-nl "env" *env-lvl* " = Cnil;"))
       (wt-nl "CLV" var-loc "=&CAR(env" *env-lvl* "=CONS(")
       (wt-coerce-loc :object loc)
       (wt ",env" *env-lvl* "));")
       (wt-comment (var-name var))))
    (LEXICAL
     (let ((var-loc (var-loc var)))
       (unless (consp var-loc)
	 ;; first binding: assign location
	 (setq var-loc (next-lex))
	 (setf (var-loc var) var-loc))
       (wt-nl) (wt-lex var-loc) (wt "= ")
       (wt-coerce-loc :object loc)
       (wt ";"))
       (wt-comment (var-name var)))
    (SPECIAL
     (bds-bind loc var))
    (t
     (cond ((not (eq (var-loc var) 'OBJECT))
	    ;; already has location (e.g. optional in lambda list)
	    ;; check they are not the same
	    (unless (equal (var-loc var) loc)
	      (wt-nl var "= ")
	      (wt-coerce-loc (var-rep-type var) loc)
	      (wt ";")))
	   ((and (consp loc) (eql (car loc) 'LCL))
	    ;; set location for lambda list requireds
	    (setf (var-loc var) loc))
	   (t
	    (error)))
	 )))

;;; Used by let*, defmacro and lambda's &aux, &optional, &rest, &keyword
(defun bind-init (var form)
  (let ((*destination* `(BIND ,var)))
    ;; assigning location must be done before calling c2expr*,
    ;; otherwise the increment to *env* or *lex* is done during
    ;; unwind-exit and will be shadowed by functions (like c2let)
    ;; which rebind *env* or *lex*.
    (case (var-kind var)
      (CLOSURE
       (unless (si:fixnump (var-loc var))
	 (setf (var-loc var) (next-env))))
      (LEXICAL
       (unless (consp (var-loc var))
	 (setf (var-loc var) (next-lex))))
      (SPECIAL
       ;; prevent BIND from pushing BDS-BIND
       (setf (var-bds-bound var) t)))
    (c2expr* form)
    (when (eq (var-kind var) 'SPECIAL)
      ;; now the binding is in effect
      (push 'BDS-BIND *unwind-exit*))))

(defun bds-bind (loc var)
  ;; Optimize the case (let ((*special-var* *special-var*)) ...)
  (cond ((and (var-p loc)
	      (eq (var-kind loc) 'global)
	      (eq (var-name loc) (var-name var)))
	 (wt-nl "bds_push(" (var-loc var) ");"))
	(t
	 (wt-nl "bds_bind(" (var-loc var) ",")
	 (wt-coerce-loc :object loc)
	 (wt ");")))
  ;; push BDS-BIND only once:
  ;; bds-bind may be called several times on the same variable, e.g.
  ;; an optional has two alternative bindings.
  ;; We use field var-bds-bound to record this fact.
  (unless (var-bds-bound var)
    (push 'BDS-BIND *unwind-exit*)
    (setf (var-bds-bound var) t))
  (wt-comment (var-name var)))

(put-sysprop 'BIND 'SET-LOC 'bind)
