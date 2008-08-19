;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPOPT. Optimization of library functions

;;;;  Copyright (c) 2008. Juan Jose Garcia-Ripol
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;;;
;;; TYPEP
;;;
;;; Some of the type checks can be expanded inline if we know the name
;;; of the type and it corresponds to either a Common-Lisp base type
;;; or to some class.
;;;

(defun expand-in-interval-p (var interval)
  (declare (si::c-local))
  (let ((forms '()))
    (destructuring-bind (&optional (lower-limit '*) (upper-limit '*))
	interval
      (unless (eq lower-limit '*)
	(push (if (consp lower-limit)
		  `(> ,var ,(first lower-limit))
		  `(>= ,var ,lower-limit))
	      forms))
      (unless (eq upper-limit '*)
	(push (if (consp upper-limit)
		  `(< ,var ,(first upper-limit))
		  `(<= ,var ,upper-limit))
	      forms)))
    forms))

(defun expand-typep (form object type env)
  (declare (si::c-local))
  ;; This function is reponsible for expanding (TYPEP object type)
  ;; forms into a reasonable set of system calls. When it fails to
  ;; match the compiler constraints on speed and space, it simply
  ;; returns the original form. Note that for successful recursion we
  ;; have to output indeed the ORIGINAL FORM, not some intermediate
  ;; step. Otherwise the compiler macro will enter an infinite loop.
  (let* ((space (cmp-env-optimization 'space env))
	 (speed (cmp-env-optimization 'speed env))
	 (safety (cmp-env-optimization 'safety env))
	 (orig-type type)
	 aux function
	 first rest)
    (declare (si::fixnum space speed))
    (cond ((not (and (constantp type) (setf type (cmp-eval type)) t))
	   form)
	  ;; Type is not known
	  ((not (known-type-p type))
	   form)
	  ;; Simple ones
	  ((subtypep 'T type) T)
	  ((eq type 'NIL) NIL)
	  ((eq aux 'SATISFIES)
	   `(funcall #',function ,object))
	  ;;
	  ;; Detect inconsistencies in the provided type. If we run at low
	  ;; safety, we will simply assume the user knows what she's doing.
	  ((subtypep type NIL)
	   (cmpwarn "TYPEP form contains an empty type ~S and cannot be optimized" type)
	   (if (< safety 1)
	       NIL
	       form))
	  ;;
	  ;; There exists a function which checks for this type?
	  ((setf function (get-sysprop type 'si::type-predicate))
	   `(,function ,object))
	  ;;
	  ;; Similar as before, but we assume the user did not give us
	  ;; the right name, or gave us an equivalent type.
	  ((loop for (a-type . function-name) in si::+known-typep-predicates+
	      when (si::type= type a-type)
	      do (return `(,function-name ,object))))
	  ;;
	  ;; The following are not real functions, but are expanded by the
	  ;; compiler into C forms.
	  ((setf function (assoc type '((SINGLE-FLOAT . SINGLE-FLOAT-P)
					(SHORT-FLOAT . SHORT-FLOAT-P)
					(DOUBLE-FLOAT . DOUBLE-FLOAT-P)
					(LONG-FLOAT . LONG-FLOAT-P))))
	   `(,(cdr function) ,object))
	  ;;
	  ;; Complex types defined with DEFTYPE.
	  ((and (atom type)
		(get-sysprop type 'SI::DEFTYPE-DEFINITION)
		(setq function (get-sysprop type 'SI::DEFTYPE-DEFINITION)))
	   (expand-typep form object `',(funcall function) env))
	  ;;
	  ;; No optimizations that take up too much space unless requested.
	  ((and (>= space 2) (> space speed))
	   form)
	  ;;
	  ;; CONS types. They must be checked _before_ sequence types. We
	  ;; do not produce optimized forms because they can be recursive.
	  ((and (consp type) (eq (first type) 'CONS))
	   form)
	  ;;
	  ;; The type denotes a known class and we can check it
	  #+clos
	  ((setf aux (find-class type nil))
	   `(si::of-class-p ,object ',type))
	  ;;
	  ;; There are no other atomic types to optimize
	  ((atom type)
	   form)
	  ;;
	  ;; Complex types with arguments.
	  ((setf rest (rest type)
		 first (first type)
		 function (get-sysprop first 'SI::DEFTYPE-DEFINITION))
	   (expand-typep form object `',(apply function rest) env))
	  ;;
	  ;; (TYPEP o '(NOT t)) => (NOT (TYPEP o 't))
	  ((eq first 'NOT)
	   `(not (typep ,object ',(first rest))))
	  ;;
	  ;; (TYPEP o '(AND t1 t2 ...)) => (AND (TYPEP o 't1) (TYPEP o 't2) ...)
	  ;; (TYPEP o '(OR t1 t2 ...)) => (OR (TYPEP o 't1) (TYPEP o 't2) ...)
	  ((member first '(OR AND))
	   (let ((var (gensym)))
	     `(let ((,var ,object))
		(,first ,@(loop for type in rest
			   collect `(typep ,var ',type))))))
	  ;;
	  ;; (TYPEP o '(MEMBER a1 a2 ...)) => (MEMBER o '(a1 a2 ...))
	  ((eq first 'MEMBER)
	   `(MEMBER ,object ',rest))
	  ;;
	  ;; (INTEGER * *), etc
	  ((member first '(INTEGER RATIONAL FLOAT REAL SINGLE-FLOAT
			   DOUBLE-FLOAT #+long-float LONG-FLOAT
			   #+short-float SHORT-FLOAT))
	   (let ((var (gensym)))
	     ;; Small optimization: it is easier to check for fixnum
	     ;; than for integer. Use it when possible.
	     (when (and (eq first 'integer)
			(subtypep type 'fixnum))
	       (setf first 'fixnum))
	     `(LET ((,var ,object))
		(AND (TYPEP ,var ',first)
		     ,@(expand-in-interval-p `(the ,first ,var) rest)))))
	  (t
	   form))))

(define-compiler-macro typep (&whole form object type &environment env)
  (expand-typep form object type env))

;;;
;;; DOLIST
;;;
;;; We overwrite the original macros introducing type declarations and
;;; other possible type checks.
;;;

(defmacro dolist ((var expression &optional output-form) &body body &environment env)
  (multiple-value-bind (declarations body)
      (si:process-declarations body nil)
    (let* ((list-var (gensym))
	   (typed-var (if (policy-check-all-arguments-p env)
			  list-var
			  `(the cons ,list-var))))
      `(block nil
	 (let* ((,list-var ,expression)
		,var)
	   (declare ,@declarations)
	   (si::while ,list-var
	      (setq ,var (first ,typed-var))
	      ,@body
	      (setq ,list-var (rest ,typed-var)))
	   ,(when output-form `(setq ,var nil))
	   ,output-form)))))

;;;
;;; COERCE
;;;
;;; Simple coercion rules are implemented using the following
;;; templates.  X is replaced by the coerced value, which can be a
;;; lisp form. We use a LET form to avoid evaluating twice the same
;;; form.
;;;
(defparameter +coercion-table+
  '((integer . (let ((y x)) (check-type y integer) x))
    (float . (float x))
    (short-float  . (float x 0.0s0))
    (single-float . (float x 0.0f0))
    (double-float . (float x 0.0d0))
    (long-float . (float x 0.0l0))
    (base-char . (character x))
    (character . (character x))
    (function . (si::coerce-to-function x))
    (complex .
     (let ((y x))
       (declare (:read-only y))
       (complex (realpart y) (imagpart y))))
    ))

(defun expand-coerce (form value type env)
  (declare (si::c-local))
  ;; This function is reponsible for expanding (TYPEP object type)
  ;; forms into a reasonable set of system calls. When it fails to
  ;; match the compiler constraints on speed and space, it simply
  ;; returns the original form. Note that for successful recursion we
  ;; have to output indeed the ORIGINAL FORM, not some intermediate
  ;; step. Otherwise the compiler macro will enter an infinite loop.
  (let* ((space (cmp-env-optimization 'space env))
	 (speed (cmp-env-optimization 'speed env))
	 (safety (cmp-env-optimization 'safety env))
	 (orig-type type)
	 first rest)
    (cond ((not (and (constantp type) (setf type (cmp-eval type))))
	   form)
	  ;;
	  ;; Trivial case
	  ((subtypep 't type)
	   value)
	  ;;
	  ;; Detect inconsistencies in the type form.
	  ((subtypep type 'nil)
	   (cmperror "Cannot COERCE an expression to an empty type."))
	  ;;
	  ;; No optimizations that take up too much space unless requested.
	  ((and (>= space 2) (> space speed))
	   form)
	  ;;
	  ;; Search for a simple template above, replacing X by the value.
	  ((loop for (a-type . template) in +coercion-table+
	      when (eq type a-type)
	      do (return (subst value 'x template))))
	  ;;
	  ;; Complex types defined with DEFTYPE.
	  ((and (atom type)
		(get-sysprop type 'SI::DEFTYPE-DEFINITION)
		(setq function (get-sysprop type 'SI::DEFTYPE-DEFINITION)))
	   (expand-coerce form value `',(funcall function) env))
	  ;;
	  ;; CONS types are not coercible.
	  ((and (consp type)
		(eq (first type) 'CONS))
	   form)
	  ;;
	  ;; Search for a simple template above, but now assuming the user
	  ;; provided a more complex form of the same value.
	  ((loop for (a-type . template) in +coercion-table+
	      when (si::type= type a-type)
	      do (return (subst value 'x template))))
	  ;;
	  ;; SEQUENCE types
	  ((subtypep type 'sequence)
	   (multiple-value-bind (elt-type length)
	       (si::closest-sequence-type type)
	     (if (eq elt-type 'list)
		 `(si::coerce-to-list ,value)
		 `(si::coerce-to-vector ,value ',elt-type ',length))))
	  ;;
	  ;; There are no other atomic types to optimize
	  ((atom type)
	   form)
	  ;;
	  ;; (TYPEP o '(AND t1 t2 ...)) => (AND (TYPEP o 't1) (TYPEP o 't2) ...)
	  ((progn
	     (setf rest (rest type) first (first type))
	     (eq first 'AND))
	   `(let ((x ,value))
	      ,@(loop for i in rest
		   collect `(setf x (coerce x ',i)))
	      x))
	  ;;
	  ;; (COMPLEX whatever) types
	  ((and (eq first 'complex)
		(= (length rest) 1))
	   `(let ((y ,value))
	      (declare (:read-only y))
	      (complex (coerce (realpart y) ',(first rest))
		       (coerce (imagpart y) ',(first rest)))))
	  ;;
	  ;; (INTEGER * *), etc We have to signal an error if the type
	  ;; does not match. However, if safety settings are low, we
	  ;; skip the interval test.
	  ((member first '(INTEGER RATIONAL FLOAT REAL SINGLE-FLOAT
			   DOUBLE-FLOAT #+long-float LONG-FLOAT
			   #+short-float SHORT-FLOAT))
	   (let ((unchecked (expand-coerce form value `',first env)))
	     (if (< safety 1)
		 unchecked
		 `(let ((x ,unchecked))
		    (declare (,first x))
		    (unless (and ,@(expand-in-interval-p 'x (rest type)))
		      (si::do-check-type x ',type nil "coerced value"))
		    x))))
	  ;;
	  ;; We did not find a suitable expansion.
	  (t
	   form)
	  )))

(define-compiler-macro coerce (&whole form value type &environment env)
  (expand-coerce form value type env))
