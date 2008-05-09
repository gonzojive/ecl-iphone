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
	 aux function
	 first rest)
    (declare (si::fixnum space speed))
    (cond ((not (and (constantp type) (setf type (cmp-eval type)) t))
	   form)
	  ;; Simple ones
	  ((eq type 'T) T)
	  ((eq type 'NIL) NIL)
	  ((eq aux 'SATISFIES)
	   `(funcall #',function ,object))
	  ;;
	  ;; There exists a function which checks for this type?
	  ((setf function (get-sysprop type 'si::type-predicate))
	   `(,function ,object))
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
	   (expand-typep form object (apply function rest) env))
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
