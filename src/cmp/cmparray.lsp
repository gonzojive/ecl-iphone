;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPARRAY. Optimizations related to arrays

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
;;; MAKE-ARRAY
;;;

(define-compiler-macro make-array (&whole form dimensions &key (element-type t)
					  (initial-element nil initial-element-supplied-p)
					  (initial-contents nil initial-contents-supplied-p)
					  adjustable fill-pointer
					  displaced-to (displaced-index-offset 0)
					  &environment env)
  ;; This optimization is always done unless we provide content. There
  ;; is no speed, debug or space reason not to do it, unless the user
  ;; specifies not to inline MAKE-ARRAY, but in that case the compiler
  ;; macro should not be used.
  (unless (or initial-element-supplied-p
	      initial-contents-supplied-p)
    ;; If the type is known and we can assume it will not change, we
    ;; replace it with the upgraded form.
    (when (and (constantp element-type env)
	       (policy-assume-types-dont-change-p env))
      (let ((new-type (cmp-eval element-type)))
	(when (known-type-p new-type)
	  (setf element-type `',(upgraded-array-element-type new-type)))))
    ;; Finally, we choose between making a vector or making a general array.
    ;; It only saves some time, since MAKE-PURE-ARRAY will call MAKE-VECTOR
    ;; if a one-dimensional array is to be created.
    (let ((function 'si::make-pure-array))
      (when (constantp dimensions env)
	(let ((d (cmp-eval dimensions)))
	  (when (or (integerp d) (and (listp d) (= (length d) 1) (setf d (first d))))
	    (setf function 'si::make-vector
		  dimensions `',d)))
	(setf form
	      `(,function ,element-type ,dimensions ,adjustable ,fill-pointer
			  ,displaced-to ,displaced-index-offset)))))
  form)

;;;
;;; VECTOR-PUSH and VECTOR-PUSH-EXTEND
;;;

(defun expand-vector-push (whole env)
  (declare (si::c-local))
  (let* ((extend (eq (first whole) 'vector-push-extend))
	 (args (rest whole)))
    (unless (or ;; Avoid infinite recursion
		(eq (first args) '.val)
		(safe-compile)
		(>= (cmp-env-optimization 'space env) 2))
      (setf whole
	    `(let* ((.val ,(car args))
		    (.vec ,(second args))
		    (.i (fill-pointer .vec))
		    (.dim (array-total-size .vec)))
	       (declare (fixnum .i .dim)
			(:read-only .vec .val .i .dim))
	       (cond ((< .i .dim)
		      (sys::fill-pointer-set .vec (the fixnum (+ 1 .i)))
		      (sys::aset .val .vec .i)
		      .i)
		     (t ,(when extend
			       `(vector-push-extend .val .vec ,@(cddr args)))))))))
  whole)

(define-compiler-macro vector-push (&whole whole &rest args &environment env)
  (expand-vector-push whole env))

(define-compiler-macro vector-push-extend (&whole whole &rest args &environment env)
  (expand-vector-push whole env))
