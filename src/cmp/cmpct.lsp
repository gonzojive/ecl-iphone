;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;; CMPCT --  Optimizer for several constant values

;;;;  Copyright (c) 2003, Juan Jose Garcia Ripoll.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defvar +optimizable-constant+ '())

(defun c1constant-value (val &key always only-small-values)
  (cond
   ((let ((x (assoc val +optimizable-constant+)))
      (when x
       (pushnew "#include <float.h>" *clines-string-list*)
       (c1expr (cdr x)))))
   ((eq val nil) (c1nil))
   ((eq val t) (c1t))
   ((sys::fixnump val)
    (make-c1form* 'LOCATION :type 'FIXNUM :args (list 'FIXNUM-VALUE val)))
   ((characterp val)
    (make-c1form* 'LOCATION :type 'CHARACTER
		  :args (list 'CHARACTER-VALUE (char-code val))))
   ((typep val 'DOUBLE-FLOAT)
    (make-c1form* 'LOCATION :type 'DOUBLE-FLOAT
		  :args (list 'DOUBLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'SINGLE-FLOAT)
    (make-c1form* 'LOCATION :type 'SINGLE-FLOAT
		  :args (list 'SINGLE-FLOAT-VALUE val (add-object val))))
   ((typep val 'LONG-FLOAT)
    (make-c1form* 'LOCATION :type 'LONG-FLOAT
		  :args (list 'LONG-FLOAT-VALUE val (add-object val))))
   (always
    (make-c1form* 'LOCATION :type (object-type val)
		  :args (list 'VV (add-object val))))
   (only-small-values nil)
   (t nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; OPTIMIZABLE DOUBLE CONSTANTS
;;;

(mapc
 #'(lambda (record)
     (let* ((name (first record))
	    (c-value (second record))
	    (value (symbol-value name))
	    (type (lisp-type->rep-type (type-of value))))
       (push (cons value `(c-inline () () ,type ,c-value :one-liner t :side-effects nil))
	     +optimizable-constant+)))

 '((MOST-POSITIVE-SHORT-FLOAT "FLT_MAX")
   (MOST-POSITIVE-SINGLE-FLOAT "FLT_MAX")

   (MOST-NEGATIVE-SHORT-FLOAT "-FLT_MAX")
   (MOST-NEGATIVE-SINGLE-FLOAT "-FLT_MAX")

   (LEAST-POSITIVE-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-SINGLE-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SHORT-FLOAT "FLT_MIN")
   (LEAST-POSITIVE-NORMALIZED-SINGLE-FLOAT" FLT_MIN")

   (LEAST-NEGATIVE-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-SINGLE-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SHORT-FLOAT "-FLT_MIN")
   (LEAST-NEGATIVE-NORMALIZED-SINGLE-FLOAT "-FLT_MIN")

   (MOST-POSITIVE-DOUBLE-FLOAT "DBL_MAX")
   (MOST-NEGATIVE-DOUBLE-FLOAT "-DBL_MAX")
   (LEAST-POSITIVE-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-POSITIVE-NORMALIZED-DOUBLE-FLOAT "DBL_MIN")
   (LEAST-NEGATIVE-DOUBLE-FLOAT "-DBL_MIN")
   (LEAST-NEGATIVE-NORMALIZED-DOUBLE-FLOAT "-DBL_MIN")

   . #+long-float NIL #-long-float
   (
    (MOST-POSITIVE-LONG-FLOAT "DBL_MAX")
    (MOST-NEGATIVE-LONG-FLOAT "-DBL_MAX")
    (LEAST-POSITIVE-LONG-FLOAT "DBL_MIN")
    (LEAST-POSITIVE-NORMALIZED-LONG-FLOAT" DBL_MIN")
    (LEAST-NEGATIVE-LONG-FLOAT "-DBL_MIN")
    (LEAST-NEGATIVE-NORMALIZED-LONG-FLOAT "-DBL_MIN")
    )
   ))
