;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")


;;; ----------------------------------------------------------------------
;;; Generic Functions
;;; ----------------------------------------------------------------------

(defclass generic-function (standard-object function) ())

(defclass standard-generic-function (generic-function)
  #.+standard-generic-function-slots+)

;;;----------------------------------------------------------------------
;;; Method
;;; ----------------------------------------------------------------------

(defclass method () ())

(defclass standard-method (method)
  #.+standard-method-slots+)


(defun function-keywords (method)
  (multiple-value-bind (reqs opts rest-var key-flag keywords)
      (si::process-lambda-list (slot-value method 'lambda-list) 'function)
    (when key-flag
      (do* ((output '())
	    (l (cdr keywords) (cddddr l)))
	   ((endp l)
	    output)
	(push (first l) output)))))

(defclass standard-accessor-method (standard-method)
  ((slot-definition :initarg :slot-definition
		    :initform nil 
		    :reader accessor-method-slot-definition)))

(defclass standard-reader-method (standard-accessor-method) ())

(defclass standard-writer-method (standard-accessor-method) ())
