;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(defpackage "CLOS"
  (:use "WALKER" "CL")
  (:import-from "SI" "UNBOUND" "GET-SYSPROP" "PUT-SYSPROP" "REM-SYSPROP"
		"COMPUTE-EFFECTIVE-METHOD"))

(in-package "CLOS")

;;; ----------------------------------------------------------------------

;(proclaim '(DECLARATION VARIABLE-REBINDING))
;;; Make this stable:
(declaim (DECLARATION VARIABLE-REBINDING))

(defvar *keyword-package* (find-package 'KEYWORD))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) *keyword-package*))

(defmacro doplist ((key val) plist &body body)
  `(let* ((.plist-tail. ,plist) ,key ,val)
     (loop (when (null .plist-tail.) (return nil))
	   (setq ,key (pop .plist-tail.))
	   (when (null .plist-tail.)
	     (error "Malformed plist in doplist, odd number of elements."))
	   (setq ,val (pop .plist-tail.))
	   (progn ,@body))))
;;;
;;;;;; FIND-CLASS  naming classes.
;;;
;;;
;;; (FIND-CLASS <name>) returns the class named <name>.  setf can be used
;;; with find-class to set the class named <name>.  These are "extrinsic"
;;; names.  Neither find-class nor setf of find-class do anything with the
;;; name slot of the class, they only lookup and change the association from
;;; name to class.
;;; 

;(defvar *class-name-hash-table* (make-hash-table :test #'eq)
; "The hash table containing all classes")

;;; This is only used during boot. The real one is in built-in.
(eval-when (compile)
  (defun setf-find-class (class new-value)
    (warn "Ignoring class definition for ~S" class)))

(defun setf-find-class (name new-value)
  (if (si:instancep new-value)
      (setf (gethash name si:*class-name-hash-table*) new-value)
    (error "~A is not a class." new-value)))

(defsetf find-class setf-find-class)

;;; ----------------------------------------------------------------------

(defmacro keyword-bind (keywords form &body body)
  `(apply (function (lambda (&key . ,keywords) . ,body)) ,form))

(defun mapappend (fun &rest args)
  (reduce #'append (apply #'mapcar fun args)))
