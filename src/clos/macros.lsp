;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(defpackage "CLOS"
  (:use "WALKER" "CL"))

(in-package "CLOS")

;;; ----------------------------------------------------------------------

;(proclaim '(DECLARATION VARIABLE-REBINDING))
;;; Make this stable:
(declaim (DECLARATION VARIABLE-REBINDING))

(defvar *keyword-package* (find-package 'KEYWORD))

(defun make-keyword (symbol)
  (intern (symbol-name symbol) *keyword-package*))

(defmacro doplist ((key val) plist &body body)
  `(let ((.plist-tail. ,plist) ,key ,val)
     (loop (when (null .plist-tail.) (return nil))
	   (setq ,key (pop .plist-tail.))
	   (when (null .plist-tail.)
	     (error "Malformed plist in doplist, odd number of elements."))
	   (setq ,val (pop .plist-tail.))
	   (progn ,@body))))
  ;;
;;;;;; FIND-CLASS  naming classes.
  ;;
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
(defun setf-find-class (name new-value)
  (if (classp new-value)
      (setf (gethash name si:*class-name-hash-table*) new-value)
    (error "~A is not a class." new-value)))

(defsetf find-class setf-find-class)

(defun legal-class-name-p (x)
  (and (symbolp x)
       (not (keywordp x))))

;;; ----------------------------------------------------------------------

(defmacro keyword-bind (keywords form &body body)
  `(apply (function (lambda (&key . ,keywords) . ,body)) ,form))

;;;----------------------------------------------------------------------
;;; Implementation of Instance

;;; ECL implementation:

(declaim (function si:instance-ref (t fixnum) t))
(declaim (function si:instance-set (t fixnum t) t))

(defmacro unbound () '(sys:nani 0))

;;; ----------------------------------------------------------------------
;;; Class CLASS
;;;
;;; The slots in Class Class are:
;;; name superiors inferiors slots methods

;(defmacro class-name		(class) `(si:instance-ref ,class 0))
;(defmacro class-superiors	(class) `(si:instance-ref ,class 1))
;(defmacro class-inferiors	(class) `(si:instance-ref ,class 2))
;(defmacro class-slots		(class) `(si:instance-ref ,class 3))
(defun class-name		(class) (si:instance-ref class 0))
(defun class-superiors		(class) (si:instance-ref class 1))
(defun class-inferiors		(class) (si:instance-ref class 2))
(defun class-slots		(class) (si:instance-ref class 3))
(defsetf class-name		(class) (x) `(si::instance-set ,class 0 ,x))
(defsetf class-superiors	(class) (x) `(si::instance-set ,class 1 ,x))
(defsetf class-inferiors	(class) (x) `(si::instance-set ,class 2 ,x))
(defsetf class-slots		(class) (x) `(si::instance-set ,class 3 ,x))
(define-compiler-macro class-name	(class) `(si:instance-ref ,class 0))
(define-compiler-macro class-superiors	(class) `(si:instance-ref ,class 1))
(define-compiler-macro class-inferiors	(class) `(si:instance-ref ,class 2))
(define-compiler-macro class-slots	(class) `(si:instance-ref ,class 3))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

;;; for compiler optimization to work, a-standard-class should be
;;; a variable declared of type standard-class.
(defmacro slot-index-table (a-standard-class)
  `(the hash-table (si:instance-ref ,a-standard-class 5)))
