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
;;; Built-in classes
;;; ----------------------------------------------------------------------

;;; ----------------------------------------------------------------------
;;; Predefined Common Lisp Classes

#|
;(defclass t (object) () (:metaclass built-in))

(defclass array (t) () (:metaclass built-in))
(defclass sequence (t) () (:metaclass built-in))
   (defclass list (sequence) () (:metaclass built-in))
      (defclass cons (list) () (:metaclass built-in))
   (defclass string (array sequence) () (:metaclass built-in))
   (defclass vector (array sequence) () (:metaclass built-in))
      (defclass bit-vector (vector) () (:metaclass built-in))

(defclass stream (t) () (:metaclass built-in))
  (defclass file-stream (stream) () (:metaclass built-in))
  (defclass echo-stream (stream) () (:metaclass built-in))
  (defclass string-stream (stream) () (:metaclass built-in))
  (defclass two-way-stream (stream) () (:metaclass built-in))
  (defclass synonym-stream (stream) () (:metaclass built-in))
  (defclass broadcast-stream (stream) () (:metaclass built-in))
  (defclass concatenated-stream (stream) () (:metaclass built-in))

(defclass character (t) () (:metaclass built-in))

(defclass number (t) () (:metaclass built-in))
   (defclass complex (number) () (:metaclass built-in))
   (defclass float (number) () (:metaclass built-in))
   (defclass rational (number) () (:metaclass built-in))
      (defclass integer (rational) () (:metaclass built-in))
      (defclass ratio (rational) () (:metaclass built-in))

(defclass symbol (t) () (:metaclass built-in))
   (defclass null (symbol list) () (:metaclass built-in))

(defclass function (t) () (:metaclass built-in))

(defclass pathname (t) () (:metaclass built-in))
   (defclass logical-pathname (pathname) () (:metaclass built-in))
|#

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod make-instance ((class-name symbol) &rest initargs)
  (apply #'make-instance (find-class class-name) initargs))

(defmethod change-class ((instance t) (new-class symbol))
  (funcall #'change-class instance (find-class new-class)))

;;; ----------------------------------------------------------------------
;;; Structures
;;; ----------------------------------------------------------------------

(defun create-structure-class (name
			       superclasses-names
			       direct-slots all-slots
			       default-initargs documentation)
  (declare (ignore default-initargs direct-slots))
  (dolist (slot all-slots)
    (unless (eq :INSTANCE (slotd-allocation slot))
      (error "The structure class ~S can't have shared slots" name)))
  (let* ((metaclass (find-class 'STRUCTURE-CLASS))
	 (existing (find-class name nil))
	 (superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))

    (flet ((unchanged-class ()
	     (and existing
		  (eq metaclass (si:instance-class existing))
		  (equal (or superclasses-names '(STRUCTURE-OBJECT))
			 ;; i.e. class-default-direct-superclasses
			 (mapcar #'(lambda (x) (class-name x))
				 (class-superiors existing)))
		  (equal all-slots (slot-value existing 'SLOTS))
		  (prog2 (setf (slot-value existing 'DOCUMENTATION)
			       documentation)
		      t))))

      (if (unchanged-class)
	  existing
	  (make-instance metaclass
			 :name name
			 :direct-superclasses superclasses
			 :slots all-slots
			 :class-precedence-list cpl)))))

;;; the method to make instances of structure-class
#+nil
(defmethod make-instance ((class structure-metaclass) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

;;; -----------------------------------------------------------------------
;;; Structure-class

(defclass structure-class (class)
  ;; class-precedence-list must be in the same position as in standard-class
  ((precedence-list :initarg :class-precedence-list)
   slot-descriptions initial-offset defstruct-form constructors documentation
		     copier predicate print-function)
  (:metaclass class))

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

;;; the method to initialize the instances of structure-class
(defmethod initialize-instance ((class structure-class)
				&rest initargs &key &allow-other-keys)
    (call-next-method)				; from class T
    
    ;; if the class has a name register it in hash table
    (when (system:sl-boundp (class-name class))
      (setf (find-class (class-name class)) class))

  (dolist (s (class-superiors class))	; inheritance lattice
	  (push class (class-inferiors s)))
  (push class (slot-value class 'PRECEDENCE-LIST)) ;; add itself in cpl
  class)

;;; ----------------------------------------------------------------------
;;; Structure-object
;;; ----------------------------------------------------------------------

;;; Structure-object has no slots and inherits only from t:
;;; (defclass structure-object (t) ())

(eval-when
 (compile load eval)
 (make-instance (find-class 'STRUCTURE-CLASS)
		:name 'STRUCTURE-OBJECT
		:direct-superclasses (list (find-class 't))
		:slots ()
		:class-precedence-list ()
		:slot-index-table ()
		:direct-slots ()
		:default-initargs ()
		:documentation "The root of inheritance for structures"
		:slot-descriptions ()
		:initial-offset 0
		:defstruct-form ()
		:constructors ()
		:copier ()
		:predicate ()
		:print-function ()))

(defmethod print-object ((obj structure-object) stream)
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class)))
    (princ "#S(" stream)
    (prin1 (class-name class) stream)
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i))
	 (sv))
	((null scan))
	(declare (fixnum i))
	(setq sv (si:instance-ref obj i))
	(princ " " stream)
	(prin1 (slotd-name (car scan)) stream)
	(princ " " stream)
	(prin1 sv stream)
	)
    (princ ")" stream)
    obj))

;;; ----------------------------------------------------------------------

(eval-when (compile load eval)
  (mapcar #'(lambda (args &aux (class (first args)) (super (cdr args)))
	      (eval `(defclass ,class ,super () (:metaclass built-in-class))))
	  '(;(t object)
	    (sequence t)
	      (list sequence)
	        (cons list)
	    (array t)
	      (vector array sequence)
	        (string vector)
	        (bit-vector vector)
	    (stream t)
	      (file-stream stream)
	      (echo-stream stream)
	      (string-stream stream)
	      (two-way-stream stream)
	      (synonym-stream stream)
	      (broadcast-stream stream)
	      (concatenated-stream stream)
	    (character t)
	    (number t)
	      (real number)
	        (rational real)
		  (integer rational)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol t)
	      (null symbol list)
	      (keyword symbol)
	    (package t)
	    (function t)
	    (pathname t)
	      (logical-pathname pathname)
	    (hash-table t)
	    (random-state)
	    (readtable))))

;;; Now we protect classes from redefinition:
(defun setf-find-class (name new-value)
  (cond
   ((typep (find-class name nil) 'built-in-class)
    (error "The class associated to the CL specifier ~S cannot be changed."
	   name))
   ((member name '(CLASS BUILT-IN-CLASS) :test #'eq)
    (error "The kernel CLOS class ~S cannot be changed." name))
   ((classp new-value)
    (setf (gethash name si:*class-name-hash-table*) new-value))
   ((null new-value) (remhash name si:*class-name-hash-table*))
   (t (error "~A is not a class." new-value))))

