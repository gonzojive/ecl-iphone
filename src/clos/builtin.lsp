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

;;; -----------------------------------------------------------------------
;;; Structure-class

(defclass structure-class (class)
  (slot-descriptions initial-offset defstruct-form constructors documentation
		     copier predicate print-function))

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

(defclass structure-object (t) ()
  (:metaclass structure-class))

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
;;; Built-in classes
;;; ----------------------------------------------------------------------
;;;
;;; IMPORTANT!
;;; This class did not exist until now. This was no problem, because it is
;;; not used anywhere in ECL. However, we have to define and we have to
;;; ensure that "T" becomes an instance of BUILT-IN-CLASS.

(defclass built-in-class (class)
  ())

(si:instance-class-set (find-class 't) (find-class 'built-in-class))

(defun create-built-in-class (options)
  (let* ((name (first options))
	 (direct-superclasses (mapcar #'find-class (rest options))))
    (make-instance (find-class 'built-in-class)
		   :name name
		   :direct-superclasses direct-superclasses
		   :slots nil)))

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(defmethod initialize-instance ((class built-in-class)
				&key name direct-superclasses)
  (let* ((cpl (compute-class-precedence-list name direct-superclasses)))
    (setf (class-name class) name
	  (class-superiors class) direct-superclasses
	  (class-inferiors class) nil
	  (class-precedence-list class) cpl
	  (find-class name) class)
    (dolist (s direct-superclasses)
      (push class (class-inferiors s)))))

(defmethod print-object ((class built-in-class) stream)
  (print-unreadable-object
      (class stream)
    (format stream "The ~A ~A" (class-name (si:instance-class class))
	    (class-name class)))
  class)

(eval-when (compile load eval)
  (mapcar #'create-built-in-class
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
