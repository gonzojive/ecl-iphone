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

(defconstant +initform-unsupplied+ '+initform-unsupplied+)

;;; ----------------------------------------------------------------------
;;; SLOT descriptors
;;;
;;; We need slot definition objects both during bootstrap and also at
;;; runtime. Here we set up a dual definition: if the class
;;; SLOT-DEFINITION has been defined, we use it; otherwise we work
;;; with slot definitions as by the effective structure
;;;
;;;	(defstruct (slot-definition (:type list))
;;;	  name initform initfunction type allocation initargs
;;;	   readers writers documentation)
;;;
;;; However, this structure is not defined explicitely, to save
;;; memory. We rather create a constructor
;;; CANONICAL-SLOT-TO-DIRECT-SLOT and several accessors (closures)
;;; down there.

(defconstant +slot-definition-slots+
  '((name :initarg :name :initform nil :accessor slot-definition-name)
    (initform :initarg :initform :initform #.+initform-unsupplied+ :accessor slot-definition-initform)
    (initfunction :initarg :initfunction :initform nil :accessor slot-definition-initfunction)
    (type :initarg :type :initform t :accessor slot-definition-type)
    (allocation :initarg :allocation :initform :instance :accessor slot-definition-allocation)
    (initargs :initarg :initargs :initform nil :accessor slot-definition-initargs)
    (readers :initarg :readers :initform nil :accessor slot-definition-readers)
    (writers :initarg :writers :initform nil :accessor slot-definition-writers)
    (documentation :initarg :documentation :initform nil :accessor slot-definition-documentation)
    (location :initarg :location :initform nil :accessor slot-definition-location)
    ))

(defun make-simple-slotd (&key name (initform +initform-unsupplied+) initfunction
			  (type 'T) (allocation :instance)
			  initargs readers writers documentation location)
  (list name initform initfunction type allocation initargs readers writers documentation location))

(defun canonical-slot-to-direct-slot (class slotd)
  (if (find-class 'slot-definition nil)
      (apply #'make-instance
	     (apply #'direct-slot-definition-class class slotd)
	     slotd)
      (apply #'make-simple-slotd slotd)))

(let ((accessors (mapcar #'first (mapcar #'last +slot-definition-slots+))))
  (dotimes (i (length accessors))
    (let ((name (first (nth i +slot-definition-slots+)))
	  (position i)
	  (f (nth i accessors)))
      (setf (fdefinition f)
	    #'(lambda (x)
		(if (consp x) (nth position x) (si:instance-ref x position))))
      (setf (fdefinition `(setf ,f))
	    #'(lambda (v x) (if (consp x) (setf (nth position x) v) (si:instance-set x position v)))))))

;;; ----------------------------------------------------------------------
;;;
;;; (PARSE-SLOTS slot-definition-form) => slot-definition-object
;;;
;;; This routine is the one responsible for parsing the definition of
;;; a slot in DEFCLASS.
;;;

(defun parse-slot (slot &optional (full nil))
  (declare (si::c-local))
  (if (symbolp slot)
      (list* :name slot
	     (when full (list :initform '+INITFORM-UNSUPPLIED+ :initfunction nil
			      :initargs nil :readers nil :writers nil
			      :allocation :instance :documentation nil
			      :type 'T)))
      (do* ((output (parse-slot (first slot) full))
	    (options (rest slot))
	    (value nil)
	    (extra nil))
	   ((null options)
	    (nconc output (nreverse extra)))
	(let ((option (pop options)))
	  (when (endp options)
	    (si::simple-program-error
	     "In the slot description ~S,~%the option ~S is missing an argument"
	     slot option))
	  (let ((value (pop options)))
	    (when (and (member option '(:allocation :initform :type :documentation))
		       (getf options option))
	      (si::simple-program-error
	       "In the slot description ~S,~%the option ~S is duplicated"
	       slot option))
	    (case option
	      (:initarg    (push value (getf output :initargs)))
	      (:initform   (setf (getf output :initform) value)
			   (setf (getf output :initfunction) nil))
	      (:accessor   (push value (getf output :readers))
			   (push `(setf ,value) (getf output :writers)))
	      (:reader     (push value (getf output :readers)))
	      (:writer     (push value (getf output :writers)))
	      (:allocation (push value (getf output :allocation)))
	      (:type       (setf (getf output :type) value))
	      (:documentation  (push value (getf output :documentation)))
	      (otherwise   (setf extra (list* value output extra)))))))))

(defun parse-slots (slots)
  (do ((scan slots (cdr scan))
       (collect))
      ((null scan) (nreverse collect))
    (let* ((slotd (parse-slot (first scan)))
	   (name (second slotd)))
      (dolist (other-slotd collect)
	(when (eq name (getf other-slotd :name))
	  (si::simple-program-error
	   "A definition for the slot ~S appeared twice in a DEFCLASS form"
	   name)))
      (push slotd collect))))

;;; ----------------------------------------------------------------------
