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

;;; ======================================================================
;;; STRUCTURES
;;;

(defclass structure-class (class)
  (slot-descriptions initial-offset defstruct-form constructors documentation
		     copier predicate print-function))

;;; structure-classes cannot be instantiated
(defmethod make-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (error "The structure-class (~A) cannot be instantiated" class))

(defmethod finalize-inheritance ((class structure-class))
  (call-next-method)
  (dolist (slot (class-slots class))
    (unless (eq :INSTANCE (slotd-allocation slot))
      (error "The structure class ~S can't have shared slots" (class-name class)))))

;;; ----------------------------------------------------------------------
;;; Structure-object
;;;

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

(defmethod slot-exists-p ((obj structure-object) slot-name)
  (let ((class (si:instance-class obj)))
    ;(declare (type structure-class class))
    ;; FIXME! NIL could, in principle, be valid slot name. We reject it here
    ;; because DEFSTRUCT uses this name to mark padding slots for initial-offset.
    (and slot-name
	 (member slot-name (slot-value class 'slots) :key #'slotd-name)
	 t)))

;;; ======================================================================
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
	 (direct-superclasses (mapcar #'find-class (or (rest options)
						       '(t)))))
    (setf (find-class name)
	  (make-instance (find-class 'built-in-class)
			 :name name
			 :direct-superclasses direct-superclasses
			 :direct-slots nil))))

(defmethod make-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "The built-in class (~A) cannot be instantiated" class))

(mapcar #'create-built-in-class
	  '(;(t object)
	    (sequence)
	      (list sequence)
	        (cons list)
	    (array)
	      (vector array sequence)
	        (string vector)
	        (bit-vector vector)
	    (stream)
	      (file-stream stream)
	      (echo-stream stream)
	      (string-stream stream)
	      (two-way-stream stream)
	      (synonym-stream stream)
	      (broadcast-stream stream)
	      (concatenated-stream stream)
	    (character)
	    (number)
	      (real number)
	        (rational real)
		  (integer rational)
		  (ratio rational)
	        (float real)
	      (complex number)
	    (symbol)
	      (null symbol list)
	      (keyword symbol)
	    (method-combination)
	    (package)
	    (function)
	    (pathname)
	      (logical-pathname pathname)
	    (hash-table)
	    (random-state)
	    (readtable)
	    #+threads (mp::process)
	    #+threads (mp::lock)))
