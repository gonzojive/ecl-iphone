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
;;; BOOT

(defun boot ()
  (let* ((class (find-class 'class))
	 (built-in-class (find-class 'built-in-class)))

    ;; class CLASS	--------
    (setf (class-slots class)
	  (parse-slots '((NAME :INITARG :NAME :INITFORM NIL)
			       (SUPERIORS :INITARG :DIRECT-SUPERCLASSES)
			       (INFERIORS :INITFORM NIL)
			       (SLOTS :INITARG :SLOTS))))

    (defmethod OPTIMIZE-SLOT-VALUE ((class class) form) form)

    (defmethod OPTIMIZE-SET-SLOT-VALUE ((class class) form) form)

    (defmethod make-instance ((class class) &rest initargs)
      (let ((instance (allocate-instance class)))
	(apply #'initialize-instance instance initargs)
	instance))

    (defmethod initialize-instance ((class class) 
				    &rest initargs 
				    &key direct-superclasses
				    &allow-other-keys)

	(call-next-method)			; from class T

	;; default inheritance
	(unless direct-superclasses 
	  (setf (class-superiors class)
		(class-default-direct-superclasses class direct-superclasses)))
	  
	;; if the class has a name register it in hash table
	(when (si:sl-boundp (class-name class))
	  (setf (find-class (class-name class)) class))
	(dolist (s (class-superiors class)) ; inheritance lattice
	  (push class (class-inferiors s)))
	class)

    (defmethod class-default-direct-superclasses ((class class)
						  supplied-superclasses)
      (or supplied-superclasses
	  (list (find-class 't))))

    ;; class BUILT-IN-CLASS	--------
    (setf (class-slots built-in-class)
	  (parse-slots '((NAME :INITARG :NAME :INITFORM NIL)
			       (SUPERIORS :INITARG :DIRECT-SUPERCLASSES)
			       (INFERIORS :INITFORM NIL)
			       (SLOTS :INITARG :SLOTS))))

    (defmethod slot-value ((self built-in-class) slot)
      (let ((position (position slot (class-slots (si:instance-class self))
				:key #'slotd-name)))
	(if position
	    (si:instance-ref self position)
	    (slot-missing (si:instance-class self) self slot 'slot-value))))

    (defmethod make-instance ((class built-in-class) &rest initargs)
      (declare (ignore initargs))
      (error "The built-in class (~A) cannot be instantiated" class))

    (defmethod initialize-instance ((class built-in-class)
				    &rest initargs &key &allow-other-keys)

	(call-next-method)		; from class T
	
	;; if the class has a name register it in hash table
	(when (si:sl-boundp (class-name class))
	  (setf (find-class (class-name class)) class)) 

	(dolist (s (class-superiors class)) ; inheritance lattice
	  (push class (class-inferiors s)))
	class)

    (defmethod print-object ((class built-in-class) stream)
      (print-unreadable-object
       (class stream)
       (format stream "The ~A ~A" (class-name (si:instance-class class))
	       (class-name class)))
      class)

    ;; class T	--------
    (defmethod initialize-instance ((instance T)
				    &rest initargs &key &allow-other-keys)
	(let ((class-slots (class-slots (si:instance-class instance))))
	  ;; initialize from initforms
	  (do ((scan class-slots (cdr scan))
	       (i 0 (1+ i)))
	      ((null scan))
	    (when (and (not (si:sl-boundp 
			     (si:instance-ref instance i)))
		       (not (eq (slotd-initform (first scan)) 
				'INITFORM-UNSUPPLIED)))
	      (si:instance-set instance i
			       (eval (slotd-initform (first scan))))))

	  ;; initialize from initargs
	  (do* ((name-loc initargs (cddr name-loc))
		(name (first name-loc) (first name-loc)))
	       ((null name-loc))
	    ;; scan the class-slots to fill them with the initargs
	    (do ((scan-slot  class-slots (cdr scan-slot))
		 (index 0 (1+ index)))
		((null scan-slot) ())
	      (declare (fixnum index))
	      ;; if the initarg is associated with a slot
	      (when (member name (slotd-initargs (first scan-slot)))
		;; fill the slot
		(setf (si:instance-ref instance index)
		      (second name-loc)))
	      ;; go on scanning the slots because a single initarg
	      ;; can initialize more than one slot
	      )))
      instance)

    (defmethod slot-missing ((class t) object slot-name operation 
			     &optional new-value)
      (declare (ignore operation new-value))
      (error "~A is not a slot of ~A" slot-name object))

    (defmethod slot-unbound ((class t) object slot-name)
      (error 'slot-unbound :instance object :name slot-name))
    ))

(boot)

;;; ----------------------------------------------------------------------
