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
;;; Building the classes T, CLASS, STANDARD-OBJECT and STANDARD-CLASS.
;;;
;;; We cannot use the functions CREATE-STANDARD-CLASS and others because
;;; SLOT-INDEX-TABLE, SLOTS, DIRECT-SLOTS, etc are empty and therefore
;;; SLOT-VALUE does not work.

(defun make-empty-standard-class (name metaclass)
  (let ((class (si:allocate-raw-instance metaclass 12)))
    (unless metaclass
      (si:instance-class-set class class))
    (setf (si:instance-ref class 0) name ; name
	  (si:instance-ref class 1) nil ; superiors
	  (si:instance-ref class 2) nil ; inferiors
	  (si:instance-ref class 3) nil ; slots
	  (si:instance-ref class 4) nil ; precedencew-list
	  (si:instance-ref class 5) (make-hash-table :size 2) ; slot-index-table
	  (si:instance-ref class 6) nil ; direct-slots
	  (si:instance-ref class 7) nil ; shared-slots
	  (si:instance-ref class 8) 0 ; instance-slot-count
	  (si:instance-ref class 9) nil ; default-initargs
	  (si:instance-ref class 10) nil ; documentation
	  (find-class name) class
	  )))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; no metaclass until we define it...
;;
(let* ((standard-class (make-empty-standard-class 'STANDARD-CLASS nil))
       (standard-object (make-empty-standard-class 'STANDARD-OBJECT standard-class))
       (the-class (make-empty-standard-class 'CLASS standard-class))
       (the-t (make-empty-standard-class 'T standard-class))
       (class-slots '((NAME :INITARG :NAME :INITFORM NIL)
		      (DIRECT-SUPERCLASSES :INITARG :DIRECT-SUPERCLASSES)
		      (DIRECT-SUBCLASSES :INITFORM NIL)
		      (SLOTS :INITARG :SLOTS)
		      (PRECEDENCE-LIST :INITARG :CLASS-PRECEDENCE-LIST)))
       (standard-slots (append class-slots
			 '((SLOT-INDEX-TABLE)
			   (DIRECT-SLOTS :INITARG :DIRECT-SLOTS)
			   (SHARED-SLOTS :INITARG :SHARED-SLOTS :INITFORM NIL
			    :ACCESSOR CLASS-SHARED-SLOTS)
			   (INSTANCE-SLOT-COUNT
			    :ACCESSOR CLASS-INSTANCE-SLOT-COUNT)
			   (DEFAULT-INITARGS :INITARG :DEFAULT-INITARGS
			     :READER DEFAULT-INITARGS-OF)
			   (DOCUMENTATION :INITARG :DOCUMENTATION
			    :ACCESSOR DOCUMENTATION-OF)
			   (FORWARD))))
       (hash-table (make-hash-table :size 24)))

  ;; ... here!!!
  ;;
  (si::instance-class-set standard-class standard-class)

  ;; 2) STANDARD-CLASS and CLASS are the only classes with slots. Create a
  ;; hash table for them, so that SLOT-VALUE works. Notice that we
  ;; make a intentional mistake: CLASS and STANDARD-CLASS share the same
  ;; hashtable!!
  (do* ((i 0 (1+ i))
	(slots standard-slots (cdr slots)))
       ((endp slots))
    (setf (gethash (caar slots) hash-table) i))
  (setf (si:instance-ref the-class 3) (parse-slots class-slots)
	(si:instance-ref the-class 5) hash-table
	(si:instance-ref the-class 6) (class-slots the-class)
	(si:instance-ref the-class 8) (length class-slots)
	(si:instance-ref standard-class 3) (parse-slots standard-slots)
	(si:instance-ref standard-class 5) hash-table
	(si:instance-ref standard-class 6) (class-slots standard-class)
	(si:instance-ref standard-class 8) (length standard-slots))

  ;; 3) Fix the class hierarchy
  (setf (class-direct-superclasses the-t) nil
	(class-direct-subclasses the-t) (list standard-object)
	(class-direct-superclasses standard-object) (list the-t)
	(class-direct-subclasses standard-object) (list the-class)
	(class-direct-superclasses the-class) (list standard-object)
	(class-direct-subclasses the-class) (list standard-class)
	(class-direct-superclasses standard-class) (list the-class))

  ;; 4) Fix the class precedence list
  (let ((cpl (list standard-class the-class standard-object the-t)))
    (setf (class-precedence-list standard-class) cpl
	  (class-precedence-list the-class) (cdr cpl)
	  (class-precedence-list standard-object) (cddr cpl)
	  (class-precedence-list the-t) nil))

  ;; 5) Generate accessors (In standard.lsp)
)

    (defmethod OPTIMIZE-SLOT-VALUE ((class t) form) form)

    (defmethod OPTIMIZE-SET-SLOT-VALUE ((class t) form) form)

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
	  (setf (class-direct-superclasses class)
		(class-default-direct-superclasses class direct-superclasses)))
	  
	;; if the class has a name register it in hash table
	(when (si:sl-boundp (class-name class))
	  (setf (find-class (class-name class)) class))
	(dolist (s (class-direct-superclasses class)) ; inheritance lattice
	  (push class (class-direct-subclasses s)))
	class)

    (defmethod class-default-direct-superclasses ((class class)
						  supplied-superclasses)
      (or supplied-superclasses
	  (list (find-class 't))))

    (defmethod slot-value ((self class) slot-name)
      (let* ((class (si:instance-class self))
	     (index (position slot-name (class-slots class)
			      :key #'slotd-name :test #'eq)))
	(values
	 (if index
	     (let ((val (si:instance-ref self (the fixnum index))))
	       (if (si:sl-boundp val)
		   val
		   (slot-unbound (si::instance-class class) class slot-name)))
	     (slot-missing (si:instance-class class) class slot-name 
			   'SLOT-VALUE)))))

    (defmethod slot-boundp ((self class) slot-name)
      (let* ((class (si:instance-class self))
	     (index (position slot-name (class-slots class)
			      :key #'slotd-name :test #'eq)))
	(values
	 (if index
	     (si:sl-boundp (si:instance-ref self (the fixnum index)))
	     (slot-missing (si:instance-class class) class slot-name
			   'SLOT-VALUE)))))

    (defmethod (setf slot-value) (val (self class) slot-name)
      (let* ((class (si:instance-class self))
	     (index (position slot-name (class-slots class)
			      :key #'slotd-name :test #'eq)))
	(if index
	    (si:instance-set self (the fixnum index) val)
	    (slot-missing (si:instance-class self) self slot-name
			  'SLOT-VALUE)))
      val)

    ;; class T	--------
    (defmethod initialize-instance ((instance T)
				    &rest initargs &key &allow-other-keys)
	(let ((class-slots (class-slots (si:instance-class instance))))
	  ;; initialize from initforms
	  (do ((scan class-slots (cdr scan))
	       (i 0 (1+ i)))
	      ((null scan))
	    (unless (si:sl-boundp (si:instance-ref instance i))
	      (let ((initform (slotd-initform (first scan))))
		(unless (eq initform 'INITFORM-UNSUPPLIED)
		  (when (functionp initform)
		    (setq initform (funcall initform)))
		  (si:instance-set instance i initform)))))

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

;;; ----------------------------------------------------------------------
