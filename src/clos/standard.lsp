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
;;; INSTANCES INITIALIZATION AND REINITIALIZATION
;;;

(defmethod initialize-instance ((instance T) &rest initargs)
  (apply #'shared-initialize instance 'T initargs))

(defmethod reinitialize-instance ((instance T) &rest initargs)
  (check-initargs (class-of instance) initargs
		  (append (compute-applicable-methods
			   #'reinitialize-instance (list instance))
			  (compute-applicable-methods
			   #'shared-initialize (list instance t))))
  (apply #'shared-initialize instance '() initargs))

(defmethod shared-initialize ((instance T) slot-names &rest initargs)
  ;;
  ;; initialize the instance's slots is a two step process
  ;;   1 A slot for which one of the initargs in initargs can set
  ;;      the slot, should be set by that initarg.  If more than
  ;;      one initarg in initargs can set the slot, the leftmost
  ;;      one should set it.
  ;;
  ;;   2 Any slot not set by step 1, may be set from its initform
  ;;      by step 2.  Only those slots specified by the slot-names
  ;;      argument are set.  If slot-names is:
  ;;       T
  ;;            any slot not set in step 1 is set from its
  ;;            initform
  ;;       <list of slot names>
  ;;            any slot in the list, and not set in step 1
  ;;            is set from its initform
  ;;
  ;;       ()
  ;;            no slots are set from initforms
  ;;
  (let* ((class (class-of instance)))
    ;; initialize-instance slots
    (dolist (slotd (class-slots class))
      (let* ((slot-initargs (slot-definition-initargs slotd))
	     (slot-name (slot-definition-name slotd)))
	(or
	 ;; Try to initialize the slot from one of the initargs.
	 (do ((l initargs) initarg val)
	     ((null l) nil)
	   (setf initarg (pop l))
	   (when (endp l)
	     (simple-program-error "Wrong number of keyword arguments for SHARED-INITIALIZE, ~A"
				   initargs))
	   (unless (symbolp initarg)
	     (simple-program-error "Not a valid initarg: ~A" initarg))
	   (setf val (pop l))
	   (when (member initarg slot-initargs :test #'eq)
	     (setf (slot-value instance slot-name) val)
	     (return t)))
	 ;; Try to initialize the slot from its initform.
	 (when (and slot-names
		    (or (eq slot-names 'T)
			(member slot-name slot-names))
		    (not (slot-boundp instance slot-name)))
	   (let ((initform (slot-definition-initform slotd)))
	     (unless (eq initform '+INITFORM-UNSUPPLIED+)
	       (when (functionp initform)
		 (setq initform (funcall initform)))
	       (setf (slot-value instance slot-name) initform)))))
	)))
  instance)

;;; ----------------------------------------------------------------------
;;; CLASSES INITIALIZATION AND REINITIALIZATION
;;;

(defun count-instance-slots (class)
  (count :instance (class-slots class) :key #'slot-definition-allocation))

(defmethod allocate-instance ((class class) &key)
  ;; FIXME! Inefficient! We should keep a list of dependent classes.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((x (si::allocate-raw-instance nil class (count-instance-slots class))))
    (si::instance-sig-set x)
    x))

(defmethod make-instance ((class class) &rest initargs)
  ;; We add the default-initargs first, because one of these initargs might
  ;; be (:allow-other-keys t), which disables the checking of the arguments.
  ;; (Paul Dietz's ANSI test suite, test CLASS-24.4)
  (setf initargs (add-default-initargs class initargs))
  (check-initargs class initargs
		  (append (compute-applicable-methods
			   #'allocate-instance (list class))
			  (compute-applicable-methods
			   #'initialize-instance (list (class-prototype class)))
			  (compute-applicable-methods
			   #'shared-initialize (list (class-prototype class) t))))
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance initargs)
    instance))

(defun add-default-initargs (class initargs)
  (declare (si::c-local))
  ;; Here, for each slot which is not mentioned in the initialization
  ;; arguments, but which has a value associated with :DEFAULT-INITARGS,
  ;; we compute the value and add it to the list of initargs.
  (dolist (scan (class-default-initargs class))
    (let ((initarg (first scan))
	  (value (third scan))
	  (supplied-value (si::search-keyword initargs initarg)))
      (when (or (eq supplied-value '+initform-unsupplied+)
		(eq supplied-value 'si::failed))
	(when (eq supplied-value '+initform-unsupplied+)
	  (remf initargs initarg))
	(setf value (if (functionp value) (funcall value) value)
	      initargs (append initargs (list initarg value))))))
  initargs)

(defmethod direct-slot-definition-class ((class T) &rest canonicalized-slot)
  (find-class 'standard-direct-slot-definition nil))

(defmethod effective-slot-definition-class ((class T) &rest canonicalized-slot)
  (find-class 'standard-effective-slot-definition nil))

(defmethod initialize-instance ((class class) &rest initargs
				&key direct-superclasses direct-slots)

  ;; this sets up all the slots of the class
  (call-next-method)

  ;; the list of direct slots is converted to direct-slot-definitions
  (setf (class-direct-slots class)
	(loop for s in direct-slots
	      collect (canonical-slot-to-direct-slot class s)))

  ;; set up inheritance checking that it makes sense
  (dolist (l (setf (class-direct-superclasses class)
		   (check-direct-superclasses class direct-superclasses)))
    (add-direct-subclass l class))
  (if (find-if #'forward-referenced-class-p (class-direct-superclasses class))
      (find-if #'forward-referenced-class-p (class-direct-superclasses class))
      (finalize-inheritance class))
)

(defmethod add-direct-subclass ((parent class) child)
  (pushnew child (class-direct-subclasses parent)))

(defmethod remove-direct-subclass ((parent class) child)
  (setf (class-direct-subclasses parent)
	(remove child (class-direct-subclasses parent))))

(defmethod check-direct-superclasses (class supplied-superclasses)
  (unless supplied-superclasses
    (setf supplied-superclasses
	  (list (find-class (typecase class
			      (STANDARD-CLASS 'STANDARD-OBJECT)
			      (STRUCTURE-CLASS 'STRUCTURE-OBJECT)
			      (otherwise (error "No :DIRECT-SUPERCLASS ~
argument was supplied for metaclass ~S." (class-of class))))))))
  ;; FIXME!!! Here should come the invocation of VALIDATE-SUPERCLASS!
  ;; FIXME!!! We should check that structures and standard objects are
  ;; not mixed, and that STANDARD-CLASS, or STANDARD-GENERIC-FUNCTION,
  ;; etc, are the first classes.
  supplied-superclasses)

;;; ----------------------------------------------------------------------
;;; FINALIZATION OF CLASS INHERITANCE
;;;
(defun forward-referenced-class-p (x)
  (let ((y (find-class 'FORWARD-REFERENCED-CLASS nil)))
    (and y (si::subclassp (class-of x) y))))

(defmethod finalize-inheritance ((class class))
  ;; FINALIZE-INHERITANCE computes the guts of what defines a class: the
  ;; slots, the list of parent class, etc. It is called when either the
  ;; class was not finalized before, or when one of the parents has been
  ;; modified.
  ;;
  (let ((cpl (compute-class-precedence-list class)))
    ;; A class cannot be finalized if any of its parents is either
    ;; a not yet defined class or it has not yet been finalized.
    ;; In the first case we can just signal an error...
    ;;
    (let ((x (find-if #'forward-referenced-class-p (rest cpl))))
      (when x
	(error "Cannot finish building the class~%  ~A~%~
because it contains a reference to the undefined class~%  ~A"
	       (class-name class) (class-name x))))
    ;;
    ;; ... and in the second case we just finalize the top-most class
    ;; which is not yet finalized and rely on the fact that this
    ;; class will also try to finalize all of its children.
    ;;
    (let ((x (find-if-not #'class-finalized-p cpl :from-end t)))
      (unless (or (null x) (eq x class))
	(return-from finalize-inheritance
	  (finalize-inheritance x))))
    (setf (class-precedence-list class) cpl
	  (class-slots class) (compute-slots class)
	  (class-default-initargs class) (compute-default-initargs class)
	  (class-finalized-p class) t)
    ;;
    ;; This is not really needed, because when we modify the list of slots
    ;; all instances automatically become obsolete (See change.lsp)
    ;(make-instances-obsolete class)
    )
  ;; As mentioned above, when a parent is finalized, it is responsible for
  ;; invoking FINALIZE-INHERITANCE on all of its children. Obviously,
  ;; this only makes sense when the class has been defined.
  (dolist (subclass (reverse (class-direct-subclasses class)))
    (reinitialize-instance subclass
			   :direct-superclasses (class-direct-superclasses subclass)))
  )

(defun std-create-slots-table (class)
  (let* ((all-slots (class-slots class))
	 (table (make-hash-table :size (max 32 (length all-slots)))))
    (dolist (slotd (class-slots class))
      (setf (gethash (slot-definition-name slotd) table) slotd))
    (setf (slot-table class) table)))

(defmethod finalize-inheritance ((class standard-class))
  (call-next-method)
  (std-create-slots-table class)
  (std-class-generate-accessors class))

(defmethod compute-class-precedence-list ((class class))
  (cons class
	(compute-clos-class-precedence-list (class-name class)
					    (class-direct-superclasses class))))

(defmethod compute-slots ((class class))
  ;; INV: for some classes ECL expects that the order of the inherited slots is
  ;; preserved. The following code ensures that, if C1 is after C2 in the
  ;; class precedence list, and the slot S1 appears both in C1 and C2,
  ;; the slot S1 will appear the new class before the slots of C2; and
  ;; whenever possible, in the same position as in C1.
  ;;
  (do* ((all-slots (mapappend #'class-direct-slots (reverse (class-precedence-list class))))
	(all-names (nreverse (mapcar #'slot-definition-name all-slots)))
	(output '())
	(scan all-names (cdr scan)))
       ((endp scan) output)
    (let ((name (first scan)))
      (unless (find name (rest scan))
	(push (compute-effective-slot-definition
	       class name (delete name (reverse all-slots) :key #'slot-definition-name
				  :test-not #'eq))
	      output)))))

(defun slot-definition-to-list (slotd)
  (list :name (slot-definition-name slotd)
	:initform (slot-definition-initform slotd)
	:initfunction (slot-definition-initfunction slotd)
	:type (slot-definition-type slotd)
	:allocation (slot-definition-allocation slotd)
	:initargs (slot-definition-initargs slotd)
	:readers (slot-definition-readers slotd)
	:writers (slot-definition-writers slotd)
	:documentation (slot-definition-documentation slotd)))

(defmethod compute-effective-slot-definition ((class class) name direct-slots)
  (flet ((direct-to-effective (old-slot)
	   (if (consp old-slot)
	       (copy-list old-slot)
	       (let ((initargs (slot-definition-to-list old-slot)))
		 (apply #'make-instance
			(apply #'effective-slot-definition-class class initargs)
			initargs))))
	 (combine-slotds (new-slotd old-slotd)
	   (let* ((new-type (slot-definition-type new-slotd))
		  (old-type (slot-definition-type old-slotd)))
	     (setf (slot-definition-initargs new-slotd)
		   (union (slot-definition-initargs new-slotd)
			    (slot-definition-initargs old-slotd)))
	     (when (eq (slot-definition-initform new-slotd) '+INITFORM-UNSUPPLIED+)
	       (setf (slot-definition-initform new-slotd) (slot-definition-initform old-slotd)))
	     (setf (slot-definition-type new-slotd)
		   ;; FIXME! we should be more smart then this:
		   (cond ((subtypep new-type old-type) new-type)
			 ((subtypep old-type new-type) old-type)
			 (T `(and ,new-type ,old-type))))
	     new-slotd)))
    (reduce #'combine-slotds (rest direct-slots)
	    :initial-value (direct-to-effective (first direct-slots)))))

(defmethod compute-default-initargs ((class class))
  (let ((all-initargs (mapappend #'class-direct-default-initargs
				 (class-precedence-list class))))
    ;; We have to use this trick because REMOVE-DUPLICATES on
    ;; ((:foo x) (:faa y) (:foo z)) would produce ((:faa y) (:foo z))
    ;; and we want ((:foo x) (:faa y))
    (nreverse (remove-duplicates (reverse all-initargs) :key #'first))))

;;; ======================================================================
;;; STANDARD-CLASS specializations
;;;
;;; IMPORTANT: The following implementation of ENSURE-CLASS-USING-CLASS is
;;; shared by the metaclasses STANDARD-CLASS and STRUCTURE-CLASS.
;;;
(defmethod ensure-class-using-class ((class class) name &rest rest
				     &key direct-slots direct-default-initargs)
  (multiple-value-bind (metaclass direct-superclasses options)
      (apply #'help-ensure-class rest)
    (cond ((forward-referenced-class-p class)
	   (change-class class metaclass))
	  ((not (eq (class-of class) metaclass))
	   (error "When redefining a class, the metaclass can not change.")))
    (apply #'reinitialize-instance class :name name options)))

(defun coerce-to-class (class-or-symbol &optional (fail nil))
  (cond ((si:instancep class-or-symbol) class-or-symbol)
	((not (symbolp class-or-symbol))
	 (error "~a is not a valid class specifier." class-or-symbol))
	((find-class class-or-symbol fail))
	(t
	 (warn "Class ~A has been forward referenced." class-or-symbol)
	 (ensure-class class-or-symbol
		       :metaclass 'forward-referenced-class
		       :direct-superclasses (list (find-class 'standard-object))
		       :direct-slots '()))))

(defun help-ensure-class (&rest options
			  &key (metaclass 'standard-class) direct-superclasses
			  &allow-other-keys)
  (remf options :metaclass)
  (remf options :direct-superclasses)
  (setf metaclass (coerce-to-class metaclass t)
	direct-superclasses (mapcar #'coerce-to-class direct-superclasses))
  (values metaclass direct-superclasses
	  (list* :direct-superclasses direct-superclasses options)))

;;; ----------------------------------------------------------------------
;;; Around methods for COMPUTE-SLOTS which assign locations to each slot.
;;;

(defun class-compute-slots (class slots)
  (let ((local-index -1))
    (declare (fixnum local-index))
    (dolist (slotd slots)
      (when (eq (slot-definition-allocation slotd) :instance)
	(setf (slot-definition-location slotd) (incf local-index))))
    slots))

(defmethod compute-slots :around ((class class))
  (class-compute-slots class (call-next-method)))

(defun std-class-compute-slots (class slots)
  (declare (si::c-local))
  (let* ((direct-slots (class-direct-slots class)))
    (dolist (slotd slots)
      (let* ((name (slot-definition-name slotd))
	     (allocation (slot-definition-allocation slotd)))
	(cond ((not (eq (slot-definition-allocation slotd) :class)))
	      ((find name direct-slots :key #'slot-definition-name) ; new shared slot
	       (setf (slot-definition-location slotd) (list (unbound))))
	      (t			; inherited shared slot
	       (dolist (c (class-precedence-list class))
		 (unless (eql c class)
		   (let ((other (find (slot-definition-name slotd)
				      (class-slots c)
				      :key #'slot-definition-name)))
		     (when (and other
				(eq (slot-definition-allocation other) allocation)
				(setf (slot-definition-location slotd)
				      (slot-definition-location other)))
		       (return)))))))))
    slots))

(defmethod compute-slots :around ((class standard-class))
  (std-class-compute-slots class (call-next-method)))

;;; ----------------------------------------------------------------------
;;; Optional accessors

(defun std-class-generate-accessors (standard-class)
  (declare (si::c-local))
  ;;
  ;; The accessors are closures, which are generated every time the
  ;; slots of the class change. The accessors are safe: they check that
  ;; the slot is bound after retreiving the value, and they may take
  ;; the liberty of using SI:INSTANCE-REF because they know the class of
  ;; the instance.
  ;;
  (dolist (slotd (class-slots standard-class))
    (let* ((slot-name (slot-definition-name slotd))
	   (index (slot-definition-location slotd))
	   reader setter)
      (declare (fixnum index))
      (if (and (eql (slot-definition-allocation slotd) :instance)
	       (si:fixnump index)
	       (slot-value standard-class 'optimize-slot-access))
	  (setf reader #'(lambda (self)
			   (let ((value (si:instance-ref self index)))
			     (if (si:sl-boundp value)
				 value
				 (values (slot-unbound (class-of self) self slot-name)))))
		setter #'(lambda (value self)
			   (si:instance-set self index value)))
	  (setf reader #'(lambda (self)
			   (slot-value-using-class (si:instance-class self)
						   self slotd))
		setter #'(lambda (value self)
			   (setf (slot-value-using-class (si:instance-class self)
							 self slotd) value))))
      (dolist (fname (slot-definition-readers slotd))
	(install-method fname nil `(,standard-class) '(self) nil nil
			reader))
      (dolist (fname (slot-definition-writers slotd))
	(install-method fname nil `(nil ,standard-class) '(value self)
			nil nil setter)))))

;;; ======================================================================
;;; STANDARD-OBJECT
;;;
;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(defmethod describe-object ((obj standard-object) (stream t))
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 slotname has-shared-slots)
    (format stream "~%~S is an instance of class ~A"
	    obj (class-name class))
    (when slotds
      ;; print instance slots
      (format stream "~%it has the following instance slots")
      (dolist (slot slotds)
	(setq slotname (slot-definition-name slot))
	(case (slot-definition-allocation slot)
	  (:INSTANCE
	   (format stream "~%~A:~24,8T~A"
		   slotname
		   (if (slot-boundp obj slotname)
		       (slot-value obj slotname) "Unbound")))
	  ;; :CLASS
	  (T (setq has-shared-slots t))))
      (when has-shared-slots
	;; print class slots
	(format stream "~%it has the following class slots")
	(dolist (slot slotds)
	  (setq slotname (slot-definition-name slot))
	  (unless (eq (slot-definition-allocation slot) :INSTANCE)
	    (format stream "~%~A:~24,8T~A"
		    slotname
		    (if (slot-boundp obj slotname)
			(slot-value obj slotname) "Unbound")))))))
  obj)

;;; ----------------------------------------------------------------------
;;; CHECK INITARGS
;;;
;;; There are different sets of initialization arguments. First we have
;;; those coming from the :INITARG option in the slots. Then we have
;;; all declared initargs which are keyword arguments to methods defined
;;; on SHARED-INITIALIZE, REINITIALIZE-INSTANCE, etc. (See ANSI 7.1.2)
;;;

(defun valid-keywords-from-methods (methods)
  (declare (si::c-local))
  ;; Given a list of methods, build up the list of valid keyword arguments
  (do ((m methods (rest m))
       (keys '()))
      ((null m)
       (values keys nil))
    (multiple-value-bind (reqs opts rest key-flag keywords allow-other-keys)
	(si::process-lambda-list (method-lambda-list (first m)) t)
      (when allow-other-keys
	(return (values nil t)))
      (do ((k (rest keywords) (cddddr k)))
	  ((null k))
	(push (first k) keys)))))

(defun check-initargs (class initargs &optional methods
		       (slots (class-slots class)))
  ;; First get all initiargs which have been declared in the given
  ;; methods, then check the list of initargs declared in the slots
  ;; of the class.
  (multiple-value-bind (method-initargs allow-other-keys)
      (valid-keywords-from-methods methods)
    (when allow-other-keys
      (return-from check-initargs))
    (do* ((name-loc initargs (cddr name-loc))
	  (allow-other-keys nil)
	  (allow-other-keys-found nil)
	  (unknown-key nil))
	 ((null name-loc)
	  (when (and (not allow-other-keys) unknown-key)
	    (simple-program-error "Unknown initialization option ~S for class ~A"
				  unknown-key class)))
      (let ((name (first name-loc)))
	(cond ((null (cdr name-loc))
	       (simple-program-error "No value supplied for the init-name ~S." name))
	      ;; This check must be here, because :ALLOW-OTHER-KEYS is a valid
	      ;; slot-initarg.
	      ((and (eql name :ALLOW-OTHER-KEYS)
		    (not allow-other-keys-found))
	       (setf allow-other-keys (second name-loc)
		     allow-other-keys-found t))
	      ;; The initialization argument has been declared in some method
	      ((member name method-initargs))
	      ;; Check if the arguments is associated with a slot
	      ((find name slots :test #'member :key #'slot-definition-initargs))
	      (t
	       (setf unknown-key name)))))))

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod describe-object ((obj standard-class) (stream t))
  (let ((slotds (class-slots (si:instance-class obj))))
    (format t "~%~A is an instance of class ~A"
	    obj (class-name (si:instance-class obj)))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i)))
	((null scan))
      (declare (fixnum i))
      (print (slot-definition-name (car scan))) (princ ":	")
      (case (slot-definition-name (car scan))
	    ((SUPERIORS INFERIORS PRECEDENCE-LIST)
	     (princ "(")
	     (do* ((scan (si:instance-ref obj i) (cdr scan))
		   (e (car scan) (car scan)))
		  ((null scan))
		  (prin1 (class-name e))
		  (when (cdr scan) (princ " ")))
	     (princ ")"))
	    (otherwise (prin1 (si:instance-ref obj i))))))
  obj)
