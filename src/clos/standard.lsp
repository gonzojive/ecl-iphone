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
  (check-initargs (class-of instance) initargs)
  (apply #'shared-initialize instance 'T initargs))

(defmethod reinitialize-instance ((instance T) &rest initargs)
  (check-initargs (class-of instance) initargs)
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
      (let* ((slot-initargs (slotd-initargs slotd))
	     (slot-name (slotd-name slotd)))
	(or
	 ;; Try to initialize the slot from one of the initargs.
	 (doplist (initarg val)
		  initargs
		  (when (member initarg slot-initargs :test #'eq)
		    (setf (slot-value instance slot-name) val)
		    (return 'T)))
	 ;; Try to initialize the slot from its initform.
	 (when (and slot-names
		    (or (eq slot-names 'T)
			(member slot-name slot-names))
		    (not (slot-boundp instance slot-name)))
	   (let ((initform (slotd-initform slotd)))
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
  (count :instance (class-slots class) :key #'slotd-allocation))

(defmethod allocate-instance ((class class) &key &allow-other-keys)
  ;; FIXME! Inefficient! We should keep a list of dependent classes.
  (unless (class-finalized-p class)
    (finalize-inheritance class))
  (let ((x (si::allocate-raw-instance nil class (count-instance-slots class))))
    (si::instance-sig-set x)
    x))

(defmethod make-instance ((class class) &rest initargs)
  (let ((instance (allocate-instance class)))
    (apply #'initialize-instance instance (add-default-initargs class initargs))
    instance))

(defun add-default-initargs (class initargs)
  (declare (si::c-local))
  ;; Here, for each slot which is not mentioned in the initialization
  ;; arguments, but which has a value associated with :DEFAULT-INITARGS,
  ;; we compute the value and add it to the list of initargs.
  (dolist (scan (class-default-initargs class))
    (let ((initarg (first scan))
	  (value (third scan)))
      (when (eql (si::search-keyword initargs initarg) 'si::failed)
	(setf value (if (functionp value) (funcall value) value)
	      initargs (append initargs (list initarg value))))))
  #+nil
  (dolist (slotd (class-slots class))
    (let ((found nil)
	  (defaults '())
	  (slotd-initargs (slotd-initargs slotd)))
      (dolist (key slotd-initargs)
	(unless (eql (si::search-keyword initargs key) 'si::failed)
	  (setq found t)))
      (unless found
	(dolist (scan (class-default-initargs class))
	  (let ((initarg (first scan))
		(value (third scan)))
	    (when (member initarg slotd-initargs)
	      (setf initargs
		    (list* initarg (if (functionp value) (funcall value) value)
			   initargs))
	      (return)))))))
  initargs)

(defmethod initialize-instance ((class class) &rest initargs
				&key direct-superclasses &allow-other-keys)

  ;; this sets up all the slots of the class
  (call-next-method)

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

(defmethod finalize-inheritance ((class standard-class))
  (call-next-method)
  (std-class-allocate-slots class)
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
	(all-names (nreverse (mapcar #'slotd-name all-slots)))
	(output '())
	(scan all-names (cdr scan)))
       ((endp scan) output)
    (let ((name (first scan)))
      (unless (find name (rest scan))
	(push (compute-effective-slot-definition
	       class name (delete name (reverse all-slots) :key #'slotd-name
				  :test-not #'eq))
	      output)))))

(defmethod compute-effective-slot-definition ((class class) name direct-slots)
  (flet ((combine-slotds (new-slotd old-slotd)
	   (let* ((new-type (slotd-type new-slotd))
		  (old-type (slotd-type old-slotd)))
	     (setf (slotd-initargs new-slotd)
		   (union (slotd-initargs new-slotd)
			  (slotd-initargs old-slotd)))
	     (when (eq (slotd-initform new-slotd) '+INITFORM-UNSUPPLIED+)
	       (setf (slotd-initform new-slotd) (slotd-initform old-slotd)))
	     (setf (slotd-type new-slotd)
		   ;; FIXME! we should be more smart then this:
		   (cond ((subtypep new-type old-type) new-type)
			 ((subtypep old-type new-type) old-type)
			 (T `(and ,new-type ,old-type))))
	     new-slotd)))
    (reduce #'combine-slotds (rest direct-slots)
	    :initial-value (copy-list (first direct-slots)))))

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
				     &key direct-slots direct-default-initargs
				     &allow-other-keys)
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
;;; Slots hashing for standard classes
;;;

(defun std-class-allocate-slots (class)
  (declare (si::c-local))
  (let* ((slots (class-slots class))
	 (direct-slots (class-direct-slots class))
	 (slot-instance-count (count-instance-slots class))
	 (table (make-hash-table :size (max 32 (* 2 slot-instance-count))))
	 (local-index -1)
	 (shared-index -1))
    (declare (fixnum local-index shared-index))
    (dolist (slot slots)
      (let* ((name (slotd-name slot))
	     (allocation (slotd-allocation slot))
	     location)
	(cond ((eq allocation :INSTANCE) ; local slot
	       (setq location (incf local-index)))
	      ((find name direct-slots :key #'slotd-name) ; new shared slot
	       (setq location (cons class (incf shared-index))))
	      (t			; inherited shared slot
	       (dolist (c (class-precedence-list class))
		 (when (and
			(not (eql c class))
			(typep c 'STANDARD-CLASS)
			(setq location
			      (gethash name (slot-value c 'SLOT-INDEX-TABLE))))
		   (return)))))
	(setf (gethash name table) location)))
    (setf (class-shared-slots class)
	  (make-array (1+ shared-index) :initial-element (unbound))
	  (slot-index-table class) table)))

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
  (do* ((slots (class-slots standard-class) (cdr slots))
	(i 0))
    ((endp slots))
    (declare (fixnum i))
    (let* ((slotd (first slots))
	   (accessor (slotd-accessors slotd))
	   (slot-name (slotd-name slotd))
	   (index i)
	   reader setter)
      (declare (fixnum index))
      (if (eql (slotd-allocation slotd) :instance)
	  (setf reader #'(lambda (self)
			   (let ((value (si:instance-ref self index)))
			     (if (si:sl-boundp value)
				 value
				 (values (slot-unbound (class-of self) self slot-name)))))
		setter #'(lambda (value self)
			   (si:instance-set self index value))
		i (1+ i))
	  (setf reader #'(lambda (self)
			   (slot-value self slot-name))
		setter #'(lambda (value self)
			   (setf (slot-value self slot-name) value))))
      (dolist (fname (append (slotd-accessors slotd) (slotd-readers slotd)))
	(install-method fname nil `(,standard-class) '(self) nil nil
			reader))
      (dolist (fname (slotd-accessors slotd))
	(install-method `(setf ,fname) nil `(nil ,standard-class) '(value self)
			nil nil setter))
      (dolist (fname (slotd-writers slotd))
	(install-method fname nil `(nil ,standard-class) '(value self)
			nil nil setter)))))

;;; ======================================================================
;;; STANDARD-OBJECT
;;;
;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(defmethod slot-value ((instance standard-object) slot-name)
  (multiple-value-bind (val condition)
      (standard-instance-get instance slot-name)
    (case condition
      (:VALUE val)
      (:UNBOUND (values (slot-unbound (si:instance-class instance) instance
				      slot-name)))
      (:MISSING (values (slot-missing (si:instance-class instance) instance
				      slot-name 'SLOT-VALUE)))
      )))

(defmethod slot-boundp ((instance standard-object) slot-name)
  (multiple-value-bind (val condition)
      (standard-instance-get instance slot-name)
    (declare (ignore val))
    (case condition
      (:VALUE t)
      (:UNBOUND nil)
      (:MISSING (values (slot-missing (si:instance-class instance) instance
				      slot-name 'SLOT-BOUNDP)))
      )))

(defmethod (setf slot-value) (val (instance standard-object) slot-name)
  (standard-instance-set val instance slot-name))

(defmethod slot-exists-p ((instance standard-object) slot-name)
  (let ((class (si:instance-class instance)))
    (declare (type standard-class class))
    (and (nth-value 0 (gethash slot-name (slot-index-table class) nil))
	 t)))

(defmethod slot-makunbound ((instance standard-object) slot-name)
  (let* ((class (si:instance-class instance))
	 (index (slot-index slot-name (slot-index-table class))))
    (if index
	(if (atom index)
	    (si:sl-makunbound instance (the fixnum index))
	    ;; else it is a shared slot
	    (setf (svref (class-shared-slots (car index)) (cdr index))
		  (unbound)))
	(slot-missing (si:instance-class instance) instance slot-name
		      'SLOT-MAKUNBOUND)))
  instance)

(defmethod describe-object ((obj standard-object))
  (let* ((class (si:instance-class obj))
	 (slotds (class-slots class))
	 slotname has-shared-slots)
    (format t "~%~A is an instance of class ~A"
	    obj (class-name class))
    (when slotds
      ;; print instance slots
      (format t "~%it has the following instance slots")
      (dolist (slot slotds)
	(setq slotname (slotd-name slot))
	(case (slotd-allocation slot)
	  (:INSTANCE
	   (format t "~%~A:~24,8T~A"
		   slotname
		   (if (slot-boundp obj slotname)
		       (slot-value obj slotname) "Unbound")))
	  ;; :CLASS
	  (T (setq has-shared-slots t))))
      (when has-shared-slots
	;; print class slots
	(format t "~%it has the following class slots")
	(dolist (slot slotds)
	  (setq slotname (slotd-name slot))
	  (unless (eq (slotd-allocation slot) :INSTANCE)
	    (format t "~%~A:~24,8T~A"
		    slotname
		    (if (slot-boundp obj slotname)
			(slot-value obj slotname) "Unbound")))))))
  obj)

;;; ----------------------------------------------------------------------
;;; check-initargs

(defun check-initargs (class initargs)
  ;; scan initarg list 
  (do* ((name-loc initargs (cddr name-loc))
	(allow-other-keys nil)
	(allow-other-keys-found nil)
	(unknown-key nil))
       ((null name-loc)
	(when (and (not allow-other-keys) unknown-key)
	  (error "Unknown initialization option ~A for class ~A"
		 unknown-key class))
	initargs)
    (let ((name (first name-loc)))
      (cond ((null (cdr name-loc))
	     (error "No value supplied for the init-name ~S." name))
	    ;; This check must be here, because :ALLOW-OTHER-KEYS is a valid
	    ;; slot-initarg.
	    ((and (eql name :ALLOW-OTHER-KEYS)
		  (not allow-other-keys-found))
	     (setf allow-other-keys (second name-loc)
		   allow-other-keys-found t))
	    (;; check if the arguments is associated with a slot
	     (do ((scan-slot (class-slots class) (cdr scan-slot)))
		 ((null scan-slot) ())
	       (when (member name (slotd-initargs (first scan-slot)))
		 (return t))))
	    (t
	     (setf unknown-key name))))))

;;; ----------------------------------------------------------------------
;;; Basic access to instances

(defun standard-instance-get (instance slot-name)
  (ensure-up-to-date-instance instance)
  (let* ((class (si:instance-class instance))
	 (index (gethash slot-name (slot-index-table class))))
    (declare (type standard-class class))
    (if (null index)
	(values nil :MISSING)
	(let ((val (if (atom index)
		       ;; local slot
		       (si:instance-ref instance (the fixnum index))
		       ;; shared slot
		       (svref (class-shared-slots (car index)) (cdr index)))))
	  (if (si:sl-boundp val)
	      (values val :VALUE)
	      (values nil :UNBOUND))))))

(defun standard-instance-set (val instance slot-name)
  (ensure-up-to-date-instance instance)
  (let* ((class (si:instance-class instance))
	 (index (gethash slot-name (slot-index-table class))))
    (declare (type standard-class class))
    (if index
	(if (atom index)
	    (si:instance-set instance (the fixnum index) val)
	    ;; else it is a shared slot
	    (setf (svref (class-shared-slots (car index)) (cdr index)) val))
	(slot-missing (si:instance-class instance) instance slot-name
		      'SETF val))
    val))

;;; ----------------------------------------------------------------------
;;;                                                             optimizers

(defmethod OPTIMIZE-SLOT-VALUE ((class standard-class) form)
  (let* ((instance (second form))
	 (slot-name (third form)))
    `(standard-instance-access ,instance
			       ',(reduce-constant slot-name) . ,(cdddr form))))

(defmethod OPTIMIZE-SET-SLOT-VALUE ((class standard-class) form)
  (let* ((instance (cadadr form))
	 (slot-name (caddr (second form)))
	 (new-value (third form)))
    `(standard-instance-access ,instance
			       ',(reduce-constant slot-name) ,new-value)))

;;; ----------------------------------------------------------------------
;;; Methods

(defmethod describe-object ((obj standard-class))
  (let ((slotds (class-slots (si:instance-class obj))))
    (format t "~%~A is an instance of class ~A"
	    obj (class-name (si:instance-class obj)))
    (do ((scan slotds (cdr scan))
	 (i 0 (1+ i)))
	((null scan))
      (declare (fixnum i))
      (print (slotd-name (car scan))) (princ ":	")
      (case (slotd-name (car scan))
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
