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

;;; bootstrap order:
(defmethod (setf slot-value) (val (instance standard-object) slot-name)
  (standard-instance-set val instance slot-name))

;;; ----------------------------------------------------------------------

(eval-when (compile load eval)
  (defun create-standard-class
      (name superclasses-names
	    direct-slots		; both instance + shared in this class
	    all-slots
	    default-initargs
	    documentation)
    (let* ((metaclass (find-class 'STANDARD-CLASS))
	   (existing (find-class name nil))
	   (superclasses (mapcar #'find-class superclasses-names))
	   (cpl (compute-class-precedence-list name superclasses)))

      (flet ((unchanged-class ()
	       (declare (inline si:instance-class))
	       (and existing
		    (eq metaclass (si:instance-class existing))
		    (equal (or superclasses-names '(STANDARD-OBJECT))
					; i.e. class-default-direct-superclasses
			   (mapcar #'(lambda (x) (class-name x))
				   (class-superiors existing)))
		    (equal direct-slots (slot-value existing 'DIRECT-SLOTS))
		    (equal all-slots (slot-value existing 'SLOTS))
		    (equal default-initargs (default-initargs-of existing))
		    (prog2 (setf (slot-value existing 'DOCUMENTATION)
				 documentation)
			t))))

	(if (unchanged-class)
	    existing
	    (let ((new-class
		   (make-instance
		    metaclass
		    :name name
		    :direct-superclasses superclasses
		    :slots all-slots

		    ;; The following slots are defined in standard-class.
		    ;; initialize-instance takes care of them
		    :direct-slots direct-slots
		    :class-precedence-list cpl
		    :default-initargs
		    (collect-default-initargs cpl default-initargs)
		    :documentation documentation)))
	      (when existing
		(redefine-class existing new-class superclasses-names
				(class-inferiors existing))) ; Beppe
	      new-class)))))
  ;;; Bootstrap versions.
  (defun redefine-class (class new-class superclasses-names inferiors)
    (declare (ignore superclasses-names inferiors))
    (format t "~%Redefinition of class ~A." (class-name class))
    new-class))

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS
;;; ----------------------------------------------------------------------

(defclass standard-class (class)
  ;; class-precedence-list must be in the same position as in structure-class
  ((precedence-list :initarg :class-precedence-list)
   slot-index-table
   ;; associates names to slot index.
   ;; For shared slots it contains (class . index) pairs for indexing into
   ;; the shared-slots of class
   ;; Be careful when changing the position of this slot!
   (direct-slots :initarg :direct-slots) ; instance + shared in this class
   (shared-slots :initarg :shared-slots	; vector of shared slots
		 :initform nil
		 :accessor class-shared-slots)
   (instance-slot-count :accessor class-instance-slot-count)
   (default-initargs :initarg :default-initargs :reader default-initargs-of)
   (documentation :initarg :documentation :accessor documentation-of)
   (forward)				; forwarding pointer to redefined class
   )
  (:metaclass class))

;;; ----------------------------------------------------------------------
;;; Standard-object
;;; ----------------------------------------------------------------------

;;; Standard-object has no slots and inherits only from t:
;;; (defclass standard-object (t) ())

(eval-when
 (compile load eval)
 (make-instance (find-class 'STANDARD-CLASS)
		:NAME 'STANDARD-OBJECT
		:DIRECT-SUPERCLASSES (list (find-class 'T))
		:SLOTS ()
		:CLASS-PRECEDENCE-LIST (list (find-class 'T))
		:DIRECT-SLOTS ()
		:DEFAULT-INITARGS ()
		:DOCUMENTATION "The root of inheritance for objects"))

#+PDE
(si:record-source-pathname 'STANDARD-OBJECT 'DEFCLASS)

(defmethod slot-value ((object standard-object) slot-name)
  (multiple-value-bind (val condition)
    (standard-instance-get object slot-name)
    (ecase condition
      (:VALUE val)
      (:UNBOUND	(slot-unbound (si:instance-class object) object slot-name))
      (:MISSING (slot-missing (si:instance-class object) object slot-name
			      'SLOT-VALUE))
      )))

(defmethod slot-boundp ((instance standard-object) slot-name)
  (multiple-value-bind (val condition)
      (standard-instance-get instance slot-name)
    (declare (ignore val))
    (ecase condition
      (:VALUE t)
      (:UNBOUND nil)
      (:MISSING (slot-missing (si:instance-class instance) instance slot-name
			      'SLOT-BOUNDP))
      )))
      
(defmethod slot-exists-p ((instance standard-object) slot-name)
  (let ((class (si:instance-class instance)))
    (declare (type standard-class class))
    (slot-index slot-name (slot-index-table class))))

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
		      'SLOT-MAKUNBOUND))))

(defmethod shared-initialize ((instance standard-object) 
			      slot-names &rest initargs)
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
  (let* ((class (si:instance-class instance)))
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
			(member slot-name slot-names :test #'eq))
		    (not (slot-boundp instance slot-name)))
	   (let ((initform (slotd-initform slotd)))
	     (unless (eq initform 'INITFORM-UNSUPPLIED)
	       (setf (slot-value instance slot-name) (eval initform)))))))))
  instance)

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
    (let* ((class (si:instance-class instance)))
      (flet ((search-allow-other-keys (initargs-list)
	       (do ((arg-list initargs-list (cddr arg-list)))
		   ((null arg-list) nil)
		 (when (eq (first arg-list) ':ALLOW-OTHER-KEYS)
		   (return (second arg-list))))))
	(unless (search-allow-other-keys initargs)
	  (check-initargs class initargs)))
      (apply #'shared-initialize instance nil initargs)
      instance))

(defmethod change-class ((instance standard-object) (new-class standard-class))
    (let* ((old-class (si:instance-class instance))
	   (old-slotds (class-slots old-class))
	   (new-slotds (class-slots new-class)))

      ;; "The values of local slots specified by both the class Cto and
      ;; Cfrom are retained.  If such a local slot was unbound, it remains
      ;; unbound."
      ;; "The values of slots specified as shared in the class Cfrom and
      ;; as local in the class Cto are retained."
      (let* ((new-i 0)
	     (old-i 0)
	     retained-correspondance)
	(declare (fixnum new-i))
	(dolist (new-slot new-slotds)
	  (setq old-i (position (slotd-name new-slot) old-slotds
				:key #'slotd-name :test #'eq))
	  (when old-i
	    (push (cons new-i old-i) retained-correspondance))
	  (incf new-i))
	(si:change-instance instance new-class
			    (class-instance-slot-count new-class)
			    (nreverse retained-correspondance)))

      ;; Compute the newly added slots.  The spec defines
      ;; newly added slots as "those local slots for which no slot of
      ;; the same name exists in the previous class."
      (let (added-slots)
	(dolist (slotd new-slotds)
	  (if (and (not (member slotd old-slotds :key #'slotd-name :test #'eq))
		   (eq (slotd-allocation slotd) ':INSTANCE))
	      (push (slotd-name slotd) added-slots)))
	(apply #'shared-initialize instance added-slots)))

  instance)


(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (declare (ignore discarded-slots property-list))
  ;; ***
  ;; *** Later we need to do initarg checking here.
  ;; ***
  (apply #'shared-initialize instance added-slots initargs))

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
;;; Now we can fix inheritance for standard-class:

(eval-when (compile load eval)
	   (setf (class-superiors (find-class 'STANDARD-CLASS))
		 (list (find-class 'CLASS)
		       (find-class 'STANDARD-OBJECT))))

;;; ----------------------------------------------------------------------
;;; default-initargs

(defmethod default-initargs ((class t) initargs)
  initargs)

(defmethod default-initargs ((class standard-class) initargs)
  (do ((scan (reverse (default-initargs-of class)) (cddr scan))
       (defaults))
      ((null scan) (nconc initargs (nreverse defaults)))
    (unless (do ((iscan initargs (cddr iscan)))
		((null iscan) nil)
	      (when (eq (first iscan) (second scan)) (return t)))
      (setq defaults (nconc defaults (list (first scan) (second scan)))))))

;;; ----------------------------------------------------------------------
;;; check-initargs

(defun check-initargs (class initargs)
  (declare (si::c-local))
  ;; scan initarg list 
  (do* ((name-loc initargs (cddr name-loc))
	(name (first name-loc) (first name-loc)))
       ((null name-loc) class)
    (if (null (cdr name-loc))
	(error "No value supplied for the init-name ~S." name)
	(unless
	    (or 
	     ;; check if the arguments is associated with a slot
	     (do ((scan-slot (class-slots class) (cdr scan-slot)))
		 ((null scan-slot) ())
	       (when (member name (slotd-initargs (first scan-slot)))
		 (return t)))
	     #| modify this
		;; check if the argument is associated with a 
		;; initialize-instance or allocate-instance method
		;; inspecting all the keywords of the applicable methods
		(let ((cpl (cons class (slot-value class
                                        'PRECEDENCE-LIST)))
		      cpl-names)
		  ; convert cpl in the list of the names of the classes
		  (do ((scan-cpl cpl (cdr scan-cpl)))
		      ((null scan-cpl) (setq cpl-names (nreverse cpl-names)))
		      (push (class-name (first scan-cpl)) cpl-names))
		  (dolist (scan cpl-names)
			  (when (member name
					(gethash scan *method-key-hash-table*))
				(return t))))
|#
	     )
	  ;; signal error
	  (error "Unknown initialization option ~A for class ~A"
		 name class)))))

;;; ----------------------------------------------------------------------
;;; Basic access to instances

(defun standard-instance-get (instance slot-name)
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
  (let* ((class (si:instance-class instance))
	 (index (gethash slot-name (slot-index-table class))))
    (declare (type standard-class class))
    (if index
	(if (atom index)
	    (si:instance-set instance (the fixnum index) val)
	    ;; else it is a shared slot
	    (setf (svref (class-shared-slots (car index)) (cdr index)) val))
	(slot-missing (si:instance-class instance) instance slot-name
		      'SLOT-VALUE))))

(defun general-instance-get (instance slot-name)
  (let* ((class (si:instance-class instance))
         (index (position slot-name (class-slots class)
                          :key #'slotd-name :test #'eq)))
    (if index
	(let ((val (si:instance-ref instance (the fixnum index))))
	  (if (si:sl-boundp val)
	      (values val :VALUE)
	      (values nil :UNBOUND)))
	(values nil :MISSING))))

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

(defmethod initialize-instance ((class standard-class)
;;;				&rest initargs 
				&key name direct-superclasses 
				&allow-other-keys)

  (call-next-method)			; from class T

  (let* ((superclasses 
	  (class-default-direct-superclasses class direct-superclasses))
	 (cpl (if (and (cdr superclasses)
		       (eq (class-name (car superclasses)) 'STANDARD-OBJECT))
		  ;; it is a class inheriting from a structure
		  ;; so standard-object must be the first in cpl
		  (cons (car superclasses)
			(remove (find-class 'STANDARD-OBJECT)
				(compute-class-precedence-list 
				 name
				 (cdr superclasses))))
		  ;; else
		  (compute-class-precedence-list name superclasses)))
	 (slots (class-slots class))
	 (class-direct-slots (slot-value class 'DIRECT-SLOTS))
	 (table (make-hash-table :size (max 32 (* 2 (length slots)))
				 :test #'eq))
	 (local-index -1)
	 (shared-index -1))
    (declare (fixnum local-index shared-index))

    (setf (slot-value class 'SUPERIORS) superclasses)
    (setf (slot-value class 'PRECEDENCE-LIST) cpl)
    (setf (slot-index-table class) table)
    (dolist (slot slots)
      (let* ((name (slotd-name slot))
	     (allocation (slotd-allocation slot))
	     location)
	(cond ((eq allocation :INSTANCE) ; local slot
	       (setq location (incf local-index)))
	      ((member slot class-direct-slots) ; new shared slot
	       (setq location (cons class (incf shared-index))))
	      (t			; inherited shared slot
	       (dolist (c cpl)
		 (when (and
			(typep c 'STANDARD-CLASS)
			(setq location
			      (gethash name (slot-value c 'SLOT-INDEX-TABLE))))
		   (return)))))
	(setf (gethash name table) location)))
    (setf (class-instance-slot-count class) (1+ local-index))
    (when (plusp (incf shared-index))
      (setf (class-shared-slots class)
	    (make-array shared-index :initial-element (unbound)))))

  class)

;;; Bootstrap problem: early version of add-method does not do this,
;;; but here initialize-instance has already been called, so we do it
;;; explicitely
(clrhash (si:gfun-method-ht (symbol-function 'initialize-instance)))

(defmethod class-default-direct-superclasses ((class standard-class)
					      supplied-superclasses)
  (let ((default-superclasses supplied-superclasses))
    (if supplied-superclasses
	(progn
	  ;; check if the class inherits from a structure. 
	  ;; A structure can be the first one in the list of superclasses.
	  (dolist (super (cdr supplied-superclasses))
	    (when (typep super 'STRUCTURE)
	      (error
	       "The standard class ~A can have the structure class ~A only~
		as first superclass in the list" class super)))
	  
	  ;; default inheritance for CLOS classes that are instances
	  ;; of standard-class is standard-object.
	  (if (eq (class-name (class-of (first supplied-superclasses)))
		  'STRUCTURE-CLASS)
	      (push (find-class 'STANDARD-OBJECT) default-superclasses)
	  
	      ;; else
	      (unless (or (eq (class-name class) 'STANDARD-OBJECT)
			  (eq (class-name class) 'STRUCTURE-OBJECT)
			  (some #'(lambda (x) (subtypep (class-name x)
							'STANDARD-OBJECT))
				supplied-superclasses))
		(setf default-superclasses
		      (nreverse (cons (find-class 'STANDARD-OBJECT)
				      (nreverse supplied-superclasses)))))))
    ;; else
      (setf default-superclasses (list (find-class 'STANDARD-OBJECT))))
    default-superclasses))

(defmethod make-instance ((class standard-class) &rest initargs)
  (setq initargs (default-initargs class initargs))
  (flet ((search-allow-other-keys (initargs-list)
	    (do ((arg-list initargs-list (cddr arg-list)))
		((null arg-list))
		(when (eq (first arg-list) ':ALLOW-OTHER-KEYS)
		      (return (second arg-list))))))
	(if (not (search-allow-other-keys initargs))
	    (check-initargs class initargs)))
  (let ((instance
	 (si:allocate-instance class (class-instance-slot-count class))))
    (apply #'initialize-instance instance initargs)
    instance))

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


;;; ----------------------------------------------------------------------
;;;                                                          documentation

#|
(defmethod documentation ((obj standard-class) &optional doc-type)
  (declare (ignore doc-type))
  (documentation-of obj))

(defmethod (setf documentation)
  ((s string) (obj standard-class) &optional doc-type)
  (declare (ignore doc-type))
  (setf (documentation-of obj) s))
|#
;;; ----------------------------------------------------------------------
;;; Generic Function
;;; ----------------------------------------------------------------------

(defclass generic-function () ())


;;; ----------------------------------------------------------------------
;;; Standard Generic Function
;;; ----------------------------------------------------------------------

(defclass standard-generic-function (generic-function)
  ((lambda-list :initarg :lambda-list :accessor lambda-list)
   (argument-precedence-order 
    :initarg :argument-precedence-order
    :accessor generic-function-argument-precedence-order)
   (method-combination 
    :initarg :method-combination 
    :accessor generic-function-method-combination
    )
   (method-combination-arguments
    :initarg :method-combination-arguments
    :accessor generic-function-method-combination-arguments
    )
   (method-class :initarg :method-class)
   (documentation :initarg :documentation 
;                 :accessor documentation
		  )
   (gfun :initarg :gfun :accessor gfun :initform nil)
   (methods :initform nil :accessor methods))) ; 7th slot as in kernel.lsp

;;;----------------------------------------------------------------------
