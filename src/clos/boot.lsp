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
    (setf (class-name                class) name
	  (class-direct-superclasses class) nil
	  (class-direct-subclasses   class) nil
	  (class-slots               class) nil
	  (class-direct-slots        class) nil
	  (class-direct-default-initargs class) nil
	  (class-default-initargs    class) nil
	  (class-precedence-list     class) nil
	  (class-finalized-p         class) t
	  (slot-index-table          class) (make-hash-table :size 2)
	  (class-shared-slots        class) nil
	  (find-class name) class
	  )
    class))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-CLASS takes care of that.
;;
(let* ((standard-class (make-empty-standard-class 'STANDARD-CLASS nil))
       (standard-object (make-empty-standard-class 'STANDARD-OBJECT standard-class))
       (the-class (make-empty-standard-class 'CLASS standard-class))
       (the-t (make-empty-standard-class 'T standard-class))
       (class-slots '#.+class-slots+)
       (standard-slots '#.+standard-class-slots+)
       (hash-table (make-hash-table :size 24)))

  ;; 2) STANDARD-CLASS and CLASS are the only classes with slots. Create a
  ;; hash table for them, so that SLOT-VALUE works. Notice that we
  ;; make a intentional mistake: CLASS and STANDARD-CLASS share the same
  ;; hashtable!!
  (do* ((i 0 (1+ i))
	(slots standard-slots (cdr slots)))
       ((endp slots))
    (setf (gethash (caar slots) hash-table) i))
  (setf (class-slots               the-class) (parse-slots class-slots)
	(slot-index-table          the-class) hash-table
	(class-direct-slots        the-class) (class-slots the-class)
	(class-slots               standard-class) (parse-slots standard-slots)
	(slot-index-table          standard-class) hash-table
	(class-direct-slots        standard-class) (class-slots standard-class))

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

  ;; 5) Generate accessors (In macros.lsp)
)

(defmethod OPTIMIZE-SLOT-VALUE ((class t) form) form)

(defmethod OPTIMIZE-SET-SLOT-VALUE ((class t) form) form)

;;; ----------------------------------------------------------------------
;;; SLOTS READING AND WRITING
;;;

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

    (defmethod slot-missing ((class t) object slot-name operation 
			     &optional new-value)
      (declare (ignore operation new-value))
      (error "~A is not a slot of ~A" slot-name object))

    (defmethod slot-unbound ((class t) object slot-name)
      (error 'slot-unbound :instance object :name slot-name))

;;; ----------------------------------------------------------------------
