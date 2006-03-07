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
  (let ((class (si:allocate-raw-instance nil metaclass #.(length +standard-class-slots+))))
    (unless metaclass
      (si:instance-class-set class class))
    (setf (class-id                  class) name
	  (class-direct-superclasses class) nil
	  (class-direct-subclasses   class) nil
	  (class-slots               class) nil
	  (class-direct-slots        class) nil
	  (class-direct-default-initargs class) nil
	  (class-default-initargs    class) nil
	  (class-precedence-list     class) nil
	  (class-finalized-p         class) t
	  (find-class name) class)
    (unless (eq name 'T)
      (setf (slot-index-table          class) (make-hash-table :size 2)
	    (class-shared-slots        class) nil))
    class))

;; 1) Create the classes
;;
;; Notice that, due to circularity in the definition, STANDARD-CLASS has
;; itself as metaclass. MAKE-EMPTY-CLASS takes care of that.
;;
(let* ((standard-class (make-empty-standard-class 'STANDARD-CLASS nil))
       (standard-object (make-empty-standard-class 'STANDARD-OBJECT standard-class))
       (the-class (make-empty-standard-class 'CLASS standard-class))
       (the-t (make-empty-standard-class 'T the-class))
       ;; It does not matter that we pass NIL instead of a class object,
       ;; because CANONICAL-SLOT-TO-DIRECT-SLOT will make simple slots.
       (class-slots (loop for s in (parse-slots '#.+class-slots+)
			  collect (canonical-slot-to-direct-slot nil s)))
       (standard-slots (loop for s in (parse-slots '#.+standard-class-slots+)
			     collect (canonical-slot-to-direct-slot nil s)))
       (hash-table (make-hash-table :size 24)))

  ;; 2) STANDARD-CLASS and CLASS are the only classes with slots. Create a
  ;; hash table for them, so that SLOT-VALUE works. Notice that we
  ;; make a intentional mistake: CLASS and STANDARD-CLASS share the same
  ;; hashtable!!
  (do* ((i 0 (1+ i))
	(slots standard-slots (cdr slots)))
       ((endp slots))
    (setf (gethash (caar slots) hash-table) i))
  (setf (class-slots               the-class) class-slots
	(slot-index-table          the-class) hash-table
	(class-direct-slots        the-class) class-slots
	(class-slots               standard-class) standard-slots
	(slot-index-table          standard-class) hash-table
	(class-direct-slots        standard-class) (set-difference standard-slots class-slots))

  ;; 3) Fix the class hierarchy
  (setf (class-direct-superclasses the-t) nil
	(class-direct-subclasses the-t) (list standard-object)
	(class-direct-superclasses standard-object) (list the-t)
	(class-direct-subclasses standard-object) (list the-class)
	(class-direct-superclasses the-class) (list standard-object)
	(class-direct-subclasses the-class) (list standard-class)
	(class-direct-superclasses standard-class) (list the-class))

  (si::instance-sig-set the-class)
  (si::instance-sig-set standard-class)
  (si::instance-sig-set standard-object)
  (si::instance-sig-set the-t)

  ;; 4) Fix the class precedence list
  (let ((cpl (list standard-class the-class standard-object the-t)))
    (setf (class-precedence-list standard-class) cpl
	  (class-precedence-list the-class) (cdr cpl)
	  (class-precedence-list standard-object) (cddr cpl)
	  (class-precedence-list the-t) nil))

  ;; 5) Generate accessors (In macros.lsp)
)

(defmethod class-prototype ((class class))
  (unless (slot-boundp class 'prototype)
    (setf (slot-value class 'prototype) (allocate-instance class)))
  (slot-value class 'prototype))

;;; ----------------------------------------------------------------------
;;; SLOTS READING AND WRITING
;;;
;;;
;;; 1) Functional interface
;;;

(defun slot-value (self slot-name)
  (slot-value-using-class (class-of self) self slot-name))

(defun slot-boundp (self slot-name)
  (slot-boundp-using-class (class-of self) self slot-name))

(defun (setf slot-value) (value self slot-name)
  (funcall #'(setf slot-value-using-class) value (class-of self) self slot-name))

(defun slot-makunbound (self slot-name)
  (slot-makunbound-using-class (class-of self) self slot-name))

(defun slot-exists-p (self slot-name)
  (slot-exists-p-using-class (class-of self) self slot-name))

;;;
;;; 2) Overloadable methods on which the previous functions are based
;;;

(defmethod slot-value-using-class ((class class) self slot-name)
  (ensure-up-to-date-instance self)
  (let* ((index (position slot-name (class-slots class)
			  :key #'slot-definition-name :test #'eq)))
    (values
     (if index
	 (let ((val (si:instance-ref self (the fixnum index))))
	   (if (si:sl-boundp val)
	       val
	       (slot-unbound (si::instance-class class) class slot-name)))
	 (slot-missing (si:instance-class class) class slot-name 
		       'SLOT-VALUE)))))

(defmethod slot-boundp-using-class ((class class) self slot-name)
  (ensure-up-to-date-instance self)
  (let* ((index (position slot-name (class-slots class)
			  :key #'slot-definition-name :test #'eq)))
    (values
     (if index
	 (si:sl-boundp (si:instance-ref self (the fixnum index)))
	 (slot-missing (si:instance-class class) class slot-name
		       'SLOT-BOUNDP)))))

(defmethod (setf slot-value-using-class) (val (class class) self slot-name)
  (ensure-up-to-date-instance self)
  (let* ((index (position slot-name (class-slots class)
			  :key #'slot-definition-name :test #'eq)))
    (if index
	(si:instance-set self (the fixnum index) val)
	(slot-missing (si:instance-class self) self slot-name
		      'SETF val)))
  val)

(defmethod slot-exists-p-using-class ((class class) self slot-name)
  (ensure-up-to-date-instance self)
  (and (position slot-name (class-slots class) :key #'slot-definition-name :test #'eq)
       t))

;;;
;;; 3) Error messages related to slot access
;;;

(defmethod slot-missing ((class t) object slot-name operation 
			 &optional new-value)
  (declare (ignore operation new-value))
  (error "~A is not a slot of ~A" slot-name object))

(defmethod slot-unbound ((class t) object slot-name)
  (error 'unbound-slot :instance object :name slot-name))

;;;
;;; For the next accessor we define a method.
;;;

(defmethod class-name ((class class))
  (class-id class))

(defmethod (setf class-name) (new-value (class class))
  (setf (class-id class) new-value))
