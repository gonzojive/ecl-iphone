;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

;;; The mechanism for updating classes.

;;; ----------------------------------------------------------------------
;;; Invalid Class
;;; ----------------------------------------------------------------------

(defclass invalid () ())

(defmethod OPTIMIZE-SLOT-VALUE ((class class) form) form)
    
(defmethod OPTIMIZE-SET-SLOT-VALUE ((class class) form) form)

(defmethod slot-value ((object invalid) slot-name)
  ;; first update the instance
  (update-instance object)
  ;; now access the slot
  (slot-value object slot-name))

(defmethod (setf slot-value) (val (object invalid) slot-name)
  ;; first update the instance
  (update-instance object)
  ;; now modify the slot
  (setf (slot-value object slot-name) val))

;;; ----------------------------------------------------------------------

(defun update-instance (instance)
  (let* ((old-class (class-of instance))
	 (new-class (slot-value old-class 'FORWARD))
					; was saved here by redefine-class
	 (old-slots (class-slots old-class))
	 (new-slots (class-slots new-class))
	 discarded-slots
	 added-slots
	 retained-correspondance
	 property-list
	 position)
    ;; dont (declare (fixnum position)) otherwise if position will fail.
    (unless (equal old-slots new-slots)
      (setq discarded-slots
	    (set-difference (mapcar #'slotd-name old-slots)
			    (mapcar #'slotd-name new-slots)))
      ;; compute the property list
      (dolist (slot-name discarded-slots)
	;; can't use slot-value or we loop
	(push (cons slot-name (standard-instance-get instance slot-name))
	      property-list)))

    ;; compute retained local slots and update instance:
    (let*((new-i 0)
	  (old-i 0)
	  (index-table (slot-index-table old-class))
	  name
	  old-slot)
      (declare (fixnum new-i old-i))
      (dolist (new-slot new-slots)
	(setq name (slotd-name new-slot)
	      old-slot (find name old-slots :key #'slotd-name :test #'eq))
	(if old-slot
	    (when (and (eq :INSTANCE (slotd-allocation new-slot))
		       (eq :INSTANCE (slotd-allocation old-slot)))
	      (push (cons new-i (gethash name index-table))
		    retained-correspondance))
	    (push new-slot added-slots))
	(incf new-i))

      (si:change-instance instance new-class
			  (count-instance-slots new-class)
			  (nreverse retained-correspondance)))

    ;; initialize newly added slots
    (update-instance-for-redefined-class instance added-slots
					 discarded-slots property-list)
    ))

(defun remove-optional-slot-accessors (class)
  (let ((class-name (class-name class)))
    (dolist (slotd (class-slots class))
      
      (dolist (accessor (slotd-accessors slotd))
	(let* ((gf-object (symbol-function accessor))
	       (setf-accessor (list 'setf accessor))
	       (setf-gf-object (fdefinition setf-accessor))
	       found)
	  ;; primary reader method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before reader method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after reader method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	  (when (null (generic-function-methods gf-object))
	    (fmakunbound accessor))
	  ;; primary writer method
	  (when (setq found
		      (find-method setf-gf-object nil (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  ;; before writer method
	  (when (setq found
		      (find-method setf-gf-object ':before (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  ;; after writer method
	  (when (setq found
		      (find-method setf-gf-object ':after (list nil class-name) nil))
	    (remove-method setf-gf-object found))
	  (when (null (generic-function-methods gf-object))
	    (fmakunbound setf-accessor))))
      
      ;; remove previous defined reader methods
      (dolist (reader (slotd-readers slotd))
	(let* ((gf-object (symbol-function reader))
	       found)
	  ;; primary method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	(when (null (generic-function-methods gf-object))
	  (fmakunbound reader))))
      
      ;; remove previous defined writer methods
      (dolist (writer (slotd-writers slotd))
	(let* ((gf-object (symbol-function writer))
	       found)
	  ;; primary method
	  (when (setq found
		      (find-method gf-object nil (list class-name) nil))
	    (remove-method gf-object found))
	  ;; before method
	  (when (setq found
		      (find-method gf-object ':before (list class-name) nil))
	    (remove-method gf-object found))
	  ;; after method
	  (when (setq found
		      (find-method gf-object ':after (list class-name) nil))
	    (remove-method gf-object found))
	(when (null (generic-function-methods gf-object))
	  (fmakunbound writer)))))))


;;; ----------------------------------------------------------------------
