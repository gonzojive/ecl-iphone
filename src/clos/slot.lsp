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
;;; SLOT descriptors
;;;

(defconstant +initform-unsupplied+ '+initform-unsupplied+)

(defvar *slot-initform-lambdas* nil)

(defstruct (slotd (:type list))
  name initargs initform accessors readers writers allocation type
  documentation)

(defun PARSE-SLOT (slot)
  (declare (si::c-local))
  (let*((name nil)
        (initargs nil)
        (initform '+INITFORM-UNSUPPLIED+)	; default
        (accessors ())
        (readers ())
        (writers ())
        (allocation ':INSTANCE)
        (type 'T)				; default
	(documentation nil)
	(slotd (make-slotd)))

    (cond ((symbolp slot)     (setq name slot))
          ((null (cdr slot))  (setq name (car slot)))
          (t
           (setq name (car slot))
           (let*((options (cdr slot))
		 (option nil)
		 (value nil))
             (loop (when (null options) (return t))
		   (setq option (pop options))
		   (unless (legal-slot-option-p option)
		     (si::simple-program-error
		      "In the slot description ~S,~%the option ~S is not legal"
		      slot option))
		   (if (endp options)
		     (si::simple-program-error
		      "In the slot description ~S,~%the option ~S is missing an argument"
		      slot option)
		     (setq value (pop options)))
		   (when (and (member option '(:allocation :initform :type :documentation))
			      (getf options option))
		     (si::simple-program-error
		      "In the slot descrition ~S,~%the option ~S is duplicated"
		      slot option))
                   (case option
                     (:initarg    (push value initargs))
                     (:initform   (setq initform value))
                     (:accessor   (push value accessors))
                     (:reader     (push value readers))
                     (:writer     (push value writers))
                     (:allocation (setq allocation value))
                     (:type       (setq type value))
                     (:documentation     (push value documentation)))))))

    (setf (slotd-name slotd)       name
	  (slotd-initargs slotd)   initargs
	  (slotd-initform slotd)   initform
	  (slotd-accessors slotd)  accessors
	  (slotd-readers slotd)    readers
	  (slotd-writers slotd)    writers
	  (slotd-allocation slotd) allocation
	  (slotd-type slotd)       type
	  (slotd-documentation slotd)    documentation)

    slotd))

(defun PARSE-SLOTS (slots)
  (do ((scan slots (cdr scan))
       (collect))
      ((null scan) (nreverse collect))
    (let* ((slotd (parse-slot (first scan)))
	   (name (slotd-name slotd)))
      (when (find name collect :key #'slotd-name)
	(si::simple-program-error
	 "A definition for the slot ~S appeared twice in a DEFCLASS form"
	 name))
      (push slotd collect))))

(defun LEGAL-SLOT-OPTION-P (option)
  (declare (si::c-local))
  (member option
	  '(:accessor :reader :writer :allocation :initarg :initform :type
		      :documentation)))

;;; ----------------------------------------------------------------------
