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
;;; DEFCLASS

(defun quote-list (x)
  (mapcar #'(lambda (x) (list 'quote x)) x))

(defun direct-slot-make-form (direct-slots)
  (declare (si::c-local))
  (do* ((output nil)
	(constants-list nil)
	(scan (reverse direct-slots) (cdr scan))
	(slotd (first scan) (first scan)))
       ((endp scan)
	(if (null output)
	    (list 'quote constants-list)
	    (cons 'list (append (quote-list constants-list) output))))
    (let ((initform (slotd-initform slotd)))
      (cond ((or (member initform '(NIL T INITFORM-UNSUPPLIED))
		 (typep initform '(or number character string array keyword)))
	     (push slotd constants-list))
	    ((and (consp initform) (eq 'quote (first initform)))
	     (setf (slotd-initform slotd) (second initform))
	     (push slotd constants-list))
	    (t
	     (setq initiform `#'(lambda () ,initform))
	     ;; Quote everything except the initalization form
	     (setq slotd (quote-list slotd))
	     (setf (slotd-initform slotd) `#'(lambda () ,initform))
	     (when constants-list
	       (setq output (append (quote-list constants-list) output))
	       (setq constants-list nil))
	     (push (cons 'list slotd) output))))))

(defun default-initargs-make-form (default-initargs)
  (declare (si::c-local))
  (do* ((must-be-evaluated nil)
	(output-list nil)
	(scan default-initargs (cddr scan))
	(already-supplied '())
	slot-name initform)
       ((endp scan)
	(and output-list `(list ,@(nreverse output-list))))
    (when (endp (cdr scan))
      (si::simple-program-error "Wrong number of elements in :DEFAULT-INITARGS option."))
    (setq slot-name (first scan)
	  initform (second scan))
    (if (member slot-name already-supplied)
      (si::simple-program-error "~S is duplicated in :DEFAULT-INITARGS form ~S"
				slot-name default-initargs)
      (push slot-name already-supplied))
    (setq output-list
	  (list*
	   (if (or (typep initform '(or number character string array keyword))
		   (and (consp initform) (eq 'quote (first initform))))
	       initform
	       `#'(lambda () ,initform))
	   `',slot-name
	   output-list))))

(defmacro DEFCLASS (&rest args)
  (multiple-value-bind (name superclasses slots 
			     metaclass-name default-initargs documentation)
      (parse-defclass args)
    (unless metaclass-name
      (setq metaclass-name 'STANDARD-CLASS)) ; the default for CLOS

    ;; process superclasses
    (dolist (superclass superclasses)
      (unless (legal-class-name-p superclass)
	(error "~A is not a legal superclass name" superclass)))

    ;; process slots
    (let* ((direct-slots (parse-slots slots))
	   (direct-slots-form (direct-slot-make-form direct-slots))
	   (default-initargs-form (default-initargs-make-form default-initargs))
	   (all-slots (collect-all-slots direct-slots name superclasses)))
      ;; at compile time just create the definition
      `(eval-when (compile load eval)
	(progn
	  #+PDE
	  (si:record-source-pathname ',name 'DEFCLASS)
	  (ensure-class
	     ',metaclass-name
	     ',name
	     ',superclasses
	     ,direct-slots-form
	     ,default-initargs-form
	     ',documentation))))))

(defun collect-all-slots (slots name superclasses-names)
  (declare (si::c-local))
  (let* ((superclasses (mapcar #'find-class superclasses-names))
	 (cpl (compute-class-precedence-list name superclasses)))
    (collect-slotds cpl slots)))

;;
;; INV: ENSURE-CLASS should always output the class it creates.
;;
(defun ensure-class (metaclass name superclasses direct-slots
			       default-initargs documentation)
  ;; update slots with inherited information
  (let ((all-slots (collect-all-slots direct-slots name superclasses)))
    (do ((scan direct-slots (cdr scan))
	 (slot))
	((null scan))
      (setq slot (find (slotd-name (first scan)) all-slots :key #'slotd-name))
      (setf (first scan) slot))
    (case metaclass
      (STANDARD-CLASS
       (create-standard-class name superclasses direct-slots all-slots
			      default-initargs documentation))
      (STRUCTURE-CLASS
       (create-structure-class name superclasses direct-slots all-slots
			       default-initargs documentation))
      (T
       (make-instance (find-class metaclass)
		      :NAME name
		      :DIRECT-SUPERCLASSES (mapcar #'find-class superclasses)
		      :DIRECT-SLOTS direct-slots
		      :SLOTS all-slots
		      :DEFAULT-INITARGS default-initargs
		      :DOCUMENTATION documentation)))))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-defclass (args)
  (declare (si::c-local))
  (let* (name superclasses slots options
	 metaclass-name default-initargs documentation)
    (unless args
      (si::simple-program-error "Illegal defclass form: the class name, the superclasses and the slots should always be provided"))
    (setq name (pop args))
    (unless args
      (si::simple-program-error "Illegal defclass form: the class name, the superclasses list and the slot specifier list should always be provided"))
    (unless (listp (first args))
      (si::simple-program-error "Illegal defclass form: the superclasses should be a list"))
    (setq superclasses (pop args))
    (unless args
      (si::simple-program-error "Illegal defclass form: the class name, the superclasses list and the slot specifier list should always be provided"))
    (unless (listp (first args))
      (si::simple-program-error "Illegal defclass form: the slots should be a list"))
    (setq slots (pop args))
    (setq options args)
    (unless (legal-class-name-p name)
      (si::simple-program-error "Illegal defclass form: the class name should be a symbol"))
    ;; process options
    (dolist (option options)
      (case (first option)
	(:metaclass
	 (if metaclass-name
	     (si::simple-program-error
	      "Option :metaclass specified more than once for class ~A" 
	      name)
	   ;; else
	   (setq metaclass-name (second option))))
	(:default-initargs
	  (if default-initargs
	      (si::simple-program-error
	       "Option :default-initargs specified more than once for class ~A"
	       name)
	      (setq default-initargs (cdr option))))
	(:documentation
	  (if documentation
	      (si::simple-program-error
	       "Option :documentation specified more than once for class ~A"
	       name)
	      (setq documentation (second option))))  
	(otherwise
	 (si::simple-program-error "~S is not a legal class-option."
				   (first option)))))
    (values name superclasses slots 
	    metaclass-name default-initargs documentation)))


;;; ----------------------------------------------------------------------
;;; SLOTS

(defun collect-slotds (classes slots)
  (flet ((combine-slotds (new-slotd old-slotd)
	   (let* ((new-type (slotd-type new-slotd))
		  (old-type (slotd-type old-slotd)))
	     (setf (slotd-initargs new-slotd)
		   (union (slotd-initargs new-slotd)
			  (slotd-initargs old-slotd)))
	     (when (eq (slotd-initform new-slotd) 'INITFORM-UNSUPPLIED)
	       (setf (slotd-initform new-slotd) (slotd-initform old-slotd)))
	     (setf (slotd-type new-slotd)
		   ;; FIXME! we should be more smart then this:
		   (cond ((subtypep new-type old-type) new-type)
			 ((subtypep old-type new-type) old-type)
			 (T `(and ,new-type ,old-type)))))
	   new-slotd))

    (let* ((collected-slots)
	   (new-slot))
      (dolist (sc classes)
	(dolist (slot (class-slots sc))
	  (if (setq new-slot
		    (find (slotd-name slot) collected-slots
			  :key #'slotd-name))
	      (combine-slotds new-slot slot) ; updates the slot
	      (if (setq new-slot
			(find (slotd-name slot) slots
			      :key #'slotd-name))
		  (progn
		    (setq slots (delete new-slot slots))
		    (combine-slotds new-slot slot) ; updates the slot
		    (push new-slot collected-slots))
		  (push (copy-slotd slot) collected-slots)))))
      (nconc (nreverse collected-slots) slots))))


;;; ----------------------------------------------------------------------
;;; support for standard-class

(defun collect-default-initargs (classes initargs)
  (dolist (sc classes)
    (setq initargs (default-initargs sc initargs)))
  initargs)

(defun compute-class-precedence-list (class-name superclasses)
  ;; We start by computing two values.
  ;;   CPL
  ;;     The depth-first left-to-right up to joins walk of the supers tree.
  ;;     This is equivalent to depth-first right-to-left walk of the
  ;;     tree with all but the last occurence of a class removed from
  ;;     the resulting list.  This is in fact how the walk is implemented.
  ;;
  ;;   PRECEDENCE-ALIST
  ;;     An alist of the precedence relations. The car of each element
  ;;     of the precedence-alist is a class C, the cdr is all the classes C'
  ;;     which should precede C because either:
  ;;       C is a local super of C'
  ;;      or
  ;;       C' appears before C in some class's local-supers.
  ;;
  ;;     Thus, the precedence-alist reflects the two constraints that:
  ;;       1. A class must appear in the CPL before its local supers.
  ;;       2. Order of local supers is preserved in the CPL.
  ;;
  (labels
   ((must-move-p (element list precedence-alist &aux move)
		 (dolist (must-precede (cdr (assoc element
						   precedence-alist
						   :test #'eq)))
			 (when (setq move (member must-precede (cdr list)
						  :test #'eq))
			       (return move))))
    (find-farthest-move
     (element move precedence-alist)
     (dolist (must-precede (transitive-closure element precedence-alist))
	     (setq move (or (member must-precede move :test #'eq) move)))
     move)
    (transitive-closure
     (class precedence-alist)
     (let* ((closure ()))
       (labels ((walk (element path)
		      (when (member element path :test #'eq)
			    (class-ordering-error
			     class-name element path precedence-alist))
		      (dolist (precede
			       (cdr (assoc element
					   precedence-alist :test #'eq)))
			      (unless (member precede closure :test #'eq)
				      (pushnew precede closure)
				      (walk precede (cons element path))))))
	       (walk class nil)
	       closure))))

   (multiple-value-bind
    (cpl precedence-alist)
    (walk-supers superclasses nil nil)
    (let* ((tail cpl)
	   (element nil)
	   (move nil))
      ;; For each class in the cpl, make sure that there are no classes after
      ;; it which should be before it.  We do this by cdring down the list,
      ;; making sure that for each element of the list, none of its
      ;; must-precedes come after it in the list. If we find one, we use the
      ;; transitive closure of the must-precedes (call find-farthest-move) to
      ;; see where the class must really be moved. We use a hand-coded loop
      ;; so that we can splice things in and out of the CPL as we go.
      (loop (when (null tail) (return))
	    (setq element (car tail)
		  move (must-move-p element tail precedence-alist))
	    (cond (move
		   (setq move (find-farthest-move element move precedence-alist))
		   (setf (cdr move) (cons element (cdr move)))
		   (setf (car tail) (cadr tail))
		   (setf (cdr tail) (cddr tail))
		   )
		  (t
		   (setq tail (cdr tail)))))
      cpl))))

(defun walk-supers (supers cpl precedence-alist)
  (declare (si::c-local))
  (do* ((pre (reverse supers))
	(sup)
	(precedence))
       ((null pre) (values cpl precedence-alist))
    (setq sup (pop pre))
    (when pre
      (if (setq precedence (assoc sup precedence-alist :test #'eq))
	  ;; don't use rplacd here:
	  (setq precedence (cons (car precedence) (union pre precedence)))
	  (push (cons sup pre) precedence-alist)))
    (multiple-value-setq (cpl precedence-alist)
      (walk-supers (class-direct-superclasses sup) cpl precedence-alist))
    (setq cpl (adjoin sup cpl))))

(defun class-ordering-error (root element path precedence-alist)
  (declare (si::c-local))
  (setq path (cons element (reverse (member element (reverse path) :test #'eq))))
  (flet ((pretty (class) (or (class-name class) class)))
    (let ((explanations ()))
      (do ((tail path (cdr tail)))
	  ((null (cdr tail)))
	(let* ((after (cadr tail))
	       (before (car tail)))
	  (if (member after (class-direct-superclasses before) :test #'eq)
	      (push (format nil
			    "~% ~A must precede ~A -- ~
                              ~A is in the local supers of ~A."
			    (pretty before) (pretty after)
			    (pretty after) (pretty before))
		    explanations)
	      (dolist (common-precede
			(intersection
			  (cdr (assoc after precedence-alist :test #'eq))
			  (cdr (assoc before precedence-alist :test #'eq))))
		(when (member after (member before
					    (class-direct-superclasses common-precede)
					    :test #'eq)
			      :test #'eq)
		  (push (format nil
				"~% ~A must precede ~A -- ~
                                  ~A has local supers ~S."
				(pretty before) (pretty after)
				(pretty common-precede)
				(mapcar #'pretty
					(class-direct-superclasses common-precede)))
			explanations))))))
      (error "While computing the class-precedence-list for the class ~A:~%~
              There is a circular constraint through the classes:~{ ~A~}.~%~
              This arises because:~{~A~}"
	     root
	     (mapcar #'pretty path)
	     (reverse explanations)))))

;;; ----------------------------------------------------------------------
