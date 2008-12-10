;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: CLOS -*-
;;;;
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

(defun self-evaluating-p (form)
  ;; Output T when the form has the same value if it appears quoted
  ;; or unquoted. It is used to check whether we can transform
  ;; (LIST form) into '(form ...)
  (declare (si::c-local))
  (and (atom form)
       (or (not (symbolp form))
	   (member form '(t nil +initform-unsupplied+))
	   (and (boundp form) (constantp form) (eq (symbol-value form) form)))))

(defun make-function-initform (form)
  ;; INITFORM is a form that is to be evaluated at runtime. If it is a
  ;; constant value, we output simply a quoted form. If it is not,
  ;; we output a function that can be invoked at runtime to retrieve
  ;; the value.
  ;;
  ;; Output => (FUNCTION (LAMBDA () form))
  ;;        => (QUOTE ...)
  ;;
  (flet ((enclose (form) `#'(lambda () ,form)))
    (cond
      ;; We generate function for non constant forms
      ((not (constantp form))
       (enclose form))
      ;; Constants other than functions become quoted
      ((and (not (functionp form))
	    (self-evaluating-p form))
       (list 'quote form))
      ;; Quoted forms with arguments which are not functions are
      ;; output as such. (the check for functions is weird, but we are
      ;; paranoid anyway)
      ((and (consp form)
	    (eq (first form) 'quote)
	    (not (functionp (second form))))
       form)
      ;; All other stuff, including symbols other than keywords, T and NIL
      ;; gets in a function form
      (t
       (enclose form)))))

(defun parse-default-initargs (default-initargs)
  (declare (si::c-local))
  (do* ((output-list nil)
	(scan default-initargs (cddr scan))
	(already-supplied '()))
       ((endp scan) `(list ,@(nreverse output-list)))
    (when (endp (rest scan))
      (si::simple-program-error "Wrong number of elements in :DEFAULT-INITARGS option."))
    (let ((slot-name (first scan))
	  (initform (second scan)))
      (if (member slot-name already-supplied)
	  (si::simple-program-error "~S is duplicated in :DEFAULT-INITARGS form ~S"
				    slot-name default-initargs)
	  (push slot-name already-supplied))
      (push `(list ',slot-name ',initform ,(make-function-initform initform))
	    output-list))))

(defmacro defclass (&whole form &rest args)
  (let* (name superclasses slots options
	 metaclass-name default-initargs documentation
	 (processed-options '()))
    (unless (>= (length args) 3)
      (si::simple-program-error "Illegal defclass form: the class name, the superclasses and the slots should always be provided"))
    (setq name (first args)
	  superclasses (second args)
	  slots (third args)
	  args (cdddr args))
    (unless (and (listp superclasses) (listp slots))
      (si::simple-program-error "Illegal defclass form: superclasses and slots should be lists"))
    (unless (and (symbolp name) (every #'symbolp superclasses))
      (si::simple-program-error "Illegal defclass form: superclasses and class name are not valid"))
    ;;
    ;; Here we compose the final form. The slots list, and the default initargs
    ;; may contain object that need to be evaluated. Hence, it cannot be always
    ;; quoted.
    ;;
    (do ((l (setf slots (parse-slots slots)) (rest l)))
	((endp l)
	 (setf slots
	       (if (every #'constantp slots)
		   (list 'quote (mapcar #'si::maybe-unquote slots))
		   `(list ,@slots))))
      (let* ((slotd (first l))
	     (initform (make-function-initform (getf slotd :initform nil))))
	(if (eq (first initform) 'QUOTE)
	    (setf (getf slotd :initform nil) (second initform)
		  slotd (list 'quote slotd))
	    (setf slotd (mapcar #'(lambda (x) `',x) slotd)
		  (getf slotd :initform nil) initform
		  slotd (list* 'list slotd)))
	(setf (first l) slotd)))
    (dolist (option args)
      (let ((option-name (first option))
	    option-value)
	(if (member option-name processed-options)
	    (si:simple-program-error
	     "Option ~s for DEFCLASS specified more than once"
	     option-name)
	    (push option-name processed-options))
	(setq option-value
	      (case option-name
		((:metaclass :documentation)
		 (list 'quote (second option)))
		(:default-initargs
		 (setf option-name :direct-default-initargs)
		 (parse-default-initargs (rest option)))
		(otherwise
		 (list 'quote (rest option)))))
	(setf options (list* `',option-name option-value options))))
    `(eval-when (compile load eval)
       ,(ext:register-with-pde form
			       `(ensure-class ',name :direct-superclasses
					      ',superclasses
					      :direct-slots ,slots ,@options)))))

;;; ----------------------------------------------------------------------
;;; ENSURE-CLASS
;;;
(defun ensure-class (name &rest initargs)
  (let* ((old-class nil)
	 new-class)
    ;; Only classes which have a PROPER name are redefined. If a class
    ;; with the same name is register, but the name of the class does not
    ;; correspond to the registered name, a new class is returned.
    ;; [Hyperspec 7.7 for DEFCLASS]
    (when name
      (when (and (setf old-class (find-class name nil))
		 (not (eq (class-name old-class) name)))
	(setf old-class nil)))
    (setf new-class (apply #'ensure-class-using-class old-class name initargs))
    (when name
      (si:create-type-name name)
      (setf (find-class name) new-class))
    new-class))

#+cross
(eval-when (compile)
  (defun ensure-class (name &rest initargs)
    (warn "Ignoring definition for class ~S" name)))

;;; ----------------------------------------------------------------------
;;; support for standard-class

(defun pair-list (l)
  (declare (si::c-local))
  (if (or (null l) (endp (cdr l)))
      nil
      (cons (cons (first l) (second l))
	    (pair-list (rest l)))))

(defun walk-supers (parent superclasses class-list precedence-alist)
  (let ((new-alist (pair-list (if parent
				  (list* parent superclasses)
				  superclasses))))
    (setf precedence-alist (nconc new-alist precedence-alist)
	  class-list (union superclasses class-list)))
  (dolist (c superclasses)
    (multiple-value-setq (class-list precedence-alist)
      (walk-supers c (class-direct-superclasses c) class-list precedence-alist)))
  (values class-list precedence-alist))

(defun compute-clos-class-precedence-list (class-name superclasses)
  (if (endp (rest superclasses))
      (let ((class (first superclasses)))
	(list* class (class-precedence-list class)))
      (multiple-value-bind (class-list precedence-alist)
	  (walk-supers nil superclasses nil nil)
	(flet ((cycle-error (class-name)
		 (error "A cycle has been detected in the class precedence list for ~A."
			class-name))
	       (free-elements (class-list precedence-alist)
		 (set-difference class-list
				 (delete-duplicates (mapcar #'cdr precedence-alist))))
	       (next-element (free-list cpl)
		 (if (or (null cpl) (endp free-list) (endp (rest free-list)))
		     (first free-list)
		     (dolist (i cpl nil)
		       (dolist (j (class-direct-superclasses i))
			 (when (member j free-list)
			   (return-from next-element j)))))))
	  (do ((cpl '()))
	      ((null class-list)
	       (if precedence-alist (cycle-error class-name) (nreverse cpl)))
	    (let* ((candidates (free-elements class-list precedence-alist))
		   (next (next-element candidates cpl)))
	      (unless next
		(cycle-error class-name))
	      (setf precedence-alist (delete next precedence-alist :key #'car)
		    class-list (delete next class-list)
		    cpl (cons next cpl))))))))

;;; ----------------------------------------------------------------------
