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
;;; Fixup

(dolist (method-info *early-methods*)
  (let* ((method-name (car method-info))
	 (gfun (fdefinition method-name))
	 (standard-method-class (find-class 'standard-method)))
    (when (eq 'T (class-name (si:instance-class gfun)))
      ;; complete the generic function object
      (si:instance-class-set gfun (find-class 'STANDARD-GENERIC-FUNCTION))
      (si::instance-sig-set gfun)
      (setf (generic-function-method-class gfun) standard-method-class)
      )
    (dolist (method (cdr method-info))
      ;; complete the method object
      (si::instance-class-set method (find-class 'standard-method))
      (si::instance-sig-set gfun)
      )
    (makunbound '*EARLY-METHODS*)))


;;; ----------------------------------------------------------------------
;;;                                                              redefined

(defun method-p (method) (typep method 'METHOD))

(defun make-method (qualifiers specializers arglist
			       function plist options gfun method-class)
  (declare (ignore options))
  (make-instance method-class
		 :generic-function gfun
		 :qualifiers qualifiers
		 :lambda-list arglist
		 :specializers specializers
		 :method-function function
		 :plist plist
		 :allow-other-keys t))

(defun add-method (gf method)
  (declare (notinline method-qualifiers)) ; during boot it's a structure accessor
  (let* ((method-qualifiers (method-qualifiers method)) 
	 (specializers (method-specializers method))
	 found)
    (when (setq found (find-method gf method-qualifiers specializers nil))
	  (remove-method gf found))
  (push method (generic-function-methods gf))
  (clrhash (generic-function-method-hash gf))
  method))

(defun remove-method (gf method)
  (setf (generic-function-methods gf)
	(delete method (generic-function-methods gf))))

;;; ----------------------------------------------------------------------
;;; Error messages

(defmethod no-applicable-method (gf &rest args)
    (declare (ignore args))
  (error "No applicable method for ~S" 
	 (generic-function-name gf)))

(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf args))
  (error "No next method for method ~A" method))

(defun no-primary-method (gf &rest args)
  (error "Generic function: ~A. No primary method given arguments: ~S"
	 (generic-function-name gf) args))

;;; Now we protect classes from redefinition:
(eval-when (compile load)
(defun setf-find-class (new-value name &optional errorp env)
  (let ((old-class (find-class name nil)))
    (cond
      ((typep old-class 'built-in-class)
       (error "The class associated to the CL specifier ~S cannot be changed."
	      name))
      ((member name '(CLASS BUILT-IN-CLASS) :test #'eq)
       (error "The kernel CLOS class ~S cannot be changed." name))
      ((classp new-value)
       (setf (gethash name si:*class-name-hash-table*) new-value))
      ((null new-value) (remhash name si:*class-name-hash-table*))
      (t (error "~A is not a class." new-value))))
  new-value)
)
