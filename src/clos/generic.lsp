;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defun legal-generic-function-p (name)
  (declare (si::c-local))
  (cond ((not (fboundp name)))
	; a generic function already exists
	((si:gfunp (fdefinition name)))
	((special-operator-p name)
	 (error "~A is a special form" name))
	((macro-function name)
	 (error "~A is a macro" name))
	(t 
	 (error "~A is a lisp function" name))))

(defmacro defgeneric (&rest args)
  (multiple-value-bind (function-specifier lambda-list options)
    (parse-defgeneric args)
    (when (legal-generic-function-p function-specifier)
      (parse-lambda-list lambda-list)
      
      ;; process options
      (multiple-value-bind (option-list method-list)
	  (parse-generic-options options lambda-list)
	#|
	       (if (fboundp function-specifier)
		   ;; remove methods defined by previous defgeneric
		   (setf (methods 
			  (si:gfun-instance 
			   (symbol-function function-specifier)) nil)))
|#
	(let ((output `(ensure-generic-function ',function-specifier
			,@option-list)))
	  (if method-list
	      (do* ((scan method-list (cdr scan)))
		   ((endp scan)
		    `(prog1 ,output ,@method-list))
		(setf (first scan)
		      `(defmethod ,function-specifier ,@(first scan))))
	      output))
	;;,(dolist (method method-list)
	;; add methods specified by defgeneric
	;;
	))))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-defgeneric (args)
  (declare (si::c-local))
  ;; (values function-specifier lambda-list options)
  (let (function-specifier)
    (unless args
      (error "Illegal defgeneric form: missing generic function name"))
    (setq function-specifier (pop args))
    (unless (legal-generic-function-name-p function-specifier)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is SETF and whose cadr is a non-nil symbol."
	     function-specifier))
    (unless args
      (error "Illegal defgeneric  form: missing lambda-list"))
    (values function-specifier (first args) (rest args))))

#|
(defun parse-generic-function (args)
  (declare (si::c-local))
  ;; (values lambda-list options)
  (unless args
    (error "Illegal generic-function form: missing lambda-list"))
  (values (first args) (rest args)))
|#
	
(defun parse-generic-options (options lambda-list)
  (declare (si::c-local))
  (let* ((processed-options '())
	 (method-list '())
	 arg-list)
    (dolist (option options)
      (let ((option-name (first option))
	    option-value)
	(cond ((eq option-name :method)
	       ;; We do not need to check the validity of this
	       ;; because DEFMETHOD will do it.
	       (push (rest option) method-list))
	      ((member option-name processed-options)
	       (error "Option ~s specified more than once" option-name))
	      (t
	       (push option-name processed-options)
	       (setq option-value
		     (case option-name
		       (:argument-precedence-order
			(parse-parameter-names (second option) lambda-list))
		       (declare
			(setq option-name :declare)
			(mapcar #'parse-legal-declaration (rest option)))
		       (:documentation
			(parse-legal-documentation (second option)))
		       (:method-combination
			(rest option))
		       (:generic-function-class
			(legal-generic-function-classp (second option)))
		       (:method-class
			(parse-legal-method-class (second option)))
		       (otherwise
			(error "~S is not a legal defgeneric option"
			       option-name))))
	       (setf arg-list `(,option-name ',option-value ,@arg-list))))))
    (values `(:lambda-list ',lambda-list ,@arg-list)
	    method-list)))

(defun ensure-generic-function (function-specifier &rest args &key
				lambda-list
				(generic-function-class 'STANDARD-GENERIC-FUNCTION)
				(method-class 'STANDARD-METHOD)
				&allow-other-keys)
  (unless (LEGAL-GENERIC-FUNCTION-NAME-P function-specifier)
    (error "Generic function ~A has incorrect function specifier
                  (a non-nil symbol, a list whose car is SETF)"
	   function-specifier))
  (when (LEGAL-GENERIC-FUNCTION-P function-specifier)
    (setf args (copy-list args))
    (remf args :generic-function-class)
    (remf args :declare)
    (remf args :environment)
    (unless (classp method-class)
      (setf (getf args :method-class) (find-class method-class)))

    (let (dispatcher gf-object)
      (if (and (fboundp function-specifier)
	       (si:gfunp (setq dispatcher (fdefinition function-specifier))))

	  ;; modify the existing object
	  (progn
	    (setf gf-object (si:gfun-instance dispatcher)
		  gf-object (apply #'reinitialize-instance gf-object args))
	    ;;(if (or
	    ;;     (not (method-exist-p function-specifier))
	    ;;     (congruent-lambda-list-p lambda-list 
	    ;;		    (lambda-list gf-object)))
	    ;;   (setf (lambda-list generic-function) lambda-list))
	    ;;(when (and generic-function-class
	    ;;           (compatible-p generic-function-class
	    ;;                         (generic-function-class gf-object)))
	    ;;     (change-class gf-object generic-function-class))
	    )
	  ;; else create a new generic function object
	  (setf dispatcher (make-gfun function-specifier lambda-list)
		gf-object (apply #'make-instance generic-function-class args)))
      (setf (si:gfun-instance dispatcher) gf-object
	    (gfun gf-object) dispatcher
	    (fdefinition function-specifier) dispatcher)
      gf-object))))

;;; ----------------------------------------------------------------------
;;;                                                             congruence

#+nil
(defun congruent-lambda-list-p (l1 l2)
  (let (post-keyword)
    (do ((scan1 l1 (cdr scan1))
	 (scan2 l2 (cdr scan2)))
	((and (null scan1) (null scan2)))
	(cond ((and (not post-keyword)
		    (or
		     (and 
		      (member (first scan1) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))
		      (not (member (first scan2) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))))
		     (and 
		      (member (first scan2) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))
		      (not (member (first scan1) 
			      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX))))))
	       (error "the two lambda lists ~S ~S have not the same number
                       of required parameters" l1 l2))
	      ((eq (first scan1) '&OPTIONAL)
	       (unless (eq (first scan2) '&OPTIONAL)
		       (error "the two lambda lists ~S ~S have not the same 
                        number of optional parameters" l1 l2))
	       (do ((scan-op1 (cdr scan1) (cdr scan-op1))
		    (scan-op2 (cdr scan2) (cdr scan-op2)))
		    ((and (null scan-op1) (null scan-op2)))
		    (cond ((or (null scan-op2)
			      (member (car scan-op2) 
				      '(&REST &KEY &ALLOW-OTHER-KEYS &AUX)))
			  (error "the two lambda lists ~S ~S have not the same 
                        number of optional parameters" l1 l2)))))))))

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun parse-lambda-list (lambda-list &optional post-keyword)
  (declare (si::c-local))
  (let ((arg (car lambda-list)))
    (cond ((null lambda-list))
	  ((eq arg '&AUX)
	   (error "&aux is not allowed in a generic function lambda-list"))
	  ((member arg lambda-list-keywords)
	   (parse-lambda-list (cdr lambda-list) t))
	  (post-keyword
	   ;; After a lambda-list-keyword there can be no specializers.
	   (parse-lambda-list (cdr lambda-list) t))
	  (t
	   (if (listp arg)
	       (error "the parameters cannot be specialized in generic function lambda-list")
	       (parse-lambda-list (cdr lambda-list)))))))

(defun parse-parameter-names (parameter-list lambda-list)
  (declare (si::c-local))
  (let* (required-list count)
    (setf required-list
	  (do ((l lambda-list (cdr l)))
	      ((or (null l)
		   (member (car l) '(&REST &KEY &ALLOW-OTHER-KEYS)))
	       (nreverse required-list))
	      (push l required-list)))
    (dolist (l required-list parameter-list)
	    (setf count (count l parameter-list))
	    (when (not (= count 1))
		  (error "the required parameter ~S must be included
                                exactly once in the argument-precedence-order 
                                option" l)))))

(defun parse-legal-declaration (decl)
  (declare (si::c-local))
  (unless (eq (first decl) 'OPTIMIZE)
	  (error "The only declaration allowed is optimize"))
  (dolist (first (rest decl))
    (when (atom first)
      (setq first (cons first 3)))
    (unless (member (car first) '(SPEED SPACE COMPILATION-SPEED DEBUG SAFETY))
      (error "The only qualities allowed are speed and space")))
  decl)

(defun parse-legal-documentation (doc)
  (declare (si::c-local))
  (unless (stringp doc)
	  (error "The documentation must be a string"))
  doc)

(defun legal-generic-function-classp (class-name)
  (declare (si::c-local))
  ; until we don't know when a class can be the class of a generic function
  (unless (subtypep (find-class class-name) 
		    (find-class 'GENERIC-FUNCTION))
	  (error "The class ~S cannnot be the class of a generic function" 
		 class-name))
  class-name)

(defun parse-legal-method-class (class-name)
  (declare (si::c-local))
  ; until we don't know when a class can be the class of a method
  (error "At the moment the class of a method can be only standard-method"))
