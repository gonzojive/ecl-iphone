;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defconstant *default-method-cache-size* 64 "Size of hash tables for methods")

;;;----------------------------------------------------------------------
;;; BOOTSTRAP FUNCTIONS TO ACCESS SLOTS
;;;
;;; ECL has some restictions regarding the basic classes CLASS,
;;; STANDARD-CLASS and STANDARD-GENERIC-FUNCTION. These are that, certain
;;; slots must have pre-defined positions which cannot change. That means
;;; that a user can extend these classes, but they must be the first ones
;;; in the class hierarchy, and the position of their slots must not change.

(eval-when (compile eval)
(defun create-accessors (slotds type)
  (let ((i 0)
	(output '())
	name)	
    (dolist (s slotds `(progn ,@output))
      (when (setf name (getf (cdr s) :accessor))
	(remf (cdr s) :accessor)
	(setf output
	      (append output
		      `((defun ,name (obj)
			  (si:instance-ref obj ,i))
			(defsetf ,name (obj) (x)
			  `(si:instance-set ,obj ,,i ,x))
			#+nil
			(define-compiler-macro ,name (obj)
			  `(si:instance-ref ,obj ,,i))
			))))
      (incf i))))
)

;;; ----------------------------------------------------------------------
;;; Class CLASS

(eval-when (compile eval)
  (defparameter +class-slots+
    '((name :initarg :name :initform nil :accessor class-id)
      (direct-superclasses :initarg :direct-superclasses
       :accessor class-direct-superclasses)
      (direct-subclasses :initform nil :accessor class-direct-subclasses)
      (slots :accessor class-slots)
      (precedence-list :accessor class-precedence-list)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots)
      (direct-default-initargs :initarg :direct-default-initargs
       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs)
      (finalized :initform nil :accessor class-finalized-p)
      (prototype :accessor class-prototype))))

#.(create-accessors +class-slots+ 'class)

;;; ----------------------------------------------------------------------
;;; STANDARD-CLASS

(eval-when (compile eval)
  (defparameter +standard-class-slots+
    (append +class-slots+
	    '((slot-index-table :accessor slot-index-table)
	      (shared-slots :initform nil :accessor class-shared-slots)
	      (documentation :initarg :documentation :initform nil)
	      (forward)))))

#.(create-accessors +standard-class-slots+ 'standard-class)

;;; ----------------------------------------------------------------------
;;; STANDARD-GENERIC-FUNCTION

(eval-when (compile eval)
  (defparameter +standard-generic-function-slots+
    '((name :initarg :name :initform nil
       :accessor generic-function-name)
      (method-hash :accessor generic-function-method-hash
       :initform (make-hash-table
		  :test #'eql
		  ;; use fixnums as limits for efficiency:
		  :size *default-method-cache-size*
		  :rehash-size #.(/ *default-method-cache-size* 2)
		  :rehash-threshold 0.5s0))
      (spec-list :initform nil :accessor generic-function-spec-list)
      (method-combination 
       :initarg :method-combination :initform '(standard)
       :accessor generic-function-method-combination)
      (lambda-list :initarg :lambda-list
       :accessor generic-function-lambda-list)
      (argument-precedence-order 
       :initarg :argument-precedence-order
       :initform nil
       :accessor generic-function-argument-precedence-order)
      (method-class
       :initarg :method-class
       :initform (find-class 'standard-method)
       :accessor generic-function-method-class)
      (documentation :initarg :documentation)
      (methods :initform nil :accessor generic-function-methods)
      (a-p-o-function :initform nil :accessor generic-function-a-p-o-function))))

#.(create-accessors +standard-generic-function-slots+
		    'standard-generic-function)

;;; ----------------------------------------------------------------------
;;; STANDARD-METHOD

(eval-when (compile eval)
  (defparameter +standard-method-slots+
    '((generic-function :initarg :generic-function
       :accessor method-generic-function)
      (lambda-list :initarg :lambda-list
       :accessor method-lambda-list)
      (specializers :initarg :specializers :accessor method-specializers)
      (qualifiers :initform nil :initarg :qualifiers :accessor method-qualifiers)
      (function :initarg :function :accessor method-function)
      (documentation :initform nil :initarg documentation)
      (declarations :initform nil)
      (plist :initform nil :initarg :plist :accessor method-plist)
      (from-defgeneric-p :initform nil :accessor method-from-defgeneric-p))))

#.(create-accessors +standard-method-slots+ 'standard-method)

;;; ----------------------------------------------------------------------

(defun classp (obj)
  (and (si:instancep obj)
       (si::subclassp (si::instance-class obj) (find-class 'CLASS))
       t))

;;; ----------------------------------------------------------------------
;;; Methods

(defun install-method (name qualifiers specializers lambda-list doc plist
			    fun &rest options)
  (declare (ignore doc)
	   (notinline ensure-generic-function))
;  (record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name))
	 (specializers (mapcar #'(lambda (x)
				   (cond ((null x) x)
					 ((consp x) x)
					 ((si::instancep x) x)
					 (t (find-class x))))
			       specializers))
	 (method (make-method qualifiers specializers lambda-list
			      fun plist options gf
			      (generic-function-method-class gf))))
    (add-method gf method)
    method))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

;;; early version used during bootstrap
(defun ensure-generic-function (name &key (lambda-list (si::unbound) l-l-p))
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
      ;; create a fake standard-generic-function object:
      (let ((gfun (si:allocate-raw-instance nil (find-class 't)
		     #.(length +standard-generic-function-slots+)))
	    (hash (make-hash-table
		   :test #'eql
		   ;; use fixnums as limits for efficiency:
		   :size *default-method-cache-size*
		   :rehash-size #.(/ *default-method-cache-size* 2)
		   :rehash-threshold 0.5s0)))
	(declare (type standard-object gfun))
	;; create a new gfun
	(si::instance-sig-set gfun)
	(setf (generic-function-name gfun) name
	      (generic-function-lambda-list gfun) lambda-list
	      (generic-function-method-combination gfun) '(standard)
	      (generic-function-methods gfun) nil
	      (generic-function-spec-list gfun) nil
	      (generic-function-method-hash gfun) hash)
	(when l-l-p
	  (setf (generic-function-argument-precedence-order gfun)
		(rest (si::process-lambda-list lambda-list t))))
	(si::set-funcallable gfun t)
	(setf (fdefinition name) gfun)
	gfun)))


;;; ----------------------------------------------------------------------
;;; COMPUTE-APPLICABLE-METHODS
;;;

(defun compute-applicable-methods (gf args)
  (let* ((methods (generic-function-methods gf))
	 applicable-list
	 args-specializers)
    ;; first compute the applicable method list
    (dolist (method methods)
      ;; for each method in the list
      (do* ((scan-args args (cdr scan-args))
	    (scan-specializers (method-specializers method)
			       (cdr scan-specializers))
	    (arg)
	    (spec))
	  ;; check if the method is applicable verifying
	  ;; if each argument satisfies the corresponding
	  ;; parameter specializers
	  ((null scan-args) (push method applicable-list))
	(setq arg (first scan-args)
	      spec (first scan-specializers))
	(unless (or (null spec)
		    (and (consp spec) (eql arg (second spec)))
		    (typep arg spec))
	  (return))))
    (dolist (arg args)
      (push (class-of arg) args-specializers))
    (setq args-specializers (nreverse args-specializers))
    ;; then order the list
    (do* ((scan applicable-list)
	  (most-specific (first scan) (first scan))
	  (f (generic-function-a-p-o-function gf))
	  (ordered-list))
	 ((null (cdr scan)) (when most-specific
			      ;; at least one method
			      ;(print (mapcar #'method-specializers
			      ;		     (reverse (cons most-specific ordered-list))))
			      (nreverse
			       (push most-specific ordered-list))))
      (dolist (meth (cdr scan))
	(when (eq (compare-methods most-specific
				   meth args-specializers f) 2)
	  (setq most-specific meth)))
      (setq scan (delete most-specific scan))
      (push most-specific ordered-list))))

;;; ----------------------------------------------------------------------
;;;                                                      method comparison

(defun compare-methods (method-1 method-2 args-specializers f)
  (declare (si::c-local))
  (let* ((specializers-list-1 (method-specializers method-1))
	 (specializers-list-2 (method-specializers method-2)))
    (compare-specializers-lists (if f (funcall f specializers-list-1) specializers-list-1)
				(if f (funcall f specializers-list-2) specializers-list-2)
				args-specializers)))

(defun compare-specializers-lists (spec-list-1 spec-list-2 args-specializers)
  (declare (si::c-local))
  (when (or spec-list-1 spec-list-2)
    (ecase (compare-specializers (first spec-list-1)
				 (first spec-list-2)
				 (first args-specializers))
      (1 '1)
      (2 '2)
      (= 
       (compare-specializers-lists (cdr spec-list-1)
				   (cdr spec-list-2)
				   (cdr args-specializers)))
      ((nil)
       (error "The type specifiers ~S and ~S can not be disambiguated~
                  with respect to the argument specializer: ~S"
	      (or (car spec-list-1) t)
	      (or (car spec-list-2) t)
	      (car args-specializers)))))
  )

(defun compare-specializers (spec-1 spec-2 arg-class)
  (declare (si::c-local))
  (let* ((cpl (class-precedence-list arg-class)))
    (cond ((equal spec-1 spec-2) '=)
	  ((null spec-1) '2)
	  ((null spec-2) '1)
	  ((subtypep spec-1 spec-2) '1)
	  ((subtypep spec-2 spec-1) '2)
	  ((and (listp spec-1) (eq (car spec-1) 'eql)) '1) ; is this engough?
	  ((and (listp spec-2) (eq (car spec-2) 'eql)) '2) ; Beppe
	  ((member spec-1 (member spec-2 cpl)) '2)
	  ((member spec-2 (member spec-1 cpl)) '1)
	  (t (error "Complex type specifiers are not yet supported."))
	  )))

(defun compute-g-f-spec-list (gf)
  (flet ((nupdate-spec-how-list (spec-how-list specializers gf)
	   ;; FIXME! This check should have happened before, shouldn't it???
	   (let ((l (length specializers)))
	     (if spec-how-list
		 (unless (= (length spec-how-list) l)
		   (error "The generic function ~A~%has ~D required arguments, but the new specialization provides ~D."
			  gf (length spec-how-list) l))
		 (setf spec-how-list (make-list l))))
	   ;; update the spec-how of the gfun 
	   ;; computing the or of the previous value and the new one
	   (do* ((l specializers (cdr l))
		 (l2 spec-how-list (cdr l2))
		 (spec-how)
		 (spec-how-old))
		((null l))
	     (setq spec-how (first l) spec-how-old (first l2))
	     (setf (first l2)
		   (if (consp spec-how)	; an eql list
		       (if (consp spec-how-old)
			   (list* (second spec-how) spec-how-old)
			   (cdr spec-how))
		       (if (consp spec-how-old)
			   spec-how-old
			   (or spec-how spec-how-old)))))
	   spec-how-list))
  (let* ((spec-how-list nil)
	 (function nil)
	 (a-p-o (generic-function-argument-precedence-order gf)))
    (dolist (method (generic-function-methods gf))
      (setf spec-how-list
	    (nupdate-spec-how-list spec-how-list (method-specializers method) gf)))
    (setf (generic-function-spec-list gf)
	  (loop for type in spec-how-list
		for name in (generic-function-lambda-list gf)
		for i from 0
		when type collect (cons type (position name a-p-o))))
    (let* ((g-f-l-l (generic-function-lambda-list gf)))
      (when (consp g-f-l-l)
	(let ((required-arguments (rest (si::process-lambda-list g-f-l-l t))))
	  (unless (equal a-p-o required-arguments)
	    (setf function
		  (coerce `(lambda (%list)
			    (destructuring-bind ,required-arguments %list
			      (list ,@a-p-o)))
			  'function))))))
    (setf (generic-function-a-p-o-function gf) function)
    (clrhash (generic-function-method-hash gf)))))

(defun print-object (object stream)
  (print-unreadable-object (object stream)))