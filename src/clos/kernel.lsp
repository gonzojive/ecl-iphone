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
    '((name :initarg :name :initform nil :accessor class-name)
      (direct-superclasses :initarg :direct-superclasses
       :accessor class-direct-superclasses)
      (direct-subclasses :initform nil :accessor class-direct-subclasses)
      (slots :accessor class-slots)
      (precedence-list :accessor class-precedence-list)
      (direct-slots :initarg :direct-slots :accessor class-direct-slots)
      (direct-default-initargs :initarg :direct-default-initargs
       :initform nil :accessor class-direct-default-initargs)
      (default-initargs :accessor class-default-initargs)
      (finalized :initform nil :accessor class-finalized-p))))

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
       :initform :default
       :accessor generic-function-argument-precedence-order)
      (method-class
       :initarg :method-class
       :initform (find-class 'standard-method)
       :accessor generic-function-method-class)
      (documentation :initarg :documentation)
      (methods :initform nil :accessor generic-function-methods))))

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
      (method-function :initarg :method-function :accessor method-function)
      (documentation :initform nil :initarg documentation)
      (declarations :initform nil)
      (plist :initform nil :initarg :plist :accessor method-plist))))

#.(create-accessors +standard-method-slots+ 'standard-method)

;;; ----------------------------------------------------------------------

(defun class-of (object)
  (if (si:instancep object)
      (si:instance-class object)
      (closest-class (type-of object))))

(defun closest-class (type)
  (or (find-class type nil)
      (case type
	((FIXNUM BIGNUM) (find-class 'integer))
	((SHORT-FLOAT LONG-FLOAT) (find-class 'float))
	((BASE-CHAR STANDARD-CHAR EXTENDED-CHAR) (find-class 'character))
	(SIMPLE-ARRAY (find-class 'array))
	(SIMPLE-VECTOR (find-class 'vector))
	(SIMPLE-BIT-VECTOR (find-class 'bit-vector))
	(SIMPLE-STRING (find-class 'string))
	((MP::PROCESS MP::LOCK) (find-class type))
	(otherwise (find-class 't)))))

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
  (let* ((gf (ensure-generic-function name :lambda-list lambda-list))
	 (method (make-method qualifiers specializers lambda-list
			      fun plist options gf
			      (generic-function-method-class gf))))

    ;; update the spec-how of the gfun 
    ;; computing the or of the previous value and the new one
    (do* ((spec-how-list (or (generic-function-spec-list gf)
			     (make-list (length specializers))))
	  (l specializers (cdr l))
	  (l2 spec-how-list (cdr l2))
	  (spec-how)
	  (spec-how-old))
	 ((null l)
	  (setf (generic-function-spec-list gf) spec-how-list))
      (setq spec-how (first l) spec-how-old (first l2))
      (setf (first l2)
	    (if (consp spec-how)		; an eql list
		(if (consp spec-how-old)
		    (list* (second spec-how) spec-how-old)
		    (cdr spec-how))
		(if (consp spec-how-old)
		    spec-how-old
		    (or spec-how spec-how-old)))))
    (add-method gf method)))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

;;; early version used during bootstrap
(defun ensure-generic-function (name &key lambda-list)
  (if (and (fboundp name) (si::instancep (fdefinition name)))
      (fdefinition name)
      ;; create a fake standard-generic-function object:
      (let ((gfun (si:allocate-raw-instance (find-class 't)
		     #.(length +standard-generic-function-slots+)))
	    (hash (make-hash-table
		   :test #'eql
		   ;; use fixnums as limits for efficiency:
		   :size *default-method-cache-size*
		   :rehash-size #.(/ *default-method-cache-size* 2)
		   :rehash-threshold 0.5s0)))
	(declare (type standard-object gfun))
	;; create a new gfun
	(setf (generic-function-name gfun) name
	      (generic-function-lambda-list gfun) lambda-list
	      (generic-function-argument-precedence-order gfun) 'default
	      (generic-function-method-combination gfun) '(standard)
	      (generic-function-methods gfun) nil
	      (generic-function-spec-list gfun) nil
	      (generic-function-method-hash gfun) hash)
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
    ;(print (generic-function-name gf))
    ;(print (mapcar #'method-specializers methods))
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
		    (and (consp spec) (eql arg (cadr spec)))
		    (typep arg spec)
		    (and (eq 'INVALID spec)
			 (si:instancep arg)
			 (eq 'INVALID (class-name (class-of arg)))))
	  (return))))
    (dolist (arg args) 
      (push (type-of arg) args-specializers))
    (setq args-specializers (nreverse args-specializers))
    ;; then order the list
    (do* ((scan applicable-list)
	  (most-specific (first scan) (first scan))
	  (ordered-list))
	 ((null (cdr scan)) (when most-specific
			      ;; at least one method
			      ;(print (mapcar #'method-specializers
			      ;		     (reverse (cons most-specific ordered-list))))
			      (nreverse
			       (push most-specific ordered-list))))
      (dolist (meth (cdr scan))
	(when (eq (compare-methods most-specific
				   meth args-specializers) 2)
	  (setq most-specific meth)))
      (setq scan (delete most-specific scan))
      (push most-specific ordered-list))))

;;; ----------------------------------------------------------------------
;;;                                                      method comparison

(defun compare-methods (method-1 method-2 args-specializers)
  (declare (si::c-local))
  (let* ((specializers-list-1 (method-specializers method-1))
	 (specializers-list-2 (method-specializers method-2)))
    (compare-specializers-lists specializers-list-1 
				specializers-list-2 args-specializers)))

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

(defun compare-specializers (spec-1 spec-2 arg-spec)
  (declare (si::c-local))
  (let* ((arg-class (closest-class arg-spec))
	 (cpl (cons arg-class (class-precedence-list arg-class)))
	 (cpl-names))
    (setq cpl-names (dolist (e cpl (nreverse cpl-names))
		      (push (class-name e) cpl-names)))
    (cond ((equal spec-1 spec-2) '=)
	  ((null spec-1) '2)
	  ((null spec-2) '1)
	  ((subtypep spec-1 spec-2) '1)
	  ((subtypep spec-2 spec-1) '2)
	  ((and (listp spec-1) (eq (car spec-1) 'eql)) '1) ; is this engough?
	  ((and (listp spec-2) (eq (car spec-2) 'eql)) '2) ; Beppe
	  ((member spec-1 (member spec-2 cpl-names)) '2)
	  ((member spec-2 (member spec-1 cpl-names)) '1)
	  (t (compare-complex-specializers spec-1 spec-2 arg-spec)))))

(defun compare-complex-specializers (spec-1 spec-2 arg-spec)
  (declare (ignore spec-1 spec-2 arg-spec)
	   (si::c-local))
  (error "Complex type specifiers are not yet supported."))

