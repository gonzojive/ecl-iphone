;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPTYPE  Type information.

(in-package "COMPILER")

;;; CL-TYPE is any valid type specification of Common Lisp.
;;;
;;; TYPE is a representation type used by ECL.  TYPE is one of:
;;;
;;;				T(BOOLEAN)
;;;
;;;	FIXNUM  CHARACTER  SHORT-FLOAT  LONG-FLOAT
;;;	(VECTOR T)  STRING  BIT-VECTOR  (VECTOR FIXNUM)
;;;	(VECTOR SHORT-FLOAT)  (VECTOR LONG-FLOAT)
;;;	(ARRAY T)  (ARRAY BASE-CHAR)  (ARRAY BIT)
;;;	(ARRAY FIXNUM)
;;;	(ARRAY SHORT-FLOAT)  (ARRAY LONG-FLOAT)
;;;	STANDARD-OBJECT STRUCTURE-OBJECT
;;;	SYMBOL
;;;	UNKNOWN
;;;
;;;				NIL
;;;
;;;
;;; immediate-type:
;;;	FIXNUM		int
;;;	CHARACTER	char
;;;	SHORT-FLOAT	float
;;;	LONG-FLOAT	double


;;; Check if THING is an object of the type TYPE.
;;; Depends on the implementation of TYPE-OF.
(defun object-type (thing)
  (let ((type (if thing (type-of thing) 'SYMBOL)))
    (case type
      ((FIXNUM SHORT-FLOAT LONG-FLOAT SYMBOL NULL) type)
      ((BASE-CHAR STANDARD-CHAR CHARACTER EXTENDED-CHAR) 'CHARACTER)
      ((STRING BIT-VECTOR) type)
      (VECTOR (list 'VECTOR (array-element-type thing)))
      (ARRAY (list 'ARRAY (array-element-type thing)))
      #+clos
      (STANDARD-OBJECT 'STANDARD-OBJECT)
      #+clos
      (STRUCTURE-OBJECT 'STRUCTURE-OBJECT)
      (t t))))

(defun type-filter (type)
  (multiple-value-bind (type-name type-args) (sys::normalize-type type)
    (case type-name
        ((FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT SYMBOL) type-name)
        (SINGLE-FLOAT 'SHORT-FLOAT)
        (DOUBLE-FLOAT 'LONG-FLOAT)
        ((SIMPLE-STRING STRING) 'STRING)
        ((SIMPLE-BIT-VECTOR BIT-VECTOR) 'BIT-VECTOR)
	((NIL T) t)
	((SIMPLE-ARRAY ARRAY)
	 (cond ((endp type-args) '(ARRAY *))		; Beppe
	       ((eq '* (car type-args)) t)
	       (t (let ((element-type
			 (sys::type-for-array (car type-args))))
		    (if (and (cdr type-args)
			     (not (eq (second type-args) '*))
			     (= (length (second type-args)) 1))
		      (case element-type
			(BASE-CHAR 'STRING)
			(BIT 'BIT-VECTOR)
			(t (list 'VECTOR element-type)))
		      (list 'ARRAY element-type))))))
	(INTEGER
	 (if (sys::sub-interval-p type-args
				  '#.(list most-negative-fixnum
					   most-positive-fixnum))
	   'FIXNUM
	   t))
	((SHORT-FLOAT SINGLE-FLOAT) 'SHORT-FLOAT)
	((LONG-FLOAT DOUBLE-FLOAT) 'LONG-FLOAT)
	((STREAM) 'STREAM)	; Beppe
	(t (cond #+clos
		 ((subtypep type 'STANDARD-OBJECT) type)
		 #+clos
		 ((subtypep type 'STRUCTURE-OBJECT) type) 
		 ((dolist (v '(FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT
			       (VECTOR T) STRING BIT-VECTOR
			       (VECTOR FIXNUM) (VECTOR SHORT-FLOAT)
			       (VECTOR LONG-FLOAT) (ARRAY BASE-CHAR)
			       (ARRAY BIT) (ARRAY FIXNUM)
			       (ARRAY SHORT-FLOAT) (ARRAY LONG-FLOAT)
			       (ARRAY T))) ; Beppe
		    (when (subtypep type v) (return v))))
		 ((eq type-name 'VALUES)
		  (if (null (cdr type-args))
		    (list 'VALUES (type-filter (car type-args)))
		    t))
		 ((and (eq type-name 'SATISFIES) ; Beppe
		       (symbolp (car type-args))
		       (get (car type-args) 'TYPE-FILTER)))
		 (t t))))))

;;; The algebra of types should be more complete. Beppe
#+nil
(defun type-and (type1 type2)
  (cond ((equal type1 type2) type1)
        ((eq type1 t) type2)
	((eq type1 '*) type2)		; optional args
	((eq type1 'OBJECT) type2)	; Beppe
	((eq type2 'OBJECT) type1)	; Beppe
        ((eq type2 t) type1)
	((eq type2 '*) type1)		; optional args
	((and (consp type2) (eq (car type2) 'VALUES))
	 (type-and type1 (second type2)))
        ((consp type1)
         (case (car type1)
               (ARRAY
                (case (cadr type1)
                      (BASE-CHAR
		       (if (eq type2 'STRING)
			   type2
			 (if (and (consp type2)	; Beppe
				  (member (car type2) '(ARRAY VECTOR))
				  (null (cddr type2))
				  (type>= 'BASE-CHAR (cadr type2)))
			     type1 nil)))
                      (BIT (if (eq type2 'BIT-VECTOR) type2 nil))
		      ;; Beppe:
                      (t (case type2
			   (STRING
			    (when (type-and (cadr type1) 'BASE-CHAR)
			      '(ARRAY BASE-CHAR)))
			   (BIT-VECTOR
			    (when (type-and (cadr type1) 'BIT)
			      '(ARRAY BIT)))
			   ((ARRAY VECTOR) type1)
			   (t (if (and (consp type2)
				       (member (car type2) '(ARRAY VECTOR)))
				  (cond ((type>= (cadr type1) (cadr type2))
					 type2)
					((type>= (cadr type2) (cadr type1))
					 type1))))))))
               (VECTOR
                (case (cadr type1)	; Beppe
                      (BASE-CHAR
		       (if (eq type2 'STRING)
			   type2
			 (if (and (consp type2)	; Beppe
				  (member (car type2) '(ARRAY VECTOR))
				  (null (cddr type2))
				  (type>= 'BASE-CHAR (cadr type2)))
			     type1 nil)))
                      (BIT (if (eq type2 'BIT-VECTOR) type2 nil))
		      ;; Beppe
                      (t (case type2
			   (STRING
			    (when (eq (cadr type1) 'BASE-CHAR) type1))
			   (BIT-VECTOR
			    (when (eq (cadr type1) 'BIT) type1))
			   ((ARRAY VECTOR) type1)
			   (t (if (and (consp type2)
				       (member (car type2) '(ARRAY VECTOR)))
				  (cond ((type>= (cadr type1) (cadr type2))
					 type2)
					((type>= (cadr type2) (cadr type1))
					 type1))))))))
	       (values (type-and (second type1) type2)))
	 )
	((and (consp type2) (eq (car type2) 'OR)) ; Beppe
	 (do ((types (cdr type2) (cdr types))
	      (res))
	     ((null types) nil)
	     (when (setq res (type-and type1 (car types)))
	       (return res))))
        (t (case type1
                 (STRING
                  (if (and (consp type2) (eq (car type2) 'ARRAY)
                           (eq (cadr type2) 'BASE-CHAR))
                      type1 nil))
                 (BIT-VECTOR
                  (if (and (consp type2) (eq (car type2) 'ARRAY)
                           (eq (cadr type2) 'BIT))
                      type1 nil))
                 (FIXNUM-FLOAT
                  (if (member type2 '(FIXNUM FLOAT SHORT-FLOAT LONG-FLOAT))
                      type2 nil))
                 (FLOAT
                  (if (member type2 '(SHORT-FLOAT LONG-FLOAT))
                      type2 nil))
                 ((LONG-FLOAT SHORT-FLOAT)
                  (if (member type2 '(FIXNUM-FLOAT FLOAT))
                      type1 nil))
		 ((BYTE8 INTEGER8 BIT)
		  (if (eq type2 'FIXNUM) type1 nil))
;;;		 ((UNSIGNED-SHORT)
;;;		  (if (subtypep type1 type2) type1 nil))
                 (FIXNUM
		  (case type2
		    ((bit FIXNUM-FLOAT) 'FIXNUM)
		    ((BYTE8 INTEGER8 BIT)
		     type2)
;;;		    ((UNSIGNED-SHORT)
;;;		     (if (subtypep type2 type1) type2 nil))
		    ))
		 #+clos
		 (STANDARD-OBJECT
                  (if (subtypep type2 'STANDARD-OBJECT) type2 nil))
		 #+clos
		 (STRUCTURE-OBJECT
                  (if (subtypep type2 'STRUCTURE-OBJECT) type2 nil))))))

;;; The algebra of types should be more complete. Beppe
(defun type-and (type1 type2 &optional finish &aux out t2 args2)
  (when (or (eq type1 type2) (eq type1 'OBJECT) (eq type1 '*))
    (return-from type-and type2))
  (when (or (eq type2 'OBJECT) (eq type2 '*))
    (return-from type-and type1))
  (when (subtypep type1 type2)
    (return-from type-and type1))
  (when (subtypep type2 type1)
    (return-from type-and type2))
  (multiple-value-setq (name2 args2) (sys::normalize-type type2))
  (case name2
    (VALUES (type-and type1 (car args2)))
    (AND (loop for i in args2
	       when (setq t2 (type-and type1 i))
	       collect i into out
	       finally (return (and out (cons 'AND out)))))
    (OR (loop for i in args2
	      when (setq t2 (type-and type1 i))
	      collect i into out
	      finally (return (and out (cons 'OR out)))))
    (MEMBER (loop for i in args2
		  when (setq t2 (typep i type1))
		  collect i into out
		  finally (return (and out (cons 'MEMBER out)))))
    (NOT (setq t2 (type-and type1 (car args2)))
	 (cond ((null t2) type1)
	       ((eq t2 type1) nil)
	       (t (list 'AND type1 type2))))
    (otherwise
     (if finish nil (type-and type2 type1 t)))))

#+nil
(defun type>= (type1 type2)
  (equal (type-and type1 type2) type2))

(defun type>= (type1 type2)
  (subtypep type2 type1))

(defun reset-info-type (info)
  (if (info-type info)
      (let ((info1 (copy-info info)))
           (setf (info-type info1) t)
           info1)
      info))

;;;
;;; and-form-type
;;;   returns a copy of form whose type is the type-and of type and the form's
;;;   type
;;;
(defun and-form-type (type form original-form &optional (mode :safe)
		      (format-string "") &rest format-args)
  (let* ((type2 (info-type (cadr form)))
	 (type1 (or (type-and type type2)
		    (when (subtypep type2 type) type2)))) ; class types. Beppe
    (unless type1
      (funcall (if (eq mode :safe) #'cmperr #'cmpwarn)
	       "~?, the type of the form ~s is ~s, not ~s." format-string
	       format-args original-form type2 type))
    (if (eq type1 type2)
      form
      (let ((info (copy-info (cadr form))))
	(setf (info-type info) type1)
	(list* (car form) info (cddr form))))))

(defun default-init (type)
  (let ((new-value (getf '(fixnum 0 character #\space long-float 0.0L1
			   short-float 0.0S1)
			 type)))
    (cond (new-value
	   (cmpwarn "The default value of NIL is not ~s. Using ~s instead."
		    type new-value)
	   (c1constant-value new-value nil))
	  (t
	   (c1nil)))))
