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
	((CONT THREAD DISPATCH-FUNCTION) (find-class 't)))))


;;; ----------------------------------------------------------------------
;;; Each instance has a pointer to the class of which it is an instance.
;;; Its is used to search for methods and class variables.

;;; Bootstrapping for class Class.


(defun search-make-instance (obj)
  (declare (si::c-local))
  (let* ((gfun (symbol-function (if (si::tracing-body 'make-instance)
				    (get-sysprop 'make-instance 'si::traced)
				    'make-instance)))
	 (table (si:gfun-method-ht gfun))
	 (key (list (class-name (si:instance-class obj))))
	 (method (si:method-ht-get key table)))
    (unless method
      (setq method (compute-applicable-methods
		    (si:gfun-instance gfun)
		    (list obj))))
    method))

(defun classp (obj)
  (and (si:instancep obj)
       (search-make-instance obj)
       t))

(defun metaclassp (obj)
  (declare (si::c-local))
  (and (si:instancep obj)
       (search-make-instance (si:instance-class obj))
       (search-make-instance obj)
       t))

;;; ----------------------------------------------------------------------
;;; Object initializations

(defun allocate-instance (class)
  (si:allocate-raw-instance class (length (class-slots class))))

;;; ----------------------------------------------------------------------
;;; Methods

(defun install-method (name qualifiers specializers lambda-list doc plist
			    fun &rest options)
  (declare (ignore doc)
	   (notinline cos ensure-generic-function method-class))
;  (record-definition 'method `(method ,name ,@qualifiers ,specializers))
  (let* ((gf (ensure-generic-function name :lambda-list lambda-list))
	 (method (make-method qualifiers specializers lambda-list
			      fun plist options gf (method-class gf)))
	 (dispatcher (generic-function-dispatcher gf)))

    ;; update the spec-how of the gfun 
    ;; computing the or of the previous value and the new one
    (do ((i 0 (1+ i))
	 (l specializers (cdr l))
	 (spec-how)
	 (spec-how-old))
	((null l))
	(declare (fixnum i))
	(setq spec-how (first l)
	      spec-how-old (si:gfun-spec-how-ref dispatcher i))
	(if (consp spec-how)		; an eql list
	    (if (consp spec-how-old)
		(push (second spec-how) (si:gfun-spec-how-ref dispatcher i))
	      (setf (si:gfun-spec-how-ref dispatcher i) (cdr spec-how)))
	  (unless (consp spec-how-old)	; either T or NIL
	    (setf (si:gfun-spec-how-ref dispatcher i)
		  (or spec-how spec-how-old)))))
    (add-method gf method)))

;;; ----------------------------------------------------------------------
;;;                                                         early versions

(defun method-class (gfun) 'standard-method)

(defun methods (gf) (si:instance-ref gf 7))

;(defun generic-function-dispatcher (gf) (si:instance-ref gf 6)) anticipata

(defun make-gfun (name lambda-list)
  (let* ((nargs
	  (or (position-if
	       #'(lambda (x)
		   (member x '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)
			   :test #'eq))
	       lambda-list)
	      (length lambda-list)))
	 (gfun
	  (si:allocate-gfun
	   name
	   nargs
	   (make-hash-table
	    :test #'equal
	    ;; use fixnums as limits for efficiency:
	    :size *default-method-cache-size*
	    :rehash-size #.(/ *default-method-cache-size* 2)
	    :rehash-threshold #.(/ *default-method-cache-size* 2)))))
    (declare (fixnum nargs))
    (dotimes (i nargs)
      (declare (fixnum i))
      (setf (si:gfun-spec-how-ref gfun i) nil))
    gfun))

;;; early version used during bootstrap
(defun ensure-generic-function (name &key lambda-list)
  (let (gfun)
    (unless (and (fboundp name)
		 (si:gfunp (setq gfun (fdefinition name))))

      ;; create a fake standard-generic-function object:
      (let ((gf-object (si:allocate-raw-instance (find-class 't) 8)))
	(declare (type standard-object gf-object))
	;; create a new gfun
	(setq gfun (make-gfun name lambda-list))

	(si:instance-set gf-object 0 lambda-list) ; lambda-list
	(si:instance-set gf-object 1 'default) ; argument-precedence-order
	(si:instance-set gf-object 2 'standard)	; method-combination
	(si:instance-set gf-object 3 nil) ; method-combination-arguments
	(si:instance-set gf-object 5 nil) ; documentation
	(si:instance-set gf-object 6 gfun) ; gfun
	(si:instance-set gf-object 7 nil) ; methods
	(si:gfun-instance-set gfun gf-object)
	(setf (fdefinition name) gfun)))

    (si:gfun-instance gfun)))

