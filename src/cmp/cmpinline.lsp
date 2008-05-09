;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPINLINE  Open coding optimizer.

(in-package "COMPILER")

;;; Valid property names for open coded functions are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; Each property is a list of 'inline-info's, where each inline-info is:
;;; ( types { type | boolean } { string | function } ).
;;;
;;; For each open-codable function, open coding will occur only if there exits
;;; an appropriate property with the argument types equal to 'types' and with
;;; the return-type equal to 'type'.  The third element
;;; is T if and only if side effects may occur by the call of the function.
;;; Even if *DESTINATION* is TRASH, open code for such a function with side
;;; effects must be included in the compiled code.
;;; The forth element is T if and only if the result value is a new Lisp
;;; object, i.e., it must be explicitly protected against GBC.

;;;
;;; inline-args:
;;;   returns a list of pairs (type loc)
;;;   side effects: emits code for temporary variables
;;;
;;; Whoever calls inline-args must bind *inline-blocks* to 0 and afterwards
;;; call close-inline-blocks
;;;
(defun inline-args (forms &optional types)
  (flet ((all-locations (args &aux (res t))
	   (dolist (arg args res)
	     (unless (member (c1form-name arg)
			     '(LOCATION VAR SYS:STRUCTURE-REF
			       #+clos SYS:INSTANCE-REF)
			     :test #'eq)
	       (setq res nil)))))

    (do ((forms forms (cdr forms))
	 (form) (locs))
	((endp forms) (nreverse locs))
      (setq form (car forms))
      (case (c1form-name form)
	(LOCATION
	 (push (list (c1form-primary-type form) (c1form-arg 0 form)) locs))
	(VAR
	 (let ((var (c1form-arg 0 form)))
	   (if (var-changed-in-form-list var (cdr forms))
	       (let* ((var-rep-type (var-rep-type var))
		      (lcl (make-lcl-var :rep-type var-rep-type :type (var-type var))))
		 (wt-nl "{" (rep-type-name var-rep-type) " " lcl "= " var ";")
		 (push (list (c1form-primary-type form) lcl) locs)
		 (incf *inline-blocks*))
	       (push (list (c1form-primary-type form) var) locs))))

	(CALL-GLOBAL
	 (let* ((fname (c1form-arg 0 form))
		(args (c1form-arg 1 form))
		(return-type (c1form-primary-type form))
		(arg-locs (inline-args args))
		(loc (inline-function fname arg-locs return-type)))
	   (if loc
	       ;; If there are side effects, we may not move the C form
	       ;; around and we have to save its value in a variable.
	       ;; We use a variable of type out-type to save the value
	       ;; if (return-type >= out-type)
	       ;; then
	       ;;   coerce the value to out-type
	       ;; otherwise
	       ;;   save the value without coercion and return the
	       ;;   variable tagged with and-type,
	       ;;   so that whoever uses it may coerce it to such type
	       (let* ((and-type (type-and return-type (loc-type loc)))
		      (out-rep-type (loc-representation-type loc))
		      (var (make-lcl-var :rep-type out-rep-type :type and-type)))
		 (wt-nl "{" (rep-type-name out-rep-type) " " var "= " loc ";")
		 (incf *inline-blocks*)
		 (setq loc var)
		 (push (list (loc-type loc) loc) locs))
	       ;; FIXME! Why is (make-temp-var) before rebinding of *temp*???
	       (let* ((temp (make-temp-var))
		      ;; bindings like c1expr*
		      (*exit* (next-label))
		      (*unwind-exit* (cons *exit* *unwind-exit*))
		      (*lcl* *lcl*)
		      (*temp* *temp*)
		      (*destination* temp))
		 (call-global fname nil nil arg-locs return-type)
		 (wt-label *exit*)
		 (push
		  (list (if (subtypep 'T return-type)
			    (or (get-return-type fname) 'T)
			    return-type)
			temp)
		  locs)))))

	(SYS:STRUCTURE-REF
	 (let ((type (c1form-primary-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (make-temp-var))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:STRUCTURE-REF
				 (first (coerce-locs
					  (inline-args (list (c1form-arg 0 form)))))
				 (c1form-arg 1 form)
				 (c1form-arg 2 form)
				 (c1form-arg 3 form)))
		     locs))))
	#+clos
	(SYS:INSTANCE-REF
	 (let ((type (c1form-primary-type form)))
	   (if (args-cause-side-effect (cdr forms))
	       (let* ((temp (make-temp-var))
		      (*destination* temp))
		 (c2expr* form)
		 (push (list type temp) locs))
	       (push (list type
			   (list 'SYS:INSTANCE-REF
				 (first (coerce-locs
					  (inline-args (list (c1form-arg 0 form)))))
				 (c1form-arg 1 form)
				#+nil (c1form-arg 2 form))) ; JJGR
		     locs))))
	(SETQ
	 (let ((vref (c1form-arg 0 form))
	       (form1 (c1form-arg 1 form)))
	   (let ((*destination* vref)) (c2expr* form1))
	   (if (eq (c1form-name form1) 'LOCATION)
	       (push (list (c1form-primary-type form1) (c1form-arg 0 form1)) locs)
	       (setq forms (list* nil	; discarded at iteration
				  (make-c1form 'VAR form vref)
				  (cdr forms))
		     ))))

	(t (let ((temp (make-temp-var)))
	     (let ((*destination* temp)) (c2expr* form))
	     (push (list (c1form-primary-type form) temp) locs))))))
  )

(defun destination-type ()
  (rep-type->lisp-type (loc-representation-type *destination*))
  ;;(loc-type *destination*)
)

;;;
;;; inline-function:
;;;   locs are typed locs as produced by inline-args
;;;   returns NIL if inline expansion of the function is not possible
;;;
(defun inline-function (fname inlined-locs return-type)
  ;; Those functions that use INLINE-FUNCTION must rebind
  ;; the variable *INLINE-BLOCKS*.
  (and (inline-possible fname)
       (not (get-sysprop fname 'C2))
       (let* ((ii (get-inline-info fname (mapcar #'first inlined-locs)
				   (type-and return-type (destination-type)))))
	 (when ii
	   (let* ((arg-types (inline-info-arg-types ii))
		  (out-rep-type (inline-info-return-rep-type ii))
		  (out-type (inline-info-return-type ii))
		  (side-effects-p (function-may-have-side-effects fname))
		  (fun (inline-info-expansion ii))
		  (one-liner (inline-info-one-liner ii)))
	     (produce-inline-loc inlined-locs arg-types (list out-rep-type)
				 fun side-effects-p one-liner))))))

(defun get-inline-info (fname types return-type &aux ii iia)
  (declare (si::c-local))
  (dolist (x *inline-functions*)
    (when (and (eq (car x) fname)
	       (setq ii (inline-type-matches (cdr x) types return-type)))
      (return-from get-inline-info ii)))
  (dolist (x (get-sysprop fname (if (safe-compile)
				    ':INLINE-SAFE
				    ':INLINE-UNSAFE)))
    (when (setq ii (inline-type-matches x types return-type))
      (return)))
  (dolist (x (get-sysprop fname ':INLINE-ALWAYS))
    (when (setq iia (inline-type-matches x types return-type))
      (return)))
  (if (and ii iia)
      ;; Choose the most specific inline form if two available
      (if (and (every #'type>=
		      (inline-info-arg-types ii)
		      (inline-info-arg-types iia))
	       ;; no contravariance here
	       (type>= (inline-info-return-type ii)
		       (inline-info-return-type iia))
	       (not (and (every #'equal
				(inline-info-arg-types ii)
				(inline-info-arg-types iia))
			 (equal (inline-info-return-type iia)
				(inline-info-return-type ii)))))
	  iia
	  ii)
      (or ii iia))
  )

(defun inline-type-matches (inline-info arg-types return-type
                                        &aux (rts nil)
					(number-max nil)
					(inline-return-type
					 (inline-info-return-type
					  inline-info)))
  ;; In sysfun.lsp optimizers must be listed with most specific cases last.
  (flet ((float-type-max (t1 t2)
	   (cond
	     ((null t1)
	      t2)
	     ((or (subtypep t1 'DOUBLE-FLOAT)
		  (subtypep t2 'DOUBLE-FLOAT))
	      'DOUBLE-FLOAT)
	     ((or (subtypep t1 'SINGLE-FLOAT)
		  (subtypep t2 'SINGLE-FLOAT))
	      'SINGLE-FLOAT)
	     #+short-float
	     ((or (subtypep t1 'SHORT-FLOAT)
		  (subtypep t2 'SHORT-FLOAT))
	      'SHORT-FLOAT)
	     #+long-float
	     ((or (subtypep t1 'LONG-FLOAT)
		  (subtypep t2 'LONG-FLOAT))
	      'LONG-FLOAT)
	     (t
	      'FIXNUM))))
    (if (and (do ((arg-types arg-types (cdr arg-types))
		  (types (inline-info-arg-types inline-info) (cdr types))
		  (arg-type)
		  (type))
		 ((or (endp arg-types) (endp types))
		  (and (endp arg-types) (endp types)))
	       (setq arg-type (car arg-types)
		     type (car types))
	       (cond ((eq type 'FIXNUM-FLOAT)
		      (cond ((type>= 'FIXNUM arg-type)
			     (push 'FIXNUM rts))
			    ((type>= 'DOUBLE-FLOAT arg-type)
			     (push 'DOUBLE-FLOAT rts))
			    ((type>= 'SINGLE-FLOAT arg-type)
			     (push 'SINGLE-FLOAT rts))
			    #+short-float
			    ((type>= 'SHORT-FLOAT arg-type)
			     (push 'SHORT-FLOAT rts))
			    #+long-float
			    ((type>= 'LONG-FLOAT arg-type)
			     (push 'LONG-FLOAT rts))
			    (t (return nil)))
		      ;; compute max of FIXNUM-FLOAT arguments types
		      (setq number-max
			    (float-type-max number-max (first rts))))
		     ((type>= (rep-type->lisp-type type) arg-type)
		      (push type rts))
		     (t (return nil))))
	     (or (eq (inline-info-return-rep-type inline-info) :bool)
		 (if number-max
		     ;; for arithmetic operators we take the maximal type
		     ;; as possible result type
		     (and (type>= return-type number-max)
			  (type>= number-max inline-return-type))
		     ;; no contravariance
		     (type>= inline-return-type return-type))))
	(let ((inline-info (copy-structure inline-info)))
	  (setf (inline-info-arg-types inline-info)
		(nreverse rts))
	  inline-info)
	nil))
  )

(defun need-to-protect (forms &aux ii)
  (do ((forms forms (cdr forms))
       (res nil))
      ((or res (endp forms)) res)
    (let ((form (car forms)))
      (declare (object form))
      (case (c1form-name form)
	(LOCATION)
	(VAR
	 (when (var-changed-in-form-list (c1form-arg 0 form) (cdr forms))
	   (setq res t)))
	(CALL-GLOBAL
	 (let ((fname (c1form-arg 0 form))
	       (args (c1form-arg 1 form)))
	   (or (function-may-have-side-effects fname)
	       (need-to-protect args))))
	(SYS:STRUCTURE-REF
	 (when (need-to-protect (list (c1form-arg 0 form)))
	   (setq res t)))
	(t (setq res t)))))
  )

(defun close-inline-blocks ()
  (dotimes (i *inline-blocks*) (declare (fixnum i)) (wt #\})))

(defun form-causes-side-effect (form)
  (case (c1form-name form)
    ((LOCATION VAR SYS:STRUCTURE-REF #+clos SYS:INSTANCE-REF)
     nil)
    (CALL-GLOBAL
     (let ((fname (c1form-arg 0 form))
	   (args (c1form-arg 1 form)))
       (or (function-may-have-side-effects fname)
	   (args-cause-side-effect args))))
    (t t)))

(defun args-cause-side-effect (forms)
  (some #'form-causes-side-effect forms))

(defun function-may-have-side-effects (fname)
  (declare (si::c-local))
  (not (get-sysprop fname 'no-side-effects)))

(defun function-may-change-sp (fname)
  (not (or (get-sysprop fname 'no-side-effects)
	   (get-sysprop fname 'no-sp-change))))
