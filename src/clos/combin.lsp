;;;;  Copyright (c) 1992, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "CLOS")

(defun get-method-qualifiers (method) (nth 1 method))

(defun get-method-function (method) (nth 4 method))

;;; They will be redefined later:
(proclaim '(notinline get-method-qualifiers get-method-function))


;;;
;;; Convert an effective method form to a compiled effective method function.
;;; The strategy is to have compiled functions around which are are templates
;;; for effective method functions.  Then the effective method functions we
;;; generate are closures over the particular methods in the effective method
;;; form.  This strategy has the advantage that we don't have to call the
;;; compiler when we combine new methods.  It also has the advantage that
;;; same shape effective methods share the same code vector.  It is of course
;;; predicated on the assumption that funcalling compiled closures is fast.
;;;
;;; *effective-method-templates* is a list of effective-method template
;;; entries.  Each entry is itself a list of the form:
;;; 
;;;   (<template> <match-function> <make-code-function> <when> <count>)
;;;
;;; The match function is simple-effective-method-match-p.
;;;
;;;

(defvar *effective-method-templates* ())

(defun make-effective-method-function (form)
  (if (and (listp form)
	   (eq (first form) 'CALL-METHOD)
	   (method-p (second form))
	   (every #'method-p (third form)))
      ;; The effective method is just a call to call-method.  This opens
      ;; up a possibility of just using the method function of the method
      ;; being called as the effective method function.
      ;;
      ;; But we have to be careful.  We must be sure to communicate the
      ;; next methods to the method if it needs them.  If there are no
      ;; next methods we must communicate that fact to prevent the leaky
      ;; next methods bug.
      (let ((method-function (get-method-function (second form))))
	(if (method-needs-next-methods-p (second form))
	      (let ((next-method-functions
		     (mapcar #'get-method-function (third form))))
		#'(lambda (&rest .combined-method-args.)
		    (let ((*next-methods* next-method-functions))
		      (apply method-function .combined-method-args.))))
	  method-function))
      ;; lookup or add an entry to *effective-method-templates*
      (let (entry)
	(dolist (e *effective-method-templates*)
	  (let ((matchp (funcall (symbol-function (second e)) (first e) form)))
	    (when matchp (return (setq entry e)))))
	(unless entry
	  ;; None of the recorded entries match.  Have to generate a new entry.
	  (setq entry
		(multiple-value-bind (template predicate constructor)
		    (compile-effective-method-template-entry form)
		  (list template predicate (compile () constructor)
			'ON-THE-FLY 0)))
	  (add-effective-method-template-entry entry))
	(incf (fifth entry))
	(funcall (third entry) form))))

(defun add-effective-method-template-entry (entry)
  ;; We keep the list of entries sorted so that the entries with complex
  ;; match functions stay at the end.  This prevents a newly defined
  ;; complex match function from slowing down all the more common cases.
  ;;
  (setq *effective-method-templates*
	(merge 'list
	       *effective-method-templates*
	       (list entry)
	       #'(lambda (a b)
		   (and (eq a 'SIMPLE-EFFECTIVE-METHOD-MATCH-P)
			(not (eq a b))))
	       :key #'second)))

(defmacro precompile-effective-method-template
    (effective-method &optional (when 'PRE-MADE))
  (multiple-value-bind (template predicate constructor)
      (compile-effective-method-template-entry effective-method)
    `(EVAL-WHEN (LOAD)
      (ADD-EFFECTIVE-METHOD-TEMPLATE-ENTRY
       (LIST ',template ',predicate (FUNCTION ,constructor) ',when 0)))))


(defun compile-effective-method-template-entry (form)
  (values (walk-form form
		     nil
		     #'(lambda (f c e)
			 (declare (ignore c e))
			 (if (not (consp f))
			     f
			     (if (eq (first f) 'CALL-METHOD)
				 (if (= (length f) 3)
				     '(_CALL-METHOD_)
				     (error "Wrong number of arguments to ~
                                                call-method."))
				 f))))
	  'SIMPLE-EFFECTIVE-METHOD-MATCH-P
	  (make-simple-effective-method-code-constructor form)))

(defun simple-effective-method-match-p (template form)
  (labels ((every* (fn l1 l2)
	     ;; This version of every is slightly different. It
	     ;; returns NIL if it reaches the end of one of the
	     ;; lists before reaching the end of the other.
	     (do ((t1 l1 (rest t1))
		  (t2 l2 (rest t2)))
		 (())
	       (cond ((null t1) (return (null t2)))
		     ((null t2) (return (null t1)))
		     ((funcall fn (first t1) (first t2)))
		     (t (return nil)))))
	   (walk (tm fm)
	     (cond ((eq tm fm) t)
		   ((and (listp tm)
			 (listp fm))
		    (if (eq (first fm) 'CALL-METHOD)
			(eq (first tm) '_CALL-METHOD_)
			(every* #'walk tm fm)))
		   ((and (stringp tm)
			 (stringp fm))
		    (string-equal tm fm))
		   (t nil))))
    (walk template form)))

(defun simple-code-walker (form env walk-function)
  (if (consp form)
      (catch form
	(let ((new (funcall walk-function form :eval nil)))
	  (walker::recons
	   new
	   (simple-code-walker (first new) env walk-function)
	   (simple-code-walker (rest new) env walk-function))))
      form))

;;;
;;; These two functions must to pass the symbols which name the functions
;;; not the actual functions.  This is done this way because we are going
;;; to have to compile forms which include them as constants.  Before the
;;; symbols are actually applied, symbol function is used to get the actual
;;; function.
;;; 
(defun make-simple-effective-method-code-constructor (form)
  (make-code-constructor form))

(defvar *combined-method-next-methods-gensyms* ())
(defvar *combined-method-method-function-gensyms* ())

(eval-when (eval load)
  (dotimes (i 10)
    (push (make-symbol (format nil ".METHOD-~A-NEXT-METHODS." (- 9 i)))
	  *combined-method-next-methods-gensyms*)
    (push (make-symbol (format nil ".METHOD-~A-FUNCTION." (- 9 i)))
	  *combined-method-method-function-gensyms*)))
    

(defun make-code-constructor (form)
  (let* ((method-vars ())
	 (code-body nil)
	 (next-method-gensyms *combined-method-next-methods-gensyms*)
	 (method-function-gensyms *combined-method-method-function-gensyms*))
    (flet ((convert-function (f c e)
	     (declare (ignore e))
	     (cond ((and (listp f)
			 (eq c ':eval))
		    (if (or (eq (first f) '_CALL-METHOD_)
			    (eq (first f) 'CALL-METHOD))
			(let ((gensym1 (or (pop method-function-gensyms)
					   (gensym)))
			      (gensym2 (or (pop next-method-gensyms)
					   (gensym))))
			  (push gensym1 method-vars)
			  (push gensym2 method-vars)
			  `(LET ((*NEXT-METHODS* ,gensym2))
			     (DECLARE (SPECIAL *NEXT-METHODS*))
			     (APPLY ,gensym1 .COMBINED-METHOD-ARGS.)))
			f))
		   ((method-p f)
		    (error "Effective method body must be malformed."))
		   (t f))))
      (setq code-body (simple-code-walker form nil #'convert-function))
      ;;
      ;; This is written in a slightly screwey way because of a bug in the
      ;; 3600 compiler.  Basically, if both of the funargs in the compiled
      ;; up function close over method-vars the 3600 compiler loses.
      ;; 
      `(LAMBDA (.FORM.)
	 (LET ((METHODS NIL) ,@method-vars)
	   (SIMPLE-CODE-WALKER
	    .FORM.
	    NIL
	    #'(lambda (f c e)
		(declare (ignore e))
		(if (and (eq c ':eval)
			 (listp f)
			 (eq (first f) 'CALL-METHOD))
		    (progn 
		      (push (get-method-function (second f)) methods)
		      (PUSH (THIRD F) methods)
		      (throw f f))
		    f)
		f))
	   ,@(do ((mvs method-vars (cddr mvs))
		  (setqs))
		 ((null mvs) (nreverse setqs))
		 (push `(SETQ ,(first mvs)
			      (mapcar #'convert-effective-method
				      (pop methods)))
		       setqs)
		 (push `(SETQ ,(second mvs) (pop methods)) setqs))
	   #'(LAMBDA (&REST .COMBINED-METHOD-ARGS.)
	       ,code-body))))))



(defun convert-effective-method (effective-method)
  (cond ((method-p effective-method)
	 (get-method-function effective-method))
	((and (listp effective-method)
	      (eq (first effective-method) 'MAKE-METHOD))
	 (make-effective-method-function
	   (make-progn (second effective-method))))
	(t
	 (error "Effective-method form is malformed."))))

(defun make-method-call (method &optional next-methods)
  `(CALL-METHOD ,method ,next-methods))

(defun make-progn (&rest forms)
  (let ((progn-form nil))
    (labels ((collect-forms (forms)
	       (when forms
		 (collect-forms (rest forms))
		 (if (and (listp (first forms))
			  (eq (caar forms) 'PROGN))
		     (collect-forms (cdar forms))
		     (push (first forms) progn-form)))))
      (collect-forms forms)
      (cons 'PROGN progn-form))))


(defun compute-combined-method (gf methods)
  (declare (ignore gf))
  (let ((before ())
	(primary ())
	(after ())
	(around ()))
    (dolist (m methods)
      (let ((qualifiers (get-method-qualifiers m)))
	(cond ((member :BEFORE qualifiers) (push m before))
	      ((member :AFTER  qualifiers) (push m after))
	      ((member :AROUND  qualifiers) (push m around))
	      (t
	       (push m primary)))))
    (setq before (nreverse before)
	  after (nreverse after)
	  primary (nreverse primary)
	  around (nreverse around))

    (make-effective-method-function
     (if (and (null before)
	      (null after))
	 (if (null around)
	     ;; By returning a single call-method `form' here we enable
	     ;; an important implementation-specific optimization.
	     `(CALL-METHOD ,(first primary) ,(rest primary))
	     `(CALL-METHOD ,(first around) ,(append (rest around) primary)))
	 (let ((main-effective-method
		`(PROGN ,@(mapcar #'make-method-call before)
		  (MULTIPLE-VALUE-PROG1
		      ,(if primary
			   `(CALL-METHOD ,(first primary) ,(rest primary))
			   '(error "No applicable primary method."))
		    ,@(mapcar #'make-method-call after)))))
	   (if around
	       `(CALL-METHOD ,(first around)
		 (,@(rest around)
		  (MAKE-METHOD ,main-effective-method)))
	       main-effective-method))))))
