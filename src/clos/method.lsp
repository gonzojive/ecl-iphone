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

(defvar *method-size* 32)		; Size of methods hash tables

;;; This holds fake methods created during bootstrap.
;;; It is  an alist of:
;;;	(method-name {method}+)
(defvar *early-methods* nil)

;;;
;;; This is used by combined methods to communicate the next methods to
;;; the methods they call.  This variable is captured by a lexical variable
;;; of the methods to give it the proper lexical scope.
;;; 
(defvar *next-methods* nil)


;;; ----------------------------------------------------------------------
;;; defmethod

;;; For setf methods the syntax is:
;;;    (defmethod (setf foo) ((nv type) (x1 type1) ... (xn typen))
;;;       ...)
;;; where nv is the new value to be assigned.

(defmacro defmethod (&rest args &environment env)

  ;; args = name {method-qualifier}* specialized-lambda-list &body body

  (multiple-value-bind (name qualifiers lambda-list body)
    (parse-defmethod args)
    (multiple-value-bind (fn-form specializers doc plist)
      (expand-defmethod name qualifiers lambda-list body env)
      (multiple-value-bind (parameters specialized-lambda-list specializers)
	(parse-specialized-lambda-list lambda-list nil)
	(declare (ignore parameters))
	`(PROGN
	  #+PDE
	  (EVAL-WHEN
	      (LOAD)
	    (SI:RECORD-SOURCE-PATHNAME
	     ',name '(DEFMETHOD ',qualifiers ',specializers)))
	  (EVAL-WHEN (COMPILE LOAD EVAL)
	    (INSTALL-METHOD
	     ',name
	     ',qualifiers
	     ',specializers
	     ',specialized-lambda-list
	     ',doc
	     ',plist
	     ,fn-form)
	    ))))))


;;; ----------------------------------------------------------------------
;;;                                                  method body expansion

(defun expand-defmethod
    (generic-function-name qualifiers specialized-lambda-list body env)
  ;; (values fn-form specializers doc)
  (declare (ignore qualifiers)
	   (si::c-local))
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
    (multiple-value-bind (parameters lambda-list specializers)
	(parse-specialized-lambda-list specialized-lambda-list 't)

      (let* ((required-parameters
	      (mapcar #'(lambda (r s) (declare (ignore s)) r)
		      parameters
		      specializers))
	     (class-declarations 
	      `(declare
		,@(mapcan #'(lambda (p s) (and (symbolp s) s
					       (not (eq s 't))
					       `((type ,s ,p))))
			  parameters
			  specializers)))
	     (method-lambda 
	      ;; Remove the documentation string and insert the
	      ;; appropriate class declarations.  The documentation
	      ;; string is removed to make it easy for us to insert
	      ;; new declarations later, they will just go after the
	      ;; second of the method lambda.  The class declarations
	      ;; are inserted to communicate the class of the method's
	      ;; arguments to the code walk.
	      `(lambda-block ,(if (listp generic-function-name)
				  (second generic-function-name)
				  generic-function-name)
		,lambda-list
		,class-declarations
		,@declarations
		;; (progn ,@parameters-to-reference)
		,@real-body))

	     (call-next-method-p nil)	;flag indicating that call-next-method
					;should be in the method definition
	     (next-method-p-p nil)	;flag indicating that next-method-p
					;should be in the method definition
	     (save-original-args nil)	;flag indicating whether or not the
					;original arguments to the method
					;must be preserved.  This happens
					;for two reasons:
					; - the method takes &mumble args,
					;   so one of the lexical functions
					;   might be used in a default value
					;   form
					; - call-next-method is used without
					;   arguments at least once in the
					;   body of the method
	     (original-args ())
	     (applyp nil)		; flag indicating whether or not the
					; method takes &mumble arguments. If
					; it does, it means call-next-method
					; without arguments must be APPLY'd
					; to original-args.  If this gets set
					; true, save-original-args is set so
					; as well
	     (aux-bindings ())		; Suffice to say that &aux is one of
					; damndest things to have put in a
					; language.
	     (slots (mapcar #'list required-parameters))
					; records for each class:
					; - slot-index-table
					; - slot-indexes for each slot
					; See optimize-standard-instance-access.
	     (plist ())
	     (walked-lambda nil))
	(flet ((walk-function (form context env)
		 (cond ((not (eq context ':EVAL)) form)
		       ((not (listp form)) form)
		       ((eq (car form) 'CALL-NEXT-METHOD)
			(setq call-next-method-p 't)
			(setq save-original-args (not (cdr form)))
			form)
		       ((eq (car form) 'NEXT-METHOD-P)
			(setq next-method-p-p 't)
			form)
		       ((and (eq (car form) 'FUNCTION)
			     (case (second form)
			       (CALL-NEXT-METHOD
				(setq call-next-method-p 'T)
				(setq save-original-args 'T)
				form)
			       (NEXT-METHOD-P
				(setq next-method-p-p 'T)
				form)
			       (t nil))))
		       ((and (eq (car form) 'SLOT-VALUE)
			     (symbolp (second form))
			     (constantp (third form)))
			(multiple-value-bind (ignore class)
			    (can-optimize-access (second form) env)
			  (if class
			      (optimize-slot-value class form)
			      form)))
		       ;; does not work for (push x (slot-value y 's))
		       ;; and similia, since push is turned into
		       ;; (|(setf slot-value)| (cons (slot-value y 's) x) x 's)
		       ((eq (car form) 'SETF)
			(if (cdddr form)
			    (do* ((setf-list (cdr form) (cddr setf-list))
				  (instance-access)
				  (value)
				  (result))
				 ((null setf-list)
				  (cons 'PROGN (nreverse result)))
			      (setq instance-access (car setf-list)
				    value (second setf-list))
			      (push
			       (if (and instance-access
					(listp instance-access)
					(eq (car instance-access)
					    'SLOT-VALUE)
					(symbolp (second instance-access))
					(constantp (third instance-access)))
				   (multiple-value-bind (ignore class)
				       (can-optimize-access
					(second instance-access) env)
				     (let ((new-form
					    (list 'SETF instance-access
						  value)))
				       (if class
					   (optimize-set-slot-value class
								    new-form)
					   new-form)))
				   (list 'SETF instance-access value))
			       result))
			    (if (and (cdr form)
				     (second form)
				     (listp (second form))
				     (eq (caadr form) 'SLOT-VALUE)
				     (symbolp (cadadr form))
				     (constantp (third (second form))))
				(multiple-value-bind (ignore class)
				    (can-optimize-access (cadadr form) env)
				  (if class
				      (optimize-set-slot-value class form)
				      form))
				form)))

		       ((eq (car form) 'STANDARD-INSTANCE-ACCESS)
			(multiple-value-bind (parameter class)
			    (can-optimize-access (second form) env)
			  (if class
			      (optimize-standard-instance-access
			       class parameter form slots)
			      form)))
		       (t form))))

	  (setq walked-lambda (walk-form method-lambda env #'walk-function))

	  ;; Scan the lambda list to determine whether this method
	  ;; takes &mumble arguments.  If it does, we set applyp and
	  ;; save-original-args true.
	  ;; 
	  ;; This is also the place where we construct the original
	  ;; arguments lambda list if there has to be one.
	  (dolist (p lambda-list)
	    (if (member p '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)
			:test #'eq)	; cant use lambda-list-keywords
		(if (eq p '&aux)
		    (progn
		      (setq aux-bindings (cdr (member '&AUX lambda-list
						      :test #'eq)))
		      (return nil))
		    (progn
		      (setq applyp t
			    save-original-args t)
		      (push '&REST original-args)
		      (push (make-symbol "AMPERSAND-ARGS") original-args)
		      (return nil)))
		(push (make-symbol (symbol-name p)) original-args)))
	  (setq original-args (when save-original-args
				(nreverse original-args)))
	  
	  (multiple-value-bind (walked-declarations walked-lambda-body)
	      (sys::find-declarations (cdddr walked-lambda) t)
	    (declare (ignore ignore))
	    
	    (when (some #'cdr slots)	; there are optimized slot accesses
	      (setq walked-lambda-body
		    (add-index-binding walked-lambda-body slots)))
	    (when (or next-method-p-p call-next-method-p)
	      (setq plist (list* :needs-next-methods-p 'T plist)))

	    (values
	     `(function ,(if (or call-next-method-p next-method-p-p)
			     (add-lexical-functions-to-method-lambda
			      walked-declarations
			      walked-lambda-body
			      `(lambda-block ,(second walked-lambda)
				,lambda-list
				,@walked-declarations
				,.walked-lambda-body)
			      original-args
			      lambda-list
			      save-original-args
			      applyp
			      aux-bindings
			      call-next-method-p
			      next-method-p-p)
			     `(lambda-block ,(second walked-lambda)
			       ,lambda-list
			       ,@walked-declarations
			       ,.walked-lambda-body)))
	     specializers
	     documentation
	     plist)))))))

(defun add-lexical-functions-to-method-lambda (walked-declarations
					       walked-lambda-body
					       walked-lambda
					       original-args
					       lambda-list
					       save-original-args
					       applyp
					       aux-bindings
					       call-next-method-p
					       next-method-p-p)
  (declare (si::c-local))
  (cond ((and (null save-original-args)
	      (null applyp))
	 ;;
	 ;; We don't have to save the original arguments.  In addition,
	 ;; this method doesn't take any &mumble arguments (this means
	 ;; that there is no way the lexical functions can be used inside
	 ;; of the default value form for an &mumble argument).
	 ;;
	 ;; We can expand this into a simple lambda expression with an
	 ;; FLET to define the lexical functions.
	 ;; 
	 `(lambda ,lambda-list
	    ,@walked-declarations
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (flet (,@(and call-next-method-p
;;;
;;; WARNING: these &rest/apply combinations produce useless garbage. Beppe
;;;
			    '((CALL-NEXT-METHOD (&REST CNM-ARGS)
				;; (declare (static-extent cnm-args))
				(IF .NEXT-METHOD.
				    (APPLY .NEXT-METHOD. CNM-ARGS)
				    (ERROR "No next method.")))))
		     ,@(and next-method-p-p
			    '((NEXT-METHOD-P ()
				(NOT (NULL .NEXT-METHOD.))))))
		,@walked-lambda-body)))

;;; Assuming that we can determine statically which is the next method,
;;; we could use this solution. Compute-effective-method can set
;;; the value of .next-method. within each closure at the appropriate
;;; value. Same thing for next case. 	Beppe
;	 `(let (.next-method.)
;	    (lambda ,lambda-list
;	      ,@walked-declarations
;	      (flet (,@(and call-next-method-p
;			    '((CALL-NEXT-METHOD (&REST CNM-ARGS)
;				;; (declare (static-extent cnm-args))
;				(IF .NEXT-METHOD.
;				    (APPLY .NEXT-METHOD. CNM-ARGS)
;				    (ERROR "No next method.")))))
;		     ,@(and next-method-p-p
;			    '((NEXT-METHOD-P ()
;				(NOT (NULL .NEXT-METHOD.))))))
;		,@walked-lambda-body)))
	 )
	((null applyp)
	 ;;
	 ;; This method doesn't accept any &mumble arguments.  But we
	 ;; do have to save the original arguments (this is because
	 ;; call-next-method is being called with no arguments).
	 ;; Have to be careful though, there may be multiple calls to
	 ;; call-next-method, all we know is that at least one of them
	 ;; is with no arguments.
	 ;; 
	 `(lambda ,original-args
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (flet (,@(and call-next-method-p
                            `((call-next-method (&rest cnm-args)
				;; (declare (static-extent cnm-args))
				(if .next-method.
				    (if cnm-args
					(apply .next-method. cnm-args)
					(funcall .next-method. ,@original-args))
				    (error "No next method.")))))
		     ,@(and next-method-p-p
			    '((NEXT-METHOD-P ()
				(NOT (NULL .NEXT-METHOD.))))))
		(let* (,@(mapcar #'list
				 (subseq lambda-list 0
					 (position '&AUX lambda-list))
				 original-args)
		       ,@aux-bindings)
		  ,@walked-declarations
		  ,@walked-lambda-body)))))
	(t
	 ;;
	 ;; This is the fully general case.
	 ;; We must allow for the lexical functions being used inside
	 ;; the default value forms of &mumble arguments, and if must
	 ;; allow for call-next-method being called with no arguments.
	 ;; 
	 `(lambda ,original-args
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (flet (,@(and call-next-method-p
			    `((call-next-method (&rest cnm-args)
				;; (declare (static-extent cnm-args))
				(if .next-method.
				    (if cnm-args
					(apply .next-method. cnm-args)
					(apply .next-method. 
					       ,@(remove '&REST original-args)))
				    (error "No next method.")))))
		     ,@(and next-method-p-p
			    '((NEXT-METHOD-P ()
				(NOT (NULL .NEXT-METHOD.))))))
		(apply (function ,walked-lambda)
		       ,@(remove '&REST original-args))))))))

#|
(defun make-parameter-references (specialized-lambda-list
				  required-parameters
				  declarations
				  generic-function-name
				  specializers)
  (flet ((ignoredp (symbol)
	   (dolist (decl (cdar declarations))
	     (when (and (eq (car decl) 'IGNORE)
			(member symbol (cdr decl) :test #'eq))
	       (return t)))))	   
    (do ((sscan specialized-lambda-list (cdr sscan))
	 (pscan required-parameters (cdr pscan))
	 (references nil))
	((null sscan) (nreverse references))
	(cond ((not (listp (car sscan))))
	      ((ignoredp (caar sscan))
	       (warn "In defmethod ~S ~S, there is a~%~
                      redundant ignore declaration for the parameter ~S."
		     generic-function-name
		     specializers
		     (caar sscan)))
	      (t (push (caar sscan) references))))))
|#

;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun legal-generic-function-name-p (name)
  (or (symbolp name) (si:setf-namep name)))

(defun parse-defmethod (args)
  (declare (si::c-local))
  ;; (values name qualifiers arglist body)
  (let* (name qualifiers)
    (unless args
      (error "Illegal defmethod form: missing method name"))
    (setq name (pop args))
    (unless (legal-generic-function-name-p name)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is setf and whose second is a non-nil symbol."
	     name))
    (unless args
      (error "Illegal defmethod form: missing lambda-list"))
    (loop (cond ((null (first args))
		 (error "Illegal defmethod form: null lambda-list"))
		((consp (first args))
		 (return (setq qualifiers (nreverse qualifiers))))
		(t (push (pop args) qualifiers))))
    (values name qualifiers (first args) (rest args))))

(defun parse-specialized-lambda-list (arglist warningp)
  (declare (si::c-local))
  ;; This function has been modified to get an easy control on the
  ;; correctness of the specialized-lambda-list. Furthermore it has became
  ;; an iterative function.
  ;; -- Daniele --
  (let* (parameters lambda-list specializers)
    (do ((arg (first arglist) (first arglist)))
	((or (null arglist)
	     (member arg '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX))))
      (pop arglist)
      (when (and warningp (member arg lambda-list-keywords))
	(warn "Unrecognized lambda-list keyword ~S in arglist.~%~
                        Assume this keyword as a required parameter."
	      arg))
      (push (if (listp arg) (first arg) arg) parameters)
      (push (if (listp arg) (first arg) arg) lambda-list)
      (push (if (listp arg) (if (consp (second arg))
				`(eql ,(eval (cadadr arg)))
				(second arg))
		())
	    specializers))
    (when (eq (first arglist) '&OPTIONAL)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (member arg '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS
			     &AUX))))
	(pop arglist)
	(when (and warningp (member arg lambda-list-keywords))
	  (warn "Unrecognized lambda-list keyword ~S in arglist.~%~
                           Assume this keyword as an optional parameter."
		arg))
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when (eq (first arglist) '&REST)
      (push (pop arglist) lambda-list)
      (when (not (symbolp (first arglist)))
	(error "~S in the lambda-list is not a symbol."
	       (first arglist)))
      (push (pop arglist) lambda-list))
    (when (eq (first arglist) '&KEY)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (member arg '(&OPTIONAL &REST &KEY &AUX))))
	(pop arglist)
	(when (eq arg '&ALLOW-OTHER-KEYS)
	  (push arg lambda-list)
	  (return))
	(when (and warningp (member arg lambda-list-keywords))
	  (warn "Unrecognized lambda-list keyword ~S in arglist.~%~
                           Assume this keyword as a keyword parameter."
		arg))
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when (eq (first arglist) '&AUX)
      (push (pop arglist) lambda-list)
      (do ((arg (first arglist) (first arglist)))
	  ((or (null arglist)
	       (member arg '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS
			     &AUX))))
	(pop arglist)
	(when (and warningp (member arg lambda-list-keywords))
	  (warn "Unrecognized lambda-list keyword ~S in arglist.~%~
                           Assume this keyword as an aux parameter."
		arg))
	(push (if (listp arg) (first arg) arg) parameters)
	(push arg lambda-list)))
    (when arglist (error "The position of the lambda-list keyword ~S~%~
                             is not correct."
			 (first arglist)))
    (values (nreverse parameters)
	    (nreverse lambda-list)
	    (nreverse specializers))))

(defun declaration-specializers (arglist declarations)
  (declare (si::c-local))
  (do ((argscan arglist (cdr argscan))
       (declist (when declarations (cdr declarations))))
      ((or
	(null argscan)
	(member (first argscan) '(&OPTIONAL &REST &KEY &ALLOW-OTHER-KEYS &AUX)))
       `(DECLARE ,@declist))
      (when (listp (first argscan))
	    (push `(TYPE ,(cadar argscan) ,(caar argscan)) declist))))


;;; ----------------------------------------------------------------------
;;;                                                             operations

;;; early version used during bootstrap
(defstruct (method (:type list)
		   (:constructor make-method
				 (qualifiers specializers lambda-list
					     function plist options
					     &rest ignore)))
  (class-name 'STANDARD-METHOD)
  qualifiers specializers lambda-list function plist options)

;;; early version used during bootstrap
(defun method-p (x) (and (listp x) (eq 'STANDARD-METHOD (first x))))

;;; early version used during bootstrap
(defun method-needs-next-methods-p (method)
  (getf (nth 5 method) :needs-next-methods-p))

(defun generic-function-dispatcher (gf) (si:instance-ref gf 6))

;;; early version used during bootstrap
(defun add-method (gf method)
  (let* ((name (si:gfun-name (generic-function-dispatcher gf)))
	 (method-entry (assoc name *early-methods*)))
    (unless method-entry
      (setq method-entry (list name))
      (push method-entry *early-methods*))
    (push method (cdr method-entry))
    method))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (declare (notinline method-qualifiers))
  (let* ((method-list (methods gf))
	 (required-args (subseq (lambda-list gf) 0 (length specializers)))
	 found)
    (dolist (method method-list)
      (when (and (equal qualifiers (method-qualifiers method))
		 (equal specializers (specializers method)))
	(setq found method)
	(return)))
    (if (and (not found) errorp)
	(error "There is no method on the generic function ~S that agrees on 
               qualifiers ~S and specializers ~S"
	       (si:gfun-name (generic-function-dispatcher gf))
	       qualifiers specializers)
	found)))


;;; ----------------------------------------------------------------------
;;;                                                              bootstrap

(defvar *method-key-hash-table* (make-hash-table :test #'eq :size 32)
  ; the hash table containing the keywords accepted by
  ; allocate-instance and initialize-instance methods
  )

(defun update-method-key-hash-table (class lambda-list)
  (declare (si::c-local))
  (let* (post-key-list keywords-list)
    ;; search &key in lambda-list
    (setq post-key-list
	  (do ((scan lambda-list (cdr scan)))
	      ((null scan))
	      (when (eq (first scan) '&KEY)
		(return (cdr scan)))))
    ;; extract keywords from post-key-list
    (when post-key-list
      (do* ((key-scan post-key-list (cdr key-scan))
	    (first-key-scan (first key-scan) (first key-scan)))
	   ((or (null key-scan)
		(member first-key-scan '(&ALLOW-OTHER-KEYS &AUX)))
	    (setq keywords-list (nreverse keywords-list)))
	(push (if (listp first-key-scan) 
		  (let ((key-par (first first-key-scan)))
		    (if (listp key-par) (first key-par)
			(make-keyword key-par)))
		  (make-keyword first-key-scan))
	      keywords-list))
      ;; insert in the hash table a new entry whose keyword is the class name
      ;; and whose value is the keyword list accepted by the method
      (setf (gethash class *method-key-hash-table*) keywords-list))))


;;; ----------------------------------------------------------------------
;;;                                                             with-slots

(defmacro with-slots (slot-entries instance-form &body body)
  (let* ((temp (gensym))
	 (accessors
	  (do ((scan slot-entries (cdr scan))
	       (res))
	      ((null scan) (nreverse res))
	      (if (symbolp (first scan))
		  (push `(,(first scan) (slot-value ,temp ',(first scan))) res)
		(push `(,(caar scan)
			(slot-value ,temp ',(cadar scan))) res)))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))

;(with-slots (x (y2 y)) inst (setq x y2))

;;; ----------------------------------------------------------------------
;;;                                                         with-accessors

(defmacro with-accessors (slot-accessor-pairs instance-form &body body)
  (let* ((temp (gensym))
	 (accessors (do ((scan slot-accessor-pairs (cdr scan))
			(res))
		       ((null scan) (nreverse res))
		       (push `(,(caar scan) (,(cadar scan) ,temp)) res))))
    `(let ((,temp ,instance-form))
       (symbol-macrolet ,accessors ,@body))))


;;; ----------------------------------------------------------------------
;;;                                                              bootstrap

(defun si:compute-applicable-methods (gf args)
  (let* ((gfun (generic-function-dispatcher gf))
	 (name (si:gfun-name gfun))
	 (setfp (and (listp name) (eq 'SETF (first name))))
	 (early-methods (cdr (assoc name *early-methods*)))
	 methods)
    (labels ((search-early-methods (class)
	       (let* ((class-name (class-name class))
		      (method (find (if setfp
					(list 'T class-name)
					(list class-name))
				    early-methods
				    :key #'method-specializers
				    :test #'(lambda (x y)
					      (if setfp
						  (eq (second x) (second y))
						  (eq (first x) (first y)))))))
		 (when method (push method methods))
		 ;; search in the superclasses
		 (dolist (c (class-superiors class))
		   (search-early-methods c)))))
      (search-early-methods
	   (find-class (type-of (if setfp (second args) (first args)))))
      (nreverse methods))))


(defun si:generic-function-method-combination (gf))

(defun si:generic-function-method-combination-args (gf))

(defun si:compute-effective-method (gf applicable-methods 
				    method-combination-type
				    method-combination-args)
  (declare (ignore method-combination-type method-combination-args))
  ; the simplest case
  (if applicable-methods
      (make-effective-method-function
       `(call-method ,(first applicable-methods)
		     ,(cdr applicable-methods)))
    (no-applicable-method gf)))



;;; ----------------------------------------------------------------------
;;;                                                             optimizers

(defun can-optimize-access (var env)
  (declare (si::c-local))
  ;; (values required-parameter class)
  (let ((required-parameter?
	  (or (third (variable-declaration 'VARIABLE-REBINDING var env))
	      var)))
    (if required-parameter?
	(values required-parameter?
		(find-class (variable-class required-parameter? env) 'NIL))
	(values nil nil))))


(defun optimize-standard-instance-access (class parameter form slots)
  (declare (si::c-local))
  ;; Returns an optimized form corresponding to FORM.
  ;; SLOTS is a list of:
  ;;	(parameter [(class . class-index-table) {(slot-name . slot-index)}+])
  ;; parameters of the same class share the cdr of such list.
  ;;
  (let* ((instance (second form))
	 (slot-name (reduce-constant (third form)))
	 (new (fourth form))
	 (entry (assoc parameter slots :test #'eq))
	 slot)
    (unless entry
      (error "Can't optimize instance access.  Report this as a bug."))
    (setq slot (find slot-name (slot-value class 'SLOTS)
		     :key #'slotd-name))
    (unless slot
      (error "Slot ~A not present in class ~A." slot-name class))
    (if (eq :INSTANCE (slotd-allocation slot))
	(let* (slot-entry slot-index)
	  (unless (cdr entry)
	    ;; there is just one index-table for each different class
	    (let ((class-slot-info (find class slots :key #'caadr :test #'eq)))
	      (setf (cdr entry)
		    (if class-slot-info
			(cdr class-slot-info)
			;; create variable for index-table
			(list (cons class (gensym)))))))
	  (setq slot-entry (assoc slot-name (cddr entry) :test #'eq))
	  (if slot-entry
	      (setq slot-index (cdr slot-entry))
	      (push (cons slot-name (setq slot-index (gensym)))
		    (cddr entry)))
	  (if new
	      `(si:instance-set ,instance ,slot-index ,new)
	      `(the ,(slotd-type slot)
		(si:instance-ref-safe ,instance ,slot-index))))
	;; dont'optimize shared slots
	(if new
	    `(standard-instance-set ,new ,instance ',slot-name)
	    `(standard-instance-get ,instance ',slot-name)))))

;(defun get-slotd-type (class slot)
;  (slotd-type (find slot (slot-value class 'SLOTS) :key #'slotd-name)))

(defun signal-slot-unbound (instance slot-name)
  (declare (si::c-local))
  (slot-unbound (si:instance-class instance) instance slot-name))

(defun add-index-binding (method-body isl)
  (declare (si::c-local))
  (let* (class-index-bindings
	 slot-index-bindings
	 slot-index-declarations)

    ;; don't forget setf! Chicca
    (setf class-index-bindings
	  (dolist (entry isl (nreverse class-index-bindings))
	    ;; check if the entry provides the information needed! Chicca
	    (when (cdr entry)
	      (unless (assoc (cdadr entry) class-index-bindings :test #'eq)
		(push `(,(cdadr entry)
			(let ((class (si:instance-class ,(first entry))))
			  (declare (type standard-class class))
			  (slot-index-table class)))
		      class-index-bindings)))))

    ;; don't forget setf! Chicca
    (setf slot-index-bindings 
	  (dolist (entry isl (nreverse slot-index-bindings))
	    (dolist (slot-entry (cddr entry))
	      (push `(,(cdr slot-entry)
		      (slot-index ',(first slot-entry) ,(cdadr entry)))
		    slot-index-bindings)
	      (push `(fixnum ,(cdr slot-entry))
		    slot-index-declarations))))

    `((let ,class-index-bindings
	(let ,slot-index-bindings
	  (declare . ,slot-index-declarations)
	  . ,method-body)))))


;;; Force the compiler into optimizing use of gethash inside methods:
(setf (symbol-function 'SLOT-INDEX) (symbol-function 'GETHASH))
(setf (get 'SLOT-INDEX ':INLINE-ALWAYS)
       '(((T T) FIXNUM NIL NIL "fix(gethash(#0,#1))")
	 ((T T) T NIL NIL "(gethash(#0,#1))")))

(defun reduce-constant (old)
  (let ((new (eval old)))
    (if (eq new old)
	new
	(if (constantp new)
	    (reduce-constant new)
	    new))))

;;; ----------------------------------------------------------------------
;;;                                                                 walker

;;; Inform the walker of the kind of declarations to consider:

(pushnew 'TYPE *variable-declarations*)
(pushnew 'VARIABLE-REBINDING *variable-declarations*)

(defun variable-class (var env)
  (second (variable-declaration 'TYPE var env)))

;;; ----------------------------------------------------------------------
