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
;;; the methods they call.
;;;
(defvar *next-methods* nil)


;;; ----------------------------------------------------------------------
;;; DEFMETHOD
;;;
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
	  (INSTALL-METHOD
	     ',name
	     ',qualifiers
	     ',specializers
	     ',specialized-lambda-list
	     ',doc
	     ',plist
	     ,fn-form)
	    )))))


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

      ;; FIXME!! This deactivates the checking of keyword arguments
      ;; inside methods. The reason is that this checking must be
      ;; supplemented the knowledge of the keyword arguments of all
      ;; applicable methods (X3J13 7.6.5). Therefore, we should insert
      ;; that check, either in the method itself so that it is done
      ;; incrementally, or in COMPUTE-EFFECTIVE-METHOD.
      (when (and (member '&key lambda-list)
		 (not (member '&allow-other-keys lambda-list)))
	(let ((x (position '&aux lambda-list)))
	  (setf lambda-list
		(append (subseq lambda-list 0 x)
			'(&allow-other-keys)
			(and x (subseq lambda-list x))))))
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
	      `(ext::lambda-block ,generic-function-name
		,lambda-list
		,class-declarations
		,@declarations
		,@real-body))

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
	     (plist ()))
	(multiple-value-bind (walked-lambda call-next-method-p
			      save-original-args next-method-p-p)
	    (walk-method-lambda method-lambda required-parameters env slots)

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
	     (let ((walked-lambda `(ext::lambda-block ,(second walked-lambda)
				    ,lambda-list
				    ,@walked-declarations
				    ,.walked-lambda-body)))
	       (if (or call-next-method-p next-method-p-p)
		   `(function ,(add-lexical-functions-to-method-lambda
				walked-declarations
				walked-lambda-body
				generic-function-name
				walked-lambda
				original-args
				lambda-list
				save-original-args
				applyp
				aux-bindings
				call-next-method-p
				next-method-p-p))
		   `(function ,walked-lambda)))
	     specializers
	     documentation
	     plist)))))))

(defun walk-method-lambda (method-lambda required-parameters env slots)
  (declare (si::c-local))
  (let ((call-next-method-p nil);flag indicating that call-next-method
				;should be in the method definition
	(next-method-p-p nil)	;flag indicating that next-method-p
				;should be in the method definition
	(save-original-args nil);flag indicating whether or not the
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
	(closurep nil)		;flag indicating whether #'call-next-method
				;was seen in the code
	)
    (flet ((walk-function (form context env)
	     (cond ((not (eq context ':EVAL)) form)
		   ((not (listp form)) form)
		   ((eq (car form) 'CALL-NEXT-METHOD)
		    (setq call-next-method-p 'T
			  save-original-args (not (cdr form)))
		    form)
		   ((eq (car form) 'NEXT-METHOD-P)
		    (setq next-method-p-p 'T)
		    form)
		   ((and (eq (car form) 'FUNCTION)
			 (case (second form)
			   (CALL-NEXT-METHOD
			    (setq call-next-method-p 'T
				  closurep 'T
				  save-original-args 'T)
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
      (values (walk-form method-lambda env #'walk-function)
	      (if closurep 'FUNCTION call-next-method-p)
	      save-original-args
	      next-method-p-p))))

(defun add-lexical-functions-to-method-lambda (walked-declarations
					       walked-lambda-body
					       generic-function-name
					       walked-lambda
					       original-args
					       lambda-list
					       save-original-args
					       applyp
					       aux-bindings
					       call-next-method-p
					       next-method-p-p)
  (declare (si::c-local))
  ;;
  ;; WARNING: these &rest/apply combinations produce useless garbage. Beppe
  ;;
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
	 `(ext::lambda-block ,generic-function-name ,lambda-list
	    ,@walked-declarations
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (flet (,@(and call-next-method-p
			    '((CALL-NEXT-METHOD (&REST CNM-ARGS)
				;; (declare (static-extent cnm-args))
				(IF .NEXT-METHOD.
				    (APPLY .NEXT-METHOD. CNM-ARGS)
				    (ERROR "No next method.")))))
		     ,@(and next-method-p-p
			    '((NEXT-METHOD-P ()
				(NOT (NULL .NEXT-METHOD.))))))
		,@walked-lambda-body)))
	 ;; Assuming that we can determine statically which is the next method,
	 ;; we could use this solution. Compute-effective-method can set
	 ;; the value of .next-method. within each closure at the appropriate
	 ;; value. Same thing for next case. 	Beppe
	 ;;	 `(let (.next-method.)
	 ;;	    (lambda ,lambda-list
	 ;;	      ,@walked-declarations
	 ;;	      (flet (,@(and call-next-method-p
	 ;;			    '((CALL-NEXT-METHOD (&REST CNM-ARGS)
	 ;;				;; (declare (static-extent cnm-args))
	 ;;				(IF .NEXT-METHOD.
	 ;;				    (APPLY .NEXT-METHOD. CNM-ARGS)
	 ;;				    (ERROR "No next method.")))))
	 ;;		     ,@(and next-method-p-p
	 ;;			    '((NEXT-METHOD-P ()
	 ;;				(NOT (NULL .NEXT-METHOD.))))))
	 ;;		,@walked-lambda-body)))
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
	 `(ext::lambda-block ,generic-function-name ,original-args
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


;;; ----------------------------------------------------------------------
;;;                                                                parsing

(defun legal-generic-function-name-p (name)
  (si::valid-function-name-p name))

(defun parse-defmethod (args)
  (declare (si::c-local))
  ;; This function has to extract the name of the method, a list of
  ;; possible qualifiers (identified by not being lists), the lambda
  ;; list of the method (which might be empty!) and the body of the
  ;; function.
  (let* (name)
    (unless args
      (error "Illegal defmethod form: missing method name"))
    (setq name (pop args))
    (unless (legal-generic-function-name-p name)
      (error "~A cannot be a generic function specifier.~%~
             It must be either a non-nil symbol or ~%~
             a list whose car is setf and whose second is a non-nil symbol."
	     name))
    (do ((qualifiers '()))
	((progn
	   (when (endp args)
	     (error "Illegal defmethod form: missing lambda-list"))
	   (listp (first args)))
	 (values name (nreverse qualifiers) (first args) (rest args)))
      (push (pop args) qualifiers))))

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

(defun make-method (qualifiers specializers lambda-list
		    fun plist options gf method-class)
  (let ((method (si:allocate-raw-instance nil (find-class 'standard-method nil)
		   #.(length +standard-method-slots+))))
    (setf (method-generic-function method) nil
	  (method-lambda-list method) lambda-list
	  (method-function method) fun
	  (method-specializers method) specializers
	  (method-qualifiers method) qualifiers
	  (method-plist method) plist)
    method))

;;; early version used during bootstrap
(defun method-p (x)
  (si::instancep x))

(defun method-needs-next-methods-p (method)
  (getf (method-plist method) :needs-next-methods-p))

;;; early version used during bootstrap
(defun add-method (gf method)
  (let* ((name (generic-function-name gf))
	 (method-entry (assoc name *early-methods*)))
    (unless method-entry
      (setq method-entry (list name))
      (push method-entry *early-methods*))
    (push method (cdr method-entry))
    (push method (generic-function-methods gf))
    (setf (method-generic-function method) gf)
    (unless (si::sl-boundp (generic-function-lambda-list gf))
      (setf (generic-function-lambda-list gf) (method-lambda-list method)))
    (setf (generic-function-spec-list gf) (compute-g-f-spec-list gf))
    method))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (declare (notinline method-qualifiers))
  (let* ((method-list (generic-function-methods gf))
	 (required-args (subseq (generic-function-lambda-list gf) 0
				(length specializers)))
	 found)
    (dolist (method method-list)
      (when (and (equal qualifiers (method-qualifiers method))
		 (equal specializers (method-specializers method)))
	(return-from find-method method)))
    ;; If we did not find any matching method, then the list of
    ;; specializers might have the wrong size and we must signal
    ;; an error.
    (cond ((/= (length specializers)
	       (length (generic-function-spec-list gf)))
	   (error
	    "The specializers list~%~A~%does not match the number of required arguments in ~A"
	    specializers (generic-function-name gf)))
	  (errorp
	   (error "There is no method on the generic function ~S that agrees on qualifiers ~S and specializers ~S"
		  (generic-function-name gf)
		  qualifiers specializers)))
    nil))


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
    (if (and slot (eq :INSTANCE (slotd-allocation slot)))
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
(put-sysprop 'SLOT-INDEX ':INLINE-ALWAYS
	     '(((T T) FIXNUM NIL NIL "fix(gethash(#0,#1))")
	       ((T T) T NIL NIL "gethash_safe(#0,#1,Cnil)")))

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
