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

(defmacro defmethod (&rest args &environment env)
  (multiple-value-bind (name qualifiers specialized-lambda-list body)
      (parse-defmethod args)
    (multiple-value-bind (lambda-list required-parameters specializers)
	(parse-specialized-lambda-list specialized-lambda-list)
      (multiple-value-bind (fn-form doc plist)
	  (expand-defmethod name qualifiers lambda-list
			    required-parameters specializers body env)
	(declare (ignore required-parameters))
	`(PROGN
	  #+PDE
	  (EVAL-WHEN (LOAD)
	    (SI:RECORD-SOURCE-PATHNAME
	     ',name '(DEFMETHOD ',qualifiers ',specializers)))
	  (INSTALL-METHOD
	   ',name
	   ',qualifiers
	   ,(list 'si::quasiquote specializers)
	   ',lambda-list
	   ',doc
	   ',plist
	   ,fn-form)
	  )))))


;;; ----------------------------------------------------------------------
;;;                                                  method body expansion

(defun expand-defmethod (generic-function-name qualifiers lambda-list
			 required-parameters specializers body env)
  (declare (ignore qualifiers)
	   (si::c-local))
  (multiple-value-bind (declarations real-body documentation)
      (sys::find-declarations body)
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
    (let* ((class-declarations
	    (nconc (mapcan #'(lambda (p s) (and (symbolp s) s
						(not (eq s 't))
						`((type ,s ,p))))
			   required-parameters
			   specializers)
		   (cdar declarations)))
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
	      ,@(and class-declarations `((declare ,@class-declarations)))
	      ,@real-body))
	   
	   (original-args ())
	   (applyp nil)		; flag indicating whether or not the
				; method takes &mumble arguments. If
				; it does, it means call-next-method
				; without arguments must be APPLY'd
				; to original-args.  If this gets set
				; true, save-original-args is set so
				; as well
	   (aux-bindings ())	; Suffice to say that &aux is one of
				; damndest things to have put in a
				; language.
	   (plist ()))
      (multiple-value-bind (walked-lambda call-next-method-p
					  save-original-args next-method-p-p)
	  (walk-method-lambda method-lambda required-parameters env)

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
	   documentation
	   plist))))))

(defun walk-method-lambda (method-lambda required-parameters env)
  (declare (si::c-local))
  (let ((call-next-method-p nil)
	(next-method-p-p nil)
	(save-original-args-p nil))
    (flet ((code-walker (form env)
	     (unless (atom form)
	       (let ((name (first form)))
		 (case name
		   (CALL-NEXT-METHOD
		    (setf call-next-method-p
			  (or call-next-method-p T))
		    (unless (rest form)
		      (setf save-original-args-p t)))
		   (NEXT-METHOD-P
		    (setf next-method-p-p t))
		   (FUNCTION
		    (when (eq (second form) 'CALL-NEXT-METHOD)
		      (setf save-original-args-p t
			    call-next-method-p 'FUNCTION))
		    (when (eq (second form) 'NEXT-METHOD-P)
		      (setf next-method-p-p 'FUNCTION))))))
	     form))
      (let ((si::*code-walker* #'code-walker))
	(coerce method-lambda 'function)))
    (values method-lambda call-next-method-p
	    save-original-args-p
	    next-method-p-p)))

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
	   (declare (special *next-methods*))
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (declare (special *next-methods*))
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
	    (declare (special *next-methods*))
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (declare (special *next-methods*))
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
	    (declare (special *next-methods*))
	    (let* ((.next-method. (car *next-methods*))
		   (*next-methods* (cdr *next-methods*)))
	      (declare (special *next-methods*))
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

(defun parse-specialized-lambda-list (specialized-lambda-list)
  "This function takes a method lambda list and outputs the list of required
arguments, the list of specializers and a new lambda list where the specializer
have disappeared."
  (declare (si::c-local))
  ;; SI:PROCESS-LAMBDA-LIST will ensure that the lambda list is
  ;; syntactically correct and will output as a first argument the
  ;; list of required arguments. We use this list to extract the
  ;; specializers and build a lambda list without specializers.
  (do* ((arglist (rest (si::process-lambda-list specialized-lambda-list 'METHOD))
		 (rest arglist))
	(lambda-list (copy-list specialized-lambda-list))
	(ll lambda-list (rest ll))
	(required-parameters '())
	(specializers '())
	arg variable specializer)
       ((null arglist)
	(values lambda-list
		(nreverse required-parameters)
		(nreverse specializers)))
    (setf arg (first arglist))
    (cond
      ;; Just a variable
      ((atom arg)
       (setf variable arg specializer T))
      ;; List contains more elements than variable and specializer
      ((not (endp (cddr arg)))
       (si::simple-program-error "Syntax error in method specializer ~A" arg))
      ;; Specializer is NIL
      ((null (setf variable (first arg)
		   specializer (second arg)))
       (si::simple-program-error
	"NIL is not a valid specializer in a method lambda list"))
      ;; Specializer is a class name
      ((atom specializer))
      ;; Specializer is (EQL value)
      ((and (eql (first specializer) 'EQL)
	    (endp (cddr specializer)))
       (let ((value (second specializer)))
	 (setf specializer
	       `(eql ,(if (constantp value)
			  (eval value)
			  (list 'si::unquote value))))))
      ;; Otherwise, syntax error
      (t
       (si::simple-program-error "Syntax error in method specializer ~A" arg)))
    (setf (first ll) variable)
    (push variable required-parameters)
    (push specializer specializers)))

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
      (setf (generic-function-lambda-list gf) (method-lambda-list method))
      (setf (generic-function-argument-precedence-order gf)
	    (rest (si::process-lambda-list (method-lambda-list method) t))))
    (compute-g-f-spec-list gf)
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
	       (length (generic-function-argument-precedence-order gf)))
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

;;; Force the compiler into optimizing use of gethash inside methods:
(setf (symbol-function 'SLOT-INDEX) (symbol-function 'GETHASH))