;;;;  CMPCALL  Function call.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.


(in-package "COMPILER")

(defun fast-link-proclaimed-type-p (fname &optional args)
  (and *compile-to-linking-call*
       (symbolp fname)
       (and (< (the fixnum (length args)) 10)
            (or (and (get-sysprop fname 'FIXED-ARGS)
                     (listp args))
                (and
                 (get-sysprop fname 'PROCLAIMED-FUNCTION)
                 (eq (get-sysprop fname 'PROCLAIMED-RETURN-TYPE) t)
                 (every #'(lambda (v) (eq v t))
                        (get-sysprop fname 'PROCLAIMED-ARG-TYPES)))))))

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name)
  (or (sch-local-macro name)
      (macro-function name)))

(defun c1funcall (args)
  (check-args-number 'FUNCALL args 1)
  (let ((fun (first args))
	(arguments (rest args)))
    (cond ;; (FUNCALL (LAMBDA ...) ...)
          ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL (EXT::LAMBDA-BLOCK ...) ...)
          ((and (consp fun)
		(eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL lisp-expression ...)
	  ((not (and (consp fun)
		     (eq (first fun) 'FUNCTION)))
	   (make-c1form* 'FUNCALL :args (c1expr fun) (c1args* arguments)))
	  ;; (FUNCALL #'GENERALIZED-FUNCTION-NAME ...)
	  ((si::valid-function-name-p (setq fun (second fun)))
	   (or (c1call-local fun arguments)
	       (c1call-global fun arguments)))
	  ;; (FUNCALL #'(LAMBDA ...) ...)
	  ((and (consp fun) (eq (first fun) 'LAMBDA))
	   (c1expr (optimize-funcall/apply-lambda (rest fun) arguments nil)))
	  ;; (FUNCALL #'(EXT::LAMBDA-BLOCK ...) ...)
	  ((and (consp fun) (eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1expr (optimize-funcall/apply-lambda (rest fun) arguments nil)))
	  (t
	   (cmperr "Malformed function name: ~A" fun)))))

(defun c2funcall (form args &optional loc narg)
  ;; Usually, ARGS holds a list of forms, which are arguments to the
  ;; function.  If, however, the arguments are on VALUES,
  ;; ARGS should be set to the symbol ARGS-PUSHED, and NARG to a location
  ;; containing the number of arguments.
  ;; LOC is the location of the function object (created by save-funob).
  (case (c1form-name form)
    (GLOBAL (c2call-global (c1form-arg 0 form) args loc t narg))
    (LOCAL (c2call-local (c1form-arg 0 form) args narg))
    ;; An ordinary expression.  In this case, if arguments are already on
    ;; VALUES, then LOC cannot be NIL.  Callers of C2FUNCALL must be
    ;; responsible for maintaining this condition.
    (otherwise
     (let ((*inline-blocks* 0)
	   (*temp* *temp*))
       (unless loc
	 (setf loc (maybe-save-value form args)))
       (c2call-unknown-global nil (if (eq args 'ARGS-PUSHED)
				      args
				      (inline-args args)) loc nil narg)
       (close-inline-blocks)))))

(defun maybe-push-args (args)
  (when (or (eq args 'ARGS-PUSHED)
	    (< (length args) SI::C-ARGUMENTS-LIMIT))
    (return-from maybe-push-args (values nil nil nil)))
  (let* ((narg (make-lcl-var :type :cl-index)))
    (wt-nl "{cl_index " narg "=0;")
    (let* ((*temp* *temp*)
	   (temp (make-temp-var))
	   (*destination* temp))
      (dolist (expr args)
	(c2expr* expr)
	(wt-nl "cl_stack_push(" temp "); " narg "++;")))
    (values `((STACK ,narg) ,@*unwind-exit*) 'ARGS-PUSHED narg)))

;;;
;;; c2call-global:
;;;   ARGS is either the list of arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;   LOC is either NIL or the location of the function object
;;;
(defun c2call-global (fname args loc return-type &optional narg)
  (multiple-value-bind (*unwind-exit* args narg)
      (maybe-push-args args)
    (when narg
      (c2call-global fname args loc return-type narg)
      (wt-nl "}")
      (return-from c2call-global)))
  (unless (eq 'ARGS-PUSHED args)
    (case fname
      (AREF
       (let (etype (elttype (c1form-type (car args))))
	 (when (or (and (eq elttype 'STRING)
			(setq elttype 'CHARACTER))
		   (and (consp elttype)
			(or (eq (car elttype) 'ARRAY)
			    (eq (car elttype) 'VECTOR))
			(setq elttype (second elttype))))
	   (setq etype (type-and return-type elttype))
	   (unless etype
	     (cmpwarn "Type mismatch found in AREF. Expected output type ~s, array element type ~s." return-type elttype)
	     (setq etype T))		; assume no information
	   (setf return-type etype))))
      (SYS:ASET				; (sys:aset value array i0 ... in)
       (let (etype
	     (valtype (c1form-type (first args)))
	     (elttype (c1form-type (second args))))
	 (when (or (and (eq elttype 'STRING)
			(setq elttype 'CHARACTER))
		   (and (consp elttype)
			(or (eq (car elttype) 'ARRAY)
			    (eq (car elttype) 'VECTOR))
			(setq elttype (second elttype))))
	   (setq etype (type-and return-type (type-and valtype elttype)))
	   (unless etype
	     (cmpwarn "Type mismatch found in (SETF AREF). Expected output type ~s, array element type ~s, value type ~s." return-type elttype valtype)
	     (setq etype T))
	   (setf return-type etype)
	   (setf (c1form-type (first args)) etype))))))
  (if (and (inline-possible fname)
	   (not (eq 'ARGS-PUSHED args))
	   *tail-recursion-info*
	   (same-fname-p (first *tail-recursion-info*) fname)
	   (last-call-p)
	   (tail-recursion-possible)
	   (= (length args) (length (cdr *tail-recursion-info*))))
      ;; Tail-recursive case.
      (let* ((*destination* 'TRASH)
	     (*exit* (next-label))
	     (*unwind-exit* (cons *exit* *unwind-exit*)))
	(c2psetq (cdr *tail-recursion-info*) args)
	(wt-label *exit*)
	(unwind-no-exit 'TAIL-RECURSION-MARK)
	(wt-nl "goto TTL;")
	(cmpnote "Tail-recursive call of ~s was replaced by iteration."
		 fname))
      ;; else
      (let ((*inline-blocks* 0))
	(call-global fname (if (eq args 'ARGS-PUSHED) args (inline-args args))
		     loc return-type narg)
	(close-inline-blocks))))

;;;
;;; call-global:
;;;   LOCS is either the list of typed locs with the arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;   LOC is either NIL or the location of the function object
;;;
(defun call-global (fname locs loc return-type narg &aux found fd maxarg)
  (flet ((emit-linking-call (fname locs narg &aux i)
	   (cond ((null *linking-calls*)
		  (cmpwarn "Emitting linking call for ~a" fname)
		  (push (list fname 0 (add-symbol fname))
			*linking-calls*)
		  (setq i 0))
		 ((setq i (assoc fname *linking-calls*))
		  (setq i (second i)))
		 (t (setq i (1+ (cadar *linking-calls*)))
		    (cmpwarn "Emitting linking call for ~a" fname)
		    (push (list fname i (add-symbol fname))
			  *linking-calls*)))
	   (unwind-exit
	    (call-loc fname (format nil "(*LK~d)" i) locs narg))))
    (cond 
     ;; It is not possible to inline the function call
     ((not (inline-possible fname))
      ;; We can only emit linking calls when function name is a symbol.
      (if (and (symbolp fname) *compile-to-linking-call*)
	(emit-linking-call fname locs narg)
	(c2call-unknown-global fname locs loc t narg)))

     ;; Open-codable function call.
     ((and (not (eq 'ARGS-PUSHED locs))
	   (null loc)
	   (setq loc (inline-function fname locs return-type)))
      (unwind-exit loc))

     ;; Call to a function defined in the same file.
     ((setq fd (assoc fname *global-funs* :test #'same-fname-p))
      (let ((cfun (second fd)))
	(unwind-exit (call-loc fname
			       (if (numberp cfun)
				 (format nil "L~d" cfun)
				 cfun)
			       locs narg))))

     ;; Call to a function whose C language function name is known,
     ;; either because it has been proclaimed so, or because it belongs
     ;; to the runtime.
     ((and (symbolp fname)
	   (or (setq maxarg -1 fd (get-sysprop fname 'Lfun))
	       (multiple-value-setq (found fd maxarg) (si::mangle-name fname t))))
      (multiple-value-bind (val found)
	  (gethash fd *compiler-declared-globals*)
	;; We only write declarations for functions which are not
	;; in lisp_external.h
	(when (and (not found) (not (si::mangle-name fname t)))
	  (wt-h "#ifdef __cplusplus")
	  (wt-h "extern cl_object " fd "(...);")
	  (wt-h "#else")
	  (wt-h "extern cl_object " fd "();")
	  (wt-h "#endif")
	  (setf (gethash fd *compiler-declared-globals*) 1)))
      (unwind-exit
       (if (minusp maxarg)
	 (call-loc fname fd locs narg)
	 (call-loc-fixed fname fd locs narg maxarg))))

     ;; Linking calls can only be made to symbols
     ((and (symbolp fname)
	   *compile-to-linking-call*)	; disabled within init_code
      (emit-linking-call fname locs narg))

     (t (c2call-unknown-global fname locs loc t narg)))
    )
  )

;;; Functions that use MAYBE-SAVE-VALUE should rebind *temp*.
(defun maybe-save-value (value &optional (other-forms nil other-forms-flag))
  (let ((name (c1form-name value)))
    (cond ((eq name 'LOCATION)
	   (c1form-arg 0 value))
	  ((and (eq name 'VAR)
		other-forms-flag
		(not (var-changed-in-forms (c1form-arg 0 value) other-forms)))
	   (c1form-arg 0 value))
	  (t
	   (let* ((temp (make-temp-var))
		  (*destination* temp))
	     (c2expr* value)
	     temp)))))

;;;
;;; call-loc:
;;;   args are typed locations as produced by inline-args
;;;
(defun call-loc (fname fun args &optional narg-loc)
  (cond ((not (eq 'ARGS-PUSHED args))
	 (list 'CALL fun (length args) (coerce-locs args) fname))
	((stringp fun)
	 (list 'CALL "APPLY" narg-loc (list fun `(STACK-POINTER ,narg-loc))
	       fname))
	(t
	 (list 'CALL "cl_apply_from_stack" narg-loc (list fun) fname))))

(defun call-loc-fixed (fname fun args narg-loc maxarg)
  (cond ((not (eq 'ARGS-PUSHED args))
	 (when (/= (length args) maxarg)
	   (cmperr "Wrong number of arguments to function ~S." fname))
	 (list 'CALL-FIX fun (coerce-locs args) fname))
	((stringp fun)
	 (wt "if(" narg-loc "!=" maxarg ") FEwrong_num_arguments_anonym();")
	 (list 'CALL-FIX "APPLY_fixed" (list fun `(STACK-POINTER ,narg-loc)) fname narg-loc))
	(t
	 (baboon))))

(defun wt-stack-pointer (narg)
  (wt "cl_env.stack_top-" narg))

(defun wt-call (fun narg args &optional fname)
  (wt fun "(" narg)
  (dolist (arg args)
    (wt "," arg))
  (wt ")")
  (when fname (wt-comment fname)))

(defun wt-call-fix (fun args &optional fname)
  (wt fun "(")
  (when args
    (wt (pop args))
    (dolist (arg args)
      (wt "," arg)))
  (wt ")")
  (when fname (wt-comment fname)))

;;;
;;; c2call-unknown-global
;;;   LOC is NIL or location containing function
;;;   ARGS is either the list of typed locations for arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;
(defun c2call-unknown-global (fname args loc inline-p narg)
  (unless loc
    (setq loc
	  (if (and (symbolp fname)
		   (not (eql (symbol-package fname) (find-package "CL"))))
	      (progn
		(cmpnote "Emiting FUNCALL for ~S" fname)
		(add-symbol fname))
	      (progn
		(cmpnote "Emiting FDEFINITION for ~S" fname)
		(setq loc (list 'FDEFINITION fname))))))
  (unwind-exit
   (cond ((eq args 'ARGS-PUSHED)
	  (list 'CALL "cl_apply_from_stack" narg (list loc) fname))
	 (t
	  (call-loc fname "funcall" (cons (list T loc) args))))))

;;; ----------------------------------------------------------------------

(put-sysprop 'funcall 'C1 #'c1funcall)
(put-sysprop 'funcall 'c2 #'c2funcall)
(put-sysprop 'call-global 'c2 #'c2call-global)

(put-sysprop 'CALL 'WT-LOC #'wt-call)
(put-sysprop 'CALL-FIX 'WT-LOC #'wt-call-fix)
(put-sysprop 'STACK-POINTER 'WT-LOC #'wt-stack-pointer)
