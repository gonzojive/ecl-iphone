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
       (unwind-exit (call-unknown-global-loc nil loc narg
					     (if (eq args 'ARGS-PUSHED)
						 args
						 (inline-args args))))
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
       (let (etype (elttype (c1form-primary-type (car args))))
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
	     (valtype (c1form-primary-type (first args)))
	     (elttype (c1form-primary-type (second args))))
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
  (when (null loc)
    (let ((fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p)))
      (when fun
	(when (c2try-tail-recursive-call fun args)
	  (return-from c2call-global))
	(setf loc fun))))
  (let ((*inline-blocks* 0))
    (call-global fname loc
		 narg (if (eq args 'ARGS-PUSHED) args (inline-args args))
		 return-type)
    (close-inline-blocks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL LOCATIONS
;;;

(defun call-global (&rest args)
  (unwind-exit (apply #'call-global-loc args)))

;;;
;;; call-global:
;;;   FNAME: the name of the function
;;;   LOC: either a function object or NIL
;;;   NARG: a location containing the number of ARGS-PUSHED
;;;   ARGS: 'ARGS-PUSHED or a list of typed locs with arguments
;;;   RETURN-TYPE: the type to which the output is coerced
;;;
(defun call-global-loc (fname loc narg args return-type &aux found fd minarg maxarg)
  (cond
    ;; Check whether it is a global function that we cannot call directly.
     ((and (or (null loc) (fun-global loc)) (not (inline-possible fname)))
      (if *compile-to-linking-call*
	  (call-linking-loc fname narg args)
	  (call-unknown-global-loc fname nil narg args)))

     ;; Open-codable function call.
     ((and (not (eq 'ARGS-PUSHED args))
	   (or (null loc) (fun-global loc))
	   (setq loc (inline-function fname args return-type)))
      loc)

     ;; Call to a function defined in the same file.
     ((fun-p loc)
      (call-loc fname loc narg args))

     ((and (null loc) (setf loc (find fname *global-funs* :test #'same-fname-p
				      :key #'fun-name)))
      (call-loc fname loc narg args))

     ;; Call to a global (SETF ...) function
     ((not (symbolp fname))
      (call-unknown-global-loc fname loc narg args))

     ;; Call to a function whose C language function name is known,
     ;; either because it has been proclaimed so, or because it belongs
     ;; to the runtime.
     ((and (setf fd (get-sysprop fname 'Lfun))
	   (multiple-value-setq (minarg maxarg) (get-proclaimed-narg fname)))
      (call-exported-function-loc fname narg args fd minarg maxarg
				  #-ecl-min nil
				  #+ecl-min (member fname *in-all-symbols-functions*)))

     ((multiple-value-setq (found fd minarg maxarg) (si::mangle-name fname t))
      (call-exported-function-loc fname narg args fd minarg maxarg t))

     ;; Linking calls can only be made to symbols
     (*compile-to-linking-call*
      (call-linking-loc fname narg args))

     (t (call-unknown-global-loc fname loc narg args))))

(defun call-loc (fname loc narg args)
  (if (eq args 'ARGS-PUSHED)
      `(CALL-ARGS-PUSHED ,fname ,loc ,narg)
      `(CALL-NORMAL ,loc ,(coerce-locs args))))

(defun call-linking-loc (fname narg args &aux i)
  (let ((fun (second (assoc fname *linking-calls*))))
    (unless fun
      (let* ((i (length *linking-calls*))
             (c-id (lisp-to-c-name fname))
	     (var-name (format nil "LK~d~A" i c-id))
	     (c-name (format nil "LKF~d~A" i c-id)))
	(cmpnote "Emitting linking call for ~a" fname)
	(setf fun (make-fun :name fname :global t :lambda 'NIL
			    :cfun (format nil "(*~A)" var-name)
			    :minarg 0 :maxarg call-arguments-limit))
	(setf *linking-calls* (cons (list fname fun (add-symbol fname) c-name var-name)
				    *linking-calls*))))
    (call-loc fname fun narg args)))

(defun call-exported-function-loc (fname narg args fun-c-name minarg maxarg in-core)
  (unless in-core
    ;; We only write declarations for functions which are not in lisp_external.h
    (multiple-value-bind (val declared)
	(gethash fun-c-name *compiler-declared-globals*)
      (unless declared
	(if (= maxarg minarg)
	    (progn
	      (wt-h1 "extern cl_object ") (wt-h1 fun-c-name) (wt-h1 "(")
	      (dotimes (i maxarg)
		(when (> i 0) (wt-h1 ","))
		(wt-h1 "cl_object"))
	      (wt-h1 ")"))
	    (progn
	      (wt-h "#ifdef __cplusplus")
	      (wt-h "extern cl_object " fun-c-name "(...);")
	      (wt-h "#else")
	      (wt-h "extern cl_object " fun-c-name "();")
	      (wt-h "#endif")))
	(setf (gethash fun-c-name *compiler-declared-globals*) 1))))
  (let ((fun (make-fun :name fname :global t :cfun fun-c-name :lambda 'NIL
		       :minarg minarg :maxarg maxarg)))
    (call-loc fname fun narg args)))

;;;
;;; call-unknown-global-loc
;;;   LOC is NIL or location containing function
;;;   ARGS is either the list of typed locations for arguments or 'ARGS-PUSHED
;;;   NARG is a location containing the number of ARGS-PUSHED
;;;
(defun call-unknown-global-loc (fname loc narg args)
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
  (if (eq args 'ARGS-PUSHED)
      `(CALL "cl_apply_from_stack" (,narg ,loc) ,fname)
      `(CALL "funcall" (,(1+ (length args)) ,loc ,@(coerce-locs args)) ,fname)))

;;; Functions that use MAYBE-SAVE-VALUE should rebind *temp*.
(defun maybe-save-value (value &optional (other-forms nil other-forms-flag))
  (let ((name (c1form-name value)))
    (cond ((eq name 'LOCATION)
	   (c1form-arg 0 value))
	  ((and (eq name 'VAR)
		other-forms-flag
		(not (var-changed-in-form-list (c1form-arg 0 value) other-forms)))
	   (c1form-arg 0 value))
	  (t
	   (let* ((temp (make-temp-var))
		  (*destination* temp))
	     (c2expr* value)
	     temp)))))

(defvar *text-for-lexical-level*
  '("lex0" "lex1" "lex2" "lex3" "lex4" "lex5" "lex6" "lex7" "lex8" "lex9"))

(defvar *text-for-closure*
  '("env0" "env1" "env2" "env3" "env4" "env5" "env6" "env7" "env8" "env9"))

(defun env-var-name (n)
  (or (nth n *text-for-closure*)
      (format nil "env~D" n)))

(defun wt-stack-pointer (narg)
  (wt "cl_env.stack_top-" narg))

(defun wt-call (fun args &optional fname)
  (wt fun "(")
  (let ((comma ""))
    (dolist (arg args)
      (wt comma arg)
      (setf comma ",")))
  (wt ")")
  (when fname (wt-comment fname)))

(defun wt-call-normal (fun args)
  (unless (fun-cfun fun)
    (baboon "Function without a C name: ~A" (fun-name fun)))
  (let* ((minarg (fun-minarg fun))
	 (maxarg (fun-maxarg fun))
	 (fun-c-name (fun-cfun fun))
	 (fun-lisp-name (fun-name fun))
	 (narg (length args)))
    (case (fun-closure fun)
      (CLOSURE
       (push (environment-accessor fun) args))
      (LEXICAL
       (let ((lex-lvl (fun-level fun)))
	 (dotimes (n lex-lvl)
	   (let* ((j (- lex-lvl n 1))
		  (x (nth j *text-for-lexical-level*)))
	     (unless x
	       (setf x (format nil "lex~d" j)
		     (nth n *text-for-lexical-level*) x))
	     (push x args))))))
    (unless (<= minarg narg maxarg)
      (error "Wrong number of arguments for function ~S"
	      (or fun-lisp-name 'ANONYMOUS)))
    (when (fun-needs-narg fun)
      (push narg args))
    (wt-call fun-c-name args fun-lisp-name)))

(defun wt-call-args-pushed (fname fun narg)
  (let* ((lex-lvl (fun-level fun))
	 (fun-c-name (fun-cfun fun))
	 (fun-lisp-name (fun-name fun)))
    (when (fun-closure fun)
      (error "WT-CALL-ARGS-PUSHED used with lexical closure.")
      (when (fun-closure fun)
	(wt "cl_stack_push(env~d" *env-lvl* ")," narg "++,"))
      (dotimes (n lex-lvl)
	(let ((j (- lex-lvl n 1)))
	  (wt "cl_stack_push(lex" j ")," narg "++,"))))
    (if (fun-needs-narg fun)
	(wt "APPLY(" narg "," fun-c-name "," `(STACK-POINTER ,narg) ")")
	(wt "((" narg "!=" maxarg ")&&FEwrong_num_arguments_anonym(),"
	    "APPLY_fixed(" narg "," fun-c-name "," `(STACK-POINTER ,narg) "))"))
    (when fun-lisp-name (wt-comment fun-lisp-name))))

;;; ----------------------------------------------------------------------

(put-sysprop 'funcall 'C1 #'c1funcall)
(put-sysprop 'funcall 'c2 #'c2funcall)
(put-sysprop 'call-global 'c2 #'c2call-global)

(put-sysprop 'CALL 'WT-LOC #'wt-call)
(put-sysprop 'CALL-NORMAL 'WT-LOC #'wt-call-normal)
(put-sysprop 'CALL-ARGS-PUSHED 'WT-LOC #'wt-call-args-pushed)
(put-sysprop 'STACK-POINTER 'WT-LOC #'wt-stack-pointer)
