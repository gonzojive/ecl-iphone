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
            (or (and (get fname 'FIXED-ARGS)
                     (listp args))
                (and
                 (get fname 'PROCLAIMED-FUNCTION)
                 (eq (get fname 'PROCLAIMED-RETURN-TYPE) t)
                 (every #'(lambda (v) (eq v t))
                        (get fname 'PROCLAIMED-ARG-TYPES)))))))

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name)
  (or (sch-local-macro name)
      (macro-function name)))

(defun c1funob (fun &aux fd function)
  ;; fun is an expression appearing in functional position, in particular
  ;; (FUNCTION (LAMBDA ..))
  (when (and (consp fun)
	     (symbolp (first fun))
	     (cmp-macro-function (first fun)))
    (setq fun (cmp-macroexpand fun)))
  (cond ((not (and (consp fun)
		   (eq (first fun) 'FUNCTION)
		   (consp (cdr fun))
		   (endp (cddr fun))))
	 (let ((x (c1expr fun)) (info (make-info :sp-change t)))
	   (add-info info (second x))
	   (list 'ORDINARY info x)))
	((symbolp (setq function (second fun)))
	 (or (c1call-local function)
	     (list 'GLOBAL
		   (make-info :sp-change
			      (not (get function 'NO-SP-CHANGE)))
		   function)))
	((and (consp function)
	      (eq (first function) 'LAMBDA)
	      (consp (rest function)))
	 ;; Don't create closure boundary like in c1function
	 ;; since funob is used in this same environment
	 (let ((lambda-expr (c1lambda-expr (rest function))))
	   (list 'LAMBDA (second lambda-expr) lambda-expr (next-cfun))))
	((and (consp function)
	      (eq (first function) 'LAMBDA-BLOCK)
	      (consp (rest function)))
	 ;; Don't create closure boundary like in c1function
	 ;; since funob is used in this same environment
	 (let ((name (second function)))
	   (unless (symbolp name)
	     (if (si::setf-namep name)
	       (setq name (si::setf-namep name))
	       (error "~S is not a valid function name" name)))
	   (let ((lambda-expr (c1lambda-expr (cddr function) name)))
	     (list 'LAMBDA (second lambda-expr) lambda-expr (next-cfun)))))
	(t (cmperr "Malformed function: ~A" fun))))

(defun c1funcall (args &aux funob (info (make-info)))
  (when (endp args) (too-few-args 'FUNCALL 1 0))
  (setq funob (c1funob (car args)))
  (add-info info (second funob))
  (list 'FUNCALL info funob (c1args (cdr args) info)))

(defun c2funcall (funob args &optional loc narg
			&aux (form (third funob)))
  ;; Usually, ARGS holds a list of forms, which are arguments to the
  ;; function.  If, however, the arguments are on VALUES,
  ;; ARGS should be set to the symbol ARGS-PUSHED, and NARG to a location
  ;; containing the number of arguments.
  ;; LOC is the location of the function object (created by save-funob).
  (case (first funob)
    (GLOBAL (c2call-global form args loc t narg))
    (LOCAL (c2call-local form args narg))
    (LAMBDA (c2call-lambda form args (fourth funob) narg))
    (ORDINARY		;;; An ordinary expression.  In this case, if
              		;;; arguments are already on VALUES, then
              		;;; LOC cannot be NIL.  Callers of C2FUNCALL must be
              		;;; responsible for maintaining this condition.
     (let ((fun (third form)))
       (unless loc
	 (cond ((eq (first form) 'LOCATION) (setq loc fun))
	       ((and (eq (first form) 'VAR)
		     (not (var-changed-in-forms (first fun) args)))
		(setq loc (cons 'VAR fun))) ; i.e. (VAR var)
	       (t
		(setq loc (list 'TEMP (next-temp)))
		(let ((*destination* loc)) (c2expr* form)))))

       (let ((*inline-blocks* 0))
	 (c2call-unknown-global nil (if (eq args 'ARGS-PUSHED)
					args
					(inline-args args)) loc nil narg)
	 (close-inline-blocks))))
    (otherwise (baboon))
    ))

(defun c2call-lambda (lambda-expr args cfun &optional narg)
  ;; ARGS is either the list of arguments or 'ARGS-PUSHED
  ;; NARG is a location containing the number of ARGS-PUSHED
  (let ((lambda-list (third lambda-expr))
	(args-pushed (eq 'ARGS-PUSHED args)))
    (if (or (second lambda-list)		;;; Has optional?
	    (third lambda-list)			;;; Has rest?
	    (fourth lambda-list)		;;; Has key?
	    args-pushed				;;; Args already pushed?
	    )
	(let* ((requireds (first lambda-list))
	       (nreq (length requireds))
	       (nopt (if args-pushed narg (- (length args) nreq)))
	       (*unwind-exit* *unwind-exit*))
	  (wt-nl "{ ")
	  (unless args-pushed
	    (setq narg `(LCL ,(next-lcl)))
	    (wt-nl "cl_index " narg "=0;"))
	  (when requireds
	    (wt-nl "cl_object ")
	    (let ((lcl (+ *lcl* nreq)))
	      (declare (fixnum lcl))
	      (do ((args requireds (cdr args)))
		  ((null args))
		(wt-lcl lcl) (when (cdr args) (wt ", ")) (decf lcl)))
	    (wt ";"))
	  (wt-nl "int narg;")
	  (wt-nl "cl_va_list args;")
	  (cond (args-pushed
		 (wt-nl "args[0].sp=cl_stack_index()-" narg ";")
		 (wt-nl "args[0].narg=" narg ";")
		 (dotimes (i nreq)
		   (wt-nl) (wt-lcl (next-lcl)) (wt "=cl_va_arg(args);")))
		(t
		 (dotimes (i nreq)
		   (let ((*destination* `(LCL ,(next-lcl))))
		     (c2expr* (pop args))))
		 (push (list STACK narg) *unwind-exit*)
		 (wt-nl "args[0].sp=cl_stack_index();")
		 (wt-nl "args[0].narg=" nopt ";")
		 (do* ((*inline-blocks* 0)
		       (vals (inline-args args) (cdr vals))
		       (i 0 (1+ i)))
		     ((null vals) (close-inline-blocks))
		   (declare (fixnum i))
		   (wt-nl "cl_stack_push(" (second (first vals)) ");")
		   (wt-nl narg "++;"))
		 (wt-nl "args[0].narg=" narg ";")))
	  (wt "narg=" narg ";")
	  (c2lambda-expr lambda-list (third (cddr lambda-expr)) cfun
			 nil nil 'CALL-LAMBDA)
	  (unless args-pushed
	    (wt-nl "cl_stack_pop_n(" narg ");"))
	  (wt-nl "}"))
	(c2let (first lambda-list) args (third (cddr lambda-expr))))))

(defun maybe-push-args (args)
  (when (or (eq args 'ARGS-PUSHED)
	    (< (length args) SI::C-ARGUMENTS-LIMIT))
    (return-from maybe-push-args (values nil nil nil)))
  (let* ((narg `(LCL ,(next-lcl))))
    (wt-nl "{cl_index " narg ";")
    (let* ((*temp* *temp*)
	   (temp `(TEMP ,(next-temp)))
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
       (let (etype (elttype (info-type (cadar args))))
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
	     (valtype (info-type (cadr (first args))))
	     (elttype (info-type (cadr (second args)))))
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
	   (setf (info-type (cadr (first args))) etype))))))
  (if (and (inline-possible fname)
	   (not (eq 'ARGS-PUSHED args))
	   *tail-recursion-info*
	   (eq (first *tail-recursion-info*) fname)
	   (last-call-p)
	   (tail-recursion-possible)
	   (= (length args) (length (cdr *tail-recursion-info*))))
      ;; Tail-recursive case.
      (let* ((*destination* 'TRASH)
	     (*exit* (next-label))
	     (*unwind-exit* (cons *exit* *unwind-exit*)))
	(c2psetq
	 (mapcar #'(lambda (v) (list v)) ; nil (ccb)
		 (cdr *tail-recursion-info*))
	 args)
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
    (if (inline-possible fname)
	(cond 
	  ;; Open-codable function call.
	  ((and (not (eq 'ARGS-PUSHED locs))
		(null loc)
		(setq loc (inline-function fname locs return-type)))
	   (unwind-exit (fix-loc loc)))

	  ;; Call to a function defined in the same file.
	  ((setq fd (assoc fname *global-funs*))
	   (let ((cfun (second fd)))
	     (unwind-exit (call-loc fname
				    (if (numberp cfun)
				      (format nil "L~d" cfun)
				      cfun)
				    locs narg))))

	  ;; Call to a function whose C language function name is known,
	  ;; either because it has been proclaimed so, or because it belongs
	  ;; to the runtime.
	  ((or (setq maxarg -1 fd (get fname 'Lfun))
	       (multiple-value-setq (found fd maxarg) (si::mangle-name fname t)))
	   (multiple-value-bind (val found)
	       (gethash fd *compiler-declared-globals*)
	     ;; We only write declarations for functions which are not
	     ;; in lisp_external.h
	     (when (and (not found) (not (si::mangle-name fname t)))
	       (wt-h "extern cl_object " fd "();")
	       (setf (gethash fd *compiler-declared-globals*) 1)))
	   (unwind-exit
	    (if (minusp maxarg)
		(call-loc fname fd locs narg)
		(call-loc-fixed fname fd locs narg maxarg))))

	  ;; Linking call
	  (*compile-to-linking-call*	; disabled within init_code
	   (emit-linking-call fname locs narg))

	  (t (c2call-unknown-global fname locs loc t narg)))

	;; else not inline-possible
	(emit-linking-call fname locs narg)))
  )

;;; Functions that use SAVE-FUNOB should rebind *temp*.
(defun save-funob (funob)
  (case (first funob)
    ((LAMBDA LOCAL))
    (GLOBAL
     (unless (and (inline-possible (third funob))
                  (or (get (third funob) 'Lfun)
                      (assoc (third funob) *global-funs*)))
       (let ((temp (list 'TEMP (next-temp))))
         (if *safe-compile*
           (wt-nl temp "=symbol_function(" (add-symbol (third funob)) ");")
           (wt-nl temp "=" (add-symbol (third funob)) "->symbol.gfdef;"))
         temp)))
    (ORDINARY (let* ((temp (list 'TEMP (next-temp)))
                     (*destination* temp))
                (c2expr* (third funob))
                temp))
    (otherwise (baboon))
    ))

;;;
;;; call-loc:
;;;   args are typed locations as produced by inline-args
;;;
(defun call-loc (fname fun args &optional narg-loc)
  (cond ((not (eq 'ARGS-PUSHED args))
	 (list 'CALL fun (length args) (coerce-locs args nil) fname))
	((stringp fun)
	 (list 'CALL "APPLY" narg-loc (list fun `(STACK-POINTER ,narg-loc))
	       fname))
	(t
	 (list 'CALL "cl_apply_from_stack" narg-loc (list fun) fname))))

(defun call-loc-fixed (fname fun args narg-loc maxarg)
  (cond ((not (eq 'ARGS-PUSHED args))
	 (when (/= (length args) maxarg)
	     (error "Too many arguments to function ~S." fname))
	 (list 'CALL-FIX fun (coerce-locs args nil) fname))
	((stringp fun)
	 (wt "if(" narg-loc "!=" maxarg ") FEwrong_num_arguments_anonym();")
	 (list 'CALL-FIX "APPLY_fixed" (list fun `(STACK-POINTER ,narg-loc)) fname narg-loc))
	(t
	 (baboon))))

(defun wt-stack-pointer (narg)
  (wt "cl_stack_top-" narg))

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
	  (if *compiler-push-events*
	      (add-symbol fname)
	      (format nil
		      (if *safe-compile* "symbol_function(~A)" "~A->symbol.gfdef")
		      (add-symbol fname)))))
  (unwind-exit
   (if (eq args 'ARGS-PUSHED)
       (list 'CALL "cl_apply_from_stack" narg (list loc) fname)
       (call-loc fname "funcall" (cons (list T loc) args)))))

;;; ----------------------------------------------------------------------

(setf (get 'funcall 'C1) #'c1funcall)
(setf (get 'funcall 'c2) #'c2funcall)
(setf (get 'call-lambda 'c2) #'c2call-lambda)
(setf (get 'call-global 'c2) #'c2call-global)

(setf (get 'CALL 'WT-LOC) #'wt-call)
(setf (get 'CALL-FIX 'WT-LOC) #'wt-call-fix)
(setf (get 'STACK-POINTER 'WT-LOC) #'wt-stack-pointer)
