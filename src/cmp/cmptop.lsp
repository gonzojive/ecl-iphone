;;;;  CMPTOP  --  Compiler top-level.

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

(defun t1expr (form)
  (let ((*vars* nil)
	(*funs* nil)
	(*blocks* nil)
	(*tags* nil)
	(*special-binding* nil))
    (push (t1expr* form) *top-level-forms*)))

(defun t1expr* (form &aux (*current-form* form) (*first-error* t)
		    (*setjmps* 0))
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd)
	(when *compile-print* (print-current-form))
	(cond
            ((symbolp fun)
             (cond ((setq fd (get-sysprop fun 'T1))
                    (funcall fd args))
                   ((get-sysprop fun 'C1) (t1ordinary form))
                   ((setq fd (macro-function fun))
                    (t1expr* (cmp-expand-macro fd fun (cdr form))))
		   ((and (setq fd (assoc fun *funs*))
			 (eq (second fd) 'MACRO))
		    (t1expr* (cmp-expand-macro (third fd) fun (cdr form))))
                   (t (t1ordinary form))
                   ))
            ((consp fun) (t1ordinary form))
            (t (cmperr "~s is illegal function." fun)))
           ))))

(defun t2expr (form)
  (when form
    (let ((def (get-sysprop (c1form-name form) 'T2)))
      (when def (apply def (c1form-args form))))))

(defvar *emitted-local-funs* nil)

(defun emit-local-funs ()
  ;; Local functions and closure functions
  (do ()
      ;; repeat until t3local-fun generates no more
      ((eq *emitted-local-funs* *local-funs*))
    ;; scan *local-funs* backwards
    (do ((lfs *local-funs* (cdr lfs)))
	((eq (cdr lfs) *emitted-local-funs*)
	 (setq *emitted-local-funs* lfs)
	 (locally (declare (notinline t3local-fun))
	   ;; so disassemble can redefine it
	   (t3local-fun (first lfs)))))))

(defun t3expr (form)
  ;(pprint (cons 'T3 form))
  (when form
    (emit-local-funs)
    (let ((def (get-sysprop (c1form-name form) 'T3)))
      (when def
	;; new local functions get pushed into *local-funs*
	(apply def (c1form-args form))))
    (emit-local-funs)))

(defun ctop-write (name h-pathname data-pathname
		        &key system-p shared-data
			&aux def top-output-string
			(*volatile* " volatile "))

  ;(let ((*print-level* 3)) (pprint *top-level-forms*))
  (setq *top-level-forms* (nreverse *top-level-forms*))
  (wt-nl1 "#include \"" (si::coerce-to-filename h-pathname) "\"")
  (wt-h "#ifdef __cplusplus")
  (wt-h "extern \"C\" {")
  (wt-h "#endif")
  ;;; Initialization function.
  (let* ((*lcl* 0) (*lex* 0) (*max-lex* 0) (*max-env* 0) (*max-temp* 0)
	 (*reservation-cmacro* (next-cmacro))
	 (c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*emitted-local-funs* nil)
	 (*compiler-declared-globals* (make-hash-table))
	 #+PDE (optimize-space (>= *space* 3)))
    (unless shared-data
      (wt-nl1 "#include \"" (si::coerce-to-filename data-pathname) "\""))
    (wt-nl1 "#ifdef __cplusplus")
    (wt-nl1 "extern \"C\"")
    (wt-nl1 "#endif")
    (wt-nl1 "void init_" (init-function-name name) "(cl_object flag)")
    (wt-nl1 "{ VT" *reservation-cmacro* " CLSR" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (when shared-data
      (wt-nl "Cblock=flag;"))
    (unless shared-data
      (wt-nl "if (!FIXNUMP(flag)){")
      (wt-nl "Cblock=flag;")
      (wt-nl "#ifndef ECL_DYNAMIC_VV")
      (wt-nl "flag->cblock.data = VV;")
      (wt-nl "#endif")
      (wt-nl "flag->cblock.data_size = VM;")
      (wt-nl "flag->cblock.data_text = compiler_data_text;")
      (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
      (wt-nl "return;}")
      (wt-nl "#ifdef ECL_DYNAMIC_VV")
      (wt-nl "VV = Cblock->cblock.data;")
      (wt-nl "#endif"))
    ;; useless in initialization.
    (dolist (form *top-level-forms*)
      (let ((*compile-to-linking-call* nil)
	    (*env* 0) (*level* 0) (*temp* 0))
	  (t2expr form))
      (let ((*compiler-output1* c-output-file))
	(t3expr form)))
    (wt-function-epilogue)
    (wt-nl1 "}")
    (setq top-output-string (get-output-stream-string *compiler-output1*)))

  ;; Declarations in h-file.
  (wt-h "static cl_object Cblock;")
  (if shared-data
      (progn
	(wt-h "#ifdef ECL_DYNAMIC_VV")
	(wt-h "extern cl_object *VV;")
	(wt-h "#else")
	(wt-h "extern cl_object VV[];")
	(wt-h "#endif"))
      (progn
	(dolist (x *reservations*)
	  (wt-h "#define VM" (car x) " " (cdr x)))
	(let ((num-objects (data-size)))
	  (if (zerop num-objects)
	      (progn
		(wt-h "#define VM " num-objects 0)
		(wt-h "#define VV NULL"))
	      (progn
		(wt-h "#define VM " num-objects)
		(wt-h "#ifdef ECL_DYNAMIC_VV")
		(wt-h "static cl_object *VV;")
		(wt-h "#else")
		(wt-h "static cl_object VV[VM];")
		(wt-h "#endif"))))))
  (when *linking-calls*
    (dotimes (i (length *linking-calls*))
      (declare (fixnum i))
      (wt-h "static cl_object LKF" i "(cl_narg, ...);")
      (wt-h "static cl_object (*LK" i ")(cl_narg, ...)=LKF" i ";"))
    )
  ;;; Global entries for directly called functions.
  (dolist (x *global-entries*)
    (apply 'wt-global-entry x))
  
  ;;; Initial functions for linking calls.
  (dolist (x *linking-calls*)
    (let ((i (second x)))
      (wt-nl1 "static cl_object LKF" i
	      "(cl_narg narg, ...) {TRAMPOLINK(narg," (third x) ",&LK" i ",Cblock);}")))

  (wt-h "#ifdef __cplusplus")
  (wt-h "}")
  (wt-h "#endif")
  (wt-nl top-output-string))

(defun t1eval-when (args &aux (load-flag nil) (compile-flag nil))
  (check-args-number 'EVAL-WHEN args 1)
  (dolist (situation (car args))
    (case situation
      ((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
      ((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
      ((EVAL :EXECUTE))
      (otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
			 situation))))
  (let ((*compile-time-too* compile-flag))
    (cond (load-flag
	   (t1progn (rest args)))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (cdr args)))
	   (make-c1form* 'PROGN :args NIL)))))

(defun t1compiler-let (args &aux (symbols nil) (values nil))
  (check-args-number 'COMPILER-LET args 1)
  (dolist (spec (car args))
    (cond ((consp spec)
           (cmpck (not (and (symbolp (car spec))
                            (or (endp (cdr spec))
                                (endp (cddr spec)))))
                  "The variable binding ~s is illegal." spec)
           (push (car spec) symbols)
           (push (if (endp (cdr spec)) nil (eval (second spec))) values))
          ((symbolp spec)
           (push spec symbols)
           (push nil values))
          (t (cmperr "The variable binding ~s is illegal." spec))))
  (setq symbols (nreverse symbols))
  (setq values (nreverse values))
  (setq args (progv symbols values (t1progn (cdr args))))
  )

(defun t1progn (args)
  (make-c1form* 'PROGN :args (mapcar #'t1expr* args)))

(defun t2progn (args)
  (mapcar #'t2expr args))

(defun t3progn (args)
  (mapcar #'t3expr args))

(defun exported-fname (name)
  (if (and (symbolp name) (setf name (get-sysprop name 'Lfun)))
      (values name t)
      (values (next-cfun "L~D") nil)))

(defun t1defun (args)
  (check-args-number 'DEFUN args 2)
  (when *compile-time-too* (cmp-eval (cons 'DEFUN args)))
  (let* ((fname (car args))
	 (setjmps *setjmps*)
	 (lambda-expr (c1lambda-expr (cdr args) (si::function-block-name fname)))
	 (no-entry nil)
	 (doc nil))
    (unless (eql setjmps *setjmps*)
      (setf (c1form-volatile lambda-expr) t))
    (multiple-value-bind (decl body doc)
	(si::process-declarations (cddr args) nil)
      (cond ((and *allow-c-local-declaration* (assoc 'si::c-local decl))
	     (setq no-entry t))
	    ((setq doc (si::expand-set-documentation fname 'function doc))
	     (t1expr `(progn ,@doc)))))
    (add-load-time-values)
    (setq output (new-defun fname lambda-expr no-entry))
    output))

;;; Mechanism for sharing code:
;;; FIXME! Revise this 'DEFUN stuff.
(defun new-defun (fname lambda-expr &optional no-entry)
  (multiple-value-bind (cfun exported)
      (exported-fname fname)
    (unless exported
      (dolist (f *global-funs*)
	(when (similar lambda-expr (fun-lambda f))
	  (cmpnote "Sharing code among functions ~A and ~A" fname (fun-name f))
	  (setf cfun (fun-cfun f) lambda-expr nil)
	  (return))))
    (let ((fun (make-fun :name fname :cfun cfun :global t :exported exported
			 :lambda lambda-expr)))
      (push fun *global-funs*)
      (make-c1form* 'DEFUN :args fun no-entry))))

(defun similar (x y)
  (or (equal x y)
      (and (consp x)
	   (consp y)
	   (similar (car x) (car y))
	   (similar (cdr x) (cdr y)))
      (and (var-p x)
	   (var-p y)
	   (equalp x y))
      (and (typep x 'VECTOR)
	   (typep y 'VECTOR)
	   (every #'similar x y))))

(defun t2defun (fun no-entry)
  (declare (ignore sp funarg-vars))
  ;; If the function is not shared, emit it.
  (when (fun-lambda fun)
    (push fun *local-funs*))
  (unless no-entry
    (let* ((fname (fun-name fun))
	   (vv (add-object fname))
	   (cfun (fun-cfun fun)))
      (if (numberp cfun)
	(wt-nl "cl_def_c_function_va(" vv ",(cl_objectfn)L" cfun ");")
	(wt-nl "cl_def_c_function_va(" vv ",(cl_objectfn)" cfun ");")))))

(defun wt-function-prolog (&optional sp local-entry)
  (wt " VT" *reservation-cmacro*
      " VLEX" *reservation-cmacro*
      " CLSR" *reservation-cmacro*)
  (wt-nl "cl_object value0;")
  (when sp (wt-nl "bds_check;"))
  ; (when (compiler-push-events) (wt-nl "ihs_check;"))
  )

(defun wt-function-epilogue (&optional closure-p)
  (push (cons *reservation-cmacro* *max-temp*) *reservations*)
  (wt-h "#define VT" *reservation-cmacro*)
  (when (plusp *max-temp*)
    (wt-h1 " cl_object ")
    (dotimes (i *max-temp*)
      (wt-h1 "T") (wt-h1 i)
      (unless (= (1+ i) *max-temp*) (wt-h1 ",")))
    (wt-h1 ";"))
;  (wt-h "#define VU" *reservation-cmacro*)
  (wt-h "#define VLEX" *reservation-cmacro*)
  (when (plusp *max-lex*)
    (wt-h1 " cl_object lex") (wt-h1 *level*)
    (wt-h1 "[") (wt-h1 *max-lex*) (wt-h1 "];"))
  (wt-h "#define CLSR" *reservation-cmacro*)
  (when (plusp *max-env*)
    (unless closure-p (wt-h1 " volatile cl_object env0;"))
    (wt-h1 " cl_object ")
    (dotimes (i *max-env*)
      (wt-h1 "*CLV") (wt-h1 i)
      (unless (= (1+ i) *max-env*) (wt-h1 ",")))
    (wt-h1 ";"))
  )

(defun wt-global-entry (fname cfun arg-types return-type)
    (when (and (symbolp fname) (get-sysprop fname 'NO-GLOBAL-ENTRY))
      (return-from wt-global-entry nil))
    (wt-comment "global entry for the function " fname)
    (wt-nl1 "static cl_object L" cfun "(cl_narg narg")
    (wt-h "static cl_object L" cfun "(cl_narg")
    (do ((vl arg-types (cdr vl))
	 (lcl (1+ *lcl*) (1+ lcl)))
	((endp vl) (wt1 ")"))
      (declare (fixnum lcl))
      (wt1 ", cl_object ") (wt-lcl lcl)
      (wt-h1 ", cl_object"))
    (wt-h1 ");")
    (wt-nl1 "{")
    (when (compiler-check-args)
      (wt-nl "check_arg(" (length arg-types) ");"))
    (wt-nl "NVALUES=1;")
    (wt-nl "return " (case return-type
                            (FIXNUM "MAKE_FIXNUM")
                            (CHARACTER "CODE_CHAR")
                            (LONG-FLOAT "make_longfloat")
                            (SHORT-FLOAT "make_shortfloat")
                            (otherwise ""))
           "(LI" cfun "(")
    (do ((types arg-types (cdr types))
         (n 1 (1+ n)))
        ((endp types))
      (declare (fixnum n))
      (wt (case (car types)
            (FIXNUM "fix")
            (CHARACTER "char_code")
            (LONG-FLOAT "lf")
            (SHORT-FLOAT "sf")
            (otherwise "")) "(")
        (wt-lcl n) (wt ")")
        (unless (endp (cdr types)) (wt ",")))
    (wt "));}")
    )

(defun rep-type (type)
  (case type
    (FIXNUM "cl_fixnum ")
    (CHARACTER "unsigned char ")
    (SHORT-FLOAT "float ")
    (LONG-FLOAT "double ")
    (otherwise "cl_object ")))

(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (setq form (c1expr form))
  (add-load-time-values)
  (make-c1form* 'ORDINARY :args form))

(defun t2ordinary (form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
        (c2expr form)
        (wt-label *exit*)))

(defun add-load-time-values ()
  (when (listp *load-time-values*)
    (setq *top-level-forms* (nconc *load-time-values* *top-level-forms*))
    (setq *load-time-values* nil)))

(defun c1load-time-value (args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((listp form)
	   (setf loc (data-empty-loc))
	   (push (make-c1form* 'LOAD-TIME-VALUE :args loc (c1expr form))
		 *load-time-values*))
	  (t
	   (setf loc (add-object (cmp-eval form)))))
    (make-c1form* 'LOCATION :type t :args loc)))

(defun t2load-time-value (vv-loc form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2declare (vv)
  (wt-nl vv "->symbol.stype=(short)stp_special;"))

(defun t1defvar (args &aux form (doc nil) (name (car args)))
  (when *compile-time-too* (cmp-eval `(defvar ,@args)))
  (push name *global-vars*)
  (if (endp (cdr args))
      (make-c1form* 'DECLARE :args (add-symbol name))
      (progn
	(when (and (setq doc (third args))
		   (setq doc (si::expand-set-documentation name 'variable doc)))
	  (t1expr `(progn ,@doc)))
	(setq form (c1expr (second args)))
	(add-load-time-values)
	(make-c1form* 'DEFVAR :args (make-var :name name :kind 'SPECIAL
					      :loc (add-symbol name)) form))))

(defun t2defvar (var form &aux (vv (var-loc var)))
  (let* ((*exit* (next-label))
	 (*unwind-exit* (list *exit*))
	 (*temp* *temp*)
	 (*destination* (make-temp-var)))
        (c2expr form)
        (wt-nl "cl_defvar(" vv "," *destination* ");")
	(wt-label *exit*)))

(defun t1decl-body (decls body)
  (if (null decls)
      (t1progn body)
      (let* ((*function-declarations* *function-declarations*)
	     (*alien-declarations* *alien-declarations*)
	     (*notinline* *notinline*)
	     (*safety* *safety*)
	     (*space* *space*)
	     (*speed* *speed*)
	     (dl (c1add-declarations decls)))
	(make-c1form* 'DECL-BODY :args dl (t1progn body)))))

(defun t2decl-body (decls body)
  (let ((*safety* *safety*)
	(*space* *space*)
	(*speed* *speed*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (t2expr body)))

(defun t3decl-body (decls body)
  (let ((*safety* *safety*)
        (*space* *space*)
	(*speed* *speed*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (t3expr body)))

(defun t1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (c1add-globals ss)
    (check-vdecl nil ts is)
    (t1decl-body other-decl body)))

(defun t1macrolet (args &aux (*funs* *funs*))
  (check-args-number 'MACROLET args 1)
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The macro definition ~s is illegal." def)
    (push (list (car def)
		'MACRO
		(si::make-lambda (car def)
				 (cdr (sys::expand-defmacro (car def) (second def) (cddr def)))))
          *funs*))
  (t1locally (cdr args)))

(defun t1symbol-macrolet (args &aux (*vars* *vars*))
  (check-args-number 'SYMBOL-MACROLET args 1)
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The symbol-macro definition ~s is illegal." def)
    (push def *vars*))
  (t1locally (cdr args)))

(defun t1clines (args)
  (dolist (s args)
    (cmpck (not (stringp s)) "The argument to CLINE, ~s, is not a string." s))
  (make-c1form* 'CLINES :args args))

(defun t3clines (ss) (dolist (s ss) (wt-nl1 s)))

(defun parse-cvspecs (x &aux (cvspecs nil))
  (dolist (cvs x (nreverse cvspecs))
    (cond ((symbolp cvs)
           (push (list :OBJECT (string-downcase (symbol-name cvs))) cvspecs))
          ((stringp cvs) (push (list :OBJECT cvs) cvspecs))
          ((and (consp cvs)
                (member (car cvs) '(OBJECT CHAR INT FLOAT DOUBLE)))
           (dolist (name (cdr cvs))
             (push (list (car cvs)
                         (cond ((symbolp name)
                                (string-downcase (symbol-name name)))
                               ((stringp name) name)
                               (t (cmperr "The C variable name ~s is illegal."
                                          name))))
                   cvspecs)))
          (t (cmperr "The C variable specification ~s is illegal." cvs))))
  )

(defun t3local-fun (fun &optional
                              &aux
			      (closure-p (fun-closure fun))
			      (lambda-expr (fun-lambda fun))
			      (level (fun-level fun))
			      (cfun (fun-cfun fun))
			      (nenvs level)
			      (*volatile* (c1form-volatile* lambda-expr))
			      (*tail-recursion-info* fun)
                              (lambda-list (c1form-arg 0 lambda-expr))
                              (requireds (car lambda-list))
                              (va_args (or (second lambda-list)
                                           (third lambda-list)
                                           (fourth lambda-list))))
  (declare (fixnum level nenvs))
  (when *compile-print* (print-emitting fun))
  (wt-comment (cond ((fun-global fun) "function definition for ")
		    ((fun-closure fun) "closure ")
		    (t "local function "))
	      (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (cond ((fun-exported fun)
	 (wt-h "cl_object " cfun "(")
	 (wt-nl1 "cl_object " cfun "("))
	(t
	 (wt-h "static cl_object " cfun "(")
	 (wt-nl1 "static cl_object " cfun "(")))
  (wt-h1 "cl_narg")
  (wt "cl_narg narg")
  (dotimes (n level)
    (wt-h1 ",cl_object *")
    (wt ",cl_object *lex" n))
  (when closure-p
    (wt-h1 ", cl_object")
    (wt ", cl_object env0"))
  (let ((lcl 0))
    (declare (fixnum lcl))
    (dolist (var requireds)
      (wt-h1 ", cl_object ")
      (wt ", cl_object ") (wt-lcl (incf lcl))))
  (when va_args
    (wt-h1 ", ...")
    (wt ", ..."))
  (wt-h1 ");")
  (wt ")")

  (let* ((*lcl* 0) (*temp* 0) (*max-temp* 0)
	 (*lex* 0) (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*max-env* *env*) (*env-lvl* 0)
	 (*level* level)
	 (*exit* 'RETURN) (*unwind-exit* '(RETURN))
	 (*destination* 'RETURN) (*reservation-cmacro* (next-cmacro)))
    (wt-nl1 "{")
    (wt " VT" *reservation-cmacro*
	" VLEX" *reservation-cmacro*
	" CLSR" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (when (fun-closure fun)
      (let ((clv-used (remove-if
		       #'(lambda (x)
			   (or
			    ;; non closure variable
			    (not (ref-ref-ccb x))
			    ;; special variable
			    (eq (var-kind x) 'special)
			    ;; parameter of this closure
			    ;; (not yet bound, therefore var-loc is OBJECT)
			    (eq (var-loc x) 'OBJECT)))
		       (c1form-local-referred lambda-expr))))
        (setq clv-used (sort clv-used #'> :key #'var-loc))
        (when clv-used
          (wt-nl "{cl_object scan=env0;")
          (do ((n (1- (fun-env fun)) (1- n))
               (bs clv-used))
              ((minusp n))
            (when (= n (var-loc (car bs)))
              (wt-nl "CLV" n "= &CAR(scan);")
	      (wt-comment (var-name (car bs)))
              (pop bs))
            (unless bs (return))
            (when (plusp n) (wt " scan=CDR(scan);")))
          (wt "}"))))
    (c2lambda-expr (c1form-arg 0 lambda-expr)
		   (c1form-arg 2 lambda-expr)
		   (fun-cfun fun) (fun-name fun)
		   (fun-closure fun))
    (wt-nl1 "}")
    (wt-function-epilogue closure-p))	; we should declare in CLSR only those used
  )

(defun t2function-constant (funob fun)
  (let ((previous (new-local *level* fun funob)))
    (if (and previous (fun-var previous))
	(setf (fun-var fun) (fun-var previous))
	(let ((loc (data-empty-loc)))
	  (wt-nl loc " = ") (wt-make-closure fun) (wt ";")
	  (setf (fun-var fun) loc))))
)

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Should remove the need for a special handling of
;;; DEFUN as a toplevel form.
;;;
(defun c1fset (args)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((fun (c1expr def)))
      (cond ((and (eq (c1form-name fun) 'FUNCTION)
		  ;; When a function is 'CONSTANT, it is not a closure!
	          (eq (c1form-arg 0 fun) 'CONSTANT)
		  (typep macro 'boolean)
		  (typep pprint '(or integer null)))
	     ;; We need no function constant
	     (pop *top-level-forms*)
	     (make-c1form* 'SI:FSET :args
			   (c1expr fname)
			   (c1form-arg 1 fun) ;; Lambda form
			   (c1form-arg 2 fun) ;; Function object
			   macro
			   pprint))
	    (t
	     (c1call-global 'SI:FSET (list fname def macro pprint)))))))

(defun c2fset (fname funob fun macro pprint)
  (let* ((*inline-blocks* 0)
	 (fname (first (coerce-locs (inline-args (list fname)))))
	 (cfun (fun-cfun fun)))
    ;; FIXME! Look at c2function!
    (new-local 0 fun funob)
    (if macro
	(wt-nl "cl_def_c_macro(" fname "," cfun ",-1);")
	(wt-nl "cl_def_c_function_va(" fname "," cfun ");"))
    (close-inline-blocks)))  

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(put-sysprop 'COMPILER-LET 'T1 #'t1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 #'t1eval-when)
(put-sysprop 'PROGN 'T1 #'t1progn)
(put-sysprop 'DEFUN 'T1 #'t1defun)
(put-sysprop 'DEFVAR 'T1 #'t1defvar)
(put-sysprop 'MACROLET 'T1 #'t1macrolet)
(put-sysprop 'LOCALLY 'T1 #'t1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 #'t1symbol-macrolet)
(put-sysprop 'CLINES 'T1 't1clines)
(put-sysprop 'DEFCFUN 'T1 't1defcfun)
;(put-sysprop 'DEFENTRY 'T1 't1defentry)
(put-sysprop 'DEFLA 'T1 't1defla)
(put-sysprop 'DEFCBODY 'T1 't1defCbody)	; Beppe
;(put-sysprop 'DEFUNC 'T1 't1defunC)	; Beppe
(put-sysprop 'LOAD-TIME-VALUE 'C1 'c1load-time-value)
(put-sysprop 'SI:FSET 'C1 'c1fset)

;;; Pass 2 initializers.

(put-sysprop 'DECL-BODY 't2 #'t2decl-body)
(put-sysprop 'PROGN 'T2 #'t2progn)
(put-sysprop 'DEFUN 'T2 #'t2defun)
(put-sysprop 'ORDINARY 'T2 #'t2ordinary)
(put-sysprop 'DECLARE 'T2 #'t2declare)
(put-sysprop 'DEFVAR 'T2 #'t2defvar)
;(put-sysprop 'DEFENTRY 'T2 't2defentry)
(put-sysprop 'DEFCBODY 'T2 't2defCbody)	; Beppe
;(put-sysprop 'DEFUNC 'T2	't2defunC); Beppe
(put-sysprop 'FUNCTION-CONSTANT 'T2 't2function-constant); Beppe
(put-sysprop 'LOAD-TIME-VALUE 'T2 't2load-time-value)
(put-sysprop 'SI:FSET 'C2 'c2fset)

;;; Pass 2 C function generators.

(put-sysprop 'DECL-BODY 't3 #'t3decl-body)
(put-sysprop 'PROGN 'T3 #'t3progn)
(put-sysprop 'CLINES 'T3 't3clines)
(put-sysprop 'DEFCFUN 'T3 't3defcfun)
;(put-sysprop 'DEFENTRY 'T3 't3defentry)
(put-sysprop 'DEFCBODY 'T3 't3defCbody)	; Beppe
;(put-sysprop 'DEFUNC 'T3 't3defunC)	; Beppe
