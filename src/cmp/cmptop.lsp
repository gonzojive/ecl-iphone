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
                    *funarg-vars*
		    (*setjmps* 0))
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd setf-symbol) ; #+cltl2
	(cond
            ((symbolp fun)
             (cond ((get fun 'PACKAGE-OPERATION)
                    (when *non-package-operation*
                      (cmpwarn "The package operation ~s was in a bad place."
                               form))
		    (cmp-eval form)
                    (wt-data-package-operation form))
                   ((setq fd (get fun 'T1))
                    (when *compile-print* (print-current-form))
                    (funcall fd args))
                   ((get fun 'C1) (t1ordinary form))
                   ((setq fd (macro-function fun))
                    (t1expr* (cmp-expand-macro fd fun (cdr form))))
		   ((and (setq fd (assoc fun *funs*))
			 (eq (second fd) 'MACRO))
		    (t1expr* (cmp-expand-macro (third fd) fun (cdr form))))
                   (t (t1ordinary form))
                   ))
	    ;; #+cltl2
            ((setq setf-symbol (si::setf-namep fun))
	     (t1ordinary form))
            ((consp fun) (t1ordinary form))
            (t (cmperr "~s is illegal function." fun)))
           ))))

(defun t2expr (form)
  ;(pprint (cons 'T2 form))
  (when form
    (let ((def (get (car form) 'T2)))
      (when def (apply def (cdr form))))))

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
	 (when *compile-print*
	   (print-emitting (fun-name (cadar lfs))))
	 (locally (declare (notinline t3local-fun))
	   ;; so disassemble can redefine it
	   (apply 't3local-fun (car lfs)))))))

(defun t3expr (form)
  ;(pprint (cons 'T3 form))
  (when form
    (emit-local-funs)
    (setq *funarg-vars* nil)
    (let ((def (get (car form) 'T3)))
      (when def
	;; new local functions get pushed into *local-funs*
	(when (and *compile-print*
		   (member (car form)
			   '(DEFUN DEFMACRO DEFCBODY) ; DEFENTRY DEFUNC
			   :test #'eq))
	  (print-emitting (second form)))
	(apply def (cdr form))))
    (emit-local-funs)
    (setq *funarg-vars* nil)))

(defun ctop-write (name h-namestring data-namestring
		        &optional system-p
			&aux (vv-reservation (next-cmacro)) def
		   	top-output-string
			(*volatile* " volatile "))

  ;(let ((*print-level* 3)) (pprint *top-level-forms*))
  (setq *top-level-forms* (nreverse *top-level-forms*))

  (wt-nl1 "#include \"" h-namestring "\"")
  (wt-h "#ifdef __cplusplus")
  (wt-h "extern \"C\" {")
  (wt-h "#endif")
  ;;; Initialization function.
  (let* ((*lcl* 0) (*lex* 0) (*max-lex* 0) (*max-env* 0) (*max-temp* 0)
	 (*unboxed* nil)
	 (*reservation-cmacro* (next-cmacro))
	 (c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*emitted-local-funs* nil)
	 (*compiler-declared-globals* (make-hash-table))
	 #+PDE (optimize-space (>= *space* 3)))
    (wt-nl1 "static const char *compiler_data_text =")
    (wt-nl1 "#include \"" data-namestring "\"")
    (wt-nl1 ";")
    (wt-nl1 "#ifdef __cplusplus")
    (wt-nl1 "extern \"C\"")
    (wt-nl1 "#endif")
    (wt-nl1 "void init_" (init-function-name name) "(cl_object flag)")
    (wt-nl1 "{ VT" *reservation-cmacro* " CLSR" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (wt-nl "if (!FIXNUMP(flag)){")
    (wt-nl "Cblock=flag;")
    #-boehm-gc
    (wt-nl "flag->cblock.data = VV;")
    (wt-nl "flag->cblock.data_size = VM;")
    (wt-nl "flag->cblock.data_text = compiler_data_text;")
    (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
    (wt-nl "return;}")
    #+boehm-gc
    (wt-nl "VV = Cblock->cblock.data;")
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
  (dolist (x *reservations*)
    (wt-h "#define VM" (car x) " " (cdr x)))
  (incf *next-vv*)
  (wt-h "#define VM" vv-reservation " " *next-vv*)
  (wt-h "#define VM " *next-vv*)
  #+boehm-gc
  (wt-h "static cl_object *VV;")
  #-boehm-gc
  (if (zerop *next-vv*)
    (wt-h "static cl_object VV[1];")
    (wt-h "static cl_object VV[VM];"))
  (when *linking-calls*
    (dotimes (i (length *linking-calls*))
      (declare (fixnum i))
      (wt-h "static cl_object LKF" i "(int, ...);")
      (wt-h "static cl_object (*LK" i ")(int, ...)=LKF" i ";"))
    )
  ;;; Global entries for directly called functions.
  (dolist (x *global-entries*)
    (apply 'wt-global-entry x))
  
  ;;; Initial functions for linking calls.
  (dolist (x *linking-calls*)
    (let ((i (second x)))
      (wt-nl1 "static cl_object LKF" i
	      "(int narg, ...) {TRAMPOLINK(narg," (third x) ",&LK" i ");}")))

  (wt-h "#define compiler_data_text_size " *wt-string-size*)

  (wt-h "#ifdef __cplusplus")
  (wt-h "}")
  (wt-h "#endif")
  (wt-h1 top-output-string))

(defun t1eval-when (args &aux (load-flag nil) (compile-flag nil))
  (when (endp args) (too-few-args 'eval-when 1 0))
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
	   '(PROGN NIL)))))

(defun t1compiler-let (args &aux (symbols nil) (values nil))
  (when (endp args) (too-few-args 'compiler-let 1 0))
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
  (list 'PROGN (mapcar #'t1expr* args)))

(defun t2progn (args)
  (mapcar #'t2expr args))

(defun t3progn (args)
  (mapcar #'t3expr args))

(defun exported-fname (name)
  (or (get name 'Lfun)
      (next-cfun)))

(defun t1defun (args &aux (setjmps *setjmps*))
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'defun 2 (length args)))
  (when (not (symbolp (car args)))
    (return-from t1defun (t1expr* (macroexpand (cons 'defun args)))))
  (when *compile-time-too* (cmp-eval (cons 'DEFUN args)))
  (setq *non-package-operation* t)
  (let* (lambda-expr
	 (fname (car args))
	 (cfun (exported-fname fname))
	 (doc nil)
	 output)
    
    (setq lambda-expr (c1lambda-expr (cdr args) fname))
    (unless (eql setjmps *setjmps*)
      (setf (info-volatile (second lambda-expr)) t))
    (when (and (setq doc (fourth lambda-expr))
	       (setq doc (si::expand-set-documentation fname 'function doc)))
      (t1expr `(progn ,@doc)))
    (add-load-time-values)
    (setq output (new-defun fname cfun lambda-expr *special-binding*))
    (when
      (and
       (get fname 'PROCLAIMED-FUNCTION)
       (let ((lambda-list (third lambda-expr)))
         (declare (list lambda-list))
         (and (null (second lambda-list))	; no optional
              (null (third lambda-list))	; no rest
              (null (fourth lambda-list))	; no keyword
              (< (length (car lambda-list)) lambda-parameters-limit))))
      (flet
	  ((make-inline-string (cfun args)
	     (if (null args)
		 (format nil "LI~a()" cfun)
		 (let ((o (make-array 100 :element-type 'BASE-CHAR
				      :fill-pointer 0)))
		   (format o "LI~a(" cfun)
		   (do ((l args (cdr l))
			(n 0 (1+ n)))
		       ((endp (cdr l))
			(format o "#~d)" n))
		     (declare (fixnum n))
		     (format o "#~d," n))
		   o))))
	(let ((pat (get fname 'PROCLAIMED-ARG-TYPES))
	      (prt (get fname 'PROCLAIMED-RETURN-TYPE)))
	  (push (list fname pat prt t
		      (not (member prt
				   '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT)
				   :test #'eq))
		      (make-inline-string cfun pat))
		*inline-functions*))))
    output))

;;; Mechanism for sharing code:
(defun new-defun (fname cfun lambda-expr special-binding)
  (let ((previous (dolist (form *global-funs*)
		    (when (and (eq 'DEFUN (car form))
			       (equal special-binding (fifth form))
			       (similar lambda-expr (third form)))
		      (return (second form))))))
    (if previous
	(progn
	  (cmpnote "Sharing code for function ~A" fname)
	  (list 'DEFUN fname previous nil special-binding *funarg-vars*))
	(let ((fun-desc (list fname cfun lambda-expr special-binding
			      *funarg-vars*)))
	  (push fun-desc *global-funs*)
	  (cons 'DEFUN fun-desc)))))

(defun similar (x y)
  (or (equal x y)
      (and (consp x)
	   (consp y)
	   (similar (car x) (car y))
	   (similar (cdr x) (cdr y)))
      (and (var-p x)
	   (var-p y)
	   (do ((i 2 (1+ i)))
	       ((= i (length x)) t)
	     (declare (fixnum i))
	     (unless (equal (svref x i) (svref y i))
	       (return nil))))
      (and (typep x 'VECTOR)
	   (typep y 'VECTOR)
	   (every #'similar x y))))

(defun wt-if-proclaimed (fname cfun vv lambda-expr)
  (when (fast-link-proclaimed-type-p fname)
    (let ((arg-c (length (car (third lambda-expr))))
	  (arg-p (length (get fname 'PROCLAIMED-ARG-TYPES))))
      (if (= arg-c arg-p)
	(cmpwarn
	 " ~a is proclaimed but not in *inline-functions* ~
          ~%T1defun could not assure suitability of args for C call" fname)
	(cmpwarn
	 "Number of proclaimed args for ~a was ~a. ~
          ~%;;; Its definition had ~a." fname arg-p arg-c)))))

(defun volatile (info)
   (if (info-volatile info) "volatile " ""))

(defun register (var)
  (if (and (equal *volatile* "")
	   (> (var-ref var) (the fixnum *register-min*)))
      "register "
      ""))

(defun t2defun (fname cfun lambda-expr sp funarg-vars
                      &aux (vv (add-symbol fname))
		      (nkey (length (fifth (third lambda-expr)))))
  (declare (ignore sp funarg-vars))
  (when (get fname 'NO-GLOBAL-ENTRY) (return-from t2defun nil))
  (if (numberp cfun)
    (wt-nl "MF(" vv ",(cl_objectfn)L" cfun ",Cblock);")
    (wt-nl "MF(" vv ",(cl_objectfn)" cfun ",Cblock);"))
  (when (get fname 'PROCLAIMED-FUNCTION)
	(wt-if-proclaimed fname cfun vv lambda-expr))
)

(defun t3defun (fname cfun lambda-expr sp funarg-vars
                      &aux inline-info lambda-list requireds
                      (*current-form* (list 'DEFUN fname))
                      (*volatile* (when lambda-expr
                                    (volatile (second lambda-expr))))
		      (*lcl* 0) (*temp* 0) (*max-temp* 0)
		      (*next-unboxed* 0) (*unboxed* nil)
		      (*lex* *lex*) (*max-lex* *max-lex*)
		      (*env* *env*) (*max-env* 0) (*level* *level*))
  (setq *funarg-vars* funarg-vars)
  (when lambda-expr		; Not sharing code.
    (setq lambda-list (third lambda-expr)
          requireds (car lambda-list))
    (analyze-regs (info-referred-vars (second lambda-expr)))

    (if (dolist (v *inline-functions*)
	  (and (eq (car v) fname)
	       (return (setq inline-info v))))
      
	;; Local entry
	(let* ((*exit* (case (third inline-info)
			 (FIXNUM 'RETURN-FIXNUM)
			 (CHARACTER 'RETURN-CHARACTER)
			 (LONG-FLOAT 'RETURN-LONG-FLOAT)
			 (SHORT-FLOAT 'RETURN-SHORT-FLOAT)
			 (otherwise 'RETURN-OBJECT)))
	       (*unwind-exit* (list *exit*))
	       (*destination* *exit*)
	       (*reservation-cmacro* (next-cmacro)))

	  ;; Add global entry information.
	  (push (list fname cfun (second inline-info) (third inline-info))
		*global-entries*)
	  (wt-comment "local entry for function " fname)
	  (let ((string
		 (with-output-to-string (*compiler-output1*)
		   (wt-nl1 "static " (rep-type (third inline-info)) "LI" cfun "(")
		   (do ((vl requireds (cdr vl))
			(types (second inline-info) (cdr types))
			(prev-type nil) (var)
			(lcl (1+ *lcl*) (1+ lcl)))
		       ((endp vl))
		     (declare (fixnum lcl))
		     (setq var (first vl))
		     (when (member (car types)
				   '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT)
				   :test #'eq)
		       ;; so that c2lambda-expr will know its proper type.
		       (setf (var-kind var) (car types)))
		     (when prev-type (wt ","))
		     (wt *volatile* (register var)
			 (rep-type (car types)))
		     (setq prev-type (car types))
		     (wt-lcl lcl))
		   (wt ")"))))
	    (wt-h string ";")
	    (wt-nl1 string))

	  ;; Now the body.
	  (let ((*tail-recursion-info* (cons fname requireds))
		(*unwind-exit* *unwind-exit*))
	    (wt-nl1 "{")
	    (wt-function-prolog nil 'LOCAL-ENTRY)
	    (c2lambda-expr lambda-list (third (cddr lambda-expr)) cfun fname
			   nil 'LOCAL-ENTRY)
	    (wt-nl1 "}")
	    (wt-function-epilogue)))

	;; normal (non proclaimed) function:
	(let ((*exit* 'RETURN) (*unwind-exit* '(RETURN))
	      (*destination* 'RETURN) (*reservation-cmacro* (next-cmacro))
	      (va_args (or (second lambda-list)
			   (third lambda-list)
			   (fourth lambda-list))))

	  (wt-comment "function definition for " fname)
	  (if (numberp cfun)
	      (progn
		(wt-nl1 "static cl_object L" cfun "(int narg")
		(wt-h "static cl_object L" cfun "(int narg"))
	      (progn
		(wt-nl1 "cl_object " cfun "(int narg")
		(wt-h "cl_object " cfun "(int narg")))
	  (do ((vl requireds (cdr vl))
	       (lcl (1+ *lcl*) (1+ lcl)))
	      ((endp vl))
	    (declare (fixnum lcl))
	    (wt ", cl_object ") (wt-lcl lcl)
	    (wt-h1 ", cl_object"))
	  (when va_args
	    (wt ", ...")
	    (wt-h1 ", ..."))
	  (wt ")")
	  (wt-h1 ");")
      
	  (wt-nl1 "{")
	  (wt-function-prolog sp)
	  (c2lambda-expr lambda-list (third (cddr lambda-expr)) cfun fname)
	  (wt-nl1 "}")
	  (wt-function-epilogue)))))

(defun wt-function-prolog (&optional sp local-entry)
  (wt " VT" *reservation-cmacro*
      " VLEX" *reservation-cmacro*
      " CLSR" *reservation-cmacro*)
  (wt-nl "cl_object value0;")
  (when (and (not local-entry) *safe-compile*)
    (wt-nl "cs_check;"))
  (when sp (wt-nl "bds_check;"))
  ; (when *compiler-push-events* (wt-nl "ihs_check;"))
  )

(defun wt-function-epilogue (&optional closure-p)
  (push (cons *reservation-cmacro* (+ *max-temp* (length *unboxed*)))
        *reservations*)
  (wt-h "#define VT" *reservation-cmacro*)
  (when (plusp *max-temp*)
    (wt-h1 " cl_object ")
    (dotimes (i *max-temp*)
      (wt-h1 "T") (wt-h1 i)
      (unless (= (1+ i) *max-temp*) (wt-h1 ",")))
    (wt-h1 ";"))
;  (wt-h "#define VU" *reservation-cmacro*)
;  (when *unboxed*
;    (format *compiler-output2* " ~{~{~aU~a; ~}~}" *unboxed*))
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

;;; Checks the register slots of variables, and finds which
;;; variables should be in registers, reducing the var-ref value
;;; in the remaining.  Data and address variables are done separately.
(defun analyze-regs (vars)
  (flet ((analyze-regs1 (vars want &aux (tem 0) (real-min 3) (this-min 100000)
			      (have 0))
           (declare (fixnum want tem real-min this-min have))
	   (do ((vs vars) (v))
	       ((null vs))
	     (setq v (pop vs)
		   tem (var-ref v))
	     (when (>= tem real-min)
	       (incf have)
	       (setq this-min (min this-min tem))
	       (when (> have want)
		 (setq have 0
		       real-min (1+ this-min)
		       this-min 1000000
		       vs vars))))
	   (when (< have want) (decf real-min))
	   (dolist (v vars)
	     (when (< (var-ref v) real-min)
	       ;; don't put 1, otherwise optimization may discard
	       ;; variable
	       (setf (var-ref v) (min (var-ref v) *register-min*))))))
    (let (addr data)
      (dolist (v vars)
	(if (member (var-type v)
		    '(FIXNUM CHARACTER SHORT-FLOAT LONG-FLOAT)
		    :test #'eq)
	    (pushnew v data)
	    (pushnew v addr)))
      (analyze-regs1 addr *free-address-registers*)
      (analyze-regs1 data *free-data-registers*))))

(defun wt-global-entry (fname cfun arg-types return-type)
    (when (get fname 'NO-GLOBAL-ENTRY) (return-from wt-global-entry nil))
    (wt-comment "global entry for the function " fname)
    (wt-nl1 "static cl_object L" cfun "(int narg")
    (wt-h "static cl_object L" cfun "(int")
    (do ((vl arg-types (cdr vl))
	 (lcl (1+ *lcl*) (1+ lcl)))
	((endp vl) (wt1 ")"))
      (declare (fixnum lcl))
      (wt1 ", cl_object ") (wt-lcl lcl)
      (wt-h1 ", cl_object"))
    (wt-h1 ");")
    (wt-nl1 "{")
    (when (or *safe-compile* *compiler-check-args*)
          (wt-nl "check_arg(" (length arg-types) ");"))
    (wt-nl "NValues=1;")
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
    (FIXNUM "int ")
    (CHARACTER "unsigned char ")
    (SHORT-FLOAT "float ")
    (LONG-FLOAT "double ")
    (otherwise "cl_object ")))

(defun t1defmacro (args)
  (when (or (endp args) (endp (cdr args)))
    (too-few-args 'defmacro 2 (length args)))
  (cmpck (not (symbolp (car args)))
         "The macro name ~s is not a symbol." (car args))
  (cmp-eval (cons 'DEFMACRO args))
  (setq *non-package-operation* t)
  (let (macro-lambda (cfun (next-cfun)) (doc nil) (ppn nil))
    (setq macro-lambda (c1dm (car args) (second args) (cddr args)))
    (when (second macro-lambda) (setq ppn (add-object (second macro-lambda))))
    (when (and (setq doc (car macro-lambda))
	       (setq doc (si::expand-set-documentation (car args) 'function doc)))
      (t1expr `(progn ,@doc)))
    (add-load-time-values)
    (list 'DEFMACRO (car args) cfun (cddr macro-lambda) ppn
		*special-binding*)))

(defun t2defmacro (fname cfun macro-lambda ppn sp &aux (vv (add-symbol fname)))
  (declare (ignore macro-lambda sp))
  (when (< *space* 3)
    (when ppn
      (wt-nl "(void)putprop(" vv "," ppn ",siSpretty_print_format);")
      (wt-nl)))
  (wt-h "static cl_object L" cfun "();")
  (wt-nl "MM(" vv ",(cl_objectfn)L" cfun ",Cblock);"))

(defun t3defmacro (fname cfun macro-lambda ppn sp
                         &aux (*lcl* 0) (*temp* 0) (*max-temp* 0)
                         (*lex* *lex*) (*max-lex* *max-lex*)
			 (*next-unboxed* 0) *unboxed*
                         (*env* *env*) (*max-env* 0) (*level* *level*)
			 (*volatile*
			  (if (get fname 'CONTAINS-SETJMP) " volatile " ""))
                         (*exit* 'RETURN) (*unwind-exit* '(RETURN))
                         (*destination* 'RETURN)
                         (*reservation-cmacro* (next-cmacro)))
  (wt-comment "macro definition for " fname)
  (wt-nl1 "static cl_object L" cfun "(int narg, cl_object V1, cl_object V2)")
  (wt-nl1 "{")
  (wt-function-prolog sp)
  (c2dm fname (car macro-lambda) (second macro-lambda) (third macro-lambda)
        (fourth macro-lambda))
  (wt-nl1 "}")
  (wt-function-epilogue)
  )

(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (setq *non-package-operation* t)
  (setq form (c1expr form))
  (add-load-time-values)
  (list 'ORDINARY form))

(defun t2ordinary (form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
        (c2expr form)
        (wt-label *exit*)))

(defun add-load-time-values ()
  (when (listp *load-time-values*)
    (setq *top-level-forms* (nconc *load-time-values* *top-level-forms*))
    (setq *load-time-values* nil)))

(defun c1load-time-value (form)
  (cond ((listp *load-time-values*)
	 (incf *next-vv*)
	 (push (list 'LOAD-TIME-VALUE *next-vv* (c1expr form)) *load-time-values*)
	 (wt-data 0)
	 (list 'LOCATION (make-info :type t)
	       (list 'VV (format nil "VV[~d]" *next-vv*))))
	(t
	 (add-object (cmp-eval form)))))

(defun t2load-time-value (ndx form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* (list 'VV ndx)))
        (c2expr form)
        (wt-label *exit*)))

(defun t2declare (vv)
  (wt-nl vv "->symbol.stype=(short)stp_special;"))

(defun t1defvar (args &aux form (doc nil) (name (car args)))
  (when *compile-time-too* (cmp-eval `(defvar ,@args)))
  (setq *non-package-operation* nil)
  (push name *global-vars*)
  (if (endp (cdr args))
      (list 'DECLARE (add-symbol name))
      (progn
	(when (and (setq doc (third args))
		   (setq doc (si::expand-set-documentation name 'variable doc)))
	  (t1expr `(progn ,@doc)))
	(setq form (c1expr (second args)))
	(add-load-time-values)
	(list 'DEFVAR (make-var :name name :kind 'SPECIAL
				      :loc (add-symbol name)) form))))

(defun t2defvar (var form &aux (vv (var-loc var)))
  (wt-nl vv "->symbol.stype=(short)stp_special;")
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* (list 'VAR var)))
        (wt-nl "if(" vv "->symbol.dbind == OBJNULL){")
        (c2expr form)
        (wt "}")
        (wt-label *exit*)))

(defun t1decl-body (decls body)
  (if (null decls)
      (t1progn body)
      (let* ((*function-declarations* *function-declarations*)
	     (*alien-declarations* *alien-declarations*)
	     (*notinline* *notinline*)
	     (*space* *space*)
	     (*compiler-check-args* *compiler-check-args*)
	     (*compiler-push-events* *compiler-push-events*)
	     (dl (c1add-declarations decls)))
	(list 'DECL-BODY dl (t1progn body)))))

(defun t2decl-body (decls body)
  (let ((*compiler-check-args* *compiler-check-args*)
        (*safe-compile* *safe-compile*)
        (*compiler-push-events* *compiler-push-events*)
        (*notinline* *notinline*)
        (*space* *space*))
    (c1add-declarations decls)
    (t2expr body)))

(defun t3decl-body (decls body)
  (let ((*compiler-check-args* *compiler-check-args*)
        (*safe-compile* *safe-compile*)
        (*compiler-push-events* *compiler-push-events*)
        (*notinline* *notinline*)
        (*space* *space*))
    (c1add-declarations decls)
    (t3expr body)))

(defun t1locally (args)
  (multiple-value-bind (body ss ts is other-decl)
      (c1body args t)
    (c1add-globals ss)
    (check-vdecl nil ts is)
    (t1decl-body other-decl body)))

(defun t1macrolet (args &aux (*funs* *funs*))
  (when (endp args) (too-few-args 'macrolet 1 0))
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
  (when (endp args) (too-few-args 'symbol-macrolet 1 0))
  (dolist (def (car args))
    (cmpck (or (endp def) (not (symbolp (car def))) (endp (cdr def)))
           "The symbol-macro definition ~s is illegal." def)
    (push def *vars*))
  (t1locally (cdr args)))

(defun t1clines (args)
  (dolist (s args)
    (cmpck (not (stringp s)) "The argument to CLINE, ~s, is not a string." s))
  (list 'CLINES args))

(defun t3clines (ss) (dolist (s ss) (wt-nl1 s)))

(defun parse-cvspecs (x &aux (cvspecs nil))
  (dolist (cvs x (nreverse cvspecs))
    (cond ((symbolp cvs)
           (push (list 'OBJECT (string-downcase (symbol-name cvs))) cvspecs))
          ((stringp cvs) (push (list 'OBJECT cvs) cvspecs))
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

(defun t3local-fun (closure-p fun lambda-expr
			      ;; if defined by labels can be tail-recursive
                              &aux (level (fun-level fun))
			      (nenvs level)
			      (*volatile* (volatile (second lambda-expr)))
                              (lambda-list (third lambda-expr))
                              (requireds (car lambda-list))
                              (va_args (or (second lambda-list)
                                           (third lambda-list)
                                           (fourth lambda-list))))
  (declare (fixnum level nenvs))
  (wt-comment (if (fun-closure fun) "closure " "local function ")
	      (let ((name (fun-name fun)))
		;; a list is used for lambda-block's
		(if (symbolp name) (or name 'CLOSURE) (first name))))
  (wt-h "static cl_object LC" (fun-cfun fun) "(")
  (wt-nl1 "static cl_object LC" (fun-cfun fun) "(")
  (wt-h1 "int")
  (wt "int narg")
  (dotimes (n level)
    (wt-h1 ",cl_object *")
    (wt ",cl_object *lex" n))
  (when closure-p
    (wt-h1 ", cl_object")
    (wt ", cl_object env0")
    (incf nenvs))
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

  (analyze-regs (info-referred-vars (second lambda-expr)))
  (let* ((*lcl* 0) (*temp* 0) (*max-temp* 0)
	 (*lex* 0) (*max-lex* 0) (*next-unboxed* 0) *unboxed*
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
    (when (plusp nenvs)
      (wt-nl "narg-=" nenvs ";"))	; APPLY counts env as arg
    (when (fun-closure fun)
      (let ((clv-used (remove-if
		       #'(lambda (x)
			   (or
			    ;; non closure variable
			    (not (ref-ref-ccb x))
			    ;; parameter of this closure
			    ;; (not yet bound, therefore var-loc is OBJECT)
			    (eq (var-loc x) 'OBJECT)))
		       (info-local-referred (second lambda-expr)))))
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
    (c2lambda-expr (third lambda-expr) (third (cddr lambda-expr))
		   (fun-cfun fun) (fun-name fun)
		   (fun-closure fun))
    (wt-nl1 "}")
    (wt-function-epilogue closure-p))	; we should declare in CLSR only those used
  )

#|
(defun t1defcfun (args &aux (body nil))
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'defcfun 2 (length args)))
  (cmpck (not (stringp (car args)))
         "The first argument to defCfun ~s is not a string." (car args))
  (cmpck (not (numberp (second args)))
         "The second argument to defCfun ~s is not a number." (second args))
  (dolist (s (cddr args))
    (cond ((stringp s) (push s body))
          ((consp s)
           (cond ((symbolp (car s))
                  (cmpck (special-form-p (car s))
                         "Special form ~s is not allowed in defCfun." (car s))
                  (push (list (cons (car s) (parse-cvspecs (cdr s)))) body))
                 ((and (consp (car s)) (symbolp (caar s))
                       (not (if (eq (caar s) 'QUOTE)
                                (or (endp (cdar s))
                                    (not (endp (cddar s)))
                                    (endp (cdr s))
                                    (not (endp (cddr s))))
                                (special-form-p (caar s)))))
                  (push (cons (cons (caar s)
                                    (if (eq (caar s) 'QUOTE)
                                        (list (add-object (cadar s)))
                                        (parse-cvspecs (cdar s))))
                              (parse-cvspecs (cdr s)))
                        body))
                 (t (cmperr "The defCfun body ~s is illegal." s))))
          (t (cmperr "The defCfun body ~s is illegal." s))))
  (push (list 'DEFCFUN (car args) (second args) (reverse body))
        *top-level-forms*)
  )

(defun t3defcfun (header vs-size body &aux fd narg)
  (wt-comment "C function defined by " 'defcfun)
  (wt-nl1 header)
  (wt-nl1 "{")
  ;;; manca un pezzo, Beppe ???
  (dolist (s body)
    (cond ((stringp s) (wt-nl1 s))
          ((eq (caar s) 'QUOTE)
           (wt-nl1 (cadadr s))
           (if (eq (caadr s) 'OBJECT)
	       (wt "=" (cadar s) ";")
	       (wt "=object_to_" (string-downcase (symbol-name (caadr s)))
		   "(" (cadar s) ");")))
          (t
           (setq narg (length cdar s))
           (cond ((setq fd (assoc (caar s) *global-funs*))
                  (cond (*compiler-push-events*
                         (wt-nl1 "ihs_push(" (add-symbol (caar s)) ");")
                         (wt-nl1 "L" (cdr fd) "();")
                         (wt-nl1 "ihs_pop();"))
                        (t (wt-nl1 "L" (cdr fd) "(" narg))))
                 (t (wt-nl1 "funcall(" (1+ narg) "," (add-symbol (caar s))
                            "->symbol.gfdef"))
                 )
           (dolist (arg (cdar s))
             (wt ",")
             (case (car arg)
               (OBJECT (wt (second arg)))
               (CHAR (wt "CODE_CHAR((int)" (second arg) ")"))
               (INT  (wt "MAKE_FIXNUM((int)" (second arg) ")"))
               (FLOAT (wt "make_shortfloat((float)" (second arg) ")"))
               (DOUBLE (wt "make_longfloat((double)" (second arg) ")"))))
           (wt ");")
           (unless (endp (cdr s))
               (wt-nl1 (cadadr s))
               (case (caadr s)
                     (object (wt "=vs_base[0];"))
                     (otherwise (wt "=object_to_"
                                    (string-downcase (symbol-name (caadr s)))
                                    "(vs_base[0]);")))
               (dolist (dest (cddr s))
                 (wt-nl1 "vs_base++;")
                 (wt-nl1 (second dest))
                 (if (eq (car dest) 'OBJECT)
		     (wt "=(vs_base<vs_top?vs_base[0]:Cnil);")
		     (wt "=object_to_"
			 (string-downcase (symbol-name (car dest)))
			 "((vs_base<vs_top?vs_base[0]:Cnil));")))
               )
             (wt-nl1 "}")
             )))
  (wt-nl1 "}")
  )

(defun t1defentry (args &aux type cname (cfun (next-cfun)) cfspec)
  (when (or (endp args) (endp (cdr args)) (endp (cddr args)))
        (too-few-args 'defentry 3 (length args)))
  (cmpck (not (symbolp (car args)))
         "The function name ~s is not a symbol." (car args))
  (dolist (x (second args))
    (cmpck (not (member x '(OBJECT CHAR* CHAR INT FLOAT DOUBLE)))
           "The C-type ~s is illegal." x))
  (setq cfspec (third args))
  (cond ((symbolp cfspec)
         (setq type 'OBJECT)
         (setq cname (string-downcase (symbol-name cfspec))))
        ((stringp cfspec)
         (setq type 'OBJECT)
         (setq cname cfspec))
        ((and (consp cfspec)
              (member (car cfspec) '(VOID OBJECT CHAR* CHAR INT FLOAT DOUBLE))
              (consp (cdr cfspec))
              (or (symbolp (second cfspec)) (stringp (second cfspec)))
              (endp (cddr cfspec)))
         (setq cname (if (symbolp (second cfspec))
                        (string-downcase (symbol-name (second cfspec)))
                        (second cfspec)))
         (setq type (car cfspec)))
        (t (cmperr "The C function specification ~s is illegal." cfspec)))
  (push (list 'DEFENTRY (car args) cfun (second args) type cname)
        *top-level-forms*)
  (push (cons (car args) cfun) *global-funs*)
  )

(defun t2defentry (fname cfun arg-types type cname
                         &aux (vv (add-symbol fname)))
  (declare (ignore arg-types type cname))
  (wt-h "static L" cfun "();")
  (wt-nl "MF(" vv ",(cl_objectfn)L" cfun ",Cblock);")
  )

(defun t3defentry (fname cfun arg-types type cname)
  (wt-comment "function definition for " fname)
  (wt-nl1 "static L" cfun "()")
  (wt-nl1 "{	cl_object *old_base=vs_base;")
  (unless (eq type 'VOID) (wt-nl (string-downcase (symbol-name type)) " x;"))
  (when *safe-compile* (wt-nl "check_arg(" (length arg-types) ");"))
  (unless (eq type 'VOID) (wt-nl "x="))
  (wt-nl cname "(")
  (unless (endp arg-types)
          (do ((types arg-types (cdr types))
               (i 0 (1+ i)))
              (nil)
              (declare (object types) (fixnum i))
              (case (car types)
                    (object (wt-nl "vs_base[" i "]"))
		    (char*
		     (if *safe-compile*
			 (wt-nl "object_to_string"
				"(vs_base[" i "])")
		       (wt-nl "(vs_base[" i "]->string.self)")))
		    (int
		     (if *safe-compile*
			 (wt-nl "object_to_int"
				"(vs_base[" i "])")
		       (wt-nl "fix(vs_base[" i "])")))
                    (otherwise
                     (wt-nl "object_to_"
                            (string-downcase (symbol-name (car types)))
                            "(vs_base[" i "])")))
              (when (endp (cdr types)) (return))
              (wt ",")))
  (wt ");")
  (wt-nl "vs_top=(vs_base=old_base)+1;")
  (wt-nl "vs_base[0]=")
  (case type
        (VOID (wt "Cnil"))
        (OBJECT (wt "x"))
        (CHAR* (wt "make_simple_string(x)"))
        (CHAR (wt "CODE_CHAR(x)"))
        (INT  (wt "MAKE_FIXNUM(x)"))
        (FLOAT (wt "make_shortfloat(x)"))
        (DOUBLE (wt "make_longfloat(x)"))
        )
  (wt ";")
  (wt-nl1 "}")
  )

(defun t1defla (args) (declare (ignore args)))
|#
;;; ----------------------------------------------------------------------
;;; Function definition with in-line body expansion.
;;; This is similar to defentry, except that the C body is supplied
;;; instead of a C function to call.
;;; Besides, Lisp types are used instead of C types, for proper coersion.
;;;
;;; (defCbody logand (fixnum fixnum) fixnum "(#0) & (#1)")
;;;
;;; ----------------------------------------------------------------------

(defun t1defCbody (args &aux fun (cfun (next-cfun)))
  (when (or (endp args) (endp (cdr args))
	    (endp (cddr args)) (endp (cdddr args)))
        (too-few-args 'defCbody 4 (length args)))
  (setq fun (first args))
  (cmpck (not (symbolp fun))
         "The function name ~s is not a symbol." fun)
  (push (list fun cfun) *global-funs*)
  (list 'DEFCBODY fun cfun (second args) (third args) (fourth args)))

(defun t2defCbody (fname cfun arg-types type body
                         &aux (vv (add-symbol fname)))
  (declare (ignore arg-types type body))
  (wt-h "static cl_object L" cfun "();")
  (wt-nl "MF(" vv ",(cl_objectfn)L" cfun ",Cblock);")
  )

#|
;;; Simpler version. Does not perform type checking of args though.

(defun t3defCbody (fname cfun arg-types type body &aux args)
  (wt-comment "function definition for " fname)
  (wt-nl1 "static L" cfun "(int narg")
  (do ((vl arg-types (cdr vl))
       (lcl 1 (1+ lcl)))
      ((endp vl))
    (declare (fixnum lcl))
    (push (list 'VAR (make-info :type (car vl))
		(list (make-var :kind 'OBJECT :loc lcl))) args)
    (wt ", cl_object ") (wt-lcl lcl)
    )
  (wt ")")
  (wt-nl1 "{")
  (when *safe-compile* (wt-nl "check_arg(" (length arg-types) ");"))
  (let ((*inline-functions* (push (list fname arg-types type T NIL body)
				  *inline-functions*))
	(*destination* (if type 'RETURN 'TRASH)))
    (c2expr* `(CALL-GLOBAL ,(make-info :type type)
	       ,fname ,(nreverse args) ,type)))
  (wt-nl "RETURN(" (if type 1 0) ");")
  (wt-nl1 "}")
  )
|#

(eval-when (compile eval)		; also in cmpinline.lsp
  ;; by mds@sepgifbr.sep.de.edf.fr (M.Decugis)
  (defmacro parse-index (fun i)
    `(multiple-value-bind (a-read endpos)
      (parse-integer ,fun :start (1+ ,i) :junk-allowed t)
      (setq ,i (1- endpos))
      a-read))
  )

(defun t3defCbody (fname cfun arg-types type body)
  (wt-comment "function definition for " fname)
  (wt-nl1 "static cl_object L" cfun "(int narg")
  (do ((vl arg-types (cdr vl))
       (lcl 1 (1+ lcl)))
      ((endp vl))
    (declare (fixnum lcl))
    (wt ", cl_object ") (wt-lcl lcl)
    )
  (wt ")")
  (wt-nl1 "{")
  (flet ((lisp2c-type (type)
		      (case type
			    ((NIL) 'VOID)
			    (CHARACTER 'CHAR)
			    (FIXNUM 'INT)
			    (LONG-FLOAT 'DOUBLE)
			    (SHORT-FLOAT 'FLOAT)
			    (otherwise 'OBJECT)))
	 (wt-inline-arg (fun locs &aux (i 0))
            (declare (fixnum i))
	    (cond ((stringp fun)
		   (when (char= (char (the string fun) 0) #\@)
			 (setq i 1)
			 (do ()
			     ((char= (char (the string fun) i) #\;) (incf i))
			     (incf i)))
		   (do ((size (length (the string fun))))
		       ((>= i size))
		       (declare (fixnum size))
		       (let ((char (char (the string fun) i)))
			 (declare (character char))
			 (if (char= char #\#)
			     (wt (nth (parse-index fun i) locs))
			   (princ char *compiler-output1*))
			 (incf i)))))))
	(when type
	  (let ((ctype (lisp2c-type type)))
	    (if (eq ctype 'OBJECT)
	      (wt-nl "cl_object x;")
	      (wt-nl (string-downcase ctype) " x;"))))
	(when *safe-compile* (wt-nl "check_arg(" (length arg-types) ");"))
	(when type (wt-nl "x="))
	(wt-inline-arg
	 body
	 (do ((types arg-types (cdr types))
	      (i 1 (1+ i))
	      (lst))
	     ((null types) (nreverse lst))
	     (declare (object types) (fixnum i))
	     (push
	      (if (eq (lisp2c-type (car types)) 'OBJECT)
		  (format nil "V~d" i)
		(format nil "object_to_~(~a~)(V~d)"
			(lisp2c-type (car types)) i))
	      lst)))
	(wt ";")
	(wt-nl "NValues=1;")
	(wt-nl "return ")
	(case type
	      ((NIL) (wt "Cnil"))
	      (BOOLEAN (wt "(x?Ct:Cnil)"))
	      (CHARACTER (wt "CODE_CHAR(x)"))
	      (FIXNUM (wt "MAKE_FIXNUM(x)"))
	      (SHORT-FLOAT (wt "make_shortfloat(x)"))
	      (LONG-FLOAT (wt "make_longfloat(x)"))
	      (otherwise (wt "x"))
	      )
	(wt ";}")
	))
#|
;;; ----------------------------------------------------------------------
;;; Lisp function defined in C.
;;; Arguments and results are passed on stack.
;;;
;;; ----------------------------------------------------------------------

(defun t1defunC (args &aux fun lambda-list (cfun (next-cfun)))
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'defunC 2 (length args)))
  (setq fun (first args))
  (cmpck (not (symbolp fun))
         "The function name ~s is not a symbol." fun)
  (setq lambda-list (second args))
  (cmpck (not (listp lambda-list))
         "The lambda list ~s is not a list." lambda-list)
  (dolist (s (cddr args))
    (cmpck (not (stringp s)) "The argument to DEFUNC, ~s, is not a string." s))
  (push (list 'DEFUNC fun cfun lambda-list (cddr args)) *top-level-forms*)
  (push (cons fun cfun) *global-funs*)
  )

(defun t2defunC (fname cfun lambda-list body
		       &aux (vv (add-symbol fname)))
  (declare (ignore lambda-list body))
  (wt-h "static L" cfun "();")
  (wt-nl "MF(" vv ",(cl_objectfn)L" cfun ",Cblock);")
  )

(defun t3defunC (fname cfun lambda-list body)
  (wt-comment "function definition for " fname)
  (wt-nl1 "static L" cfun "()")
  (multiple-value-bind
	(requireds optionals rest key-flag keywords allow-other-keys auxs)
      (parse-lambda-list lambda-list)
    (let ((nreq (length requireds))
	  (nopt (length optionals)))
      (declare (fixnum nreq nopt))
      ;; Emit declarations:
      (wt-nl1 "{")
      (dolist (v requireds)
	(wt-nl "cl_object " (string-downcase v) ";"))
      (do ((scan optionals (cdr scan))
	   (v))
	  ((null scan))
	(setq v (car scan))
	(when (consp v)
	  (setq v (car v))
	  (cmpwarn "Discarding default value for optional variable ~s" v))
        (setq v (string-downcase v))
	(rplaca scan v)
	(wt-nl "cl_object " v "=Cnil;"))
      (when rest
	(setq rest (string-downcase rest))
	(wt-nl "cl_object " rest "=Cnil;"))
      (dolist (v keywords)
	(wt-nl "cl_object " (string-downcase (second v)) ";"))
      (when auxs
	(cmpwarn "&aux variables in defunC discarded"))
      (if (and (null optionals)
	       (null rest)
	       (null key-flag))
	  (wt-nl "check_arg(" nreq ");")
	  (wt-nl "if (narg < " nreq  ") FEtoo_few_arguments(&narg);"))
      ;; Assign requireds:
      (do ((i 0 (1+ i))
	   (vars requireds (cdr vars)))
	  ((null vars))
	(declare (fixnum i))
	(wt-nl (string-downcase (car vars)) "=vs_base[" i "];"))
      ;; Assign optionals:
      (do ((i nreq (1+ i))
	   (vars optionals (cdr vars)))
	  ((null vars))
	(declare (fixnum i))
	(wt-nl (car vars) "=(narg>" i ") ? vs_base[" i "] : Cnil;"))
      (when (and (> nopt 0) (not key-flag) (null rest))
	(wt-nl "if (narg > " (+ nreq nopt) ") FEtoo_many_arguments(fname,narg);"))
      (when rest
         (wt-nl "{cl_object *p=vs_top;")
         (wt-nl " for(;p>vs_base+" (+ nreq nopt) ";p--)"
		rest "=CONS(p[-1]," rest ");}"))
      (when key-flag
	(cmperr "Keywords not allowed in defunC"))
      #+nil
      (when key-flag
	(wt-nl "parse_key(vs_base+" (+ nreq nopt) ",FALSE,"
	       (if allow-other-keys "TRUE," "FALSE,") (length keywords))
	(dolist (k keywords)
	  (wt-nl "," (add-object (car k))))
	(wt ");")
	(do ((ks keywords (cdr ks))
	     (i (+ nreq nopt) (1+ i)))
	    ((null ks))
	  (declare (fixnum i))
	  (wt-nl (string-downcase (second (car ks))) "=vs_base[" i "];")))
      ))
  ;; Now the supplied body:
  (dolist (s body)
    (wt-nl1 s))
  (wt-nl1 "}")
  )
|#

(defun t2function-constant (funob fun)
  (let ((previous (new-local *level* fun funob)))
    (if (and previous (fun-var previous))
	(setf (fun-var fun) (fun-var previous))
	(let ((loc (progn (wt-data nil) `(VV ,(incf *next-vv*)))))
	  (wt-nl loc " = ") (wt-make-closure fun) (wt ";")
	  (setf (fun-var fun) loc))))
)

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(setf (get 'COMPILER-LET 'T1) #'t1compiler-let)
(setf (get 'EVAL-WHEN 'T1) #'t1eval-when)
(setf (get 'PROGN 'T1) #'t1progn)
(setf (get 'DEFUN 'T1) #'t1defun)
(setf (get 'DEFMACRO 'T1) #'t1defmacro)
(setf (get 'DEFVAR 'T1) #'t1defvar)
(setf (get 'MACROLET 'T1) #'t1macrolet)
(setf (get 'LOCALLY 'T1) #'t1locally)
(setf (get 'SYMBOL-MACROLET 'T1) #'t1symbol-macrolet)
(setf (get 'CLINES 'T1) 't1clines)
(setf (get 'DEFCFUN 'T1) 't1defcfun)
;(setf (get 'DEFENTRY 'T1) 't1defentry)
(setf (get 'DEFLA 'T1) 't1defla)
(setf (get 'DEFCBODY 'T1) 't1defCbody)	; Beppe
;(setf (get 'DEFUNC 'T1) 't1defunC)	; Beppe
(setf (get 'LOAD-TIME-VALUE 'C1) 'c1load-time-value)

;;; Pass 2 initializers.

(setf (get 'DECL-BODY 't2) #'t2decl-body)
(setf (get 'PROGN 'T2) #'t2progn)
(setf (get 'DEFUN 'T2) #'t2defun)
(setf (get 'DEFMACRO 'T2) #'t2defmacro)
(setf (get 'ORDINARY 'T2) #'t2ordinary)
(setf (get 'DECLARE 'T2) #'t2declare)
(setf (get 'DEFVAR 'T2) #'t2defvar)
;(setf (get 'DEFENTRY 'T2) 't2defentry)
(setf (get 'DEFCBODY 'T2) 't2defCbody)	; Beppe
;(setf (get 'DEFUNC 'T2)	't2defunC); Beppe
(setf (get 'FUNCTION-CONSTANT 'T2) 't2function-constant); Beppe
(setf (get 'LOAD-TIME-VALUE 'T2) 't2load-time-value)

;;; Pass 2 C function generators.

(setf (get 'DECL-BODY 't3) #'t3decl-body)
(setf (get 'PROGN 'T3) #'t3progn)
(setf (get 'DEFUN 'T3) #'t3defun)
(setf (get 'DEFMACRO 'T3) #'t3defmacro)
(setf (get 'CLINES 'T3) 't3clines)
(setf (get 'DEFCFUN 'T3) 't3defcfun)
;(setf (get 'DEFENTRY 'T3) 't3defentry)
(setf (get 'DEFCBODY 'T3) 't3defCbody)	; Beppe
;(setf (get 'DEFUNC 'T3) 't3defunC)	; Beppe

;;; Package operations.

(setf (get 'si::select-package 'PACKAGE-OPERATION) t)
(setf (get 'si::%defpackage 'PACKAGE-OPERATION) t)

