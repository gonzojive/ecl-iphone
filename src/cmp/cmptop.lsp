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
	(*tags* nil))
    (push (t1expr* form) *top-level-forms*)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (form &aux (*current-form* form) (*first-error* t)
		    (*setjmps* 0))
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd)
	(when (and *compile-print*
		   (member fun *toplevel-forms-to-print*))
	  (print-current-form))
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

#+nil
(defun emit-local-funs ()
  ;; Local functions and closure functions
  (do ()
      ((eq *local-funs* *emitted-local-funs*))
    (let ((to-be-emitted (ldiff *local-funs* *emitted-local-funs*)))
      (setf *emitted-local-funs* *local-funs*)
      (mapc #'t3local-fun (nreverse to-be-emitted)))))

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
  (when si::*compiler-constants*
    (wt-h "#include <string.h>"))
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
    (wt-nl1 "void " (init-function-name name) "(cl_object flag)")
    (wt-nl1 "{ VT" *reservation-cmacro* " CLSR" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (when shared-data
      (wt-nl "Cblock=flag;")
      (wt-nl "VV = flag->cblock.data;"))
    (unless shared-data
      (wt-nl "if (!FIXNUMP(flag)){")
      (wt-nl "Cblock=flag;")
      (wt-nl "#ifndef ECL_DYNAMIC_VV")
      (wt-nl "flag->cblock.data = VV;")
      (wt-nl "#endif")
      (wt-nl "flag->cblock.self_destruct="
	     (if *self-destructing-fasl* "1;" "0;"))
      (wt-nl "flag->cblock.data_size = VM;")
      (wt-nl "flag->cblock.data_text = compiler_data_text;")
      (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
      (wt-nl "return;}")
      (wt-nl "#ifdef ECL_DYNAMIC_VV")
      (wt-nl "VV = Cblock->cblock.data;")
      (wt-nl "#endif"))
    (when si::*compiler-constants*
      (wt-nl "{cl_object data = symbol_value("
	     (nth-value 1 (si::mangle-name '*compiler-constants* nil))
	     ");")
      (wt-nl "memcpy(VV, data->vector.self.t, VM*sizeof(cl_object));}"))
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
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
	(progn
	  (wt-h "#undef ECL_DYNAMIC_VV")
	  (wt-h "#define compiler_data_text \"\"")
	  (wt-h "#define compiler_data_text_size 0")
	  (wt-h "#define VM " num-objects 0)
	  (wt-h "#define VV NULL"))
	(progn
	  (wt-h "#define VM " num-objects)
	  (wt-h "#ifdef ECL_DYNAMIC_VV")
	  (wt-h "static cl_object *VV;")
	  (wt-h "#else")
	  (wt-h "static cl_object VV[VM];")
	  (wt-h "#endif"))))
  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-h "static cl_object " c-name "(cl_narg, ...);")
      (wt-h "static cl_object (*" var-name ")(cl_narg, ...)=" c-name ";")))

  ;;; Global entries for directly called functions.
  (dolist (x *global-entries*)
    (apply 'wt-global-entry x))

  ;;; Initial functions for linking calls.
  (dolist (l *linking-calls*)
    (let* ((var-name (fifth l))
	   (c-name (fourth l))
	   (lisp-name (third l)))
      (wt-nl1 "static cl_object " c-name "(cl_narg narg, ...)"
	      "{TRAMPOLINK(narg," lisp-name ",&" var-name ",Cblock);}")))

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
  (let (cname)
    (if (and (symbolp name) (setf cname (get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

(defun t1defun (args)
  (check-args-number 'DEFUN args 2)
  (when *compile-time-too* (cmp-eval (cons 'DEFUN args)))
  (let* ((fname (first args))
	 (lambda-list-and-body (rest args))
	 (fun (c1compile-function lambda-list-and-body :name fname :global t))
	 (no-entry nil)
	 (doc nil))
    (multiple-value-bind (decl body doc)
	(si::process-declarations (rest lambda-list-and-body) nil)
      (cond ((and *allow-c-local-declaration* (assoc 'si::c-local decl))
	     (setq no-entry t))
	    #+ecl-min
	    ((member fname c::*in-all-symbols-functions*)
	     (setq no-entry t))
	    ((setq doc (si::expand-set-documentation fname 'function doc))
	     (t1expr `(progn ,@doc)))))
    (add-load-time-values)
    (setq output (new-defun fun no-entry))
    output))

;;; Mechanism for sharing code:
;;; FIXME! Revise this 'DEFUN stuff.
(defun new-defun (new &optional no-entry)
  (unless (fun-exported new)
    ;; Check whether this function is similar to a previous one and
    ;; share code with it.
    (dolist (old *global-funs*)
      (when (similar (fun-lambda new) (fun-lambda old))
	(cmpnote "Sharing code among functions ~A and ~A"
		 (fun-name new) (fun-name old))
	(setf (fun-cfun new) (fun-cfun old)
	      (fun-lambda new) nil
	      (fun-minarg new) (fun-minarg old)
	      (fun-maxarg new) (fun-maxarg old))
	(return))))
  (push new *global-funs*)
  (make-c1form* 'DEFUN :args new no-entry))

(defun print-function (x)
  (format t "~%<a FUN: ~A, CLOSURE: ~A, LEVEL: ~A, ENV: ~A>"
	  (fun-name x) (fun-closure x) (fun-level x) (fun-env x)))

(defun similar (x y)
  ;; FIXME! This could be more accurate
  (labels ((similar-ref (x y)
	     (and (equal (ref-ref-ccb x) (ref-ref-ccb y))
		  (equal (ref-ref-clb x) (ref-ref-clb y))
		  (equal (ref-ref x) (ref-ref y))))
	   (similar-var (x y)
	     (and (similar-ref x y)
		  (equal (var-name x) (var-name y))
		  (equal (var-kind x) (var-kind y))
		  (equal (var-loc x) (var-loc y))
		  (equal (var-type x) (var-type y))
		  (equal (var-index x) (var-index y))))
	   (similar-c1form (x y)
	     (and (equal (c1form-name x) (c1form-name y))
		  (similar (c1form-args x) (c1form-args y))
		  (similar (c1form-local-vars x) (c1form-local-vars y))
		  (eql (c1form-sp-change x) (c1form-sp-change y))
		  (eql (c1form-volatile x) (c1form-volatile y))))
	   (similar-fun (x y)
	     (and (similar-ref x y)
		  (eql (fun-global x) (fun-global y))
		  (eql (fun-exported x) (fun-exported y))
		  (eql (fun-closure x) (fun-closure y))
		  (similar (fun-var x) (fun-var y))
		  (similar (fun-lambda x) (fun-lambda y))
		  (= (fun-level x) (fun-level y))
		  (= (fun-env x) (fun-env y))
		  (= (fun-minarg x) (fun-minarg y))
		  (eql (fun-maxarg x) (fun-maxarg y))
		  (similar (fun-local-vars x) (fun-local-vars y))
		  (similar (fun-referred-vars x) (fun-referred-vars y))
		  (similar (fun-referred-funs x) (fun-referred-funs y))
		  (similar (fun-child-funs x) (fun-child-funs y)))))
    (and (eql (type-of x) (type-of y))
	 (typecase x
	   (CONS (and (similar (car x) (car y))
		      (similar (cdr x) (cdr y))))
	   (VAR (similar-var x y))
	   (FUN (similar-fun x y))
	   (REF (similar-ref x y))
	   (TAG NIL)
	   (BLK NIL)
	   (C1FORM (similar-c1form x y))
	   (SEQUENCE (and (every #'similar x y)))
	   (T (equal x y))))))

(defun t2defun (fun no-entry)
  (declare (ignore sp funarg-vars))
  ;; If the function is not shared, emit it.
  (when (fun-lambda fun)
    (push fun *local-funs*))
  (unless no-entry
    (let* ((fname (fun-name fun))
	   (vv (add-object fname))
	   (cfun (fun-cfun fun))
	   (minarg (fun-minarg fun))
	   (maxarg (fun-maxarg fun))
	   (narg (if (= minarg maxarg) maxarg nil)))
      (if narg
	(wt-nl "cl_def_c_function(" vv ",(void*)" cfun "," narg ");")
	(wt-nl "cl_def_c_function_va(" vv ",(void*)" cfun ");")))))

(defun wt-function-prolog (&optional sp local-entry)
  (wt " VT" *reservation-cmacro*
      " VLEX" *reservation-cmacro*
      " CLSR" *reservation-cmacro*)
  (wt-nl "cl_object value0;")
  (when sp (wt-nl "bds_check;"))
  ; (when (compiler-push-events) (wt-nl "ihs_check;"))
  )

(defun wt-function-epilogue (&optional closure-type)
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
    (unless (eq closure-type 'CLOSURE)
      (wt-h1 " volatile cl_object env0;"))
    (wt-h1 " cl_object ")
    (dotimes (i *max-env*)
      (wt-h1 "CLV") (wt-h1 i)
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
    (c1declare-specials ss)
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
    (cmpck (not (stringp s)) "The argument to CLINES, ~s, is not a string." s))
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

(defun t3local-fun (fun &aux  (lambda-expr (fun-lambda fun))
			      (level (if (eq (fun-closure fun) 'LEXICAL)
					 (fun-level fun)
					 0))
			      (cfun (fun-cfun fun))
		    	      (minarg (fun-minarg fun))
		    	      (maxarg (fun-maxarg fun))
		    	      (narg (fun-needs-narg fun))
			      (nenvs level)
			      (*volatile* (c1form-volatile* lambda-expr))
			      (*tail-recursion-info* fun)
                              (lambda-list (c1form-arg 0 lambda-expr))
                              (requireds (car lambda-list)))
  (declare (fixnum level nenvs))
  (when *compile-print* (print-emitting fun))
  (wt-comment (cond ((fun-global fun) "function definition for ")
		    ((eq (fun-closure fun) 'CLOSURE) "closure ")
		    (t "local function "))
	      (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (cond ((fun-exported fun)
	 (wt-h #+(and msvc (not ecl-min)) "__declspec(dllexport) " "cl_object " cfun "(")
	 (wt-nl1 "cl_object " cfun "("))
	(t
	 (wt-h "static cl_object " cfun "(")
	 (wt-nl1 "static cl_object " cfun "(")))
  (let ((comma ""))
    (when narg
      (wt-h1 "cl_narg")
      (wt "cl_narg narg")
      (setf comma ", "))
    (dotimes (n level)
      (wt-h1 comma) (wt-h1 "cl_object *")
      (wt comma "cl_object *lex" n)
      (setf comma ", "))
    (when (eq (fun-closure fun) 'CLOSURE)
      (wt-h1 comma) (wt-h1 "cl_object ")
      (wt comma "cl_object env0")
      (setf comma ", "))
    (let ((lcl 0))
      (declare (fixnum lcl))
      (dolist (var requireds)
	(wt-h1 comma) (wt-h1 "cl_object ")
	(wt comma "cl_object ") (wt-lcl (incf lcl))
	(setf comma ", ")))
    (when narg
      (wt-h1 ", ...")
      (wt ", ..."))
    (wt-h1 ");")
    (wt ")"))

  (let* ((*lcl* 0) (*temp* 0) (*max-temp* 0)
	 (*lex* 0) (*max-lex* 0)
	 (*env* (fun-env fun))		; continue growing env
	 (*max-env* *env*) (*env-lvl* 0)
	 (*level* level)
	 (*exit* 'RETURN) (*unwind-exit* '(RETURN))
	 (*destination* 'RETURN)
	 (*reservation-cmacro* (next-cmacro))
	 (*inline-blocks* 1))
    (wt-nl1 "{")
    (wt " VT" *reservation-cmacro*
	" VLEX" *reservation-cmacro*
	" CLSR" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (when (eq (fun-closure fun) 'CLOSURE)
      (let ((clv-used (remove-if
		       #'(lambda (x)
			   (or
			    ;; non closure variable
			    (not (ref-ref-ccb x))
			    ;; special variable
			    (eq (var-kind x) 'special)
			    ;; not actually referenced
			    (and (not (var-referenced-in-form x (fun-lambda fun)))
				 (not (var-changed-in-form x (fun-lambda fun))))
			    ;; parameter of this closure
			    ;; (not yet bound, therefore var-loc is OBJECT)
			    (eq (var-loc x) 'OBJECT)))
		       (fun-referred-vars fun)))
	    l)
	(when clv-used
	  (setf clv-used (sort clv-used #'> :key #'var-loc))
		l (var-loc (first clv-used)))
	  (wt-nl "/* Scanning closure data ... */")
	  (do ((n (1- (fun-env fun)) (1- n))
	       (bs clv-used)
	       (first t))
	      ((or (minusp n) (null bs)))
	    (wt-nl "CLV" n)
	    (if first
		(progn (wt "=env0;") (setf first nil))
		(wt "=CDR(CLV" (1+ n) ");"))
	    (when (= n (var-loc (first bs)))
	      (wt-comment (var-name (first clv-used)))
              (pop clv-used)))
	  (wt-nl "{ /* ... closure scanning finished */")
	  (incf *inline-blocks*)))
    (c2lambda-expr (c1form-arg 0 lambda-expr)
		   (c1form-arg 2 lambda-expr)
		   (fun-cfun fun) (fun-name fun)
		   narg
		   (fun-closure fun))
    (wt-nl1)
    (close-inline-blocks)
    (wt-function-epilogue (fun-closure fun)))	; we should declare in CLSR only those used
  )

;;; ----------------------------------------------------------------------
;;; Optimizer for FSET. Should remove the need for a special handling of
;;; DEFUN as a toplevel form.
;;;
(defun c1fset (args)
  ;; When the function or macro to be defined is not a closure, we can use the
  ;; auxiliary C functions c_def_c_*() instead of creating a closure and
  ;; invoking si_fset(). However until the C2 phase of the compiler we do not
  ;; know whether a function is a closure, hence the need for a c2fset.
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((args (mapcar #'c1expr args))
	   (fun (second args)))
      (if (and (eq (c1form-name fun) 'FUNCTION)
	       (not (eq (c1form-arg 0 fun) 'GLOBAL))
	       (typep macro 'boolean)
	       (typep pprint '(or integer null)))
	  (make-c1form* 'SI:FSET :args
			(c1form-arg 2 fun) ;; Function object
			(c1expr fname)
			macro
			pprint
			args) ;; The c1form, when we do not optimize
	  (c1call-global 'SI:FSET (list fname def macro pprint))))))

(defun c2fset (fun fname macro pprint c1forms)
  (unless (and (not (fun-closure fun))
	       (eq *destination* 'TRASH))
    (return-from c2fset
      (c2call-global 'SI:FSET c1forms 'NIL
		     (c1form-primary-type (second c1forms)))))
  (let* ((*inline-blocks* 0)
	 (fname (first (coerce-locs (inline-args (list fname)))))
	 (cfun (fun-cfun fun))
	 (minarg (fun-minarg fun))
	 (maxarg (fun-maxarg fun))
	 (narg (if (= minarg maxarg) maxarg nil)))
    ;; FIXME! Look at c2function!
    (new-local fun)
    (if macro
	(if narg
	    (wt-nl "cl_def_c_macro(" fname ",(void*)" cfun "," narg ");")
	    (wt-nl "cl_def_c_macro(" fname ",(void*)" cfun ",-1);"))
	(if narg
	    (wt-nl "cl_def_c_function(" fname ",(void*)" cfun "," narg ");")
	    (wt-nl "cl_def_c_function_va(" fname ",(void*)" cfun ");")))
    (close-inline-blocks)))

;;; ----------------------------------------------------------------------

;;; Pass 1 top-levels.

(put-sysprop 'COMPILER-LET 'T1 #'t1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 #'t1eval-when)
(put-sysprop 'PROGN 'T1 #'t1progn)
(put-sysprop 'DEFUN 'T1 #'t1defun)
(put-sysprop 'MACROLET 'T1 #'t1macrolet)
(put-sysprop 'LOCALLY 'T1 #'t1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 #'t1symbol-macrolet)
(put-sysprop 'CLINES 'T1 't1clines)
(put-sysprop 'LOAD-TIME-VALUE 'C1 'c1load-time-value)
(put-sysprop 'SI:FSET 'C1 'c1fset)

;;; Pass 2 initializers.

(put-sysprop 'DECL-BODY 't2 #'t2decl-body)
(put-sysprop 'PROGN 'T2 #'t2progn)
(put-sysprop 'DEFUN 'T2 #'t2defun)
(put-sysprop 'ORDINARY 'T2 #'t2ordinary)
(put-sysprop 'LOAD-TIME-VALUE 'T2 't2load-time-value)
(put-sysprop 'SI:FSET 'C2 'c2fset)

;;; Pass 2 C function generators.

(put-sysprop 'DECL-BODY 't3 #'t3decl-body)
(put-sysprop 'PROGN 'T3 #'t3progn)
(put-sysprop 'CLINES 'T3 't3clines)
