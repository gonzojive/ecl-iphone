;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
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
  (let ((*cmp-env* (cmp-env-new)))
    (push (t1expr* form) *top-level-forms*)))

(defvar *toplevel-forms-to-print*
  '(defun defmacro defvar defparameter defclass defmethod defgeneric))

(defun t1expr* (form &aux (*current-form* form) (*first-error* t)
		    (*setjmps* 0))
  ;(let ((*print-level* 3)) (print form))
  (catch *cmperr-tag*
    (when (consp form)
      (let ((fun (car form)) (args (cdr form)) fd)
	(when (member fun *toplevel-forms-to-print*)
	  (print-current-form))
	(cond
            ((consp fun) (t1ordinary form))
            ((not (symbolp fun))
	     (cmperr "~s is illegal function." fun))
	    ((eq fun 'QUOTE)
	     (t1ordinary 'NIL))
	    ((setq fd (get-sysprop fun 'T1))
	     (funcall fd args))
	    ((or (get-sysprop fun 'C1) (get-sysprop fun 'C1SPECIAL))
             (t1ordinary form))
	    ((and (setq fd (compiler-macro-function fun))
		  (inline-possible fun)
		  (let ((success nil))
		    (multiple-value-setq (fd success)
		      (cmp-expand-macro fd form))
		    success))
	     (t1expr* fd))
	    ((setq fd (cmp-macro-function fun))
	     (t1expr* (cmp-expand-macro fd form)))
	    (t (t1ordinary form))
	   )))))

(defun t1/c1expr (form)
  (cond ((not *compile-toplevel*)
	 (c1expr form))
	((atom form)
	 (t1ordinary form))
	(t
	 (t1expr* form))))	

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
  (do ((*compile-time-too* nil)
       (*compile-toplevel* nil))
      ;; repeat until t3local-fun generates no more
      ((eq *emitted-local-funs* *local-funs*))
    ;; scan *local-funs* backwards
    (do ((lfs *local-funs* (cdr lfs)))
	((eq (cdr lfs) *emitted-local-funs*)
	 (setq *emitted-local-funs* lfs)
	 (locally (declare (notinline t3local-fun))
	   ;; so disassemble can redefine it
	   (t3local-fun (first lfs)))))))

(defun ctop-write (name h-pathname data-pathname
		        &key shared-data
			&aux def top-output-string
			(*volatile* " volatile "))

  ;(let ((*print-level* 3)) (pprint *top-level-forms*))
  (setq *top-level-forms* (nreverse *top-level-forms*))
  (wt-nl1 "#include \"" (si::coerce-to-filename h-pathname) "\"")
  ;; All lines from CLINES statements are grouped at the beginning of the header
  ;; Notice that it does not make sense to guarantee that c-lines statements
  ;; are produced in-between the function definitions, because two functions
  ;; might be collapsed into one, or we might not produce that function at all
  ;; and rather inline it.
  (do ()
      ((null *clines-string-list*))
    (wt-nl-h (pop *clines-string-list*)))
  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "extern \"C\" {")
  (wt-nl-h "#endif")
  (when si::*compiler-constants*
    (wt-nl-h "#include <string.h>"))
  ;;; Initialization function.
  (let* ((*lcl* 0) (*lex* 0) (*max-lex* 0) (*max-env* 0) (*max-temp* 0)
	 (*max-stack* 0)
	 (*reservation-cmacro* (next-cmacro))
	 (c-output-file *compiler-output1*)
	 (*compiler-output1* (make-string-output-stream))
	 (*emitted-local-funs* nil)
	 (*compiler-declared-globals* (make-hash-table)))
    (unless shared-data
      (wt-nl1 "#include \"" (si::coerce-to-filename data-pathname) "\""))
    (wt-nl1 "#ifdef __cplusplus")
    (wt-nl1 "extern \"C\"")
    (wt-nl1 "#endif")
    (wt-nl1 "ECL_DLLEXPORT void " name "(cl_object flag)")
    (wt-nl1 "{ VT" *reservation-cmacro*
	    " VLEX" *reservation-cmacro*
            " CLSR" *reservation-cmacro*
	    " STCK" *reservation-cmacro*)
    (wt-nl "cl_object value0;")
    (wt-nl "cl_object *VVtemp;")
    (when shared-data
      (wt-nl "Cblock=flag;")
      (wt-nl "VV = flag->cblock.data;"))
    (unless shared-data
      (wt-nl "if (!FIXNUMP(flag)){")
      (wt-nl "Cblock=flag;")
      (wt-nl "#ifndef ECL_DYNAMIC_VV")
      (wt-nl "flag->cblock.data = VV;")
      (wt-nl "#endif")
      (when *self-destructing-fasl*
	(wt-nl "flag->cblock.self_destruct=1;"))
      (wt-nl "flag->cblock.data_size = VM;")
      (wt-nl "flag->cblock.temp_data_size = VMtemp;")
      (wt-nl "flag->cblock.data_text = compiler_data_text;")
      (wt-nl "flag->cblock.data_text_size = compiler_data_text_size;")
      (wt-nl "return;}")
      (wt-nl "#ifdef ECL_DYNAMIC_VV")
      (wt-nl "VV = Cblock->cblock.data;")
      (wt-nl "#endif")
      ;; With this we ensure creating a constant with the tag
      ;; and the initialization file
      (wt-nl "Cblock->cblock.data_text = \"" (init-name-tag name) "\";")
      )
    (when si::*compiler-constants*
      (wt-nl "{cl_object data = ecl_symbol_value("
	     (nth-value 1 (si::mangle-name '*compiler-constants* nil))
	     ");")
      (wt-nl "memcpy(VV, data->vector.self.t, VM*sizeof(cl_object));}"))
    (wt-nl "VVtemp = Cblock->cblock.temp_data;")

    (setq *compiler-phase* 't2)

    ;; useless in initialization.
    (dolist (form (nconc (nreverse *make-forms*) *top-level-forms*))
      (let ((*compile-to-linking-call* nil)
	    (*env* 0) (*level* 0) (*temp* 0))
	  (t2expr form))
      (let ((*compiler-output1* c-output-file))
	(emit-local-funs)))
    (wt-function-epilogue)
    (wt-nl1 "}")
    (setq top-output-string (get-output-stream-string *compiler-output1*)))

  ;; Declarations in h-file.
  (wt-nl-h "static cl_object Cblock;")
  (dolist (x *reservations*)
    (wt-nl-h "#define VM" (car x) " " (cdr x)))
  (let ((num-objects (data-size)))
    (if (zerop num-objects)
	(progn
	  (wt-nl-h "#undef ECL_DYNAMIC_VV")
	  (wt-nl-h "#define compiler_data_text \"\"")
	  (wt-nl-h "#define compiler_data_text_size 0")
	  (wt-nl-h "#define VM 0")
	  (wt-nl-h "#define VMtemp 0")
	  (wt-nl-h "#define VV NULL"))
	(progn
	  (wt-nl-h "#define VM " (data-permanent-storage-size))
	  (wt-nl-h "#define VMtemp "  (data-temporary-storage-size))
	  (wt-nl-h "#ifdef ECL_DYNAMIC_VV")
	  (wt-nl-h "static cl_object *VV;")
	  (wt-nl-h "#else")
	  (wt-nl-h "static cl_object VV[VM];")
	  (wt-nl-h "#endif"))))
  (dolist (l *linking-calls*)
    (let* ((c-name (fourth l))
	   (var-name (fifth l)))
      (wt-nl-h "static cl_object " c-name "(cl_narg, ...);")
      (wt-nl-h "static cl_object (*" var-name ")(cl_narg, ...)=" c-name ";")))

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

  (setq *compiler-phase* 't3)

  ;;; Callbacks
  (when *callbacks*
    (wt-nl-h "#include <ecl/internal.h>")
    (dolist (x *callbacks*)
      (apply #'t3-defcallback x)))

  (wt-nl-h "#ifdef __cplusplus")
  (wt-nl-h "}")
  (wt-nl-h "#endif")


  (wt-nl top-output-string))

(defun c1eval-when (args)
  (check-args-number 'EVAL-WHEN args 1)
  (let ((load-flag nil)
	(compile-flag nil)
	(execute-flag nil))
    (dolist (situation (car args))
      (case situation
	((LOAD :LOAD-TOPLEVEL) (setq load-flag t))
	((COMPILE :COMPILE-TOPLEVEL) (setq compile-flag t))
	((EVAL :EXECUTE)
	 (if *compile-toplevel*
	     (setq compile-flag (or *compile-time-too* compile-flag))
	     (setq execute-flag t)))
	(otherwise (cmperr "The EVAL-WHEN situation ~s is illegal."
			   situation))))
    (cond ((not *compile-toplevel*)
	   (c1progn (and execute-flag (rest args))))
	  (load-flag
	   (let ((*compile-time-too* compile-flag))
	     (c1progn (rest args))))
	  (compile-flag
	   (cmp-eval (cons 'PROGN (rest args)))
	   (c1progn 'NIL))
	  (t
	   (c1progn 'NIL)))))

(defun t2compiler-let (symbols values body)
  (progv symbols values (c2expr body)))

(defun t2progn (args)
  (mapcar #'t2expr args))

(defun exported-fname (name)
  (let (cname)
    (if (and (symbolp name) (setf cname (get-sysprop name 'Lfun)))
        (values cname t)
        (values (next-cfun "L~D~A" name) nil))))

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
	(setf (fun-shares-with new) old
	      (fun-cfun new) (fun-cfun old)
	      (fun-minarg new) (fun-minarg old)
	      (fun-maxarg new) (fun-maxarg old))
	(return))))
  (push new *global-funs*))

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

(defun wt-function-prolog (&optional sp local-entry)
  (wt " VT" *reservation-cmacro*
      " VLEX" *reservation-cmacro*
      " CLSR" *reservation-cmacro*
      " STCK" *reservation-cmacro*)
  (wt-nl "cl_object value0;")
  (when sp (wt-nl "bds_check;"))
  ; (when (compiler-push-events) (wt-nl "ihs_check;"))
  )

(defun wt-function-epilogue (&optional closure-type)
  (push (cons *reservation-cmacro* *max-temp*) *reservations*)
  ;; FIXME! Are we careful enough with temporary variables that
  ;; we need not make them volatile?
  (wt-nl-h "#define VT" *reservation-cmacro*)
  (when (plusp *max-temp*)
    (wt-h " cl_object ")
    (dotimes (i *max-temp*)
      (wt-h "T" i)
      (unless (= (1+ i) *max-temp*) (wt-h ",")))
    (wt-h ";"))
;  (wt-nl-h "#define VU" *reservation-cmacro*)
  (wt-nl-h "#define VLEX" *reservation-cmacro*)
  ;; There should be no need to mark lex as volatile, since we
  ;; are going to pass pointers of this array around and the compiler
  ;; should definitely keep this in memory.
  (when (plusp *max-lex*)
    (wt-h " volatile cl_object lex" *level* "[" *max-lex* "];"))
  (wt-nl-h "#define CLSR" *reservation-cmacro*)
  (wt-nl-h "#define STCK" *reservation-cmacro*)
  (unless (zerop *max-stack*)
    (wt-h " cl_object " +ecl-local-stack-variable+ "[" *max-stack* "]; "
	  "struct ecl_stack_frame " +ecl-local-stack-frame-variable+
	  " = { t_frame, 0, 0, 0, " +ecl-local-stack-variable+ ", 0, 0 };"))
  (when (plusp *max-env*)
    (unless (eq closure-type 'CLOSURE)
      (wt-h " cl_object " *volatile* "env0;"))
    (wt-h " cl_object " *volatile*)
    (dotimes (i *max-env*)
      (wt-h "CLV" i)
      (unless (= (1+ i) *max-env*) (wt-h ",")))
    (wt-h ";"))
  )

(defun wt-global-entry (fname cfun arg-types return-type)
    (when (and (symbolp fname) (get-sysprop fname 'NO-GLOBAL-ENTRY))
      (return-from wt-global-entry nil))
    (wt-comment "global entry for the function " fname)
    (wt-nl1 "static cl_object L" cfun "(cl_narg narg")
    (wt-nl-h "static cl_object L" cfun "(cl_narg")
    (do ((vl arg-types (cdr vl))
	 (lcl (1+ *lcl*) (1+ lcl)))
	((endp vl) (wt1 ")"))
      (declare (fixnum lcl))
      (wt1 ", cl_object ") (wt-lcl lcl)
      (wt-h ", cl_object"))
    (wt-h1 ");")
    (wt-nl1 "{")
    (when (compiler-check-args)
      (wt-nl "check_arg(" (length arg-types) ");"))
    (wt-nl "NVALUES=1;")
    (wt-nl "return " (case return-type
                            (FIXNUM "MAKE_FIXNUM")
                            (CHARACTER "CODE_CHAR")
                            (DOUBLE-FLOAT "ecl_make_doublefloat")
                            (SINGLE-FLOAT "ecl_make_singlefloat")
			    #+long-float
                            (LONG-FLOAT "make_longfloat")
                            (otherwise ""))
           "(LI" cfun "(")
    (do ((types arg-types (cdr types))
         (n 1 (1+ n)))
        ((endp types))
      (declare (fixnum n))
      (wt (case (car types)
            (FIXNUM "fix")
            (CHARACTER "ecl_char_code")
            (DOUBLE-FLOAT "df")
            (SINGLE-FLOAT "sf")
	    #+long-float
	    (LONG-FLOAT "ecl_long_float")
            (otherwise "")) "(")
        (wt-lcl n) (wt ")")
        (unless (endp (cdr types)) (wt ",")))
    (wt "));}")
    )

(defun rep-type (type)
  (case type
    (FIXNUM "cl_fixnum ")
    (CHARACTER "unsigned char ")
    (SINGLE-FLOAT "float ")
    (DOUBLE-FLOAT "double ")
    (otherwise "cl_object ")))

(defun t1ordinary (form)
  (when *compile-time-too* (cmp-eval form))
  (let ((*compile-toplevel* nil)
	(*compile-time-too* nil))
    (add-load-time-values (make-c1form* 'ORDINARY :args (c1expr form)))))

(defun t2ordinary (form)
  (let* ((*exit* (next-label))
	 (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun add-load-time-values (form)
  (let ((previous (append (and (consp *load-time-values*)
			       (nreverse *load-time-values*))
			  (nreverse *make-forms*))))
    (when previous
      (setf *load-time-values* nil
	    *make-forms* nil)
      (setf form (make-c1form* 'PROGN :args (nconc previous (list form))))))
  form)

(defun c1load-time-value (args)
  (check-args-number 'LOAD-TIME-VALUE args 1 2)
  (let ((form (first args))
	loc)
    (cond ((not (listp *load-time-values*))
	   ;; When using COMPILE, we set *load-time-values* to 'VALUES and
	   ;; thus signal that we do not want to compile these forms, but
	   ;; just to retain their value.
	   (return-from c1load-time-value (c1constant-value (cmp-eval form) :always t)))
          ((typep form '(or list symbol))
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

(defun t2make-form (vv-loc form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* vv-loc))
    (c2expr form)
    (wt-label *exit*)))

(defun t2init-form (vv-loc form)
  (let* ((*exit* (next-label)) (*unwind-exit* (list *exit*))
         (*destination* 'TRASH))
    (c2expr form)
    (wt-label *exit*)))

(defun t2decl-body (decls body)
  (let ((*cmp-env* *cmp-env*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (t2expr body)))

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
  (print-emitting fun)
  (wt-comment (cond ((fun-global fun) "function definition for ")
		    ((eq (fun-closure fun) 'CLOSURE) "closure ")
		    (t "local function "))
	      (or (fun-name fun) (fun-description fun) 'CLOSURE))
  (when (fun-shares-with fun)
    (wt-comment "... shares definition with " (fun-name (fun-shares-with fun)))
    (return-from t3local-fun))
  (cond ((fun-exported fun)
	 (wt-nl-h "ECL_DLLEXPORT cl_object " cfun "(")
	 (wt-nl1 "cl_object " cfun "("))
	(t
	 (wt-nl-h "static cl_object " cfun "(")
	 (wt-nl1 "static cl_object " cfun "(")))
  (let ((comma ""))
    (when narg
      (wt-h *volatile* "cl_narg")
      (wt *volatile* "cl_narg narg")
      (setf comma ", "))
    (dotimes (n level)
      (wt-h comma "volatile cl_object  *")
      (wt comma "volatile cl_object *lex" n)
      (setf comma ", "))
    (when (eq (fun-closure fun) 'CLOSURE)
      (wt-h comma "cl_object " *volatile*)
      (wt comma "cl_object " *volatile* "env0")
      (setf comma ", "))
    (let ((lcl 0))
      (declare (fixnum lcl))
      (dolist (var requireds)
	(wt-h comma "cl_object " *volatile*)
	(wt comma "cl_object " *volatile*) (wt-lcl (incf lcl))
	(setf comma ", ")))
    (when narg
      (wt-h ", ...")
      (wt ", ..."))
    (wt-h ");")
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
	" CLSR" *reservation-cmacro*
	" STCK" *reservation-cmacro*)
    (wt-nl *volatile* "cl_object value0;")
    (when (>= (fun-debug fun) 2)
      (wt-nl "struct ihs_frame ihs;"))
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
    ;; If we ask for high level of debugging information, we push the function
    ;; name into the invocation stack
    (when (>= (fun-debug fun) 2)
      (push 'IHS *unwind-exit*)
      (wt-nl "ihs_push(&ihs," (add-symbol (fun-name fun)) ",Cnil);"))

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
;;; Optimizer for FSET. Removes the need for a special handling of DEFUN as a
;;; toplevel form and also allows optimizing calls to DEFUN or DEFMACRO which
;;; are not toplevel, but which create no closures.
;;;
;;; The idea is as follows: when the function or macro to be defined is not a
;;; closure, we can use the auxiliary C functions c_def_c_*() instead of
;;; creating a closure and invoking si_fset(). However until the C2 phase of
;;; the compiler we do not know whether a function is a closure, hence the need
;;; for a c2fset.
;;;
(defun c1fset (args)
  (destructuring-bind (fname def &optional (macro nil) (pprint nil))
      args
    (let* ((fun-form (c1expr def)))
      (if (and (eq (c1form-name fun-form) 'FUNCTION)
	       (not (eq (c1form-arg 0 fun-form) 'GLOBAL)))
	  (let ((fun-object (c1form-arg 2 fun-form)))
	    (when (fun-no-entry fun-object)
	      (when macro
		(cmperr "Declaration C-LOCAL used in macro ~a" (fun-name fun)))
	      (return-from c1fset
		(make-c1form* 'SI:FSET :args fun-object nil nil nil nil)))
	    (when (and (typep macro 'boolean)
		       (typep pprint '(or integer null)))
	      (return-from c1fset
		(make-c1form* 'SI:FSET :args
			      fun-object ;; Function object
			      (c1expr fname)
			      macro
			      pprint
			      ;; The c1form, when we do not optimize
			      (list (c1expr fname)
				    fun-form
				    (c1expr macro)
				    (c1expr pprint))))))))
    (c1call-global 'SI:FSET (list fname def macro pprint))))

(defun c2fset (fun fname macro pprint c1forms)
  (when (fun-no-entry fun)
    (wt-nl "(void)0; /* No entry created for "
	   (format nil "~A" (fun-name fun))
	   " */")
    ;; FIXME! Look at c2function!
    (new-local fun)
    (return-from c2fset))
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

(put-sysprop 'COMPILER-LET 'T1 #'c1compiler-let)
(put-sysprop 'EVAL-WHEN 'T1 #'c1eval-when)
(put-sysprop 'PROGN 'T1 #'c1progn)
(put-sysprop 'MACROLET 'T1 #'c1macrolet)
(put-sysprop 'LOCALLY 'T1 #'c1locally)
(put-sysprop 'SYMBOL-MACROLET 'T1 #'c1symbol-macrolet)
(put-sysprop 'LOAD-TIME-VALUE 'C1 'c1load-time-value)
(put-sysprop 'SI:FSET 'C1 'c1fset)

;;; Pass 2 initializers.

(put-sysprop 'COMPILER-LET 'T2 #'t2compiler-let)
(put-sysprop 'DECL-BODY 't2 #'t2decl-body)
(put-sysprop 'PROGN 'T2 #'t2progn)
(put-sysprop 'ORDINARY 'T2 #'t2ordinary)
(put-sysprop 'LOAD-TIME-VALUE 'T2 't2load-time-value)
(put-sysprop 'MAKE-FORM 'T2 't2make-form)
(put-sysprop 'INIT-FORM 'T2 't2init-form)
(put-sysprop 'SI:FSET 'C2 'c2fset)
