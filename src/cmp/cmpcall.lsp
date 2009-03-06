;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
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

;;; Like macro-function except it searches the lexical environment,
;;; to determine if the macro is shadowed by a function or a macro.
(defun cmp-macro-function (name)
  (or (cmp-env-search-macro name)
      (macro-function name)))

(defun unoptimized-long-call (fun arguments)
  (let ((frame (gensym)))
    (c1expr `(with-stack ,frame
	       ,@(loop for i in arguments collect `(stack-push ,frame ,i))
	       (si::apply-from-stack-frame ,frame ,fun)))))

(defun unoptimized-funcall (fun arguments)
  (let ((l (length arguments)))
    (if (<= l si::c-arguments-limit)
	(make-c1form* 'FUNCALL :args (c1expr fun) (c1args* arguments))
	(unoptimized-long-call fun arguments))))

(defun c1funcall (args)
  (check-args-number 'FUNCALL args 1)
  (let ((fun (first args))
	(arguments (rest args))
	fd)
    (cond ;; (FUNCALL (LAMBDA ...) ...)
          ((and (consp fun)
		(eq (first fun) 'LAMBDA))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL (EXT::LAMBDA-BLOCK ...) ...)
          ((and (consp fun)
		(eq (first fun) 'EXT::LAMBDA-BLOCK))
	   (setf fun (macroexpand-1 fun))
	   (c1expr (optimize-funcall/apply-lambda (cdr fun) arguments nil)))
	  ;; (FUNCALL atomic-expression ...)
	  ((atom fun)
	   (unoptimized-funcall fun arguments))
	  ;; (FUNCALL macro-expression ...)
	  ((let ((name (first fun)))
	     (setq fd (and (symbolp name)
                           ;; We do not want to macroexpand 'THE
                           (not (eq name 'THE))
			   (cmp-macro-function name))))
	   (c1funcall (list* (cmp-expand-macro fd fun) arguments)))
	  ;; (FUNCALL lisp-expression ...)
	  ((not (eq (first fun) 'FUNCTION))
	   (unoptimized-funcall fun arguments))
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

(defun c2funcall (form args)
  (let* ((*inline-blocks* 0)
         (*temp* *temp*)
         (form-type (c1form-primary-type form))
         (function-p (and (subtypep form-type 'function)
                          (policy-assume-right-type)))
         (loc (maybe-save-value form args)))
    (unwind-exit (call-unknown-global-loc nil loc (inline-args args) function-p))
    (close-inline-blocks)))

;;;
;;; c2call-global:
;;;   ARGS is the list of arguments
;;;   LOC is either NIL or the location of the function object
;;;
(defun c2call-global (fname args &optional (return-type T))
  (let ((fun (find fname *global-funs* :key #'fun-name :test #'same-fname-p)))
    (when (and fun (c2try-tail-recursive-call fun args))
      (return-from c2call-global))
    (let* ((*inline-blocks* 0)
           (destination-rep-type (loc-representation-type *destination*))
           (destination-type (loc-type *destination*)))
      (unwind-exit (call-global-loc fname fun (inline-args args)
                                    (type-and return-type destination-type)
                                    destination-rep-type))
      (close-inline-blocks))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; CALL LOCATIONS
;;;
;;;
;;; call-global:
;;;   FNAME: the name of the function
;;;   LOC: either a function object or NIL
;;;   ARGS: a list of typed locs with arguments
;;;   RETURN-TYPE: the type to which the output is coerced
;;;
(defun call-global-loc (fname fun args return-type &optional (return-rep-type 'any)
                        &aux loc found fd minarg maxarg)
  (cond
    ;; Check whether it is a global function that we cannot call directly.
     ((and (or (null fun) (fun-global fun)) (not (inline-possible fname)))
      (call-unknown-global-loc fname nil args))

     ;; Open-codable function call.
     ((and (or (null fun) (fun-global fun))
	   (setq loc (inline-function fname args return-type return-rep-type)))
      loc)

     ;; Call to a function defined in the same file. Direct calls are
     ;; only emitted for low or neutral values of DEBUG is >= 2.
     ((and (<= (cmp-env-optimization 'debug) 1)
	   (or (fun-p fun)
	       (and (null fun)
		    (setf fun (find fname *global-funs* :test #'same-fname-p
				      :key #'fun-name)))))
      (call-loc fname fun args))

     ;; Call to a global (SETF ...) function
     ((not (symbolp fname))
      (call-unknown-global-loc fname fun args))

     ;; Call to a function whose C language function name is known,
     ;; either because it has been proclaimed so, or because it belongs
     ;; to the runtime.
     ((and (<= (cmp-env-optimization 'debug) 1)
	   (setf fd (get-sysprop fname 'Lfun))
	   (multiple-value-setq (minarg maxarg) (get-proclaimed-narg fname)))
      (call-exported-function-loc fname args fd minarg maxarg
				  #-ecl-min nil
				  #+ecl-min (member fname *in-all-symbols-functions*)))

     ((multiple-value-setq (found fd minarg maxarg) (si::mangle-name fname t))
      (call-exported-function-loc fname args fd minarg maxarg t))

     (t (call-unknown-global-loc fname fun args))))

(defun call-loc (fname fun args)
  `(CALL-NORMAL ,fun ,(coerce-locs args)))

(defun call-exported-function-loc (fname args fun-c-name minarg maxarg in-core)
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
	      (wt-nl-h "#ifdef __cplusplus")
	      (wt-nl-h "extern cl_object " fun-c-name "(...);")
	      (wt-nl-h "#else")
	      (wt-nl-h "extern cl_object " fun-c-name "();")
	      (wt-nl-h "#endif")))
	(setf (gethash fun-c-name *compiler-declared-globals*) 1))))
  (let ((fun (make-fun :name fname :global t :cfun fun-c-name :lambda 'NIL
		       :minarg minarg :maxarg maxarg)))
    (call-loc fname fun args)))

;;;
;;; call-unknown-global-loc
;;;   LOC is NIL or location containing function
;;;   ARGS is the list of typed locations for arguments
;;;
(defun call-unknown-global-loc (fname loc args &optional function-p)
  (unless loc
    (if (and (symbolp fname)
                       (not (eql (symbol-package fname) 
                                 (find-package "CL"))))
        (setf loc (add-symbol fname)
              function-p nil)
        (setf loc (list 'FDEFINITION fname)
              function-p t)))
  `(CALL-INDIRECT ,loc ,(coerce-locs args) ,fname ,function-p))

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
  (wt "cl_env_copy->stack_top-" narg))

(defun wt-call (fun args &optional fname env)
  (if env
      (progn
        (setf *aux-closure* t)
        (wt "(aux_closure.env="env",cl_env_copy->function=(void*)&aux_closure,")
        (wt-call fun args)
        (wt ")"))
      (progn
        (wt fun "(")
        (let ((comma ""))
          (dolist (arg args)
            (wt comma arg)
            (setf comma ",")))
        (wt ")")))
  (when fname (wt-comment fname)))

(defun wt-call-indirect (fun-loc args fname function-p)
  (let ((narg (length args)))
    (if function-p
        (wt "(cl_env_copy->function=" fun-loc ")->cfun.entry(" narg)
        (wt "ecl_function_dispatch(cl_env_copy," fun-loc ")(" narg))
    (dolist (arg args)
      (wt "," arg))
    (wt ")")
    (when fname (wt-comment fname))))

(defun wt-call-normal (fun args)
  (unless (fun-cfun fun)
    (baboon "Function without a C name: ~A" (fun-name fun)))
  (let* ((minarg (fun-minarg fun))
	 (maxarg (fun-maxarg fun))
	 (fun-c-name (fun-cfun fun))
	 (fun-lisp-name (fun-name fun))
	 (narg (length args))
	 (env nil))
    (case (fun-closure fun)
      (CLOSURE
       (setf env (environment-accessor fun)))
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
    (wt-call fun-c-name args fun-lisp-name env)))

;;; ----------------------------------------------------------------------

(put-sysprop 'funcall 'C1 'c1funcall)
(put-sysprop 'funcall 'c2 'c2funcall)
(put-sysprop 'call-global 'c2 'c2call-global)

(put-sysprop 'CALL 'WT-LOC #'wt-call)
(put-sysprop 'CALL-NORMAL 'WT-LOC #'wt-call-normal)
(put-sysprop 'CALL-INDIRECT 'WT-LOC #'wt-call-indirect)
