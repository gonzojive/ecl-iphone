;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMULT  Multiple-value-call and Multiple-value-prog1.

(in-package "COMPILER")

(defun c1multiple-value-call (args &aux info funob)
  (when (endp args) (too-few-args 'multiple-value-call 1 0))
  (cond ((endp (rest args)) (c1funcall args))
        (t (setq funob (c1funob (first args)))
           (setq info (copy-info (second funob)))
           (setq args (c1args (rest args) info))
           (list 'MULTIPLE-VALUE-CALL info funob args)))
  )

(defun c2multiple-value-call (funob forms)
  (let ((tot (list 'LCL (next-lcl)))
	(nr (list 'LCL (next-lcl)))
        (loc (save-funob funob)))
    (wt-nl "{ cl_index " tot "=0;")
    (let ((*unwind-exit* `((STACK ,tot) ,@*unwind-exit*)))
      (let ((*destination* 'VALUES))
	(dolist (form forms)
	  (c2expr* form)
	  (wt-nl tot "+=cl_stack_push_values();")))
      (c2funcall funob 'ARGS-PUSHED loc tot))
    (wt "}"))
  )

(defun c1multiple-value-prog1 (args &aux (info (make-info)) form)
  (when (endp args) (too-few-args 'multiple-value-prog1 1 0))
  (setq form (c1expr* (first args) info))
  (setq args (c1args (rest args) info))
  (list 'MULTIPLE-VALUE-PROG1 info form args)
  )

(defun c2multiple-value-prog1 (form forms)
  (if (eq 'TRASH *destination*)
      ;; dont bother saving values
      (c2progn (cons form forms))
      (let ((nr `(LCL ,(next-lcl))))
	(let ((*destination* 'VALUES)) (c2expr* form))
	(wt-nl "{ cl_index " nr "=cl_stack_push_values();")
	(let ((*destination* 'TRASH)
	      (*unwind-exit* `((STACK ,nr) ,@*unwind-exit*)))
	  (dolist (form forms)
	    (c2expr* form)))
	(wt-nl "cl_stack_pop_values(" nr ");}")
	(unwind-exit 'VALUES)))))

;;; Beppe:
;;; this is the WRONG way to handle 1 value problem.
;;; should be done in c2values, so that (values (truncate a b)) can
;;; be used to restrict to one value, so we would not have to warn
;;; if this occurred in a proclaimed fun.

(defun c1values (args &aux (info (make-info)))
  (if (and args (null (rest args)))
    ;; unnecessary code is produced for expression (values nil)
    (c1expr (first args))
    (progn
      (setq args (c1args args info))
      (list 'VALUES info args))))

(defun c2values (forms)
  (when (and (eq *destination* 'RETURN-OBJECT)
             (rest forms)
             (consp *current-form*)
             (eq 'DEFUN (first *current-form*)))
    (cmpwarn "Trying to return multiple values. ~
              ~%;But ~a was proclaimed to have single value.~
              ~%;Only first one will be assured."
             (second *current-form*)))
  (let ((nv (length forms)))
    (declare (fixnum nv))
    (case nv
      (0 (wt-nl "value0=Cnil;NValues=0;")
	 (unwind-exit 'RETURN))
      (1 (c2expr (first forms)))
      (t (let ((*inline-blocks* 0))
	   ;; 1) By inlining arguments we make sure that VL has no call to funct.
	   ;; 2) Reverse args to avoid clobbering VALUES(0)
	   (do ((vl (nreverse (inline-args forms)) (rest vl))
		(i (1- (length forms)) (1- i)))
	       ((null vl))
	     (declare (fixnum i))
	     (wt-nl "VALUES(" i ")=" (second (first vl)) ";"))
	   (wt-nl "NValues=" nv ";")
	   (unwind-exit 'VALUES)
	   (close-inline-blocks))))))

(defun c1multiple-value-setq (args &aux (info (make-info)) (vrefs nil)
			      (vars nil) (temp-vars nil) (late-bindings nil))
  (when (or (endp args) (endp (rest args)))
        (too-few-args 'multiple-value-setq 2 0))
  (unless (endp (cddr args))
          (too-many-args 'multiple-value-setq 2 (length args)))
  (dolist (var (reverse (first args)))
          (cmpck (not (symbolp var)) "The variable ~s is not a symbol." var)
	  (setq var (chk-symbol-macrolet var))
	  (cond ((symbolp var)
		 (cmpck (constantp var)
			"The constant ~s is being assigned a value." var)
		 (push var vars))
		(t (let ((new-var (gensym)))
		     (push new-var vars)
		     (push new-var temp-vars)
		     (push `(setf ,var ,new-var) late-bindings)))))
  (if temp-vars
    (c1expr `(let* (,@temp-vars)
	      (multiple-value-setq ,vars ,@(second args))
	      ,@late-bindings))
    (dolist (var vars
	     (list 'MULTIPLE-VALUE-SETQ info (nreverse vrefs)
		   (c1expr* (second args) info)))
      (setq var (c1vref var))
      (push var vrefs)
      (push (first var) (info-changed-vars info)))))

(defun multiple-value-check (vrefs form)
  (and (rest vrefs)
       (eq (first form) 'CALL-GLOBAL)
       (let ((fname (third form)))
         (when (and (symbolp fname)
                    (get fname 'PROCLAIMED-RETURN-TYPE))
           (cmpwarn "~A was proclaimed to have only one return value. ~
                     ~%;;; But you appear to want multiple values." fname)))))

(defun c2multiple-value-setq (vrefs form)
  (multiple-value-check vrefs form)
  (let* ((*lcl* *lcl*)
         (nr (list 'LCL (next-lcl))))
    (let ((*destination* 'VALUES)) (c2expr* form))
    (wt-nl "{int " nr "=NValues;")
    (do ((vs vrefs (rest vs))
         (i 0 (1+ i))
         (vref))
        ((endp vs))
      (declare (fixnum i))
      (setq vref (first vs))
      (wt-nl "if (" nr ">0) {")
      (set-var (list 'VALUE i) (first vref)) ; (second vref) ccb
      (unless (endp (rest vs)) (wt-nl nr "--;"))
      (wt-nl "} else {") (set-var nil (first vref)) ; (second vref) ccb
      (wt "}"))
    (unless (eq *exit* 'RETURN) (wt-nl))
    (wt-nl "if (NValues>1) NValues=1;}")
    (unwind-exit (if vrefs (caar vrefs) '(VALUE 0)))))

(defun c1multiple-value-bind (args &aux (info (make-info))
                                   (vars nil) (vnames nil) init-form
                                   ss is ts body other-decls
                                   (*vars* *vars*))
  (when (or (endp args) (endp (rest args)))
    (too-few-args 'multiple-value-bind 2 (length args)))

  (multiple-value-setq (body ss ts is other-decls) (c1body (cddr args) nil))

  (c1add-globals ss)

  (dolist (s (first args))
    (push s vnames)
    (push (c1make-var s ss is ts) vars))
  (setq init-form (c1expr* (second args) info))
  (dolist (v (setq vars (nreverse vars)))
    (push-vars v))
  (check-vdecl vnames ts is)
  (setq body (c1decl-body other-decls body))
  (add-info info (second body))
  (setf (info-type info) (info-type (second body)))
  (dolist (var vars) (check-vref var))
  (list 'MULTIPLE-VALUE-BIND info vars init-form body)
  )

(defun c2multiple-value-bind (vars init-form body)
  (multiple-value-check vars init-form)

  ;; 0) Compile the form which is going to give us the values
  (let ((*destination* 'VALUES)) (c2expr* init-form))

  (let* ((*unwind-exit* *unwind-exit*)
	 (*env-lvl* *env-lvl*)
	 (*env* *env*)
	 (*lcl* *lcl*)
	 (labels nil)
	 (env-grows nil)
	 (nr (list 'LCL (next-lcl))))
    ;; 1) Retrieve the number of output values
    (wt-nl "{ int " nr "=NValues;")

    ;; 2) For all variables which are not special and do not belong to
    ;;    a closure, make a local C variable.
    (dolist (var vars)
      (declare (type var var))
      (let ((kind (local var)))
	(if kind
	  (let ((lcl (next-lcl)))
	    (setf (var-loc var) lcl)
	    (wt-nl *volatile* (register var) (rep-type kind)) (wt-lcl lcl)
	    (wt ";")
	    (wt-comment (var-name var)))
	  (unless env-grows (setq env-grows (var-ref-ccb var))))))

    ;; 3) If there are closure variables, set up an environment.
    (when (setq env-grows (env-grows env-grows))
      (let ((env-lvl *env-lvl*))
	(wt-nl "{ volatile cl_object env" (incf *env-lvl*)
	       " = env" env-lvl ";")))

    ;; 4) Loop for assigning values to variables 
    (do ((vs vars (rest vs))
	 (i 0 (1+ i))
	 (value '(VALUE 0)))
	((endp vs))
      (declare (fixnum i))
      (push (next-label) labels)
      (wt-nl "if (" nr "--<=0) ") (wt-go (first labels))
      (setf (second value) i)
      (bind value (first vs)))

    ;; 5) Loop for setting default values when there are less output
    ;;    than variables.
    (let ((label (next-label)))
      (wt-nl) (wt-go label)
      (setq labels (nreverse labels))
      (let ((*suppress-compiler-warnings* t))
	;; suppress the warning by default-init
	(dolist (v vars)
	  (wt-label (first labels))
	  (pop labels)
	  (bind (third (default-init (var-type v))) v)))
      (wt-label label))

    ;; 6) Compile the body. If there are bindings of special variables,
    ;;    these bindings are undone here.
    (c2expr body)

    ;; 7) Close the C expression.
    (when env-grows (wt "}"))
    (wt "}"))
  )

;;; ----------------------------------------------------------------------

(setf (get 'multiple-value-call 'c1special) #'c1multiple-value-call)
(setf (get 'multiple-value-call 'c2) #'c2multiple-value-call)
(setf (get 'multiple-value-prog1 'c1special) #'c1multiple-value-prog1)
(setf (get 'multiple-value-prog1 'c2) #'c2multiple-value-prog1)
(setf (get 'values 'c1) #'c1values)
(setf (get 'values 'c2) #'c2values)
(setf (get 'multiple-value-setq 'c1) #'c1multiple-value-setq)
(setf (get 'multiple-value-setq 'c2) #'c2multiple-value-setq)
(setf (get 'multiple-value-bind 'c1) #'c1multiple-value-bind)
(setf (get 'multiple-value-bind 'c2) #'c2multiple-value-bind)
