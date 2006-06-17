;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPENV  Environments of the Compiler.

(in-package "COMPILER")

;;; Only these flags are set by the user.
;;; If (safe-compile) is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defun init-env ()
  (setq *compiler-phase* 't1)
  (setq *callbacks* nil)
  (setq *max-temp* 0)
  (setq *temp* 0)
  (setq *next-cmacro* 0)
  (setq *next-cfun* 0)
  (setq *last-label* 0)
  (setq *load-objects* (make-hash-table :size 128 :test #'equal))
  (setq *make-forms* nil)
  (setq *permanent-objects* nil)
  (setq *temporary-objects* nil)
  (setq *local-funs* nil)
  (setq *global-var-objects* nil)
  (setq *global-vars* nil)
  (setq *global-funs* nil)
  (setq *linking-calls* nil)
  (setq *global-entries* nil)
  (setq *undefined-vars* nil)
  (setq *reservations* nil)
  (setq *top-level-forms* nil)
  (setq *compile-time-too* nil)
  (setq *clines-string-list* '())
  (setq *function-declarations* nil)
  (setq *inline-functions* nil)
  (setq *inline-blocks* 0)
  (setq *notinline* nil)
  )

(defun next-lcl () (list 'LCL (incf *lcl*)))

(defun next-cfun (&optional (prefix "L~D~A") (lisp-name nil))
  (let ((code (incf *next-cfun*)))
    (format nil prefix code (lisp-to-c-name lisp-name))))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env () (prog1 *env*
		     (incf *env*)
		     (setq *max-env* (max *env* *max-env*))))

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
      (push (type-filter (car al)) types)))

;;; The valid return type declaration is:
;;;	(( VALUES {type}* )) or ( {type}* ).

(defun function-return-type (return-types)
  (cond ((endp return-types) t)
        ((and (consp (car return-types))
              (eq (caar return-types) 'VALUES))
         (cond ((not (endp (cdr return-types)))
                (warn "The function return types ~s is illegal." return-types)
                t)
               ((or (endp (cdar return-types))
                    (member (cadar return-types) '(&optional &rest &key)))
                t)
               (t (type-filter (cadar return-types)))))
        (t (type-filter (car return-types)))))

(defun add-function-proclamation (fname decl &aux
         arg-types return-types)
  (cond ((and (symbolp fname)
	      (listp decl) (listp (cdr decl)))
	 (cond ((or (null decl)(eq (car decl) '*)) (setq arg-types '*)
		(rem-sysprop fname 'PROCLAIMED-ARG-TYPES))
	       (t (setq arg-types (function-arg-types (car decl)))
		  (put-sysprop fname 'PROCLAIMED-ARG-TYPES arg-types)))
	 (cond ((or (null (cdr decl))(eq (second decl) '*))
		(setq return-types '*))
	       (t (setq return-types (function-return-type (cdr decl)))))
         (put-sysprop fname 'PROCLAIMED-RETURN-TYPE return-types)
	 (cond((eql return-types '*))
	      (t(setq return-types (cdr decl))))
	 ;;; A non-local function may have local entry only if it returns
	 ;;; a single value.
         (if (and (not (endp return-types))
                  (endp (cdr return-types))
                  (not (and (consp (car return-types))
                            (eq (caar return-types) 'VALUES)
                            (or (endp (cdar return-types))
                                (not (endp (cddar return-types)))))))
             (put-sysprop fname 'PROCLAIMED-FUNCTION t)
	   (rem-sysprop fname 'PROCLAIMED-FUNCTION)))
        (t (warn "The function procl ~s ~s is not valid." fname decl))))

(defun add-function-declaration (fname arg-types return-types)
  (if (si::valid-function-name-p fname)
      (let ((fun (cmp-env-search-function fname)))
	(if (functionp fun)
	    (warn "Found function declaration for local macro ~A" fname)
	    (push (list fun
			(function-arg-types arg-types)
			(function-return-type return-types))
		  *function-declarations*)))
      (warn "In (DECLARE (FTYPE ...)): ~s is not a valid function name" fname)))

(defun get-arg-types (fname)
  (let ((x (assoc fname *function-declarations*)))
    (if x
	(second x)
	(get-sysprop fname 'PROCLAIMED-ARG-TYPES))))

(defun get-return-type (fname)
  (let ((x (assoc fname *function-declarations*)))
    (if x
	(third x)
	(get-sysprop fname 'PROCLAIMED-RETURN-TYPE))))

(defun get-local-arg-types (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (second x)
      nil))

(defun get-local-return-type (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (caddr x)
      nil))

(defun get-proclaimed-narg (fun)
  (multiple-value-bind (x found)
      (get-sysprop fun 'PROCLAIMED-ARG-TYPES)
    (if found
      (let ((minarg (length x)))
	(if (eq (first (last x)) '*)
	  (setf minarg (1- minarg)
		maxarg call-arguments-limit)
	  (setf maxarg minarg))
	(values minarg maxarg))
      (values 0 call-arguments-limit))))

;;; Proclamation and declaration handling.

(defun inline-possible (fname)
  (not (or ; (compiler-push-events)
	(member fname *notinline* :test #'same-fname-p)
	(and (symbolp fname) (get-sysprop fname 'CMP-NOTINLINE)))))

#-:CCL
(defun proclaim (decl)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (car decl)
    (SPECIAL
     (dolist (var (cdr decl))
       (if (symbolp var)
           (sys:*make-special var)
           (error "Syntax error in proclamation ~s" decl))))
    (OPTIMIZE
     (dolist (x (cdr decl))
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
		 (DEBUG)
                 (SAFETY (setq *safety* (second x)))
                 (SPACE (setq *space* (second x)))
                 (SPEED (setq *speed* (second x)))
                 (COMPILATION-SPEED (setq *speed* (- 3 (second x))))
                 (t (warn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (proclaim-var (second decl) (cddr decl))
         (error "Syntax error in proclamation ~s" decl)))
    (FTYPE
     (let (ftype)
       (cond ((and (consp (cdr decl))
		   (consp (setf ftype (second decl)))
		   (eq (first ftype) 'FUNCTION))
	      (dolist (v (cddr decl))
		(add-function-proclamation v (rest ftype))))
	     (t (error "Syntax error in proclamation ~a" decl)))))
    (INLINE
     (dolist (fun (cdr decl))
       (if (si::valid-function-name-p fun)
	   (rem-sysprop fun 'CMP-NOTINLINE)
	   (error "Not a valid function name ~s in proclamation ~s" fun decl))))
    (NOTINLINE
     (dolist (fun (cdr decl))
       (if (si::valid-function-name-p fun)
	   (put-sysprop fun 'CMP-NOTINLINE t)
	   (error "Not a valid function name ~s in proclamation ~s" fun decl))))
    ((OBJECT IGNORE DYNAMIC-EXTENT IGNORABLE)
     ;; FIXME! IGNORED!
     (dolist (var (cdr decl))
       (unless (si::valid-function-name-p var)
	 (error "Not a valid function name ~s in ~s proclamation" fun (car decl)))))
    (DECLARATION
     (do-declaration (rest decl) #'error))
    (SI::C-EXPORT-FNAME
     (dolist (x (cdr decl))
       (if (symbolp x)
	 (multiple-value-bind (found fname)
	     (si::mangle-name x t)
	   (if found
	     (warn "The function ~s is already in the runtime." x)
	     (put-sysprop x 'Lfun fname)))
	 (error "Syntax error in proclamation ~s" decl))))
    ((ARRAY ATOM BASE-CHAR BIGNUM BIT BIT-VECTOR CHARACTER COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT EXTENDED-CHAR FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE FUNCTION)
     (proclaim-var (car decl) (cdr decl)))
    (otherwise
     (unless (member (car decl) si:*alien-declarations*)
       (warn "The declaration specifier ~s is unknown." (car decl)))
     (and (functionp (get-sysprop (car decl) :proclaim))
	  (dolist (v (cdr decl))
		    (funcall (get-sysprop (car decl) :proclaim) v))))
    )
  nil
  )

(defun type-name-p (name)
  (or (get-sysprop name 'SI::DEFTYPE-DEFINITION)
      (find-class name nil)
      (get-sysprop name 'SI::STRUCTURE-TYPE)))

(defun do-declaration (names-list error)
  (declare (si::c-local))
  (dolist (new-declaration names-list)
    (unless (symbolp new-declaration)
      (funcall error "The declaration ~s is not a symbol" new-declaration))
    (when (type-name-p new-declaration)
      (funcall error "Symbol name ~S cannot be both the name of a type and of a declaration"
	       new-declaration))
    (pushnew new-declaration si:*alien-declarations*)))

(defun proclaim-var (type vl)
  (setq type (type-filter type))
  (dolist (var vl)
    (if (symbolp var)
	(let ((type1 (get-sysprop var 'CMP-TYPE))
	      (v (sch-global var)))
	  (setq type1 (if type1 (type-and type1 type) type))
	  (when v (setq type1 (type-and type1 (var-type v))))
	  (unless type1
	    (warn
	     "Inconsistent type declaration was found for the variable ~s."
	     var)
	    (setq type1 T))
	  (put-sysprop var 'CMP-TYPE type1)
	  (when v (setf (var-type v) type1)))
	(warn "The variable name ~s is not a symbol." var))))

(defun c1body (body doc-p &aux
	            (all-declarations nil)
		    (ss nil)		; special vars
		    (is nil)		; ignored vars
		    (ts nil)		; typed vars (var . type)
		    (others nil)	; all other vars
	            doc form)
  (loop
    (when (endp body) (return))
    (setq form (cmp-macroexpand (car body)))
    (cond
     ((stringp form)
      (when (or (null doc-p) (endp (cdr body)) doc) (return))
      (setq doc form))
     ((and (consp form) (eq (car form) 'DECLARE))
      (push form all-declarations)
      (dolist (decl (cdr form))
        (cmpassert (and (proper-list-p decl) (symbolp (first decl)))
		   "Syntax error in declaration ~s" form)
	(let* ((decl-name (first decl))
	       (decl-args (rest decl)))
	  (flet ((declare-variables (type var-list)
		   (cmpassert (proper-list-p var-list #'symbolp)
			      "Syntax error in declaration ~s" decl)
		   (when type
		     (dolist (var var-list)
		       (push (cons var type) ts)))))
	    (case decl-name
	      (SPECIAL
	       (cmpassert (proper-list-p decl-args #'symbolp)
			  "Syntax error in declaration ~s" decl)
	       (setf ss (append decl-args ss)))
	      (IGNORE
	       (cmpassert (proper-list-p decl-args #'symbolp)
			  "Syntax error in declaration ~s" decl)
	       (setf is (append decl-args is)))
	      (TYPE
	       (cmpassert decl-args "Syntax error in declaration ~s" decl)
	       (declare-variables (type-filter (first decl-args))
				  (rest decl-args)))
	      (OBJECT
	       (declare-variables 'OBJECT decl-args))
	      ;; read-only variable treatment. obsolete!
	      (:READ-ONLY)
	      ((OPTIMIZE FTYPE INLINE NOTINLINE DECLARATION SI::C-LOCAL SI::C-GLOBAL
		DYNAMIC-EXTENT IGNORABLE VALUES)
	       (push decl others))
	      (otherwise
	       (if (member decl-name si::*alien-declarations*)
		 (push decl others)
		 (multiple-value-bind (ok type)
		     (valid-type-specifier decl-name)
		   (cmpassert ok "The declaration specifier ~s is unknown." decl-name)
		   (declare-variables type decl-args))))
	      )))))
     (t (return)))
    (pop body)
    )
  (values body ss ts is others doc all-declarations)
  )

(defun c1add-declarations (decls &aux (dl nil))
  (dolist (decl decls dl)
    (case (car decl)
      (OPTIMIZE
       (push decl dl)
       (dolist (x (cdr decl))
	 (when (symbolp x) (setq x (list x 3)))
	 (if (or (not (consp x))
		 (not (consp (cdr x)))
		 (not (numberp (second x)))
		 (not (<= 0 (second x) 3)))
	   (cmpwarn "The OPTIMIZE proclamation ~s is illegal." x)
	   (case (car x)
	     (DEBUG)
	     (SAFETY (setq *safety* (second x)))
	     (SPACE (setq *space* (second x)))
	     ((SPEED COMPILATION-SPEED))
	     (t (cmpwarn "The OPTIMIZE quality ~s is unknown." (car x)))))))
      (FTYPE
       (let (ftype)
	 (cond ((and (consp (cdr decl))
		     (consp (setq ftype (second decl)))
		     (eq (first ftype) 'FUNCTION))
		(dolist (v (cddr decl))
		  (add-function-declaration v (second ftype) (cddr ftype))))
	       (t (cmpwarn "Syntax error in declaration ~s" decl)))))
      (INLINE
       (push decl dl)
       (dolist (fun (cdr decl))
	 (if (symbolp fun)
	   (setq *notinline* (remove fun *notinline*))
	   (cmperr "Not a valid function name ~s in declaration ~s" fun decl))))
      (NOTINLINE
       (push decl dl)
       (dolist (fun (cdr decl))
	 (if (symbolp fun)
	   (push fun *notinline*)
	   (cmperr "Not a valid function name ~s in declaration ~s" fun decl))))
      (DECLARATION
       (do-declaration (rest decl) #'cmperr))
      ((SI::C-LOCAL SI::C-GLOBAL))
      ((DYNAMIC-EXTENT IGNORABLE)
       ;; FIXME! SOME ARE IGNORED!
       )
      (otherwise
       (unless (member (car decl) si:*alien-declarations*)
	 (cmpwarn "The declaration specifier ~s is unknown." (car decl)))))))

(defun c1decl-body (decls body)
  (if (null decls)
      (c1progn body)
      (let* ((*function-declarations* *function-declarations*)
	     (si:*alien-declarations* si:*alien-declarations*)
	     (*notinline* *notinline*)
	     (*safety* *safety*)
	     (*space* *space*)
 	     (*speed* *speed*)
	     (dl (c1add-declarations decls)))
	(setq body (c1progn body))
	(make-c1form 'DECL-BODY body dl body))))

(put-sysprop 'decl-body 'c2 'c2decl-body)

(defun c2decl-body (decls body)
  (let ((*safety* *safety*)
        (*space* *space*)
	(*speed* *speed*)
        (*notinline* *notinline*))
    (c1add-declarations decls)
    (c2expr body)))

(defun check-vdecl (vnames ts is)
  (dolist (x ts)
    (unless (member (car x) vnames)
      (cmpwarn "Type declaration was found for not bound variable ~s."
               (car x))))
  (dolist (x is)
    (unless (member x vnames)
      (cmpwarn "Ignore declaration was found for not bound variable ~s." x)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; COMPILER ENVIRONMENT
;;;

(defmacro cmp-env-new ()
  '(cons nil nil))

(defun cmp-env-copy (&optional (env *cmp-env*))
  (cons (car env) (cdr env)))

(defmacro cmp-env-variables (&optional (env '*cmp-env*))
  `(car ,env))

(defmacro cmp-env-functions (&optional (env '*cmp-env*))
  `(cdr ,env))

(defun cmp-env-register-var (var &optional (env *cmp-env*) (boundp t))
  (push (list (var-name var)
	      (if (member (var-kind var) '(special global))
		  :special
		  t)
	      boundp
	      var)
	(cmp-env-variables)))

(defun cmp-env-declare-special (name &optional (env *cmp-env*))
  (cmp-env-register-var (c1make-global-variable name :warn nil :kind 'SPECIAL)
			env nil))

(defun cmp-env-register-function (fun &optional (env *cmp-env*))
  (push (list (fun-name fun) 'function fun)
	(cmp-env-functions env)))

(defun cmp-env-register-macro (name function &optional (env *cmp-env*))
  (push (list name 'si::macro function)
	(cmp-env-functions env)))

(defun cmp-env-register-symbol-macro (name form &optional (env *cmp-env*))
  (push (list name 'si::symbol-macro #'(lambda (whole env) form))
	(cmp-env-variables env)))

(defun cmp-env-register-block (blk &optional (env *cmp-env*))
  (push (list :block (blk-name blk) blk)
	(cmp-env-variables env)))

(defun cmp-env-register-tag (tag &optional (env *cmp-env*))
  (push (list :tag (list (tag-name tag)) tag)
	(cmp-env-variables env)))

(defun cmp-env-search-function (name &optional (env *cmp-env*))
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-functions env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ((eq (first record) name)
	     (setf found (first (last record)))
	     (return))))
    (values found ccb clb unw)))

(defun cmp-env-search-variables (type name env)
  (let ((ccb nil)
	(clb nil)
	(unw nil)
	(found nil))
    (dolist (record (cmp-env-variables env))
      (cond ((eq record 'CB)
	     (setf ccb t))
	    ((eq record 'LB)
	     (setf clb t))
	    ((eq record 'UNWIND-PROTECT)
	     (setf unw t))
	    ((atom record)
	     (baboon))
	    ((not (eq (first record) type)))
	    ((eq type :block)
	     (when (eq name (second record))
	       (setf found record)
	       (return)))
	    ((eq type :tag)
	     (when (member name (second record) :test #'eql)
	       (setf found record)
	       (return)))
	    ((eq (second record) 'si::symbol-macro)
	     (when (eq name 'si::symbol-macro)
	       (setf found record))
	     (return))
	    (t
	     (setf found record)
	     (return))))
    (values (first (last found)) ccb clb unw)))

(defun cmp-env-search-block (name &optional (env *cmp-env*))
  (cmp-env-search-variables :block name env))

(defun cmp-env-search-tag (name &optional (env *cmp-env*))
  (cmp-env-search-variables :tag name env))

(defun cmp-env-search-symbol-macro (name &optional (env *cmp-env*))
  (cmp-env-search-variables name 'si::symbol-macro env))

(defun cmp-env-search-var (name &optional (env *cmp-env*))
  (cmp-env-search-variables name t env))

(defun cmp-env-search-macro (name &optional (env *cmp-env*))
  (let ((f (cmp-env-search-function name env)))
    (if (functionp f) f nil)))

(defun cmp-env-mark (mark &optional (env *cmp-env*))
  (cons (cons mark (car env))
	(cons mark (cdr env))))

(defun cmp-env-new-variables (new-env old-env)
  (loop for i in (ldiff (cmp-env-variables *cmp-env*)
			(cmp-env-variables old-env))
	when (and (consp i) (var-p (fourth i)))
	collect (fourth i)))
