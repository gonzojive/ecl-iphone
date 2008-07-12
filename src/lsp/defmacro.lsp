;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: SYSTEM -*-
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;         defines SYS:DEFMACRO*, the defmacro preprocessor

(in-package "SYSTEM")

#-ecl-min
(defvar *dl*)
#-ecl-min
(defvar *key-check*)
#-ecl-min
(defvar *arg-check*)

#+ecl-min
(sys:*make-special '*dl*)
#+ecl-min
(sys:*make-special '*key-check*)
#+ecl-min
(sys:*make-special '*arg-check*)

#+ecl-min
(si::fset 'push
	  #'(ext::lambda-block push (args env)
	      (let* ((what (second args))
		     (where (caddr args)))
		`(setq ,where (cons ,what ,where))))
	  t)

#+ecl-min
(si::fset 'pop
	  #'(ext::lambda-block pop (args env)
	      (let ((where (cadr args)))
		`(let* ((l ,where)
			(v (car l)))
		  (setq ,where (cdr l))
		  v)))
	  t)

#+ecl-min
(si::fset 'incf
	  #'(ext::lambda-block incf (args env)
	      (let* ((where (second args))
		     (what (caddr args)))
		(if what
		  `(setq ,where (+ ,where ,what))
		  `(setq ,where (1+ ,where)))))
	  t)

#+ecl-min
(si::fset 'decf
	  #'(ext::lambda-block decf (args env)
	      (let* ((where (second args))
		     (what (caddr args)))
		(if what
		  `(setq ,where (- ,where ,what))
		  `(setq ,where (1- ,where)))))
	  t)

(defun sys::search-keyword (list key)
  (cond ((atom list) 'failed)
	((atom (cdr list)) 'failed)
	((eq (car list) key) (cadr list))
	(t (search-keyword (cddr list) key))))

(defun check-keyword (tail keywords &optional (allow-other-keys nil aok-flag))
  (do (head
       arg
       (err nil))
      ((null tail)
       (when (and err (not allow-other-keys))
	 (error "They key ~s is not allowed" err)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq arg (car tail) tail (cdr tail)))
    (cond ((eq head :allow-other-keys)
	   (when (not aok-flag)
	     (setq allow-other-keys tail aok-flag t)))
	  ((not (member head keywords))
	   (setq err head)))))

(defun check-arg-length (list max-length)
  (when (> (length list) max-length)
    (error "Too many arguments supplied to a macro or a destructuring-bind form.")))

(defun dm-bad-key (key)
  (error "Defmacro-lambda-list contains illegal use of ~s." key))

(defun dm-too-few-arguments ()
  (error "Too few arguments supplied to a macro or a destructuring-bind form."))

(defun sys::destructure (vl macro)
  (declare (si::c-local))
  (labels ((dm-vl (vl whole macro)
	     (let* ((n (if macro 1 0))
		    (ppn 0)
		    (no-check nil)
		    all-keywords)
	       (multiple-value-bind (reqs opts rest key-flag keys allow-other-keys auxs)
		   (si::process-lambda-list vl (if macro 'macro 'destructuring-bind))
		 (dolist (v (cdr reqs))
		   (dm-v v `(if ,(dm-nth-cdr n whole)
			     ,(dm-nth n whole)
			     (dm-too-few-arguments)))
		   (incf n))
		 (dotimes (i (pop opts))
		   (let* ((x (first opts))
			  (init (second opts))
			  (sv (third opts)))
		     (setq opts (cdddr opts))
		     (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
		     (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole)))))
		     (incf n)))
		 (when rest
		   (dm-v rest (dm-nth-cdr n whole))
		   (setq no-check t
			 rest nil)
		   (when (and (null (last vl 0)) (member '&body vl))
		     (setq ppn (if macro (1- n) n))))
		 (dotimes (i (pop keys))
		   (when (null rest)
		     (setq rest (gensym))
		     (dm-v rest (dm-nth-cdr n whole))
		     (setq no-check t))
		   (let* ((temp (gensym))
			  (k (first keys))
			  (v (second keys))
			  (init (third keys))
			  (sv (fourth keys)))
		     (setq keys (cddddr keys))
		     (dm-v temp `(search-keyword ,rest ',k))
		     (dm-v v `(if (eq ,temp 'failed) ,init ,temp))
		     (when sv (dm-v sv `(not (eq ,temp 'failed))))
		     (push k all-keywords)))
		 (do ((l auxs (cddr l))) ((endp l))
		   (let* ((v (first l))
			  (init (second l)))
		     (dm-v v init)))
		 (cond (key-flag
			(push `(check-keyword ,rest ',all-keywords
				,@(if allow-other-keys '(t) '()))
			      *key-check*))
		       ((not no-check)
			(push `(check-arg-length ,whole ,n) *arg-check*))))
	       ppn))

	   (dm-v (v init)
	     (cond ((and v (symbolp v))
		    (push (if init (list v init) v) *dl*))
		   ((and v (atom v))
		    (error "destructure: ~A is not a list nor a symbol" v))
		   ((eq (first v) '&whole)
		    (let ((whole-var (second v)))
		      (if (listp whole-var)
			  (let ((new-whole (gensym)))
			    (dm-v new-whole init)
			    (dm-vl whole-var new-whole nil)
			    (setq whole-var new-whole))
			  (dm-v whole-var init))
		      (dm-vl (cddr v) whole-var nil)))
		   (t
		    (let ((temp (gensym)))
		      (push (if init (list temp init) temp) *dl*)
		      (dm-vl v temp nil)))))

	   (dm-nth (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 (list 'CAR v))
		 (1 (list 'CADR v))
		 (2 (list 'CADDR v))
		 (3 (list 'CADDDR v))
		 )))

	   (dm-nth-cdr (n v)
	     (multiple-value-bind (q r) (floor n 4)
	       (declare (fixnum q r))
	       (dotimes (i q) (setq v (list 'CDDDDR v)))
	       (case r
		 (0 v)
		 (1 (list 'CDR v))
		 (2 (list 'CDDR v))
		 (3 (list 'CDDDR v))
		 ))))

    (let* ((whole nil)
	   (*dl* nil)
	   (*key-check* nil)
	   (*arg-check* nil))
      (cond ((listp vl)
	     (when (eq (first vl) '&whole)
	       (setq whole (second vl) vl (cddr vl))
	       (when (listp whole)
		 (let ((new-whole (gensym)))
		   (dm-vl whole new-whole nil)
		   (setq whole new-whole)))))
	    ((symbolp vl)
	     (setq vl (list '&rest vl)))
	    (t (error "The destructuring-lambda-list ~s is not a list." vl)))
      (if (null whole) (setq whole (gensym)))
      (values (dm-vl vl whole macro) whole (nreverse *dl*) *key-check* *arg-check*))))

;;; valid lambda-list to DEFMACRO is:
;;;
;;;	( [ &whole sym ]
;;;	  [ &environment sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { defmacro-lambda-list | sym }.
;;; A symbol may be accepted as a DEFMACRO lambda-list, in which case
;;; (DEFMACRO <name> <symbol> ... ) is equivalent to
;;; (DEFMACRO <name> (&REST <symbol>) ...).
;;; Defmacro-lambda-list is defined as:
;;;
;;;	( { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
(defun find-documentation (body)
  (nth-value 3 (process-declarations body t)))

(defun remove-documentation (body)
  (multiple-value-bind (decls body doc)
      (process-declarations body t)
    (when decls (push `(declare ,@decls) body))
    (values body doc)))

(defun find-declarations (body &optional (doc t))
  (multiple-value-bind (decls body doc)
      (process-declarations body doc)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

(defun sys::expand-defmacro (name vl body
			     &aux *dl* *key-check* *arg-check*
			     doc decls ppn env)
  (multiple-value-setq (decls body doc)
    (find-declarations body))
  ;; We turn (a . b) into (a &rest b)
  ;; This is required because MEMBER (used below) does not like improper lists
  (let ((cell (last vl)))
    (when (rest cell)
      (setq vl (nconc (butlast vl 0) (list '&rest (rest cell))))))
  ;; If we find an &environment variable in the lambda list, we take not of the
  ;; name and remove it from the list so that DESTRUCTURE does not get confused
  (if (setq env (member '&environment vl :test #'eq))
      (setq vl (nconc (ldiff vl env) (cddr env))
	    env (second env))
      (progn
	(setq env (gensym))
	(push `(declare (ignore ,env)) decls)))
  (multiple-value-bind (ppn whole *dl* *key-check* *arg-check*)
      (destructure vl t)
    (setq body (nconc decls (append *arg-check* *key-check* body)))
    (values `(ext::lambda-block ,name (,whole ,env &aux ,@*dl*) ,@body)
	    ppn
	    doc)))

(si::fset 'defmacro
	  #'(ext::lambda-block defmacro (def env)
	      (let* ((name (second def))
		     (vl (third def))
		     (body (cdddr def))
		     (function))
		(multiple-value-bind (function pprint doc)
		    (sys::expand-defmacro name vl body)
		  (setq function `(function ,function))
		  (when *dump-defmacro-definitions*
		    (print function)
		    (setq function `(si::bc-disassemble ,function)))
		  (ext:register-with-pde def `(si::fset ',name ,function t ,pprint)))))
	  t)

;;; valid lambda-list to DESTRUCTURING-BIND is:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )
;;;
;;; where v is short for { destructuring-bind-lambda-list | sym }.
;;; A symbol may be accepted as a DESTRUCTURING-BIND lambda-list, in which case
;;; (DESTRUCTURING-BIND <name> <symbol> ... ) is equivalent to
;;; (DESTRUCTURING-BIND <name> (&REST <symbol>) ...).
;;; Destructuring-bind-lambda-list is defined as:
;;;
;;;	( [ &whole sym ]
;;;	  { v }*
;;;	  [ &optional { sym | ( v [ init [ v ] ] ) }* ]
;;;	  {  [ { &rest | &body } v ]
;;;	     [ &key { sym | ( { sym | ( key v ) } [ init [ v ]] ) }*
;;;		    [ &allow-other-keys ]]
;;;	     [ &aux { sym | ( v [ init ] ) }* ]
;;;	  |  . sym }
;;;	 )

(defmacro destructuring-bind (vl list &body body &aux (decls nil))
  (multiple-value-setq (decls body) (find-declarations body))
  (multiple-value-bind (ppn whole *dl* *key-check* *arg-check*)
      (destructure vl nil)
    (setq body (nconc decls (append *arg-check* *key-check* body)))
    (list* 'let* (cons (list whole list) *dl*) body)))

(defun warn (&rest foo) nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; MACROLET HELPER
;;;

(defun cmp-env-for-bytecodes (old-env)
  "Produce an environment which is safe to pass to the bytecodes
compiler. We remove all blocks and tags and ensure that
references to local variables will cause an error. This
environment can be used to bytecompile the functions in MACROLET
or SYMBOL-MACRO forms, and also to evaluate other forms."
  (declare (si::c-local))
  (flet ((local-var-error-function (name)
	  #'(lambda (whole env)
	      (error
"In a MACROLET function you tried to access a local variable, ~A,
from the function in which it appears." name)))
	 (local-fun-error-function (name)
	  #'(lambda (whole env)
	      (error
"In a MACROLET function you tried to access a local function, ~A,
from the function in which it appears." name))))
    (cons (do ((env (car old-env) (cdr env))
	       (variables '()))
	      ((endp env) (nreverse variables))
	    (let ((i (car env)))
	      (if (consp i)
		(let ((name (first i)))
		  (if (not (keywordp name))
		      (push (if (second i)
				i
			      (list name 'si::symbol-macro (local-var-error-function name)))
			    variables))))))
	  (do ((env (cdr old-env) (cdr env))
	       (macros '()))
	      ((endp env) (nreverse macros))
	    (let ((i (car env)))
	      (if (consp i)
		(push (if (eq (second i) 'SI::MACRO)
			  i
			(list (first i) 'SI:MACRO (local-fun-error-function (first i))))
		      macros)))))))

(defun macrolet-functions (definitions old-env)
  (declare (si::c-local))
  (let ((env (cmp-env-for-bytecodes old-env)))
    (si::eval-with-env
     (cons 'list
	   (mapcar #'(lambda (x)
		       (let* ((name (first x))
			      (llist (second x))
			      (def (cddr x)))
			 `(list ',name ,(si::expand-defmacro name llist def))))
		   definitions))
     env nil t)))

(defun cmp-env-register-macrolet (definitions old-env)
  (let ((macros (cdr old-env)))
    (dolist (record (macrolet-functions definitions old-env))
      (push (list (first record) 'si::macro (second record))
	    macros))
    (rplacd old-env macros)))
