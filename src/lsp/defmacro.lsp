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

(si::select-package "SYSTEM")

#-ecls-min
(defvar *dl*)
#-ecls-min
(defvar *key-check*)
#-ecls-min
(defvar *arg-check*)

#+ecls-min
(sys:*make-special '*dl*)
#+ecls-min
(sys:*make-special '*key-check*)
#+ecls-min
(sys:*make-special '*arg-check*)

#+ecls-min
(si::fset 'push
	  #'(lambda-block push (args env)
	      (let* ((what (second args))
		     (where (caddr args)))
		`(setq ,where (cons ,what ,where))))
	  t)

#+ecls-min
(si::fset 'pop
	  #'(lambda-block pop (args env)
	      (let ((where (cadr args)))
		`(let* ((l ,where)
			(v (car l)))
		  (setq ,where (cdr l))
		  v)))
	  t)

#+ecls-min
(si::fset 'incf
	  #'(lambda-block incf (args env)
	      (let* ((where (second args))
		     (what (caddr args)))
		(if what
		  `(setq ,where (+ ,where ,what))
		  `(setq ,where (1+ ,where)))))
	  t)

#+ecls-min
(si::fset 'decf
	  #'(lambda-block decf (args env)
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

(defun check-keyword (tail keywords)
  (do (head
       arg
       (err nil))
      ((null tail)
       (when err (error "They key ~s is not allowed" err)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq head (car tail) tail (cdr tail)))
    (if (atom tail)
      (error "keyword list is not a proper list")
      (setq arg (car tail) tail (cdr tail)))
    (cond ((and (eq head :allow-other-keys) tail)
	   (return-from check-keyword))
	  ((not (member head keywords))
	   (setq err head)))))

(defun dm-bad-key (key)
       (error "Defmacro-lambda-list contains illegal use of ~s." key))

(defun dm-too-few-arguments ()
       (error "Too few arguments are supplied to defmacro-lambda-list."))

(defun dm-too-many-arguments ()
       (error "Too many arguments are supplied to defmacro-lambda-list."))

(defun sys::destructure (vl whole macro &aux (*dl* nil) (*key-check* nil) (*arg-check* nil))
  (labels ((dm-vl (vl whole top &aux v allow-other-keys-p)
	     (do*((optionalp) (restp) (keyp)
		  (allow-other-keys-p) (auxp)
		  (rest) (allow-other-keys) (keys) (no-check)
		  (n (if top 1 0)) (ppn 0) (v))
		 ((not (consp vl))
		  (when vl
		    (when restp (dm-bad-key '&rest))
		    (push (list vl (dm-nth-cdr n whole)) *dl*)
		    (setq no-check t))
		  (when (and rest (not allow-other-keys))
		    (push (cons rest keys) *key-check*))
		  (unless no-check (push (cons whole n) *arg-check*))
		  ppn)
	       (declare (fixnum n ppn))
	       (setq v (car vl))
	       (cond
		 ((eq v '&optional)
		  (when optionalp (dm-bad-key '&optional))
		  (setq optionalp t)
		  (pop vl))
		 ((and macro (eq v '&body))
		  (when restp (dm-bad-key v))
		  (dm-v (second vl) (dm-nth-cdr n whole))
		  (setq restp t optionalp t no-check t)
		  (setq vl (cddr vl))
		  (setq ppn (if top (1- n) n)))
		 ((or (eq v '&rest) (eq v '&body))
		  (when restp (dm-bad-key v))
		  (dm-v (second vl) (dm-nth-cdr n whole))
		  (setq restp t optionalp t no-check t)
		  (setq vl (cddr vl)))
		 ((eq v '&key)
		  (when keyp (dm-bad-key '&key))
		  (setq rest (gensym))
		  (push (list rest (dm-nth-cdr n whole)) *dl*)
		  (setq keyp t restp t optionalp t no-check t)
		  (pop vl))
		 ((eq v '&allow-other-keys)
		  (when (or (not keyp) allow-other-keys-p)
		    (dm-bad-key '&allow-other-keys))
		  (setq allow-other-keys-p t)
		  (setq allow-other-keys t)
		  (pop vl))
		 ((eq v '&aux)
		  (when auxp (dm-bad-key '&aux))
		  (setq auxp t allow-other-keys-p t keyp t restp t optionalp t)
		  (pop vl))
		 (auxp
		  (let (x (init nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v)) (setq init (second v)))))
		    (dm-v x init))
		  (pop vl))
		 (keyp
		  (let ((temp (gensym)) x k (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v
					     k (intern (string v) 'keyword)))
			  (t (if (symbolp (car v))
				 (setq x (car v)
				       k (intern (string (car v)) 'keyword))
				 (setq x (cadar v) k (caar v)))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v temp `(search-keyword ,rest ',k))
		    (dm-v x `(if (eq ,temp 'failed) ,init ,temp))
		    (when sv (dm-v sv `(not (eq ,temp 'failed))))
		    (push k keys))
		  (pop vl))
		 (optionalp
		  (let (x (init nil) (sv nil))
		    (cond ((symbolp v) (setq x v))
			  (t (setq x (car v))
			     (unless (endp (cdr v))
			       (setq init (second v))
			       (unless (endp (cddr v))
				 (setq sv (caddr v))))))
		    (dm-v x `(if ,(dm-nth-cdr n whole) ,(dm-nth n whole) ,init))
		    (when sv (dm-v sv `(not (null ,(dm-nth-cdr n whole))))))
		  (incf n)
		  (pop vl))
		 (t (dm-v v `(if ,(dm-nth-cdr n whole)
			      ,(dm-nth n whole)
			      (dm-too-few-arguments)))
		    (incf n)
		    (pop vl)))))

	   (dm-v (v init)
	     (cond ((symbolp v)
		    (push (if init (list v init) v) *dl*))
		   ((atom v)
		    (error "destructure: ~A is not a list nor a symbol" v))
		   ((eq (car v) '&whole)
		    (let ((temp (cadr v)))
		      (push (if init (list temp init) temp) *dl*)
		      (dm-vl (cddr v) temp nil)))
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

  (cond ((listp vl))
        ((symbolp vl) (setq vl (list '&rest vl)))
        (t (error "The destructuring-lambda-list ~s is not a list." vl)))
  (let ((ppn (dm-vl vl whole macro)))
    (values ppn (nreverse *dl*) *key-check* *arg-check*))))

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

(defun find-declarations (body &optional (doc t))
  (multiple-value-bind (decls body doc)
      (process-declarations body doc)
    (values (if decls `((declare ,@decls)) nil)
	    body doc)))

(defun sys::expand-defmacro (name vl body
			     &aux *dl* *key-check* *arg-check*
			     doc decls whole ppn env)
  (multiple-value-setq (decls body doc)
    (find-declarations body))
  (if (and (consp vl) (eq (car vl) '&whole))
    (setq whole (second vl) vl (cddr vl))
    (setq whole (gensym)))
  (if (setq env (member '&environment vl :test #'eq))
      (setq vl (nconc (ldiff vl env) (cddr env))
	    env (second env))
      (progn
	(setq env (gensym))
	(push `(declare (ignore ,env)) decls)))
  (multiple-value-bind (ppn *dl* *key-check* *arg-check*)
      (destructure vl whole t)
    (dolist (kc *key-check*)
      (push `(check-keyword ,(car kc) ',(cdr kc)) body))
    (dolist (ac *arg-check*)
      (push `(unless (<= (list-length ,(car ac)) ,(cdr ac))
	      (dm-too-many-arguments)) body))
    (setq body (nconc decls body))
    (when doc (push doc body))
    (values (list* 'LAMBDA (list* whole env '&aux *dl*) body)
	    ppn)))

(si::fset 'defmacro
	  #'(lambda-block defmacro (def env)
	      (let* ((name (second def))
		     (vl (third def))
		     (body (cdddr def))
		     (function))
		(multiple-value-bind (expr pprint)
		    (sys::expand-defmacro name vl body)
		  (setq function `#'(lambda-block ,name ,@(cdr expr)))
		  (when *dump-defmacro-definitions*
		    (print function)
		    (setq function `(si::bc-disassemble ,function)))
		  `(si::fset ',name ,function t ,pprint))))
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
  (do ()
      ((or (atom body)
	   (atom (car body))
	   (not (eq (caar body) 'declare))))
    (push (car body) decls)
    (pop body))
  (multiple-value-bind (ppn *dl* *key-check* *arg-check*)
      (destructure vl '<destructured-form> nil)
    (dolist (kc *key-check*)
      (push `(check-keyword ,(car kc) ',(cdr kc)) body))
    (dolist (ac *arg-check*)
      (push `(unless (<= (list-length ,(car ac)) ,(cdr ac))
	      (dm-too-many-arguments)) body))
    (list* 'let* (cons (list '<destructured-form> list) *dl*)
	   (nconc decls body))))

(defun warn (&rest foo) nil)
