;;;; CMPEVAL --  The Expression Dispatcher.

;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECoLisp is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1expr (form)
  (setq form (catch *cmperr-tag*
    (cond ((symbolp form)
	   (setq form (chk-symbol-macrolet form))
	   (cond ((not (symbolp form))
		  (c1expr form))
		 ((eq form nil) (c1nil))
		 ((eq form t) (c1t))
		 ((keywordp form)
		  (make-c1form* 'LOCATION :type (object-type form)
				:args (list 'VV (add-object form))))
		 ((constantp form)
		  (or (c1constant-value (symbol-value form) nil)
		      (c1var form)))
		 (t (c1var form))))
	  ((consp form)
	   (let ((fun (car form))
		 ;; #+cltl2
		 setf-symbol)
	     (cond ((symbolp fun)
		    (c1call-symbol fun (cdr form)))
		   ((and (consp fun) (eq (car fun) 'LAMBDA))
		    (c1funcall form))
		   (t (cmperr "~s is not a legal function name." fun)))))
	  (t (c1constant-value form t)))))
  (if (eq form '*cmperr-tag*)
      (c1nil)
      form))

(defvar *c1nil* (make-c1form* 'LOCATION :type (object-type nil) :args nil))
(defun c1nil () *c1nil*)
(defvar *c1t* (make-c1form* 'LOCATION :type (object-type t) :args t))
(defun c1t () *c1t*)

(defun c1call-symbol (fname args &aux fd)
  (cond ((setq fd (get-sysprop fname 'c1special)) (funcall fd args))
	((c1call-local fname args))
	((setq fd (sch-local-macro fname))
	 (c1expr (cmp-expand-macro fd fname args)))
	((and (setq fd (get-sysprop fname 'C1))
	      (inline-possible fname))
	 (funcall fd args))
	((and (setq fd (get-sysprop fname 'C1CONDITIONAL))
	      (inline-possible fname)
	      (funcall fd args)))
	((setq fd (macro-function fname))
	 (c1expr (cmp-expand-macro fd fname args)))
	((and (setq fd (compiler-macro-function fname))
	      (let ((success nil))
		(multiple-value-setq (fd success)
		  (cmp-expand-compiler-macro fd fname args))
		success))
	 (c1expr fd))
	((and (setq fd (get-sysprop fname 'SYS::STRUCTURE-ACCESS))
	      (inline-possible fname)
	      ;;; Structure hack.
	      (consp fd)
	      (sys::fixnump (cdr fd))
	      (not (endp args))
	      (endp (cdr args)))
	 (case (car fd)
	   (VECTOR (c1expr `(svref ,(car args) ,(cdr fd)))) ; Beppe3
	   (LIST (c1expr `(sys:list-nth ,(cdr fd) ,(car args))))
	   (t (c1structure-ref1 (car args) (car fd) (cdr fd)))
	   )
	 )
	(t (c1call-global fname args))))

(defun c1call-local (fname args)
  (let ((fun (local-function-ref fname)))
    (when fun
      (let* ((forms (c1args* args))
	     (referred-vars (list (fun-var fun)))
	     (referred-local (list (fun-var fun)))
	     (return-type (or (get-local-return-type fun) 'T))
	     (arg-types (get-local-arg-types fun)))
	  ;; Add type information to the arguments.
	(when arg-types
	  (let ((fl nil))
	    (dolist (form forms)
	      (cond ((endp arg-types) (push form fl))
		    (t (push (and-form-type (car arg-types) form (car args)
					    :safe "In a call to ~a" fname)
			     fl)
		       (pop arg-types)
		       (pop args))))
	    (setq forms (nreverse fl))))
	(make-c1form* 'CALL-LOCAL
		      :sp-change t
		      :referred-vars referred-local
		      :local-referred referred-vars
		      :type return-type
		      :args fun forms)))))

(defun c1call-global (fname args)
  (let* ((forms (c1args* args))
	 (return-type (or (get-return-type fname) 'T)))
    (let ((arg-types (get-arg-types fname)))
      ;; Add type information to the arguments.
      (when arg-types
	(do ((fl forms (cdr fl))
	     (fl1 nil)
	     (al args (cdr al)))
	    ((endp fl)
	     (setq forms (nreverse fl1)))
	  (cond ((endp arg-types) (push (car fl) fl1))
		(t (push (and-form-type (car arg-types) (car fl) (car al)
					:safe "In a call to ~a" fname)
			 fl1)
		   (pop arg-types))))))
    (let ((arg-types (get-sysprop fname 'ARG-TYPES)))
      ;; Check argument types.
      (when arg-types
	(do ((fl forms (cdr fl))
	     (al args (cdr al)))
	    ((or (endp arg-types) (endp fl)))
	  (and-form-type (car arg-types) (car fl) (car al) :safe
			 "In a call to ~a" fname)
	  (pop arg-types))))
    (make-c1form* 'CALL-GLOBAL
		  :sp-change (not (get-sysprop fname 'NO-SP-CHANGE))
		  :type return-type
		  :args fname forms)))

(defun c2expr (form &aux (name (c1form-name form)) (args (c1form-args form)))
  (if (eq name 'CALL-GLOBAL)
      (c2call-global (first args) (second args) nil (destination-type))
      (let ((dispatch (get-sysprop name 'C2)))
	(if (or (eq name 'LET) (eq name 'LET*))
	    (let ((*volatile* (c1form-volatile* form)))
	      (declare (special *volatile*))
	      (apply dispatch args))
	    (apply dispatch args)))))

(defun c2expr* (form)
  (let* ((*exit* (next-label))
	 (*unwind-exit* (cons *exit* *unwind-exit*))
	 ;;(*lex* *lex*)
	 (*lcl* *lcl*)
	 (*temp* *temp*))
    (c2expr form)
    (wt-label *exit*))
  )

(defun c1progn (forms)
  (cond ((endp forms) (c1nil))
	((endp (cdr forms)) (c1expr (car forms)))
	(t (let* ((fl (mapcar #'c1expr forms))
		  (output-form (first (last fl)))
		  (output-type (and output-form (c1form-type output-form))))
	     (make-c1form* 'PROGN :type output-type :args fl)))))

(defun c2progn (forms)
  ;; c1progn ensures that the length of forms is not less than 1.
  (do ((l forms (cdr l))
       (lex *lex*))
      ((endp (cdr l))
       (c2expr (car l)))
    (let ((*destination* 'TRASH)) (c2expr* (car l)))
    (setq *lex* lex)			; recycle lex locations
  ))

(defun c1args* (forms)
  (mapcar #'(lambda (form) (c1expr form)) forms))

;;; Structures

(defun c1structure-ref (args)
  (if (and (not *safe-compile*)         ; Beppe
	   (not (endp args))
	   (not (endp (cdr args)))
	   (consp (second args))
	   (eq (caadr args) 'QUOTE)
	   (not (endp (cdadr args)))
	   (symbolp (cadadr args))
	   (endp (cddadr args))
	   (not (endp (cddr args)))
	   (sys::fixnump (third args))
	   (endp (cdddr args)))
      (c1structure-ref1 (car args) (cadadr args) (third args))
      (c1call-global 'SYS:STRUCTURE-REF args)))

(defun c1structure-ref1 (form name index)
  ;;; Explicitly called from c1expr and c1structure-ref.
  (make-c1form* 'SYS:STRUCTURE-REF :type (get-slot-type name index)
		:args (c1expr form) (add-symbol name) index))

(defun get-slot-type (name index)
  ;; default is t
  (type-filter
   (or (third (nth index (get-sysprop name 'SYS::STRUCTURE-SLOT-DESCRIPTIONS))) 'T)))

(defun c2structure-ref (form name-vv index
			     &aux (*inline-blocks* 0))
  (let ((loc (first (coerce-locs (inline-args (list form))))))
       (unwind-exit (list 'SYS:STRUCTURE-REF loc name-vv index)))
  (close-inline-blocks)
  )

(defun wt-structure-ref (loc name-vv index)
  (if *safe-compile*
      (wt "structure_ref(" loc "," name-vv "," `(COERCE-LOC :fixnum ,index) ")")
      #+clos
      (wt "(" loc ")->instance.slots[" `(COERCE-LOC :fixnum ,index) "]")
      #-clos
      (wt "(" loc ")->str.self[" `(COERCE-LOC :fixnum ,index) "]")))

(defun c1structure-set (args)
  (if (and (not *safe-compile*)         ; Beppe
	   (not (endp args))
	   (not (endp (cdr args)))
	   (consp (second args))
	   (eq (caadr args) 'QUOTE)
	   (not (endp (cdadr args)))
	   (symbolp (cadadr args))
	   (endp (cddadr args))
	   (not (endp (cddr args)))
	   (sys::fixnump (third args))
	   (not (endp (cdddr args)))
	   (endp (cddddr args)))
      (let ((x (c1expr (car args)))
	    (y (c1expr (fourth args)))
	    (name (cadadr args)))       ; remove QUOTE.
	;; Beppe. Type check added:
	(let* ((slot-type (get-slot-type name (third args)))
	       (new-type (type-and slot-type (c1form-type y))))
	  (if (null new-type)
	      (cmpwarn "The type of the form ~s is not ~s."
		       (fourth args) slot-type)
	      (progn
		(when (eq 'VAR (c1form-name y))
		  ;; it's a variable, propagate type
		  (setf (var-type (c1form-arg 0 y)) new-type))
		(setf (c1form-type y) new-type))))
	(make-c1form* 'SYS:STRUCTURE-SET :type (c1form-type y)
		      :args x (add-symbol name) (third args) y))
      (c1call-global 'SYS:STRUCTURE-SET args)))

(defun c2structure-set (x name-vv index y
			  &aux locs (*inline-blocks* 0))
  ;; the third argument here *c1t* is just a hack to ensure that
  ;; a variable is introduced for y if it is an expression with side effects
  (setq locs (inline-args (list x y *c1t*)))
  (setq x (second (first locs)))
  (setq y (second (second locs)))
  (if *safe-compile*
      (wt-nl "structure_set(" x "," name-vv "," index "," y ");")
      #+clos
      (wt-nl "(" x ")->instance.slots[" index "]= " y ";")
      #-clos
      (wt-nl "(" x ")->str.self[" index "]= " y ";"))
  (unwind-exit y)
  (close-inline-blocks)
  )

;;; ----------------------------------------------------------------------

(defun c1constant-value (val always-p)
  (cond
   ((eq val nil) (c1nil))
   ((eq val t) (c1t))
   ((sys::fixnump val)
    (make-c1form* 'LOCATION :type 'FIXNUM :args (list 'FIXNUM-VALUE val)))
   ((characterp val)
    (make-c1form* 'LOCATION :type 'CHARACTER
		  :args (list 'CHARACTER-VALUE (char-code val))))
   ((typep val 'LONG-FLOAT)
    (make-c1form* 'LOCATION :type 'LONG-FLOAT
		  :args (list 'LONG-FLOAT-VALUE val (add-object val))))
   ((typep val 'SHORT-FLOAT)
    (make-c1form* 'LOCATION :type 'SHORT-FLOAT
		  :args (list 'SHORT-FLOAT-VALUE val (add-object val))))
   (always-p
    (make-c1form* 'LOCATION :type (object-type val)
		  :args (list 'VV (add-object val))))
   (t nil)))

(defvar *compiler-temps*
	'(tmp0 tmp1 tmp2 tmp3 tmp4 tmp5 tmp6 tmp7 tmp8 tmp9))

(defmacro sys::define-inline-function (name vars &body body)
  (let ((temps nil)
	(*compiler-temps* *compiler-temps*))
    (dolist (var vars)
      (if (and (symbolp var)
	       (not (member var '(&OPTIONAL &REST &KEY &AUX) :test #'eq)))
	(push (or (pop *compiler-temps*)
		  (gentemp "TMP" (find-package 'COMPILER)))
	      temps)
	(error "The parameter ~s for the inline function ~s is illegal."
	       var name)))
    (let ((binding (cons 'LIST (mapcar
				#'(lambda (var temp) `(list ',var ,temp))
				vars temps))))
      `(progn
	 (defun ,name ,vars ,@body)
	 (define-compiler-macro ,name ,temps (list* 'LET ,binding ',body))))))

;;; ----------------------------------------------------------------------

(put-sysprop 'PROGN 'C1SPECIAL 'c1progn)
(put-sysprop 'PROGN 'C2 'c2progn)

(put-sysprop 'SYS:STRUCTURE-REF 'C1 'c1structure-ref)
(put-sysprop 'SYS:STRUCTURE-REF 'C2 'c2structure-ref)
(put-sysprop 'SYS:STRUCTURE-REF 'WT-LOC 'wt-structure-ref)
(put-sysprop 'SYS:STRUCTURE-SET 'C1 'c1structure-set)
(put-sysprop 'SYS:STRUCTURE-SET 'C2 'c2structure-set)
