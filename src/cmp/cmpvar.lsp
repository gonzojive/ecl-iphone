;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPVAR  Variables.

(in-package "COMPILER")

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (var (:type vector) :named)
  name		;;; Variable name.
  (ref 0 :type fixnum)
		;;; Number of references to the variable (-1 means IGNORE).
		;;; During Pass 2: set below *register-min* for non register.
  ref-ccb	;;; Cross closure reference: T or NIL.
  kind		;;; One of LEXICAL, SPECIAL, GLOBAL, OBJECT, FIXNUM,
  		;;; CHARACTER, LONG-FLOAT, SHORT-FLOAT, or REPLACED (used for
		;;; LET variables).
  (loc 'OBJECT)	;;; During Pass 1: indicates whether the variable can
		;;; be allocated on the c-stack: OBJECT means
		;;; the variable is declared as OBJECT, and CLB means
		;;; the variable is referenced across Level Boundary and thus
		;;; cannot be allocated on the C stack.  Note that OBJECT is
		;;; set during variable binding and CLB is set when the
		;;; variable is used later, and therefore CLB may supersede
		;;; OBJECT.
		;;; During Pass 2:
  		;;; For REPLACED: the actual location of the variable.
  		;;; For FIXNUM, CHARACTER, LONG-FLOAT, SHORT-FLOAT, OBJECT:
  		;;;   the cvar for the C variable that holds the value.
  		;;; For LEXICAL: the frame-relative address for the variable.
		;;; For SPECIAL and GLOBAL: the vv-index for variable name.
  (type t)	;;; Type of the variable.
  (index -1)    ;;; position in *vars*. Used by similar.
  ) |#

;;; A special binding creates a var object with the kind field SPECIAL,
;;; whereas a special declaration without binding creates a var object with
;;; the kind field GLOBAL.  Thus a reference to GLOBAL may need to make sure
;;; that the variable has a value.

;;;  Bootstrap problem: proclaim needs this function:
(defun sch-global (name)
  (dolist (var *undefined-vars* nil)
    (declare (type var var))
    (when (eq (var-name var) name)
      (return-from sch-global var))))

;;;
;;; Check if a variable has been declared as a special variable with a global
;;; value.

(defun check-global (name)
  (member name *global-vars* :test #'eq))

;;;
;;; Check if the symbol has a symbol macro
;;;
(defun chk-symbol-macrolet (symbol)
  (do ((form symbol))
      ((not (symbolp form)) form)
    (dolist (v *vars*
	     ;; At the end, loof for a DEFINE-SYMBOL-MACRO definition
	     (let ((expansion (get-sysprop form 'si::symbol-macro)))
	       (if expansion
		 (setq form expansion)
		 (return-from chk-symbol-macrolet form))))
      ;; Search for a SYMBOL-MACROLET definition
      (cond ((consp v)
	     (when (eq (first v) form)
	       (setq form (second v))
	       (return)))
	    ((symbolp v))
	    ((eq (var-name v) form)
	     ;; Any macro definition has been shadowed by LET/LET*, etc.
	     (return-from chk-symbol-macrolet form))))))

;;; During Pass 1, *vars* emulates the environment: it holds a list of var
;;; objects and the symbols 'CB' (Closure Boundary) and 'LB' (Level Boundary).
;;; 'CB' is pushed on *vars* when the compiler begins to process a closure.
;;; 'LB' is pushed on *vars* when *level* is incremented.
;;; *GLOBALS* holds a list of var objects for those variables that are
;;; not defined.  This list is used only to suppress duplicated warnings when
;;; undefined variables are detected.

(defun c1make-var (name specials ignores types &aux x)
  (let ((var (make-var :name name)))
    (declare (type var var))		; Beppe
    (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
    (cmpck (constantp name) "The constant ~s is being bound." name)

    (cond ((or (member name specials) (sys:specialp name)
               (check-global name))	;; added. Beppe 17 Aug 1987
           
           (setf (var-kind var) 'SPECIAL)
           (setf (var-loc var) (add-symbol name))
           (cond ((setq x (assoc name types))
                  (setf (var-type var) (cdr x)))
                 ((setq x (get-sysprop name 'CMP-TYPE))
                  (setf (var-type var) x)))
           (setq *special-binding* t))
          (t
           (dolist (v types)
             (when (eq (car v) name)
               (case (cdr v)
;                 (OBJECT (setf (var-loc var) 'OBJECT))
                 (REGISTER
                  (incf (var-ref var) 100))
                 (t (setf (var-type var) (cdr v))))))
;           (when (or (null (var-type var))
;                     (eq t (var-type var)))
;             (setf (var-loc var) 'OBJECT))
           ;; :READ-ONLY variable treatment.
;           (when (eq 'READ-ONLY (var-type var))
;             (setf (var-type var) 't))
           (setf (var-kind var) 'LEXICAL))) ; we rely on check-vref to fix it
    (when (member name ignores) (setf (var-ref var) -1)) ; IGNORE.
    var)
  )

(defun check-vref (var)
  (when (and (eq (var-kind var) 'LEXICAL)
             (not (var-ref-ccb var)))
    (when (zerop (var-ref var)) ;;; This field may be -1 (IGNORE). Beppe
        (cmpwarn "The variable ~s is not used." (var-name var)))
    (when (not (eq (var-loc var) 'CLB))
      ;; if the variable can be stored locally, set it var-kind to its type
      (setf (var-kind var)
	    (if (> (var-ref var) 1)
		(lisp-type->rep-type (var-type var))
		:OBJECT))))
  )

(defun c1var (name)
  (let ((vref (c1vref name)))
    (unless (var-p vref)
      ;; This might be the case if there is a symbol macrolet
      (return-from c1var vref))
    (make-c1form* 'VAR
		  :referred-vars (list vref)
		  :local-referred (list vref)
		  :type (var-type vref)
		  :args vref)))

(defun make-lcl-var (&key rep-type (type 'T))
  (unless rep-type
    (setq rep-type (if type (lisp-type->rep-type type) :object)))
  (unless type
    (setq type 'T))
  (make-var :kind rep-type :type type :loc `(LCL ,(incf *lcl*))))

(defun make-temp-var (&optional (type 'T))
  (make-var :kind :object :type type :loc `(TEMP ,(next-temp))))

;;; A variable reference (vref for short) is a list: pair
;;;	( var-object ) Beppe(ccb) ccb-reference )

(defun c1vref (name &aux (ccb nil) (clb nil))
  (dolist (var *vars*)
    (declare (type var var))
    (cond ((eq var 'CB) (setq ccb t))	; closure boundary
          ((eq var 'LB) (setq clb t))	; level boundary
	  ((consp var)
	   (when (eq (first var) name) ; symbol macrolet
	     (baboon)
	     (return-from c1vref (c1expr (second var)))))
          ((eq (var-name var) name)
           (when (minusp (var-ref var)) ; IGNORE.
             (cmpwarn "The ignored variable ~s is used." name)
             (setf (var-ref var) 0))
	   (when (eq (var-kind var) 'LEXICAL)
	     (cond (ccb (setf (var-ref-ccb var) t
			      (var-loc var) 'OBJECT)) ; replace a previous 'CLB
		   (clb (setf (var-loc var) 'CLB))))
           (incf (var-ref var))
           (return-from c1vref var)))) ; ccb
  (let ((var (sch-global name)))
    (unless var
      (unless (or (sys:specialp name) (check-global name))
	(undefined-variable name))
      (setq var (make-var :name name
                          :kind 'GLOBAL
                          :loc (add-symbol name)
                          :type (or (get-sysprop name 'CMP-TYPE) t)))
      (push var *undefined-vars*))
    var)				; ccb
  )


;;; At each variable binding, the variable is added to *vars* which
;;; emulates the environment.
;;; The index is computed, which is used by similar to compare functions.
;;;
(defun push-vars (v)
  (setf (var-index v) (length *vars*))
  (push v *vars*))

(defun unboxed (var)
  (not (eq (var-rep-type var) :object)))

(defun local (var)
  (and (not (member (var-kind var) '(LEXICAL SPECIAL GLOBAL REPLACED)))
       (var-kind var)))

(defun c2var (vref) (unwind-exit vref))

(defun c2location (loc) (unwind-exit loc))

(defun wt-var (var &aux (var-loc (var-loc var))) ; ccb
  (declare (type var var))
  (case (var-kind var)
    (LEXICAL (cond ;(ccb (wt-env var-loc))
                   ((var-ref-ccb var) (wt-env var-loc))
                   (t (wt-lex var-loc))))
    (SPECIAL (wt "(" var-loc "->symbol.dbind)"))
    (REPLACED (wt var-loc))
    (GLOBAL (if *safe-compile*
              (wt "symbol_value(" var-loc ")")
              (wt "(" var-loc "->symbol.dbind)")))
    (t (wt var-loc))
    ))

(defun var-rep-type (var)
  (case (var-kind var)
    ((LEXICAL SPECIAL GLOBAL) :object)
    (REPLACED (loc-representation-type (var-loc var)))
    (t (var-kind var))))

(defun set-var (loc var &aux (var-loc (var-loc var))) ;  ccb
  (if (var-p var)
    (case (var-kind var)
      (LEXICAL
       (wt-nl)
       (if (var-ref-ccb var)
	   (wt-env var-loc)
	   (wt-lex var-loc))
       (wt "= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
      (SPECIAL
       (wt-nl "(" var-loc "->symbol.dbind)= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
      (GLOBAL
       (if *safe-compile*
	   (wt-nl "cl_set(" var-loc ",")
	   (wt-nl "(" var-loc "->symbol.dbind)= "))
       (wt-coerce-loc (var-rep-type var) loc)
       (wt (if *safe-compile* ");" ";")))
      (t
       (wt-nl var-loc "= ")
       (wt-coerce-loc (var-rep-type var) loc)
       (wt #\;))
    )
    (baboon)))

(defun wt-lex (lex)
  (if (consp lex)
    (wt "lex" (car lex) "[" (cdr lex) "]")
    (wt-lcl lex)))

;;; reference to variable of inner closure.
(defun wt-env (clv) (wt "*CLV" clv))

;;; ----------------------------------------------------------------------

(defun c1add-globals (globals)
  (dolist (name globals)
    (push (make-var :name name
                    :kind 'GLOBAL
                    :loc (add-symbol name)
                    :type (let ((x (get-sysprop name 'CMP-TYPE))) (if x x t))
                    )
          *vars*))
  )

(defun c1setq (args)
  (let ((l (length args)))
    (declare (fixnum l))
    (cmpck (oddp l) "SETQ requires an even number of arguments.")
    (cond ((zerop l) (c1nil))
	  ((= l 2) (c1setq1 (first args) (second args)))
	  (t
	   (do ((pairs args (cddr pairs))
		(forms nil))
	       ((endp pairs)
		(make-c1form* 'PROGN
			      :type (c1form-type (first forms))
			      :args (nreverse forms)))
             (push (c1setq1 (first pairs) (second pairs)) forms)
             )))))

(defun c1setq1 (name form)
  (cmpck (not (symbolp name)) "The variable ~s is not a symbol." name)
  (cmpck (constantp name) "The constant ~s is being assigned a value." name)
  (setq name (chk-symbol-macrolet name))
  (unless (symbolp name)
    (return-from c1setq1 (c1expr `(setf ,name ,form))))
  (let* ((name1 (c1vref name))
	 (form1 (c1expr form))
	 (type (type-and (var-type name1) (c1form-type form1))))
    (unless type
      (cmpwarn "Type mismatch between ~s and ~s." name form)
      (setq type T))
    ;; Is this justified????
    #+nil(setf (c1form-type form1) type)
    (make-c1form* 'SETQ :type type
		  :changed-vars (list name1)
		  :referred-vars (list name1)
		  :local-referred (list name1)
		  :args name1 form1)))

(defun c2setq (vref form)
  (let ((*destination* vref)) (c2expr* form))
  (if (eq (c1form-name form) 'LOCATION)
    (c2location (c1form-arg 0 form))
    (unwind-exit vref))
  )

(defun c1progv (args)
  (check-args-number 'PROGV args 2)
  (let ((symbols (c1expr (first args)))
	(values (c1expr (second args)))
	(forms (c1progn (cddr args))))
    (make-c1form* 'PROGV :type (c1form-type forms)
		  :args symbols values forms)))

(defun c2progv (symbols values body
                &aux (*unwind-exit* *unwind-exit*))
  (let* ((*lcl* *lcl*)
         (lcl (next-lcl))
         (sym-loc (make-lcl-var))
         (val-loc (make-lcl-var)))
    (wt-nl "{cl_object " sym-loc "," val-loc ";")
    (wt-nl "bds_ptr " lcl "=bds_top;")
    (push lcl *unwind-exit*)
    
    (let ((*destination* sym-loc)) (c2expr* symbols))
    
    (let ((*destination* val-loc)) (c2expr* values))
    
    (wt-nl "while(!endp(" sym-loc ")) {")
    (when *safe-compile*
      (wt-nl "if(type_of(CAR(" sym-loc "))!=t_symbol)")
      (wt-nl
       "FEinvalid_variable(\"~s is not a symbol.\",CAR(" sym-loc "));"))
    (wt-nl "if(endp(" val-loc "))bds_bind(CAR(" sym-loc "),OBJNULL);")
    (wt-nl "else{bds_bind(CAR(" sym-loc "),CAR(" val-loc "));")
    (wt-nl val-loc "=CDR(" val-loc ");}")
    (wt-nl sym-loc "=CDR(" sym-loc ");}")

    (c2expr body)
    (wt "}")
    )
  )

(defun c1psetq (old-args &aux (args nil) (use-psetf nil))
  (do (var (l old-args (cddr l)))
      ((endp l))
      (declare (object l))
      (setq var (car l))
      (cmpck (not (symbolp var))
             "The variable ~s is not a symbol." var)
      (cmpck (endp (cdr l))
             "No form was given for the value of ~s." var)
      (setq var (chk-symbol-macrolet var))
      (setq args (nconc args (list var (second l))))
      (if (symbolp var)
	(cmpck (constantp var)
	       "The constant ~s is being assigned a value." var)
	(setq use-psetf t)))
  (when use-psetf
    (return-from c1psetq (c1expr `(psetf ,@args))))
  (do ((l args (cddr l))
       (vrefs '())
       (forms '()))
      ((endp l)
       (make-c1form* 'PSETQ :type '(MEMBER NIL) :changed-vars vrefs
		     :args (reverse vrefs) (nreverse forms)))
      (let* ((vref (c1vref (first l)))
             (form (c1expr (second l)))
             (type (type-and (var-type vref) (c1form-type form))))
	(unless type
	  (cmpwarn "Type mismatch between ~s and ~s." name form)
	  (setq type T))
	;; Is this justified????
	#+nil(setf (c1form-type form) type)
	(push vref vrefs)
	(push form forms))))

(defun var-referred-in-forms (var forms)
  (let ((check-specials (member (var-kind var) '(SPECIAL GLOBAL))))
    (dolist (form forms nil)
      (when (or (member var (c1form-referred-vars form))
		(and check-specials (c1form-sp-change form)))
	(return-from var-referred-in-forms t)))))

(defun c2psetq (vrefs forms &aux (*lcl* *lcl*) (saves nil) (blocks 0))
  ;; similar to inline-args
  (do ((vrefs vrefs (cdr vrefs))
       (forms forms (cdr forms))
       (var) (form))
      ((null vrefs))
    (setq var (first vrefs)
	  form (car forms))
    (if (or (var-changed-in-forms var (cdr forms))
            (var-referred-in-forms var (cdr forms)))
        (case (c1form-name form)
          (LOCATION (push (cons var (c1form-arg 0 form)) saves))
          (otherwise
            (if (local var)
                (let* ((rep-type (var-rep-type var))
		       (rep-type-name (rep-type-name rep-type))
		       (temp (make-lcl-var :rep-type rep-type)))
                  (wt-nl "{" *volatile* rep-type-name " " temp ";")
                  (incf blocks)
                  (let ((*destination* temp)) (c2expr* form))
                  (push (cons var temp) saves))
                (let ((*destination* (make-temp-var)))
                  (c2expr* form)
                  (push (cons var *destination*) saves)))))
        (let ((*destination* var)) (c2expr* form))))
  (dolist (save saves) (set-var (cdr save) (car save)))
  (dotimes (i blocks) (wt "}"))
  (unwind-exit nil)
  )

;;; ----------------------------------------------------------------------

(put-sysprop 'VAR 'C2 'c2var)
(put-sysprop 'LOCATION 'C2 'c2location)
(put-sysprop 'SETQ 'c1special 'c1setq)
(put-sysprop 'SETQ 'C2 'c2setq)
(put-sysprop 'PROGV 'c1special 'c1progv)
(put-sysprop 'PROGV 'C2 'c2progv)
(put-sysprop 'PSETQ 'c1 'c1psetq)
(put-sysprop 'PSETQ 'C2 'c2psetq)
