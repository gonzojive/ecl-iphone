;;;;  CMPLET  Let and Let*.
;;;;
;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    ECL is free software; you can redistribute it and/or modify it
;;;;    under the terms of the GNU Library General Public License as
;;;;    published by the Free Software Foundation; either version 2 of
;;;;    the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1let (args &aux	(setjmps *setjmps*)
                        (forms nil) (vars nil) (vnames nil)
                        ss is ts body other-decls
                        (*vars* *vars*))
  (check-args-number 'LET args 1)

  (multiple-value-setq (body ss ts is other-decls) (c1body (cdr args) nil))

  (c1add-globals ss)

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts)))
                (push x vnames)
                (push v vars)
                (push (default-init v) forms)))
          (t (cmpck (not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
                    "The variable binding ~s is illegal." x)
             (let* ((vname (car x))
		    (v (c1make-var vname ss is ts))
		    (form (if (endp (cdr x))
                            (default-init v)
                            (and-form-type (var-type v)
                                           (c1expr (second x))
                                           (second x)
					   :unsafe
					   "In LET bindings"))))
	       ;; :read-only variable handling. Beppe
;	       (when (read-only-variable-p vname ts)
;		     (setf (var-type v) (c1form-type form)))
	       (push vname vnames)
	       (push v vars)
	       (push form forms)))))

  (setf vars (nreverse vars) forms (nreverse forms))

  (mapc #'push-vars vars)

  (check-vdecl vnames ts is)

  (setq body (c1decl-body other-decls body))

  ;; since the body may produce type constraints on variables:
  ;; (let (x) (foo x)) where (type (function (fixnum) fixnum) foo)
  ;; do it again
  (do ((vars vars (cdr vars))
       (forms forms (cdr forms))
       (all-vars vars)
       (used-vars '())
       (used-forms '()))
      ((null vars)
       (make-c1form* 'LET :type (c1form-type body)
		     :volatile (not (eql setjmps *setjmps*))
		     :args (nreverse used-vars) (nreverse used-forms) body))
    (let* ((var (first vars))
	   (form (and-form-type (var-type var) (first forms) (var-name var)
				:unsafe "In LET body"))
	   (form-type (c1form-type form)))
      (declare (type var var))
      ;; Automatic treatement for READ-ONLY variables:
      (unless (var-changed-in-forms var (list body))
	(setf (var-type var) form-type)
	(update-var-type var form-type body)
	;; * (let ((v2 e2)) e3 e4) => (let () e3 e4)
	;;   provided
	;;   - v2 does not appear in body
	;;   - e2 produces no side effects
	(when (and (= 0 (var-ref var))
		   (not (member (var-kind var) '(special global)))
		   (not (form-causes-side-effect form)))
	  (go continue))
	;;  (let ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
	;;  can become
	;;  (let ((v1 e1) (v3 e3)) (expr e4 e2 e5))
	;;  provided
	;;  - v2 appears only once
	;;  - v2 appears only in body
	;;  - e2 does not affect v1 nor e3, e3 does not affect e2
	;;  - e4 does not affect e2
	(when (and (= 1 (var-ref var))
		   (member-var var (c1form-referred-vars body))
		   ;; it does not refer to special variables which
		   ;; are changed in the LET form
		   (dolist (v all-vars t)
		     (when (member-var v (c1form-referred-vars form))
		       (return nil)))
		   (catch var
		     (replaceable var body)))
	  (unless (nsubst-var var form body)
 	    (baboon))
	  (go continue))
	)
      #+nil
      (when (member-type form-type '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT))
	(incf (var-ref var)))		; force unboxing
      (check-vref var)
      (push var used-vars)
      (push form used-forms))
    continue))

(defun update-var-type (var type x)
  (when (listp x)
    (if (and (eq (car x) 'VAR)
	     (eq var (third x)))
	(setf (c1form-type x)
	      ;; some occurrences might be typed with 'the'
	      (type-and (c1form-type x) type))
	(dolist (e x)
	  (update-var-type var type e)))))

;(defun read-only-variable-p (v l) (eq 'READ-ONLY (cdr (assoc v l))))

(defun c2let (vars forms body
                   &aux (block-p nil) (bindings nil)
                   initials
                   (*unwind-exit* *unwind-exit*)
		   (*env* *env*)
                   (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  ;; Allocation is needed for:
  ;; 1. each variable which is LOCAL and which is not REPLACED
  ;;    or whose value is not DISCARDED
  ;; 2. each init form for the remaining variables except last

  ;; Determine which variables are really necessary and create list of init's
  ;; and list of bindings. Bindings for specials must be done after all inits.
  (labels ((do-decl (var)
	     (declare (type var var))
	     (wt-nl)
	     (unless block-p
	       (wt "{") (setq block-p t))
	     (wt *volatile* (register var) (rep-type-name (var-rep-type var)) " "
		 var ";")
	     (when (local var)
	       (wt-comment (var-name var))))
	   (do-init (var form fl)
	     (if (and (local var)
		      (not (args-cause-side-effect (cdr fl))))
		 ;; avoid creating temporary for init
		 (push (cons var form) initials)
		 (let* ((loc (make-lcl-var :rep-type (var-rep-type var)
					   :type (var-type var))))
		   (do-decl loc)
		   (push (cons loc form) initials)
		   (push (cons var loc) bindings)))))

    (do ((vl vars (rest vl))
         (fl forms (rest fl))
         (form) (var)
         (prev-ss nil) (used t t))
        ((endp vl))
      (declare (type var var))
      (setq form (first fl)
            var (first vl))
      (when (and (local var)
		 (setq used (not (discarded var form body))))
	(setf (var-loc var) (next-lcl))
	(do-decl var))
      (when used
	(if (unboxed var)
	    (push (cons var form) initials)	; nil (ccb)
	    ;; LEXICAL, SPECIAL, GLOBAL or :OBJECT
	    (case (c1form-name form)
	      (LOCATION
	       (if (can-be-replaced var body)
		   (setf (var-kind var) 'REPLACED
			 (var-loc var) (third form))
		   (push (cons var (third form)) bindings)))
	      (VAR
	       (let* ((var1 (third form)))
		 (cond ((or (var-changed-in-forms var1 (cdr fl))
			    (and (member (var-kind var1) '(SPECIAL GLOBAL))
				 (member (var-name var1) prev-ss)))
			(do-init var form fl))
		       ((and (can-be-replaced var body)
			     (member (var-kind var1) '(LEXICAL REPLACED :OBJECT))
			     (not (var-ref-ccb var1))
			     (not (member var1 (c1form-changed-vars body))))
			(setf (var-kind var) 'REPLACED
			      (var-loc var) var1))
		       (t (push (cons var var1) bindings)))))
	      (t (do-init var form fl))))
	(unless env-grows
	  (setq env-grows (var-ref-ccb var))))
      (when (eq (var-kind var) 'SPECIAL) (push (var-name var) prev-ss))))

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "volatile cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))

  ;; eval INITFORM's and bind variables
  (dolist (init (nreverse initials))
    (let ((*destination* (car init))
	  (*lcl* *lcl*))
      (c2expr* (cdr init))))
  ;; bind LET variables
  (dolist (binding (nreverse bindings))
    (bind (cdr binding) (car binding)))

  (c2expr body)
  (when block-p (wt-nl "}"))
  )

(defun env-grows (possibily)
  ;; if additional closure variables are introduced and this is not
  ;; last form, we must use a new env.
  (and possibily
       (plusp *env*)
       (dolist (exit *unwind-exit*)
	 (case exit
	   (RETURN (return NIL))
	   (BDS-BIND)
	   (t (return T))))))

(defun c1let* (args &aux (forms nil) (vars nil) (vnames nil)
                    (setjmps *setjmps*)
                    ss is ts body other-decls
                    (*vars* *vars*))
  (check-args-number 'LET* args 1)

  (multiple-value-setq (body ss ts is other-decls) (c1body (cdr args) nil))
  (c1add-globals ss)

  (dolist (x (car args))
    (cond ((symbolp x)
           (let ((v (c1make-var x ss is ts)))
	     (push x vnames)
	     (push (default-init v) forms)
	     (push v vars)
	     (push-vars v)))
          ((not (and (consp x) (or (endp (cdr x)) (endp (cddr x)))))
           (cmperr "The variable binding ~s is illegal." x))
          (t (let* ((v (c1make-var (car x) ss is ts))
		    (form (if (endp (cdr x))
			      (default-init v)
			      (and-form-type (var-type v)
					     (c1expr (second x))
					     (second x)
					     :unsafe
					     "In LET* bindings"))))
	       ;; :read-only variable handling.
;	       (when (read-only-variable-p (car x) ts)
;		     (setf (var-type v) (c1form-type form)))
	       (push (car x) vnames)
	       (push form forms)
	       (push v vars)
	       (push-vars v)))))

  (check-vdecl vnames ts is)
  (setq body (c1decl-body other-decls body))

  ;; since the body may produce type constraints on variables,
  ;; do it again:
  (do ((vs (setq vars (nreverse vars)) (cdr vs))
       (fs (nreverse forms) (cdr fs))
       (used-vars '())
       (used-forms '()))
      ((null vs)
       (make-c1form* 'LET* :type (c1form-type body)
		     :volatile (not (eql setjmps *setjmps*))
		     :args (nreverse used-vars) (nreverse used-forms) body))
    (let* ((var (first vs))
	   (form (and-form-type (var-type var) (car fs) (cadar args)
				:unsafe "~&;;; In LET* body"))
	   (form-type (c1form-type form))
	   (rest-forms (cons body (rest fs))))
      ;; Automatic treatement for READ-ONLY variables:
      (unless (var-changed-in-forms var rest-forms)
	(setf (var-type var) form-type)
	(update-var-type var form-type rest-forms)
	;; * (let* ((v2 e2)) e3 e4) => (let () e3 e4)
	;;   provided
	;;   - v2 does not appear in body
	;;   - e2 produces no side effects
	(when (and (= 0 (var-ref var))
		   (not (member (var-kind var) '(SPECIAL GLOBAL)))
		   (not (form-causes-side-effect form)))
	  (go continue))
	;;  (let* ((v1 e1) (v2 e2) (v3 e3)) (expr e4 v2 e5))
	;;  can become
	;;  (let* ((v1 e1) (v3 e3)) (expr e4 e2 e5))
	;;  provided
	;;  - v2 appears only once
	;;  - v2 appears only in body
	;;  - e2 does not affect v1 nor e3, e3 does not affect e2
	;;  - e4 does not affect e2
	(when (and (= 1 (var-ref var))
		   (member-var var (c1form-referred-vars body))
		   ;; it does not refer to special variables which
		   ;; are changed in later assignments
		   (dolist (v (rest vs) t)
		     (when (member-var v (c1form-referred-vars form))
		       (return nil)))
		   (or (and (null (rest vs))	; last variable
			    ;; its form does not affect previous variables
			    (let ((tforms (list form)))
			      (dolist (v vars)
				(when (eq v var) (return t))
				(when (var-changed-in-forms v tforms)
				  (return nil)))))
		       (not (args-cause-side-effect fs)))
		   (catch var
		     (replaceable var body)))
	  (unless (nsubst-var var form body)
 	    (baboon))
	  (go continue))
	)
      #+nil
      ;; Force unboxing
      (when (member-type (c1form-type form)
			 '(FIXNUM CHARACTER LONG-FLOAT SHORT-FLOAT))
	(incf (var-ref var)))
      (check-vref var)
      (push var used-vars)
      (push form used-forms))
    continue))

;; should check whether a form before var causes a side-effect
;; exactly one occurrence of var is present in forms
(defun replaceable (var form &aux (args (c1form-args form)))
  (case (c1form-name form)
    (VAR
     (if (eq var (first args))
	 (throw var T)
	 T))
    ((LOCATION SYS:STRUCTURE-REF) T)
    (CALL-GLOBAL
     (dolist (subform (second args) T)
       (when (or (not (replaceable var subform))
		 (form-causes-side-effect subform))
	 (return nil))))
    (SETQ (replaceable var (second args)))))

(defun c2let* (vars forms body
                    &aux (block-p nil)
                    (*unwind-exit* *unwind-exit*)
		    (*env* *env*)
		    (*env-lvl* *env-lvl*) env-grows)
  (declare (type boolean block-p))

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl))
       (var) (form) (kind))
      ((endp vl))
    (declare (type var var))
    (setq form (car fl)
          var (car vl)
          kind (local var))
    (unless (unboxed var)
      ;; LEXICAL, SPECIAL, GLOBAL or OBJECT
      (case (c1form-name form)
        (LOCATION
         (when (can-be-replaced* var body (cdr fl))
           (setf (var-kind var) 'REPLACED
                 (var-loc var) (third form))))
        (VAR
         (let* ((var1 (third form)))
           (declare (type var var1))
           (when (and (can-be-replaced* var body (cdr fl))
		      (member (var-kind var1) '(LEXICAL REPLACED :OBJECT))
		      (not (var-ref-ccb var1))
		      (not (var-changed-in-forms var1 (cdr fl)))
		      (not (member var1 (c1form-changed-vars body))))
             (setf (var-kind var) 'REPLACED
                   (var-loc var) var1)))))
      (unless env-grows
	(setq env-grows (var-ref-ccb var))))
    (when (and kind (not (eq (var-kind var) 'REPLACED)))
      (bind (next-lcl) var)
      (wt-nl) (unless block-p (wt "{") (setq block-p t))
      (wt *volatile* (register var) (rep-type-name kind) " " var ";")
      (wt-comment (var-name var)))
    )

  (when (env-grows env-grows)
    (unless block-p
      (wt-nl "{ ") (setq block-p t))
    (let ((env-lvl *env-lvl*))
      (wt "volatile cl_object env" (incf *env-lvl*) " = env" env-lvl ";")))

  (do ((vl vars (cdr vl))
       (fl forms (cdr fl))
       (var nil) (form nil))
      ((null vl))
    (declare (type var var))
    (setq var (car vl)
	  form (car fl))
    (case (var-kind var)
      (REPLACED)
      ((LEXICAL SPECIAL GLOBAL)
       (case (c1form-name form)
	 (LOCATION (bind (c1form-arg 0 form) var))
	 (VAR (bind (c1form-arg 0 form) var))
	 (t (bind-init var form))))
      (t ; local var
       (let ((*destination* var)) ; nil (ccb)
	 (c2expr* form)))
      )
    )
  (c2expr body)

  (when block-p (wt-nl "}"))
  )

(defun discarded (var form body &aux last)
  (labels ((last-form (x &aux (args (c1form-args x)))
	     (case (c1form-name x)
	       (PROGN
		 (last-form (car (last (first args)))))
	       ((LET LET* FLET LABELS BLOCK CATCH)
		(last-form (car (last args))))
	       (VAR (first x))
	       (t x))))
    (and (not (form-causes-side-effect form))
	 (or (< (var-ref var) 1)
	     (and (= (var-ref var) 1)
		  (eq var (last-form body))
		  (eq 'TRASH *destination*))))))

(defun can-be-replaced (var body)
  (declare (type var var))
  (and (eq (var-kind var) :OBJECT)
       (< (var-ref var) *register-min*)
       (not (member var (c1form-changed-vars body)))))
#|  (and (or (eq (var-kind var) 'LEXICAL)
	   (and (eq (var-kind var) :OBJECT)
		(< (var-ref var) *register-min*)))
       (not (var-ref-ccb var))
       (not (member var (c1form-changed-vars body))))
|#

(defun can-be-replaced* (var body forms)
  (declare (type var var))
  (and (can-be-replaced var body)
       (dolist (form forms t)
         (when (member var (c1form-changed-vars form))
               (return nil)))))

(defun nsubst-var (var form where)
  (cond ((null where)
	 nil)
	((c1form-p where)
	 (cond ((not (member var (c1form-referred-vars where)))
		nil)
	       ((and (eql (c1form-name where) 'VAR)
		     (eql (c1form-arg 0 where) var))
		(setf (c1form-changed-vars where) (c1form-changed-vars form)
		      (c1form-referred-vars where) (c1form-referred-vars form)
		      (c1form-type where) (c1form-type form)
		      (c1form-sp-change where) (c1form-sp-change form)
		      (c1form-volatile where) (c1form-volatile form)
		      (c1form-local-referred where) (c1form-local-referred form)
		      (c1form-name where) (c1form-name form)
		      (c1form-args where) (c1form-args form))
		t)
	       ((nsubst-var var form (c1form-args where))
		(c1form-add-info1 where form)
		(setf (c1form-referred-vars where)
		      (delete var (c1form-referred-vars where))
		      (c1form-local-referred where)
		      (delete var (c1form-local-referred where)))
		t)))
	((atom where)
	 nil)
	(t
	 (let ((output NIL))
	   (dolist (subform where)
	     (when (nsubst-var var form subform)
	       (setf output T)))
	   output))))

(defun member-var (var list)
  (let ((kind (var-kind var)))
    (if (member kind '(SPECIAL GLOBAL))
	(member var list :test
		#'(lambda (v1 v2)
		    (and (member (var-kind v2) '(SPECIAL GLOBAL))
			 (eql (var-name v1) (var-name v2)))))
	(member var list))))

;;; ----------------------------------------------------------------------

(put-sysprop 'LET 'C1SPECIAL #'c1let)
(put-sysprop 'LET 'C2 'c2let)
(put-sysprop 'LET* 'C1SPECIAL #'c1let*)
(put-sysprop 'LET* 'C2 'c2let*)
