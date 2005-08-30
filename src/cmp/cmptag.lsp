;;;;  CMPTAG  --  Tagbody and Go.

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

;;; During Pass 1, *tags* holds a list of tag objects and the symbols 'CB'
;;; (Closure Boundary), 'LB' (Level Boundary) or 'UNWIND-PROTECT'.
;;; 'CB' will be pushed on *tags* when the compiler begins to process
;;;  a closure.
;;; 'LB' will be pushed on *tags* when *level* is incremented.
;;; 'UNWIND-PROTECT' is pushed when entering an unwind-protect.
;;;  A dummy variable is created to hold the tag identifier and one tag
;;;  structure (containing reference to such variable) is created for each
;;;  label in the body.
;;;  When a reference to a tag (go instruction) is found, the
;;;  var-kind is stepped from NIL to OBJECT (if appearing inside an
;;;  unwind-protect) to LEXICAL or CLOSURE (if appearing across a boundary).
;;;  The tag-ref is also incremented.
;;;  Therefore var-ref represents whether some tag is used at all and var-kind
;;;  variable represents whether a tag identifier must be created and the
;;;  kind of the dummy variable to store it.


(defvar *reg-amount* 60)
;;; amount to increase var-ref for each variable reference inside a loop

(defun add-loop-registers (tagbody)
  ;; Find a maximal iteration interval in TAGBODY from first to end
  ;; then increment the var-ref slot.
  (labels ((add-reg1 (form)
	     ;; increase the var-ref in FORM for all vars
	     (cond ((c1form-p form)
		    (dolist (v (c1form-args form))
		      (add-reg1 v)))
		   ((consp form)
		    (dolist (v form)
		      (add-reg1 v)))
		   ((var-p form)
		    (incf (var-ref form) (the fixnum *reg-amount*)))))
	   (jumps-to-p (clause tag-name)
	     ;; Does CLAUSE have a go TAG-NAME in it?
	     (cond ((c1form-p clause)
		    (and (eq (c1form-name clause) 'GO)
			 (eq (tag-name (c1form-arg 0 clause)) tag-name)))
		   ((atom clause) nil)
		   (t (or (jumps-to-p (car clause) tag-name)
			  (jumps-to-p (cdr clause) tag-name))))))
    (do ((v tagbody (cdr v))
	 (end nil)
	 (first nil))
	((null v)
	 (do ((ww first (cdr ww)))
	     ((eq ww end) (add-reg1 (car ww)))
	   (add-reg1 (car ww))))
      (when (tag-p (car v))
	(unless first (setq first v))
	(do ((w (cdr v) (cdr w))
	     (name (tag-name (car v))))
	    ((null w))
	  (when (jumps-to-p (car w) name)
	    (setq end w)))))))

;; FIXME! The variable name should not be a usable one!
(defun c1tagbody (body &aux (*tags* *tags*)
                       (tag-var (make-var :name 'TAGBODY :kind NIL))
		       (tag-index 0))
  ;;; Establish tags.
  (setq body
        (mapcar
         #'(lambda (x)
             (if (not (consp x))
               (let ((tag (make-tag :name x :var tag-var :index tag-index)))
                 (push tag *tags*)
		 (incf tag-index)
                 tag)
               x))
         body))

  ;;; Process non-tag forms.
  (setq body (mapcar #'(lambda (x) (if (tag-p x) x (c1expr x))) body))

  ;;; Delete redundant tags.
  (let ((body1 nil) (ref nil))
    (dolist (form body)
      (if (tag-p form)
	  (when (plusp (tag-ref form))
	    (push form body1))
	  (push form body1)))
    (if (plusp (var-ref tag-var))
	(progn (setq body1 (nreverse body1))
	       (when (var-ref-ccb tag-var)
		 (incf *setjmps*))
	       (add-loop-registers body1)
	       (make-c1form* 'TAGBODY :local-vars (list tag-var)
			     :args tag-var body1))
	(make-c1form* 'PROGN :args (nreverse (cons (c1nil) body1))))))

(defun c2tagbody (tag-loc body)
  (declare (type var tag-loc))
  (if (null (var-kind tag-loc))
      ;; only local goto's
      (let ((label (next-label)))
	;; Allocate labels.
	(dolist (x body)
	  (when (and (tag-p x) (plusp (tag-ref x)))
	    (setf (tag-label x) (next-label*))
	    (setf (tag-unwind-exit x) label)))
	(let ((*unwind-exit* (cons label *unwind-exit*)))
	  (c2tagbody-body body)))
      ;; some tag used non locally or inside an unwind-protect
      (let ((*unwind-exit* (cons 'FRAME *unwind-exit*))
	    (label (next-label))
	    (*env* *env*) (*env-lvl* *env-lvl*)
	    (*lex* *lex*) (*lcl* *lcl*)
	    (env-grows (env-grows (var-ref-ccb tag-loc))))
	(when env-grows
	  (let ((env-lvl *env-lvl*))
	    (wt-nl "{ volatile cl_object env" (incf *env-lvl*)
		   " = env" env-lvl ";")))
	(when (eq :OBJECT (var-kind tag-loc))
	  (setf (var-loc tag-loc) (next-lcl))
	  (wt-nl "{ cl_object " tag-loc ";")
	  (setq env-grows t))		; just to ensure closing the block
	(bind "new_frame_id()" tag-loc)
	(wt-nl "if (frs_push(" tag-loc ")) {")
	;; Allocate labels.
	(dolist (tag body)
	  (when (and (tag-p tag) (plusp (tag-ref tag)))
	    (setf (tag-label tag) (next-label))
	    (setf (tag-unwind-exit tag) label)
	    (wt-nl "if (VALUES(0)==MAKE_FIXNUM(" (tag-index tag) "))")
	    (wt-go (tag-label tag))))
	(when (var-ref-ccb tag-loc)
	  (wt-nl "internal_error(\"GO found an inexistent tag\");"))
	(wt-nl "}")
	(let ((*unwind-exit* (cons label *unwind-exit*)))
	  (c2tagbody-body body))
	(when env-grows (wt "}"))))
  )

(defun c2tagbody-body (body)
  (do ((l body (cdr l)) (written nil))
      ((endp (cdr l))
       (cond (written (unwind-exit nil))
             ((tag-p (car l))
              (wt-label (tag-label (car l)))
              (unwind-exit nil))
             (t (let* ((*exit* (next-label))
                       (*unwind-exit* (cons *exit* *unwind-exit*))
                       (*destination* 'TRASH))
                  (c2expr (car l))
                  (wt-label *exit*))
                (unless (eq (c1form-name (first l)) 'GO)
		  (unwind-exit nil)))))
    (let ((this-form (first l)))
      (cond (written (setq written nil))
	    ((tag-p this-form) (wt-label (tag-label this-form)))
	    (t (let* ((next-form (second l))
		      (*exit* (if (tag-p next-form)
				  (progn (setq written t) (tag-label next-form))
				  (next-label)))
		      (*unwind-exit* (cons *exit* *unwind-exit*))
		      (*destination* 'TRASH))
		 (c2expr this-form)
		 (wt-label *exit*)))))))

(defun c1go (args)
  (check-args-number 'GO args 1 1)
  (unless (or (symbolp (car args)) (integerp (car args)))
    (cmperr "The tag name ~s is not a symbol nor an integer." (car args)))
  (do ((tags *tags* (cdr tags))
       (name (car args))
       (ccb) (clb) (unw) (tag) (var))
      ((endp tags) (cmperr "The tag ~s is undefined." name))
    (declare (type var var))
    (setq tag (car tags))
    (case tag
      (CB (setq ccb t))
      (LB (setq clb t))
      (UNWIND-PROTECT (setq unw T))
      (T (when (eql (tag-name tag) name)
	   (setq var (tag-var tag))
	   (cond (ccb (setf (tag-ref-ccb tag) t
			    (var-ref-ccb var) T
			    (var-kind var) 'CLOSURE))
		 (clb (setf (tag-ref-clb tag) t
			    (var-ref-clb var) t
			    (var-kind var) 'LEXICAL))
		 (unw (unless (var-kind var)
			(setf (var-kind var) :OBJECT))))
	   (incf (var-ref var))
	   (incf (tag-ref tag))
	   (return (add-to-read-nodes var (make-c1form* 'GO :args tag
							(or ccb clb unw))))
	   )))))

(defun c2go (tag nonlocal)
  (if nonlocal
      (let ((var (tag-var tag)))
	(wt-nl "cl_go(" var ",MAKE_FIXNUM(" (tag-index tag) "));"))
      ;; local go
      (progn
	(unwind-no-exit (tag-unwind-exit tag))
	(wt-nl) (wt-go (tag-label tag)))))

;;; ------------------------------------------------------------

(put-sysprop 'tagbody 'c1special 'c1tagbody)
(put-sysprop 'tagbody 'c2 'c2tagbody)

(put-sysprop 'go 'c1special 'c1go)
(put-sysprop 'go 'c2 'c2go)
