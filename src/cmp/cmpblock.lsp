;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPBLOCK  Block and Return-from.

(in-package "COMPILER")

#|
;;; Use a structure of type vector to avoid creating
;;; normal structures before booting CLOS:
(defstruct (blk (:type vector) :named)
   name			;;; Block name.
   (ref 0 :type fixnum)	;;; Number of references.
   ref-ccb		;;; Cross closure reference.
			;;; During Pass1, T or NIL.
   ref-clb		;;; Cross local function reference.
       			;;; During Pass1, T or NIL.
       			;;; block id, or NIL.
   exit			;;; Where to return.  A label.
   destination		;;; Where the value of the block to go.
   var			;;; variable containing the block ID.
) |#

;;; During Pass 1, *blocks* holds a list of blk objects and the
;;; symbols 'CB' (Closure Boundary), 'LB' (Level Boundary) or
;;; 'UNWIND-PROTECT'.  'CB' will be pushed on *blocks* when the
;;; compiler begins to process a closure.  'LB' will be pushed on
;;; *blocks* when *level* is incremented.  'UNWIND-PROTECT' is pushed
;;; when entering an unwind-protect.  A dummy variable is created to
;;; hold the block identifier.  When a reference to the block (via
;;; return-from) is found, the var-ref count for that variable is
;;; incremented only if the reference appears across a boundary (CB,
;;; LB or UNWIND-PROTECT), while the blk-ref is always incremented.
;;; Therefore blk-ref represents whether the block is used at all and
;;; var-ref for the dummy variable represents whether a block
;;; identifier must be created and stored in such variable.

(defun c1block (args)
  (check-args-number 'BLOCK args 1)
  (let ((block-name (first args)))
    (unless (symbolp block-name)
      (cmperr "The block name ~s is not a symbol." block-name))
    (let* ((blk (make-blk :var (make-var :name block-name :kind 'LEXICAL)
			  :name block-name))
	   (*blocks* (cons blk *blocks*))
	   (body (c1progn (rest args))))
      (when (or (blk-ref-ccb blk) (blk-ref-clb blk))
	(incf *setjmps*))
      (if (plusp (blk-ref blk))
	  ;; FIXME! By simplifying the type of a BLOCK form so much (it is
	  ;; either NIL or T), we lose a lot of information.
	  (make-c1form* 'BLOCK :type (type-or (blk-type blk) (c1form-type body))
			:args blk body)
	  body))))

(defun c2block (blk body)
  (if (plusp (var-ref (blk-var blk)))
      (let* ((blk-var (blk-var blk))
	     (*env-lvl* *env-lvl*))
	(setf (blk-exit blk) *exit*
	      (blk-destination blk) *destination*)
	(wt "{")
	(unless (or (blk-ref-ccb blk) (blk-ref-clb blk))
	  (setf (var-kind blk-var) :object
		(var-loc blk-var) (next-lcl))
	  (wt " cl_object " blk-var ";"))
	(when (env-grows (blk-ref-ccb blk))
	  (let ((env-lvl *env-lvl*))
	    (wt-nl "volatile cl_object env" (incf *env-lvl*)
		   " = env" env-lvl ";")))
	(bind "new_frame_id()" blk-var)
	(wt-nl "if (frs_push(FRS_CATCH," blk-var ")!=0) {")
	(let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
	  (unwind-exit 'VALUES)
	  (wt-nl "} else {")
	  (c2expr body)
	  (wt "}"))
	(when (blk-ref-ccb blk) (decf *env*))
	(wt-nl "}"))
      (progn
	(setf (blk-exit blk) *exit*)
	(setf (blk-destination blk) *destination*)
	(c2expr body)))
  )

(defun c1return-from (args)
  (check-args-number 'RETURN-FROM args 1 2)
  (unless (symbolp (car args))
    (cmperr "The block name ~s is not a symbol." (car args)))
  (do ((blks *blocks* (cdr blks))
       (name (car args))
       (ccb) (clb) (unw) (blk) (type T))
      ((endp blks)
       (cmperr "The block ~s is undefined." name))
    (setq blk (car blks))
    (case blk
      (CB (setq ccb t))
      (LB (setq clb t))
      (UNWIND-PROTECT (setq unw T))
      (t (when (eq (blk-name blk) name)
	   (let* ((val (c1expr (second args)))
		  (var (blk-var blk)))
             (cond (ccb (setf (blk-ref-ccb blk) t
			      type 'CCB
			      (var-ref-ccb var) T)
			(incf (var-ref var)))
		   (clb (setf (blk-ref-clb blk) t
			      type 'CLB)
			(incf (var-ref var)))
		   (unw (setf type 'UNWIND-PROTECT)
			(incf (var-ref var))))
	     (incf (blk-ref blk))
	     (setf (blk-type blk) (type-or (blk-type blk) (c1form-type val)))
             (return (make-c1form* 'RETURN-FROM
				   :local-referred (list var)
				   ;;:referred-tags (list blk)
				   :referred-vars (list var)
				   :type (and (c1form-type val) 'T)
				   :args blk type val))))))))

(defun c2return-from (blk type val)
  (case type
    (CCB
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "cl_return_from(" (blk-var blk) "," (add-symbol (blk-name blk)) ");"))
    ((CLB UNWIND-PROTECT)
     (let ((*destination* 'VALUES)) (c2expr* val))
     (wt-nl "cl_return_from(" (blk-var blk) ",Cnil);"))
    (T (let ((*destination* (blk-destination blk))
	     (*exit* (blk-exit blk)))
	 (c2expr val))))
  )

;;; ----------------------------------------------------------------------

(put-sysprop 'BLOCK 'C1SPECIAL 'c1block)
(put-sysprop 'BLOCK 'C2 'c2block)

(put-sysprop 'RETURN-FROM 'C1SPECIAL 'c1return-from)
(put-sysprop 'RETURN-FROM 'C2 'c2return-from)
