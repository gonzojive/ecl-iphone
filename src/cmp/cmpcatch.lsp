;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPCATCH  Catch, Unwind-protect, and Throw.

(in-package "COMPILER")

(defun c1catch (args &aux (info (make-info :sp-change t)) tag)
  (incf *setjmps*)
  (when (endp args) (too-few-args 'CATCH 1 0))
  (setq tag (c1expr (car args)))
  (add-info info (second tag))
  (setq args (c1progn (cdr args)))
  (add-info info (second args))
  (list 'CATCH info tag args))

(defun c2catch (tag body)
  (let* ((*lcl* *lcl*)
	 (tag-lcl (list 'LCL (next-lcl))))
    (wt-nl "{ cl_object " tag-lcl ";")
    (let* ((*destination* tag-lcl))
      (c2expr* tag))
    (let* ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
      (wt-nl "if (frs_push(FRS_CATCH," tag-lcl ")!=0){")
      (unwind-exit 'VALUES)
      (wt-nl "} else {")
      (c2expr body)
      (wt-nl "}}"))))

(defun c1unwind-protect (args &aux (info (make-info :sp-change t)) form)
  (incf *setjmps*)
  (when (endp args) (too-few-args 'UNWIND-PROTECT 1 0))
  (setq form (let ((*blocks* (cons 'UNWIND-PROTECT *blocks*))
                   (*tags* (cons 'UNWIND-PROTECT *tags*))
                   ;(*vars* (cons 'LB *vars*))
		   )
                  (c1expr (car args))))
  (add-info info (second form))
  (setq args (c1progn (cdr args)))
  (add-info info (second args))
  (list 'UNWIND-PROTECT info form args)
  )

(defun c2unwind-protect (form body &aux (nr (list 'LCL (next-lcl))))
  (wt-nl "{ volatile bool unwinding = FALSE;")
  (wt-nl "if (frs_push(FRS_PROTECT,Cnil)) {")
  (wt-nl "unwinding = TRUE;} else {")
  (let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
    (let ((*destination* 'VALUES)) (c2expr* form))
    (wt-nl "}")
    (wt-nl "MV_SAVE(" nr ");")
    (let ((*destination* 'TRASH)) (c2expr* body))
    (wt-nl "MV_RESTORE(" nr ");")
    (wt-nl "if (unwinding) unwind(nlj_fr,nlj_tag);")
    (wt-nl "else {")
    (unwind-exit 'VALUES)
    (wt "}}")))

(defun c1throw (args &aux (info (make-info)) tag)
  (when (or (endp args) (endp (cdr args)))
        (too-few-args 'THROW 2 (length args)))
  (unless (endp (cddr args))
          (too-many-args 'THROW 2 (length args)))
  (setq tag (c1expr (car args)))
  (add-info info (second tag))
  (setq args (c1expr (second args)))
  (add-info info (second args))
  (list 'THROW info tag args)
  )

(defun c2throw (tag val &aux loc)
  (case (car tag)
    (LOCATION (setq loc (third tag)))
    (VAR (setq loc (cons 'VAR (third tag))))
    (t (setq loc (list 'TEMP (next-temp)))
       (let ((*destination* loc)) (c2expr* tag))))
  (let ((*destination* 'VALUES)) (c2expr* val))
  (wt-nl "throw(" loc ");"))

;;; ----------------------------------------------------------------------

(setf (get 'CATCH 'C1SPECIAL) 'c1catch)
(setf (get 'CATCH 'C2) 'c2catch)
(setf (get 'UNWIND-PROTECT 'C1SPECIAL) 'c1unwind-protect)
(setf (get 'UNWIND-PROTECT 'C2) 'c2unwind-protect)
(setf (get 'THROW 'C1SPECIAL) 'c1throw)
(setf (get 'THROW 'C2) 'c2throw)
