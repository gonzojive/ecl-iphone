;;;;  Copyright (c) 2006, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPSTACK Manipulation of the lisp stack from C code
;;;;
;;;; Following special forms are provided:
;;;;
;;;;	(WITH-STACK {form}*)
;;;;		Executes given forms, restoring the lisp stack on output.
;;;;	(STACK-PUSH form)
;;;;	(STACK-PUSH-VALUES form)
;;;;	(STACK-POP nvalues)
;;;;

(in-package "COMPILER")

(defun c1with-stack (forms)
  (let ((body (c1expr `(progn ,@forms))))
    (make-c1form* 'WITH-STACK :type (c1form-type body)
		  :args body)))

(defun c2with-stack (body)
  (let* ((new-destination (tmp-destination *destination*))
	 (*temp* *temp*)
	 (sp (make-lcl-var :rep-type :cl-index)))
    (wt-nl "{cl_index " sp "=cl_stack_index();")
    (let* ((*destination* new-destination)
	   (*unwind-exit* `((STACK ,sp) ,@*unwind-exit*)))
      (c2expr* body))
    (wt-nl "cl_stack_set_index(" sp ");}")
    (unwind-exit new-destination)))

(defun c1stack-push (args)
  (c1expr `(progn
	     (c-inline ,args (t) :void "cl_stack_push(#0)"
		       :one-liner t :side-effects t)
	     1)))

(defun c1stack-push-values (args)
  (make-c1form* 'STACK-PUSH-VALUES :type 'fixnum
		:args (c1expr (first args))
		(c1expr `(c-inline () () fixnum "cl_stack_push_values()"
				   :one-liner t :side-effects t))))

(defun c2stack-push-values (form push-statement)
  (let ((*destination* 'VALUES))
    (c2expr* form))
  (c2expr push-statement))

(defun c1stack-pop (args)
  (let ((action (c1expr `(c-inline ,args (fixnum) :void
				  "cl_stack_pop_values(#0)"
				  :one-liner t
				  :side-effects t))))
    (make-c1form* 'STACK-POP :type t :args action)))

(defun c2stack-pop (action)
  (let ((*destination* 'TRASH))
    (c2expr* action))
  (unwind-exit 'VALUES))

(defun c1apply-from-stack (args)
  (c1expr `(c-inline ,args (fixnum t) (values &rest t) "cl_apply_from_stack(#0,#1);"
		     :one-liner nil :side-effects t)))

(put-sysprop 'with-stack 'C1 #'c1with-stack)
(put-sysprop 'with-stack 'c2 #'c2with-stack)
(put-sysprop 'stack-push 'C1 #'c1stack-push)
(put-sysprop 'stack-push-values 'C1 #'c1stack-push-values)
(put-sysprop 'stack-push-values 'C2 #'c2stack-push-values)
(put-sysprop 'stack-pop 'C1 #'c1stack-pop)
(put-sysprop 'stack-pop 'C2 #'c2stack-pop)
(put-sysprop 'apply-from-stack 'c1 #'c1apply-from-stack)