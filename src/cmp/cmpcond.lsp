;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

(defun c1handler-bind (args &aux options)
  (check-args-number 'HANDLER-BIND args 1)
  (let* ((handlers (first args))
	 (types (mapcar #'first handlers))
	 (condition (c1make-var '.HANDLER-CONDITION. nil nil nil))
	 (functions (mapcar #'second handlers))
	 (body (c1progn (rest args))))
    (push-vars condition)
    (setf functions (mapcar #'(lambda (x) (c1expr `(funcall ,x .HANDLER-CONDITION.)))
			    functions))
    (check-vref condition)
    (make-c1form 'HANDLER-BIND (make-info :sp-change t)
		 types (add-object types) condition functions body)))

(defun c2handler-bind (types types-VV condition functions body)
  (wt-nl "if (frs_push(FRS_HANDLER," types-VV ") == 0) {")
  (wt-comment "begin HANDLER")
  (let ((*unwind-exit* (cons 'FRAME *unwind-exit*)))
    (c2expr body))
  (wt-nl "} else {")
  (bind (next-lcl) condition)
  (wt-nl "cl_object " condition "= VALUES(1);")
  (wt-nl "cl_fixnum code = fix(VALUES(0));")
  (wt-nl "frs_pop();")
  (wt-nl "switch (code) {")
  (do* ((i 0 (1+ i))
	(l functions (cdr l)))
       ((endp l))
    (if (endp (rest l))
	(wt-nl "default:")
	(wt-nl "case " i ":"))
    (wt-comment (format nil "handle ~A" (pop types)))
    (c2expr (first l))
    (wt-nl "break;"))
  (wt "}}")
  (wt-comment "end HANDLER"))

(put-sysprop 'HANDLER-BIND 'C1SPECIAL #'c1handler-bind)
(put-sysprop 'HANDLER-BIND 'C2 #'c2handler-bind)

