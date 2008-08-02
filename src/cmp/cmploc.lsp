;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
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

;;;; CMPLOC  Set-loc and Wt-loc.

(in-package "COMPILER")

;;; Valid locations are:
;;;	NIL
;;;	T
;;;	fixnum
;;;	VALUE0
;;;	VALUES
;;;	var-object
;;;	( VALUE i )			VALUES(i)
;;;	( VV vv-index )
;;;	( VV-temp vv-index )
;;;	( LCL lcl [representation-type]) local variable, type unboxed
;;;	( TEMP temp )			local variable, type object
;;;	( FRAME ndx )			variable in local frame stack
;;;	( CALL c-fun-name args fname )	locs are locations containing the arguments
;;;	( CALL-NORMAL fun locs)		similar as CALL, but number of arguments is fixed
;;;	( C-INLINE output-type fun/string locs side-effects output-var )
;;;	( COERCE-LOC representation-type location)
;;;	( CAR lcl )
;;;	( CDR lcl )
;;;	( CADR lcl )
;;;	( FDEFINITION vv-index )
;;;	( MAKE-CCLOSURE cfun )
;;;	( FIXNUM-VALUE fixnum-value )
;;;	( CHARACTER-VALUE character-code )
;;;	( LONG-FLOAT-VALUE long-float-value vv )
;;;	( DOUBLE-FLOAT-VALUE double-float-value vv )
;;;	( SINGLE-FLOAT-VALUE single-float-value vv )
;;;	( STACK-POINTER index )	retrieve a value from the stack
;;;	( SYS:STRUCTURE-REF loc slot-name-vv slot-index )
;;;	( KEYVARS n )
;;;	VA-ARG
;;;	CL-VA-ARG

;;; Valid *DESTINATION* locations are:
;;;
;;;	VALUE0
;;;	RETURN				Object returned from current function.
;;;	TRASH				Value may be thrown away.
;;;	VALUES				Values vector.
;;;	var-object
;;;	( LCL lcl )
;;;	( LEX lex-address )
;;;	( BIND var alternative )	Alternative is optional
;;;	( JUMP-TRUE label )
;;;	( JUMP-FALSE label )

(defun tmp-destination (loc)
  (case loc
    (VALUES 'VALUES)
    (TRASH 'TRASH)
    (T 'RETURN)))

(defun set-loc (loc &aux fd
		    (is-call (and (consp loc)
				  (member (car loc) '(CALL CALL-NORMAL)  :test #'eq))))
  (when (eql *destination* loc)
    (return-from set-loc))
  (case *destination*
    (VALUES
     (cond (is-call
	    (wt-nl "VALUES(0)=") (wt-coerce-loc :object loc) (wt ";"))
	   ((eq loc 'VALUES) (return-from set-loc))
	   (t
	    (wt-nl "VALUES(0)=") (wt-coerce-loc :object loc) (wt "; NVALUES=1;"))))
    (VALUE0
     (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";"))
    (RETURN
     (cond ((or is-call (eq loc 'VALUES))
	    (wt-nl "value0=") (wt-coerce-loc :object loc) (wt ";"))
	   ((eq loc 'VALUE0) (wt-nl "NVALUES=1;"))
	   ((eq loc 'RETURN) (return-from set-loc))
	   (t
	    (wt-nl "value0=") (wt-coerce-loc :object loc) (wt "; NVALUES=1;"))))
    (TRASH
     (cond (is-call (wt-nl "(void)" loc ";"))
	   ((and (consp loc)
		 (eq (first loc) 'C-INLINE)
		 (fifth loc)) ; side effects?
            (wt-nl loc ";"))))
    (t (cond
	((var-p *destination*)
	 (set-var loc *destination*))
        ((or (not (consp *destination*))
             (not (symbolp (car *destination*))))
         (baboon))
        ((setq fd (get-sysprop (car *destination*) 'SET-LOC))
         (apply fd loc (cdr *destination*)))
        ((setq fd (get-sysprop (car *destination*) 'WT-LOC))
         (wt-nl) (apply fd (cdr *destination*)) (wt "= ")
	 (wt-coerce-loc (loc-representation-type *destination*) loc)
	 (wt ";"))
        (t (baboon)))))
  )

(defun wt-loc (loc &aux fd)
  (cond ((eq loc nil) (wt "Cnil"))
        ((eq loc t) (wt "Ct"))
        ((eq loc 'RETURN)
	 (wt "value0"))	; added for last inline-arg
	((eq loc 'VALUES)
	 (wt "VALUES(0)"))
	((eq loc 'VA-ARG)
	 (wt "va_arg(args,cl_object)"))
	((eq loc 'CL-VA-ARG)
	 (wt "cl_va_arg(args)"))
	((eq loc 'VALUE0)
	 (wt "value0"))
	((var-p loc)
	 (wt-var loc))
        ((or (not (consp loc))
             (not (symbolp (car loc))))
         (baboon))
        ((setq fd (get-sysprop (car loc) 'WT-LOC))
	 (apply fd (cdr loc)))
	(t (baboon)))
  )

(defun last-call-p ()
  (member *exit*
          '(RETURN RETURN-FIXNUM RETURN-CHARACTER RETURN-SINGLE-FLOAT
            RETURN-DOUBLE-FLOAT RETURN-LONG-FLOAT RETURN-OBJECT)))

(defun wt-car (loc) (wt "CAR(" loc ")"))

(defun wt-cdr (loc) (wt "CDR(" loc ")"))

(defun wt-cadr (loc) (wt "CADR(" loc ")"))

(defun wt-lcl (lcl) (unless (numberp lcl) (baboon)) (wt "V" lcl))

(defun wt-vv (vv)
  (if (numberp vv)
    (wt "VV[" vv "]")
    (wt vv)))

(defun wt-vv-temp (vv)
  (if (numberp vv)
    (wt "VVtemp[" vv "]")
    (wt vv)))

(defun wt-lcl-loc (lcl &optional type)
  (wt-lcl lcl))

(defun wt-temp (temp)
  (wt "T" temp))

(defun wt-number (value &optional vv)
  (wt value))

(defun wt-character (value &optional vv)
  (wt (format nil "'\\~O'" value)))

(defun wt-value (i) (wt "VALUES(" i ")"))

(defun wt-keyvars (i) (wt "keyvars[" i "]"))

(defun loc-refers-to-special (loc)
  (cond ((var-p loc)
	 (member (var-kind loc) '(SPECIAL GLOBAL)))
	((atom loc)
	 nil)
	((eq (setf loc (first loc)) 'BIND)
	 t)
	((eq loc 'C-INLINE)
	 t) ; We do not know, so guess yes
	(t nil)))

(defun values-loc (n)
  (list 'VALUE n))

(defun wt-local-frame (n)
  (if n
      (wt +ecl-local-stack-variable+ "[" n "]")
      (wt "((cl_object)&" +ecl-local-stack-frame-variable+ ")")))

;;; -----------------------------------------------------------------

(put-sysprop 'LOCAL-FRAME 'WT-LOC #'wt-local-frame)
(put-sysprop 'TEMP 'WT-LOC #'wt-temp)
(put-sysprop 'LCL 'WT-LOC #'wt-lcl-loc)
(put-sysprop 'VV 'WT-LOC #'wt-vv)
(put-sysprop 'VV-temp 'WT-LOC #'wt-vv-temp)
(put-sysprop 'CAR 'WT-LOC #'wt-car)
(put-sysprop 'CDR 'WT-LOC #'wt-cdr)
(put-sysprop 'CADR 'WT-LOC #'wt-cadr)
(put-sysprop 'FIXNUM-VALUE 'WT-LOC #'wt-number)
(put-sysprop 'CHARACTER-VALUE 'WT-LOC #'wt-character)
(put-sysprop 'LONG-FLOAT-VALUE 'WT-LOC #'wt-number)
(put-sysprop 'DOUBLE-FLOAT-VALUE 'WT-LOC #'wt-number)
(put-sysprop 'SINGLE-FLOAT-VALUE 'WT-LOC #'wt-number)
(put-sysprop 'VALUE 'WT-LOC #'wt-value)
(put-sysprop 'KEYVARS 'WT-LOC #'wt-keyvars)
