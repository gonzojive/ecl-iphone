;;;;  -*- Mode: Lisp; Syntax: Common-Lisp; Package: C -*-
;;;;
;;;;  CMPFFI --  Foreign functions interface.

;;;;  Copyright (c) 2003, Juan Jose Garcia-Ripoll.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "COMPILER")

;; ----------------------------------------------------------------------
;; REPRESENTATION TYPES
;;

(defconstant +representation-types+
  '(;; These types can be used by ECL to unbox data
    ;; They are sorted from the most specific, to the least specific one.
    :byte ((signed-byte 8) "int8_t")
    :unsigned-byte ((unsigned-byte 8) "uint8_t")
    :fixnum (fixnum "cl_fixnum")
    :int ((integer #.si:c-int-min #.si:c-int-max) "int")
    :unsigned-int ((integer 0 #.si:c-uint-max) "unsigned int")
    :long ((integer #.si:c-long-min #.si:c-long-max) "long")
    :unsigned-long ((integer 0 #.si:c-ulong-max) "unsigned long")
    :cl-index ((integer 0 #.most-positive-fixnum) "cl_index")
    :float (single-float "float")
    :double (double-float "double")
    #+:long-float :long-double #+:long-float (long-float "long double")
    :char (base-char "char")
    :unsigned-char (base-char "char")
    :wchar (character "cl_index")
    :object (t "cl_object")
    :bool (t "bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    :void (nil "void")
    :pointer-void (si::foreign-data "void*")
    :cstring (string "char*")
    :char* (string "char*")
    :short ((integer #.si:c-short-min #.si:c-short-max) "short")
    :unsigned-short ((integer 0 #.si:c-ushort-max) "unsigned short")
    ))


(defun rep-type->lisp-type (rep-type)
  (let ((output (getf +representation-types+ rep-type)))
    (cond (output
           (if (eq rep-type :void) nil
	     (or (first output)
	         (error "Representation type ~S cannot be coerced to lisp"
		        rep-type))))
	  ((lisp-type-p rep-type) rep-type)
	  (t (error "Unknown representation type ~S" rep-type)))))

(defun lisp-type->rep-type (type)
  (cond
    ;; We expect type = NIL when we have no information. Should be fixed. FIXME!
    ((null type)
     :object)
    ((getf +representation-types+ type)
     type)
    (t
     (do ((l +representation-types+ (cddr l)))
	 ((endp l) :object)
       (when (subtypep type (first (second l)))
	 (return-from lisp-type->rep-type (first l)))))))

(defun rep-type-name (type)
  (or (second (getf +representation-types+ type))
      (error "Not a valid type name ~S" type)))

(defun lisp-type-p (type)
  (subtypep type 'T))


;; ----------------------------------------------------------------------
;; LOCATIONS and representation types
;;
;; Locations are lisp expressions which represent actual C data. To each
;; location we can associate a representation type, which is the type of
;; the C data. The following routines help in determining these types,
;; and also in moving data from one location to another.

(defun loc-movable-p (loc)
  (if (atom loc)
      t
      (case (first loc)
	((CALL CALL-LOCAL) NIL)
	((C-INLINE) (not (fifth loc))) ; side effects?
	(otherwise t))))

(defun loc-type (loc)
  (cond ((eq loc NIL) 'NULL)
	((var-p loc) (var-type loc))
	((si::fixnump loc) 'fixnum)
	((atom loc) 'T)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE 'FIXNUM)
	   (CHARACTER-VALUE (type-of (code-char (second loc))))
	   (DOUBLE-FLOAT-VALUE 'DOUBLE-FLOAT)
	   (SINGLE-FLOAT-VALUE 'SINGLE-FLOAT)
	   (LONG-FLOAT-VALUE 'LONG-FLOAT)
	   (C-INLINE (let ((type (first (second loc))))
		       (if (lisp-type-p type) type (rep-type->lisp-type type))))
	   (BIND (var-type (second loc)))
	   (LCL (or (third loc) T))
	   (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
	((si::fixnump loc) :fixnum)
	((atom loc) :object)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE :fixnum)
	   (CHARACTER-VALUE (if (<= (second loc) 255) :char :wchar))
	   (DOUBLE-FLOAT-VALUE :double)
	   (SINGLE-FLOAT-VALUE :float)
	   (LONG-FLOAT-VALUE :long-double)
	   (C-INLINE (let ((type (first (second loc))))
		       (if (lisp-type-p type) (lisp-type->rep-type type) type)))
	   (BIND (var-rep-type (second loc)))
	   (LCL (lisp-type->rep-type (or (third loc) T)))
	   (otherwise :object)))))

(defun wt-coerce-loc (dest-rep-type loc)
  (setq dest-rep-type (lisp-type->rep-type dest-rep-type))
  ;(print dest-rep-type)
  ;(print loc)
  (let* ((dest-type (rep-type->lisp-type dest-rep-type))
	 (loc-type (loc-type loc))
	 (loc-rep-type (loc-representation-type loc)))
    (labels ((coercion-error ()
	       (cmperr "Unable to coerce lisp object from type (~S,~S)~%~
			to C/C++ type (~S,~S)"
		       loc-type loc-rep-type dest-type dest-rep-type))
	     (ensure-valid-object-type (a-lisp-type)
	       (when (subtypep `(AND ,loc-type ,a-lisp-type) NIL)
		 (coercion-error))))
      (when (eq dest-rep-type loc-rep-type)
	(wt loc)
	(return-from wt-coerce-loc))
      (case dest-rep-type
	((:byte :unsigned-byte :short :unsigned-short :int :unsigned-int
	  :long :unsigned-long :fixnum :cl-index)
	 (case loc-rep-type
	   (#1=(:byte :unsigned-byte :short :unsigned-short :int :unsigned-int
		:long :unsigned-long :fixnum :cl-index
		:float :double :long-double) ; number types
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt (cond ((or (subtypep (loc-type loc) 'fixnum)
			(not (policy-check-all-arguments-p)))
		       "fix(")
		      ((member dest-rep-type '(:unsigned-short :unsigned-long :cl-index))
		       "ecl_to_unsigned_integer(")
		      (t
		       "ecl_to_fixnum("))
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:char :unsigned-char :wchar)
	 (case loc-rep-type
	   ((:char :unsigned-char :wchar)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt "ecl_char_code(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:float :double :long-double)
	 (case loc-rep-type
	   (#1# ; number type
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt (ecase dest-rep-type
		  (:float "ecl_to_float(")
		  (:double "ecl_to_double(")
		  (:long-double "ecl_to_long_double("))
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:bool)
	 (case loc-rep-type
	   (#1# ; number type
	    (wt "1"))
	   ((:object)
	    (wt "(" loc ")!=Cnil"))
	   (otherwise
	    (coercion-error))))
	((:object)
	 (case loc-rep-type
	   ((:short :int :long)
	    (wt "ecl_make_integer(" loc ")"))
	   ((:unsigned-short :unsigned-int :unsigned-long)
	    (wt "ecl_make_unsigned_integer(" loc ")"))
	   ((:byte :unsigned-byte :fixnum)
	    (wt "MAKE_FIXNUM(" loc ")"))
	   ((:float)
	    (if (and (consp loc) (eq (first loc) 'SINGLE-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "ecl_make_singlefloat(" loc ")")))
	   ((:double)
	    (if (and (consp loc) (eq (first loc) 'DOUBLE-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "ecl_make_doublefloat(" loc ")")))
	   ((:long-double)
	    (if (and (consp loc) (eq (first loc) 'LONG-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "make_longfloat(" loc ")")))
	   ((:bool)
	    (wt "((" loc ")?Ct:Cnil)"))
	   ((:char :unsigned-char :wchar)
	    (wt "CODE_CHAR(" loc ")"))
	   ((:cstring)
	    (wt "ecl_cstring_to_base_string_or_nil(" loc ")"))
	   ((:pointer-void)
	    (wt "ecl_make_foreign_data(Cnil, 0, " loc ")"))
	   (otherwise
	    (coercion-error))))
	((:pointer-void)
	 (case loc-rep-type
	   ((:object)
	    ;; Only foreign data types can be coerced to a pointer
	    (wt "ecl_foreign_data_pointer_safe(" loc ")"))
	   ((:cstring)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:cstring)
	 (coercion-error))
	((:char*)
	 (case loc-rep-type
	   ((:object)
	    (wt "ecl_base_string_pointer_safe(" loc ")"))
	   ((:pointer-void)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion-error))))
	(t
	 (coercion-error))))))

;; ----------------------------------------------------------------------
;; C/C++ DECLARATIONS AND HEADERS
;;

(defun c1clines (args)
  (unless (every #'stringp args)
    (cmperr "The argument to CLINES, ~s, is not a list of strings." args))
  (setf *clines-string-list* (nconc *clines-string-list* (copy-list args)))
  (c1expr '(progn)))

;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &rest rest
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (unless (= (length arguments) (length arg-types))
      (cmperr "In a C-INLINE form the number of declare arguments and the number of supplied ones do not match:~%~S"
	      `(C-INLINE ,@args)))
    ;; We cannot handle :cstrings as input arguments. :cstrings are
    ;; null-terminated strings, but not all of our lisp strings will
    ;; be null terminated. In particular, those with a fill pointer
    ;; will not.
    (let ((ndx (position :cstring arg-types)))
      (when ndx
	(let* ((var (gensym))
	       (value (elt arguments ndx)))
	  (setf (elt arguments ndx) var
		(elt arg-types ndx) :char*)
	  (return-from c1c-inline
	    (c1expr
	     `(ffi::with-cstring (,var ,value)
	       (c-inline ,arguments ,arg-types ,output-type ,c-expression
		,@rest)))))))
    ;; Find out the output types of the inline form. The syntax is rather relax
    ;; 	output-type = lisp-type | c-type | (values {lisp-type | c-type}*)
    (flet ((produce-type-pair (type)
	     (if (lisp-type-p type)
		 (cons type (lisp-type->rep-type type))
		 (cons (rep-type->lisp-type type) type))))
      (cond ((eq output-type ':void)
	     (setf output-rep-type '()
		   output-type 'NIL))
	    ((equal output-type '(VALUES &REST t))
	     (setf output-rep-type '(VALUES &REST t)))
	    ((and (consp output-type) (eql (first output-type) 'VALUES))
	     (when one-liner
	       (cmpwarn "A FFI:C-INLINE form cannot be :ONE-LINER and output more than one value: ~A"
			args)
	       (setf one-liner nil))
	     (setf output-rep-type (mapcar #'cdr (mapcar #'produce-type-pair (rest output-type)))
		   output-type 'T))
	    (t
	     (let ((x (produce-type-pair output-type)))
	       (setf output-type (car x)
		     output-rep-type (list (cdr x)))))))
    (let* ((processed-arguments '()))
      (unless (and (listp arguments)
		   (listp arg-types)
		   (stringp c-expression))
	(cmperr "C-INLINE: wrong type of arguments ~S"
		arguments arg-types c-expression))
      (do ((processed-arguments '())
	   (processed-arg-types '()))
	  ((and (endp arguments) (endp arg-types))
	   (make-c1form* 'C-INLINE :type output-type :args
			 (nreverse processed-arguments)
			 (nreverse processed-arg-types)
			 output-rep-type
			 c-expression
			 side-effects
			 one-liner))
	(push (or (pop arg-types) 'T) processed-arg-types)
	(push (c1expr (pop arguments)) processed-arguments)))))

(defun produce-inline-loc (inlined-arguments arg-types output-rep-type
			   c-expression side-effects one-liner)
  (let* (args-to-be-saved
	 coerced-arguments)
    ;; If the expression begins with @[0-9a-z]*, this means we are
    ;; saving some variables.
    (when (and (> (length c-expression) 1)
	       (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
	  ((>= ndx (length c-expression)))
	(let ((c (char c-expression ndx)))
	  (when (eq c #\;)
	    (setf c-expression (subseq c-expression (1+ ndx)))
	    (return))
	  (unless (alphanumericp c)
	    (setf args-to-be-saved nil)
	    (return))
	  (push (- (char-code c) (char-code #\0))
		args-to-be-saved))))

    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    ;;(setf output-rep-type (lisp-type->rep-type output-rep-type))

    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (when (null output-rep-type)
      (if side-effects
	  (progn
	    (wt-nl)
	    (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	    (when one-liner (wt ";")))
	  (cmpwarn "Ignoring form ~S" c-expression))
      (return-from produce-inline-loc NIL))

    ;; If the form is a one-liner, we can simply propagate this expression until the
    ;; place where the value is used.
    (when one-liner
      (return-from produce-inline-loc
	`(C-INLINE ,output-rep-type ,c-expression ,coerced-arguments ,side-effects NIL)))

    ;; If the output is a in the VALUES vector, just write down the form and output
    ;; the location of the data.
    (when (equalp output-rep-type '(VALUES &REST T))
      (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects 'VALUES)
      (return-from produce-inline-loc 'VALUES))

    ;; Otherwise we have to set up variables for holding the output.
    (flet ((make-output-var (type)
	     (let ((var (make-lcl-var :rep-type type)))
	       (wt-nl (rep-type-name type) " " var ";")
	       var)))
      (incf *inline-blocks*)
      (wt-nl "{")
      (let ((output-vars (mapcar #'make-output-var output-rep-type)))
	(wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-vars)
	(cond ((= (length output-vars) 1)
	       (first output-vars))
	      (t
	       (loop for v in output-vars
		     for i from 0
		     do (let ((*destination* `(VALUE ,i))) (set-loc v)))
	       (wt "NVALUES=" (length output-vars) ";")
	       'VALUES))))))

(defun c2c-inline (arguments &rest rest)
  (let ((*inline-blocks* 0))
    (unwind-exit (apply #'produce-inline-loc (inline-args arguments) rest))
    (close-inline-blocks)))

(defun coerce-locs (inlined-args &optional types args-to-be-saved)
  (do* ((l inlined-args (cdr l))
	(item (first l) (first l))
	(i 0 (1+ i))
	(block-opened nil))
       ((endp l)
	inlined-args)
    (let* ((type (if types (pop types) :object))
	   (rep-type (lisp-type->rep-type type))
	   (lisp-type (first item))
	   (loc (second item)))
      (cond ((and (not (loc-movable-p loc)) (member i args-to-be-saved))
	     (let ((lcl (make-lcl-var :rep-type rep-type)))
	       (wt-nl)
	       (unless block-opened
		 (incf *inline-blocks*)
		 (wt-nl "{"))
	       (wt (rep-type-name rep-type) " " lcl "= ")
	       (wt-coerce-loc rep-type loc)
	       (wt ";")
	       (setq loc lcl)))
	    ((and (not (equal rep-type (loc-representation-type loc))))
	     (setq loc `(COERCE-LOC ,rep-type ,loc))))
      (setf (first l) loc))))

(defun wt-c-inline-loc (output-rep-type c-expression coerced-arguments side-effects output-vars)
  (with-input-from-string (s c-expression)
    (when output-vars
      (wt-nl))
    (do ((c (read-char s nil nil)
	    (read-char s nil nil)))
	((null c))
      (case c
	(#\@
	 (let ((object (read s)))
	   (cond ((and (consp object) (equal (first object) 'RETURN))
		  (if (eq output-vars 'VALUES)
		      (cmperr "User @(RETURN ...) in a C-INLINE form with no output values")
		      (let ((ndx (or (second object) 0))
			    (l (length output-vars)))
			(if (< ndx l)
			    (wt (nth ndx output-vars))
			  (cmperr "Used @(RETURN ~D) in a C-INLINE form with ~D output values"
				  ndx l)))))
		 (t
		  (when (and (consp object) (eq (first object) 'QUOTE))
		    (setq object (second object)))
		  (wt (add-object object :permanent t))))))
	(#\#
	 (let* ((k (read-char s))
		(next-char (peek-char nil s nil nil))
		(index (digit-char-p k 36)))
	   (cond ((or (null index) (and next-char (alphanumericp next-char)))
		  (wt #\# k))
		 ((< index (length coerced-arguments))
		  (wt (nth index coerced-arguments)))
		 (t
		  (cmperr "C-INLINE: Variable code exceeds number of arguments")))))
	(otherwise
	 (write-char c *compiler-output1*))))))

(put-sysprop 'FFI:CLINES 'C1SPECIAL #'c1clines)
(put-sysprop 'FFI:C-INLINE 'C1SPECIAL #'c1c-inline)
(put-sysprop 'FFI:C-INLINE 'C2 #'c2c-inline)
(put-sysprop 'FFI:C-INLINE 'WT-LOC #'wt-c-inline-loc)
(put-sysprop 'COERCE-LOC 'WT-LOC #'wt-coerce-loc)
