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
    :byte ((signed-byte 8) "byte")
    :unsigned-byte ((unsigned-byte 8) "unsigned byte")
    :fixnum (fixnum "cl_fixnum")
    :int ((integer #.si:c-int-min #.si:c-int-max) "int")
    :unsigned-int ((integer 0 #.si:c-uint-max) "unsigned int")
    :long ((integer #.si:c-long-min #.si:c-long-max) "long")
    :unsigned-long ((integer 0 #.si:c-ulong-max) "unsigned long")
    :cl-index ((integer 0 #.most-positive-fixnum) "cl_index")
    :float (short-float "float")
    :double (long-float "double")
    :char (character "char")
    :unsigned-char (character "char")
    :object (t "cl_object")
    :bool (t "bool")
    ;; These types are never selected to unbox data.
    ;; They are here, because we need to know how to print them.
    :void (nil "void")
    :pointer-void (foreign-data "void*")
    :cstring (string "char*")
    :short ((integer #.si:c-short-min #.si:c-short-max) "short")
    :unsigned-short ((integer 0 #.si:c-short-max) "unsigned short")
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
  (if (getf +representation-types+ type)
      type
      (do ((l +representation-types+ (cddr l)))
	  ((endp l) :object)
	(when (subtypep type (first (second l)))
	  (return-from lisp-type->rep-type (first l))))))

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
	   (CHARACTER-VALUE 'CHARACTER)
	   (LONG-FLOAT-VALUE 'LONG-FLOAT)
	   (SHORT-FLOAT-VALUE 'SHORT-FLOAT)
	   (C-INLINE (rep-type->lisp-type (second loc)))
	   (BIND (var-type (second loc)))
	   (otherwise T)))))

(defun loc-representation-type (loc)
  (cond ((member loc '(NIL T)) :object)
	((var-p loc) (var-rep-type loc))
	((si::fixnump loc) :fixnum)
	((atom loc) :object)
	(t
	 (case (first loc)
	   (FIXNUM-VALUE :fixnum)
	   (CHARACTER-VALUE :char)
	   (LONG-FLOAT-VALUE :double)
	   (SHORT-FLOAT-VALUE :float)
	   (C-INLINE (second loc))
	   (BIND (var-rep-type (second loc)))
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
	((:int :unsigned-int :long :unsigned-long :byte :unsigned-byte :fixnum)
	 (case loc-rep-type
	   ((:int :unsigned-int :long :unsigned-long :byte :unsigned-byte :fixnum
		  :float :double)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt (if (subtypep (loc-type loc) 'fixnum) "fix(" "object_to_fixnum(")
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:char :unsigned-char)
	 (case loc-rep-type
	   ((:char :unsigned-char)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    (ensure-valid-object-type dest-type)
	    (wt "char_code(" loc ")"))
	   (otherwise
	    (coercion-error))))
	((:float :double)
	 (case loc-rep-type
	   ((:int :unsigned-int :long :unsigned-long :byte :unsigned-byte :fixnum
		  :float :double)
	    (wt "((" (rep-type-name dest-rep-type) ")" loc ")"))
	   ((:object)
	    ;; We relax the check a bit, because it is valid in C to coerce
	    ;; between floats of different types.
	    (ensure-valid-object-type 'FLOAT)
	    (wt (if (eq loc-rep-type :float) "object_to_float(" "object_to_double(")
		loc ")"))
	   (otherwise
	    (coercion-error))))
	((:bool)
	 (case loc-rep-type
	   ((:int :unsigned-int :long :unsigned-long :byte :unsigned-byte :fixnum
		  :float :double :char :unsigned-char)
	    (wt "1"))
	   ((:object)
	    (wt "(" loc ")!=Cnil"))
	   (otherwise
	    (coercion-error))))
	((:object)
	 (case loc-rep-type
	   ((:int :unsigned-int :long :unsigned-long)
	    (wt "make_integer(" loc ")"))
	   ((:byte :unsigned-byte :fixnum)
	    (wt "MAKE_FIXNUM(" loc ")"))
	   ((:float)
	    (if (and (consp loc) (eq (first loc) 'SHORT-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "make_shortfloat(" loc ")")))
	   ((:double)
	    (if (and (consp loc) (eq (first loc) 'LONG-FLOAT-VALUE))
		(wt (third loc)) ;; VV index
		(wt "make_longfloat(" loc ")")))
	   ((:bool)
	    (wt "((" loc ")?Ct:Cnil)"))
	   ((:char :unsigned-char)
	    (wt "CODE_CHAR(" loc ")"))
	   ((:cstring)
	    (wt "ecl_cstring_to_string_or_nil(" loc ")"))
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
	 (case loc-rep-type
	   ((:object)
	    (if (safe-compile)
		(wt "ecl_string_pointer_safe(" loc ")")
	        (wt "(" loc ")->string.self")))
	   ((:pointer-void)
	    (wt "(char *)(" loc ")"))
	   (otherwise
	    (coercion error))))
	(t
	 (coercion-error))))))


;; ----------------------------------------------------------------------
;; C/C++ INLINE CODE
;;

(defun c1c-inline (args)
  ;; We are on the safe side by assuming that the form has side effects
  (destructuring-bind (arguments arg-types output-type c-expression
				 &key (side-effects t) one-liner
				 &aux output-rep-type)
      args
    (if (lisp-type-p output-type)
	(setq output-rep-type (lisp-type->rep-type output-type))
	(setq output-rep-type output-type
	      output-type (rep-type->lisp-type output-type)))
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
    (when (and (> (length c-expression) 1)
	       (eq (char c-expression 0) #\@))
      (do ((ndx 1 (1+ ndx)))
	  ((or (>= ndx (length c-expression))
	       (eq (char c-expression ndx) #\;)))
	(push (- (char-code (char c-expression ndx)) (char-code #\0))
	      args-to-be-saved)))
   
    (setf coerced-arguments (coerce-locs inlined-arguments arg-types args-to-be-saved))
    (setf output-rep-type (lisp-type->rep-type output-rep-type))
    ;; If the form does not output any data, and there are no side
    ;; effects, try to omit it.
    (cond ((eq output-rep-type :void)
	   (if side-effects
	     (progn
	       (wt-c-inline-loc output-rep-type c-expression coerced-arguments t nil)
	       (wt ";"))
	     (cmpwarn "Ignoring form ~S" c-expression))
	   NIL)
	  (one-liner
	   `(C-INLINE ,output-rep-type ,c-expression ,coerced-arguments ,side-effects NIL))
	  (t
	   (let ((output-var (make-lcl-var :rep-type output-rep-type)))
	     (incf *inline-blocks*)
	     (wt-nl "{" (rep-type-name output-rep-type) " " output-var ";")
	     (wt-c-inline-loc output-rep-type c-expression coerced-arguments side-effects output-var)
	     output-var)))))

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
	   (expected-arg-type (rep-type->lisp-type type))
	   (lisp-type (first item))
	   (loc (second item)))
;      (unless (and (eql rep-type (loc-representation-type loc))
;		   (or (loc-movable-p loc)
;		       (not (member i args-to-be-saved))))
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
      (setf (first l) loc)
      )))

(defun wt-c-inline-loc (output-rep-type c-expression coerced-arguments side-effects output-var)
  (with-input-from-string (s c-expression
			     :start
			     (if (eq (char c-expression 0) #\@)
				 (1+ (or (position #\; c-expression)
					 -1))
				 0))
    (when output-var
      (wt-nl))
    (do ((c (read-char s nil nil)
	    (read-char s nil nil)))
	((null c))
      (case c
	(#\@
	 (let ((object (read s)))
	   (cond ((equal object '(RETURN))
		  (if output-var
		      (wt output-var)
		      (cmperr "Tried to use @RETURN within a one-line C-INLINE form")))
		 (t
		  (when (and (consp object) (eq (first object) 'QUOTE))
		    (setq object (second object)))
		  (wt (add-object object))))))
	(#\#
	 (let* ((k (char-downcase (read-char s)))
		(index (digit-char-p k 36)))
	   (unless (and index (< index (length coerced-arguments)))
	     (cmperr "C-INLINE: Variable code exceeds number of arguments"))
	   (wt (nth index coerced-arguments))))
	(otherwise
	 (write-char c *compiler-output1*))))))

;; ----------------------------------------------------------------------
;; SIMPLIFIED INTERFACES TO C-INLINE
;;

(defmacro defentry (lisp-name c-types c-name)
  (let ((out-type (if (consp c-name) (first c-name) :object))
	(arg-names (mapcar #'(lambda (x) (gensym)) c-types)))
    (when (consp c-name)
      (setq c-name (second c-name)))
    (cond ((symbolp c-name)
	   (setq c-name (string-downcase (symbol-name c-name))))
	  ((not (stringp c-name))
	   (error "~S is not a valid C/C++ function name" c-name)))
  `(defun ,lisp-name ,arg-names
     (c-inline ,arg-names ,c-types ,out-type
       ,(with-output-to-string (s)
	  (format s "~a(" c-name)
	  (do ((l c-types (cdr l))
	       (i 0 (1+ i)))
	      ((endp l) (princ #\) s))
	    (format s "#~d~:[~;,~]" i (cdr l))))
      :one-liner t))))

(put-sysprop 'C-INLINE 'C1SPECIAL #'c1c-inline)
(put-sysprop 'C-INLINE 'C2 #'c2c-inline)
(put-sysprop 'C-INLINE 'WT-LOC #'wt-c-inline-loc)
(put-sysprop 'COERCE-LOC 'WT-LOC #'wt-coerce-loc)
