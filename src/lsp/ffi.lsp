;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; FFI	Symbols used in the foreign function interface

(defpackage "FFI"
  (:export "CLINES" "DEFCFUN" "DEFENTRY" "DEFLA" "DEFCBODY"
	   "DEFINLINE" "DEFUNC" "C-INLINE"

	   "VOID" "OBJECT" "CHAR*" "INT" "DOUBLE"

	   "DEF-CONSTANT" "DEF-FOREIGN-TYPE" "DEF-ENUM" "DEF-STRUCT"
	   "DEF-ARRAY-POINTER" "DEF-FUNCTION" "DEF-UNION" "DEF-ARRAY"
	   "ALLOCATE-FOREIGN-OBJECT" "FREE-FOREIGN-OBJECT" "MAKE-NULL-POINTER"
	   "GET-SLOT-VALUE" "GET-SLOT-POINTER" "DEREF-ARRAY" "DEREF-POINTER"
	   "POINTER-ADDRESS" "SIZE-OF-FOREIGN-TYPE"
	   "NULL-CHAR-P" "ENSURE-CHAR-CHARACTER" "ENSURE-CHAR-INTEGER"
	   "NULL-POINTER-P" "+NULL-CSTRING-POINTER+"
	   ))

(in-package "FFI")

;;;----------------------------------------------------------------------
;;; FOREIGN TYPES
;;;

(defvar *ffi-types* (make-hash-table :size 128))

(dolist (i '(clines defcfun defentry defla defcbody defunC))
  (let ((fname i))
    (si::fset i
       #'(lambda (&rest x)
	   (error "The special form ~S cannot be used in the interpreter"
		  fname)))))

(defmacro definline (fun arg-types type code)
  `(eval-when (compile load eval)
              ;; defCbody must go first, because it clears symbol-plist of fun
              (defCbody ,fun ,arg-types ,type ,code)
              (proclaim '(function ,fun ,arg-types ,type))
              (setf (get ',fun ':inline-always)
                    '((,arg-types ,type
                       t                ; side-effect-p
                       nil ,code)))))

(defun foreign-elt-type-p (name)
  (and (symbolp name)
       (member name '(:byte :unsigned-byte :short :unsigned-short
		      :int :unsigned-int :char :unsigned-char
		      :long :unsigned-long :pointer-void :object
		      :float :double)
	       :test 'eq)))

(defmacro def-foreign-type (name definition)
  `(eval-when (compile load eval)
     (setf (gethash ',name ffi::*ffi-types*) ',definition)))


(defun size-of-foreign-type (name)
  (let* ((size 0)
	 (type (gethash name *ffi-types* name)))
    (unless type
      (error "Incomplete or unknown foreign type ~A" name))
    (cond ((symbolp type)
	   (setf size (si::size-of-foreign-elt-type type)))
	  ((atom type)
	   (error "~A is not a valid foreign type identifier" name))
	  ((eq (setf name (first type)) :struct)
	   (setf size (slot-position type nil)))
	  ((eq name :array)
	   (when (eq (setf size (second array)) '*)
	     (error "Incomplete foreign type"))
	   (setf size (* size (size-of-foreign-type (third array)))))
	  ((eq name '*)
	   (si::size-of-foreign-elt-type :pointer-void))
	  (t
	   (error "~A does not denote a foreign type" name)))
    size))

(defun allocate-foreign-object (type &optional (size 0 size-flag))
  (declare (fixnum size))
  (let ((type-size (size-of-foreign-type type)))
    (cond ((null size-flag)
	   (si::allocate-foreign-data type type-size))
	  ((>= size 0)
	   (let ((bytes (* size type-size)))
	     (si::allocate-foreign-data `(array ,size ,type) bytes)))
	  (t
	   (error "~A is not a valid array dimension size" size)))))

(defun free-foreign-object (ptr)
  (si::free-foreign-data ptr))

;;;----------------------------------------------------------------------
;;; ENUMERATION TYPES
;;;

(defmacro def-enum (name values-list &key (separator-string "#"))
  (let ((constants '())
	(value 0)
	field)
    (setf name (string name)
	  separator-string (string separator-string))
    (dolist (item values-list)
      (cond ((symbolp item)
	     (setf field (string item))
	     (incf value))
	    ((and (consp item)
		  (symbolp (setf field (first item)))
		  (integerp (setf value (second item)))
		  (endp (cddr item))))
	    (t
	     (error "Not a valid argument to DEF-ENUM~%~a" values-list)))
      (setf field (concatenate 'string
			       (symbol-name name)
			       separator-string
			       field))
      (push `(defconstant ,(intern field (symbol-package name))
	       ',value)
	    forms))
    `(progn
       (def-foreign-type ,name :int)
       ,@forms)))


;;;----------------------------------------------------------------------
;;; STRUCTURE TYPES
;;;
;;; The structure type is represented by the following list:
;;;
;;;	(STRUCT (SLOT-NAME1 . SLOT-TYPE1)*)
;;;
;;; FIXME! We do not care about slot alignment!
;;;

(defmacro def-struct (name &rest slots)
  (let ((struct-type (list :struct))
	field
	type)
    (dolist (item (subst `(* ,struct-type) :pointer-self slots))
      (if (and (consp item)
	       (= (length item) 2)
	       (symbolp (setf field (first item))))
	(setf type (second item))
	(error "Not a valid DEF-STRUCT slot ~A" item))
      (push (cons field type) struct-type))
    `(def-foreign-type ,name ,(nreverse struct-type))))

(defun slot-position (type field)
  (setf type (gethash type *ffi-types* type))
  (let ((ndx 0)
	(is-union nil))
    (cond ((atom type)
	   (error "~A is not a foreign STRUCT or UNION type" type))
	  ((eq (first type) :struct))
	  ((eq (first type) :union)
	   (setf is-union t))
	  (t
	   (error "~A is not a foreign STRUCT or UNION type" type)))
    (dolist (slot (rest type))
      (let* ((slot-name (car slot))
	     (slot-type (cdr slot))
	     (slot-size (size-of-foreign-type slot-type)))
	(when (eq slot-name field)
	  (return-from slot-position (values ndx slot-type slot-size)))
	(unless is-union
	  (incf ndx slot-size))))
    (values ndx nil nil)))

(defun get-slot-value (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (if (foreign-elt-type-p slot-type)
	(si::foreign-data-ref-elt object slot-ndx slot-type)
	(si::foreign-data-ref object slot-ndx slot-size slot-type))))

(defun (setf get-slot-value) (value object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (if (foreign-elt-type-p slot-type)
	(si::foreign-data-set-elt object slot-ndx slot-type value)
	(si::foreign-data-set object slot-ndx value))))

(defun get-slot-pointer (object struct-type field)
  (multiple-value-bind (slot-ndx slot-type slot-size)
      (slot-position struct-type field)
    (unless slot-size
      (error "~A is not a field of the type ~A" field struct-type))
    (si::foreign-data-pointer object ndx slot-size field-type)))


;;;----------------------------------------------------------------------
;;; ARRAYS
;;;

(defmacro def-array-pointer (name element-type)
  `(def-foreign-type ,name (* (array * ,element-type))))

(defun deref-array (array array-type position)
  (let* ((element-type (third array-type))
	 (element-size (size-of-foreign-type array-type))
	 (ndx (* position element-size))
	 (length (second array-type)))
    (unless (or (eq length *)
		(> length position -1))
      (error "Out of bounds when accessing array ~A." array))
    (if (foreign-elt-type-p element-type)
	(si::foreign-data-ref-elt array ndx element-type)
        (si::foreign-data-ref array ndx element-size element-type))))


;;;----------------------------------------------------------------------
;;; UNIONS
;;;

(defmacro def-union (name &rest slots)
  (let ((struct-type (list :union))
	field
	type)
    (dolist (item (subst `(* ,struct-type) :pointer-self slots))
      (unless (and (consp item)
		   (= (length item) 2)
		   (symbolp (setf field (first item))))
	(error "Not a valid DEF-UNION slot ~A" item))
      (push (cons field type) struct-type))
    `(def-foreign-type ,name (nreverse struct-type))))

;;;----------------------------------------------------------------------
;;; POINTERS
;;;

(defvar +null-cstring-pointer+ (si:allocate-foreign-data :pointer-void 0))

(defun pointer-address (ptr)
  (error "POINTER-ADDRESS not yet implemented."))

(defun deref-pointer (ptr type)
  ;; FIXME! No checking!
  (setf type (gethash type *ffi-types* type))
  (if (foreign-elt-type-p type)
      (si::foreign-data-ref-elt ptr ndx type)
      (error "Cannot dereference pointer to foreign data, ~A" ptr))

(defun (setf deref-pointer) (value ptr type)
  ;; FIXME! No checking!
  (setf type (gethash type *ffi-types* type))
  (if (foreign-elt-type-p type)
      (si::foreign-data-set-elt ptr ndx type value)
      (si::foreign-data-set ptr ndx value)))

(defun make-null-pointer (type)
  (setf type (gethash type *ffi-types* type))
  (si::allocate-foreign-data type 0))

(defun null-pointer-p (object)
  (si::null-pointer-p object))


;;;----------------------------------------------------------------------
;;; CHARACTERS AND STRINGS
;;;
;;; ECL always returns characters when dereferencing (:array * :char)
;;;

(defun null-char-p (char)
  (eq char #.(code-char 0)))

(defun ensure-char-character (char)
  (cond ((characterp char) char)
	((integerp char) (code-char char))
	(t (error "~a cannot be coerced to type CHARACTER" char))))

(defun ensure-char-integer (char)
  (cond ((characterp char) (char-code char))
	((integerp char) char)
	(t (error "~a cannot be coerced to type INTEGER" char))))

(defmacro convert-from-cstring (object)
  object)

(defmacro convert-to-cstring (object)
  object)

(defmacro free-cstring (object)
  object)

(defmacro with-cstring ((cstring string) &body body)
  `(let ((,cstring ,string)) ,@body))

(defun foreign-string-length (foreign-string)
  (c-inline (foreign-string) (t) :int
	    "strlen((#0)->foreign.data)"
	    :side-effects nil
	    :one-liner t))

(defun convert-from-foreign-string (foreign-string
				    &key length null-terminated-p)
  (cond ((and (not length) null-terminated-p)
	 (setf length (foreign-string-length foreign-string)))
	((not (integerp length))
	 (error "~A is not a valid string length" length)))
  (c-inline (foreign-string length) (t fixnum) string
       "{
	cl_index length = #1;
	cl_object output = cl_alloc_simple_string(length);
	@(return) = memcpy(output->string.self, (#0)->foreign.data, length);
	}"
       :one-liner nil
       :side-effects t))

(defun convert-to-foreign-string (string-designator)
  (let ((lisp-string (string string-designator)))
    (c-inline (lisp-string) (t) t
       "{
	cl_object lisp_string = #0;
	cl_index size = lisp_string->string.dim;
	cl_object output = ecl_allocate_foreign_data(@(* :char), size);
	memcpy(output->foreign.data, lisp_string->string.self, size);
	@(return) = output;
	}"
	:one-liner nil
	:side-effects t)
    ))

(defun allocate-foreign-string (size &key unsigned)
  (si::allocate-foreign-data `(* ,(if unsigned :unsigned-char :char))
			     size))

;;;----------------------------------------------------------------------
;;; MACROLOGY
;;;

(defmacro with-foreign-object ((var type) &body body)
  `(let ((,var (allocate-foreign-object type)))
     (unwind-protect
	 (progn ,@body)
       (free-foreign-object ,var))))

(defmacro with-cast-pointer (bind &body body)
  (let (binding-name ptr type)
    (case (length bind)
      (2 (setf binding-name (first bind)
	       ptr binding-name
	       type (second bind)))
      (3 (setf binding-name (first bind)
	       ptr (second bind)
	       type (third bind)))
      (otherwise (error "Arguments missing in WITH-CAST-POINTER")))
    `(let ((,binding-name (si::foreign-data-pointer ,ptr 0
						    (size-of-foreign-type ',type)
						    ',type)))
       ,@body)))

;;;----------------------------------------------------------------------
;;; INTERFACE TO C FUNCTIONS AND VARIABLES
;;;

(defun lisp-to-c-name (name)
  (cond ((stringp name)
	 (values name (intern (string-upcase (substitute #\- #\_ name)))))
	((and (consp name)
	      (= (length name) 2))
	 (values (first name) (second name)))))

(defmacro def-function (name args &key module (returning :void))
  (multiple-value-bind (c-name lisp-name)
      (lisp-to-c-name)
    (let* ((arguments (mapcar #'first args))
	   (arg-types (mapcar #'second args))
	   (nargs (length arguments))
	   (c-string (format nil "~s(~s)" c-name
			     (subseq 'string "0,1,2,3,4,5,6,7,8,9,a,b,c,d,e,f"
				     :end (if arguments (1- (* nargs 2)) 0))))
	   (casting-required (not (or (eq returning :cstring)
				      (foreign-elt-type-p returning))))
	   (inline-form `(c-inline ,arguments ,arg-types
				   ,(if casting-required :pointer-void returning)
				   ,c-string
				   :one-liner t
				   :side-effects t)))
      (when casting-required
	(setf inline-form
	      `(si::foreign-data-recast ,inline-form
					(size-of-foreign-type ',returning)
					',returning)))
      (when (> nargs 36)
	(error "FFI can only handle C functions with up to 36 arguments"))
      `(defun ,lisp-name (,@arguments)
	 ,inline-form)
      )))

