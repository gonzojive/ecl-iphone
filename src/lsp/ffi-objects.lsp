;;;;  Copyright (c) 2001, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;  Routines to handle foreign objects, structures, arrays, etc.

(in-package "FFI")

;; ----------------------------------------------------------------------
;; OPERATIONS WITH FOREIGN TYPES
;;

(defmacro def-foreign-type (name type)
  `(si::set-sysprop ',name 'FOREIGN-TYPE ',type))

(defun basic-foreign-type-size (type)
  (case type
    ((:unsigned-char :char :byte :unsigned-byte) 1)
    ((:short :unsigned-short) 2)
    ((:int :unsigned-int) 4)
    ((:long :unsigned-long) 4)
    ((:float) 4)
    ((:double) 8)
    ((:pointer-void) 4)
    (:void 0)
    (t (error-foreign-type type))))

(defun error-foreign-type (type)
  (error 'simple-type-error
	 :format-control "~S is not a valid foreign type"
	 :format-args (list type)
	 :datum type
	 :expected-type 'FOREIGN-TYPE))

(defun compute-foreign-type-size (type &aux name args)
  (if (symbolp type)
      (if (setq args (si::get-sysprop type 'FOREIGN-TYPE))
	  (compute-foreign-type-size type)
	  (basic-foreign-type-size type))
      (case (first type)
	(* (basic-foreign-type-size :pointer-void))
	(:struct
	 (reduce #'+ (rest type) :key #'second :initial-value 0))
	(:union
	 (reduce #'max (rest type) :initial-value 0))
	(:enum
	 (basic-foreign-type-size :int))
	(:array
	 (let ((elt-type-size (compute-foreign-type-size (second type))))
	   (unless (integerp (third type))
	     (error-foreign-type type))
	   (* elt-type-size (third type)))))))

;; ----------------------------------------------------------------------
;; ENUMERATIONS
;;

(defmacro def-enum (enum-name &optional keys &key (separator-string "#"))
  (let ((counter 0)
	(output '())
	(name))
    (dolist (i keys)
      (cond ((symbolp i) (setq name i))
	    ((listp i) (setq name (first i) counter (second i))))
      (unless (and (symbolp name) (integerp counter))
	(error "~S is not a valid enumeration key" (list name counter)))
      (setq name (intern (concatenate 'string
				      (symbol-name enum-name)
				      separator-string
				      (symbol-name name))))
      (push (list name counter) output)
      (incf counter))
    `(progn
      (def-foreign-type ,enum-name (ENUM ,@output))
      ,@(mapcar #'(lambda (x) (cons 'DEFCONSTANT x)) output))))

;; ----------------------------------------------------------------------
;; ARRAYS
;;

(defmacro def-array (name elt-type)
  `(def-foreign-type ,name (:array ,elt-type)))

;; ----------------------------------------------------------------------
;; UTILITIES
;;

(defun null-char-p (char)
  (or (eql char 0)
      (eql char (code-char 0))))

(defun ensure-char-character (char)
  (cond ((integerp char)
	 (code-char char))
	((characterp char)
	 char)
	(t
	 (error 'simple-type-error :datum char :expected-type 'character))))

(defun ensure-char-integer (char)
  (cond ((integerp char)
	 char)
	((characterp char)
	 (char-code char))
	(t
	 (error 'simple-type-error :datum char :expected-type 'character))))

