;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPMAIN  Compiler main program.

;;;		**** Caution ****
;;;	This file is machine/OS dependant.
;;;		*****************


(in-package "COMPILER")

(defvar *cmpinclude* "<ecl-cmp.h>")
;;; This is copied into each .h file generated, EXCEPT for system-p calls.
;;; The constant string *include-string* is the content of file "ecl.h".
;;; Here we use just a placeholder: it will be replaced with sed.

(defvar *cc* "cc"
  "This variable controls how the C compiler is invoked by ECL.
The default value is \"cc -I. -I/usr/local/include/\".
The second -I option names the directory where the file ECL.h has been installed.
One can set the variable appropriately adding for instance flags which the 
C compiler may need to exploit special hardware features (e.g. a floating point
coprocessor).")

(defvar *cc-flags* "-g -I.")
(defvar *cc-optimize* "-O")		; C compiler otimization flag
(defvar *cc-format* "~A ~A ~:[~*~;~A~] -I~A/h -w -c ~A -o ~A"))
;(defvar *cc-format* "~A ~A ~:[~*~;~A~] -I~A/h -c ~A -o ~A"))
(defvar *ld-flags* "")
(defvar *ld-format* "~A -o ~A -L~A ~{~A ~} ~@?")
#+dlopen
(defvar *ld-shared-flags* "")
#+dlopen
(defvar *ld-bundle-flags* "")

(defun safe-system (string)
  (print string)
  (let ((result (si:system string)))
    (unless (zerop result)
      (cerror "Continues anyway."
	      "(SYSTEM ~S) returned non-zero value ~D"
	      string result))
    result))

(defun compile-file-pathname (name &key (output-file name) (type :fasl))
  (let ((format '())
	(extension '()))
    (case type
      ((:shared-library :dll) (setf format #.+shared-library-format+))
      ((:static-library :library :lib) (setf format #.+static-library-format+))
      (:data (setf extension "data"))
      (:sdata (setf extension "sdat"))
      (:c (setf extension "c"))
      (:h (setf extension "h"))
      (:object (setf extension #.+object-file-extension+))
      (:program (setf format #.+executable-file-format+))
      (:fasl (setf extension "fas")))
    (if format
	(merge-pathnames (format nil format (pathname-name output-file))
			 output-file)
	(make-pathname :type extension :defaults output-file))))

(defun linker-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options
	   *ld-flags* (namestring (translate-logical-pathname "SYS:")))))

#+dlopen
(defun shared-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options
	   *ld-shared-flags*
	   (namestring (translate-logical-pathname "SYS:")))))

#+dlopen
(defun bundle-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options
	   *ld-bundle-flags*
	   (namestring (translate-logical-pathname "SYS:")))))

(defconstant +lisp-program-header+ "
#include <ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~{	extern ECL_CPP_TAG void init_~A();~%~}

")

(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif
int init_~A(cl_object cblock)
{
	static cl_object Cblock;
	cl_object subblock;
        if (!FIXNUMP(cblock)) {
		Cblock = cblock;
		cblock->cblock.data_text = compiler_data_text;
		cblock->cblock.data_text_size = compiler_data_text_size;
#ifndef ECL_DYNAMIC_VV
		cblock->cblock.data = VV;
#endif
		cblock->cblock.data_size = VM;
		return;
	}
#if defined(ECL_DYNAMIC_VV) && defined(ECL_SHARED_DATA)
	VV = Cblock->cblock.data;
#endif
	~A
~:[~{	subblock = read_VV(OBJNULL,init_~A); subblock->cblock.next = Cblock;~%~}
~;~{	init_~A(Cblock);~%~}~]

	~A
}")

(defconstant +lisp-program-main+ "
int
main(int argc, char **argv)
{
	~A
	cl_boot(argc, argv);
	read_VV(OBJNULL, init_~A);
	~A
}")

(defun init-function-name (s)
  (flet ((translate-char (c)
	   (cond ((and (char>= c #\a) (char<= c #\z))
		  (char-upcase c))
		 ((and (char>= c #\A) (char<= c #\Z))
		  c)
		 ((or (eq c #\-) (eq c #\_))
		  #\_)
		 ((eq c #\*)
		  #\x)
		 ((eq c #\?)
		  #\a)
		 (t
		  #\p))))
    (setq s (map 'string #'translate-char (string s)))
    (if si::*init-function-prefix*
	(concatenate 'string si::*init-function-prefix* "_" s)
	s)))

(defun builder (target output-name &key lisp-files ld-flags shared-data-file
		(prologue-code "")
		(epilogue-code (if (eq target :program) "
	funcall(1,_intern(\"TOP-LEVEL\",cl_core.system_package));
	return 0;" "")))
  (let* ((c-name (si::coerce-to-filename
		  (compile-file-pathname output-name :type :c)))
	 (o-name (si::coerce-to-filename
		  (compile-file-pathname output-name :type :object)))
	 (init-name (string-upcase (pathname-name c-name)))
	 submodules
	 c-file)
    (dolist (item (reverse lisp-files))
      (cond ((symbolp item)
	     (push (format nil "-l~A" (string-downcase item)) ld-flags)
	     (push (init-function-name item) submodules))
	    (t
	     (push (si::coerce-to-filename
		    (compile-file-pathname item :type :object)) ld-flags)
	     (setq item (pathname-name item))
	     (push (init-function-name item) submodules))))
    (setq c-file (open c-name :direction :output))
    (format c-file +lisp-program-header+ submodules)
    (cond (shared-data-file
	   (data-init shared-data-file)
	   (format c-file "
#define VM ~A
#ifdef ECL_DYNAMIC_VV
cl_object *VV;
#else
cl_object VV[VM];
#endif
cl_object Cblock;
#define ECL_SHARED_DATA_FILE 1
" (data-size))
	   (data-dump c-file))
	  (t
	   (format c-file "
#define compiler_data_text NULL
#define compiler_data_text_size 0
#define VV NULL
#define VM 0" c-file)))
    (ecase target
      (:program
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :program)))
       (format c-file +lisp-program-init+ init-name "" shared-data-file
	       submodules "")
       (format c-file +lisp-program-main+ prologue-code init-name epilogue-code)
       (close c-file)
       (si:system (format nil "cat ~A" (namestring c-name)))
       (compiler-cc c-name o-name)
       (apply #'linker-cc output-name (namestring o-name) ld-flags))
      ((:library :static-library :lib)
       (if (or (symbolp output-name) (stringp output-name))
	   (setf output-name (compile-file-pathname output-name :type :lib))
	   ;; Remove the leading "lib"
	   (setf init-name (subseq init-name #.(length +static-library-prefix+))))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (si:system (format nil "cat ~A" (namestring c-name)))
       (compiler-cc c-name o-name)
       (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			    output-name o-name ld-flags))
       (safe-system (format nil "ranlib ~A" output-name)))
      #+dlopen
      ((:shared-library :dll)
       (if (or (symbolp output-name) (stringp output-name))
	   (setf output-name (compile-file-pathname output-name :type :dll))
	   ;; Remove the leading "lib"
	   (setf init-name (subseq init-name #.(length +static-library-prefix+))))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (si:system (format nil "cat ~A" (namestring c-name)))
       (compiler-cc c-name o-name)
       (apply #'shared-cc output-name o-name ld-flags))
      #+dlopen
      (:fasl
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :fasl)))
       (format c-file +lisp-program-init+ "CODE" prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (si:system (format nil "cat ~A" (namestring c-name)))
       (compiler-cc c-name o-name)
       (apply #'bundle-cc output-name o-name ld-flags)))
    (delete-file c-name)
    (delete-file o-name)
    output-name))

(defun build-fasl (&rest args)
  (apply #'builder :fasl args))

(defun build-program (&rest args)
  (apply #'builder :program args))

(defun build-static-library (&rest args)
  (apply #'builder :static-library args))

(defun build-shared-library (&rest args)
  #-dlopen
  (error "Dynamically loadable libraries not supported in this system.")
  #+dlopen
  (apply #'builder :shared-library args))

(eval-when (compile eval)
  (defmacro get-output-pathname (input-file output-file ext)
    `(compile-file-pathname ,input-file
      :output-file (if (member ,output-file '(T NIL)) ,input-file ,output-file)
      :type ,ext)))

(defun compile-file (input-pathname
                      &key (output-file 'T)
		      (verbose *compile-verbose*)
		      (print *compile-print*)
		      (c-file nil)
		      (h-file nil)
		      (data-file nil)
		      (shared-data-file nil)
		      (system-p nil)
		      (load nil)
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
                           (*package* *package*)
			   (*print-pretty* nil)
                           (*error-count* 0)
			   (*compile-file-pathname* nil)
			   (*compile-file-truename* nil)
			   #+PDE sys:*source-pathname*)
  (declare (notinline compiler-cc))

  #-ecl-min
  (require 'sysfun "sys:sysfun")

  #-dlopen
  (unless system-p
    (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE-FILE without :SYSTEM-P T is unsupported.~
~%;;;"))

  (setq *compile-file-pathname* (make-pathname :type "lsp" :defaults input-pathname))
  (unless (probe-file *compile-file-pathname*)
    (setq *compile-file-pathname* (make-pathname :type "lisp" :defaults input-pathname))
    (unless (probe-file *compile-file-pathname*)
      (format t "~&;;; The source file ~a is not found.~%"
	      (namestring input-pathname))
      (setq *error-p* t)
      (return-from compile-file (values nil t t))))
  (setq *compile-file-truename* (truename *compile-file-pathname*))

  #+PDE (setq sys:*source-pathname* *compile-file-truename*)

  (when (and system-p load)
    (error "Cannot load system files."))

  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~%~
Cannot compile ~a."
	    (namestring input-pathname))
    (setq *error-p* t)
    (return-from compile-file (values nil t t)))

  (setq *error-p* nil
	*compiler-in-use* t)

  (when *compile-verbose*
    (format t "~&;;; Compiling ~a."
            (namestring input-pathname)))

  (let* ((eof '(NIL))
	 (*load-time-values* nil) ;; Load time values are compiled
	 (o-pathname (get-output-pathname input-pathname output-file :object))
	 #+dlopen
         (so-pathname (unless system-p (compile-file-pathname o-pathname)))
         (c-pathname (get-output-pathname o-pathname c-file :c))
         (h-pathname (get-output-pathname o-pathname h-file :h))
         (data-pathname (get-output-pathname o-pathname data-file :data))
	 (shared-data-pathname (get-output-pathname o-pathname shared-data-file
						    :sdata)))

    (with-lock (+load-compile-lock+)
      (init-env)

      (when (probe-file "./cmpinit.lsp")
	(load "./cmpinit.lsp" :verbose *compile-verbose*))

      (if shared-data-file
	  (if system-p
	      (data-init shared-data-pathname)
	      (error "Shared data files are only allowed when compiling ~&
		    with the flag :SYSTEM-P set to T."))
	  (data-init))

      (with-open-file (*compiler-input* *compile-file-pathname*)
	(do ((form (read *compiler-input* nil eof)
		   (read *compiler-input* nil eof)))
	    ((eq form eof))
	  (t1expr form)))

      (when (zerop *error-count*)
	(when *compile-verbose* (format t "~&;;; End of Pass 1.  "))
	(compiler-pass2 c-pathname h-pathname data-pathname system-p
			(if system-p
			    (pathname-name input-pathname)
			    "code")
			shared-data-file))

      (if shared-data-file
	  (data-dump shared-data-pathname t)
	  (data-dump data-pathname))

      (init-env)
      )

    (if (zerop *error-count*)
        (progn
          (cond (output-file
		 (when *compile-verbose*
		   (format t "~&;;; Calling the C compiler... "))
                 (compiler-cc c-pathname o-pathname)
		 #+dlopen
		 (unless system-p (bundle-cc so-pathname o-pathname))
                 (cond #+dlopen
		       ((and (not system-p) (probe-file so-pathname))
                        (when load (load so-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a.~%"
				  (namestring input-pathname))))
		       ((and system-p (probe-file o-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a.~%"
				  (namestring input-pathname))))
                       (t (format t "~&;;; The C compiler failed to compile the intermediate file.~%")
                          (setq *error-p* t))))
		(*compile-verbose*
		 (print-compiler-info)
		 (format t "~&;;; Finished compiling ~a.~%"
			 (namestring input-pathname))))
          (unless c-file (delete-file c-pathname))
          (unless h-file (delete-file h-pathname))
          (unless (or data-file shared-data-file)
	    (delete-file data-pathname))
	  #+dlopen
	  (unless system-p (delete-file o-pathname))
	  #+dlopen
	  (if system-p o-pathname so-pathname)
	  #-dlopen
	  (values o-pathname nil nil))

        (progn
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (when (probe-file shared-data-pathname) (delete-file shared-data-pathname))
	  (when (probe-file o-pathname) (delete-file o-pathname))
          (format t "~&;;; Due to errors in the compilation process, no FASL was generated.
;;; Search above for the \"Error:\" tag to find the error messages.~%")
          (setq *error-p* t)
	  (values nil t t))
        ))
  ) ; with-lock
)

#-dlopen
(defun compile (name &optional (def nil supplied-p))
  (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE is unsupported.~
~%;;;"))

#+dlopen
(defvar *gazonk-counter* 0)

#+dlopen
(defun compile (name &optional (def nil supplied-p)
                      &aux form data-pathname
                      (*compiler-in-use* *compiler-in-use*)
                      (*standard-output* *standard-output*)
                      (*error-output* *error-output*)
                      (*package* *package*)
                      (*compile-print* nil)
		      (*print-pretty* nil)
                      (*error-count* 0))

  (unless (symbolp name) (error "~s is not a symbol." name))

  #-ecl-min
  (require 'sysfun "sys:sysfun")

  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~
		~%Cannot compile ~s." name)
    (setq *error-p* t)
    (return-from compile (values name nil t)))

  (setq *error-p* nil
	*compiler-in-use* t)

  (cond ((and supplied-p def)
         (setq form (if name
                        `(setf (symbol-function ',name) #',def)
                        `(set 'GAZONK #',def))))
        ((and (fboundp name)
	      (setq def (symbol-function name))
	      (setq form (function-lambda-expression def)))
	 (setq form `(setf (symbol-function ',name) #',form)))
        (t (error "No lambda expression is assigned to the symbol ~s." name)))

  (let ((template (format nil "~A/ecl~3,'0x"
			  (or (si::getenv "TMPDIR") "/tmp")
			  (incf *gazonk-counter*))))
    (unless (setq data-pathname (pathname (si::mkstemp template)))
      (format t "~&;;; Unable to create temporay file~%~
;;;	~AXXXXXX
;;; Make sure you have enough free space in disk, check permissions or set~%~
;;; the environment variable TMPDIR to a different value." template)
      (setq *error-p* t)
      (return-from compile (values nil t t))))

  (let ((*load-time-values* 'values) ;; Only the value is kept
	(c-pathname (compile-file-pathname data-pathname :type :c))
	(h-pathname (compile-file-pathname data-pathname :type :h))
	(o-pathname (compile-file-pathname data-pathname :type :object))
	(so-pathname (compile-file-pathname data-pathname)))

    (with-lock (+load-compile-lock+)
      (init-env)
      (data-init)
      (t1expr form)
      (when (zerop *error-count*)
	(when *compile-verbose* (format t "~&;;; End of Pass 1.  "))
	(compiler-pass2 c-pathname h-pathname data-pathname nil "code" nil))
      (data-dump data-pathname)
      (init-env)
      )

    (if (zerop *error-count*)
        (progn
          (when *compile-verbose*
	    (format t "~&;;; Calling the C compiler... "))
	  ;;(si::system (format nil "cat ~A" (namestring c-pathname)))
          (compiler-cc c-pathname o-pathname)
	  (bundle-cc so-pathname o-pathname)
          (delete-file c-pathname)
          (delete-file h-pathname)
	  (delete-file o-pathname)
          (cond ((probe-file so-pathname)
                 (load so-pathname :verbose nil)
                 (when *compile-verbose* (print-compiler-info))
                 (delete-file so-pathname)
		 (delete-file data-pathname)
		 (setf name (or name (symbol-value 'GAZONK)))
		 ;; By unsetting GAZONK we avoid spurious references to the
		 ;; loaded code.
		 (set 'GAZONK nil)
		 (si::gc t)
		 (values name nil nil))
		(t (delete-file data-pathname)
                   (format t "~&;;; The C compiler failed to compile~
			~the intermediate code for ~s.~%" name)
                   (setq *error-p* t)
		   (values name t t))))
        (progn
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file so-pathname) (delete-file so-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (format t "~&;;; Failed to compile ~s.~%" name)
          (setq *error-p* t)
          (values name t t)))))

(defun disassemble (&optional (thing nil)
			      &key (h-file nil) (data-file nil)
			      &aux def disassembled-form
			      (*compiler-in-use* *compiler-in-use*)
			      (*print-pretty* nil))
  (when (si::valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (cond ((null thing))
	((functionp thing)
	 (unless (si::bc-disassemble thing)
	   (error "The function definition for ~S was lost." thing)))
	((and (consp thing) (eq (car thing) 'LAMBDA))
	 (setq disassembled-form `(defun gazonk ,@(cdr thing))))
	(t (setq disassembled-form thing)))
  #-ecl-min
  (require 'sysfun "sys:sysfun")
  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~
                   ~%Cannot disassemble ~a." thing)
    (setq *error-p* t)
    (return-from disassemble))
  (setq *error-p* nil
	*compiler-in-use* t)

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
				 (open h-file :direction :output)
				 null-stream))
         (*error-count* 0)
         (t3local-fun (symbol-function 'T3LOCAL-FUN))
	 (t3fun (get-sysprop 'DEFUN 'T3)))
    (with-lock (+load-compile-lock+)
      (unwind-protect
	   (progn
	     (put-sysprop 'DEFUN 'T3
			  #'(lambda (&rest args)
			      (let ((*compiler-output1* *standard-output*))
				(apply t3fun args))))
	     (setf (symbol-function 'T3LOCAL-FUN)
		   #'(lambda (&rest args)
		       (let ((*compiler-output1* *standard-output*))
			 (apply t3local-fun args))))
	     (init-env)
	     (data-init)
	     (t1expr disassembled-form)
	     (if (zerop *error-count*)
		 (catch *cmperr-tag* (ctop-write "code"
						 (if h-file h-file "")
						 (if data-file data-file "")
						 :system-p nil))
		 (setq *error-p* t))
	     (data-dump data-file)
	     (init-env)
	     )
	(put-sysprop 'DEFUN 'T3 t3fun)
	(setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
	(when h-file (close *compiler-output2*)))))
  (values)
  )

(defun compiler-pass2 (c-pathname h-pathname data-pathname system-p init-name
		       shared-data)
  (with-open-file (*compiler-output1* c-pathname :direction :output)
    (with-open-file (*compiler-output2* h-pathname :direction :output)
      (wt-nl1 "#include " *cmpinclude*)
      (catch *cmperr-tag* (ctop-write (string-upcase init-name)
				      h-pathname
				      data-pathname
				      :system-p system-p
				      :shared-data shared-data))
      (terpri *compiler-output1*)
      (terpri *compiler-output2*))))

(defun compiler-cc (c-pathname o-pathname)
  (safe-system
   (format nil
	   *cc-format*
	   *cc* *cc-flags* (>= *speed* 2) *cc-optimize*
	   (namestring (translate-logical-pathname "SYS:"))
	   (si::coerce-to-filename c-pathname)
	   (si::coerce-to-filename o-pathname))
; Since the SUN4 assembler loops with big files, you might want to use this:
;   (format nil
;	   "~A ~@[~*-O1~] -S -I. -I~A -w ~A ; as -o ~A ~A"
;	   *cc* (>= *speed* 2)
;          *include-directory*
;	   (namestring c-pathname)
;	   (namestring o-pathname)
;	   (namestring s-pathname))
   ))

(defun print-compiler-info ()
  (format t "~&;;; OPTIMIZE levels: Safety=~d~:[ (No runtime error checking)~;~], Space=~d, Speed=~d~%"
          (cond ((null *compiler-check-args*) 0)
                ((null *safe-compile*) 1)
                ((null *compiler-push-events*) 2)
                (t 3))
          *safe-compile* *space* *speed*))

#+dlopen
(defun load-o-file (file verbose print)
  (let ((tmp (compile-file-pathname file)))
    (bundle-cc tmp file)
    (when (probe-file tmp)
      (load tmp :verbose nil :print nil)
      (delete-file tmp)
      nil)))

#+dlopen
(push (cons #.+object-file-extension+ #'load-o-file) si::*load-hooks*)

(defmacro with-compilation-unit (options &rest body)
  `(progn ,@body))

(si::package-lock "CL" nil)

;;; ----------------------------------------------------------------------
(provide "compiler")
