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
(defvar *ld-format* "~A ~A -w -o ~A -L~A ~{~A ~} ~A")
#+dlopen
(defvar *ld-shared-flags* "")
#+dlopen
(defvar *ld-shared-format* "~A ~A -o ~A -L~A ~{~A ~} ~A")

(eval-when (compile eval)
  (defmacro get-output-pathname (file ext)
    `(make-pathname
      :directory (or (and (or (stringp ,file) (pathnamep ,file))
		          (pathname-directory ,file))
		     directory)
      :name (if (or (null ,file) (eq ,file T)) name (pathname-name ,file))
      :type ,ext)))

(defun safe-system (string)
  (print string)
  (let ((result (si:system string)))
    (unless (zerop result)
      (cerror "Continues anyway."
	      "(SYSTEM ~S) returned non-zero value ~D"
	      string result))
    result))

(defun static-library-pathname (output-file)
  (let* ((real-name (format nil "lib~A.a" (pathname-name output-file))))
    (merge-pathnames real-name output-file)))

(defun shared-library-pathname (output-file)
  #-dlopen
  (error "Dynamically loadable libraries not supported in this system.")
  #+dlopen
  (let* ((real-name (format nil "~A.so" (pathname-name output-file))))
    (merge-pathnames real-name output-file)))

(defun compile-file-pathname (name &key output-file system-p)
  (let ((extension ".o"))
    (unless system-p
      #+dlopen
      (setq extension ".so")
      #-dlopen
      (error "This platform only supports compiling files with :SYSTEM-P T"))
    (merge-pathnames extension (or output-file name))))

(defun linker-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   ""
	   (namestring o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options *ld-flags*)))

#+dlopen
(defun shared-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-shared-format*
	   *cc*
	   *ld-shared-flags*
	   (namestring o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options
	   "")))

(defconstant +lisp-program-main+ "
#include <ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

int
main(int argc, char **argv)
{
~{	extern ECL_CPP_TAG void init_~A(cl_object);~%~}
	~A
	cl_boot(argc, argv);
~{	read_VV(OBJNULL,init_~A);~%~}
	~A
}")

(defconstant +lisp-library-main+ "
#include <ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~{	extern ECL_CPP_TAG void init_~A();~%~}

#ifdef __cplusplus
extern \"C\"
#endif
int init_~A(cl_object cblock)
{
	cl_object next;
        if (FIXNUMP(cblock))
		return;
	cblock->cblock.data = NULL;
	cblock->cblock.data_size = 0;
	cblock->cblock.data_text = \"\";
	cblock->cblock.data_text_size = 0;
	~A
~{	next = read_VV(OBJNULL,init_~A); next->cblock.next = cblock; cblock = next; ~%~}
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

(defun builder (target output-name &key lisp-files ld-flags (prologue-code "")
		(epilogue-code (if (eq target :program) "
	funcall(1,_intern(\"TOP-LEVEL\",system_package));
	return 0;" "")))
  (let* (init-name c-name o-name)
    (when (eq target :program)
      (setq ld-flags (append ld-flags '("-lecl" "-lclos" "-llsp"))))
    (dolist (item (reverse lisp-files))
      (cond ((symbolp item)
	     (push (format nil "-l~A" (string-downcase item)) ld-flags)
	     (push (init-function-name item) init-name))
	    (t
	     (push (namestring (merge-pathnames ".o" item)) ld-flags)
	     (setq item (pathname-name item))
	     (push (init-function-name item) init-name))))
    (setq c-name (namestring (merge-pathnames ".c" output-name))
	  o-name (namestring (merge-pathnames ".o" output-name)))
    (ecase target
      (:program
       (setq output-name (namestring output-name))
       (with-open-file (c-file c-name :direction :output)
	 (format c-file +lisp-program-main+ init-name prologue-code init-name
		 epilogue-code))
       (compiler-cc c-name o-name)
       (apply #'linker-cc output-name (namestring o-name) ld-flags))
      (:static-library
       (when (symbolp output-name)
	 (setq output-name (static-library-pathname output-name)))
       (let ((library-name (string-upcase (pathname-name output-name))))
	 (unless (equalp (subseq library-name 0 3) "LIB")
	   (error "Filename ~A is not a valid library name."
		  output-name))
	 (with-open-file (c-file c-name :direction :output)
	   (format c-file +lisp-library-main+ init-name
		   ;; Remove the leading "lib"
		   (subseq library-name 3)
		   prologue-code init-name epilogue-code)))
       (compiler-cc c-name o-name)
       (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			    output-name o-name ld-flags))
       (safe-system (format nil "ranlib ~A" output-name)))
      #+dlopen
      (:shared-library
       (when (or (symbolp output-name) (not (pathname-type output-name)))
	 (setq output-name (shared-library-pathname output-name)))
       (with-open-file (c-file c-name :direction :output)
	 (format c-file +lisp-library-main+
		 init-name "CODE" prologue-code init-name epilogue-code))
       (compiler-cc c-name o-name)
       (apply #'shared-cc output-name o-name ld-flags)))
    ;(delete-file c-name)
    (delete-file o-name)
    output-name))

(defun build-program (&rest args)
  (apply #'builder :program args))

(defun build-static-library (&rest args)
  (apply #'builder :static-library args))

(defun build-shared-library (&rest args)
  #-dlopen
  (error "Dynamically loadable libraries not supported in this system.")
  #+dlopen
  (apply #'builder :shared-library args))

(defun compile-file (input-pathname
                      &key (output-file 'T)
		      (verbose *compile-verbose*)
		      (print *compile-print*)
		      (c-file nil)
		      (h-file nil)
		      (data-file nil)
		      (system-p nil)
		      (load nil)
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
                           (*package* *package*)
			   (*print-pretty* nil)
                           (*error-count* 0)
			   (*compile-file-pathname* nil)
			   (*compile-file-trueame* nil)
			   #+PDE sys:*source-pathname*)
  (declare (notinline compiler-cc))

  #-dlopen
  (unless system-p
    (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE-FILE without :SYSTEM-P T is unsupported.~
~%;;;"))

  (setq *compile-file-pathname* (merge-pathnames input-pathname #P".lsp"))
  (unless (probe-file *compile-file-pathname*)
    (setq *compile-file-pathname* (merge-pathnames input-pathname #P".lisp"))
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
	 (output-default (if (or (eq output-file 'T)
				 (null output-file))
			     input-pathname
			     output-file))
         (directory (pathname-directory output-default))
         (name (pathname-name output-default))
	 (o-pathname (get-output-pathname output-file "o"))
	 #+dlopen
         (so-pathname (if system-p o-pathname
			  (get-output-pathname output-file "so")))
         (c-pathname (get-output-pathname c-file "c"))
         (h-pathname (get-output-pathname h-file "h"))
         (data-pathname (get-output-pathname data-file "data")))

    (init-env)

    (when (probe-file "./cmpinit.lsp")
      (load "./cmpinit.lsp" :verbose *compile-verbose*))

    (with-open-file (*compiler-output-data* data-pathname :direction :output)
      (wt-data-begin)

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
                            "code")))

      (wt-data-end)

      ) ;;; *compiler-output-data* closed.

    (init-env)

    (if (zerop *error-count*)
        (progn
          (cond (output-file
		 (when *compile-verbose*
		   (format t "~&;;; Calling the C compiler... "))
                 (compiler-cc c-pathname o-pathname)
		 #+dlopen
		 (unless system-p (shared-cc so-pathname o-pathname))
                 (cond #+dlopen
		       ((and (not system-p) (probe-file so-pathname))
                        (when load (load so-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a."
				  (namestring input-pathname))))
		       ((and system-p (probe-file o-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a."
				  (namestring input-pathname))))
                       (t (format t "~&;;; The C compiler failed to compile the intermediate file.~%")
                          (setq *error-p* t))))
		(*compile-verbose*
		 (print-compiler-info)
		 (format t "~&;;; Finished compiling ~a."
			 (namestring input-pathname))))
          (unless c-file (delete-file c-pathname))
          (unless h-file (delete-file h-pathname))
          (unless data-file (delete-file data-pathname))
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
	  (when (probe-file o-pathname) (delete-file o-pathname))
          (format t "~&;;; Due to errors in the compilation process, no FASL was generated.
;;; Search above for the \"Error:\" tag to find the error messages.~%")
          (setq *error-p* t)
	  (values nil t t))
        ))
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
	(c-pathname (make-pathname :type "c" :defaults data-pathname))
	(h-pathname (make-pathname :type "h" :defaults data-pathname))
	(o-pathname (make-pathname :type "o" :defaults data-pathname))
	(so-pathname (make-pathname :type "so" :defaults data-pathname)))

    (init-env)

    (with-open-file (*compiler-output-data* data-pathname
					    :direction :output)
      (wt-data-begin)

      (t1expr form)

      (when (zerop *error-count*)
        (when *compile-verbose* (format t "~&;;; End of Pass 1.  "))
        (compiler-pass2 c-pathname h-pathname data-pathname nil "code"))

      (wt-data-end)
      ) ;;; *compiler-output-data* closed.

    (init-env)

    (if (zerop *error-count*)
        (progn
          (when *compile-verbose*
	    (format t "~&;;; Calling the C compiler... "))
          (compiler-cc c-pathname o-pathname)
	  (shared-cc so-pathname o-pathname)
          (delete-file c-pathname)
          (delete-file h-pathname)
	  (delete-file o-pathname)
          (cond ((probe-file so-pathname)
                 (load so-pathname :verbose nil)
                 (when *compile-verbose* (print-compiler-info))
                 (delete-file so-pathname)
                 (delete-file data-pathname)
		 (values (or name (symbol-value 'GAZONK)) nil nil))
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
  (when (or (symbolp thing) (si::setf-namep thing))
    (setq thing (eval `(function ,thing))))
  (cond ((null thing))
	((functionp thing)
	 (unless (si::bc-disassemble thing)
	   (error "The function definition for ~S was lost." thing)))
	((and (consp thing) (eq (car thing) 'LAMBDA))
	 (setq disassembled-form `(defun gazonk ,@(cdr thing))))
	(t (setq disassembled-form thing)))

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
         (*compiler-output-data* (if data-file
				     (open data-file :direction :output)
				     null-stream))
         (*error-count* 0)
         (t3local-fun (symbol-function 'T3LOCAL-FUN))
	 (t3fun (get-sysprop 'DEFUN 'T3)))
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
        (when data-file (wt-data-begin))
        (t1expr disassembled-form)
        (if (zerop *error-count*)
          (catch *cmperr-tag* (ctop-write "code"
					  (if h-file (namestring h-file) "")
					  (if data-file (namestring data-file) "")))
          (setq *error-p* t))
	(when data-file (wt-data-end))
        )
      (put-sysprop 'DEFUN 'T3 t3fun)
      (setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
      (when h-file (close *compiler-output2*))
      (when data-file (close *compiler-output-data*))))
  (values)
  )

(defun compiler-pass2 (c-pathname h-pathname data-pathname system-p init-name)
  (with-open-file (*compiler-output1* c-pathname :direction :output)
    (with-open-file (*compiler-output2* h-pathname :direction :output)
      (wt-nl1 "#include " *cmpinclude*)
      (catch *cmperr-tag* (ctop-write (string-upcase init-name)
				      (namestring h-pathname)
				      (namestring data-pathname)
				      system-p))
      (terpri *compiler-output1*)
      ;; write ctl-z at end to make sure preprocessor stops!
;      #+ms-dos (write-char (code-char 26) *compiler-output1*)
      (terpri *compiler-output2*))))

(defun compiler-cc (c-pathname o-pathname)
  (safe-system
   (format nil
	   *cc-format*
	   *cc* *cc-flags* (>= *speed* 2) *cc-optimize*
	   (namestring (translate-logical-pathname "SYS:"))
	   (namestring c-pathname)
	   (namestring o-pathname))
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
  (let ((tmp (merge-pathnames ".so" file)))
    (shared-cc tmp file)
    (when (probe-file tmp)
      (load tmp :verbose nil :print nil)
      ;(delete-file tmp)
      nil)))

#+dlopen
(push (cons "o" #'load-o-file) si::*load-hooks*)

(defmacro with-compilation-unit (options &rest body)
  `(progn ,@body))

;;; ----------------------------------------------------------------------
(provide "compiler")
