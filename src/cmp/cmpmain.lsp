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

(defvar *cmpinclude* "<ecls-cmp.h>")
;;; This is copied into each .h file generated, EXCEPT for system-p calls.
;;; The constant string *include-string* is the content of file "ecl.h".
;;; Here we use just a placeholder: it will be replaced with sed.

(defvar *cc* "cc"))
(defvar *cc-flags* "-g -I.")
(defvar *cc-optimize* "-O")		; C compiler otimization flag
(defvar *cc-format* "~A ~A ~:[~*~;~A~] -I~A/h -w -c ~A -o ~A"))
;(defvar *cc-format* "~A ~A ~:[~*~;~A~] -I~A/h -c ~A -o ~A"))
(defvar *ld-flags* "")
(defvar *ld-format* "~A -w -o ~A -L~A ~{~A ~} -llsp ~A")

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

(defun library-pathname (name &optional (directory "./"))
  (make-pathname :name (concatenate 'string "lib" name) :type "a" :defaults directory))

(defun compile-file-pathname (name &key output-file)
  (merge-pathnames (or output-file name) #P".o"))

(defun make-library (lib objects &key (output-dir "./"))
  (let* ((lib (string-upcase lib))
	 (init-name (mapcar #'(lambda (x) (string-upcase (pathname-name x)))
			    objects))
	 (liba (library-pathname (string-downcase lib) output-dir))
	 (libc (make-pathname :name lib :type "c" :defaults output-dir))
	 (libo (make-pathname :name lib :type "o" :defaults output-dir)))
    (with-open-file (libc-file libc :direction :output)
      (format libc-file
	      "
void
init_~A(cl_object)
{
~{	extern void init_~A(cl_object);~%~}
~{	read_VV((void*)0,init_~A);~%~}
}
"
	      lib init-name init-name)
    (compiler-cc libc libo)
    (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			 (namestring liba) (namestring libo) objects))
    (delete-file (namestring libc))
    (delete-file (namestring libo)))
    liba))

(defun linker-cc (o-pathname options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc* (namestring o-pathname)
	   (namestring (translate-logical-pathname "SYS:"))
	   options *ld-flags*)))

(defun rsym (name)
  (let ((output (make-pathname :name (pathname-name name) :type "sym"))
	(rsym (translate-logical-pathname "SYS:rsym")))
    (cond ((not (probe-file rsym))
	   (error "rsym executable not found"))
	  ((not (probe-file name))
	   (error "executable to be scanned not found"))
	  (t
	   (safe-system (format nil "~A ~A ~A" rsym name output))))))

(defun build-ecls (name &rest components)
  (let ((c-name (make-pathname :name name :type "c"))
	(o-name (make-pathname :name name :type "o"))
	(ld-flags nil))
    (with-open-file (c-file c-name :direction :output)
      (format c-file "
#include \"ecls.h\"

extern cl_object lisp_package;
#ifdef RSYM
extern cl_object siVsymbol_table;
#endif
void
init_lisp_libs(void)
{
	init_LSP();
	siLpackage_lock(2, lisp_package, Ct);~%")
      (dolist (item (append '(#+clos clos) components))
	(cond ((symbolp item)
	       (format c-file "	init_~A();~%" (string-upcase item))
	       (setq ld-flags (nconc ld-flags (list (format nil "-l~A" (string-downcase item))))))
	      ((stringp item)
	       (setq ld-flags (nconc ld-flags (list item))))
	      (t
	       (error "compiler::build-ecls wrong argument ~A" item))))
      (format c-file "
#ifdef RSYM
	SYM_VAL(siVsymbol_table) = make_simple_string(\"SYS:~A.sym\");
#endif
	return;~%}~%" name))
    (compiler-cc c-name o-name)
    (linker-cc name (cons (namestring o-name) ld-flags))
    (rsym name)
    (delete-file c-name)
    ))

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
			   #+PDE sys:*source-pathname*)
  (declare (notinline compiler-cc))

  (setq input-pathname (merge-pathnames input-pathname #".lsp"))

  #+PDE (setq sys:*source-pathname* (truename input-pathname))

  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~%~
Cannot compile ~a."
	    (namestring input-pathname))
    (setq *error-p* t)
    (return-from compile-file (values)))

  (setq *error-p* nil
	*compiler-in-use* t)

  (unless (probe-file input-pathname)
    (format t "~&;;; The source file ~a is not found.~%"
            (namestring input-pathname))
    (setq *error-p* t)
    (return-from compile-file (values)))

  (when *compile-verbose*
    (format t "~&;;; Compiling ~a."
            (namestring input-pathname)))

  (let* ((eof '(NIL))
	 (output-default (if (or (eq output-file 'T)
				 (null output-file))
			     input-pathname
			     output-file))
         (directory (pathname-directory output-default))
         (name (pathname-name output-default))
         (o-pathname (get-output-pathname output-file "o"))
         (c-pathname (get-output-pathname c-file "c"))
         (h-pathname (get-output-pathname h-file "h"))
         (data-pathname (get-output-pathname data-file "data")))

    (init-env)

    (when (probe-file "./cmpinit.lsp")
      (load "./cmpinit.lsp" :verbose *compile-verbose*))

    (with-open-file (*compiler-output-data*
                     data-pathname :direction :output)
      (wt-data-begin)

      (with-open-file
          (*compiler-input* input-pathname)
        (let* ((rtb *readtable*)
               (prev (when (eq (get-macro-character #\# rtb)
			       (get-macro-character
				#\# (si:standard-readtable)))
		       (get-dispatch-macro-character #\# #\, rtb))))
          (if (and prev (eq prev (get-dispatch-macro-character
                                   #\# #\, (si:standard-readtable))))
              (set-dispatch-macro-character #\# #\,
                'SYS:SHARP-COMMA-READER-FOR-COMPILER rtb)
              (setq prev nil))
          (unwind-protect
            (do ((form (read *compiler-input* nil eof)
                       (read *compiler-input* nil eof)))
                ((eq form eof))
              (t1expr form))
            (when prev (set-dispatch-macro-character #\# #\, prev rtb)))))

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
                 (cond ((probe-file o-pathname)
                        (when load (load o-pathname))
                        (when *compile-verbose*
			  (print-compiler-info)
			  (format t "~&;;; Finished compiling ~a."
				  (namestring input-pathname))))
		       #+(or SYSTEM-V APOLLO) ;tito 
		       ((probe-file (setq ob-name
					  (format nil "~a.o"
						  (pathname-name o-pathname))))
			(si:system (format nil "mv ~A ~A" (namestring ob-name)
					    (namestring o-pathname)))
                        (when load (load o-pathname))
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
	  o-pathname)

        (progn
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (format t "~&;;; No FASL generated.~%")
          (setq *error-p* t)
	  (values))
        ))
  )

(defun compile (name &optional (def nil supplied-p)
                      &aux form gazonk-name
                      data-pathname
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
    (return-from compile))

  (setq *error-p* nil
	*compiler-in-use* t)

  (cond ((and supplied-p def)
         (unless (and (consp def) (eq (car def) 'LAMBDA))
                 (error "~s is invalid lambda expression." def))
         (setq form (if name
                        `(defun ,name ,@(cdr def))
                        `(set 'GAZONK #',def))))
        ((and (fboundp name)
              (consp (setq def (symbol-function name))))
         (cond ((and (eq (car def) 'LAMBDA-BLOCK)
                     (consp (cdr def)) (consp (cddr def)))
                (if (eq (cadr def) name)
                    (setq form `(defun ,name ,@(cddr def)))
                    (setq form `(defun ,name ,(caddr def)
                                  (block ,(cadr def) ,@(cdddr def))))))
               ((eq (car def) 'LAMBDA)
                (setq form `(defun ,name ,@(cdr def))))
               ((and (eq (car def) 'LAMBDA-CLOSURE)
                     (consp (cdr def)) (null (cadr def))
                     (consp (cddr def)) (null (caddr def))
                     (consp (cdddr def)) (null (cadddr def)))
                (setq form `(defun ,name ,@(cddddr def))))
               ((and (eq (car def) 'LAMBDA-BLOCK-CLOSURE)
                     (consp (cdr def)) (null (cadr def))
                     (consp (cddr def)) (null (caddr def))
                     (consp (cdddr def)) (null (cadddr def))
                     (consp (cddddr def)))
                (setq form `(defun ,name
                              (block ,(car (cddddr def))
                                ,@(cdr (cddddr def))))))
               (t (error "I cannot compile such ~Ss, sorry." (car def)))))
        (t (error "No lambda expression is assigned to the symbol ~s." name)))

  (dotimes (n 1000
              (progn
                (format t "~&;;; The name space for GAZONK files exhausted.~%~
;;; Delete one of your GAZONK*** files before compiling ~s." name)
                (setq *error-p* t)
                (return-from compile (values))))
    (setq gazonk-name (format nil "gazonk~3,'0d" n))
    (setq data-pathname (make-pathname :name gazonk-name :type "data"))
    (unless (probe-file data-pathname)
      (return)))

  (let ((c-pathname (make-pathname :name gazonk-name :type "c"))
        (h-pathname (make-pathname :name gazonk-name :type "h"))
        (o-pathname (make-pathname :name gazonk-name :type "o")))

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
          (delete-file c-pathname)
          (delete-file h-pathname)
          (cond ((probe-file o-pathname)
                 (load o-pathname :verbose nil)
                 (when *compile-verbose* (print-compiler-info))
                 (delete-file o-pathname)
                 (delete-file data-pathname))
                (t (delete-file data-pathname)
                   (format t "~&;;; The C compiler failed to compile~
			~the intermediate code for ~s.~%" name)
                   (setq *error-p* t)))
	  (or name (symbol-value 'GAZONK)))

        (progn
	  (print c-pathname)
          (when (probe-file c-pathname) (delete-file c-pathname))
          (when (probe-file h-pathname) (delete-file h-pathname))
          (when (probe-file data-pathname) (delete-file data-pathname))
          (format t "~&;;; Failed to compile ~s.~%" name)
          (setq *error-p* t)
          name))))

(defun disassemble (&optional (thing nil)
			      &key (h-file nil) (data-file nil)
			      &aux def disassembled-form
			      (*compiler-in-use* *compiler-in-use*)
			      (*print-pretty* nil))
 (when *compiler-in-use*
   (format t "~&;;; The compiler was called recursively.~
                   ~%Cannot disassemble ~a." thing)
   (setq *error-p* t)
   (return-from disassemble))
 (setq *error-p* nil
       *compiler-in-use* t)

 (cond ((null thing))
       ((symbolp thing)
	(setq def (symbol-function thing))
	(when (macro-function thing)
	  (setq def (cdr def)))
	(if (and (consp def)
		 (eq (car def) 'LAMBDA-BLOCK)
		 (consp (cdr def)))
	    (setq disassembled-form `(defun ,thing ,@(cddr def)))
	    (error "The function object ~s cannot be disassembled." def)))
       ((and (consp thing) (eq (car thing) 'LAMBDA))
	(setq disassembled-form `(defun gazonk ,@(cdr thing))))
       (t (setq disassembled-form thing)))

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
	 (t3fun (get 'DEFUN 'T3)))
    (unwind-protect
      (progn
        (setf (get 'DEFUN 'T3)
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
      (setf (get 'DEFUN 'T3) t3fun)
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

;;; ----------------------------------------------------------------------
(provide "compiler")
