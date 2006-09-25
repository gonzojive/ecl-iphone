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

(in-package "COMPILER")

(defun safe-system (string)
  (cmpnote "Invoking external command:~%;;; ~A~%" string)
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
      ((:shared-library :dll) (setf format +shared-library-format+))
      ((:static-library :library :lib) (setf format +static-library-format+))
      (:data (setf extension "data"))
      (:sdata (setf extension "sdat"))
      (:c (setf extension "c"))
      (:h (setf extension "h"))
      (:object (setf extension +object-file-extension+))
      (:program (setf format +executable-file-format+))
      ((:fasl :fas) (setf extension "fas")))
    (if format
	(merge-pathnames (format nil format (pathname-name output-file))
			 output-file)
	(make-pathname :type extension :defaults output-file))))

#+msvc
(defun delete-msvc-generated-files (output-pathname)
  (loop for i in '("lib" "exp" "ilk" "pdb")
        do (let ((the-pathname (merge-pathnames (make-pathname :type i) output-pathname)))
	     (when (probe-file the-pathname)
	       (cmp-delete-file the-pathname)))))

(defun cmp-delete-file (file)
  (if *debug-compiler*
      (progn
	(format t "~%Postponing deletion of ~A" file)
	(push file *files-to-be-deleted*))
      (delete-file file)))

(push #'(lambda () (mapc #'delete-file *files-to-be-deleted*))
      si::*exit-hooks*)

(defun linker-cc (o-pathname &rest options)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (ecl-library-directory)
	   options
	   *ld-flags* (ecl-library-directory))))

#+dlopen
(defun shared-cc (o-pathname &rest options)
  #-(or mingw32)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (ecl-library-directory)
	   options
	   *ld-shared-flags* (ecl-library-directory)))
  #+(or mingw32)
  (let ((lib-file (compile-file-pathname o-pathname :type :lib)))
    (safe-system
     (format nil
	     "dllwrap --export-all-symbols -o ~S -L~S ~{~S ~} ~@?"
	     (si::coerce-to-filename o-pathname)
	     (ecl-library-directory)
	     options
	     *ld-shared-flags*
	     (ecl-library-directory)))))

#+dlopen
(defun bundle-cc (o-pathname &rest options)
  #-(or mingw32)
  (safe-system
   (format nil
	   *ld-format*
	   *cc*
	   (si::coerce-to-filename o-pathname)
	   (ecl-library-directory)
	   options
	   #-msvc *ld-bundle-flags*
	   #+msvc (concatenate 'string *ld-bundle-flags* " /EXPORT:init_CODE")
	   (ecl-library-directory)))
  #+(or mingw32)
  (safe-system
   (format nil
	   "dllwrap -o ~A --export-all-symbols -L~S ~{~S ~} ~@?"
	   (si::coerce-to-filename o-pathname)
	   (ecl-library-directory)
	   options
	   *ld-bundle-flags*
	   (ecl-library-directory))))

(defconstant +lisp-program-header+ "
#include <ecl/ecl.h>

#ifdef __cplusplus
#define ECL_CPP_TAG \"C\"
#else
#define ECL_CPP_TAG
#endif

~{	extern ECL_CPP_TAG void ~A(cl_object);~%~}

")

;;
;; This format string contains the structure of the code that initializes
;; a program, a library, a module, etc. Basically, it processes a codeblock
;; just like in a normal compiled file, but then adds all the codeblocks of
;; its corresponding modules.
;;
;; IMPORTANT: Notice how the modules are linked to the parent forming a
;; circular chain. This disables the garbage collection of the library until
;; _ALL_ functions in all modules are unlinked.
;;
(defconstant +lisp-program-init+ "
#ifdef __cplusplus
extern \"C\"
#endif
void ~A(cl_object cblock)
{
	static cl_object Cblock;
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
{
	cl_object current, next = Cblock;
~:[~{	current = read_VV(OBJNULL, ~A); current->cblock.next = next; next = current; ~%~}
	Cblock->cblock.next = current;
~;~{	~A(Cblock);~%~}~]
}
	~A
}")

(defconstant +lisp-program-main+ "
int
main(int argc, char **argv)
{
	~A
	cl_boot(argc, argv);
	read_VV(OBJNULL, ~A);
	~A
}")

#+:win32
(defconstant +lisp-program-winmain+ "
#include <windows.h>
int
WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPSTR lpCmdLine, int nCmdShow)
{
	char **argv;
	int argc;
	~A
	ecl_get_commandline_args(&argc, &argv);
	cl_boot(argc, argv);
	read_VV(OBJNULL, ~A);
	~A
}")

(defun init-function-name (s &optional (si::*init-function-prefix* si::*init-function-prefix*))
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
		 ((digit-char-p c)
		  c)
		 (t
		  #\p))))
    (setq s (map 'string #'translate-char (string s)))
    (concatenate 'string "init_"
		 (if si::*init-function-prefix*
		     (concatenate 'string si::*init-function-prefix* "_" s)
		   s))))

(defun builder (target output-name &key lisp-files ld-flags shared-data-file
		(init-name nil)
		(prologue-code "")
		(epilogue-code (when (eq target :program) '(SI::TOP-LEVEL)))
		#+:win32 (system :console))
  ;;
  ;; The epilogue-code can be either a string made of C code, or a
  ;; lisp form.  In the latter case we add some additional C code to
  ;; clean up, and the lisp form is stored in a text representation,
  ;; to avoid using the compiler.
  ;;
  (cond ((null epilogue-code)
	 (setf epilogue-code ""))
	((stringp epilogue-code)
	 )
	(t
	 (with-standard-io-syntax
	   (setq epilogue-code
		 (with-output-to-string (stream)
		   (princ "{ const char *lisp_code = " stream)
		   (wt-filtered-data (write-to-string epilogue-code) stream)
		   (princ ";
cl_object output;
si_select_package(make_simple_base_string(\"CL-USER\"));
output = cl_safe_eval(c_string_to_object(lisp_code), Cnil, OBJNULL);
" stream)
		   (when (eq target :program)
		     (princ "cl_shutdown(); return (output != OBJNULL);" stream))
		   (princ #\} stream)
		   )))))
  ;;
  ;; When a module is built out of several object files, we have to
  ;; create an additional object file that initializes those ones.
  ;; This routine is responsible for creating this file.
  ;;
  ;; To avoid name clashes, this object file will have a temporary
  ;; file name (tmp-name).
  ;;
  (let* ((tmp-name (si::mkstemp #P"TMP:ECLINIT"))
	 (c-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :c)))
	 (o-name (si::coerce-to-filename
		  (compile-file-pathname tmp-name :type :object)))
	 submodules
	 c-file)
    (dolist (item (reverse lisp-files))
      (cond ((symbolp item)
	     (push (format nil #-msvc "-l~A" #+msvc "~A.lib" (string-downcase item)) ld-flags)
	     (push (init-function-name item nil) submodules))
	    (t
	     (push (si::coerce-to-filename
		    (compile-file-pathname item :type :object)) ld-flags)
	     (setq item (pathname-name item))
	     (push (init-function-name item) submodules))))
    (setq c-file (open c-name :direction :output))
    (format c-file +lisp-program-header+ 
            #-(or :win32 :mingw32 :darwin) (if (eq :fasl target) nil submodules)
            #+(or :win32 :mingw32 :darwin) submodules)
    (cond (shared-data-file
	   (data-init shared-data-file)
	   (format c-file "
#define VM ~A
#ifdef ECL_DYNAMIC_VV
static cl_object *VV;
#else
static cl_object VV[VM];
#endif
#define ECL_SHARED_DATA_FILE 1
" (data-permanent-storage-size))
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
       (unless init-name
	 (setf init-name (init-function-name (pathname-name output-name) nil)))
       (format c-file +lisp-program-init+ init-name "" shared-data-file
	       submodules "")
       (format c-file #+:win32 (ecase system (:console +lisp-program-main+)
				             (:windows +lisp-program-winmain+))
	              #-:win32 +lisp-program-main+
		      prologue-code init-name epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'linker-cc output-name (namestring o-name) ld-flags))
      ((:library :static-library :lib)
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :lib)))
       (unless init-name
	 ;; Remove the leading "lib"
	 (setf init-name (subseq (pathname-name output-name) (length +static-library-prefix+)))
	 (setf init-name (init-function-name init-name nil)))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       #-msvc
       (progn
       (safe-system (format nil "ar cr ~A ~A ~{~A ~}"
			    output-name o-name ld-flags))
       (safe-system (format nil "ranlib ~A" output-name)))
       #+msvc
       (unwind-protect
         (progn
           (with-open-file (f "static_lib.tmp" :direction :output :if-does-not-exist :create :if-exists :supersede)
             (format f "/DEBUGTYPE:CV /OUT:~A ~A ~{~&\"~A\"~}"
                     output-name o-name ld-flags))
           (safe-system "link -lib @static_lib.tmp"))
         (when (probe-file "static_lib.tmp")
           (cmp-delete-file "static_lib.tmp")))
       )
      #+dlopen
      ((:shared-library :dll)
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :dll)))
       (unless init-name
	 ;; Remove the leading "lib"
	 (setf init-name (subseq (pathname-name output-name)
				 (length +static-library-prefix+)))
	 (setf init-name (init-function-name init-name nil)))
       (format c-file +lisp-program-init+ init-name prologue-code
	       shared-data-file submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'shared-cc output-name o-name ld-flags))
      #+dlopen
      (:fasl
       (when (or (symbolp output-name) (stringp output-name))
	 (setf output-name (compile-file-pathname output-name :type :fasl)))
       (unless init-name
	 (setf init-name (init-function-name "CODE" nil)))
       #-(or :win32 :mingw32 :darwin)
       (setf submodules
	     (mapcar #'(lambda (sm)
			 (format nil "((ecl_init_function_t) ecl_library_symbol(Cblock, \"~A\", 0))" sm))
		     submodules))
       (format c-file +lisp-program-init+ init-name prologue-code shared-data-file
	       submodules epilogue-code)
       (close c-file)
       (compiler-cc c-name o-name)
       (apply #'bundle-cc output-name o-name ld-flags)))
    (cmp-delete-file tmp-name)
    (cmp-delete-file c-name)
    (cmp-delete-file o-name)
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
                      &key
		      (verbose *compile-verbose*)
		      (print *compile-print*)
		      (c-file nil)
		      (h-file nil)
		      (data-file nil)
		      (shared-data-file nil)
		      (system-p nil)
		      (load nil)
		      (output-file 'T output-file-p)
                      &aux (*standard-output* *standard-output*)
                           (*error-output* *error-output*)
                           (*compiler-in-use* *compiler-in-use*)
                           (*package* *package*)
			   (*print-pretty* nil)
                           (*error-count* 0)
			   (*compile-file-pathname* nil)
			   (*compile-file-truename* nil)
			   (*compile-verbose* verbose)
			   (*suppress-compiler-notes* (not verbose))
			   #+PDE sys:*source-pathname*)
  (declare (notinline compiler-cc))

  #-dlopen
  (unless system-p
    (format t "~%;;;~
~%;;; This system does not support loading dynamically linked libraries.~
~%;;; Therefore, COMPILE-FILE without :SYSTEM-P T is unsupported.~
~%;;;"))

  (setq *compile-file-pathname* input-pathname)
  (unless (probe-file *compile-file-pathname*)
    (if (pathname-type input-pathname)
	(error 'file-error :pathname input-pathname)
	(dolist (ext '("lsp" "LSP" "lisp" "LISP")
		 (error 'file-error :pathname input-pathname))
	  (setq *compile-file-pathname* (make-pathname :type ext :defaults input-pathname))
	  (when (probe-file *compile-file-pathname*)
	    (return)))))
  (setq *compile-file-truename* (truename *compile-file-pathname*))

  (when (eq output-file 'T)
    (setf output-file (compile-file-pathname *compile-file-truename* :type (if system-p :object :fasl))))

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
	 (o-pathname (or #+dlopen (and system-p output-file)
			 #-dlopen output-file
			 (compile-file-pathname (or output-file input-pathname) :type :object)))
	 #+dlopen
	 (so-pathname (unless system-p output-file))
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
			    (pathname-name output-file)
			    "code")
			shared-data-file))

      (if shared-data-file
	  (data-dump shared-data-pathname t)
	  (data-dump data-pathname))

      (init-env)
      );; with-lock

    (if (zerop *error-count*)
        (progn
          (cond (output-file
		 (when *compile-verbose*
		   (format t "~&;;; Calling the C compiler... "))
                 (compiler-cc c-pathname o-pathname)
		 #+dlopen
		 (unless system-p (bundle-cc (si::coerce-to-filename so-pathname)
					     (si::coerce-to-filename o-pathname)))
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
          (unless c-file (cmp-delete-file c-pathname))
          (unless h-file (cmp-delete-file h-pathname))
          (unless (or data-file shared-data-file)
	    (cmp-delete-file data-pathname))
	  #+dlopen
	  (unless system-p (cmp-delete-file o-pathname))
	  (values (truename #+dlopen (if system-p o-pathname so-pathname)
			    #-dlopen o-pathname)
		  nil nil))
        (progn
          (when (probe-file c-pathname) (cmp-delete-file c-pathname))
          (when (probe-file h-pathname) (cmp-delete-file h-pathname))
          (when (probe-file data-pathname) (cmp-delete-file data-pathname))
          (when (probe-file shared-data-pathname) (cmp-delete-file shared-data-pathname))
	  (when (probe-file o-pathname) (cmp-delete-file o-pathname))
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
		      (*compiler-constants* t)
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
	 (when (functionp def)
	   (unless (function-lambda-expression def)
	     (return-from compile def))
	   (setf def (function-lambda-expression def)))
         (setq form (if name
                        `(setf (symbol-function ',name) #',def)
                        `(set 'GAZONK #',def))))
	((not (fboundp name))
	 (error "Symbol ~s is unbound." name))
	((typep (setf def (symbol-function name)) 'standard-generic-function)
	 (warn "COMPILE can not compile generic functions yet")
	 (return-from compile (values def t nil)))
	((null (setq form (function-lambda-expression def)))
	 (warn "We have lost the original function definition for ~s. Compilation to C failed")
	 (return-from compile (values def t nil)))
	(t
	 (setq form `(setf (symbol-function ',name) #',form))))

  (let ((template (format nil "TMP:ECL~3,'0x" (incf *gazonk-counter*))))
    (unless (setq data-pathname (si::mkstemp template))
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
	(let (#+(or mingw32 msvc)(*self-destructing-fasl* t))
	  (compiler-pass2 c-pathname h-pathname data-pathname nil "code" nil)))
      (setf *compiler-constants* (data-dump data-pathname))
      (init-env)
      )

    (if (zerop *error-count*)
        (progn
          (when *compile-verbose*
	    (format t "~&;;; Calling the C compiler... "))
          (compiler-cc c-pathname o-pathname)
	  (bundle-cc (si::coerce-to-filename so-pathname)
		     (si::coerce-to-filename o-pathname))
          (cmp-delete-file c-pathname)
          (cmp-delete-file h-pathname)
	  (cmp-delete-file o-pathname)
	  (cmp-delete-file data-pathname)
          (cond ((probe-file so-pathname)
                 (load so-pathname :verbose nil)
		 #-(or mingw32 msvc)(cmp-delete-file so-pathname)
		 #+msvc (delete-msvc-generated-files so-pathname)
                 (when *compile-verbose* (print-compiler-info))
		 (setf name (or name (symbol-value 'GAZONK)))
		 ;; By unsetting GAZONK we avoid spurious references to the
		 ;; loaded code.
		 (set 'GAZONK nil)
		 (si::gc t)
		 (values name nil nil))
		(t (format t "~&;;; The C compiler failed to compile~
			~the intermediate code for ~s.~%" name)
                   (setq *error-p* t)
		   (values name t t))))
        (progn
          (when (probe-file c-pathname) (cmp-delete-file c-pathname))
          (when (probe-file h-pathname) (cmp-delete-file h-pathname))
          (when (probe-file so-pathname) (cmp-delete-file so-pathname))
          (when (probe-file data-pathname) (cmp-delete-file data-pathname))
	  #+msvc (delete-msvc-generated-files so-pathname)
          (format t "~&;;; Failed to compile ~s.~%" name)
          (setq *error-p* t)
          (values name t t)))))

(defun disassemble (thing &key (h-file nil) (data-file nil)
		    &aux def disassembled-form
		    (*compiler-in-use* *compiler-in-use*)
		    (*print-pretty* nil))
  (when (si::valid-function-name-p thing)
    (setq thing (fdefinition thing)))
  (cond ((null thing))
	((functionp thing)
	 (unless (si::bc-disassemble thing)
	   (warn "Cannot disassemble the binary function ~S because I do not have its source code." thing)
	   (return-from disassemble nil)))
	((atom thing)
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing)))
	((eq (car thing) 'LAMBDA)
	 (setq disassembled-form `(defun gazonk ,@(cdr thing))))
	((eq (car thing) 'EXT:LAMBDA-BLOCK)
	 (setq disassembled-form `(defun ,@(rest thing))))
	(t
	 (error 'simple-type-error
		:datum thing
		:expected-type '(OR FUNCTION (SATISFIES SI:VALID-FUNCTION-NAME-P))
		:format-control "DISASSEMBLE cannot accept ~A"
		:format-arguments (list thing))))
  (when *compiler-in-use*
    (format t "~&;;; The compiler was called recursively.~
                   ~%Cannot disassemble ~a." thing)
    (setq *error-p* t)
    (return-from disassemble nil))
  (setq *error-p* nil
	*compiler-in-use* t)

  (let* ((null-stream (make-broadcast-stream))
         (*compiler-output1* null-stream)
         (*compiler-output2* (if h-file
				 (open h-file :direction :output)
				 null-stream))
         (*error-count* 0)
         (t3local-fun (symbol-function 'T3LOCAL-FUN)))
    (with-lock (+load-compile-lock+)
      (unwind-protect
	   (progn
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
	(setf (symbol-function 'T3LOCAL-FUN) t3local-fun)
	(when h-file (close *compiler-output2*)))))
  nil
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

(defun ecl-include-directory ()
  "Finds the directory in which the header files were installed."
  (cond ((and *ecl-include-directory*
	      (probe-file (merge-pathnames "ecl/config.h" *ecl-include-directory*)))
	 *ecl-include-directory*)
	((probe-file "SYS:ecl;config.h")
	 (setf *ecl-include-directory* (namestring (translate-logical-pathname "SYS:"))))
	((error "Unable to find include directory"))))

(defun ecl-library-directory ()
  "Finds the directory in which the ECL core library was installed."
  (cond ((and *ecl-library-directory*
	      (probe-file (merge-pathnames (compile-file-pathname "ecl" :type
					    #+dlopen :shared-library
					    #-dlopen :static-library)
					   *ecl-library-directory*)))
	 *ecl-library-directory*)
	((probe-file "SYS:BUILD-STAMP")
	 (setf *ecl-library-directory* (namestring (translate-logical-pathname "SYS:"))))
	((error "Unable to find library directory"))))

(defun compiler-cc (c-pathname o-pathname)
  (safe-system
   (format nil
	   *cc-format*
	   *cc* *cc-flags* (>= *speed* 2) *cc-optimize*
	   (ecl-include-directory)
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
  (format t "~&;;; OPTIMIZE levels: Safety=~d, Space=~d, Speed=~d~%"
	  *safety* *space* *speed*))

#+dlopen
(defun load-o-file (file verbose print)
  (let ((tmp (compile-file-pathname file)))
    (bundle-cc tmp file)
    (when (probe-file tmp)
      (load tmp :verbose nil :print nil)
      (cmp-delete-file tmp)
      nil)))

#+dlopen
(push (cons +object-file-extension+ #'load-o-file) si::*load-hooks*)

(defmacro with-compilation-unit (options &rest body)
  `(progn ,@body))

(si::package-lock "CL" nil)

#-ecl-min
(load "sys:sysfun")

(provide 'cmp)
