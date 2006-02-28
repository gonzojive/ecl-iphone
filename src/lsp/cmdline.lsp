;;;;  cmdline.lsp -- command line processing
;;;;
;;;;  Copyright (c) 2005, Juan Jose Garcia-Ripoll
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.
;;;;

(in-package "SYSTEM")

(export '(*lisp-init-file-list* command-args process-command-args))

(defvar *lisp-init-file-list* '("~/.ecl" "~/.eclrc")
  "List of files automatically loaded when ECL is invoked.")

(defun command-args ()
  "Returns the command line arguments as list"
  (loop for i from 0 below (argc)
	collect (argv i)))

(defun help-message (stream)
  "Prints a help message about command line arguments of ECL"
  (princ "
Usage: ecl [-? | --help]
           [-dir dir] [-load file] [-shell file] [-eval expr] [-rc | -norc]
           [[-o ofile] [-c [cfile]] [-h [hfile]] [-data [datafile]] [-s] [-q]
            -compile file]

"
	 stream))

(defun command-arg-error (str &rest fmt-args)
  ;; Format an error message and quit
  (declare (si::c-local))
  (apply #'format *error-output* str fmt-args)
  (help-message *error-output*)
  (quit 1))

(defconstant +command-arg-rules+
  '(("--help" 0 #0=(progn (help-message *standard-output*) (quit)) :noloadrc)
    ("-?" 0 #0# :noloadrc)
    ("-h" 0 #0# :noloadrc)
    ("-norc" 0 nil :noloadrc)
    ("--" 0 nil :stop)
    ("-eval" 1 (eval (read-from-string 1)))
    ("-shell" 1 (progn (setq quit 0) (load 1 :verbose nil)))
    ("-load" 1 (load 1 :verbose verbose))
    ("-dir" 1 (setf (logical-pathname-translations "SYS")
	       `(("**;*.*" ,(merge-pathnames "**/*.*" (truename 1))))))
    ("-compile" 1
     (progn
       (setq quit
	     (if (nth-value 3
		     (compile-file 1 :output-file output-file :c-file c-file
				   :h-file h-file :data-file data-file
				   :verbose verbose :system-p system-p))
		 1
		 0)
	     output-file t
	     c-file nil
	     h-file nil
	     data-file nil
	     system-p nil)))
    ("-o" 1 (setq output-file 1))
    ("-c" 1 (setq c-file 1))
    ("-h" 1 (setq h-file 1))
    ("-data" 1 (setq data-file 1))
    ("-q" 0 (setq verbose nil))
    ("-s" 0 (setq system-p t))))

(defun produce-init-code (option-list rules error-on-unknown)
  (declare (si::c-local))
  (do* ((commands '())
	(loadrc t))
       ((null option-list)
	(values `(let ((output-file t)
		       (c-file nil)
		       (h-file nil)
		       (data-file nil)
		       (verbose t)
		       (system-p nil)
		       (quit nil))
		   ,@(nreverse commands)
		   (when quit (quit 0)))
		loadrc))
    (let* ((option (pop option-list))
	   (rule (assoc option rules :test #'string=)))
      (if (null rule)
	  (if error-on-unknown
	      (command-arg-error "Unknown command line option ~A.~%" option)
	      (setf option-list nil))
	  (let ((pattern (copy-tree (third rule))))
	    (case (fourth rule)
	      (:noloadrc (setf loadrc nil))
	      (:loadrc (setf loadrc t))
	      (:stop (setf option-list nil)))
	    (let ((pattern (copy-tree (third rule))))
	      (unless (zerop (second rule))
		(when (null option-list)
		  (command-arg-error
		   "Missing argument after command line option ~A.~%"
		   option))
		(nsubst (pop option-list) 1 pattern))
	      (push pattern commands)))))))

(defun process-command-args (&key
			     (args (rest (command-args)))
			     (rules +command-arg-rules+)
			     (error-on-unknown t))
"PROCESS-COMMAND-ARGS takes a list of arguments and processes according
to a set of rules. These rules are of the format

	(option-name nargs template [ :stop | :noloadrc | :loadrc ] )

OPTION-NAME is a string containing the command line option. NARGS is
the number of arguments that this option takes. TEMPLATE is a lisp
form where numbers from 0 to NARGS will be substituted by the
arguments, and which will be evaluated afterwards. The flags :STOP,
:NOLOADRC and :LOADRC denote whether to stop processing the command
line after this option and whether the initialization file will be
loaded before evaluating all forms.

An excerpt of the rules used by ECL:
'((\"--help\" 0 #0=(progn (help-message *standard-output*) (quit)) :noloadrc)
  (\"-?\" 0 #0# :noloadrc)
  (\"-h\" 0 #0# :noloadrc)
  (\"-norc\" 0 nil :noloadrc)
  (\"--\" 0 nil :stop)
  (\"-eval\" 1 (eval (read-from-string 1))))
"
  (multiple-value-bind (commands loadrc)
      (produce-init-code args rules error-on-unknown)
    (handler-case
	(progn
	  (when loadrc
	    (dolist (file *lisp-init-file-list*)
	      (when (load file :if-does-not-exist nil :search-list nil :verbose nil)
		(return))))
	  (eval commands))
      (error (c)
	(format *error-output*
		"An error occurred during initialization:~%~A.~%" c)
	(quit 1)))))
