;; Test-Suiten ablaufen lassen: -*- mode: lisp -*-
(in-package :user)

(declaim (optimize (speed 0)
		   (safety 3)
		   (debug 3)))

(defmacro with-ignored-errors (&rest forms)
  "This macro will evaluate the forms and return
the returnvalues or the type of the condition used."
  (let ((tag (gensym)))
    `(block ,tag
       (handler-bind
	((serious-condition
	  #'(lambda (condition)
	      (return-from ,tag
		(values :ERROR
			condition)))))
	,@forms))))

(defvar *log* nil)
(defvar *output-generated* nil)
(defvar *lisp-type*
  #+ecl "ECL"
  #+CLISP "CLISP"
  #+AKCL "AKCL"
  #+CMU "CMUCL"
  #+sbcl "SBCL")

(defun check-and-puke (mode form result my-result condition why)
  (flet ((safe-format (stream string &rest args)
		      (unless (ignore-errors
				(progn
				  (apply #'format stream string args)
				  t))
			(format stream "~&~%format of ~S failed!"
				string))))
    (cond
     ((eql result my-result)
      (safe-format t "~%EQL-OK: ~S" my-result))
     ((equal result my-result)
      (safe-format t "~%EQUAL-OK: ~S" my-result))
     ((equalp result my-result)
      (safe-format t "~%EQUALP-OK: ~S" my-result))
     ((eq my-result :ERROR)
      (cond
       ((ignore-errors
	  (typep condition result))
	(safe-format t "~%TYPEP-OK, is of the expected error :~S"
		     result))
       (t
	(safe-format
	 t
	 "~&~%ERROR!! Got an error ~S (~A) I expected a instance of ~S~%"
	 condition condition
	 result)
	(safe-format
	 t
	 "~%Form: ~S~%Should be an error of type: ~S~%~A: ~S (~A)~%Why: ~S~%"
	 form result *lisp-type*
	 condition condition
	 why)
	(setf *output-generated* t)
	(safe-format
	 *log*
	 "~&~%~A Form: ~S~%Should be an error of type: ~S~%~A: ~S (~A) ~%Why: ~S~%"
	 mode form result *lisp-type*
	 condition condition
	 why))))
     (t
      (safe-format t
		   "~&~%ERROR!! Got ~S solution ~S expected!"
		   my-result result)
      (safe-format t
		   "~%~A Form: ~S~%Should be: ~S~%~A: ~S~%Why: ~S~%"
		   mode form result *lisp-type*
		   my-result why)
      (setf *output-generated* t)
      (safe-format *log*
		   "~&~%~A Form: ~S~%Should be: ~S~%~A: ~S~%Why : ~S~%"
		   mode form result *lisp-type*
		   my-result why)))))

(defmacro my-assert (form result &optional (why ""))
  `(progn
     (format t "~&~%testing : ~S~%"
	     ',form)

     ;;; first we check if it work in interpreted mode

     (multiple-value-bind (my-result condition)
	 (with-ignored-errors
	  (eval ',form))
       (check-and-puke "interpreted"
		       ',form ',result
		       my-result condition
		       ,why))

     (force-output)

     ;;; now we try to compile...
     #+nil ; HACK
     (multiple-value-bind (my-result condition)
	 (with-ignored-errors
	  (multiple-value-bind (function warnings-p failure-p)
	      (compile nil
		       #'(lambda ()
			   ,form))
	    (format t "~&compiled  ~S ~S ~S"
		    function warnings-p failure-p)

	    (multiple-value-bind (my-result condition)
		(with-ignored-errors
		 (funcall function))
	      (check-and-puke "compiled"
			      ',form ',result
			      my-result condition
			      ,why))))
       (when (eq my-result :error)
	 (check-and-puke "while compiling"
			 ',form ',result
			 my-result condition
			 ,why)))))

(defun run-test (testname &optional (source-path nil))
  (let ((*package* *package*)
	(*print-pretty* nil)
	(*print-circle* nil)
	;; to make the system quiet:
	#+(or cmu sbcl)
	(*gc-verbose* nil)
	#+(or cmu sbcl)
	(*compile-verbose* nil)
	#+(or cmu sbcl)
	(*compile-print* nil)
	#+(or cmu sbcl)
	(*compile-progress* nil)
	#+(or cmu sbcl)
	(*TOP-LEVEL-AUTO-DECLARE* nil)
	(err-file (merge-pathnames testname "foo.erg"))
	(source-file (merge-pathnames testname "foo.lisp")))
    (when source-path
      (setq source-file (merge-pathnames source-file source-path)))
    (with-open-file (*log* err-file :direction :output)
		    (setf *output-generated* nil)
		    (load source-file)
		    (force-output *log*))
    (unless *output-generated*
      (delete-file err-file)))
  (values))

(defun run-all-tests (&optional source-path)
  (mapc #'(lambda (x) (print x) (run-test x source-path))
	'(
	  "symboltest"
	  #-akcl "alltest"
	  "array"
	  "backquot"
	  #-akcl "characters"
	  #+(or CLISP ALLEGRO CMU SBCL)"clos"
	  #-ECL
	  "cmucl-bugs"
	  #+(or CLISP ALLEGRO CMU SBCL ECL) "conditions"
	  "eval20"
	  #-ecl
	  "excepsit"
	  "format"
	  #+xcl "hash"
	  "hashlong"
	  "iofkts"
	  "lambda"
	  "lists151"
	  "lists152"
	  "lists153"
	  "lists154"
	  "lists155"
	  "lists156"
	  #+(or CLISP ALLEGRO CMU SBCL) "loop"
	  "macro8"
	  "map"
	  #+(or CLISP ALLEGRO CMU SBCL) "mop"
	  "new-bugs"
	  #-(or cmu sbcl ecl) "number"
	  #+clisp "number2"
	  #+(or XCL CLISP) "path"
	  #+xcl "readtable"
	  "section10"
	  "section11"
	  "section12"
	  "section13"
	  "section14"
	  "section15"
	  "section16"
	  "section17"
	  #-ecl
	  "section18-errors"
	  "section18"
	  "section19"
	  "section2"
	  "section20"
	  "section21"
	  "section22"
	  "section3"
	  "section4"
	  "section5"
	  "section6"
	  "section7"
	  "section8"
	  "section9"
	  "setf"
	  "steele7"
	  #-allegro "streams"
	  "streamslong"
	  "strings"
	  #-akcl "symbol10"
	  "symbols"
	  "type"
	  #+(or sbcl cmu)
	  "unix-tests"
	  ))
  t)

;(run-test "unix-tests")
;(run-test "steele7")
;(quit)
;(run-all-tests)
;(format t "~%~%alles ok...~%")
;(quit)
