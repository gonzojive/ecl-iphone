;; Test-Suiten ablaufen lassen:

#+CLISP
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (LET ((*ERROR-HANDLER*
               #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
            ))
         ,@forms
     ) )
) )

#+AKCL
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(BLOCK ,b
       (LET ((,h (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)))
         (UNWIND-PROTECT
           (PROGN (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER)
                        #'(LAMBDA (&REST ARGS) (RETURN-FROM ,b 'ERROR))
                  )
                  ,@forms
           )
           (SETF (SYMBOL-FUNCTION 'SYSTEM:UNIVERSAL-ERROR-HANDLER) ,h)
     ) ) )
) )

#+ECL
(defmacro with-ignored-errors (&rest forms)
  `(catch si::*ignore-errors-tag*
      (let* ((si::*ignore-errors* t)
	     (output ,@forms))
	(if (eq output 'ERROR) *last-error* output))))

#+ALLEGRO
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(LET ((,r (MULTIPLE-VALUE-LIST (EXCL:ERRORSET (PROGN ,@forms)))))
       (IF (CAR ,r) (VALUES-LIST (CDR ,r)) 'ERROR)
     )
) )

#-(or CLISP AKCL ALLEGRO ECL)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(BLOCK ,b
       (HANDLER-BIND
         ((ERROR #'(LAMBDA (CONDITION) (RETURN-FROM ,b 'ERROR))))
         ,@forms
     ) )
) )

#-ALLEGRO
(defun merge-extension (type filename)
  (merge-pathnames type filename)
)
#+ALLEGRO
(defun merge-extension (type filename)
  (merge-pathnames (make-pathname :type (subseq (string type) 1)) filename)
)

(defun do-safe (stream log &optional (ignore-errors t))
  (let ((eof "EOF"))
    (loop
      (let* ((form (read stream nil eof))
	     (result (read stream nil eof)))
	(force-output *standard-output*)
        (when (or (eq form eof) (eq result eof)) (return))
        (let ((my-result
                (if ignore-errors
                  (with-ignored-errors (eval form)) ; return ERROR on errors
                  (eval form) ; don't disturb the condition system when testing it!
             )) )
	  (print my-result)
          (cond ((eql result my-result)
                 (format t "~%EQL-OK: ~S" result)
                )
                ((equal result my-result)
                 (format t "~%EQUAL-OK: ~S" result)
                )
                ((equalp result my-result)
                 (format t "~%EQUALP-OK: ~S" result)
                )
                (t
                 (format t "~%FEHLER!! ~S sollte ~S sein!" my-result result)
                 (format log "~%Form: ~S~%SOLL: ~S~%~A: ~S~%"
                             form result
                             #+CLISP "CLISP" #+AKCL "AKCL" #+ALLEGRO "ALLEGRO" #+ECL "ECL"
                             my-result)
		 (stop))
) ) ) ) ) )

(defun do-test (stream log &optional (ignore-errors t))
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (result (read stream nil eof)))
        (when (or (eq form eof) (eq result eof)) (return))
        (print form)
        (let ((my-result
                (if ignore-errors
                  (with-ignored-errors (eval form)) ; return ERROR on errors
                  (eval form) ; don't disturb the condition system when testing it!
             )) )
	  (cos 1.0)
          (cond ((eql result my-result)
                 (format t "~%EQL-OK: ~S" result)
                )
                ((equal result my-result)
                 (format t "~%EQUAL-OK: ~S" result)
                )
                ((equalp result my-result)
                 (format t "~%EQUALP-OK: ~S" result)
                )
                (t
                 (format t "~%FEHLER!! ~S sollte ~S sein!" my-result result)
                 (format log "~%Form: ~S~%SOLL: ~S~%~A: ~S~%"
                             form result
                             #+CLISP "CLISP" #+AKCL "AKCL" #+ALLEGRO "ALLEGRO" #+ECL "ECL"
                             my-result
                ))
) ) ) ) ) )

(defun do-errcheck (stream log)
  (let ((eof "EOF"))
    (loop
      (let ((form (read stream nil eof))
            (errtype (read stream nil eof)))
        (when (or (eq form eof) (eq errtype eof)) (return))
        (print form)
        (let ((my-result (nth-value 1 (ignore-errors (eval form)))))
          (multiple-value-bind (typep-result typep-error)
              (ignore-errors (typep my-result errtype))
            (cond ((and (not typep-error) typep-result)
                   (format t "~%OK: ~S" errtype)
                  )
                  (t
                   (format t "~%FEHLER!! ~S statt ~S !" my-result errtype)
                   (format log "~%Form: ~S~%SOLL: ~S~%~A: ~S~%"
                               form errtype
                               #+CLISP "CLISP" #+AKCL "AKCL" #+ALLEGRO "ALLEGRO"
                               my-result
                  ))
) ) ) ) ) ) )

(defun run-test (testname
                 &key (tester #'do-test)
		 (logname (merge-extension ".erg" testname))
		 logname2
                 &aux log-empty-p)
  (with-open-file (s (merge-extension ".tst" testname) :direction :input)
    (with-open-file (log logname :direction :output
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (if logname2
	(with-open-file (log2 logname2 :direction :output
			      #+ANSI-CL :if-exists #+ANSI-CL :new-version)
	  (let ((*package* *package*)
		(*print-pretty* nil)
		(*standard-output* log2))
	    (funcall tester s log)))
	(let ((*package* *package*)
	      (*print-pretty* nil))
	    (funcall tester s log)))
      (setq log-empty-p (zerop (file-length log)))
  ) )
  (when log-empty-p (delete-file logname))
  (values)
)

(defun run-test-no-log (testname
			&optional (tester #'do-test)
			&aux (logname "/dev/null"))
  (with-open-file (s (merge-extension ".tst" testname) :direction :input)
    (with-open-file (log logname :direction :output
                                 #+ANSI-CL :if-exists #+ANSI-CL :new-version)
      (let ((*package* *package*)
            (*print-pretty* nil))
        (funcall tester s log)
      )
  ) )
  (values)
)

(defun run-all-tests (&optional (source-directory nil) (log-directory nil))
  (mapc #'(lambda (test)
	    (format t ";;; Doing ~S~%" test)
	    (run-test (if source-directory
			(merge-pathnames source-directory test)
			test)
		      :logname
		      (merge-extension ".erg"
				       (if log-directory
					 (merge-pathnames log-directory test)
					 test))
		      :logname2
		      (merge-extension ".out"
				       (if log-directory
					 (merge-pathnames log-directory test)
					 test))))
        '( #-AKCL               "alltest"
                                "array"
                                "backquot"
           #-AKCL               "characters"
           #+(or CLISP ALLEGRO) "clos"
                                "eval20"
                                "format"
           #+CLISP              "genstream"
           #+XCL                "hash"
                                "hashlong"
                                "iofkts"
                                "lambda"
                                "lists151"
                                "lists152"
                                "lists153"
                                "lists154"
                                "lists155"
                                "lists156"
           #+(or ECL CLISP ALLEGRO)
	  			"loop"
                                "macro8"
                                "map"
           #+(or CLISP ALLEGRO) "mop"
                                "number"
           #+CLISP              "number2"
           #-(or ECL AKCL ALLEGRO)
	  			"pack11"
           #+(or XCL CLISP ECL)
	  			"path"
           #+XCL                "readtable"
                                "setf"
                                "steele7"
           #-ALLEGRO            "streams"
                                "streamslong"
                                "strings"
           #-AKCL               "symbol10"
                                "symbols"
           #+XCL                "tprint"
           #+XCL                "tread"
                                "type"
	  			"ecl_chap12_spec"
	  			"ecl_chap14_cons"
				"ecl_chap14_plist"
	  			"ecl_chap11_pack"
  )      )
  #+(or CLISP ALLEGRO)
  (run-test "conditions" #'(lambda (stream log) (do-test stream log nil)))
  #-ECL
  (run-test "excepsit" #'do-errcheck)
  t
)
