(in-package "CL-USER")

#+clisp
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(block ,b
       (let ((*error-handler*
               #'(lambda (&rest args) (return-from ,b 'error))
            ))
         ,@forms
     ) )
) )

#+akcl
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym))
        (h (gensym)))
    `(block ,b
       (let ((,h (symbol-function 'system:universal-error-handler)))
         (unwind-protect
           (progn (setf (symbol-function 'system:universal-error-handler)
                        #'(lambda (&rest args) (return-from ,b 'error))
                  )
                  ,@forms
           )
           (setf (symbol-function 'system:universal-error-handler) ,h)
     ) ) )
) )

#+allegro
(defmacro with-ignored-errors (&rest forms)
  (let ((r (gensym)))
    `(let ((,r (multiple-value-list (excl:errorset (progn ,@forms)))))
       (if (car ,r) (values-list (cdr ,r)) 'error)
     )
) )

#-(or clisp akcl allegro)
(defmacro with-ignored-errors (&rest forms)
  (let ((b (gensym)))
    `(block ,b
       (handler-bind
         ((error #'(lambda (condition) (return-from ,b 'error))))
         ,@forms
     ) )
) )

(defvar *source-dir* "./")
(defvar *output-dir* "./")

(defconstant +all-tests+ '("boyer" "browse" "ctak" "dderiv" "deriv"
			   "destru-mod" "destru" "div2"
			   "fprint" "fread" "frpoly"
			   "puzzle" "puzzle-mod" "puzzle-mod2"
			   "stak" "tak" "tak-mod" "takl" "takr"
			   "tprint" "traverse" "triang-mod" "triang"
			   ;; These two are at the end because they cause
			   ;; SIGSEGVS in ECLS.
			   #+nil"fft-mod" #+nil"fft"))

(defconstant +repeats+ '(("destru" 4)("destru-mod" 4)("fprint" 4)("fread" 4)
			 ("stak" 4)("takr" 4)("tprint" 4)))

#+(or (and ecls (not boehm-gc)) cmu)
(setq system::*gc-verbose* nil)

(defun do-test (file &key (repeat 1 given) compile &aux tem)
  (when (find-package "TESTING")
    (delete-package "TESTING"))
  #+cmu
  (gc :full t)
  #+clisp
  (system::gc)
  #+ecls
  (system::gc t)
  (let ((source-file (merge-pathnames (merge-pathnames file *source-dir*)
				      "foo.cl"))
	(fasl-file (compile-file-pathname (merge-pathnames file *output-dir*)))
	(*package* (make-package "TESTING")))
    (cond (compile
	   (proclaim-file source-file)
	   (compile-file source-file :output-file fasl-file
			 #+ecls :c-file #+ecls t #+ecls :h-file #+ecls t
			 #+ecls :verbose #+ecls t)
	   (print fasl-file)
	   (load fasl-file :verbose t))
	  (t
	   (load source-file :verbose t)))
    (if (and (not given)
	     (setq tem (assoc file +repeats+ :test 'equalp)))
      (setq repeat (second tem)))
    (let* ((pos (position #\- file))
	   (name (if pos (subseq file 0 pos) file))
	   (command (intern (string-upcase (format nil "TEST~a" name))))
	   (start (get-internal-run-time)))
      (dotimes (i repeat) (funcall command))
      (setq start (- (get-internal-run-time) start))
      (/ (float start) (* (float internal-time-units-per-second) repeat)))))


(defun do-all-tests (name *source-dir* *output-dir* &optional (compile nil))
  (with-open-file (st name :direction :output
		      :if-does-not-exist :create
		      :if-exists :supersede)
    (print (cons name (mapcar #'(lambda (name)
				  (with-ignored-errors
				      (do-test name :compile compile)))
			      +all-tests+))
	   st)))

(defun beautify-output (output)
  (let ((data nil) (envs 0) (tests 0))
    (do ((item (read) (read)))
	((null item))
      (push item data))
    (setq envs (length data)
	  tests (length (car data))
	  data (make-array (list (1+ envs) tests)
			   :initial-contents (cons (cons "" +all-tests+) data)))
    (with-open-file (st output :direction :output :if-exists :append
			:if-does-not-exist :create)
      (dotimes (row tests)
	(dotimes (col (1+ envs))
	  (let ((data (aref data col row)))
	    (cond ((zerop col)
		   (format st "~%~12a" data))
		  ((numberp data)
		   (format st "   ~7,3f" data))
		  ((eq data 'ERROR)
		   (format st "     *****"))
		  (t
		   (format st "   ~7<~a~>" data))))))
      (format st "

IMPLi = Implementation IMPL interpreted
IMPLc = Implementation IMPL compiled

CLISP = CLISP 2000-03-06 (March 2000)
CMUCL = CMUCL 18c
ECLS  = ECLS ~A
" (lisp-implementation-version))
      (terpri st))
    (quit)))
