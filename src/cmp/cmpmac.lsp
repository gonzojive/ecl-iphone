;;; ----------------------------------------------------------------------
;;; Macros only used in the code of the compiler itself:

(in-package "COMPILER")
(import 'sys::arglist "COMPILER")

(defun same-fname-p (name1 name2) (equal name1 name2))

;;; from cmpenv.lsp
(defmacro next-cmacro () '(incf *next-cmacro*))
(defmacro next-cfun () '(incf *next-cfun*))

;;; from cmplabel.lsp
(defmacro next-label () `(cons (incf *last-label*) nil))

(defmacro next-label* () `(cons (incf *last-label*) t))

(defmacro wt-label (label)
  `(when (cdr ,label) (wt-nl1 "L" (car ,label) ":;")))

(defmacro wt-go (label)
  `(progn (rplacd ,label t) (wt "goto L" (car ,label) ";")))

;;; from cmplam.lsp
(defmacro ck-spec (condition)
  `(unless ,condition
           (cmperr "The parameter specification ~s is illegal." spec)))

(defmacro ck-vl (condition)
  `(unless ,condition
           (cmperr "The lambda list ~s is illegal." vl)))

;;; fromcmputil.sp
(defmacro safe-compile (&rest forms) `(when *safe-compile* ,@forms))
(defmacro cmpck (condition string &rest args)
  `(if ,condition (cmperr ,string ,@args)))

;;; from cmpwt.lsp
(defmacro wt (&rest forms &aux (fl nil))
  (dolist (form forms (cons 'progn (nreverse (cons nil fl))))
    (if (stringp form)
        (push `(princ ,form *compiler-output1*) fl)
        (push `(wt1 ,form) fl))))

(defmacro wt-h (&rest forms &aux (fl nil))
  (dolist (form forms)
    (if (stringp form)
      (push `(princ ,form *compiler-output2*) fl)
      (push `(wt-h1 ,form) fl)))
  `(progn (terpri *compiler-output2*) ,@(nreverse (cons nil fl))))

(defmacro princ-h (form) `(princ ,form *compiler-output2*))

(defmacro wt-nl (&rest forms)
  `(wt #\Newline #\Tab ,@forms))

(defmacro wt-nl1 (&rest forms)
  `(wt #\Newline ,@forms))

;; ----------------------------------------------------------------------
;; C1-FORMS
;;

(defun make-c1form (name info &rest args)
  (let ((form (list* name info args)))
    (c1form-add-info form args)
    form))

(defun make-c1form* (name &rest args)
  (let ((info-args '())
	(form-args '()))
    (do ((l args (cdr l)))
	((endp l))
      (let ((key (first l)))
	(cond ((not (keywordp key))
	       (baboon))
	      ((eq key ':args)
	       (setf form-args (rest l))
	       (return))
	      (t
	       (setf info-args (list* key (second l) info-args)
		     l (cdr l))))))
    (let ((form (list* name (apply #'make-info info-args) form-args)))
      (c1form-add-info form form-args)
      form)))

(defun c1form-add-info (form dependents)
  (dolist (subform dependents form)
    (cond ((c1form-p subform)
	   (add-info (second form) (second subform)
		     (eql (c1form-name subform) 'FUNCTION)))
	  ((consp subform)
	   (c1form-add-info form subform)))))

(defun c1form-add-info1 (form subform)
  (add-info (second form) (second subform)
	    (eql (c1form-name subform) 'FUNCTION)))

(defun c1form-args (form)
  (cddr form))

(defun (setf c1form-args) (value form)
  (setf (cddr form) value))

(defun c1form-p (form)
  (and (consp form)
       (consp (cdr form))
       (info-p (second form))))

(defun c1form-name (form)
  (first form))

(defun (setf c1form-name) (value form)
  (setf (first form) value))

(defmacro c1form-referred-vars (form)
  `(info-referred-vars (second ,form)))

(defmacro c1form-local-referred (form)
  `(info-local-referred (second ,form)))

(defmacro c1form-changed-vars (form)
  `(info-changed-vars (second ,form)))

(defun c1form-type (form)
  (info-type (second form)))

(defun (setf c1form-type) (value form)
  (setf (info-type (second form)) value))

(defmacro c1form-sp-change (form)
  `(info-sp-change (second ,form)))

(defmacro c1form-volatile (form)
  `(info-volatile (second ,form)))

(defun c1form-volatile* (form)
  (if (c1form-volatile form) "volatile " ""))

(defun copy-c1form (form)
  (let* ((info (copy-info (second form)))
	 (output (copy-list form)))
    (setf (second output) info)
    output))

(defmacro c1form-arg (nth form)
  (case nth
    (0 `(first (c1form-args ,form)))
    (1 `(second (c1form-args ,form)))
    (otherwise `(nth ,nth (c1form-args ,form)))))

