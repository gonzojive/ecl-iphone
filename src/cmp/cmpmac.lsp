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
