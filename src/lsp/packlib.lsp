;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;;                    package routines

(in-package "SYSTEM")

(defun find-all-symbols (string-or-symbol)
  "Args: (string-designator)
Returns a list of all symbols that have the specified print name.
STRING-DESIGNATOR may be a symbol, in which case the print name of the symbol
is used."
  (when (symbolp string-or-symbol)
        (setq string-or-symbol (symbol-name string-or-symbol)))
  (mapcan #'(lambda (p)
              (multiple-value-bind (s i)
                  (find-symbol string-or-symbol p)
                (if (or (eq i :internal) (eq i :external))
                    (list s)
                    nil)))
          (list-all-packages)))

(defun packages-iterator (packages options maybe-list)
  (let ((all-symbols nil))
    (when (or (atom packages) (not maybe-list))
      (setq packages (list packages)))
    (dolist (package packages)
      (multiple-value-bind (hash-ext hash-int packages-used)
	  (si::package-hash-tables (si::coerce-to-package package))
	(when (member :external options :test #'eq)
	  (push (list package :external hash-ext) all-symbols))
	(when (member :internal options :test #'eq)
	  (push (list package :internal hash-int) all-symbols))
	(when (member :inherited options :test #'eq)
	  (dolist (p packages-used)
	    (push (list package :internal (si::package-hash-tables p))
		  all-symbols)))))
    (let* ((current (pop all-symbols))
	   (package (first current))
	   (type (second current))
	   (iterator (si::hash-table-iterator (third current))))
      (flet ((iterate ()
	       (do () (nil)
		(multiple-value-bind (found key value)
		    (funcall iterator)
		  (cond (found (return (values t value type package)))
			((null all-symbols) (return (values nil nil nil nil)))
			(t
			 (setq current (pop all-symbols))
			 (setq package (first current)
			       type (second current)
			       iterator (si::hash-table-iterator (third current))
			       )))))))
	#'iterate))))

(defmacro with-package-iterator ((iterator package-list &rest conditions)
				 &rest body)
  `(let ((,iterator (packages-iterator ,package-list ',conditions t)))
    (macrolet ((,iterator () (list 'funcall ',iterator)))
      ,@body)))

(defun expand-do-symbols (var package result-form body options)
  (declare (si::c-local))
  (let* ((i (gensym))
	 (found (gensym))
	 declaration)
    (multiple-value-setq (declaration body doc)
      (find-declarations body nil))
    `(let* ((,i (packages-iterator ,package ',options t)))
      (loop
       (multiple-value-bind (,found ,var)
	   (funcall ,i)
	 ,@declaration
	 (unless ,found (return ,result-form))
	 ,@body)))))

(defmacro do-symbols ((var &optional (package '*package*) (result-form nil))
                      &rest body)
  "Syntax: (do-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Executes STATEMENTs once for each symbol in PACKAGE (which defaults to the
current package), with VAR bound to the symbol.  Then evaluates RESULT (which
defaults to NIL) and returns all values."
  (expand-do-symbols var package result-form body '(:external :internal :inherited)))

(defmacro do-external-symbols
          ((var &optional (package '*package*) (result-form nil)) &rest body)
  "Syntax: (do-external-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each external symbol
in PACKAGE (which defaults to the current package), with VAR bound to the
variable.  Then evaluates RESULT (which defaults to NIL) and returns all
values."
  (expand-do-symbols var package result-form body '(:external)))

(defmacro do-all-symbols ((var &optional (result-form nil)) &rest body)
  "Syntax: (do-all-symbols (var [result]) {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each symbol in each
package, with VAR bound to the symbol.  Then evaluates RESULT (which defaults
to NIL) and returns all values."
  (expand-do-symbols var '(list-all-packages) result-form body '(:external :internal)))

(defun substringp (sub str)
  (do ((i (the fixnum (- (length str) (length sub))))
       (l (length sub))
       (j 0 (1+ j)))
      ((> j i) nil)
    (declare (fixnum l j))
    (when (string-equal sub str :start2 j :end2 (the fixnum (+ j l)))
          (return t))))


(defun print-symbol-apropos (symbol)
  (prin1 symbol)
  (when (fboundp symbol)
        (if (special-operator-p symbol)
            (princ "  Special form")
            (if (macro-function symbol)
                (princ "  Macro")
                (princ "  Function"))))
  (when (boundp symbol)
        (if (constantp symbol)
            (princ "  Constant: ")
            (princ "  has value: "))
        (prin1 (symbol-value symbol)))
  (terpri))


(defun apropos (string &optional package)
  "Args: (string &optional (package nil))
Prints those symbols whose print-names contain STRING as substring.  If
PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  (setq string (string string))
  (cond (package
         (do-symbols (symbol package)
           (when (substringp string (string symbol))
                 (print-symbol-apropos symbol)))
         (do ((p (package-use-list package) (cdr p)))
             ((null p))
           (do-external-symbols (symbol (car p))
             (when (substringp string (string symbol))
                   (print-symbol-apropos symbol)))))
        (t
         (do-all-symbols (symbol)
           (when (substringp string (string symbol))
                 (print-symbol-apropos symbol)))))
  (values))


(defun apropos-list (string &optional package &aux list)
  "Args: (string &optional (package nil))
Returns a list of all symbols whose print-names contain STRING as substring.
If PACKAGE is non-NIL, then only the specified PACKAGE is searched."
  (setq list nil)
  (setq string (string string))
  (cond (package
         (do-symbols (symbol package)
           (when (substringp string (string symbol))
                 (setq list (cons symbol list))))
         (do ((p (package-use-list package) (cdr p)))
             ((null p))
           (do-symbols (symbol (car p))
             (when (substringp string (string symbol))
                   (setq list (cons symbol list))))))
        (t
         (do-all-symbols (symbol)
           (when (substringp string (string symbol))
                 (setq list (cons symbol list))))))
  list)
