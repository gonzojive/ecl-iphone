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

(defmacro coerce-to-package (p)
  (if (eq p '*package*)
      p
      (let ((g (gensym)))
        `(let ((,g ,p))
           (if (packagep ,g)
               ,g
               (find-package (string ,g)))))))

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


(defmacro do-symbols ((var &optional (package '*package*) (result-form nil))
                      &rest body)
  "Syntax: (do-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Executes STATEMENTs once for each symbol in PACKAGE (which defaults to the
current package), with VAR bound to the symbol.  Then evaluates RESULT (which
defaults to NIL) and returns all values."
  (let ((p (gensym)) (i (gensym))
        (x (gensym)) (y (gensym))
	declaration)
    (multiple-value-setq (declaration body doc)
      (find-declarations body nil))
    `(let ((,p (coerce-to-package ,package)) ,var)
       ,@declaration
       (multiple-value-bind (,y ,x)
	    (package-size ,p)
            (declare (fixnum ,x ,y))
       (dotimes (,i (the fixnum (+ ,x ,y))
		 (progn (setq ,var nil) ,result-form))
         (setq ,var (if (< ,i ,x)
                      (sys:package-internal ,p ,i)
                      (sys:package-external ,p (the fixnum (- ,i ,x)))))
         (unless (fixnump ,var)
	   ,@body))))))
       

(defmacro do-external-symbols
          ((var &optional (package '*package*) (result-form nil)) &rest body)
  "Syntax: (do-external-symbols (var [package [result]])
          {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each external symbol
in PACKAGE (which defaults to the current package), with VAR bound to the
variable.  Then evaluates RESULT (which defaults to NIL) and returns all
values."
  (let ((p (gensym))
	(i (gensym))
        declaration)
    (multiple-value-setq (declaration body)
      (find-declarations body nil))
    `(let ((,p (coerce-to-package ,package)) ,var)
       
       ,@declaration
       (dotimes (,i (the fixnum (package-size ,p))
		 (progn (setq ,var nil) ,result-form))
         (setq ,var (sys:package-external ,p ,i))
         (unless (fixnump ,var)
	   ,@body)))))

(defmacro do-all-symbols ((var &optional (result-form nil)) &rest body)
  "Syntax: (do-all-symbols (var [result]) {decl}* {tag | statement}*)
Establishes a NIL block and executes STATEMENTs once for each symbol in each
package, with VAR bound to the symbol.  Then evaluates RESULT (which defaults
to NIL) and returns all values."
  `(dolist (.v (list-all-packages) ,result-form)
	   (do-symbols (,var .v)
		       ,@ body)))


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
        (if (special-form-p symbol)
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
