;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

(in-package "SYSTEM")

(defmacro check-type (place typespec &optional (string nil s))
  "Args: (check-type place typespec [string-form])
Signals a continuable error, if the value of PLACE is not of the specified
type.  Before continuing, receives a new value of PLACE from the user and
checks the type again.  Repeats this process until the value of PLACE becomes
of the specified type.  STRING-FORM, if given, is evaluated only once and the
value is used to indicate the expected type in the error message."
  `(do ((*print-level* 4)
        (*print-length* 4))
       ((typep ,place ',typespec) nil)
       (cerror ""
               "The value of ~:@(~S~), ~:@(~S~), is not ~A."
               ',place ,place
               ,(if s string `',typespec))
       (setf ,place (si::ask-for-form2 ',place))))


(defmacro assert (test-form &optional places string &rest args)
  "Args: (assert form [({place}*) [string {arg}*]])
Evaluates FORM and signals a continuable error if the value is NIL.  Before
continuing, receives new values of PLACEs from user.  Repeats this process
until FORM returns a non-NIL value.  Returns NIL.  STRING is the format string
for the error message and ARGs are arguments to the format string."
  `(do ((*print-level* 4)
        (*print-length* 4))
       (,test-form nil)
       ,(if string
            `(cerror "" ,string ,@args)
            `(cerror "" "The assertion ~:@(~S~) is failed." ',test-form))
       ,@(mapcar #'ask-for-form places)
       (format *error-output* "Now continuing ...~%")))


(defun ask-for-form (place)
  (declare (si::c-local))
  `(progn (format  *error-output*
                   "Please input the new value for the place ~:@(~S~): "
                   ',place)
          (finish-output *error-output*)
          (setf ,place (read))))

(defun ask-for-form2 (place)
  (format  *error-output*
	   "Please input the new value for the place ~:@(~S~): "
	   place)
  (finish-output *error-output*)
  (prog1 (read)
    (format *error-output* "Now continuing ...~%")))

(defun ecase-error (keyform value &rest values)
  (let ((*print-level* 4)
	(*print-length* 4))
    (error
     "The value of ~:@(~S~), ~:@(~S~), is ~
     ~#[nonsense~;not ~:@(~S~)~;neither ~:@(~S~) nor ~:@(~S~)~
     ~:;not ~@{~#[~;or ~]~:@(~S~)~^, ~}~]."
     keyform value values)))

(defun case-values (clauses)
  (declare (si::c-local))
  (mapcan #'(lambda (x)
	      (if (listp (car x))
		(mapcar #'(lambda (y) `',y) (car x))
		`(',(car x))))
	  clauses))

(defmacro ecase (keyform &rest clauses)
  "Syntax: (ecase keyform {({key | ({key}*)} {form}*)}*)
Evaluates KEYFORM and tries to find the KEY that is EQL to the value of
KEYFORM.  If found, then evaluates FORMs that follow the KEY (or the key list
that contains the KEY) and returns all values of the last FORM.  If not,
signals an error."
  (let* ((key (if (atom keyform) keyform '#:key))
	 (let (if (atom keyform) nil (list (list key keyform)))))
    `(let ,let
      (case ,key ,@clauses
	    (t (si::ecase-error ',keyform ,key ,@(case-values clauses)))))))

(defun ccase-error (keyform key &rest values)
  (let ((*print-level* 4)
	(*print-length* 4)
	(value))
    (cerror ""
	    "The value of ~:@(~S~), ~:@(~S~), is ~
            ~#[nonsense~;not ~:@(~S~)~;neither ~
            ~:@(~S~) nor ~:@(~S~)~
            ~:;not ~@{~#[~;or ~]~:@(~S~)~^, ~}~]."
            keyform key values)
    (set value (si::ask-for-form2 keyform))
    value))

(defmacro ccase (keyform &rest clauses)
  "Syntax: (ccase place {({key | ({key}*)} {form}*)}*)
Searches a KEY that is EQL to the value of PLACE.  If found, then evaluates
FORMs in order that follow the KEY (or the key list that contains the KEY) and
returns all values of the last FORM.  If no such KEY is found, signals a
continuable error.  Before continuing, receives a new value of PLACE from
user and searches a KEY again.  Repeats this process until the value of PLACE
becomes EQL to one of the KEYs."
  (let* ((key (if (atom keyform) keyform '#:key))
	 (let (if (atom keyform) nil (list (list key keyform))))
	 (repeat '#:repeat))
    `(let ,let
      (tagbody ,repeat
	 (case ,key ,@clauses
	       (t (setq ,key (si::ccase-error ',keyform ,key ,@(case-values clauses)))
		  (go ,repeat)))))))

(defmacro typecase (keyform &rest clauses)
  "Syntax: (typecase keyform {(type {form}*)}*)
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, simply returns NIL.  The symbols T and OTHERWISE may
be used as a TYPE to specify the default case."
  (do ((l (reverse clauses) (cdr l))
       (form nil) (key (gensym)))
      ((endp l) `(let ((,key ,keyform)) ,form))
      (if (or (eq (caar l) 't) (eq (caar l) 'otherwise))
          (setq form `(progn ,@(cdar l)))
          (setq form
                `(if (typep ,key (quote ,(caar l)))
                     (progn ,@(cdar l))
                     ,form))))
  )

(defmacro etypecase (keyform &rest clauses &aux (key (gensym)))
  "Syntax: (etypecase keyform {(type {form}*)}*)
Evaluates KEYFORM and searches a TYPE to which the value of KEYFORM belongs.
If found, then evaluates FORMs that follow the TYPE and returns all values of
the last FORM.  If not, signals an error."
   (do ((l (reverse clauses) (cdr l))	; Beppe
        (form `(error (typecase-error-string
                       ',keyform ,key
                       ',(mapcar #'(lambda (l) (car l)) clauses)))))
       ((endp l) `(let ((,key ,keyform)) ,form))
       (setq form `(if (typep ,key ',(caar l))
                       (progn ,@(cdar l))
                       ,form))
       )
   )

(defmacro ctypecase (keyplace &rest clauses &aux (key (gensym)))
  "Syntax: (ctypecase place {(type {form}*)}*)
Searches a TYPE to which the value of PLACE belongs.  If found, then evaluates
FORMs that follow the TYPE and returns all values of the last FORM.  If no
such TYPE is found, signals a continuable error.  Before continuing, receives
a new value of PLACE from the user and searches an appropriate TYPE again.
Repeats this process until the value of PLACE becomes of one of the TYPEs."
  `(loop (let ((,key ,keyplace))
              ,@(mapcar #'(lambda (l)
                                 `(when (typep ,key ',(car l))
                                        (return (progn ,@(cdr l)))))
                        clauses)
              (cerror ""
                      (typecase-error-string
                       ',keyplace ,key
                       ',(mapcar #'(lambda (l) (car l)) clauses))))
         ,(ask-for-form keyplace)
         (format *error-output* "Now continuing ...~%"))
  )

(defun typecase-error-string
       (keyform keyvalue negs
                &aux (negs1 nil) (poss nil) (poss1 nil))
   (do ()
       ((endp negs))
       (if (symbolp (car negs))
           (progn (push (list (car negs)) negs1) (pop negs))
           (case (caar negs)
                 (or (setq negs (append (cdar negs) (cdr negs))))
                 (member (mapc #'(lambda (x) (push `(member ,x) negs1))
                               (cdar negs))
                         (pop negs))
                 (not (push (cadar negs) poss) (pop negs))
                 (otherwise (push (car negs) negs1) (pop negs)))))
   (do ()
       ((endp poss))
       (cond ((symbolp (car poss)) (push (list (car poss)) poss1) (pop poss))
             ((eq (caar poss) 'and)
              (setq poss (append (cdar poss) (cdr poss))))
             (t (push (car poss) poss1) (pop poss))))
   (format
    nil
    "The value of ~:@(~S~), ~:@(~S~), is ~?~?."
    keyform
    keyvalue
    "~#[~;~;~?~;~;~? and ~?~:;~%~@{~#[~;~;and ~]~?~^, ~}~]"
    (mapcan 'typecase-error-strings poss1)
    "~:[~[something~;~:;~%~]~;~[~:;, but~%~]~]~
     ~#[~;~;not ~?~;~;neither ~? nor ~?~:;not ~@{~#[~;~;or ~]~?~^, ~}~]"
    (cons poss1 (cons (length negs1)
                      (mapcan 'typecase-error-strings (nreverse negs1))))
    )
   )

(defun typecase-error-strings (type)
  (flet ((vowel-p (symbol)
	   (member (elt (symbol-name symbol) 0)
		   '(#\A #\I #\U #\E #\O #\a #\i #\u #\e #\o) :test #'eq)))
    (cond ((eq (car type) 'member)
	   (case (length (cdr type))
	     (0 `("one of none" nil))
	     (1 `("~:@(~S~)" (,(cadr type))))
	     (2 `("either ~:@(~S~) or ~:@(~S~)" ,(cdr type)))
	     (t `("one of ~:@(~S~)" (,(cdr type))))))
	  ((eq (car type) 'satisfies)
	   `("an object satisfying ~:@(~S~)" ,(cdr type)))
	  ((or (endp (cdr type)) (null (remove '* (cdr type))))
	   (let ((x (assoc (car type)
			   '((t "anything")
			     (nil "none")
			     (null "nil")
			     (common "an object of a standard data type")))))
             (if x
                 `(,(cadr x) nil)
                 `("~:[a~;an~] ~(~A~)" (,(vowel-p (car type)) ,(car type))))))
	  (t `("~:[a~;an~] ~:@(~S~)" (,(vowel-p (car type)) ,type)))))
 )
