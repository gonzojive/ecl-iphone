;;;;  Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
;;;;  Copyright (c) 1990, Giuseppe Attardi.
;;;;
;;;;    This program is free software; you can redistribute it and/or
;;;;    modify it under the terms of the GNU Library General Public
;;;;    License as published by the Free Software Foundation; either
;;;;    version 2 of the License, or (at your option) any later version.
;;;;
;;;;    See file '../Copyright' for full details.

;;;; CMPENV  Environments of the Compiler.

(in-package "COMPILER")

;;; Only these flags are set by the user.
;;; If *safe-compile* is ON, some kind of run-time checks are not
;;; included in the compiled code.  The default value is OFF.

(defun init-env ()
  (setq *max-temp* 0)
  (setq *temp* 0)
  (setq *next-cmacro* 0)
  (setq *next-vv* -1)
  (setq *next-cfun* 0)
  (setq *last-label* 0)
  (setq *objects* nil)
  (setq *keywords* nil)
  (setq *local-funs* nil)
  (setq *global-funs* nil)
  (setq *linking-calls* nil)
  (setq *global-entries* nil)
  (setq *undefined-vars* nil)
  (setq *reservations* nil)
  (setq *top-level-forms* nil)
  (setq *compile-time-too* nil)
  (setq *function-declarations* nil)
  (setq *inline-functions* nil)
  (setq *inline-blocks* 0)
  (setq *notinline* nil)
  )

(defun next-lcl () (list 'LCL (incf *lcl*)))

(defun next-temp ()
  (prog1 *temp*
         (incf *temp*)
         (setq *max-temp* (max *temp* *max-temp*))))

(defun next-lex ()
  (prog1 (cons *level* *lex*)
         (incf *lex*)
         (setq *max-lex* (max *lex* *max-lex*))))

(defun next-env () (prog1 *env*
		     (incf *env*)
		     (setq *max-env* (max *env* *max-env*))))

(defun add-symbol (symbol)
  (add-object symbol))

(defun add-keywords (keywords)
  ;; We have to build, in the vector VV[], a sequence with all
  ;; the keywords that this function uses. It does not matter
  ;; whether each keyword has appeared separately before, because
  ;; cl_parse_key() needs the whole list. However, we can reuse
  ;; keywords lists from other functions when they coincide with ours.
  (flet ((add-keyword (keyword &aux x)
	   (incf *next-vv*)
	   (setq x (format nil "VV[~d]" *next-vv*))
	   (let ((y (assoc keyword *objects*)))
	     (if y
		 (wt-filtered-data (format nil "#!~d" (- (1+ (third y)))))
	         (wt-data keyword)))
	   (push (list keyword x *next-vv*) *objects*)
	   x))
    ;; We search for keyword lists that are similar. However, the list
    ;; *OBJECTS* contains elements in decreasing order!!!
    (let ((x (search (reverse keywords) *objects*
		     :test #'(lambda (k rec) (eq k (first rec))))))
      (if x
	  (progn
	    (cmpnote "Reusing keywords lists for ~S" keywords)
	    (second (elt *objects* (+ x (length keywords) -1))))
	  (let ((x (add-keyword (first keywords))))
	    (dolist (k (rest keywords) x)
	      (add-keyword k))))))))

(defun add-object (object &aux x found)
  (cond ((setq x (assoc object *objects* :test 'equal))
         (second x))
	((and (symbolp object)
	      (multiple-value-setq (found x) (si::mangle-name object)))
	 x)
        (t (incf *next-vv*)
	   (setq x (format nil "VV[~d]" *next-vv*))
	   (push (list object x *next-vv*) *objects*)
           (wt-data object)
           x)))

(defun function-arg-types (arg-types &aux (types nil))
  (do ((al arg-types (cdr al)))
      ((or (endp al)
           (member (car al) '(&optional &rest &key)))
       (nreverse types))
      (declare (object al))
      (push (type-filter (car al)) types)))

;;; The valid return type declaration is:
;;;	(( VALUES {type}* )) or ( {type}* ).

(defun function-return-type (return-types)
  (cond ((endp return-types) t)
        ((and (consp (car return-types))
              (eq (caar return-types) 'VALUES))
         (cond ((not (endp (cdr return-types)))
                (warn "The function return types ~s is illegal." return-types)
                t)
               ((or (endp (cdar return-types))
                    (member (cadar return-types) '(&optional &rest &key)))
                t)
               (t (type-filter (cadar return-types)))))
        (t (type-filter (car return-types)))))

(defun add-function-proclamation (fname decl &aux
         arg-types return-types)
  (cond ((and (symbolp fname)
	      (listp decl) (listp (cdr decl)))
	 (cond ((or (null decl)(eq (car decl) '*)) (setq arg-types '*)
		(rem-sysprop fname 'PROCLAIMED-ARG-TYPES))
	       (t (setq arg-types (function-arg-types (car decl)))
		  (put-sysprop fname 'PROCLAIMED-ARG-TYPES arg-types)))
	 (cond ((or (null (cdr decl))(eq (second decl) '*))
		(setq return-types '*))
	       (t (setq return-types (function-return-type (cdr decl)))))
         (put-sysprop fname 'PROCLAIMED-RETURN-TYPE return-types)
	 (cond((eql return-types '*))
	      (t(setq return-types (cdr decl))))
	 ;;; A non-local function may have local entry only if it returns
	 ;;; a single value.
         (if (and (not (endp return-types))
                  (endp (cdr return-types))
                  (not (and (consp (car return-types))
                            (eq (caar return-types) 'VALUES)
                            (or (endp (cdar return-types))
                                (not (endp (cddar return-types)))))))
             (put-sysprop fname 'PROCLAIMED-FUNCTION t)
	   (rem-sysprop fname 'PROCLAIMED-FUNCTION)))
        (t (warn "The function procl ~s ~s is not valid." fname decl))))

(defun add-function-declaration (fname arg-types return-types)
  (if (symbolp fname)
      (push (list (sch-local-fun fname)
		  (function-arg-types arg-types)
		  (function-return-type return-types))
	    *function-declarations*)
      (warn "The function name ~s is not a symbol." fname)))

(defun get-arg-types (fname &aux x)
  (if (setq x (assoc fname *function-declarations*))
      (second x)
      (get-sysprop fname 'PROCLAIMED-ARG-TYPES)))

(defun get-return-type (fname)
  (let* ((x (assoc fname *function-declarations*))
         (type1 (if x (caddr x) (get-sysprop fname 'PROCLAIMED-RETURN-TYPE))))
        (cond (type1
               (let ((type (get-sysprop fname 'RETURN-TYPE)))
                    (cond (type
                           (cond ((setq type (type-and type type1)) type)
                                 (t
                                  (cmpwarn
                                   "The return type of ~s was badly declared."
                                   fname))))
                          (t type1))))
              (t (get-sysprop fname 'RETURN-TYPE)))
        ))

(defun get-local-arg-types (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (second x)
      nil))

(defun get-local-return-type (fun &aux x)
  (if (setq x (assoc fun *function-declarations*))
      (caddr x)
      nil))

;;; Proclamation and declaration handling.

(defun inline-possible (fname)
  (not (or ; *compiler-push-events*
	(member fname *notinline* :test #'same-fname-p)
	(and (symbolp fname) (get-sysprop fname 'CMP-NOTINLINE)))))

#-:CCL
(defun proclaim (decl)
  (unless (listp decl)
	  (error "The proclamation specification ~s is not a list" decl))
  (case (car decl)
    (SPECIAL
     (dolist (var (cdr decl))
       (if (symbolp var)
           (sys:*make-special var)
           (warn "The variable name ~s is not a symbol." var))))
    (OPTIMIZE
     (dolist (x (cdr decl))
       (when (symbolp x) (setq x (list x 3)))
       (if (or (not (consp x))
               (not (consp (cdr x)))
               (not (numberp (second x)))
               (not (<= 0 (second x) 3)))
           (warn "The OPTIMIZE proclamation ~s is illegal." x)
           (case (car x)
		 (DEBUG)
                 (SAFETY (setq *compiler-check-args* (>= (second x) 1))
                         (setq *safe-compile* (>= (second x) 2))
                         (setq *compiler-push-events* (>= (second x) 3)))
                 (SPACE (setq *space* (second x)))
                 (SPEED (setq *speed* (second x)))
                 (COMPILATION-SPEED (setq *speed* (- 3 (second x))))
                 (t (warn "The OPTIMIZE quality ~s is unknown." (car x)))))))
    (TYPE
     (if (consp (cdr decl))
         (proclaim-var (second decl) (cddr decl))
         (warn "The type declaration ~s is illegal." decl)))
    (FTYPE
     (let (ftype)
       (cond ((and (consp (cdr decl))
		   (consp (setf ftype (second decl)))
		   (eq (first ftype) 'FUNCTION))
	      (dolist (v (cddr decl))
		(add-function-proclamation v (rest ftype))))
	     (t (cmpwarn "Bad function proclamation ~a" decl)))))
    (INLINE
     (dolist (fun (cdr decl))
               (if (symbolp fun)
                   (rem-sysprop fun 'CMP-NOTINLINE)
                   (warn "The function name ~s is not a symbol." fun))))
    (NOTINLINE
     (dolist (fun (cdr decl))
               (if (symbolp fun)
                   (put-sysprop fun 'CMP-NOTINLINE t)
                   (warn "The function name ~s is not a symbol." fun))))
    ((OBJECT IGNORE)
     (dolist (var (cdr decl))
       (unless (symbolp var)
               (warn "The variable name ~s is not a symbol." var))))
    (DECLARATION
     (dolist (x (cdr decl))
       (if (symbolp x)
           (pushnew x *alien-declarations*)
           (warn "The declaration specifier ~s is not a symbol." x))))
    (SI::C-EXPORT-FNAME
     (dolist (x (cdr decl))
       (if (symbolp x)
	 (multiple-value-bind (found fname)
	     (si::mangle-name x t)
	   (if found
	     (warn "The function ~s is already in the runtime." x)
	     (put-sysprop x 'Lfun fname)))
	 (warn "The function name ~ is not a symbol." x))))
    ((ARRAY ATOM BASE-CHAR BIGNUM BIT BIT-VECTOR CHARACTER COMMON COMPILED-FUNCTION
      COMPLEX CONS DOUBLE-FLOAT EXTENDED-CHAR FIXNUM FLOAT HASH-TABLE INTEGER KEYWORD LIST
      LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME RANDOM-STATE RATIO RATIONAL
      READTABLE SEQUENCE SHORT-FLOAT SIMPLE-ARRAY SIMPLE-BIT-VECTOR
      SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT STANDARD-CHAR STREAM STRING
      SYMBOL T VECTOR SIGNED-BYTE UNSIGNED-BYTE FUNCTION)
     (proclaim-var (car decl) (cdr decl)))
    (otherwise
     (unless (member (car decl) *alien-declarations*)
             (warn "The declaration specifier ~s is unknown." (car decl)))
     (and (functionp (get-sysprop (car decl) :proclaim))
	  (dolist (v (cdr decl))
		    (funcall (get-sysprop (car decl) :proclaim) v))))
    )
  nil
  )

(defun proclaim-var (type vl)
  (setq type (type-filter type))
  (dolist (var vl)
    (if (symbolp var)
	(let ((type1 (get-sysprop var 'CMP-TYPE))
	      (v (sch-global var)))
	  (setq type1 (if type1 (type-and type1 type) type))
	  (when v (setq type1 (type-and type1 (var-type v))))
	  (unless type1
	    (warn
	     "Inconsistent type declaration was found for the variable ~s."
	     var)
	    (setq type1 T))
	  (put-sysprop var 'CMP-TYPE type1)
	  (when v (setf (var-type v) type1)))
	(warn "The variable name ~s is not a symbol." var))))

(defun c1body (body doc-p &aux
		    (ss nil)		; special vars
		    (is nil)		; ignored vars
		    (ts nil)		; typed vars (var . type)
		    (others nil)	; all other vars
                    doc form)
  (loop
    (when (endp body) (return))
    (setq form (cmp-macroexpand (car body)))
    (cond
     ((stringp form)
      (when (or (null doc-p) (endp (cdr body)) doc) (return))
      (setq doc form))
     ((and (consp form) (eq (car form) 'DECLARE))
      (dolist (decl (cdr form))
        (cmpck (or (not (consp decl)) (not (symbolp (car decl))))
               "The declaration ~s is illegal." (cons form decl))
        (case (car decl)
          (SPECIAL
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The special declaration ~s contains a non-symbol ~s."
                    decl var)
             (push var ss)))
          (IGNORE
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The ignore declaration ~s contains a non-symbol ~s."
                    decl var)
             (push var is)))
          (TYPE
           (cmpck (endp (cdr decl))
                  "The type declaration ~s is illegal." decl)
           (let ((type (type-filter (second decl))))
                (when type
                      (dolist (var (cddr decl))
                        (cmpck (not (symbolp var))
                          "The type declaration ~s contains a non-symbol ~s."
                          decl var)
                        (push (cons var type) ts)))))
          (OBJECT
           (dolist (var (cdr decl))
             (cmpck (not (symbolp var))
                    "The object declaration ~s contains a non-symbol ~s."
                    decl var)
             (push (cons var 'OBJECT) ts)))
	  (:REGISTER
           (dolist (var (cdr decl))
	      (cmpck (not (symbolp var))
		     "The register declaration ~s contains a non-symbol ~s."
		     decl var)
	      (push (cons var 'REGISTER) ts)
	      ))
	  ;; read-only variable treatment. Beppe
	  (:READ-ONLY
#| obsolete
           (dolist (var (cdr decl))
	      (cmpck (not (symbolp var))
		     "In the :read-only declaration ~s, ~s is not a symbol."
		     decl var)
	      (push (cons var 'READ-ONLY) ts))
|#
	      )
          ((FIXNUM BASE-CHAR EXTENDED-CHAR CHARACTER DOUBLE-FLOAT SHORT-FLOAT ARRAY ATOM BIGNUM BIT
            BIT-VECTOR COMMON COMPILED-FUNCTION COMPLEX CONS FLOAT HASH-TABLE
            INTEGER KEYWORD LIST LONG-FLOAT NIL NULL NUMBER PACKAGE PATHNAME
            RANDOM-STATE RATIO RATIONAL READTABLE SEQUENCE SIMPLE-ARRAY
            SIMPLE-BIT-VECTOR SIMPLE-STRING SIMPLE-VECTOR SINGLE-FLOAT
            STANDARD-CHAR STREAM STRING SYMBOL T VECTOR
            SIGNED-BYTE UNSIGNED-BYTE FUNCTION)
           (let ((type (type-filter (car decl))))
                (when type
                      (dolist (var (cdr decl))
                        (cmpck (not (symbolp var))
                          "The type declaration ~s contains a non-symbol ~s."
                          decl var)
                        (push (cons var type) ts)))))
          (otherwise (push decl others))
          )))
     (t (return)))
    (pop body)
    )
  (values body ss ts is others doc)
  )

(defun c1add-declarations (decls &aux (dl nil))
  (dolist (decl decls dl)
    (case (car decl)
      (OPTIMIZE
       (push decl dl)
       (dolist (x (cdr decl))
	 (when (symbolp x) (setq x (list x 3)))
	 (if (or (not (consp x))
		 (not (consp (cdr x)))
		 (not (numberp (second x)))
		 (not (<= 0 (second x) 3)))
	   (warn "The OPTIMIZE proclamation ~s is illegal." x)
	   (case (car x)
	     (DEBUG)
	     (SAFETY
	      (let ((level (second x)))
		(declare (fixnum level))
		(setq *compiler-check-args* (>= level 1)
		      *safe-compile* (>= level 2)
		      *compiler-push-events* (>= level 3))))
	     (SPACE (setq *space* (second x)))
	     ((SPEED COMPILATION-SPEED))
	     (t (warn "The OPTIMIZE quality ~s is unknown."
		      (car x)))))))
      (FTYPE
       (let (ftype)
	 (cond ((and (consp (cdr decl))
		     (consp (setq ftype (second decl)))
		     (eq (first ftype) 'FUNCTION))
		(dolist (v (cddr decl))
		  (add-function-declaration v (second ftype) (cddr ftype))))
	       (t (cmpwarn "The function declaration ~a is illegal" decl)))))
      (INLINE
       (push decl dl)
       (dolist (fun (cdr decl))
	 (if (symbolp fun)
	   (setq *notinline* (remove fun *notinline*))
	   (warn "The function name ~s is not a symbol." fun))))
      (NOTINLINE
       (push decl dl)
       (dolist (fun (cdr decl))
	 (if (symbolp fun)
	   (push fun *notinline*)
	   (warn "The function name ~s is not a symbol." fun))))
      (DECLARATION
       (dolist (x (cdr decl))
	 (if (symbolp x)
	   (pushnew x *alien-declarations*)
	   (warn "The declaration specifier ~s is not a symbol."
		 x))))
      (SI::C-LOCAL)
      (otherwise
       (unless (member (car decl) *alien-declarations*)
	 (warn "The declaration specifier ~s is unknown."
	       (car decl)))))))

(defun c1decl-body (decls body)
  (if (null decls)
      (c1progn body)
      (let* ((*function-declarations* *function-declarations*)
	     (*alien-declarations* *alien-declarations*)
	     (*notinline* *notinline*)
	     (*space* *space*)
	     (*compiler-check-args* *compiler-check-args*)
	     (*compiler-push-events* *compiler-push-events*)
	     (dl (c1add-declarations decls)))
	(setq body (c1progn body))
	(list 'DECL-BODY (second body) dl body))))

(put-sysprop 'decl-body 'c2 'c2decl-body)

(defun c2decl-body (decls body)
  (let ((*compiler-check-args* *compiler-check-args*)
        (*safe-compile* *safe-compile*)
        (*compiler-push-events* *compiler-push-events*)
        (*notinline* *notinline*)
        (*space* *space*))
    (c1add-declarations decls)
    (c2expr body)))

(defun check-vdecl (vnames ts is)
  (dolist (x ts)
    (unless (member (car x) vnames)
      (cmpwarn "Type declaration was found for not bound variable ~s."
               (car x))))
  (dolist (x is)
    (unless (member x vnames)
      (cmpwarn "Ignore declaration was found for not bound variable ~s." x)))
  )
