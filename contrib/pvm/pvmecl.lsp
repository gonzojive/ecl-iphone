;;;-*-Mode:LISP; Syntax: Common LISP; Base: 10-*-
;;;
;;; File = pvmecl.lsp
;;; Interface between ECoLISP and PVM.
;;; This file contains the C function interface between ECoLisp and PVM.
;;; It is not portable.
;;;
;;;
;;; (c) 1994, I.D. Alexander-Craig, all rights reserved.
;;;
;;;

;;;
;;; pvmconsts.lsp must be loaded before this file.
;;;

;;;
;;; Error function for PVM interface.
;;;

(defun pvm-error (errno routine)
  (error "PVM interface error ~d in ~a~%" errno routine))


(clines "
#include \"/project/pvm/pvm3/include/pvm3.h\"
")


;;;
;;; Begin with buffering routines.
;;;

;;
;; Start with output buffering routines for simple types.
;; Each C function is followed by the corresponding entry
;; definition. Then comes the LISP function.
;;


(definline c_pvm_pkint (fixnum) fixnum
   "({int x = #0; pvm_pkint(&x,1,1);})"
)

(defun obuffer-int (i)
  (let ((info (c_pvm_pkint i)))
    (unless (= %PvmOk info)
      (pvm-error info "obuffer-int")))
  (values))

;;
;; Packing routine for message types. This is a LISP function
;; that calls c_pvm_pkint to pack the type.
;;

(defun pack-type-tag (typetag)
  (let ((return-code (c_pvm_pkint typetag)))
    (unless (= %PvmOk return-code)
      (pvm-error return-code "pack-type-tag")))
  (values))

(defun C-obuffer-nil ()
  (pack-type-tag LISP_NIL_TYPE))

(defun C-obuffer-t ()
  (pack-type-tag LISP_T_TYPE))

(definline c_pvm_pkchar (character) fixnum
   "({char x = #0; pvm_pkbyte(&x,1,1);})"
)

(defun C-obuffer-char (ch)
  (pack-type-tag LISP_CHAR_TYPE)
  (let ((info (c_pvm_pkchar ch)))
    (unless (= %PvmOk info)
	    (pvm-error info "pvm_pkchar call")))
  (values))

(defun C-obuffer-int (i)
  (pack-type-tag LISP_INT_TYPE)
  (let ((info (c_pvm_pkint i)))
    (unless (= %PvmOk info)
	    (pvm-error info "pvm_pkint call")))
  (values))

(definline c_pvm_pkfloat (short-float) fixnum
  "({float x = #0; pvm_pkfloat(&x,1,1);})"
)

(defun obuffer-float (fl)
  (let ((info (c_pvm_pkfloat fl)))
    (unless (= %PvmOk info)
	    (pvm-error info "obuffer-float")))
  (values))

(definline c_pvm_pkdouble (long-float) fixnum
  "({double x = #0; pvm_pkdouble(&x,1,1);})"
)

(defun C-obuffer-double (db)
  (let ((info (c_pvm_pkdouble db)))
    (unless (= %PvmOk info)
	    (pvm-error info "obuffer-double")))
  (values))

;;
;; Packing routines for symbol and string.
;; Both routines expect a string and a number (in that order)
;; to be supplied to them.
;; The number is the length of the string.
;;
;;
;; The first function packs the length and the string into 
;; the output buffer.
;;
(definline c_pvm_pkstr (string fixnum) fixnum
  "({int type = #1;
     type = pvm_pkint(&type,1,1);
     ((type == PvmOk) ? pvm_pkstr((#0)->st.st_self) : type);})"
)
;;
;; Now define the routines that manipulate symbols and strings.
;;

(defun C-obuffer-symbol (s)
  (let ((pname (symbol-name s)))
    (let ((len (length pname)))
      (pack-type-tag LISP_SYMBOL_TYPE)
      (let ((info (c_pvm_pkstr pname len)))
	(unless (= %PvmOk info)
		(pvm-error info "obuffer-symbol")))))
  (values))

(defun C-obuffer-string (str)
  (let ((len (length str)))
    (pack-type-tag LISP_STRING_TYPE)
    (let ((info (c_pvm_pkstr str len)))
      (unless (= %PvmOk info)
	      (pvm-error info "obuffer-string"))))
  (values))

;;
;; Packing routines for vector and list headers.
;;

(defun C-obuffer-vector-header (vector-length)
  (pack-type-tag LISP_VECTOR_TYPE)
  (let ((info (c_pvm_pkint vector-length)))
    (unless (= %PvmOk info)
	    (pvm-error info "obuffer-vector-header")))
  (values))

(defun C-obuffer-list-header ()
  (pack-type-tag LISP_LIST_TYPE)
  (values))

;;
;; Unpacking routines for scalar types.
;;

(defcbody c_pvm_unpack_tag () object
" Cnil;
  { int tagval, info;
    info = pvm_upkint(&tagval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = MAKE_FIXNUM(tagval);
    RETURN(2);
  }"
)

;(proclaim '(inline ibuffer-tag))
(defun ibuffer-tag ()
  (multiple-value-bind (info value)
      (c_pvm_unpack_int)
    (if info
	value
	(pvm-error info "ibuffer-tag"))))

(defun C-next-msg-type ()
  (ibuffer-tag))

(defun C-next-type-name ()
  (ibuffer-tag))

(defcbody c_pvm_unpack_int () object
" Cnil;
  { int ival, info;
    info = pvm_upkint(&ival,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = MAKE_FIXNUM(ival);
    RETURN(2);
  }"
)

(defun C-ibuffer-int ()
  (multiple-value-bind (info value)
      (c_pvm_unpack_int)
    (if info
	value
	(pvm-error info "ibuffer-int"))))

(defcbody c_pvm_unpack_char () object
" Cnil;
  { int info;
    char chval;
    info = pvm_upkbyte(&chval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = code_char(chval);
    RETURN(2);
  }"
)

(defun C-ibuffer-char ()
  (multiple-value-bind (info value)
      (c_pvm_unpack_char)
    (if info
	value
	(pvm-error info "ibuffer-char"))))

(defcbody c_pvm_unpack_float () object
" Cnil;
  { int info;
    float fval;
    info = pvm_upkfloat(&fval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_shortfloat(fval);
    RETURN(2);
  }"
)

(defun ibuffer-float ()
  (multiple-value-bind (info value)
      (c_pvm_unpack_float)
    (if info
	value
	(pvm-error info "ibuffer-float"))))

(defcbody c_pvm_unpack_double () object
" Cnil;
  {
    int info;
    double dval;
    info = pvm_upkdouble(&dval,1,1);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_longfloat(dval);
    RETURN(2);
  }"
)

(defun C-ibuffer-double ()
  (multiple-value-bind (info value)
      (c_pvm_unpack_double)
    (if info
	value
	(pvm-error info "ibuffer-double"))))


;;
;; Routines to get symbols and strings from the PVM
;; buffer.
;; This is a little tricky!
;;

;;
;; First, a general unpacking routine for strings.
;;

(defun setstring (chr indx str)
  (setf (aref str indx) chr)
  (values))

(defcbody c_pvm_unpack_chars (fixnum) object
"
  Cnil;
  { char *strchrs;
    int info;
    info = pvm_upkstr(strchrs);
    if (info != PvmOk) { VALUES(0) = Cnil; RETURN(1);}
    VALUES(0) = MAKE_FIXNUM(info);
    VALUES(1) = make_simple_string(strchrs);
    RETURN(2);
  }"
)


;;
;; Now the routine which gets the length and the string
;; from the buffer.
;;

(defun get-length-and-string ()
  (let ((len (ibuffer-int)))
    (multiple-value-bind (info str)
	(c_pvm_unpack_chars len)
      (if info
	  (if (= (length str) len)
	      str
	      (format 
	       t 
	       "received string has length ~a, not ~a as promised.~%"
	       (length str)
	       len))
	  (pvm-error info "get-length-and-string")))))

(defun C-ibuffer-symbol ()
  ; It might be useful sometimes just to return the string.
  (let ((pname (get-length-and-string)))
    (make-symbol pname)))

(defun C-ibuffer-string ()
  (get-length-and-string))


(defun C-ibufer-vector-length ()
  (C-ibuffer-int))


;;;
;;; Send and received routines (together with registration and exit).
;;;

(definline c_pvm_initsend (fixnum) fixnum
  "pvm_initsend(#0)")

(defun lpvm-init-send (encoding)
  (cond ((not (integerp encoding))
	 (error "lpvm-init-send expects an int, not a ~a~%"
		(type-of encoding)))
	((minusp encoding)
	 (error
	  "lpvm-init-send: encoding must be non-negative (~d)~%"
	  encoding))
	(t
	 (let ((bufid (c_pvm_initsend encoding)))
	   (when (minusp bufid)
		 (pvm-error bufid "pvm_initsend call"))
	   bufid))))

(definline c_pvm_send (fixnum fixnum) fixnum
  "pvm_send(#0, #1)")

;;;
;;; The send routine.
;;;

(defun lpvm-send-message (lisp-object
			  reader-object
			  message-type
			  destination-task
			  &optional (encoding %PvmDataDefault))
  (lpvm-init-send encoding)
  (write-object lisp-object reader-object)
  (let ((info (c_pvm_send destination-task message-type)))
    (when (minusp info)
	  (pvm-error info "pvm_send call")))
  (values))

;;;
;;; The multi-cast routine is similar, but we set up the buffer
;;; once and then repeatedly send the message.
;;;

(defun lpvm-multicast (lisp-object
		       reader-object
		       message-type
		       destination-tasks
		       &optional (encoding %PvmDataDefault))
  (lpvm-init-send encoding)
  (write-object lisp-object reader-object)
  (dolist (tid destination-tasks)
	  (let ((info (c_pvm_send tid message-type)))
	    (when (minusp info)
		  (pvm-error info "pvm_multicast"))))
  (values))

;;;
;;; Receive routines.
;;;

(definline c_pvm_nrecv (fixnum fixnum) fixnum
  "pvm_nrecv(#0,#1)"
)

(defun lpvm-nonblocking-recv (object-reader tid msgtag)
  (let ((bufid (c_pvm_nrecv tid msgtag)))
    (cond ((minusp bufid)
	   (pvm-error bufid "pvm_nrecv"))
	  ((= %PvmOk bufid)
	   ()) ; nothing there
	  ((plusp bufid)
	   (read-object object-reader))
	  (t
	   (error
	    "something weird has happened---nonblocking-recv")))))

(definline c_pvm_recv (fixnum fixnum) fixnum
  "pvm_recv(#0, #1)"
)

(defun lpvm-blocking-read (object-reader tid msgtag)
  (let ((bufid (c_pvm_recv tid msgtag)))
    (when (minusp bufid)
	  (pvm-error bufid "pvm_recv"))
    (read-object object-reader)))


;;;
;;; Join PVM primitive.
;;;

(definline c_pvm_mytid () fixnum
  "pvm_mytid()"
)

(defun lpvm-my-tid ()
  (let ((info (c_pvm_mytid)))
    (when (minusp info)
	  (pvm-error info "pvm_mytid call"))
    info))


;;;
;;; Leave PVM primitive.
;;;

(definline c_pvm_exit () fixnum
  "pvm_exit()")

(defun lpvm-exit ()
  (let ((info (c_pvm_exit)))
    (unless (= %PvmOk info)
	    (pvm-error info "pvm_exit call")))
  (values))


(definline c_pvm_kill (fixnum) fixnum
  "pvm_kill(#0)"
)

(defun lpvm-kill (tid)
  (let ((info (c_pvm_kill tid)))
    (when (minusp info)
	  (pvm-error info "pvm_kill call")))
  (values))

(definline c_pvm_parent () fixnum
  "pvm_parent()"
)

(defun lpvm-parent ()
  (let ((info (c_pvm_parent)))
    (when (= info %PvmNoParent)
	  (pvm-error info "pvm_parent")))
  (values))

(definline c_pvm_pstat (fixnum) fixnum
  "pvm_pstat(#0)"
)

(defun lpvm-pstat (tid)
  (let ((info (c_pvm_pstat tid)))
    (cond ((= info %PvmOk)
	   info)
	  ((= info %PvmNoTask)
	   info)
	  (t
	   (pvm-error info "pvm_stat call")))))

(definline c_pvm_mstat (string) fixnum
  "pvm_mstat(#0->st.st_self)"
)

(defun lpvm-mstat (hostname)
  (unless (stringp hostname)
	  (error "lpvm-mstat: hostnames must be strings, not ~a~%"
		 (type-of hostname)))
  (let ((info (c_pvm_mstat hostname)))
    (cond ((= info %PvmOk)
	   'running)
	  ((= info %PvmNoHost)
	   'no-such-host)
	  ((= info %PvmHostFail)
	   'host-unreachable)
	  (t
	   (pvm-error info "pvm_mstat call")))))

(defcbody c_pvm_spawn (string fixnum string fixnum) object
"
   Cnil;
   {
     int numt, tid, i;
     int sz = #1;
     object v;
     extern object lisp_package;

     siLmake_vector(7, intern(\"FIXNUM\", lisp_package),
                        MAKE_FIXNUM(sz), Cnil, Cnil, Cnil, Cnil, Cnil);
     v = VALUES(0);
     numt = pvm_spawn(#0->st.st_self, 0, #1, #2->st.st_self, #3, v->v.v_self);
     if (numt < PvmOk) RETURN(1);
     VALUES(0) = MAKE_FIXNUM(numt);
     VALUES(1) = v;
     RETURN(2);
   }"
)

(defun lpvm-spawn (taskname flag where numtasks)
  (cond ((not (stringp taskname))
	 (error "spawn -- wrong type: ~A" (type-of taskname)))
	((not (integerp flag))
	 (error "spawn -- wrong type: ~A" (type-of flag)))
	((not (stringp where))
	 (error "spawn -- wrong type: ~A" (type-of where)))
	((not (integerp numtasks))
	 (error "spawn -- wrong type: ~A" (type-of numtasks)))
	((not (and (<= 1 numtasks)
		   (<= numtasks 32)))
	 (error "spawn -- wrong number of tasks: ~D" numtasks))
	(t
	 (multiple-value-bind (num-spawned tids)
	     (c_pvm_spawn taskname flag where numtasks)
	   (if (minusp num-spawned)
	       (pvm-error num-spawned "pvm_spawn call")
	       (values num-spawned tids))))))


(definline c_pvm_sendsig (fixnum fixnum) fixnum
  "pvm_sendsig(#0,#1)"
)

(defun lpvm-sendsig (tid signum)
  (let ((info (c_pvm_sendsig tid signum)))
    (when (minusp info)
	  (pvm-error info "pvm_sendsig call")))
  (values))

(definline c_pvm_advise (fixnum) fixnum
  "pvm_advise(#0)"
)

(defun lpvm-advise (route)
  (let ((info (c_pvm_advise route)))
    (unless (= info %PvmOk)
	    (pvm-error info "pvm_advise call")))
  (values))

;;;;
;;;; Group operations.
;;;;


(definline c_pvm_join_group (object) fixnum
  "pvm_joingroup(#0->st.st_self)"
)

(defun lpvm-join-group (group)
  (unless (stringp group)
	  (error "lpvm-join-grou expects a string, not a ~a~%"
		 (type-of group)))
  (let ((inum (c_pvm_joingroup group)))
    (when (minusp inum)
	  (pvm-error inum "pvm_joingroup call"))
    inum))

(definline c_pvm_leave_group (object) fixnum
  "pvm_lvgroup(#0->st.st_self)"
)

(defun lpvm-leave-group (group)
  (unless (stringp group)
	  (error
	   "lpvm-leave-group expects a string, not a ~a~%"
	   (type-of group)))
  (let ((info (c_pvm_leave_group group)))
    (when (minusp info)
	  (pvm-error info "pvm_lvgroup call")))
  (values))

(definline c_pvm_get_tid (object fixnum) fixnum
  "pvm_gettid(#0->st.st_self, #1)"
)

(defun lpvm-get-tid (group inum)
  (unless (stringp group)
	  (error
	   "lpvm-get-tid expects arg 1 to be a string, not a ~a~%"
	   (type-of group)))
  (unless (integerp inum)
	  (error
	   "lpvm-get-tid expects arg 2 to be an int, not a ~a~%"
	   (type-of inum)))
  (let ((info (c_pvm_get_tid group inum)))
    (cond ((plusp info)
	   info)
	  ((minusp info)
	   (pvm-error info "pvm_gettid call"))
	  (t
	   (pvm-error 0 "pvm_gettid: should not happen")))))

(definline c_pvm_get_inst (object fixnum) fixnum
  "pvm_getinst(#0->st.st_self, #1)"
)

(defun lpvm-get-inst-no (group tid)
  (cond ((not (stringp group))
	 (error
	  "lpvm-get-inst-no expects arg1 to be a string, not a ~a~%"
	  (type-of group)))
	((not (integerp tid))
	 (error
	  "lpvm-get-inst-no expects arg2 to be an int, not a ~a~%"
	  (type-of tid)))
	(t
	 (let ((inum (c_pvm_get_inst group tid)))
	   (when (minusp inum)
		 (pvm-error inum "pvm_getinst call"))
	   inum))))

(definline c_pvm_grpsize (object) fixnum
  "pvm_gsize(#0->st.st_self)"
)

(defun lpvm-group-size (group)
  (unless (stringp group)
	  (error
	   "lpvm-group-size expects a string not a ~a~%"
	   (type-of group)))
  (let ((size (c_pvm_grpsize group)))
    (when (minusp size)
	  (pvm-error size "pvm_gsize call"))
    size))

(definline c_pvm_barrier (object fixnum) fixnum
  "pvm_barrier(#0->st.st_self,#1)"
)

(defun lpvm-barrier (group count)
  (cond ((not (stringp group))
	 (error
	  "lpvm-barrier expects arg 1 to be a string, not a ~a~%"
	  (type-of group)))
	((not (integerp count))
	 (error
	  "lpvm-barriet expects arg 2 to be an int, not a ~a~%"
	  (type-of count)))
	(t
	 (let ((info (c_pvm_barrier group count)))
	   (unless (= %PvmOk info)
		   (pvm-error info "pvm_barrier call")))))
  (values))

(definline c_pvm_broadcast (object fixnum) fixnum
  "pvm_bcast(#0->st.st_self,#1)"
)

(defun lpvm-broadcast (lisp-object
		       reader-object
		       message-type
		       group-name
		       &optional (encoding %PvmDataDefault))
  (lpvm-init-send encoding)
  (write-object lisp-object reader-object)
  (let ((info (c_pvm_broadcast group-name message-type)))
    (when (minusp info)
	  (pvm-error info "pvm_bcast call")))
  (values))


(defCbody c_pvm_probe (fixnum fixnum) fixnum
  "0;
   { int bufid, info;
     int *bytes;
     int out_tid, out_tag;
     VALUES(0) = Cnil;
     bufid = pvm_probe(#0,#1);
     if (bufid == 0) RETURN(1);
     if (bufid < 0) { 
       VALUES(0) = CONS(MAKE_FIXNUM(bufid), Cnil);
       RETURN(1);
     }
     info = pvm_bufinfo(bufid,bytes,&out_tag,&out_tid);
     VALUES(0) = list(3, MAKE_FIXNUM(info), MAKE_FIXNUM(out_tag),
                         MAKE_FIXNUM(out_tid));
     RETURN(1);
   }"
)

(defun lpvm-probe (tid msgno)
  (let ((return-val (c_pvm_probe tid msgno)))
    (let ((num-returned (length return-val))
	  (out-tid 0)
	  (out-tag 0)
	  (info    0))
      (cond ((= num-returned 1)
	     (pvm-error (car return-val) "pvm_probe call"))
	    (t
	     (setf info    (first  return-val))
	     (setf out-tag (second return-val))
	     (setf out-tid (third  return-val))
	     (if (= info %PvmOk)
		 (values out-tid out-tag)
	       (pvm-error info "pvm_probe call")))))))
	

;;;;
;;;; Add and delete hosts.
;;;;

;;
;; add_host adds a single host to the machine. hostname is the
;; string name of the host. The function returns a pair.

(defCbody c_pvm_add_host (object) object
  "Cnil;
   { int host_info[1];
     int info, hival;
     info = pvm_addhosts(&(#0)->st.st_self,1,host_info);
     hival = host_info[0];
     VALUES(0) = list(2, MAKE_FIXNUM(info), MAKE_FIXNUM(hival));
     RETURN(1);
   }"
)

(defun add-hosts (hostnames)
  (let ((results (make-array (length hostnames))))
    (dotimes (host (length hostnames))
      (let ((host (aref hostnames)))
	(c_pvm_add_host host)
	(setf (aref results host)(cadr host))))
    results))
	       

(defCbody c_pvm_del_host (object) object
  "Cnil;
   { int host_info[1];
     int info, hival;
     info = pvm_delhosts(&(#0)->st.st_self,1,host_info);
     hival = host_info[0];
     VALUES(0) = list(2, MAKE_FIXNUM(info), MAKE_FIXNUM(hival));
     RETURN(1);
   }"
)

(defun del-hosts (hostnames)
  (let ((results (make-array (length hostnames))))
    (dotimes (host (length hostnames))
      (let ((host (aref hostnames)))
	(c_pvm_add_host host)
	(setf (aref results host) (cadr host))))
    results))

