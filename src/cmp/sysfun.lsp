;;; CMPSYSFUN   Database for system functions.
;;;
;;; Copyright (c) 1991, Giuseppe Attardi. All rights reserved.
;;;    Copying of this file is authorized to users who have executed the true
;;;    and proper "License Agreement for ECoLisp".

;;;
;;; For each system function it provides:
;;;
;;; 	name		of corresponding C function
;;;	argtypes	(list of types of arguments, * means optionals)
;;;	return-type
;;;	never-change-special-var-p
;;;	predicate
;;;	optimizers
;;;
;;; An optimizer is a property list:
;;;	{ property inline-info }*
;;;
;;; Valid property names are:
;;;  :INLINE-ALWAYS
;;;  :INLINE-SAFE	safe-compile only
;;;  :INLINE-UNSAFE	non-safe-compile only
;;;
;;; An inline-info is:
;;; ( types { type | boolean } side-effect new-object { string | function } ).
;;; string is:
;;; [@i..;]Cexpr(#0, ..., #n)
;;; where #0 indicates the first argument. The optional @i indicates an
;;; argument to be saved into a variable before evaluating Cexpr.

;;; The following flag:
;;;	side-effect-p
;;; which is repeated in each optimizer, could be supplied just once
;;; per function, while
;;;	allocates-new-storage
;;; could be just eliminated since we use a conservative GC.

(in-package "COMPILER")

(defun defsysfun (fname &optional
		  arg-types return-type
		  never-change-special-var-p predicate
		  &rest optimizers)
  ;; The value NIL for each parameter except for fname means "not known".
  ;; optimizers is a list of alternating {safety inline-info}* as above.
  (when arg-types
    (put-sysprop fname 'arg-types
		 (mapcar #'(lambda (x) (if (eql x '*) '* (type-filter x)))
			 arg-types)))
  (when (and return-type (not (eq 'T return-type)))
    (put-sysprop fname 'return-type
		 (if (eql return-type '*) '* (type-filter return-type))))
  (when never-change-special-var-p (put-sysprop fname 'no-sp-change t))
  (when predicate (put-sysprop fname 'predicate t))
  (rem-sysprop fname ':inline-always)
  (rem-sysprop fname ':inline-safe)
  (rem-sysprop fname ':inline-unsafe)
  (do ((scan optimizers (cddr scan))
       (safety) (inline-info))
      ((null scan))
      (setq safety (first scan)
	    inline-info (second scan))
      (put-sysprop fname safety (cons inline-info (get-sysprop fname safety))))
  )

; file alloc.c
#-boehm-gc
(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
(si::ALLOC)
(si::NPAGE)
(si::MAXPAGE)
(si::ALLOC-CONTPAGE)
(si::NCBPAGE)
(si::MAXCBPAGE)
(si::ALLOC-RELPAGE)
(si::NRBPAGE)
(si::GET-HOLE-SIZE)
(si::SET-HOLE-SIZE)))

(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
(si::RPLACA-NTHCDR nil T)
(si::LIST-NTH nil T)
(si::MAKE-PURE-ARRAY nil array)
(si::MAKE-VECTOR nil vector)
;(si::MAKE-BITVECTOR nil bit-vector nil nil)
(AREF (array *) T NIL NIL
	:inline-unsafe ((t t t) t nil t
		"@0;aref(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
	:inline-unsafe (((array t) t t) t nil nil
		"@0;(#0)->array.self.t[fix(#1)*(#0)->array.dims[1]+fix(#2)]")
	:inline-unsafe (((array bit) t t) fixnum nil nil
		"@0;aref_bv(#0,fix(#1)*(#0)->array.dims[1]+fix(#2))")
	:inline-unsafe (((array t) fixnum fixnum) t nil nil
		"@0;(#0)->array.self.t[#1*(#0)->array.dims[1]+#2]")
	:inline-unsafe (((array bit) fixnum fixnum) fixnum nil nil
		"@0;aref_bv(#0,(#1)*(#0)->array.dims[1]+#2)")
	:inline-unsafe (((array base-char) fixnum fixnum) character nil nil
		"@0;(#0)->string.self[#1*(#0)->array.dims[1]+#2]")
	:inline-unsafe (((array long-float) fixnum fixnum) long-float nil nil
		"@0;(#0)->array.self.lf[#1*(#0)->array.dims[1]+#2]")
	:inline-unsafe (((array short-float) fixnum fixnum) short-float nil nil
		"@0;(#0)->array.self.sf[#1*(#0)->array.dims[1]+#2]")
	:inline-unsafe (((array fixnum) fixnum fixnum) fixnum nil nil
		"@0;(#0)->array.self.fix[#1*(#0)->array.dims[1]+#2]")

	:inline-always ((t t) t nil t "aref1(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "aref1(#0,#1)")
	:inline-unsafe ((t t) t nil t "aref1(#0,fix(#1))")
	:inline-unsafe ((t fixnum) t nil t "aref1(#0,#1)")
	:inline-unsafe (((array bit) t) fixnum nil nil "aref_bv(#0,fix(#1))")
	:inline-unsafe (((array bit) fixnum) fixnum nil nil "aref_bv(#0,#1)")
	:inline-unsafe (((array base-char) fixnum) t nil nil
		"CODE_CHAR((#0)->string.self[#1])")
	:inline-unsafe (((array long-float) fixnum) t nil nil
		"make_longfloat((#0)->array.self.lf[#1])")
	:inline-unsafe (((array short-float) fixnum) t nil nil
		"make_shortfloat((#0)->array.self.sf[#1])")
	:inline-unsafe (((array fixnum) fixnum) t nil nil
		"MAKE_FIXNUM((#0)->array.self.fix[#1])")
	:inline-unsafe (((array base-char) fixnum) fixnum nil nil
		"(#0)->string.self[#1]")
	:inline-unsafe (((array base-char) fixnum) character nil nil
		"(#0)->string.self[#1]")
	:inline-unsafe (((array long-float) fixnum) long-float nil nil
		"(#0)->array.self.lf[#1]")
	:inline-unsafe (((array short-float) fixnum) short-float nil nil
		"(#0)->array.self.sf[#1]")
	:inline-unsafe (((array fixnum) fixnum) fixnum nil nil
		"(#0)->array.self.fix[#1]")
)
(SI::ASET (T ARRAY *) NIL NIL NIL
	:inline-unsafe ((t t t t) t t nil
		"@0;aset(#1,fix(#2)*(#1)->array.dims[1]+fix(#3),#0)")
	:inline-unsafe ((t t fixnum fixnum) t t nil
		"@0;aset(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")
	:inline-unsafe ((t (array t) fixnum fixnum) t t nil
		"@1;(#1)->array.self.t[#2*(#1)->array.dims[1]+#3]= #0")
	:inline-unsafe ((t (array bit) fixnum fixnum) fixnum t nil
		"@0;aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),fix(#0))")

	:inline-unsafe
	((character (array base-char) fixnum fixnum) character t nil
		"@1;(#1)->string.self[#2*(#1)->array.dims[1]+#3]= #0")
	:inline-unsafe 
	((long-float (array long-float) fixnum fixnum) long-float t nil
		"@1;(#1)->array.self.lf[#2*(#1)->array.dims[1]+#3]= #0")
	:inline-unsafe
	((short-float (array short-float) fixnum fixnum) short-float t nil
		"@1;(#1)->array.self.sf[#2*(#1)->array.dims[1]+#3]= #0")
	:inline-unsafe
	((fixnum (array fixnum) fixnum fixnum) fixnum t nil
		"@1;(#1)->array.self.fix[#2*(#1)->array.dims[1]+#3]= #0")
	:inline-unsafe ((fixnum (array bit) fixnum fixnum) fixnum t nil
		"@0;aset_bv(#1,(#2)*(#1)->array.dims[1]+(#3),#0)")

	:inline-always ((t t t) t t nil "aset1(#1,fixint(#2),#0)")
	:inline-always ((t t fixnum) t t nil "aset1(#1,#2,#0)")

	:inline-unsafe ((t t t) t t nil "aset1(#1,fix(#2),#0)")
	:inline-unsafe ((t (array t) fixnum) t t nil "(#1)->vector.self.t[#2]= #0")
	:inline-unsafe ((t (array bit) fixnum) fixnum t nil "aset_bv(#1,#2,fix(#0))")
	:inline-unsafe ((character (array base-char) fixnum) character t nil
		"(#1)->string.self[#2]= #0")
	:inline-unsafe ((long-float (array long-float) fixnum) long-float t nil
		"(#1)->array.self.lf[#2]= #0")
	:inline-unsafe ((short-float (array short-float) fixnum) short-float t nil
		"(#1)->array.self.sf[#2]= #0")
	:inline-unsafe ((fixnum (array fixnum) fixnum) fixnum t nil
		"(#1)->array.self.fix[#2]= #0")
	:inline-unsafe ((fixnum (array bit) fixnum) fixnum t nil
		"aset_bv(#1,#2,#0)"))
(ROW-MAJOR-AREF (array fixnum) t
	:inline-always ((array fixnum) t nil t "aref(#0,#1)"))
(SI::ROW-MAJOR-ASET (array fixnum t) t
	:inline-always ((array fixnum t) t nil t "aset(#0,#1,#2)"))
(ARRAY-ELEMENT-TYPE (array) T)
(ARRAY-RANK (array) fixnum)
(ARRAY-DIMENSION (array fixnum) fixnum)
(ARRAY-TOTAL-SIZE (array) T nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.dim)"))
(ADJUSTABLE-ARRAY-P (array) T nil t)
(ARRAY-DISPLACEMENT (array) (VALUES T FIXNUM) nil t)
(SVREF (simple-vector fixnum) T nil nil
	:inline-always ((t t) t nil t "aref1(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "aref1(#0,#1)")
	:inline-unsafe ((t t) t nil nil "(#0)->vector.self.t[fix(#1)]")
	:inline-unsafe ((t fixnum) t nil nil "(#0)->vector.self.t[#1]"))
(si::SVSET (simple-vector fixnum t) T nil nil
	:inline-always ((t t t) t t nil "aset1(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "aset1(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "((#0)->vector.self.t[fix(#1)]=(#2))")
	:inline-unsafe ((t fixnum t) t t nil "(#0)->vector.self.t[#1]= #2"))
(ARRAY-HAS-FILL-POINTER-P nil T nil t)
(FILL-POINTER (vector) fixnum nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.fillp)"))
(si::FILL-POINTER-SET (vector fixnum) fixnum nil nil
	:inline-unsafe ((t fixnum) fixnum t nil "((#0)->string.fillp)=(#1)"))
(si::REPLACE-ARRAY nil T nil nil)
;(si::ASET-BY-CURSOR nil T nil nil)

; file assignment.c
(SET (symbol t) T)
(si::FSET (symbol t) T)
(MAKUNBOUND (symbol) T)
(FMAKUNBOUND (symbol) T)
(si::CLEAR-COMPILER-PROPERTIES nil T)

; file catch.c
;#-clcs (SI::ERROR-SET (T) * NIL NIL)

; file cfun.c
(si::COMPILED-FUNCTION-NAME nil T)

; file character.c
(STANDARD-CHAR-P (character) T nil t)
(GRAPHIC-CHAR-P (character) T nil t)
(ALPHA-CHAR-P (character) T nil t
	:inline-always ((character) boolean nil nil "isalpha(#0)"))
(UPPER-CASE-P (character) T nil t
	:inline-always ((character) boolean nil nil "isupper(#0)"))
(LOWER-CASE-P (character) T nil t
	:inline-always ((character) boolean nil nil "islower(#0)"))
(BOTH-CASE-P (character) T nil t
	:inline-always ((character) boolean nil nil "(islower(#0)||isupper(#0))"))
(DIGIT-CHAR-P (character *) T nil nil
	:inline-always
	((character) boolean nil nil "@0; ((#0) <= '9' && (#0) >= '0')"))
(ALPHANUMERICP (character) T nil t
	:inline-always ((character) boolean nil nil "isalnum(#0)"))
(CHARACTER (T) CHARACTER)
(CHAR= (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)==char_code(#1)"))
(CHAR/= (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)!=(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)!=char_code(#1)"))
(CHAR< (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<(#1)"))
(CHAR> (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>(#1)"))
(CHAR<= (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<=(#1)"))
(CHAR>= (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>=(#1)"))
(CHAR-EQUAL (character *) T nil t)
(CHAR-NOT-EQUAL (character *) T nil t)
(CHAR-LESSP (character *) T nil t)
(CHAR-GREATERP (character *) T nil t)
(CHAR-NOT-GREATERP (character *) T nil t)
(CHAR-NOT-LESSP (character *) T nil t)
(CHARACTER nil character nil nil)
(CHAR-CODE (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(CODE-CHAR (fixnum) character nil nil
	:inline-always ((fixnum) character nil nil "#0"))
(CHAR-UPCASE (character) character nil nil
	:inline-always ((character) character nil nil "toupper(#0)"))
(CHAR-DOWNCASE (character) character nil nil
	:inline-always ((character) character nil nil "tolower(#0)"))
(DIGIT-CHAR (fixnum *) (or character null) nil nil)
(CHAR-INT (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(CHAR-NAME (character) (or string null))
(NAME-CHAR (string) (or character null))

; ; file error.c
#-clcs
(ERROR (T *) T nil nil)
#-clcs
(CERROR (T T *) T nil nil)

(si::IHS-TOP (T) T)
(si::IHS-FUN)
(si::IHS-ENV)
(si::FRS-TOP)
(si::FRS-VS)
(si::FRS-BDS)
(si::FRS-CLASS)
(si::FRS-TAG)
(si::FRS-IHS)
(si::BDS-TOP)
(si::BDS-VAR)
(si::BDS-VAL)
(si::VS-TOP)
(si::VS)
(si::SCH-FRS-BASE)

; file eval.c
(APPLY (T T *) T)
(FUNCALL (T *) T)
(EVAL (T) T)
(EVALHOOK (T T T *) T)
(APPLYHOOK (T T T T *) T)
(CONSTANTP (T) T NIL T)
(si::UNLINK-SYMBOL nil T)
(si::LINK-ENABLE nil T)

; file file.d
(MAKE-SYNONYM-STREAM (T) T)
(MAKE-BROADCAST-STREAM (*) T)
(MAKE-CONCATENATED-STREAM nil T)
(MAKE-TWO-WAY-STREAM (T T) T)
(MAKE-ECHO-STREAM (T T) T)
(MAKE-STRING-INPUT-STREAM nil T)
(MAKE-STRING-OUTPUT-STREAM nil T)
(GET-OUTPUT-STREAM-STRING nil T)
(SI::OUTPUT-STREAM-STRING (T) T)
(STREAMP (T) T NIL T)
(INPUT-STREAM-P (T) T NIL T)
(OUTPUT-STREAM-P (T) T NIL T)
(STREAM-ELEMENT-TYPE (T) T)
(CLOSE (T *) T)
;#-clcs (OPEN (T *) T)
(FILE-POSITION (T *) T)
(FILE-LENGTH (T) T)
;#-clcs (LOAD (T *) T)
(si::GET-STRING-INPUT-STREAM-INDEX nil T)
(si::MAKE-STRING-OUTPUT-STREAM-FROM-STRING nil T)

; file gbc.c
(si::ROOM-REPORT nil T)
(si::RESET-GBC-COUNT nil T)
(GBC nil T)

; file unixfsys.c
(TRUENAME (T) T)
(RENAME-FILE (T T) T)
(SI::SPECIALP (T) T NIL T)
(DELETE-FILE (T) T)
(PROBE-FILE (T) T)
(FILE-WRITE-DATE (T) T)
(FILE-AUTHOR (T) T)
(PATHNAME (T) T)
(USER-HOMEDIR-PATHNAME (*) T)
(DIRECTORY (T) T)
(si::CHDIR nil T)

; file unixint.c
(si::CATCH-BAD-SIGNALS nil T)
(si::UNCATCH-BAD-SIGNALS nil T)

; file format.c
(FORMAT (T string *) T)

; file hash.d
(MAKE-HASH-TABLE (*) T)
(HASH-TABLE-P (T) T NIL T)
(VALUES (*) *)
(GETHASH (T T *) (VALUES T T))
(REMHASH (T T) T)
(MAPHASH (T T) T)
(CLRHASH (T) T)
(HASH-TABLE-COUNT (T) T)
(SXHASH (T) FIXNUM)
(SI::HASH-SET NIL T)

; file list.d
(CAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(CDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(CAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAAR(#0)"))
(CADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(CDAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDAR(#0)"))
(CDDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDR(#0)"))
(CAAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAAAR(#0)"))
(CAADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAADR(#0)"))
(CADAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADAR(#0)"))
(CADDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(CDAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDAAR(#0)"))
(CDADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDADR(#0)"))
(CDDAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDAR(#0)"))
(CDDDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDDR(#0)"))
(CAAAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAAAAR(#0)"))
(CAAADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAAADR(#0)"))
(CAADAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAADAR(#0)"))
(CAADDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CAADDR(#0)"))
(CADAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADAAR(#0)"))
(CADADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADADR(#0)"))
(CADDAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADDAR(#0)"))
(CADDDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(CDAAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDAAAR(#0)"))
(CDAADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDAADR(#0)"))
(CDADAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDADAR(#0)"))
(CDADDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDADDR(#0)"))
(CDDAAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDAAR(#0)"))
(CDDADR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDADR(#0)"))
(CDDDAR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDDAR(#0)"))
(CDDDDR (T) T NIL NIL
	:inline-unsafe ((t) t nil nil "CDDDDR(#0)"))
(CONS (T T) T NIL NIL
	:inline-always ((t t) t nil t "CONS(#0,#1)"))
(TREE-EQUAL (T T *) T NIL T)
(ENDP (T) T NIL T
	:inline-safe ((t) boolean nil nil "endp(#0)")
	:inline-unsafe ((t) boolean nil nil "#0==Cnil"))
(LIST-LENGTH (T) T NIL NIL)
(NTH (T T) T NIL NIL
	:inline-always ((t t) t nil nil "nth(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nth(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nth(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nth(#0,#1)"))
(FIRST (T) T NIL NIL
;	:inline-safe ((t) t nil nil "cl_car(#0)")
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(SECOND (T) T nil nil
;	:inline-safe ((t) t nil nil "cl_cadr(#0)")
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(THIRD (T) T nil nil
;	:inline-safe ((t) t nil nil "cl_caddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(FOURTH (T) T nil nil
;	:inline-safe ((t) t nil nil "cl_cadddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(FIFTH (T) T)
(SIXTH (T) T)
(SEVENTH (T) T)
(EIGHTH (T) T)
(NINTH (T) T)
(TENTH (T) T)
(REST (T) T NIL NIL
;	:inline-safe ((t) t nil nil "cl_cdr(#0)")
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(NTHCDR (fixnum t) T nil nil
	:inline-always ((t t) t nil nil "nthcdr(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nthcdr(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nthcdr(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nthcdr(#0,#1)"))
(LAST (T) T)
(LIST (*) LIST NIL NIL
	:inline-always (nil t nil nil "Cnil")
	:inline-always ((t) t nil t "CONS(#0,Cnil)"))
(LIST* (T *) LIST NIL NIL
	:inline-always ((t) t nil nil "#0")
	:inline-always ((t t) t nil t "CONS(#0,#1)"))
(MAKE-LIST (fixnum *) T)
(APPEND (*) T NIL NIL
	:inline-always ((t t) t nil t "append(#0,#1)"))
(COPY-LIST (T) LIST)
(COPY-ALIST (T) LIST)
(COPY-TREE (T) LIST)
(REVAPPEND (T T) T)
(NCONC (*) T NIL NIL
	:inline-always ((t t) t t nil "nconc(#0,#1)"))
(NRECONC (T T) T)
(BUTLAST (T *) T)
(NBUTLAST (T *) T)
(LDIFF (T T) T)
(RPLACA (cons T) CONS)
(RPLACD (cons T) CONS)
(SUBST (T T T *) T)
(SUBST-IF (T T T *) T)
(SUBST-IF-NOT (T T T *) T)
(NSUBST (T T T *) T)
(NSUBST-IF (T T T *) T)
(NSUBST-IF-NOT (T T T *) T)
(SUBLIS (T T *) T)
(NSUBLIS (T T *) T)
(MEMBER (T T *) T)
(MEMBER-IF (T T *) T)
(MEMBER-IF-NOT (T T *) T)
(MEMBER1 (T T *) T)
(TAILP (T T) T NIL T)
(ADJOIN (T T *) T)
(ACONS (T T T) T)
(PAIRLIS (T T *) T)
(ASSOC (T T *) LIST)
(ASSOC-IF (T T) LIST)
(ASSOC-IF-NOT (T T) LIST)
(RASSOC (T T *) LIST)
(RASSOC-IF (T T) LIST)
(RASSOC-IF-NOT (T T) LIST)
(si::MEMQ (T T T) T)

; file lwp.c
;to do

; file macros.c
(si::DEFINE-MACRO nil T)
(MACROEXPAND (T *) (VALUES T T))
(MACROEXPAND-1 (T *) (VALUES T T))

; file main.c
(QUIT nil T)
(IDENTITY (T) T)
(si::ARGC nil T)
(si::ARGV nil T)
(si::GETENV nil T)
(si::RESET-STACK-LIMITS nil T)
#-sparc
(si::INIT-SYSTEM nil T)
(si::POINTER nil T)

; file mapfun.c
(MAPCAR (T T *) T)
(MAPLIST (T T *) T)
(MAPC (T T *) T)
(MAPL (T T *) T)
(MAPCAN (T T *) T)
(MAPCON (T T *) T)

; file multival.c
(VALUES nil T)
(VALUES-LIST (T) *)

; file num_arith.c
(+ (*) T NIL NIL
	:inline-always ((t t) t nil t "number_plus(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)+(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)+(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)+(#1)"))
(- (T *) T NIL NIL
	:inline-always ((t) t nil t "number_negate(#0)")
	:inline-always ((t t) t nil t "number_minus(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)-(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)-(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)-(#1)")

	:inline-always ((fixnum-float) long-float nil nil "-(double)(#0)")
	:inline-always ((fixnum-float) short-float nil nil "-(float)(#0)")
	:inline-always ((fixnum) fixnum nil nil "-(#0)")) 
(* (*) T NIL NIL
	:inline-always ((t t) t nil t "number_times(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)*(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)*(float)(#1)")
	:inline-always ((fixnum fixnum) t nil nil "fixnum_times(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)*(#1)"))
(/ (T *) T NIL NIL
	:inline-always ((t t) t nil t "number_divide(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)/(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)/(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)/(#1)"))
(1+ (T) T NIL NIL
	:inline-always ((t) t nil t "one_plus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)+1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)+1")
	:inline-always ((fixnum) fixnum nil nil "(#0)+1"))
(1- (T) T NIL NIL
	:inline-always ((t) t nil t "one_minus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)-1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)-1")
	:inline-always ((fixnum) fixnum nil nil "(#0)-1"))
(CONJUGATE (T) T)
(GCD (*) T)
(LCM (T *) T)

; file num_co.c
(FLOAT (T *) T NIL NIL
;	:inline-always ((T) short-float nil nil "(Lfloat(1,#0),sf(VALUES(0)))")
	:inline-always ((fixnum-float) long-float nil nil "((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil "((float)(#0))"))
(NUMERATOR (T) T)
(DENOMINATOR (T) T)
(FLOOR (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)/(#1):ifloor(#0,#1))"))
(CEILING (T *) (VALUES T T) NIL NIL)
(TRUNCATE (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum-float) fixnum nil nil "(fixnum)(#0)"))
(ROUND (T *) (VALUES T T))
(MOD (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)%(#1):imod(#0,#1))"))
(REM (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)%(#1)"))
(DECODE-FLOAT (T) (VALUES T T T))
(SCALE-FLOAT (T T) T)
(FLOAT-RADIX (T) FIXNUM)
(FLOAT-SIGN (T *) T)
(FLOAT-DIGITS (T) FIXNUM)
(FLOAT-PRECISION (T) FIXNUM)
(INTEGER-DECODE-FLOAT (T) (VALUES T T T))
(COMPLEX (T *) T)
(REALPART (T) T)
(IMAGPART (T) T)
(= (T *) T NIL T
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)==0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)==(#1)"))
(/= (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)!=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)!=(#1)"))
(< (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<(#1)"))
(> (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>(#1)"))
(<= (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<=(#1)"))
(>= (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>=(#1)"))
(MAX (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)>=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)>=(#1)?#0:#1"))
(MIN (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)<=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)<=(#1)?#0:#1"))
(LOGIOR (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) | (#1))"))
(LOGXOR (*) T NIL NIL)
(LOGAND (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) & (#1))"))
(LOGEQV (*) T NIL NIL)
(BOOLE (T T T) T NIL NIL)
(LOGBITP (T T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#1 >> #0) & 1"))
(ASH (T T) T)
(LOGCOUNT (T) T)
(INTEGER-LENGTH (T) FIXNUM)
(si::BIT-ARRAY-OP nil T)
(ZEROP (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)==0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)==0"))
(PLUSP (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)<0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)>0"))
(MINUSP (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)>0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)<0"))
(ODDP (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#0) & 1"))
(EVENP (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "~(#0) & 1"))
(RANDOM (T *) T)
(MAKE-RANDOM-STATE (*) T)
(RANDOM-STATE-P (T) T NIL T)
;(EXP (T) T NIL NIL :inline-always ((number) t nil t "cl_exp(#0)"))
(EXPT (T T) T NIL NIL
;	:inline-always ((t t) t nil t "cl_expt(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil
		(lambda (loc1 loc2)
		  (if (and (consp loc1) (eq (car loc1) 'fixnum)
			   (consp (cadr loc1)) (eq (caadr loc1) 'fixnum-value)
			   (eq (cadr (cadr loc1)) 2))
		      (progn (wt1 "(1<<(") (wt1 loc2) (wt1 "))"))
		    (progn (wt1 "fixnum_expt(") (wt1 loc1) (wt1 #\,) (wt1 loc2)
			   (wt1 #\) ))))))
(LOG (T *) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "log((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)log((double)(#0))"))
(SQRT (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "sqrt((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sqrt((double)(#0))"))
(SIN (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "sin((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sin((double)(#0))"))
(COS (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "cos((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)cos((double)(#0))"))
(tan (number) number nil nil
	:inline-always ((fixnum-float) long-float nil nil "tan((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)tan((double)(#0))"))
(ATAN (T *) T)

; file package.d
(MAKE-PACKAGE (T *) T)
(si::SELECT-PACKAGE (T) T)
(FIND-PACKAGE (T) T)
(PACKAGE-NAME (T) T)
(PACKAGE-NICKNAMES (T) T)
(RENAME-PACKAGE (T T *) T)
(PACKAGE-USE-LIST (T) T)
(PACKAGE-USED-BY-LIST (T) T)
(PACKAGE-SHADOWING-SYMBOLS (T) T)
(LIST-ALL-PACKAGES NIL T)
(INTERN (string *) (VALUES T T))
(FIND-SYMBOL (string *) (VALUES T T))
(UNINTERN (symbol t) T)
(EXPORT (T *) T)
(UNEXPORT (T *) T)
(IMPORT (T *) T)
(SHADOWING-IMPORT (T *) T)
(SHADOW (T *) T)
(USE-PACKAGE (T *) T)
(UNUSE-PACKAGE (T *) T)
(si::PACKAGE-INTERNAL nil T)
(si::PACKAGE-EXTERNAL nil T)
(PATHNAME (T) T)
(PARSE-NAMESTRING (T *) T)
(MERGE-PATHNAMES (T *) T)
(MAKE-PATHNAME (*) T)
(PATHNAMEP (T) T NIL T)
(PATHNAME-HOST (T) T)
(PATHNAME-DEVICE (T) T)
(PATHNAME-DIRECTORY (T) T)
(PATHNAME-NAME (T) T)
(PATHNAME-TYPE (T) T)
(PATHNAME-VERSION (T) T)
(NAMESTRING (T) string NIL NIL)
(FILE-NAMESTRING (T) STRING)
(DIRECTORY-NAMESTRING (T) STRING)
(HOST-NAMESTRING (T) STRING)
(ENOUGH-NAMESTRING (T *) STRING)
(NULL (T) T NIL T
	:inline-always ((t) boolean nil nil "#0==Cnil"))
(SYMBOLP (T) T NIL T
	:inline-always ((t) boolean nil nil "SYMBOLP(#0)"))
(ATOM (T) T NIL T
	:inline-always ((t) boolean nil nil "ATOM(#0)"))
(CONSP (T) T NIL T
	:inline-always ((t) boolean nil nil "CONSP(#0)"))
(LISTP (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;LISTP(#0)"))
(NUMBERP (T) T NIL T
	:inline-always ((t) boolean nil nil "numberp(#0)"))
(INTEGERP (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum"))
(RATIONALP (T) T nil t)
(FLOATP (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_shortfloat||type_of(#0)==t_longfloat"))
(COMPLEXP (T) T NIL T)
(CHARACTERP (T) T NIL T
	:inline-always ((t) boolean nil nil "CHARACTERP(#0)"))
(STRINGP (T) T NIL T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_string"))
(BIT-VECTOR-P (T) T NIL T
	:inline-always ((t) boolean nil nil "(type_of(#0)==t_bitvector)"))
(VECTORP (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_vector||
type_of(#0)==t_string||
type_of(#0)==t_bitvector"))
(SIMPLE-STRING-P (T) T NIL T)
(SIMPLE-BIT-VECTOR-P (T) T NIL T)
(SIMPLE-VECTOR-P (T) T NIL T)
(ARRAYP (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;ARRAYP(#0)"))
(PACKAGEP (T) T NIL T)
(FUNCTIONP (T) T NIL T)
(COMPILED-FUNCTION-P (T) T NIL T)
(EQ (T T) T NIL T
	:inline-always ((t t) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQL (T T) T NIL T
	:inline-always ((t t) boolean nil nil "eql(#0,#1)")
	:inline-always ((character t) boolean nil nil	; Beppe
		"(CHARACTERP(#1) && (#0)==CHAR_CODE(#1))")
	:inline-always ((t character) boolean nil nil	; Beppe
		"(CHARACTERP(#0) && CHAR_CODE(#0)==(#1))")
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUAL (T T) T nil t
	:inline-always ((t t) boolean nil nil "equal(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUALP (T T) T NIL T
	:inline-always ((t t) boolean nil nil "equalp(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(NOT (T) T NIL T
	:inline-always ((t) boolean nil nil "(#0)==Cnil"))

; file print.d
(CLEAR-OUTPUT (*) T)
(FINISH-OUTPUT (*) T)
(FORCE-OUTPUT (*) T)
(FRESH-LINE (*) T)
(LISTEN (*) T)
(PEEK-CHAR (*) T)
(PPRINT (T *) T)
(PRIN1 (T *) T NIL NIL
	:inline-always ((t t) t t nil "prin1(#0,#1)")
	:inline-always ((t) t t nil "prin1(#0,Cnil)"))
(PRINC (T *) T NIL NIL
	:inline-always ((t t) t t nil "princ(#0,#1)")
	:inline-always ((t) t t nil "princ(#0,Cnil)"))
(PRINT (T *) T NIL NIL
	:inline-always ((t t) t t nil "print(#0,#1)")
	:inline-always ((t) t t nil "print(#0,Cnil)"))
(PROBE-FILE (T) T NIL T
	:inline-always ((t) boolean nil nil "(file_exists(#0))"))
(UNREAD-CHAR (T *) T)
(READ (*) T)
(READ-CHAR (*) T)
(READ-DELIMITED-LIST (T *) T)
(READ-LINE (*) (VALUES T T))
(READ-PRESERVING-WHITESPACE nil T)
(TERPRI (*) T NIL T
	:inline-always ((t) t t nil "terpri(#0)")
	:inline-always (nil t t nil "terpri(Cnil)"))
(WRITE (T *) T)
(WRITE-BYTE (fixnum stream) T)
(WRITE-CHAR (T *) T NIL NIL
	:inline-always ((t) t t nil "@0;(princ_char(char_code(#0),Cnil),(#0))"))
(WRITE-LINE (T *) T)
(WRITE-STRING (T *) T)
(READ-CHAR-NO-HANG (*) T)
(CLEAR-INPUT (*) T)
(PARSE-INTEGER (T *))
(READ-BYTE (T *) T)
(COPY-READTABLE (*) T NIL NIL
	:inline-always ((null null) t nil nil "standard_readtable"))
(READTABLEP (T) T NIL T)
(SET-SYNTAX-FROM-CHAR (T T *) T)
(SET-MACRO-CHARACTER (T T *) T)
(GET-MACRO-CHARACTER (T *) T)
(MAKE-DISPATCH-MACRO-CHARACTER nil T)
(SET-DISPATCH-MACRO-CHARACTER nil T)
(GET-DISPATCH-MACRO-CHARACTER nil T)
(SI::STRING-TO-OBJECT (T) T)
(si::STANDARD-READTABLE (T) T)
(SYMBOL-FUNCTION (T) T NIL NIL)
(FBOUNDP (symbol) T nil t)
(SYMBOL-VALUE (symbol) T)
(BOUNDP (symbol) T nil t
	:inline-unsafe ((t) boolean nil nil "(#0)->symbol.dbind!=OBJNULL"))
(MACRO-FUNCTION (symbol) T)
(SPECIAL-OPERATOR-P (symbol) T nil t)

; file unixsave.c
(SAVE (T) T)

; file unixsys.c
(si::SYSTEM nil T)

; file sequence.d
(ELT (sequence fixnum) T nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil t "elt(#0,fix(#1))")
	:inline-unsafe ((t fixnum) t nil t "elt(#0,#1)"))
(si::ELT-SET (sequence fixnum t) T nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "elt_set(#0,fix(#1),#2)"))
(SUBSEQ (sequence fixnum *) sequence)
(COPY-SEQ (sequence) sequence)
(LENGTH (sequence) fixnum t nil
	:inline-always ((t) fixnum nil nil "length(#0)")
	:inline-unsafe (((array t)) fixnum nil nil "(#0)->vector.fillp")
	:inline-unsafe ((string) fixnum nil nil "(#0)->string.fillp"))
(REVERSE (sequence) sequence nil nil)
(NREVERSE (sequence) sequence nil nil)

; file character.d
(CHAR (string fixnum) character nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil nil "CODE_CHAR((#0)->string.self[fix(#1)])")
	:inline-unsafe ((t fixnum) fixnum nil nil "(#0)->string.self[#1]")
	:inline-unsafe ((t fixnum) character nil nil "(#0)->string.self[#1]"))
(si::CHAR-SET
	(string fixnum character) character nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil
		"@2;((#0)->string.self[fix(#1)]=char_code(#2),(#2))")
	:inline-unsafe ((t fixnum character) character t nil
		"(#0)->string.self[#1]= #2"))
(SCHAR (string fixnum) character nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil nil "CODE_CHAR((#0)->string.self[fix(#1)])")
	:inline-unsafe ((t t) fixnum nil nil "(#0)->string.self[fix(#1)]")
	:inline-unsafe ((t fixnum) fixnum nil nil "(#0)->string.self[#1]")
	:inline-unsafe ((t fixnum) character nil nil "(#0)->string.self[#1]"))
(si::SCHAR-SET
	(string fixnum character) character nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil
		"@2;((#0)->string.self[fix(#1)]=char_code(#2),(#2))")
	:inline-unsafe ((t fixnum character) character t nil
		"(#0)->string.self[#1]= #2"))
(STRING= (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_eq(#0,#1)"))
(STRING-EQUAL (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_equal(#0,#1)"))
(STRING< (string string *) T nil t)
(STRING> (string string *) T nil t)
(STRING<= (string string *) T nil t)
(STRING>= (string string *) T nil t)
(STRING/= (string string *) T nil t)
(STRING-LESSP (string string *) T nil t)
(STRING-GREATERP (string string *) T nil t)
(STRING-NOT-LESSP (string string *) T nil t)
(STRING-NOT-GREATERP (string string *) T nil t)
(STRING-NOT-EQUAL (string string *) T nil t)
(MAKE-STRING (fixnum *) string)
(STRING-TRIM (t string) string)
(STRING-LEFT-TRIM (t string) string)
(STRING-RIGHT-TRIM (t string) string)
(STRING-UPCASE (string *) string)
(STRING-DOWNCASE (string *) string)
(STRING-CAPITALIZE (string *) string)
(NSTRING-UPCASE (string *) string)
(NSTRING-DOWNCASE (string *) string)
(NSTRING-CAPITALIZE (string *) string)
(STRING (T) string nil t)
(STRING-CONCATENATE (T) string nil nil)

; file structure.d
(si::MAKE-STRUCTURE (T *) T)
(COPY-STRUCTURE (T) T)
(SI::STRUCTURE-NAME (T) SYMBOL NIL NIL
	:inline-always ((structure) symbol nil nil "SNAME(#0)"))
(si::STRUCTURE-REF (t t fixnum) T nil nil
	:inline-always ((t t fixnum) t nil nil "structure_ref(#0,#1,#2)"))
(si::STRUCTURE-SET (t t fixnum t) T nil nil
	:inline-always ((t t fixnum t) t T nil "structure_set(#0,#1,#2,#3)"))
(SI::STRUCTUREP (T) T NIL T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_structure"))
(SI::STRUCTURE-SUBTYPE-P (T T) T NIL T)
(si::RPLACA-NTHCDR (T T T) nil T nil t)
(si::LIST-NTH (T T) T nil t)

; file toplevel.c
(si::*MAKE-SPECIAL nil T)
(si::*MAKE-CONSTANT nil T)

; file symbol.d
(GET (symbol t *) T nil nil
	:inline-always ((t t t) t nil nil "ecl_get(#0,#1,#2)")
	:inline-always ((t t) t nil nil "ecl_get(#0,#1,Cnil)"))
(REMPROP (symbol t) T nil nil)
(SYMBOL-PLIST (symbol) T nil T
	:inline-always ((t) t nil nil "((#0)->symbol.plist)"))
(GETF (T T *) T)
(GET-PROPERTIES (T T) *)
(SYMBOL-NAME (symbol) string nil nil
	:inline-always ((symbol) t nil t "((#0)->symbol.name)")
;	:inline-always ((t) t nil t "cl_symbol_name(#0)")
)
(MAKE-SYMBOL (string) symbol)
(COPY-SYMBOL (symbol *) symbol)
(GENSYM (*) symbol)
(GENTEMP (*) symbol)
(SYMBOL-PACKAGE (symbol) T)
(KEYWORDP (T) T NIL T
;  :inline-always ((t) boolean nil nil
;        "@0;(type_of(#0)==t_symbol&&(#0)->symbol.hpack==keyword_package)")
 )
(SI::PUT-F NIL (T T))
(SI::REM-F NIL (T T))
(si::SET-SYMBOL-PLIST (symbol t) T)
(SI::PUTPROP (T T T) T NIL NIL)
(SI::PUT-SYSPROP (T T T) T NIL NIL)
(SI::GET-SYSPROP (T T T) T NIL NIL)
(SI::REM-SYSPROP (T T) T NIL NIL)

; file tcp.c
(si::OPEN-TCP-STREAM (T T) T)

; file unixfasl.c
(si::READ-EXTERNALS nil T)
(si::SET-UP-COMBINED nil T)
(si::BUILD-SYMBOL-TABLE nil T)

; file unixtime.c
(si::DAYLIGHT-SAVING-TIME-P nil T nil t)
(GET-UNIVERSAL-TIME nil T)
(GET-INTERNAL-RUN-TIME nil T)
(GET-INTERNAL-REAL-TIME nil T)
(si::GET-LOCAL-TIME-ZONE nil T)
(SLEEP (real) T)

(TYPE-OF (T) T NIL NIL)

;;; Beppe's additions
(READ-BYTES (stream vector fixnum fixnum) T)
(WRITE-BYTES (stream vector fixnum fixnum) T)

;;; AKCL additions:
(SI::COPY-STREAM (T T) T)

;; file numlib.lsp:
(LOGNOT nil nil nil NIL NIL
	:inline-always ((fixnum) fixnum nil nil "(~(#0))"))

;;; file cmpfun.lsp:
;;; The following functions are introduced by the compiler in pass 1 

(shift>> nil nil nil NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) >> (- (#1)))"))
(shift<< nil nil nil NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) << (#1))"))
(short-float-p nil nil nil T T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_shortfloat"))
(long-float-p nil nil nil T T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_longfloat"))
(si:fixnump nil nil nil T T
	:inline-always ((t) boolean nil nil "FIXNUMP(#0)")
	:inline-always ((fixnum) boolean nil nil "1"))
(si::put-properties (*) nil T)
)) ; end of inlines

#+clos
(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
; file instance.c
(si::ALLOCATE-RAW-INSTANCE (t fixnum) T)
(si::INSTANCE-REF (t fixnum) T nil nil
	:inline-always ((standard-object fixnum) t nil nil
		"(#0)->instance.slots[#1]"))
(si::INSTANCE-REF-SAFE (t fixnum) T nil nil
	:inline-unsafe ((standard-object fixnum) t nil nil
		"(#0)->instance.slots[#1]"))
(si::INSTANCE-SET (t fixnum t) T nil nil
	:inline-always ((standard-object fixnum t) t t nil
		"(#0)->instance.slots[#1]=(#2)"))
(si::INSTANCE-CLASS (t) T nil nil
	:inline-always ((standard-object) t nil nil "CLASS_OF(#0)"))
(si::INSTANCE-CLASS-SET (t t) T)
(si::INSTANCEP (t) T nil t)
(si::UNBOUND nil T nil t
	:inline-always (nil T nil nil "OBJNULL"))
(si::SL-BOUNDP (t) T nil t
	:inline-always ((t) boolean nil nil "(#0)!=OBJNULL"))
(si::SL-MAKUNBOUND (t fixnum) T nil t)

; file gfun.c
(si::ALLOCATE-GFUN nil T)
(si::GFUN-NAME  nil T)
(si::GFUN-NAME-SET nil T)
(si::GFUN-METHOD-HT nil T)
(si::GFUN-METHOD-HT-SET nil T)
(si::GFUN-SPEC-HOW-REF  nil T)
(si::GFUN-SPEC-HOW-SET nil T)
(si::GFUN-INSTANCE  nil T)
(si::GFUN-INSTANCE-SET nil T)
(si::GFUNP nil T)
)) ; end of of #+clos

(in-package "SI")
(proclaim '(si::c-export-fname
	    make-array vector array-dimensions
	    array-in-bounds-p array-row-major-index bit sbit bit-and bit-ior
	    bit-xor bit-eqv bit-nand bit-nor bit-andc1 bit-andc2 bit-orc1 bit-not
	    vector-push vector-push-extend vector-pop adjust-array ecase-error
	    ccase-error typecase-error-string find-documentation find-declarations
	    si::check-keyword si::check-arg-length si::dm-too-few-arguments si::dm-bad-key
	    remove-documentation si::get-documentation
	    si::set-documentation si::expand-set-documentation read-from-string
	    write-to-string prin1-to-string princ-to-string union nunion
	    intersection nintersection set-difference nset-difference
	    set-exclusive-or nset-exclusive-or subsetp
	    logical-pathname-translations decode-universal-time
	    encode-universal-time get-decoded-time isqrt abs phase signum cis asin
	    acos asinh acosh atanh rational ffloor fceiling ftruncate fround
	    logtest byte byte-size byte-position ldb ldb-test mask-field dpb
	    deposit-field upgraded-array-element-type typep subtypep coerce make-sequence
	    concatenate map some every notany notevery map-into reduce fill
	    replace remove remove-if remove-if-not delete delete-if delete-if-not
	    count count-if count-if-not substitute substitute-if substitute-if-not
	    nsubstitute nsubstitute-if nsubstitute-if-not find find-if find-if-not
	    position position-if position-if-not remove-duplicates
	    delete-duplicates mismatch search sort stable-sort merge))
