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

(defun defsysfun (fname cname-string &optional
		  arg-types return-type
		  never-change-special-var-p predicate
		  &rest optimizers)
  ;; The value NIL for each parameter except for fname means "not known".
  ;; optimizers is a list of alternating {safety inline-info}* as above.
  (when cname-string (setf (get fname 'Lfun) cname-string))
  (when arg-types
    (setf (get fname 'arg-types)
	  (mapcar #'(lambda (x) (if (eql x '*) '* (type-filter x)))
		  arg-types)))
  (when (and return-type (not (eq 'T return-type)))
    (setf (get fname 'return-type) (type-filter return-type)))
  (when never-change-special-var-p (setf (get fname 'no-sp-change) t))
  (when predicate (setf (get fname 'predicate) t))
  (remprop fname ':inline-always)
  (remprop fname ':inline-safe)
  (remprop fname ':inline-unsafe)
  (do ((scan optimizers (cddr scan))
       (safety) (inline-info))
      ((null scan))
      (setq safety (first scan)
	    inline-info (second scan))
      (push inline-info (get fname safety)))
  )

; file alloc.c
#-boehm-gc
(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
(si::ALLOC "siLalloc")
(si::NPAGE "siLnpage")
(si::MAXPAGE "siLmaxpage")
(si::ALLOC-CONTPAGE "siLalloc_contpage")
(si::NCBPAGE "siLncbpage")
(si::MAXCBPAGE "siLmaxcbpage")
(si::ALLOC-RELPAGE "siLalloc_relpage")
(si::NRBPAGE "siLnrbpage")
(si::GET-HOLE-SIZE "siLget_hole_size")
(si::SET-HOLE-SIZE "siLset_hole_size")))

(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
(si::RPLACA-NTHCDR "siLrplaca_nthcdr" nil T)
(si::LIST-NTH "siLlist_nth" nil T)
(si::MAKE-PURE-ARRAY "siLmake_pure_array" nil array)
(si::MAKE-VECTOR "siLmake_vector" nil vector)
;(si::MAKE-BITVECTOR "siLmake_bitvector" nil bit-vector nil nil)
(AREF "Laref" (array *) T NIL NIL
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
	:inline-unsafe (((array t) t) t nil nil "(#0)->vector.self.t[fix(#1)]")
	:inline-unsafe (((array bit) t) fixnum nil nil "aref_bv(#0,fix(#1))")
	:inline-unsafe (((array t) fixnum) t nil nil "(#0)->vector.self.t[#1]")
	:inline-unsafe (((array bit) fixnum) fixnum nil nil "aref_bv(#0,#1)")
	:inline-unsafe (((array base-char) fixnum) fixnum nil nil
		"(#0)->string.self[#1]")
	:inline-unsafe (((array base-char) fixnum) character nil nil
		"(#0)->string.self[#1]")
	:inline-unsafe (((array long-float) fixnum) long-float nil nil
		"(#0)->array.self.lf[#1]")
	:inline-unsafe (((array short-float) fixnum) short-float nil nil
		"(#0)->array.self.sf[#1]")
	:inline-unsafe (((array fixnum) fixnum) fixnum nil nil
		"(#0)->array.self.fix[#1]"))
(SI::ASET "siLaset" (T ARRAY *) NIL NIL NIL
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
(ARRAY-ELEMENT-TYPE "Larray_element_type" (array) T)
(ARRAY-RANK "Larray_rank" (array) fixnum)
(ARRAY-DIMENSION "Larray_dimension" (array fixnum) fixnum)
(ARRAY-TOTAL-SIZE "Larray_total_size" (array) T nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.dim)"))
(ADJUSTABLE-ARRAY-P "Ladjustable_array_p" (array) T nil t)
(si::DISPLACED-ARRAY-P "siLdisplaced_array_p" (array) T nil t)
(SVREF "Lsvref" (simple-vector fixnum) T nil nil
	:inline-always ((t t) t nil t "aref1(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "aref1(#0,#1)")
	:inline-unsafe ((t t) t nil nil "(#0)->vector.self.t[fix(#1)]")
	:inline-unsafe ((t fixnum) t nil nil "(#0)->vector.self.t[#1]"))
(si::SVSET "siLsvset" (simple-vector fixnum t) T nil nil
	:inline-always ((t t t) t t nil "aset1(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "aset1(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "((#0)->vector.self.t[fix(#1)]=(#2))")
	:inline-unsafe ((t fixnum t) t t nil "(#0)->vector.self.t[#1]= #2"))
(ARRAY-HAS-FILL-POINTER-P "Larray_has_fill_pointer_p" nil T nil t)
(FILL-POINTER "Lfill_pointer" (vector) fixnum nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.fillp)"))
(si::FILL-POINTER-SET "siLfill_pointer_set"
	(vector fixnum) fixnum nil nil
	:inline-unsafe ((t fixnum) fixnum t nil "((#0)->string.fillp)=(#1)"))
(si::REPLACE-ARRAY "siLreplace_array" nil T nil nil)
;(si::ASET-BY-CURSOR "siLaset_by_cursor" nil T nil nil)

; file assignment.c
(SET "Lset" (symbol t) T)
(si::FSET "siLfset" (symbol t) T)
(MAKUNBOUND "Lmakunbound" (symbol) T)
(FMAKUNBOUND "Lfmakunbound" (symbol) T)
(si::CLEAR-COMPILER-PROPERTIES "siLclear_compiler_properties" nil T)

; file catch.c
;#-clcs (SI::ERROR-SET "siLerror_set" (T) * NIL NIL)

; file cfun.c
(si::COMPILED-FUNCTION-NAME "siLcompiled_function_name" nil T)

; file character.c
(STANDARD-CHAR-P "Lstandard_char_p" (character) T nil t)
(GRAPHIC-CHAR-P "Lgraphic_char_p" (character) T nil t)
(ALPHA-CHAR-P "Lalpha_char_p" (character) T nil t)
(UPPER-CASE-P "Lupper_case_p" (character) T nil t)
(LOWER-CASE-P "Llower_case_p" (character) T nil t)
(BOTH-CASE-P "Lboth_case_p" (character) T nil t)
(DIGIT-CHAR-P "Ldigit_char_p" (character *) T nil nil
	:inline-always
	((character) boolean nil nil "@0; ((#0) <= '9' && (#0) >= '0')"))
(ALPHANUMERICP "Lalphanumericp" (character) T nil t)
(CHARACTER "Lcharacter" (T) CHARACTER)
(CHAR= "Lchar_eq" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)==char_code(#1)"))
(CHAR/= "Lchar_neq" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)!=(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)!=char_code(#1)"))
(CHAR< "Lchar_l" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<(#1)"))
(CHAR> "Lchar_g" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>(#1)"))
(CHAR<= "Lchar_le" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<=(#1)"))
(CHAR>= "Lchar_ge" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>=(#1)"))
(CHAR-EQUAL "Lchar_equal" (character *) T nil t)
(CHAR-NOT-EQUAL "Lchar_not_equal" (character *) T nil t)
(CHAR-LESSP "Lchar_lessp" (character *) T nil t)
(CHAR-GREATERP "Lchar_greaterp" (character *) T nil t)
(CHAR-NOT-GREATERP "Lchar_not_greaterp" (character *) T nil t)
(CHAR-NOT-LESSP "Lchar_not_lessp" (character *) T nil t)
(CHARACTER "Lcharacter" nil character nil nil)
(CHAR-CODE "Lchar_code" (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(CODE-CHAR "Lcode_char" (fixnum) character nil nil
	:inline-always ((fixnum) character nil nil "#0"))
(CHAR-UPCASE "Lchar_upcase" (character) character nil nil)
(CHAR-DOWNCASE "Lchar_downcase" (character) character nil nil)
(DIGIT-CHAR "Ldigit_char" (fixnum *) character nil nil)
(CHAR-INT "Lchar_int" (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(INT-CHAR "Lint_char" (fixnum) character nil nil
	:inline-always ((fixnum) character nil nil "#0"))
(CHAR-NAME "Lchar_name" (character) symbol)
(NAME-CHAR "Lname_char" (symbol) character)

; ; file error.c
#-clcs
(ERROR "Lerror" (T *) T nil nil)
#-clcs
(CERROR "Lcerror" (T T *) T nil nil)

(si::IHS-TOP "siLihs_top")
(si::IHS-FUN "siLihs_fun")
(si::IHS-ENV "siLihs_env")
(si::FRS-TOP "siLfrs_top")
(si::FRS-VS "siLfrs_vs")
(si::FRS-BDS "siLfrs_bds")
(si::FRS-CLASS "siLfrs_class")
(si::FRS-TAG "siLfrs_tag")
(si::FRS-IHS "siLfrs_ihs")
(si::BDS-TOP "siLbds_top")
(si::BDS-VAR "siLbds_var")
(si::BDS-VAL "siLbds_val")
(si::VS-TOP "siLvs_top")
(si::VS "siLvs")
(si::SCH-FRS-BASE "siLsch_frs_base")
(si::UNIVERSAL-ERROR-HANDLER "siLuniversal_error_handler")

; file eval.c
(APPLY "Lapply" (T T *) T)
(FUNCALL "Lfuncall" (T *) T)
(EVAL "Leval" (T) T)
(EVALHOOK "Levalhook" (T T T *) T)
(APPLYHOOK "Lapplyhook" (T T T T *) T)
(CONSTANTP "Lconstantp" (T) T NIL T)
(si::UNLINK-SYMBOL "siLunlink_symbol" nil T)
(si::LINK-ENABLE "siLlink_enable" nil T)

; file file.d
(MAKE-SYNONYM-STREAM "Lmake_synonym_stream" (T) T)
(MAKE-BROADCAST-STREAM "Lmake_broadcast_stream" (*) T)
(MAKE-CONCATENATED-STREAM "Lmake_concatenated_stream" nil T)
(MAKE-TWO-WAY-STREAM "Lmake_two_way_stream" (T T) T)
(MAKE-ECHO-STREAM "Lmake_echo_stream" (T T) T)
(MAKE-STRING-INPUT-STREAM "Lmake_string_input_stream" nil T)
(MAKE-STRING-OUTPUT-STREAM "Lmake_string_output_stream" nil T)
(GET-OUTPUT-STREAM-STRING "Lget_output_stream_string" nil T)
(SI::OUTPUT-STREAM-STRING "siLoutput_stream_string" (T) T)
(STREAMP "Lstreamp" (T) T NIL T)
(INPUT-STREAM-P "Linput_stream_p" (T) T NIL T)
(OUTPUT-STREAM-P "Loutput_stream_p" (T) T NIL T)
(STREAM-ELEMENT-TYPE "Lstream_element_type" (T) T)
(CLOSE "Lclose" (T *) T)
;#-clcs (OPEN "Lopen" (T *) T)
(FILE-POSITION "Lfile_position" (T *) T)
(FILE-LENGTH "Lfile_length" (T) T)
;#-clcs (LOAD "Lload" (T *) T)
(si::GET-STRING-INPUT-STREAM-INDEX "siLget_string_input_stream_index" nil T)
(si::MAKE-STRING-OUTPUT-STREAM-FROM-STRING
	"siLmake_string_output_stream_from_string" nil T)

; file gbc.c
(si::ROOM-REPORT "siLroom_report" nil T)
(si::RESET-GBC-COUNT "siLreset_gbc_count" nil T)
(GBC "Lgbc" nil T)

; file unixfsys.c
(TRUENAME "Ltruename" (T) T)
(RENAME-FILE "Lrename_file" (T T) T)
(SI::SPECIALP "siLspecialp" (T) T NIL T)
(DELETE-FILE "Ldelete_file" (T) T)
(PROBE-FILE "Lprobe_file" (T) T)
(FILE-WRITE-DATE "Lfile_write_date" (T) T)
(FILE-AUTHOR "Lfile_author" (T) T)
(PATHNAME "Lpathname" (T) T)
(USER-HOMEDIR-PATHNAME "Luser_homedir_pathname" (*) T)
(DIRECTORY "Ldirectory" (T) T)
(si::CHDIR "siLchdir" nil T)

; file unixint.c
(si::CATCH-BAD-SIGNALS "siLcatch_bad_signals" nil T)
(si::UNCATCH-BAD-SIGNALS "siLuncatch_bad_signals" nil T)

; file format.c
(FORMAT "Lformat" (T string *) T)

; file hash.d
(MAKE-HASH-TABLE "Lmake_hash_table" (*) T)
(HASH-TABLE-P "Lhash_table_p" (T) T NIL T)
(VALUES "Lvalues" (*) *)
(GETHASH "Lgethash" (T T *) (VALUES T T))
(REMHASH "Lremhash" (T T) T)
(MAPHASH "Lmaphash" (T T) T)
(CLRHASH "Lclrhash" (T) T)
(HASH-TABLE-COUNT "Lhash_table_count" (T) T)
(SXHASH "Lsxhash" (T) FIXNUM)
(SI::HASH-SET "siLhash_set" NIL T)

; file list.d
(CAR "Lcar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "car(#0)")
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(CDR "Lcdr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdr(#0)")
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(CAAR "Lcaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caar(#0)")
	:inline-unsafe ((t) t nil nil "CAAR(#0)"))
(CADR "Lcadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadr(#0)")
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(CDAR "Lcdar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdar(#0)")
	:inline-unsafe ((t) t nil nil "CDAR(#0)"))
(CDDR "Lcddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDR(#0)"))
(CAAAR "Lcaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaar(#0)")
	:inline-unsafe ((t) t nil nil "CAAAR(#0)"))
(CAADR "Lcaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caadr(#0)")
	:inline-unsafe ((t) t nil nil "CAADR(#0)"))
(CADAR "Lcadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadar(#0)")
	:inline-unsafe ((t) t nil nil "CADAR(#0)"))
(CADDR "Lcaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(CDAAR "Lcdaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaar(#0)")
	:inline-unsafe ((t) t nil nil "CDAAR(#0)"))
(CDADR "Lcdadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdadr(#0)")
	:inline-unsafe ((t) t nil nil "CDADR(#0)"))
(CDDAR "Lcddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddar(#0)")
	:inline-unsafe ((t) t nil nil "CDDAR(#0)"))
(CDDDR "Lcdddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDDR(#0)"))
(CAAAAR "Lcaaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaaar(#0)")
	:inline-unsafe ((t) t nil nil "CAAAAR(#0)"))
(CAAADR "Lcaaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaadr(#0)")
	:inline-unsafe ((t) t nil nil "CAAADR(#0)"))
(CAADAR "Lcaadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caadar(#0)")
	:inline-unsafe ((t) t nil nil "CAADAR(#0)"))
(CAADDR "Lcaaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaddr(#0)")
	:inline-unsafe ((t) t nil nil "CAADDR(#0)"))
(CADAAR "Lcadaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadaar(#0)")
	:inline-unsafe ((t) t nil nil "CADAAR(#0)"))
(CADADR "Lcadadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadadr(#0)")
	:inline-unsafe ((t) t nil nil "CADADR(#0)"))
(CADDAR "Lcaddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caddar(#0)")
	:inline-unsafe ((t) t nil nil "CADDAR(#0)"))
(CADDDR "Lcadddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(CDAAAR "Lcdaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaaar(#0)")
	:inline-unsafe ((t) t nil nil "CDAAAR(#0)"))
(CDAADR "Lcdaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaadr(#0)")
	:inline-unsafe ((t) t nil nil "CDAADR(#0)"))
(CDADAR "Lcdadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdadar(#0)")
	:inline-unsafe ((t) t nil nil "CDADAR(#0)"))
(CDADDR "Lcdaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaddr(#0)")
	:inline-unsafe ((t) t nil nil "CDADDR(#0)"))
(CDDAAR "Lcddaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddaar(#0)")
	:inline-unsafe ((t) t nil nil "CDDAAR(#0)"))
(CDDADR "Lcddadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddadr(#0)")
	:inline-unsafe ((t) t nil nil "CDDADR(#0)"))
(CDDDAR "Lcdddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdddar(#0)")
	:inline-unsafe ((t) t nil nil "CDDDAR(#0)"))
(CDDDDR "Lcddddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDDDR(#0)"))
(CONS "Lcons" (T T) T NIL NIL
	:inline-always ((t t) t nil t "CONS(#0,#1)"))
(TREE-EQUAL "Ltree_equal" (T T *) T NIL T)
(ENDP "Lendp" (T) T NIL T
	:inline-safe ((t) boolean nil nil "endp(#0)")
	:inline-unsafe ((t) boolean nil nil "#0==Cnil"))
(LIST-LENGTH "Llist_length" (T) T NIL NIL)
(NTH "Lnth" (T T) T NIL NIL
	:inline-always ((t t) t nil nil "nth(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nth(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nth(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nth(#0,#1)"))
(FIRST "Lcar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "car(#0)")
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(SECOND "Lcadr" (T) T nil nil
	:inline-safe ((t) t nil nil "cadr(#0)")
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(THIRD "Lcaddr" (T) T nil nil
	:inline-safe ((t) t nil nil "caddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(FOURTH "Lcadddr" (T) T nil nil
	:inline-safe ((t) t nil nil "cadddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(FIFTH "Lfifth" (T) T)
(SIXTH "Lsixth" (T) T)
(SEVENTH "Lseventh" (T) T)
(EIGHTH "Leighth" (T) T)
(NINTH "Lninth" (T) T)
(TENTH "Ltenth" (T) T)
(REST "Lcdr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdr(#0)")
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(NTHCDR "Lnthcdr" (fixnum t) T nil nil
	:inline-always ((t t) t nil nil "nthcdr(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nthcdr(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nthcdr(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nthcdr(#0,#1)"))
(LAST "Llast" (T) T)
(LIST "Llist" (*) T NIL NIL
	:inline-always (nil t nil nil "Cnil")
	:inline-always ((t) t nil t "CONS(#0,Cnil)")
	:inline-always ((t t) t nil t list-inline)
	:inline-always ((t t t) t nil t list-inline)
	:inline-always ((t t t t) t nil t list-inline)
	:inline-always ((t t t t t) t nil t list-inline)
	:inline-always ((t t t t t t) t nil t list-inline)
	:inline-always ((t t t t t t t) t nil t list-inline)
	:inline-always ((t t t t t t t t) t nil t list-inline)
	:inline-always ((t t t t t t t t t) t nil t list-inline)
	:inline-always ((t t t t t t t t t t) t nil t list-inline))
(LIST* "LlistA" (T *) T NIL NIL
	:inline-always ((t) t nil nil "#0")
	:inline-always ((t t) t nil t "CONS(#0,#1)")
	:inline-always ((t t t) t nil t list*-inline)
	:inline-always ((t t t t) t nil t list*-inline)
	:inline-always ((t t t t t) t nil t list*-inline)
	:inline-always ((t t t t t t) t nil t list*-inline)
	:inline-always ((t t t t t t t) t nil t list*-inline)
	:inline-always ((t t t t t t t t) t nil t list*-inline)
	:inline-always ((t t t t t t t t t) t nil t list*-inline)
	:inline-always ((t t t t t t t t t t) t nil t list*-inline))
(MAKE-LIST "Lmake_list" (fixnum *) T)
(APPEND "Lappend" (*) T NIL NIL
	:inline-always ((t t) t nil t "append(#0,#1)"))
(COPY-LIST "Lcopy_list" (T) T)
(COPY-ALIST "Lcopy_alist" (T) T)
(COPY-TREE "Lcopy_tree" (T) T)
(REVAPPEND "Lrevappend" (T T) T)
(NCONC "Lnconc" (*) T NIL NIL
	:inline-always ((t t) t t nil "nconc(#0,#1)"))
(NRECONC "Lreconc" (T T) T)
(BUTLAST "Lbutlast" (T *) T)
(NBUTLAST "Lnbutlast" (T *) T)
(LDIFF "Lldiff" (T T) T)
(RPLACA "Lrplaca" (cons T) T)
(RPLACD "Lrplacd" (cons T) T)
(SUBST "Lsubst" (T T T *) T)
(SUBST-IF "Lsubst_if" (T T T *) T)
(SUBST-IF-NOT "Lsubst_if_not" (T T T *) T)
(NSUBST "Lnsubst" (T T T *) T)
(NSUBST-IF "Lnsubst_if" (T T T *) T)
(NSUBST-IF-NOT "Lnsubst_if_not" (T T T *) T)
(SUBLIS "Lsublis" (T T *) T)
(NSUBLIS "Lnsublis" (T T *) T)
(MEMBER "Lmember" (T T *) T)
(MEMBER-IF "Lmember_if" (T T *) T)
(MEMBER-IF-NOT "Lmember_if_not" (T T *) T)
(MEMBER1 "Lmember1"(T T *) T)
(TAILP "Ltailp" (T T) T NIL T)
(ADJOIN "Ladjoin" (T T *) T)
(ACONS "Lacons" (T T T) T)
(PAIRLIS "Lpairlis" (T T *) T)
(ASSOC "Lassoc" (T T *) T)
(ASSOC-IF "Lassoc_if" (T T) T)
(ASSOC-IF-NOT "Lassoc_if_not" (T T) T)
(RASSOC "Lrassoc" (T T *) T)
(RASSOC-IF "Lrassoc_if" (T T) T)
(RASSOC-IF-NOT "Lrassoc_if_not" (T T) T)
(si::MEMQ "siLmemq" (T T T) T)

; file lwp.c
;to do

; file macros.c
(si::DEFINE-MACRO "siLdefine_macro" nil T)
(MACROEXPAND "Lmacroexpand" (T *) (VALUES T T))
(MACROEXPAND-1 "Lmacroexpand_1" (T *) (VALUES T T))

; file main.c
(QUIT "Lquit" nil T)
(IDENTITY "Lidentity" (T) T)
(si::ARGC "siLargc" nil T)
(si::ARGV "siLargv" nil T)
(si::GETENV "siLgetenv" nil T)
(si::RESET-STACK-LIMITS "siLreset_stack_limits" nil T)
#-sparc
(si::INIT-SYSTEM "siLinit_system" nil T)
(si::POINTER "siLaddress" nil T)
(si::NANI "siLnani" nil T)

; file mapfun.c
(MAPCAR "Lmapcar" (T T *) T)
(MAPLIST "Lmaplist" (T T *) T)
(MAPC "Lmapc" (T T *) T)
(MAPL "Lmapl" (T T *) T)
(MAPCAN "Lmapcan" (T T *) T)
(MAPCON "Lmapcon" (T T *) T)

; file multival.c
(VALUES "Lvalues" nil T)
(VALUES-LIST "Lvalues_list" (T) *)

; file num_arith.c
(+ "Lplus" (*) T NIL NIL
	:inline-always ((t t) t nil t "number_plus(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)+(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)+(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)+(#1)"))
(- "Lminus" (T *) T NIL NIL
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
(* "Ltimes" (*) T NIL NIL
	:inline-always ((t t) t nil t "number_times(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)*(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)*(float)(#1)")
	:inline-always ((fixnum fixnum) t nil nil "fixnum_times(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)*(#1)"))
(/ "Ldivide" (T *) T NIL NIL
	:inline-always ((t t) t nil t "number_divide(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)/(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)/(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)/(#1)"))
(1+ "Lone_plus" (T) T NIL NIL
	:inline-always ((t) t nil t "one_plus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)+1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)+1")
	:inline-always ((fixnum) fixnum nil nil "(#0)+1"))
(1- "Lone_minus" (T) T NIL NIL
	:inline-always ((t) t nil t "one_minus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)-1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)-1")
	:inline-always ((fixnum) fixnum nil nil "(#0)-1"))
(CONJUGATE "Lconjugate" (T) T)
(GCD "Lgcd" (*) T)
(LCM "Llcm" (T *) T)

; file num_co.c
(FLOAT "Lfloat" (T *) T NIL NIL
	:inline-always ((T) short-float nil nil "(Lfloat(1,#0),sf(VALUES(0)))")
	:inline-always ((fixnum-float) long-float nil nil "((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil "((float)(#0))"))
(NUMERATOR "Lnumerator" (T) T)
(DENOMINATOR "Ldenominator" (T) T)
(FLOOR "Lfloor" (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)/(#1):ifloor(#0,#1))"))
(CEILING "Lceiling" (T *) (VALUES T T) NIL NIL)
(TRUNCATE "Ltruncate" (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum-float) fixnum nil nil "(fixnum)(#0)"))
(ROUND "Lround" (T *) (VALUES T T))
(MOD "Lmod" (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)%(#1):imod(#0,#1))"))
(REM "Lrem" (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)%(#1)"))
(DECODE-FLOAT "Ldecode_float" (T) (VALUES T T T))
(SCALE-FLOAT "Lscale_float" (T T) T)
(FLOAT-RADIX "Lfloat_radix" (T) FIXNUM)
(FLOAT-SIGN "Lfloat_sign" (T *) T)
(FLOAT-DIGITS "Lfloat_digits" (T) FIXNUM)
(FLOAT-PRECISION "Lfloat_precision" (T) FIXNUM)
(INTEGER-DECODE-FLOAT "Linteger_decode_float" (T) (VALUES T T T))
(COMPLEX "Lcomplex" (T *) T)
(REALPART "Lrealpart" (T) T)
(IMAGPART "Limagpart" (T) T)
(= "Lall_the_same" (T *) T NIL T
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)==0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)==(#1)"))
(/= "Lall_different" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)!=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)!=(#1)"))
(< "Lmonotonically_increasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<(#1)"))
(> "Lmonotonically_decreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>(#1)"))
(<= "Lmonotonically_nondecreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<=(#1)"))
(>= "Lmonotonically_nonincreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>=(#1)"))
(MAX "Lmax" (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)>=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)>=(#1)?#0:#1"))
(MIN "Lmin" (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)<=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)<=(#1)?#0:#1"))
(LOGIOR "Llogior" (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) | (#1))"))
(LOGXOR "Llogxor" (*) T NIL NIL)
(LOGAND "Llogand" (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) & (#1))"))
(LOGEQV "Llogeqv" (*) T NIL NIL)
(BOOLE "Lboole" (T T T) T NIL NIL)
(LOGBITP "Llogbitp" (T T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#1 >> #0) & 1"))
(ASH "Lash" (T T) T)
(LOGCOUNT "Llogcount" (T) T)
(INTEGER-LENGTH "Linteger_length" (T) FIXNUM)
(si::BIT-ARRAY-OP "siLbit_array_op" nil T)
(ZEROP "Lzerop" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)==0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)==0"))
(PLUSP "Lplusp" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)<0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)>0"))
(MINUSP "Lminusp" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)>0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)<0"))
(ODDP "Loddp" (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#0) & 1"))
(EVENP "Levenp" (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "~(#0) & 1"))
(RANDOM "Lrandom" (T *) T)
(MAKE-RANDOM-STATE "Lmake_random_state" (*) T)
(RANDOM-STATE-P "Lrandom_state_p" (T) T NIL T)
(EXP "Lexp" (T) T NIL NIL
	:inline-always ((number) t nil t "number_exp(#0)"))
(EXPT "Lexpt" (T T) T NIL NIL
	:inline-always ((t t) t nil t "number_expt(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil
		(lambda (loc1 loc2)
		  (if (and (consp loc1) (eq (car loc1) 'fixnum)
			   (consp (cadr loc1)) (eq (caadr loc1) 'fixnum-value)
			   (eq (cadr (cadr loc1)) 2))
		      (progn (wt1 "(1<<(") (wt1 loc2) (wt1 "))"))
		    (progn (wt1 "fixnum_expt(") (wt1 loc1) (wt1 #\,) (wt1 loc2)
			   (wt1 #\) ))))))
(LOG "Llog" (T *) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "log((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)log((double)(#0))"))
(SQRT "Lsqrt" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "sqrt((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sqrt((double)(#0))"))
(SIN "Lsin" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "sin((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sin((double)(#0))"))
(COS "Lcos" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "cos((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)cos((double)(#0))"))
(tan "Ltan" (number) number nil nil
	:inline-always ((fixnum-float) long-float nil nil "tan((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)tan((double)(#0))"))
(ATAN "Latan" (T *) T)

; file package.d
(MAKE-PACKAGE "Lmake_package" (T *) T)
(si::SELECT-PACKAGE "siLselect_package" (T) T)
(FIND-PACKAGE "Lfind_package" (T) T)
(PACKAGE-NAME "Lpackage_name" (T) T)
(PACKAGE-NICKNAMES "Lpackage_nicknames" (T) T)
(RENAME-PACKAGE "Lrename_package" (T T *) T)
(PACKAGE-USE-LIST "Lpackage_use_list" (T) T)
(PACKAGE-USED-BY-LIST "Lpackage_used_by_list" (T) T)
(PACKAGE-SHADOWING-SYMBOLS "Lpackage_shadowing_symbols" (T) T)
(LIST-ALL-PACKAGES "Llist_all_packages" NIL T)
(INTERN "Lintern" (string *) (VALUES T T))
(FIND-SYMBOL "Lfind_symbol" (string *) (VALUES T T))
(UNINTERN "Lunintern" (symbol t) T)
(EXPORT "Lexport" (T *) T)
(UNEXPORT "Lunexport" (T *) T)
(IMPORT "Limport" (T *) T)
(SHADOWING-IMPORT "Lshadowing_import" (T *) T)
(SHADOW "Lshadow" (T *) T)
(USE-PACKAGE "Luse_package" (T *) T)
(UNUSE-PACKAGE "Lunuse_package" (T *) T)
(si::PACKAGE-INTERNAL "siLpackage_internal" nil T)
(si::PACKAGE-EXTERNAL "siLpackage_external" nil T)
(PATHNAME "Lpathname" (T) T)
(PARSE-NAMESTRING "Lparse_namestring" (T *) T)
(MERGE-PATHNAMES "Lmerge_pathnames" (T *) T)
(MAKE-PATHNAME "Lmake_pathname" (*) T)
(PATHNAMEP "Lpathnamep" (T) T NIL T)
(PATHNAME-HOST "Lpathname_host" (T) T)
(PATHNAME-DEVICE "Lpathname_device" (T) T)
(PATHNAME-DIRECTORY "Lpathname_directory" (T) T)
(PATHNAME-NAME "Lpathname_name" (T) T)
(PATHNAME-TYPE "Lpathname_type" (T) T)
(PATHNAME-VERSION "Lpathname_version" (T) T)
(NAMESTRING "Lnamestring" (T) string NIL NIL
	:inline-always ((t) t nil t "coerce_to_namestring(#0)"))
(FILE-NAMESTRING "Lfile_namestring" (T) STRING)
(DIRECTORY-NAMESTRING "Ldirectory_namestring" (T) STRING)
(HOST-NAMESTRING "Lhost_namestring" (T) STRING)
(ENOUGH-NAMESTRING "Lenough_namestring" (T *) STRING)
(NULL "Lnull" (T) T NIL T
	:inline-always ((t) boolean nil nil "#0==Cnil"))
(SYMBOLP "Lsymbolp" (T) T NIL T
	:inline-always ((t) boolean nil nil "SYMBOLP(#0)"))
(ATOM "Latom" (T) T NIL T
	:inline-always ((t) boolean nil nil "ATOM(#0)"))
(CONSP "Lconsp" (T) T NIL T
	:inline-always ((t) boolean nil nil "CONSP(#0)"))
(LISTP "Llistp" (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;LISTP(#0)"))
(NUMBERP "Lnumberp" (T) T NIL T
	:inline-always ((t) boolean nil nil "numberp(#0)"))
(INTEGERP "Lintegerp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum"))
(RATIONALP "Lrationalp" (T) T nil t)
(FLOATP "Lfloatp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_shortfloat||type_of(#0)==t_longfloat"))
(COMPLEXP "Lcomplexp" (T) T NIL T)
(CHARACTERP "Lcharacterp" (T) T NIL T
	:inline-always ((t) boolean nil nil "CHARACTERP(#0)"))
(STRINGP "Lstringp" (T) T NIL T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_string"))
(BIT-VECTOR-P "Lbit_vector_p" (T) T NIL T
	:inline-always ((t) boolean nil nil "(type_of(#0)==t_bitvector)"))
(VECTORP "Lvectorp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_vector||
type_of(#0)==t_string||
type_of(#0)==t_bitvector"))
(SIMPLE-STRING-P "Lsimple_string_p" (T) T NIL T)
(SIMPLE-BIT-VECTOR-P "Lsimple_bit_vector_p" (T) T NIL T)
(SIMPLE-VECTOR-P "Lsimple_vector_p" (T) T NIL T)
(ARRAYP "Larrayp" (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;ARRAYP(#0)"))
(PACKAGEP "Lpackagep" (T) T NIL T)
(FUNCTIONP "Lfunctionp" (T) T NIL T)
(COMPILED-FUNCTION-P "Lcompiled_function_p" (T) T NIL T)
(EQ "Leq" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQL "Leql" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "eql(#0,#1)")
	:inline-always ((character t) boolean nil nil	; Beppe
		"(CHARACTERP(#1) && (#0)==CHAR_CODE(#1))")
	:inline-always ((t character) boolean nil nil	; Beppe
		"(CHARACTERP(#0) && CHAR_CODE(#0)==(#1))")
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUAL "Lequal" (T T) T nil t
	:inline-always ((t t) boolean nil nil "equal(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUALP "Lequalp" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "equalp(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(NOT "Lnull" (T) T NIL T
	:inline-always ((t) boolean nil nil "(#0)==Cnil"))

; file print.d
(CLEAR-OUTPUT "Lclear_output" (*) T)
(FINISH-OUTPUT "Lforce_output" (*) T)
(FORCE-OUTPUT "Lforce_output" (*) T)
(FRESH-LINE "Lfresh_line" (*) T)
(LISTEN "Llisten" (*) T)
(PEEK-CHAR "Lpeek_char" (*) T)
(PPRINT "Lpprint" (T *) T)
(PRIN1 "Lprin1" (T *) T NIL NIL
	:inline-always ((t t) t t nil "prin1(#0,#1)")
	:inline-always ((t) t t nil "prin1(#0,Cnil)"))
(PRINC "Lprinc" (T *) T NIL NIL
	:inline-always ((t t) t t nil "princ(#0,#1)")
	:inline-always ((t) t t nil "princ(#0,Cnil)"))
(PRINT "Lprint" (T *) T NIL NIL
	:inline-always ((t t) t t nil "print(#0,#1)")
	:inline-always ((t) t t nil "print(#0,Cnil)"))
(PROBE-FILE "Lprobe_file" (T) T NIL T
	:inline-always ((t) boolean nil nil "(file_exists(#0))"))
(UNREAD-CHAR "Lunread_char" (T *) T)
(READ "Lread" (*) T)
(READ-CHAR "Lread_char" (*) T)
(READ-DELIMITED-LIST "Lread_delimited_list" (T *) T)
(READ-LINE "Lread_line" (*) (VALUES T T))
(READ-PRESERVING-WHITESPACE "Lread_preserving_whitespace" nil T)
(TERPRI "Lterpri" (*) T NIL T
	:inline-always ((t) t t nil "terpri(#0)")
	:inline-always (nil t t nil "terpri(Cnil)"))
(WRITE "Lwrite" (T *) T)
(WRITE-BYTE "Lwrite_byte" (fixnum stream) T)
(si::WRITE-BYTES "Lwrite_bytes"	(stream string fixnum fixnum) T)
(WRITE-CHAR "Lwrite_char" (T *) T NIL NIL
	:inline-always ((t) t t nil "@0;(princ_char(char_code(#0),Cnil),(#0))"))
(WRITE-LINE "Lwrite_line" (T *) T)
(WRITE-STRING "Lwrite_string" (T *) T)
(READ-CHAR-NO-HANG "Lread_char_no_hang" (*) T)
(CLEAR-INPUT "Lclear_input" (*) T)
(PARSE-INTEGER "Lparse_integer" (T *))
(READ-BYTE "Lread_byte" (T *) T)
(si::READ-BYTES "Lread_bytes" (stream string fixnum fixnum) T)
(COPY-READTABLE "Lcopy_readtable" (*) T NIL NIL
	:inline-always ((null null) t nil nil "standard_readtable"))
(READTABLEP "Lreadtablep" (T) T NIL T)
(SET-SYNTAX-FROM-CHAR "Lset_syntax_from_char" (T T *) T)
(SET-MACRO-CHARACTER "Lset_macro_character" (T T *) T)
(GET-MACRO-CHARACTER "Lget_macro_character" (T *) T)
(MAKE-DISPATCH-MACRO-CHARACTER "Lmake_dispatch_macro_character" nil T)
(SET-DISPATCH-MACRO-CHARACTER "Lset_dispatch_macro_character" nil T)
(GET-DISPATCH-MACRO-CHARACTER "Lget_dispatch_macro_character" nil T)
(SI::STRING-TO-OBJECT "siLstring_to_object" (T) T)
(si::STANDARD-READTABLE "siLstandard_readtable" (T) T)
(SYMBOL-FUNCTION "Lsymbol_function" (T) T NIL NIL
	:inline-always ((t) t nil t "symbol_function(#0)"))
(FBOUNDP "Lfboundp" (symbol) T nil t)
(SYMBOL-VALUE "Lsymbol_value" (symbol) T)
(BOUNDP "Lboundp" (symbol) T nil t
	:inline-unsafe ((t) boolean nil nil "(#0)->symbol.dbind!=OBJNULL"))
(MACRO-FUNCTION "Lmacro_function" (symbol) T)
(SPECIAL-FORM-P "Lspecial_form_p" (symbol) T nil t)

; file unixsave.c
(SAVE "Lsave" (T) T)

; file unixsys.c
(si::SYSTEM "siLsystem" nil T)

; file sequence.d
(ELT "Lelt" (sequence fixnum) T nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil t "elt(#0,fix(#1))")
	:inline-unsafe ((t fixnum) t nil t "elt(#0,#1)"))
(si::ELT-SET "siLelt_set" (sequence fixnum t) T nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "elt_set(#0,fix(#1),#2)"))
(SUBSEQ "Lsubseq" (sequence fixnum *) sequence)
(COPY-SEQ "Lcopy_seq" (sequence) sequence)
(LENGTH "Llength" (sequence) fixnum t nil
	:inline-always ((t) fixnum nil nil "length(#0)")
	:inline-unsafe (((array t)) fixnum nil nil "(#0)->vector.fillp")
	:inline-unsafe ((string) fixnum nil nil "(#0)->string.fillp"))
(REVERSE "Lreverse" (sequence) sequence nil nil
	:inline-always ((t) t nil t "reverse(#0)"))
(NREVERSE "Lnreverse" (sequence) sequence nil nil
	:inline-always ((t) t t t "nreverse(#0)"))

; file character.d
(CHAR "Lchar" (string fixnum) character nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil nil "code_char((#0)->string.self[fix(#1)])")
	:inline-unsafe ((t fixnum) fixnum nil nil "(#0)->string.self[#1]")
	:inline-unsafe ((t fixnum) character nil nil "(#0)->string.self[#1]"))
(si::CHAR-SET "siLchar_set"
	(string fixnum character) character nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil
		"@2;((#0)->string.self[fix(#1)]=char_code(#2),(#2))")
	:inline-unsafe ((t fixnum character) character t nil
		"(#0)->string.self[#1]= #2"))
(SCHAR "Lchar" (string fixnum) character nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil nil "code_char((#0)->string.self[fix(#1)])")
	:inline-unsafe ((t t) fixnum nil nil "(#0)->string.self[fix(#1)]")
	:inline-unsafe ((t fixnum) fixnum nil nil "(#0)->string.self[#1]")
	:inline-unsafe ((t fixnum) character nil nil "(#0)->string.self[#1]"))
(si::SCHAR-SET "siLchar_set"
	(string fixnum character) character nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil
		"@2;((#0)->string.self[fix(#1)]=char_code(#2),(#2))")
	:inline-unsafe ((t fixnum character) character t nil
		"(#0)->string.self[#1]= #2"))
(STRING= "Lstring_eq" (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_eq(#0,#1)"))
(STRING-EQUAL "Lstring_equal" (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_equal(#0,#1)"))
(STRING< "Lstring_l" (string string *) T nil t)
(STRING> "Lstring_g" (string string *) T nil t)
(STRING<= "Lstring_le" (string string *) T nil t)
(STRING>= "Lstring_ge" (string string *) T nil t)
(STRING/= "Lstring_neq" (string string *) T nil t)
(STRING-LESSP "Lstring_lessp" (string string *) T nil t)
(STRING-GREATERP "Lstring_greaterp" (string string *) T nil t)
(STRING-NOT-LESSP "Lstring_not_lessp" (string string *) T nil t)
(STRING-NOT-GREATERP "Lstring_not_greaterp" (string string *) T nil t)
(STRING-NOT-EQUAL "Lstring_not_equal" (string string *) T nil t)
(MAKE-STRING "Lmake_string" (fixnum *) string)
(STRING-TRIM "Lstring_trim" (t string) string)
(STRING-LEFT-TRIM "Lstring_left_trim" (t string) string)
(STRING-RIGHT-TRIM "Lstring_right_trim" (t string) string)
(STRING-UPCASE "Lstring_upcase" (string *) string)
(STRING-DOWNCASE "Lstring_downcase" (string *) string)
(STRING-CAPITALIZE "Lstring_capitalize" (string *) string)
(NSTRING-UPCASE "Lnstring_upcase" (string *) string)
(NSTRING-DOWNCASE "Lnstring_downcase" (string *) string)
(NSTRING-CAPITALIZE "Lnstring_capitalize" (string *) string)
(STRING "Lstring" (T) string nil t
	:inline-always ((t) t nil nil "coerce_to_string(#0)"))
(STRING-CONCATENATE "siLstring_concatenate" (T) string nil nil)

; file structure.d
(si::MAKE-STRUCTURE "siLmake_structure" (T *) T)
(si::COPY-STRUCTURE "siLcopy_structure" (T T) T)
(SI::STRUCTURE-NAME "siLstructure_name" (T) SYMBOL NIL NIL
	:inline-always ((structure) symbol nil nil "SNAME(#0)"))
(si::STRUCTURE-REF "siLstructure_ref" (t t fixnum) T nil nil
	:inline-always ((t t fixnum) t nil nil "structure_ref(#0,#1,#2)"))
(si::STRUCTURE-SET "siLstructure_set" (t t fixnum t) T nil nil
	:inline-always ((t t fixnum t) t T nil "structure_set(#0,#1,#2,#3)"))
(SI::STRUCTUREP "siLstructurep" (T) T NIL T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_structure"))
(SI::STRUCTURE-SUBTYPE-P "siLstructure_subtype_p" (T T) T NIL T)
(si::RPLACA-NTHCDR "siLrplaca_nthcdr" (T T T) nil T nil t)
(si::LIST-NTH "siLlist_nth" (T T) T nil t)

; file toplevel.c
(si::*MAKE-SPECIAL "siLAmake_special" nil T)
(si::*MAKE-CONSTANT "siLAmake_constant" nil T)

; file symbol.d
(GET "Lget" (symbol t *) T nil nil
	:inline-always ((t t t) t nil nil "get(#0,#1,#2)")
	:inline-always ((t t) t nil nil "get(#0,#1,Cnil)")
	:inline-unsafe ((t t t) t nil nil "getf((#0)->symbol.plist,#1,#2)")
	:inline-unsafe ((t t) t nil nil "getf((#0)->symbol.plist,#1,Cnil)"))
(REMPROP "Lremprop" (symbol t) T nil nil
	:inline-always ((t t) t t nil "remprop(#0,#1)"))
(SYMBOL-PLIST "Lsymbol_plist" (symbol) T nil T
	:inline-always ((t) t nil nil "((#0)->symbol.plist)"))
(GETF "Lgetf" (T T *) T)
(GET-PROPERTIES "Lget_properties" (T T) *)
(SYMBOL-NAME "Lsymbol_name" (symbol) string nil nil
	:inline-always ((symbol) t nil t "((#0)->symbol.name)")
	:inline-always ((t) t nil t "symbol_name(#0)"))
(MAKE-SYMBOL "Lmake_symbol" (string) symbol)
(COPY-SYMBOL "Lcopy_symbol" (symbol *) symbol)
(GENSYM "Lgensym" (*) symbol)
(GENTEMP "Lgentemp" (*) symbol)
(SYMBOL-PACKAGE "Lsymbol_package" (symbol) T)
(KEYWORDP "Lkeywordp" (T) T NIL T
;  :inline-always ((t) boolean nil nil
;        "@0;(type_of(#0)==t_symbol&&(#0)->symbol.hpack==keyword_package)")
 )
(SI::PUT-F "siLput_f" NIL (T T))
(SI::REM-F "siLrem_f" NIL (T T))
(si::SET-SYMBOL-PLIST "siLset_symbol_plist" (symbol t) T)
(SI::PUTPROP "siLputprop" (T T T) T NIL NIL
	:inline-always ((t t t) t t nil "putprop(#0,#1,#2)"))

; file tcp.c
(si::OPEN-TCP-STREAM "Lopen_tcp_stream" (T T) T)

; file unixfasl.c
(si::READ-EXTERNALS "siLread_externals" nil T)
(si::SET-UP-COMBINED "siLset_up_combined" nil T)
(si::BUILD-SYMBOL-TABLE "siLbuild_symbol_table" nil T)
#+bsd
(si::FASLINK "siLfaslink" nil T)

; file unixtime.c
(si::DAYLIGHT-SAVING-TIME-P "Ldaylight_saving_timep" nil T nil t)
(GET-UNIVERSAL-TIME "Lget_universal_time" nil T)
(GET-INTERNAL-RUN-TIME "Lget_internal_run_time" nil T)
(GET-INTERNAL-REAL-TIME "Lget_internal_real_time" nil T)
(si::GET-LOCAL-TIME-ZONE "Lget_local_time_zone" nil T)
(SLEEP "Lsleep" (real) T)

(TYPE-OF "Ltype_of" (T) T NIL NIL
	:inline-always ((t) t nil t "TYPE_OF(#0)"))

;;; Beppe's additions
(READ-BYTES "Lread_bytes" (stream string fixnum fixnum) T)
(WRITE-BYTES "Lwrite_bytes" (stream string fixnum fixnum) T)

;;; AKCL additions:
(SI::COPY-STREAM "siLcopy_stream" (T T) T)

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
(si::put-properties "siLput_properties" (*) nil T)
)) ; end of inlines

;;; Prolog:
#+locative
(setf (get 'si::trail-restore 'proclaimed-return-type) 'null) ; C2OR optimization
#+locative
(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
(si::trail-mark nil nil nil nil nil
	:inline-always (() nil t nil "trail_mark"))
(si::trail-restore nil nil nil nil nil
	:inline-always (() nil t nil "trail_restore"))
(si::trail-unmark nil nil nil nil nil
	:inline-always (() nil t nil "trail_unmark"))
(si::get-value nil nil nil nil nil
	:inline-always ((t t) boolean t nil "get_value(#0, #1)"))
(si::get-constant nil nil nil nil nil
	:inline-always ((t t) boolean t nil "get_constant(#0, #1)"))
(si::get-nil nil nil nil nil nil
	:inline-always ((t) boolean t nil "get_nil(#0)"))
(si::get-cons nil nil nil nil nil
	:inline-always ((t) boolean t nil "get_cons(#0)"))
(si::unify-slot nil nil nil nil nil
	:inline-always (() t t nil "(*slotf)(*slot)"))
(si::unify-value nil nil nil nil nil
	:inline-always ((t) boolean t nil "(*slotf)(#0)"))
(si::unify-constant nil nil nil nil nil
	:inline-always ((t) boolean t nil "(*slotf)(#0)"))
(si::unify-nil nil nil nil nil nil
	:inline-always (() boolean t nil "(*slotf)(Cnil)"))
)) ; end of #+locative

#+clos
(mapcar #'(lambda (x) (apply #'defsysfun x)) '(
; file setf.c
(si::SETF-NAMEP "siLsetf_namep" nil T nil t)

; file instance.c
(si::ALLOCATE-INSTANCE "siLallocate_instance" (t fixnum) T)
(si::INSTANCE-REF "siLinstance_ref" (t fixnum) T nil nil
	:inline-always ((standard-object fixnum) t nil nil
		"(#0)->instance.slots[#1]"))
(si::INSTANCE-REF-SAFE "siLinstance_ref_safe" (t fixnum) T nil nil
	:inline-unsafe ((standard-object fixnum) t nil nil
		"(#0)->instance.slots[#1]"))
(si::INSTANCE-SET "siLinstance_set" (t fixnum t) T nil nil
	:inline-always ((standard-object fixnum t) t t nil
		"(#0)->instance.slots[#1]=(#2)"))
(si::INSTANCE-CLASS "siLinstance_class" (t) T nil nil
	:inline-always ((standard-object) t nil nil "(#0)->instance.class"))
(si::INSTANCE-CLASS-SET "siLinstance_class_set" (t t) T)
(si::INSTANCEP "siLinstancep" (t) T nil t)
(si::SL-BOUNDP "siLsl_boundp" (t) T nil t
	:inline-always ((t) boolean nil nil "(#0)!=OBJNULL"))
(si::SL-MAKUNBOUND "siLsl_makunbound" (t fixnum) T nil t)

; file gfun.c
(si::ALLOCATE-GFUN "siLallocate_gfun" nil T)
(si::GFUN-NAME  "siLgfun_name" nil T)
(si::GFUN-NAME-SET "siLgfun_name_set" nil T)
(si::GFUN-METHOD-HT "siLgfun_method_ht" nil T)
(si::GFUN-METHOD-HT-SET "siLgfun_method_ht_set" nil T)
(si::GFUN-SPEC-HOW-REF  "siLgfun_spec_how_ref" nil T)
(si::GFUN-SPEC-HOW-SET "siLgfun_spec_how_set" nil T)
(si::GFUN-INSTANCE  "siLgfun_instance" nil T)
(si::GFUN-INSTANCE-SET "siLgfun_instance_set" nil T)
(si::GFUNP "siLgfunp" nil T)
)) ; end of of #+clos

