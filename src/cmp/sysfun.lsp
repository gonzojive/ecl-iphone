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
(AREF "clLaref" (array *) T NIL NIL
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
(ARRAY-ELEMENT-TYPE "clLarray_element_type" (array) T)
(ARRAY-RANK "clLarray_rank" (array) fixnum)
(ARRAY-DIMENSION "clLarray_dimension" (array fixnum) fixnum)
(ARRAY-TOTAL-SIZE "clLarray_total_size" (array) T nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.dim)"))
(ADJUSTABLE-ARRAY-P "clLadjustable_array_p" (array) T nil t)
(si::DISPLACED-ARRAY-P "siLdisplaced_array_p" (array) T nil t)
(SVREF "clLsvref" (simple-vector fixnum) T nil nil
	:inline-always ((t t) t nil t "aref1(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "aref1(#0,#1)")
	:inline-unsafe ((t t) t nil nil "(#0)->vector.self.t[fix(#1)]")
	:inline-unsafe ((t fixnum) t nil nil "(#0)->vector.self.t[#1]"))
(si::SVSET "siLsvset" (simple-vector fixnum t) T nil nil
	:inline-always ((t t t) t t nil "aset1(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "aset1(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "((#0)->vector.self.t[fix(#1)]=(#2))")
	:inline-unsafe ((t fixnum t) t t nil "(#0)->vector.self.t[#1]= #2"))
(ARRAY-HAS-FILL-POINTER-P "clLarray_has_fill_pointer_p" nil T nil t)
(FILL-POINTER "clLfill_pointer" (vector) fixnum nil nil
	:inline-unsafe ((t) fixnum nil nil "((#0)->string.fillp)"))
(si::FILL-POINTER-SET "siLfill_pointer_set"
	(vector fixnum) fixnum nil nil
	:inline-unsafe ((t fixnum) fixnum t nil "((#0)->string.fillp)=(#1)"))
(si::REPLACE-ARRAY "siLreplace_array" nil T nil nil)
;(si::ASET-BY-CURSOR "siLaset_by_cursor" nil T nil nil)

; file assignment.c
(SET "clLset" (symbol t) T)
(si::FSET "siLfset" (symbol t) T)
(MAKUNBOUND "clLmakunbound" (symbol) T)
(FMAKUNBOUND "clLfmakunbound" (symbol) T)
(si::CLEAR-COMPILER-PROPERTIES "siLclear_compiler_properties" nil T)

; file catch.c
;#-clcs (SI::ERROR-SET "siLerror_set" (T) * NIL NIL)

; file cfun.c
(si::COMPILED-FUNCTION-NAME "siLcompiled_function_name" nil T)

; file character.c
(STANDARD-CHAR-P "clLstandard_char_p" (character) T nil t)
(GRAPHIC-CHAR-P "clLgraphic_char_p" (character) T nil t)
(ALPHA-CHAR-P "clLalpha_char_p" (character) T nil t)
(UPPER-CASE-P "clLupper_case_p" (character) T nil t)
(LOWER-CASE-P "clLlower_case_p" (character) T nil t)
(BOTH-CASE-P "clLboth_case_p" (character) T nil t)
(DIGIT-CHAR-P "clLdigit_char_p" (character *) T nil nil
	:inline-always
	((character) boolean nil nil "@0; ((#0) <= '9' && (#0) >= '0')"))
(ALPHANUMERICP "clLalphanumericp" (character) T nil t)
(CHARACTER "clLcharacter" (T) CHARACTER)
(CHAR= "clLcharE" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)==char_code(#1)"))
(CHAR/= "clLcharNE" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)!=(#1)")
	:inline-always ((t t) boolean nil nil "char_code(#0)!=char_code(#1)"))
(CHAR< "clLcharL" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<(#1)"))
(CHAR> "clLcharG" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>(#1)"))
(CHAR<= "clLcharLE" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)<=(#1)"))
(CHAR>= "clLcharGE" (character *) T nil t
	:inline-always ((character character) boolean nil nil "(#0)>=(#1)"))
(CHAR-EQUAL "clLchar_equal" (character *) T nil t)
(CHAR-NOT-EQUAL "clLchar_not_equal" (character *) T nil t)
(CHAR-LESSP "clLchar_lessp" (character *) T nil t)
(CHAR-GREATERP "clLchar_greaterp" (character *) T nil t)
(CHAR-NOT-GREATERP "clLchar_not_greaterp" (character *) T nil t)
(CHAR-NOT-LESSP "clLchar_not_lessp" (character *) T nil t)
(CHARACTER "clLcharacter" nil character nil nil)
(CHAR-CODE "clLchar_code" (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(CODE-CHAR "clLcode_char" (fixnum) character nil nil
	:inline-always ((fixnum) character nil nil "#0"))
(CHAR-UPCASE "clLchar_upcase" (character) character nil nil)
(CHAR-DOWNCASE "clLchar_downcase" (character) character nil nil)
(DIGIT-CHAR "clLdigit_char" (fixnum *) character nil nil)
(CHAR-INT "clLchar_int" (character) fixnum nil nil
	:inline-always ((character) fixnum nil nil "#0"))
(INT-CHAR "clLint_char" (fixnum) character nil nil
	:inline-always ((fixnum) character nil nil "#0"))
(CHAR-NAME "clLchar_name" (character) symbol)
(NAME-CHAR "clLname_char" (symbol) character)

; ; file error.c
#-clcs
(ERROR "clLerror" (T *) T nil nil)
#-clcs
(CERROR "clLcerror" (T T *) T nil nil)

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
(APPLY "clLapply" (T T *) T)
(FUNCALL "clLfuncall" (T *) T)
(EVAL "clLeval" (T) T)
(EVALHOOK "clLevalhook" (T T T *) T)
(APPLYHOOK "clLapplyhook" (T T T T *) T)
(CONSTANTP "clLconstantp" (T) T NIL T)
(si::UNLINK-SYMBOL "siLunlink_symbol" nil T)
(si::LINK-ENABLE "siLlink_enable" nil T)

; file file.d
(MAKE-SYNONYM-STREAM "clLmake_synonym_stream" (T) T)
(MAKE-BROADCAST-STREAM "clLmake_broadcast_stream" (*) T)
(MAKE-CONCATENATED-STREAM "clLmake_concatenated_stream" nil T)
(MAKE-TWO-WAY-STREAM "clLmake_two_way_stream" (T T) T)
(MAKE-ECHO-STREAM "clLmake_echo_stream" (T T) T)
(MAKE-STRING-INPUT-STREAM "clLmake_string_input_stream" nil T)
(MAKE-STRING-OUTPUT-STREAM "clLmake_string_output_stream" nil T)
(GET-OUTPUT-STREAM-STRING "clLget_output_stream_string" nil T)
(SI::OUTPUT-STREAM-STRING "siLoutput_stream_string" (T) T)
(STREAMP "clLstreamp" (T) T NIL T)
(INPUT-STREAM-P "clLinput_stream_p" (T) T NIL T)
(OUTPUT-STREAM-P "clLoutput_stream_p" (T) T NIL T)
(STREAM-ELEMENT-TYPE "clLstream_element_type" (T) T)
(CLOSE "clLclose" (T *) T)
;#-clcs (OPEN "clLopen" (T *) T)
(FILE-POSITION "clLfile_position" (T *) T)
(FILE-LENGTH "clLfile_length" (T) T)
;#-clcs (LOAD "clLload" (T *) T)
(si::GET-STRING-INPUT-STREAM-INDEX "siLget_string_input_stream_index" nil T)
(si::MAKE-STRING-OUTPUT-STREAM-FROM-STRING
	"siLmake_string_output_stream_from_string" nil T)

; file gbc.c
(si::ROOM-REPORT "siLroom_report" nil T)
(si::RESET-GBC-COUNT "siLreset_gbc_count" nil T)
(GBC "clLgbc" nil T)

; file unixfsys.c
(TRUENAME "clLtruename" (T) T)
(RENAME-FILE "clLrename_file" (T T) T)
(SI::SPECIALP "siLspecialp" (T) T NIL T)
(DELETE-FILE "clLdelete_file" (T) T)
(PROBE-FILE "clLprobe_file" (T) T)
(FILE-WRITE-DATE "clLfile_write_date" (T) T)
(FILE-AUTHOR "clLfile_author" (T) T)
(PATHNAME "clLpathname" (T) T)
(USER-HOMEDIR-PATHNAME "clLuser_homedir_pathname" (*) T)
(DIRECTORY "clLdirectory" (T) T)
(si::CHDIR "siLchdir" nil T)

; file unixint.c
(si::CATCH-BAD-SIGNALS "siLcatch_bad_signals" nil T)
(si::UNCATCH-BAD-SIGNALS "siLuncatch_bad_signals" nil T)

; file format.c
(FORMAT "clLformat" (T string *) T)

; file hash.d
(MAKE-HASH-TABLE "clLmake_hash_table" (*) T)
(HASH-TABLE-P "clLhash_table_p" (T) T NIL T)
(VALUES "clLvalues" (*) *)
(GETHASH "clLgethash" (T T *) (VALUES T T))
(REMHASH "clLremhash" (T T) T)
(MAPHASH "clLmaphash" (T T) T)
(CLRHASH "clLclrhash" (T) T)
(HASH-TABLE-COUNT "clLhash_table_count" (T) T)
(SXHASH "clLsxhash" (T) FIXNUM)
(SI::HASH-SET "siLhash_set" NIL T)

; file list.d
(CAR "clLcar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "car(#0)")
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(CDR "clLcdr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdr(#0)")
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(CAAR "clLcaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caar(#0)")
	:inline-unsafe ((t) t nil nil "CAAR(#0)"))
(CADR "clLcadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadr(#0)")
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(CDAR "clLcdar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdar(#0)")
	:inline-unsafe ((t) t nil nil "CDAR(#0)"))
(CDDR "clLcddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDR(#0)"))
(CAAAR "clLcaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaar(#0)")
	:inline-unsafe ((t) t nil nil "CAAAR(#0)"))
(CAADR "clLcaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caadr(#0)")
	:inline-unsafe ((t) t nil nil "CAADR(#0)"))
(CADAR "clLcadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadar(#0)")
	:inline-unsafe ((t) t nil nil "CADAR(#0)"))
(CADDR "clLcaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(CDAAR "clLcdaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaar(#0)")
	:inline-unsafe ((t) t nil nil "CDAAR(#0)"))
(CDADR "clLcdadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdadr(#0)")
	:inline-unsafe ((t) t nil nil "CDADR(#0)"))
(CDDAR "clLcddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddar(#0)")
	:inline-unsafe ((t) t nil nil "CDDAR(#0)"))
(CDDDR "clLcdddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDDR(#0)"))
(CAAAAR "clLcaaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaaar(#0)")
	:inline-unsafe ((t) t nil nil "CAAAAR(#0)"))
(CAAADR "clLcaaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaadr(#0)")
	:inline-unsafe ((t) t nil nil "CAAADR(#0)"))
(CAADAR "clLcaadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caadar(#0)")
	:inline-unsafe ((t) t nil nil "CAADAR(#0)"))
(CAADDR "clLcaaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caaddr(#0)")
	:inline-unsafe ((t) t nil nil "CAADDR(#0)"))
(CADAAR "clLcadaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadaar(#0)")
	:inline-unsafe ((t) t nil nil "CADAAR(#0)"))
(CADADR "clLcadadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadadr(#0)")
	:inline-unsafe ((t) t nil nil "CADADR(#0)"))
(CADDAR "clLcaddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "caddar(#0)")
	:inline-unsafe ((t) t nil nil "CADDAR(#0)"))
(CADDDR "clLcadddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cadddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(CDAAAR "clLcdaaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaaar(#0)")
	:inline-unsafe ((t) t nil nil "CDAAAR(#0)"))
(CDAADR "clLcdaadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaadr(#0)")
	:inline-unsafe ((t) t nil nil "CDAADR(#0)"))
(CDADAR "clLcdadar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdadar(#0)")
	:inline-unsafe ((t) t nil nil "CDADAR(#0)"))
(CDADDR "clLcdaddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdaddr(#0)")
	:inline-unsafe ((t) t nil nil "CDADDR(#0)"))
(CDDAAR "clLcddaar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddaar(#0)")
	:inline-unsafe ((t) t nil nil "CDDAAR(#0)"))
(CDDADR "clLcddadr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddadr(#0)")
	:inline-unsafe ((t) t nil nil "CDDADR(#0)"))
(CDDDAR "clLcdddar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdddar(#0)")
	:inline-unsafe ((t) t nil nil "CDDDAR(#0)"))
(CDDDDR "clLcddddr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cddddr(#0)")
	:inline-unsafe ((t) t nil nil "CDDDDR(#0)"))
(CONS "clLcons" (T T) T NIL NIL
	:inline-always ((t t) t nil t "CONS(#0,#1)"))
(TREE-EQUAL "clLtree_equal" (T T *) T NIL T)
(ENDP "clLendp" (T) T NIL T
	:inline-safe ((t) boolean nil nil "endp(#0)")
	:inline-unsafe ((t) boolean nil nil "#0==Cnil"))
(LIST-LENGTH "clLlist_length" (T) T NIL NIL)
(NTH "clLnth" (T T) T NIL NIL
	:inline-always ((t t) t nil nil "nth(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nth(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nth(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nth(#0,#1)"))
(FIRST "clLcar" (T) T NIL NIL
	:inline-safe ((t) t nil nil "car(#0)")
	:inline-unsafe ((t) t nil nil "CAR(#0)"))
(SECOND "clLcadr" (T) T nil nil
	:inline-safe ((t) t nil nil "cadr(#0)")
	:inline-unsafe ((t) t nil nil "CADR(#0)"))
(THIRD "clLcaddr" (T) T nil nil
	:inline-safe ((t) t nil nil "caddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDR(#0)"))
(FOURTH "clLcadddr" (T) T nil nil
	:inline-safe ((t) t nil nil "cadddr(#0)")
	:inline-unsafe ((t) t nil nil "CADDDR(#0)"))
(FIFTH "clLfifth" (T) T)
(SIXTH "clLsixth" (T) T)
(SEVENTH "clLseventh" (T) T)
(EIGHTH "clLeighth" (T) T)
(NINTH "clLninth" (T) T)
(TENTH "clLtenth" (T) T)
(REST "clLcdr" (T) T NIL NIL
	:inline-safe ((t) t nil nil "cdr(#0)")
	:inline-unsafe ((t) t nil nil "CDR(#0)"))
(NTHCDR "clLnthcdr" (fixnum t) T nil nil
	:inline-always ((t t) t nil nil "nthcdr(fixint(#0),#1)")
	:inline-always ((fixnum t) t nil nil "nthcdr(#0,#1)")
	:inline-unsafe ((t t) t nil nil "nthcdr(fix(#0),#1)")
	:inline-unsafe ((fixnum t) t nil nil "nthcdr(#0,#1)"))
(LAST "clLlast" (T) T)
(LIST "clLlist" (*) T NIL NIL
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
(LIST* "clLlistX" (T *) T NIL NIL
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
(MAKE-LIST "clLmake_list" (fixnum *) T)
(APPEND "clLappend" (*) T NIL NIL
	:inline-always ((t t) t nil t "append(#0,#1)"))
(COPY-LIST "clLcopy_list" (T) T)
(COPY-ALIST "clLcopy_alist" (T) T)
(COPY-TREE "clLcopy_tree" (T) T)
(REVAPPEND "clLrevappend" (T T) T)
(NCONC "clLnconc" (*) T NIL NIL
	:inline-always ((t t) t t nil "nconc(#0,#1)"))
(NRECONC "clLreconc" (T T) T)
(BUTLAST "clLbutlast" (T *) T)
(NBUTLAST "clLnbutlast" (T *) T)
(LDIFF "clLldiff" (T T) T)
(RPLACA "clLrplaca" (cons T) T)
(RPLACD "clLrplacd" (cons T) T)
(SUBST "clLsubst" (T T T *) T)
(SUBST-IF "clLsubst_if" (T T T *) T)
(SUBST-IF-NOT "clLsubst_if_not" (T T T *) T)
(NSUBST "clLnsubst" (T T T *) T)
(NSUBST-IF "clLnsubst_if" (T T T *) T)
(NSUBST-IF-NOT "clLnsubst_if_not" (T T T *) T)
(SUBLIS "clLsublis" (T T *) T)
(NSUBLIS "clLnsublis" (T T *) T)
(MEMBER "clLmember" (T T *) T)
(MEMBER-IF "clLmember_if" (T T *) T)
(MEMBER-IF-NOT "clLmember_if_not" (T T *) T)
(MEMBER1 "clLmember1"(T T *) T)
(TAILP "clLtailp" (T T) T NIL T)
(ADJOIN "clLadjoin" (T T *) T)
(ACONS "clLacons" (T T T) T)
(PAIRLIS "clLpairlis" (T T *) T)
(ASSOC "clLassoc" (T T *) T)
(ASSOC-IF "clLassoc_if" (T T) T)
(ASSOC-IF-NOT "clLassoc_if_not" (T T) T)
(RASSOC "clLrassoc" (T T *) T)
(RASSOC-IF "clLrassoc_if" (T T) T)
(RASSOC-IF-NOT "clLrassoc_if_not" (T T) T)
(si::MEMQ "siLmemq" (T T T) T)

; file lwp.c
;to do

; file macros.c
(si::DEFINE-MACRO "siLdefine_macro" nil T)
(MACROEXPAND "clLmacroexpand" (T *) (VALUES T T))
(MACROEXPAND-1 "clLmacroexpand_1" (T *) (VALUES T T))

; file main.c
(QUIT "clLquit" nil T)
(IDENTITY "clLidentity" (T) T)
(si::ARGC "siLargc" nil T)
(si::ARGV "siLargv" nil T)
(si::GETENV "siLgetenv" nil T)
(si::RESET-STACK-LIMITS "siLreset_stack_limits" nil T)
#-sparc
(si::INIT-SYSTEM "siLinit_system" nil T)
(si::POINTER "siLaddress" nil T)
(si::NANI "siLnani" nil T)

; file mapfun.c
(MAPCAR "clLmapcar" (T T *) T)
(MAPLIST "clLmaplist" (T T *) T)
(MAPC "clLmapc" (T T *) T)
(MAPL "clLmapl" (T T *) T)
(MAPCAN "clLmapcan" (T T *) T)
(MAPCON "clLmapcon" (T T *) T)

; file multival.c
(VALUES "clLvalues" nil T)
(VALUES-LIST "clLvalues_list" (T) *)

; file num_arith.c
(+ "clLplus" (*) T NIL NIL
	:inline-always ((t t) t nil t "number_plus(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)+(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)+(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)+(#1)"))
(- "clLminus" (T *) T NIL NIL
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
(* "clLtimes" (*) T NIL NIL
	:inline-always ((t t) t nil t "number_times(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)*(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)*(float)(#1)")
	:inline-always ((fixnum fixnum) t nil nil "fixnum_times(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)*(#1)"))
(/ "clLdivide" (T *) T NIL NIL
	:inline-always ((t t) t nil t "number_divide(#0,#1)")
	:inline-always ((fixnum-float fixnum-float) long-float nil nil
		"(double)(#0)/(double)(#1)")
	:inline-always ((fixnum-float fixnum-float) short-float nil nil
		"(float)(#0)/(float)(#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)/(#1)"))
(1+ "clLone_plus" (T) T NIL NIL
	:inline-always ((t) t nil t "one_plus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)+1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)+1")
	:inline-always ((fixnum) fixnum nil nil "(#0)+1"))
(1- "clLone_minus" (T) T NIL NIL
	:inline-always ((t) t nil t "one_minus(#0)")
	:inline-always ((fixnum-float) long-float nil nil "(double)(#0)-1")
	:inline-always ((fixnum-float) short-float nil nil "(float)(#0)-1")
	:inline-always ((fixnum) fixnum nil nil "(#0)-1"))
(CONJUGATE "clLconjugate" (T) T)
(GCD "clLgcd" (*) T)
(LCM "clLlcm" (T *) T)

; file num_co.c
(FLOAT "clLfloat" (T *) T NIL NIL
	:inline-always ((T) short-float nil nil "(Lfloat(1,#0),sf(VALUES(0)))")
	:inline-always ((fixnum-float) long-float nil nil "((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil "((float)(#0))"))
(NUMERATOR "clLnumerator" (T) T)
(DENOMINATOR "clLdenominator" (T) T)
(FLOOR "clLfloor" (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)/(#1):ifloor(#0,#1))"))
(CEILING "clLceiling" (T *) (VALUES T T) NIL NIL)
(TRUNCATE "clLtruncate" (T *) (VALUES T T) NIL NIL
	:inline-always ((fixnum-float) fixnum nil nil "(fixnum)(#0)"))
(ROUND "clLround" (T *) (VALUES T T))
(MOD "clLmod" (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil
		"@01;(#0>=0&&#1>0?(#0)%(#1):imod(#0,#1))"))
(REM "clLrem" (T T) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "(#0)%(#1)"))
(DECODE-FLOAT "clLdecode_float" (T) (VALUES T T T))
(SCALE-FLOAT "clLscale_float" (T T) T)
(FLOAT-RADIX "clLfloat_radix" (T) FIXNUM)
(FLOAT-SIGN "clLfloat_sign" (T *) T)
(FLOAT-DIGITS "clLfloat_digits" (T) FIXNUM)
(FLOAT-PRECISION "clLfloat_precision" (T) FIXNUM)
(INTEGER-DECODE-FLOAT "clLinteger_decode_float" (T) (VALUES T T T))
(COMPLEX "clLcomplex" (T *) T)
(REALPART "clLrealpart" (T) T)
(IMAGPART "clLimagpart" (T) T)
(= "clLall_the_same" (T *) T NIL T
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)==0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)==(#1)"))
(/= "clLall_different" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)!=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)!=(#1)"))
(< "clLmonotonically_increasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<(#1)"))
(> "clLmonotonically_decreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>(#1)"))
(<= "clLmonotonically_nondecreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)<=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)<=(#1)"))
(>= "clLmonotonically_nonincreasing" (T *) T nil t
	:inline-always ((t t) boolean nil nil "number_compare(#0,#1)>=0")
	:inline-always ((fixnum-float fixnum-float) boolean nil nil "(#0)>=(#1)"))
(MAX "clLmax" (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)>=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)>=(#1)?#0:#1"))
(MIN "clLmin" (T *) T NIL NIL
	:inline-always ((t t) t nil nil "@01;(number_compare(#0,#1)<=0?#0:#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil "@01;(#0)<=(#1)?#0:#1"))
(LOGIOR "clLlogior" (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) | (#1))"))
(LOGXOR "clLlogxor" (*) T NIL NIL)
(LOGAND "clLlogand" (*) T NIL NIL
	:inline-always ((fixnum fixnum) fixnum nil nil "((#0) & (#1))"))
(LOGEQV "clLlogeqv" (*) T NIL NIL)
(BOOLE "clLboole" (T T T) T NIL NIL)
(LOGBITP "clLlogbitp" (T T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#1 >> #0) & 1"))
(ASH "clLash" (T T) T)
(LOGCOUNT "clLlogcount" (T) T)
(INTEGER-LENGTH "clLinteger_length" (T) FIXNUM)
(si::BIT-ARRAY-OP "siLbit_array_op" nil T)
(ZEROP "clLzerop" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)==0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)==0"))
(PLUSP "clLplusp" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)<0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)>0"))
(MINUSP "clLminusp" (T) T NIL T
	:inline-always ((t) boolean nil nil "number_compare(MAKE_FIXNUM(0),#0)>0")
	:inline-always ((fixnum-float) boolean nil nil "(#0)<0"))
(ODDP "clLoddp" (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "(#0) & 1"))
(EVENP "clLevenp" (T) T NIL T
	:inline-always ((fixnum fixnum) boolean nil nil "~(#0) & 1"))
(RANDOM "clLrandom" (T *) T)
(MAKE-RANDOM-STATE "clLmake_random_state" (*) T)
(RANDOM-STATE-P "clLrandom_state_p" (T) T NIL T)
(EXP "clLexp" (T) T NIL NIL
	:inline-always ((number) t nil t "number_exp(#0)"))
(EXPT "clLexpt" (T T) T NIL NIL
	:inline-always ((t t) t nil t "number_expt(#0,#1)")
	:inline-always ((fixnum fixnum) fixnum nil nil
		(lambda (loc1 loc2)
		  (if (and (consp loc1) (eq (car loc1) 'fixnum)
			   (consp (cadr loc1)) (eq (caadr loc1) 'fixnum-value)
			   (eq (cadr (cadr loc1)) 2))
		      (progn (wt1 "(1<<(") (wt1 loc2) (wt1 "))"))
		    (progn (wt1 "fixnum_expt(") (wt1 loc1) (wt1 #\,) (wt1 loc2)
			   (wt1 #\) ))))))
(LOG "clLlog" (T *) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "log((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)log((double)(#0))"))
(SQRT "clLsqrt" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil t "sqrt((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sqrt((double)(#0))"))
(SIN "clLsin" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "sin((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)sin((double)(#0))"))
(COS "clLcos" (T) T NIL NIL
	:inline-always ((fixnum-float) long-float nil nil "cos((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)cos((double)(#0))"))
(tan "clLtan" (number) number nil nil
	:inline-always ((fixnum-float) long-float nil nil "tan((double)(#0))")
	:inline-always ((fixnum-float) short-float nil nil
		"(float)tan((double)(#0))"))
(ATAN "clLatan" (T *) T)

; file package.d
(MAKE-PACKAGE "clLmake_package" (T *) T)
(si::SELECT-PACKAGE "siLselect_package" (T) T)
(FIND-PACKAGE "clLfind_package" (T) T)
(PACKAGE-NAME "clLpackage_name" (T) T)
(PACKAGE-NICKNAMES "clLpackage_nicknames" (T) T)
(RENAME-PACKAGE "clLrename_package" (T T *) T)
(PACKAGE-USE-LIST "clLpackage_use_list" (T) T)
(PACKAGE-USED-BY-LIST "clLpackage_used_by_list" (T) T)
(PACKAGE-SHADOWING-SYMBOLS "clLpackage_shadowing_symbols" (T) T)
(LIST-ALL-PACKAGES "clLlist_all_packages" NIL T)
(INTERN "clLintern" (string *) (VALUES T T))
(FIND-SYMBOL "clLfind_symbol" (string *) (VALUES T T))
(UNINTERN "clLunintern" (symbol t) T)
(EXPORT "clLexport" (T *) T)
(UNEXPORT "clLunexport" (T *) T)
(IMPORT "clLimport" (T *) T)
(SHADOWING-IMPORT "clLshadowing_import" (T *) T)
(SHADOW "clLshadow" (T *) T)
(USE-PACKAGE "clLuse_package" (T *) T)
(UNUSE-PACKAGE "clLunuse_package" (T *) T)
(si::PACKAGE-INTERNAL "siLpackage_internal" nil T)
(si::PACKAGE-EXTERNAL "siLpackage_external" nil T)
(PATHNAME "clLpathname" (T) T)
(PARSE-NAMESTRING "clLparse_namestring" (T *) T)
(MERGE-PATHNAMES "clLmerge_pathnames" (T *) T)
(MAKE-PATHNAME "clLmake_pathname" (*) T)
(PATHNAMEP "clLpathnamep" (T) T NIL T)
(PATHNAME-HOST "clLpathname_host" (T) T)
(PATHNAME-DEVICE "clLpathname_device" (T) T)
(PATHNAME-DIRECTORY "clLpathname_directory" (T) T)
(PATHNAME-NAME "clLpathname_name" (T) T)
(PATHNAME-TYPE "clLpathname_type" (T) T)
(PATHNAME-VERSION "clLpathname_version" (T) T)
(NAMESTRING "clLnamestring" (T) string NIL NIL
	:inline-always ((t) t nil t "coerce_to_namestring(#0)"))
(FILE-NAMESTRING "clLfile_namestring" (T) STRING)
(DIRECTORY-NAMESTRING "clLdirectory_namestring" (T) STRING)
(HOST-NAMESTRING "clLhost_namestring" (T) STRING)
(ENOUGH-NAMESTRING "clLenough_namestring" (T *) STRING)
(NULL "clLnull" (T) T NIL T
	:inline-always ((t) boolean nil nil "#0==Cnil"))
(SYMBOLP "clLsymbolp" (T) T NIL T
	:inline-always ((t) boolean nil nil "SYMBOLP(#0)"))
(ATOM "clLatom" (T) T NIL T
	:inline-always ((t) boolean nil nil "ATOM(#0)"))
(CONSP "clLconsp" (T) T NIL T
	:inline-always ((t) boolean nil nil "CONSP(#0)"))
(LISTP "clLlistp" (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;LISTP(#0)"))
(NUMBERP "clLnumberp" (T) T NIL T
	:inline-always ((t) boolean nil nil "numberp(#0)"))
(INTEGERP "clLintegerp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_fixnum||type_of(#0)==t_bignum"))
(RATIONALP "clLrationalp" (T) T nil t)
(FLOATP "clLfloatp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_shortfloat||type_of(#0)==t_longfloat"))
(COMPLEXP "clLcomplexp" (T) T NIL T)
(CHARACTERP "clLcharacterp" (T) T NIL T
	:inline-always ((t) boolean nil nil "CHARACTERP(#0)"))
(STRINGP "clLstringp" (T) T NIL T
	:inline-always ((t) boolean nil nil "type_of(#0)==t_string"))
(BIT-VECTOR-P "clLbit_vector_p" (T) T NIL T
	:inline-always ((t) boolean nil nil "(type_of(#0)==t_bitvector)"))
(VECTORP "clLvectorp" (T) T NIL T
	:inline-always ((t) boolean nil nil
		"@0;type_of(#0)==t_vector||
type_of(#0)==t_string||
type_of(#0)==t_bitvector"))
(SIMPLE-STRING-P "clLsimple_string_p" (T) T NIL T)
(SIMPLE-BIT-VECTOR-P "clLsimple_bit_vector_p" (T) T NIL T)
(SIMPLE-VECTOR-P "clLsimple_vector_p" (T) T NIL T)
(ARRAYP "clLarrayp" (T) T NIL T
	:inline-always ((t) boolean nil nil "@0;ARRAYP(#0)"))
(PACKAGEP "clLpackagep" (T) T NIL T)
(FUNCTIONP "clLfunctionp" (T) T NIL T)
(COMPILED-FUNCTION-P "clLcompiled_function_p" (T) T NIL T)
(EQ "clLeq" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQL "clLeql" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "eql(#0,#1)")
	:inline-always ((character t) boolean nil nil	; Beppe
		"(CHARACTERP(#1) && (#0)==CHAR_CODE(#1))")
	:inline-always ((t character) boolean nil nil	; Beppe
		"(CHARACTERP(#0) && CHAR_CODE(#0)==(#1))")
	:inline-always ((character character) boolean nil nil "(#0)==(#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUAL "clLequal" (T T) T nil t
	:inline-always ((t t) boolean nil nil "equal(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(EQUALP "clLequalp" (T T) T NIL T
	:inline-always ((t t) boolean nil nil "equalp(#0,#1)")
	:inline-always ((fixnum fixnum) boolean nil nil "(#0)==(#1)"))
(NOT "clLnull" (T) T NIL T
	:inline-always ((t) boolean nil nil "(#0)==Cnil"))

; file print.d
(CLEAR-OUTPUT "clLclear_output" (*) T)
(FINISH-OUTPUT "clLforce_output" (*) T)
(FORCE-OUTPUT "clLforce_output" (*) T)
(FRESH-LINE "clLfresh_line" (*) T)
(LISTEN "clLlisten" (*) T)
(PEEK-CHAR "clLpeek_char" (*) T)
(PPRINT "clLpprint" (T *) T)
(PRIN1 "clLprin1" (T *) T NIL NIL
	:inline-always ((t t) t t nil "prin1(#0,#1)")
	:inline-always ((t) t t nil "prin1(#0,Cnil)"))
(PRINC "clLprinc" (T *) T NIL NIL
	:inline-always ((t t) t t nil "princ(#0,#1)")
	:inline-always ((t) t t nil "princ(#0,Cnil)"))
(PRINT "clLprint" (T *) T NIL NIL
	:inline-always ((t t) t t nil "print(#0,#1)")
	:inline-always ((t) t t nil "print(#0,Cnil)"))
(PROBE-FILE "clLprobe_file" (T) T NIL T
	:inline-always ((t) boolean nil nil "(file_exists(#0))"))
(UNREAD-CHAR "clLunread_char" (T *) T)
(READ "clLread" (*) T)
(READ-CHAR "clLread_char" (*) T)
(READ-DELIMITED-LIST "clLread_delimited_list" (T *) T)
(READ-LINE "clLread_line" (*) (VALUES T T))
(READ-PRESERVING-WHITESPACE "clLread_preserving_whitespace" nil T)
(TERPRI "clLterpri" (*) T NIL T
	:inline-always ((t) t t nil "terpri(#0)")
	:inline-always (nil t t nil "terpri(Cnil)"))
(WRITE "clLwrite" (T *) T)
(WRITE-BYTE "clLwrite_byte" (fixnum stream) T)
(si::WRITE-BYTES "clLwrite_bytes"	(stream string fixnum fixnum) T)
(WRITE-CHAR "clLwrite_char" (T *) T NIL NIL
	:inline-always ((t) t t nil "@0;(princ_char(char_code(#0),Cnil),(#0))"))
(WRITE-LINE "clLwrite_line" (T *) T)
(WRITE-STRING "clLwrite_string" (T *) T)
(READ-CHAR-NO-HANG "clLread_char_no_hang" (*) T)
(CLEAR-INPUT "clLclear_input" (*) T)
(PARSE-INTEGER "clLparse_integer" (T *))
(READ-BYTE "clLread_byte" (T *) T)
(si::READ-BYTES "clLread_bytes" (stream string fixnum fixnum) T)
(COPY-READTABLE "clLcopy_readtable" (*) T NIL NIL
	:inline-always ((null null) t nil nil "standard_readtable"))
(READTABLEP "clLreadtablep" (T) T NIL T)
(SET-SYNTAX-FROM-CHAR "clLset_syntax_from_char" (T T *) T)
(SET-MACRO-CHARACTER "clLset_macro_character" (T T *) T)
(GET-MACRO-CHARACTER "clLget_macro_character" (T *) T)
(MAKE-DISPATCH-MACRO-CHARACTER "clLmake_dispatch_macro_character" nil T)
(SET-DISPATCH-MACRO-CHARACTER "clLset_dispatch_macro_character" nil T)
(GET-DISPATCH-MACRO-CHARACTER "clLget_dispatch_macro_character" nil T)
(SI::STRING-TO-OBJECT "siLstring_to_object" (T) T)
(si::STANDARD-READTABLE "siLstandard_readtable" (T) T)
(SYMBOL-FUNCTION "clLsymbol_function" (T) T NIL NIL
	:inline-always ((t) t nil t "symbol_function(#0)"))
(FBOUNDP "clLfboundp" (symbol) T nil t)
(SYMBOL-VALUE "clLsymbol_value" (symbol) T)
(BOUNDP "clLboundp" (symbol) T nil t
	:inline-unsafe ((t) boolean nil nil "(#0)->symbol.dbind!=OBJNULL"))
(MACRO-FUNCTION "clLmacro_function" (symbol) T)
(SPECIAL-FORM-P "clLspecial_form_p" (symbol) T nil t)

; file unixsave.c
(SAVE "clLsave" (T) T)

; file unixsys.c
(si::SYSTEM "siLsystem" nil T)

; file sequence.d
(ELT "clLelt" (sequence fixnum) T nil nil
	:inline-always ((t t) t nil t "elt(#0,fixint(#1))")
	:inline-always ((t fixnum) t nil t "elt(#0,#1)")
	:inline-unsafe ((t t) t nil t "elt(#0,fix(#1))")
	:inline-unsafe ((t fixnum) t nil t "elt(#0,#1)"))
(si::ELT-SET "siLelt_set" (sequence fixnum t) T nil nil
	:inline-always ((t t t) t t nil "elt_set(#0,fixint(#1),#2)")
	:inline-always ((t fixnum t) t t nil "elt_set(#0,#1,#2)")
	:inline-unsafe ((t t t) t t nil "elt_set(#0,fix(#1),#2)"))
(SUBSEQ "clLsubseq" (sequence fixnum *) sequence)
(COPY-SEQ "clLcopy_seq" (sequence) sequence)
(LENGTH "clLlength" (sequence) fixnum t nil
	:inline-always ((t) fixnum nil nil "length(#0)")
	:inline-unsafe (((array t)) fixnum nil nil "(#0)->vector.fillp")
	:inline-unsafe ((string) fixnum nil nil "(#0)->string.fillp"))
(REVERSE "clLreverse" (sequence) sequence nil nil
	:inline-always ((t) t nil t "reverse(#0)"))
(NREVERSE "clLnreverse" (sequence) sequence nil nil
	:inline-always ((t) t t t "nreverse(#0)"))

; file character.d
(CHAR "clLchar" (string fixnum) character nil nil
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
(SCHAR "clLchar" (string fixnum) character nil nil
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
(STRING= "clLstringE" (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_eq(#0,#1)"))
(STRING-EQUAL "clLstring_equal" (string string *) T nil t
	:inline-always ((string  string) boolean nil nil "string_equal(#0,#1)"))
(STRING< "clLstringL" (string string *) T nil t)
(STRING> "clLstringG" (string string *) T nil t)
(STRING<= "clLstringLE" (string string *) T nil t)
(STRING>= "clLstringGE" (string string *) T nil t)
(STRING/= "clLstringNE" (string string *) T nil t)
(STRING-LESSP "clLstring_lessp" (string string *) T nil t)
(STRING-GREATERP "clLstring_greaterp" (string string *) T nil t)
(STRING-NOT-LESSP "clLstring_not_lessp" (string string *) T nil t)
(STRING-NOT-GREATERP "clLstring_not_greaterp" (string string *) T nil t)
(STRING-NOT-EQUAL "clLstring_not_equal" (string string *) T nil t)
(MAKE-STRING "clLmake_string" (fixnum *) string)
(STRING-TRIM "clLstring_trim" (t string) string)
(STRING-LEFT-TRIM "clLstring_left_trim" (t string) string)
(STRING-RIGHT-TRIM "clLstring_right_trim" (t string) string)
(STRING-UPCASE "clLstring_upcase" (string *) string)
(STRING-DOWNCASE "clLstring_downcase" (string *) string)
(STRING-CAPITALIZE "clLstring_capitalize" (string *) string)
(NSTRING-UPCASE "clLnstring_upcase" (string *) string)
(NSTRING-DOWNCASE "clLnstring_downcase" (string *) string)
(NSTRING-CAPITALIZE "clLnstring_capitalize" (string *) string)
(STRING "clLstring" (T) string nil t
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
(si::*MAKE-SPECIAL "siLXmake_special" nil T)
(si::*MAKE-CONSTANT "siLXmake_constant" nil T)

; file symbol.d
(GET "clLget" (symbol t *) T nil nil
	:inline-always ((t t t) t nil nil "get(#0,#1,#2)")
	:inline-always ((t t) t nil nil "get(#0,#1,Cnil)")
	:inline-unsafe ((t t t) t nil nil "getf((#0)->symbol.plist,#1,#2)")
	:inline-unsafe ((t t) t nil nil "getf((#0)->symbol.plist,#1,Cnil)"))
(REMPROP "clLremprop" (symbol t) T nil nil
	:inline-always ((t t) t t nil "remprop(#0,#1)"))
(SYMBOL-PLIST "clLsymbol_plist" (symbol) T nil T
	:inline-always ((t) t nil nil "((#0)->symbol.plist)"))
(GETF "clLgetf" (T T *) T)
(GET-PROPERTIES "clLget_properties" (T T) *)
(SYMBOL-NAME "clLsymbol_name" (symbol) string nil nil
	:inline-always ((symbol) t nil t "((#0)->symbol.name)")
	:inline-always ((t) t nil t "symbol_name(#0)"))
(MAKE-SYMBOL "clLmake_symbol" (string) symbol)
(COPY-SYMBOL "clLcopy_symbol" (symbol *) symbol)
(GENSYM "clLgensym" (*) symbol)
(GENTEMP "clLgentemp" (*) symbol)
(SYMBOL-PACKAGE "clLsymbol_package" (symbol) T)
(KEYWORDP "clLkeywordp" (T) T NIL T
;  :inline-always ((t) boolean nil nil
;        "@0;(type_of(#0)==t_symbol&&(#0)->symbol.hpack==keyword_package)")
 )
(SI::PUT-F "siLput_f" NIL (T T))
(SI::REM-F "siLrem_f" NIL (T T))
(si::SET-SYMBOL-PLIST "siLset_symbol_plist" (symbol t) T)
(SI::PUTPROP "siLputprop" (T T T) T NIL NIL
	:inline-always ((t t t) t t nil "putprop(#0,#1,#2)"))

; file tcp.c
(si::OPEN-TCP-STREAM "clLopen_tcp_stream" (T T) T)

; file unixfasl.c
(si::READ-EXTERNALS "siLread_externals" nil T)
(si::SET-UP-COMBINED "siLset_up_combined" nil T)
(si::BUILD-SYMBOL-TABLE "siLbuild_symbol_table" nil T)
#+bsd
(si::FASLINK "siLfaslink" nil T)

; file unixtime.c
(si::DAYLIGHT-SAVING-TIME-P "clLdaylight_saving_timep" nil T nil t)
(GET-UNIVERSAL-TIME "clLget_universal_time" nil T)
(GET-INTERNAL-RUN-TIME "clLget_internal_run_time" nil T)
(GET-INTERNAL-REAL-TIME "clLget_internal_real_time" nil T)
(si::GET-LOCAL-TIME-ZONE "clLget_local_time_zone" nil T)
(SLEEP "clLsleep" (real) T)

(TYPE-OF "clLtype_of" (T) T NIL NIL
	:inline-always ((t) t nil t "TYPE_OF(#0)"))

;;; Beppe's additions
(READ-BYTES "clLread_bytes" (stream string fixnum fixnum) T)
(WRITE-BYTES "clLwrite_bytes" (stream string fixnum fixnum) T)

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

