#define _ARGS(x) (int n, ...)

#include "ecl.h"
#include "page.h"

#define form 2
#define cl 0
#define si 1

const struct function_info all_functions[] = {

	/* alloc.c */

#if !defined(GBC_BOEHM)
	{"ALLOCATE", siLallocate, si},
	{"ALLOCATED-PAGES", siLallocated_pages, si},
	{"MAXIMUM-ALLOCATABLE-PAGES", siLmaxpage, si},
	{"ALLOCATE-CONTIGUOUS-PAGES", siLalloc_contpage, si},
	{"ALLOCATED-CONTIGUOUS-PAGES", siLncbpage, si},
	{"MAXIMUM-CONTIGUOUS-PAGES", siLmaxcbpage, si},
	{"GET-HOLE-SIZE", siLget_hole_size, si},
	{"SET-HOLE-SIZE", siLset_hole_size, si},
	{"IGNORE-MAXIMUM-PAGES", siLignore_maximum_pages, si},
#endif /* !GBC_BOEHM */

	/* alloc_2.c */

#ifdef GBC_BOEHM
	{"GC", clLgc, cl},
#endif

	/* all_symbols.c */

	{"MANGLE-NAME", siLmangle_name, si},

	/* array.c */

	{"MAKE-PURE-ARRAY", siLmake_pure_array, si},
	{"MAKE-VECTOR", siLmake_vector, si},
	{"AREF", clLaref, cl},
	{"ASET", siLaset, si},
	{"ARRAY-ELEMENT-TYPE", clLarray_element_type, cl},
	{"ARRAY-RANK", clLarray_rank, cl},
	{"ARRAY-DIMENSION", clLarray_dimension, cl},
	{"ARRAY-TOTAL-SIZE", clLarray_total_size, cl},
	{"ADJUSTABLE-ARRAY-P", clLadjustable_array_p, cl},
	{"DISPLACED-ARRAY-P", siLdisplaced_array_p, si},
	{"ROW-MAJOR-AREF", clLrow_major_aref, cl},
	{"ROW-MAJOR-ASET", siLrow_major_aset, si},

	{"SVREF", clLsvref, cl},
	{"SVSET", siLsvset, si},

	{"ARRAY-HAS-FILL-POINTER-P", clLarray_has_fill_pointer_p, cl},
	{"FILL-POINTER", clLfill_pointer, cl},
	{"FILL-POINTER-SET", siLfill_pointer_set, si},

	{"REPLACE-ARRAY", siLreplace_array, si},

	/* assignment.c */

	{"CLEAR-COMPILER-PROPERTIES", siLclear_compiler_properties, si},
	{"SETQ", NULL, form},
	{"PSETQ", NULL, form},
	{"SET", clLset, cl},
	{"FSET", siLfset, si},
	{"MULTIPLE-VALUE-SETQ", NULL, form},
	{"MAKUNBOUND", clLmakunbound, cl},
	{"FMAKUNBOUND", clLfmakunbound, cl},
#if 0
	{"SETF", NULL, form},
	{"PUSH", NULL, form},
	{"POP", NULL, form},
	{"INCF", NULL, form},
	{"DECF", NULL, form},
#endif
	{"SETF-NAMEP", siLsetf_namep, si},

	/* block.c */

	{"BLOCK", NULL, form},
	{"RETURN-FROM", NULL, form},
	{"RETURN", NULL, form},

	/* catch.c */

	{"CATCH", NULL, form},
	{"UNWIND-PROTECT", NULL, form},
	{"THROW", NULL, form},

	/* cfun.c */

	{"COMPILED-FUNCTION-NAME", siLcompiled_function_name, si},
	{"COMPILED-FUNCTION-BLOCK", siLcompiled_function_block, si},
	{"COMPILED-FUNCTION-SOURCE", siLcompiled_function_source, si},

	/* character.d */

	{"STANDARD-CHAR-P", clLstandard_char_p, cl},
	{"GRAPHIC-CHAR-P", clLgraphic_char_p, cl},
	{"ALPHA-CHAR-P", clLalpha_char_p, cl},
	{"UPPER-CASE-P", clLupper_case_p, cl},
	{"LOWER-CASE-P", clLlower_case_p, cl},
	{"BOTH-CASE-P", clLboth_case_p, cl},
	{"DIGIT-CHAR-P", clLdigit_char_p, cl},
	{"ALPHANUMERICP", clLalphanumericp, cl},
	{"CHAR=", clLcharE, cl},
	{"CHAR/=", clLcharNE, cl},
	{"CHAR<", clLcharL, cl},
	{"CHAR>", clLcharG, cl},
	{"CHAR<=", clLcharLE, cl},
	{"CHAR>=", clLcharGE, cl},
	{"CHAR-EQUAL", clLchar_equal, cl},
	{"CHAR-NOT-EQUAL", clLchar_not_equal, cl},
	{"CHAR-LESSP", clLchar_lessp, cl},
	{"CHAR-GREATERP", clLchar_greaterp, cl},
	{"CHAR-NOT-GREATERP", clLchar_not_greaterp, cl},
	{"CHAR-NOT-LESSP", clLchar_not_lessp, cl},
	{"CHARACTER", clLcharacter, cl},
	{"CHAR-CODE", clLchar_code, cl},
	{"CODE-CHAR", clLcode_char, cl},
	{"CHAR-UPCASE", clLchar_upcase, cl},
	{"CHAR-DOWNCASE", clLchar_downcase, cl},
	{"DIGIT-CHAR", clLdigit_char, cl},
	{"CHAR-INT", clLchar_int, cl},
	{"INT-CHAR", clLint_char, cl},
	{"CHAR-NAME", clLchar_name, cl},
	{"NAME-CHAR", clLname_char, cl},

	/* cmpaux.c */

	{"SPECIALP", siLspecialp, si},

	/* conditional.c */

	{"IF", NULL, form},
	{"COND", NULL, form},
	{"CASE", NULL, form},
	{"WHEN", NULL, form},
	{"UNLESS", NULL, form},

	/* disassembler.c */
	{"BC-DISASSEMBLE", siLbc_disassemble, si},
	{"BC-SPLIT", siLbc_split, si},

	/* error.c */

	{"ERROR", clLerror, cl},
	{"CERROR", clLcerror, cl},

	/* eval.c */

	{"EVAL", clLeval, cl},
	{"EVAL-WITH-ENV", siLeval_with_env, si},
	{"CONSTANTP", clLconstantp, cl},
	{"UNLINK-SYMBOL", siLunlink_symbol, si},
	{"APPLY", clLapply, cl},
	{"FUNCALL", clLfuncall, cl},

	/* file.d */

	{"MAKE-SYNONYM-STREAM", clLmake_synonym_stream, cl},
	{"MAKE-BROADCAST-STREAM", clLmake_broadcast_stream, cl},
	{"MAKE-CONCATENATED-STREAM", clLmake_concatenated_stream, cl},
	{"MAKE-TWO-WAY-STREAM", clLmake_two_way_stream, cl},
	{"MAKE-ECHO-STREAM", clLmake_echo_stream, cl},
	{"MAKE-STRING-INPUT-STREAM", clLmake_string_input_stream, cl},
	{"MAKE-STRING-OUTPUT-STREAM", clLmake_string_output_stream, cl},
	{"GET-OUTPUT-STREAM-STRING", clLget_output_stream_string, cl},

	{"OUTPUT-STREAM-STRING", siLoutput_stream_string, si},

	{"STREAMP", clLstreamp, cl},
	{"INPUT-STREAM-P", clLinput_stream_p, cl},
	{"OUTPUT-STREAM-P", clLoutput_stream_p, cl},
	{"STREAM-ELEMENT-TYPE", clLstream_element_type, cl},
	{"CLOSE", clLclose, cl},
	{"OPEN", clLopen, cl},
	{"FILE-POSITION", clLfile_position, cl},
	{"FILE-LENGTH", clLfile_length, cl},
	{"OPEN-STREAM-P", clLopen_stream_p, cl},
	{"GET-STRING-INPUT-STREAM-INDEX", siLget_string_input_stream_index, si},
	{"MAKE-STRING-OUTPUT-STREAM-FROM-STRING", siLmake_string_output_stream_from_string, si},
	{"COPY-STREAM", siLcopy_stream, si},

	/* format. c */

	{"FORMAT", clLformat, cl},

	/* gbc.c */

#if !defined(GBC_BOEHM)
	{"ROOM-REPORT", siLroom_report, si},
	{"RESET-GC-COUNT", siLreset_gc_count, si},
	{"GC", clLgc, cl},
	{"GC-TIME", siLgc_time, si},
#endif

	/* gfun.c */
#ifdef CLOS
	{"ALLOCATE-GFUN", siLallocate_gfun, si},
	{"GFUN-NAME", siLgfun_name, si},
	{"GFUN-NAME-SET", siLgfun_name_set, si},
	{"GFUN-METHOD-HT", siLgfun_method_ht, si},
	{"GFUN-METHOD-HT-SET", siLgfun_method_ht_set, si},
	{"GFUN-SPEC-HOW-REF", siLgfun_spec_how_ref, si},
	{"GFUN-SPEC-HOW-SET", siLgfun_spec_how_set, si},
	{"GFUN-INSTANCE", siLgfun_instance, si},
	{"GFUN-INSTANCE-SET", siLgfun_instance_set, si},
	{"GFUNP", siLgfunp, si},
	{"METHOD-HT-GET", siLmethod_ht_get, si},
	{"SET-COMPILED-FUNCTION-NAME", siLset_compiled_function_name, si},
#endif CLOS

	/* hash.d */

	{"MAKE-HASH-TABLE", clLmake_hash_table, cl},
	{"HASH-TABLE-P", clLhash_table_p, cl},
	{"GETHASH", clLgethash, cl},
	{"REMHASH", clLremhash, cl},
   	{"MAPHASH", clLmaphash, cl},
	{"CLRHASH", clLclrhash, cl},
	{"HASH-TABLE-COUNT", clLhash_table_count, cl},
   	{"SXHASH", clLsxhash, cl},
	{"HASH-SET", siLhash_set, si},
	{"HASH-TABLE-REHASH-SIZE", clLhash_table_rehash_size, cl},
	{"HASH-TABLE-REHASH-THRESHOLD", clLhash_table_rehash_threshold, cl},

	/* instance.c */
#ifdef CLOS
	{"ALLOCATE-INSTANCE", siLallocate_instance, si},
	{"CHANGE-INSTANCE", siLchange_instance, si},
	{"INSTANCE-REF-SAFE", siLinstance_ref_safe, si},
	{"INSTANCE-REF", siLinstance_ref, si},
	{"INSTANCE-SET", siLinstance_set, si},
	{"INSTANCE-CLASS", siLinstance_class, si},
	{"INSTANCE-CLASS-SET", siLinstance_class_set, si},
	{"INSTANCEP", siLinstancep, si},
	{"SL-BOUNDP", siLsl_boundp, si},
	{"SL-MAKUNBOUND", siLsl_makunbound, si},
#endif CLOS

	/* interpreter.c */
	{"INTERPRETER-STACK", siLinterpreter_stack, si},
	{"MAKE-LAMBDA", siLmake_lambda, si},
	{"FUNCTION-BLOCK-NAME", siLfunction_block_name, si},

	/* iteration.c */

	{"DO", NULL, form},
	{"DO*", NULL, form},
	{"DOLIST", NULL, form},
	{"DOTIMES", NULL, form},

	/* let.c */

	{"LET", NULL, form},
	{"LET*", NULL, form},
	{"MULTIPLE-VALUE-BIND", NULL, form},
	{"COMPILER-LET", NULL, form},
	{"FLET", NULL, form},
	{"LABELS", NULL, form},
	{"MACROLET", NULL, form},
	{"SYMBOL-MACROLET", NULL, form},

	/* list.d */

	{"CAR", clLcar, cl},
	{"CDR", clLcdr, cl},
	{"CAAR", clLcaar, cl},
	{"CADR", clLcadr, cl},
	{"CDAR", clLcdar, cl},
	{"CDDR", clLcddr, cl},
	{"CAAAR", clLcaaar, cl},
	{"CAADR", clLcaadr, cl},
	{"CADAR", clLcadar, cl},
	{"CADDR", clLcaddr, cl},
	{"CDAAR", clLcdaar, cl},
	{"CDADR", clLcdadr, cl},
	{"CDDAR", clLcddar, cl},
	{"CDDDR", clLcdddr, cl},
	{"CAAAAR", clLcaaaar, cl},
	{"CAAADR", clLcaaadr, cl},
	{"CAADAR", clLcaadar, cl},
	{"CAADDR", clLcaaddr, cl},
	{"CADAAR", clLcadaar, cl},
	{"CADADR", clLcadadr, cl},
	{"CADDAR", clLcaddar, cl},
	{"CADDDR", clLcadddr, cl},
	{"CDAAAR", clLcdaaar, cl},
	{"CDAADR", clLcdaadr, cl},
	{"CDADAR", clLcdadar, cl},
	{"CDADDR", clLcdaddr, cl},
	{"CDDAAR", clLcddaar, cl},
	{"CDDADR", clLcddadr, cl},
	{"CDDDAR", clLcdddar, cl},
	{"CDDDDR", clLcddddr, cl},

	{"CONS", clLcons, cl},
	{"TREE-EQUAL", clLtree_equal, cl},
	{"ENDP", clLendp, cl},
	{"LIST-LENGTH", clLlist_length, cl},
	{"NTH", clLnth, cl},

	{"FIRST", clLcar, cl},
	{"SECOND", clLcadr, cl},
	{"THIRD", clLcaddr, cl},
	{"FOURTH", clLcadddr, cl},
	{"FIFTH", clLfifth, cl},
	{"SIXTH", clLsixth, cl},
	{"SEVENTH", clLseventh, cl},
	{"EIGHTH", clLeighth, cl},
	{"NINTH", clLninth, cl},
	{"TENTH", clLtenth, cl},

	{"REST", clLcdr, cl},
	{"NTHCDR", clLnthcdr, cl},
	{"LAST", clLlast, cl},
	{"LIST", clLlist, cl},
	{"LIST*", clLlistX, cl},
	{"MAKE-LIST", clLmake_list, cl},
	{"APPEND", clLappend, cl},
	{"COPY-LIST", clLcopy_list, cl},
	{"COPY-ALIST", clLcopy_alist, cl},
	{"COPY-TREE", clLcopy_tree, cl},
	{"REVAPPEND", clLrevappend, cl},
	{"NCONC", clLnconc, cl},
	{"NRECONC", clLnreconc, cl},

	{"BUTLAST", clLbutlast, cl},
	{"NBUTLAST", clLnbutlast, cl},
	{"LDIFF", clLldiff, cl},
	{"RPLACA", clLrplaca, cl},
	{"RPLACD", clLrplacd, cl},
	{"SUBST", clLsubst, cl},
	{"SUBST-IF", clLsubst_if, cl},
	{"SUBST-IF-NOT", clLsubst_if_not, cl},
	{"NSUBST", clLnsubst, cl},
	{"NSUBST-IF", clLnsubst_if, cl},
	{"NSUBST-IF-NOT", clLnsubst_if_not, cl},
	{"SUBLIS", clLsublis, cl},
	{"NSUBLIS", clLnsublis, cl},
	{"MEMBER", clLmember, cl},
	{"MEMBER-IF", clLmember_if, cl},
	{"MEMBER-IF-NOT", clLmember_if_not, cl},
	{"MEMBER1", siLmember1, si},
	{"TAILP", clLtailp, cl},
	{"ADJOIN", clLadjoin, cl},

	{"ACONS", clLacons, cl},
	{"PAIRLIS", clLpairlis, cl},
	{"ASSOC", clLassoc, cl},
	{"ASSOC-IF", clLassoc_if, cl},
	{"ASSOC-IF-NOT", clLassoc_if_not, cl},
	{"RASSOC", clLrassoc, cl},
	{"RASSOC-IF", clLrassoc_if, cl},
	{"RASSOC-IF-NOT", clLrassoc_if_not, cl},

	{"MEMQ", siLmemq, si},

	/* load.d */

	{"LOAD", clLload, cl},
#ifdef ENABLE_DLOPEN
	{"LOAD-BINARY", siLload_binary, si},
#endif
	{"LOAD-SOURCE", siLload_source, si},

	/* lwp.d */
#ifdef THREADS
	{"THREAD-BREAK-IN", siLthread_break_in, si},
	{"THREAD-BREAK-QUIT", siLthread_break_quit, si},
	{"THREAD-BREAK-RESUME", siLthread_break_resume, si},
	{"MAKE-THREAD", clLmake_thread, cl},
	{"DEACTIVATE", clLdeactivate, cl},
	{"REACTIVATE", clLreactivate, cl},
	{"KILL-THREAD", clLkill_thread, cl},
	{"CURRENT-THREAD", clLcurrent_thread, cl},
	{"THREAD-STATUS", clLthread_status, cl},
	{"THREAD-LIST", clLthread_list, cl},
	{"MAKE-CONTINUATION", clLmake_continuation, cl},
	{"THREAD-OF", clLthread_of, cl},
	{"CONTINUATION-OF", clLcontinuation_of, cl},
	{"RESUME", clLresume, cl},

	{"%DISABLE-SCHEDULER", clLdisable_scheduler, cl},
	{"%ENABLE-SCHEDULER", clLenable_scheduler, cl},
	{"%SUSPEND", clLsuspend, cl},
	{"%DELAY", clLdelay, cl},
	{"%THREAD-WAIT", clLthread_wait, cl},
	{"%THREAD-WAIT-WITH-TIMEOUT", clLthread_wait_with_timeout, cl},
#endif THREADS

	/* macros.c */

	{"MACROEXPAND", clLmacroexpand, cl},
	{"MACROEXPAND-1", clLmacroexpand_1, cl},

	/* main.c */

	{"QUIT", clLquit, cl},
	{"ARGC", siLargc, si},
	{"ARGV", siLargv, si},
	{"GETENV", siLgetenv, si},
	{"POINTER", siLpointer, si},

	/* mapfun.c */

	{"MAPCAR", clLmapcar, cl},
	{"MAPLIST", clLmaplist, cl},
	{"MAPC", clLmapc, cl},
	{"MAPL", clLmapl, cl},
	{"MAPCAN", clLmapcan, cl},
	{"MAPCON", clLmapcon, cl},

	/* multival.c */

	{"VALUES", clLvalues, cl},
	{"VALUES-LIST", clLvalues_list, cl},
	{"MULTIPLE-VALUE-CALL", NULL, form},
	{"MULTIPLE-VALUE-PROG1", NULL, form},
	{"MULTIPLE-VALUE-LIST", NULL, form},
	{"NTH-VALUE", NULL, form},


	/* num-arith.c */

	{"+", clLP, cl},
	{"-", clLM, cl},
	{"*", clLX, cl},
	{"/", clLN, cl},
	{"1+", clL1P, cl},
	{"1-", clL1M, cl},
	{"CONJUGATE", clLconjugate, cl},
	{"GCD", clLgcd, cl},
	{"LCM", clLlcm, cl},


	/* num_co.c */

	{"FLOAT", clLfloat, cl},
	{"NUMERATOR", clLnumerator, cl},
	{"DENOMINATOR", clLdenominator, cl},
	{"FLOOR", clLfloor, cl},
	{"CEILING", clLceiling, cl},
	{"TRUNCATE", clLtruncate, cl},
	{"ROUND", clLround, cl},
	{"MOD", clLmod, cl},
	{"REM", clLrem, cl},
	{"DECODE-FLOAT", clLdecode_float, cl},
	{"SCALE-FLOAT", clLscale_float, cl},
	{"FLOAT-RADIX", clLfloat_radix, cl},
	{"FLOAT-SIGN", clLfloat_sign, cl},
	{"FLOAT-DIGITS", clLfloat_digits, cl},
	{"FLOAT-PRECISION", clLfloat_precision, cl},
	{"INTEGER-DECODE-FLOAT", clLinteger_decode_float, cl},
	{"COMPLEX", clLcomplex, cl},
	{"REALPART", clLrealpart, cl},
	{"IMAGPART", clLimagpart, cl},

	/* num_comp.c */

	{"=", clLE, cl},
	{"/=", clLNE, cl},
	{"<", clLL, cl},
	{">", clLG, cl},
	{"<=", clLLE, cl},
	{">=", clLGE, cl},
	{"MAX", clLmax, cl},
	{"MIN", clLmin, cl},

	/* num_log.c */

	{"LOGIOR", clLlogior, cl},
	{"LOGXOR", clLlogxor, cl},
	{"LOGAND", clLlogand, cl},
	{"LOGEQV", clLlogeqv, cl},
	{"LOGNAND", clLlognand, cl},
	{"LOGNOR", clLlognor, cl},
	{"LOGANDC1", clLlogandc1, cl},
	{"LOGANDC2", clLlogandc1, cl},
	{"LOGORC1", clLlogorc1, cl},
	{"LOGORC2", clLlogorc2, cl},
	{"LOGNOT", clLlognot, cl},
	{"BOOLE", clLboole, cl},
	{"LOGBITP", clLlogbitp, cl},
	{"ASH", clLash, cl},
	{"LOGCOUNT", clLlogcount, cl},
	{"INTEGER-LENGTH", clLinteger_length, cl},
	{"BIT-ARRAY-OP", siLbit_array_op, si},

	/* num_pred.c */

	{"ZEROP", clLzerop, cl},
	{"PLUSP", clLplusp, cl},
	{"MINUSP", clLminusp, cl},
	{"ODDP", clLoddp, cl},
	{"EVENP", clLevenp, cl},
	{"NANI", siLnani, si},

	/* num_rand.c */

	{"RANDOM", clLrandom, cl},
	{"MAKE-RANDOM-STATE", clLmake_random_state, cl},
	{"RANDOM-STATE-P", clLrandom_state_p, cl},

	/* num_sfun.c */

	{"EXP", clLexp, cl},
	{"EXPT", clLexpt, cl},
	{"LOG", clLlog, cl},
	{"SQRT", clLsqrt, cl},
	{"SIN", clLsin, cl},
	{"COS", clLcos, cl},
	{"TAN", clLtan, cl},
	{"ATAN", clLatan, cl},
	{"SINH", clLsinh, cl},
	{"COSH", clLcosh, cl},
	{"TANH", clLtanh, cl},

	/* package.d */

	{"MAKE-PACKAGE", clLmake_package, cl},
	{"SELECT-PACKAGE", siLselect_package, si},
	{"FIND-PACKAGE", clLfind_package, cl},
	{"PACKAGE-NAME", clLpackage_name, cl},
	{"PACKAGE-NICKNAMES", clLpackage_nicknames, cl},
	{"RENAME-PACKAGE", clLrename_package, cl},
	{"PACKAGE-USE-LIST", clLpackage_use_list, cl},
	{"PACKAGE-USED-BY-LIST", clLpackage_used_by_list, cl},
	{"PACKAGE-SHADOWING-SYMBOLS", clLpackage_shadowing_symbols, cl},
	{"LIST-ALL-PACKAGES", clLlist_all_packages, cl},
	{"INTERN", clLintern, cl},
	{"FIND-SYMBOL", clLfind_symbol, cl},
	{"UNINTERN", clLunintern, cl},
	{"EXPORT", clLexport, cl},
	{"UNEXPORT", clLunexport, cl},
	{"IMPORT", clLimport, cl},
	{"SHADOWING-IMPORT", clLshadowing_import, cl},
	{"SHADOW", clLshadow, cl},
	{"USE-PACKAGE", clLuse_package, cl},
	{"UNUSE-PACKAGE", clLunuse_package, cl},
	{"DELETE-PACKAGE", clLdelete_package, cl},

	{"PACKAGE-SIZE", siLpackage_size, si},
	{"PACKAGE-INTERNAL", siLpackage_internal, si},
	{"PACKAGE-EXTERNAL", siLpackage_external, si},
	{"PACKAGE-LOCK", siLpackage_lock, si},

	/* pathname.d */

	{"PATHNAME", clLpathname, cl},
	{"PARSE-NAMESTRING", clLparse_namestring, cl},
	{"MERGE-PATHNAMES", clLmerge_pathnames, cl},
	{"MAKE-PATHNAME", clLmake_pathname, cl},
	{"PATHNAMEP", clLpathnamep, cl},
	{"PATHNAME-HOST", clLpathname_host, cl},
	{"PATHNAME-DEVICE", clLpathname_device, cl},
	{"PATHNAME-DIRECTORY", clLpathname_directory, cl},
	{"PATHNAME-NAME", clLpathname_name, cl},
	{"PATHNAME-TYPE", clLpathname_type, cl},
	{"PATHNAME-VERSION", clLpathname_version, cl},
	{"NAMESTRING", clLnamestring, cl},
	{"FILE-NAMESTRING", clLfile_namestring, cl},
	{"DIRECTORY-NAMESTRING", clLdirectory_namestring, cl},
	{"HOST-NAMESTRING", clLhost_namestring, cl},
	{"ENOUGH-NAMESTRING", clLenough_namestring, cl},
	{"LOGICAL-PATHNAME-P", siLlogical_pathname_p, si},
	{"PATHNAME-MATCH-P", clLpathname_match_p, cl},
	{"TRANSLATE-PATHNAME", clLtranslate_pathname, cl},
	{"TRANSLATE-LOGICAL-PATHNAME", clLtranslate_logical_pathname, cl},
	{"PATHNAME-TRANSLATIONS", siLpathname_translations, si},

	/* predicate.c */

	{"IDENTITY", clLidentity, cl},
	{"NULL", clLnull, cl},
	{"SYMBOLP", clLsymbolp, cl},
	{"ATOM", clLatom, cl},
	{"CONSP", clLconsp, cl},
	{"LISTP", clLlistp, cl},
	{"NUMBERP", clLnumberp, cl},
	{"INTEGERP", clLintegerp, cl},
	{"RATIONALP", clLrationalp, cl},
	{"FLOATP", clLfloatp, cl},
	{"REALP", clLrealp, cl},
	{"COMPLEXP", clLcomplexp, cl},
	{"CHARACTERP", clLcharacterp, cl},
	{"STRINGP", clLstringp, cl},
	{"BIT-VECTOR-P", clLbit_vector_p, cl},
	{"VECTORP", clLvectorp, cl},
	{"SIMPLE-STRING-P", clLsimple_string_p, cl},
	{"SIMPLE-BIT-VECTOR-P", clLsimple_bit_vector_p, cl},
	{"SIMPLE-VECTOR-P", clLsimple_vector_p, cl},
	{"ARRAYP", clLarrayp, cl},
	{"PACKAGEP", clLpackagep, cl},
	{"FUNCTIONP", clLfunctionp, cl},
	{"COMPILED-FUNCTION-P", clLcompiled_function_p, cl},
	{"COMMONP", clLcommonp, cl},

	{"EQ", clLeq, cl},
	{"EQL", clLeql, cl},
	{"EQUAL", clLequal, cl},
	{"EQUALP", clLequalp, cl},

	{"NOT", clLnull, cl},

	{"FIXNUMP", siLfixnump, si},

	/* print.d */

	{"WRITE", clLwrite, cl},
	{"PRIN1", clLprin1, cl},
	{"PRINT", clLprint, cl},
	{"PPRINT", clLpprint, cl},
	{"PRINC", clLprinc, cl},
	{"WRITE-CHAR", clLwrite_char, cl},
	{"WRITE-STRING", clLwrite_string, cl},
	{"WRITE-LINE", clLwrite_line, cl},
	{"WRITE-BYTE", clLwrite_byte, cl},
	{"WRITE-BYTES", siLwrite_bytes, si},
	{"TERPRI", clLterpri, cl},
	{"FRESH-LINE", clLfresh_line, cl},
	{"FINISH-OUTPUT", clLforce_output, cl},
	{"FORCE-OUTPUT", clLforce_output, cl},
	{"CLEAR-OUTPUT", clLclear_output, cl},

	/* profile.c */
#ifdef PROFILE
	{"PROFILE", siLprofile, si},
	{"CLEAR-PROFILE", siLclear_profile, si},
	{"DISPLAY-PROFILE", siLdisplay_profile, si},
#endif PROFILE

	/* prog.c */

	{"TAGBODY", NULL, form},
	{"PROG", NULL, form},
	{"PROG*", NULL, form},
	{"GO", NULL, form},
	{"PROGV", NULL, form},
	{"PROGN", NULL, form},
	{"PROG1", NULL, form},
	{"PROG2", NULL, form},

	/* read.d */

	{"READ", clLread, cl},
	{"READ-PRESERVING-WHITESPACE", clLread_preserving_whitespace, cl},
	{"READ-DELIMITED-LIST", clLread_delimited_list, cl},
	{"READ-LINE", clLread_line, cl},
	{"READ-CHAR", clLread_char, cl},
	{"UNREAD-CHAR", clLunread_char, cl},
	{"PEEK-CHAR", clLpeek_char, cl},
	{"LISTEN", clLlisten, cl},
	{"READ-CHAR-NO-HANG", clLread_char_no_hang, cl},
	{"CLEAR-INPUT", clLclear_input, cl},

	{"PARSE-INTEGER", clLparse_integer, cl},

	{"READ-BYTE", clLread_byte, cl},
	{"READ-BYTES", siLread_bytes, si},

	{"COPY-READTABLE", clLcopy_readtable, cl},
	{"READTABLEP", clLreadtablep, cl},
	{"SET-SYNTAX-FROM-CHAR", clLset_syntax_from_char, cl},
	{"SET-MACRO-CHARACTER", clLset_macro_character, cl},
	{"GET-MACRO-CHARACTER", clLget_macro_character, cl},
	{"MAKE-DISPATCH-MACRO-CHARACTER", clLmake_dispatch_macro_character, cl},
	{"SET-DISPATCH-MACRO-CHARACTER", clLset_dispatch_macro_character, cl},
	{"GET-DISPATCH-MACRO-CHARACTER", clLget_dispatch_macro_character, cl},
	{"STRING-TO-OBJECT", siLstring_to_object, si},
	{"STANDARD-READTABLE", siLstandard_readtable, si},

	/* reference.c */

	{"SYMBOL-FUNCTION", clLsymbol_function, cl},
	{"FBOUNDP", clLfboundp, cl},
	{"QUOTE", NULL, form},
	{"SYMBOL-VALUE", clLsymbol_value, cl},
	{"BOUNDP", clLboundp, cl},
	{"MACRO-FUNCTION", clLmacro_function, cl},
	{"SPECIAL-FORM-P", clLspecial_form_p, cl},
	{"COERCE-TO-FUNCTION", siLcoerce_to_function, si},
	{"FUNCTION", NULL, form},
	{"PROCESS-DECLARATIONS", siLprocess_declarations, si},
	{"PROCESS-LAMBDA-LIST", siLprocess_lambda_list, si},

	/* sequence.d */

	{"ELT", clLelt, cl},
	{"ELT-SET", siLelt_set, si},
	{"SUBSEQ", clLsubseq, cl},
	{"COPY-SEQ", clLcopy_seq, cl},
	{"LENGTH", clLlength, cl},
	{"REVERSE", clLreverse, cl},
	{"NREVERSE", clLnreverse, cl},

	/* stacks.c */

	{"IHS-TOP", siLihs_top, si},
	{"IHS-FUN", siLihs_fun, si},
	{"IHS-ENV", siLihs_env, si},
	{"IHS-NEXT", siLihs_next, si},
	{"IHS-PREV", siLihs_prev, si},
	{"FRS-TOP", siLfrs_top, si},
	{"FRS-BDS", siLfrs_bds, si},
	{"FRS-CLASS", siLfrs_class, si},
	{"FRS-TAG", siLfrs_tag, si},
	{"FRS-IHS", siLfrs_ihs, si},
	{"BDS-TOP", siLbds_top, si},
	{"BDS-VAR", siLbds_var, si},
	{"BDS-VAL", siLbds_val, si},
	{"SCH-FRS-BASE", siLsch_frs_base, si},
	{"RESET-STACK-LIMITS", siLreset_stack_limits, si},

	/* string.d */

	{"CHAR", clLchar, cl},
	{"CHAR-SET", siLchar_set, si},
	{"SCHAR", clLchar, cl},
	{"SCHAR-SET", siLchar_set, si},
	{"STRING=", clLstringE, cl},
	{"STRING-EQUAL", clLstring_equal, cl},
	{"STRING<", clLstringL, cl},
	{"STRING>", clLstringG, cl},
	{"STRING<=", clLstringLE, cl},
	{"STRING>=", clLstringGE, cl},
	{"STRING/=", clLstringNE, cl},
	{"STRING-LESSP", clLstring_lessp, cl},
	{"STRING-GREATERP", clLstring_greaterp, cl},
	{"STRING-NOT-LESSP", clLstring_not_lessp, cl},
	{"STRING-NOT-GREATERP", clLstring_not_greaterp, cl},
	{"STRING-NOT-EQUAL", clLstring_not_equal, cl},
	{"MAKE-STRING", clLmake_string, cl},
	{"STRING-TRIM", clLstring_trim, cl},
	{"STRING-LEFT-TRIM", clLstring_left_trim, cl},
	{"STRING-RIGHT-TRIM", clLstring_right_trim, cl},
	{"STRING-UPCASE", clLstring_upcase, cl},
	{"STRING-DOWNCASE", clLstring_downcase, cl},
	{"STRING-CAPITALIZE", clLstring_capitalize, cl},
	{"NSTRING-UPCASE", clLnstring_upcase, cl},
	{"NSTRING-DOWNCASE", clLnstring_downcase, cl},
	{"NSTRING-CAPITALIZE", clLnstring_capitalize, cl},
	{"STRING", clLstring, cl},
	{"STRING-CONCATENATE", siLstring_concatenate, si},

	/* structure.c */

	{"MAKE-STRUCTURE", siLmake_structure, si},
	{"COPY-STRUCTURE", siLcopy_structure, si},
	{"STRUCTURE-NAME", siLstructure_name, si},
	{"STRUCTURE-REF", siLstructure_ref, si},
	{"STRUCTURE-SET", siLstructure_set, si},
	{"STRUCTUREP", siLstructurep, si},
	{"STRUCTURE-SUBTYPE-P", siLstructure_subtype_p, si},
	{"RPLACA-NTHCDR", siLrplaca_nthcdr, si},
	{"LIST-NTH", siLlist_nth, si},

	/* symbol.d */

	{"GET", clLget, cl},
	{"REMPROP", clLremprop, cl},
	{"SYMBOL-PLIST", clLsymbol_plist, cl},
	{"GETF", clLgetf, cl},
	{"GET-PROPERTIES", clLget_properties, cl},
	{"SYMBOL-NAME", clLsymbol_name, cl},
	{"MAKE-SYMBOL", clLmake_symbol, cl},
	{"COPY-SYMBOL", clLcopy_symbol, cl},
	{"GENSYM", clLgensym, cl},
	{"GENTEMP", clLgentemp, cl},
	{"SYMBOL-PACKAGE", clLsymbol_package, cl},
	{"KEYWORDP", clLkeywordp, cl},
	{"PUT-F", siLput_f, si},
	{"REM-F", siLrem_f, si},
	{"SET-SYMBOL-PLIST", siLset_symbol_plist, si},
	{"PUTPROP", siLputprop, si},
	{"PUT-PROPERTIES", siLput_properties, si},

	/* tcp.c */
#ifdef TCP
	{"OPEN-CLIENT-STREAM", siLopen_client_stream, si},
	{"OPEN-SERVER-STREAM", siLopen_server_stream, si},
#endif

	/* time.c */

	{"GET-UNIVERSAL-TIME", clLget_universal_time, cl},
	{"SLEEP", clLsleep, cl},
	{"GET-INTERNAL-RUN-TIME", clLget_internal_run_time, cl},
	{"GET-INTERNAL-REAL-TIME", clLget_internal_real_time, cl},
	{"GET-LOCAL-TIME-ZONE", siLget_local_time_zone, si},
	{"DAYLIGHT-SAVING-TIME-P", siLdaylight_saving_time_p, si},

	/* toplevel.c */

	{"LAMBDA", NULL, form},
	{"NAMED-LAMBDA", NULL, form},
	{"*MAKE-SPECIAL", siLXmake_special, si},
	{"*MAKE-CONSTANT", siLXmake_constant, si},
	{"EVAL-WHEN", NULL, form},
	{"THE", NULL, form},
	{"DECLARE", NULL, form},
	{"LOCALLY", NULL, form},

	/* typespec.c */

	{"TYPE-OF", clLtype_of, cl},

	/* unixint.c */

#ifdef unix
	{"CATCH-BAD-SIGNALS", siLcatch_bad_signals, si},
	{"UNCATCH-BAD-SIGNALS", siLuncatch_bad_signals, si},
#endif unix

	/* unixfsys.c */

	{"TRUENAME", clLtruename, cl},
	{"RENAME-FILE", clLrename_file, cl},
	{"DELETE-FILE", clLdelete_file, cl},
	{"PROBE-FILE", clLprobe_file, cl},
	{"FILE-WRITE-DATE", clLfile_write_date, cl},
	{"FILE-AUTHOR", clLfile_author, cl},
	{"USER-HOMEDIR-PATHNAME", clLuser_homedir_pathname, cl},
	{"STRING-MATCH", siLstring_match, si},
	{"DIRECTORY", clLdirectory, cl},
	{"CHDIR", siLchdir, si},

	/* unixsys.c */

	{"SYSTEM", siLsystem, si},
	{"OPEN-PIPE", siLopen_pipe, si},

	/* end of list */
	{NULL, NULL, 0}
};


void
init_all_functions(void) {
  const struct function_info *f = all_functions;

  for (f = all_functions; f->name != NULL; f++) {
    switch (f->type) {
    case cl:
      make_function(f->name, f->f);
      break;
    case si:
      make_si_function(f->name, f->f);
      break;
    case form: {
      cl_object s = make_ordinary(f->name);
      s->symbol.isform = TRUE;
      s->symbol.mflag = FALSE;
    }
    }
  }
}
