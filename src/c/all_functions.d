#define _ARGS(x) (int n, ...)

#include "ecls.h"
#include "page.h"

struct function_info {
  const char *name;
  cl_object (*f)(int, ...);
  short type;
};

#define form 2
#define cl 0
#define si 1

static const struct function_info all_functions[] = {

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
	{"GC", Lgc, cl},
#endif

	/* array.c */

	{"MAKE-PURE-ARRAY", siLmake_pure_array, si},
	{"MAKE-VECTOR", siLmake_vector, si},
	{"AREF", Laref, cl},
	{"ASET", siLaset, si},
	{"ARRAY-ELEMENT-TYPE", Larray_element_type, cl},
	{"ARRAY-RANK", Larray_rank, cl},
	{"ARRAY-DIMENSION", Larray_dimension, cl},
	{"ARRAY-TOTAL-SIZE", Larray_total_size, cl},
	{"ADJUSTABLE-ARRAY-P", Ladjustable_array_p, cl},
	{"DISPLACED-ARRAY-P", siLdisplaced_array_p, si},

	{"SVREF", Lsvref, cl},
	{"SVSET", siLsvset, si},

	{"ARRAY-HAS-FILL-POINTER-P", Larray_has_fill_pointer_p, cl},
	{"FILL-POINTER", Lfill_pointer, cl},
	{"FILL-POINTER-SET", siLfill_pointer_set, si},

	{"REPLACE-ARRAY", siLreplace_array, si},

	/* assignment.c */

	{"CLEAR-COMPILER-PROPERTIES", siLclear_compiler_properties, si},
	{"SETQ", NULL, form},
	{"PSETQ", NULL, form},
	{"SET", Lset, cl},
	{"FSET", siLfset, si},
	{"MULTIPLE-VALUE-SETQ", NULL, form},
	{"MAKUNBOUND", Lmakunbound, cl},
	{"FMAKUNBOUND", Lfmakunbound, cl},
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

	/* character.d */

	{"STANDARD-CHAR-P", Lstandard_char_p, cl},
	{"GRAPHIC-CHAR-P", Lgraphic_char_p, cl},
	{"ALPHA-CHAR-P", Lalpha_char_p, cl},
	{"UPPER-CASE-P", Lupper_case_p, cl},
	{"LOWER-CASE-P", Llower_case_p, cl},
	{"BOTH-CASE-P", Lboth_case_p, cl},
	{"DIGIT-CHAR-P", Ldigit_char_p, cl},
	{"ALPHANUMERICP", Lalphanumericp, cl},
	{"CHAR=", Lchar_eq, cl},
	{"CHAR/=", Lchar_neq, cl},
	{"CHAR<", Lchar_l, cl},
	{"CHAR>", Lchar_g, cl},
	{"CHAR<=", Lchar_le, cl},
	{"CHAR>=", Lchar_ge, cl},
	{"CHAR-EQUAL", Lchar_equal, cl},
	{"CHAR-NOT-EQUAL", Lchar_not_equal, cl},
	{"CHAR-LESSP", Lchar_lessp, cl},
	{"CHAR-GREATERP", Lchar_greaterp, cl},
	{"CHAR-NOT-GREATERP", Lchar_not_greaterp, cl},
	{"CHAR-NOT-LESSP", Lchar_not_lessp, cl},
	{"CHARACTER", Lcharacter, cl},
	{"CHAR-CODE", Lchar_code, cl},
	{"CODE-CHAR", Lcode_char, cl},
	{"CHAR-UPCASE", Lchar_upcase, cl},
	{"CHAR-DOWNCASE", Lchar_downcase, cl},
	{"DIGIT-CHAR", Ldigit_char, cl},
	{"CHAR-INT", Lchar_int, cl},
	{"INT-CHAR", Lint_char, cl},
	{"CHAR-NAME", Lchar_name, cl},
	{"NAME-CHAR", Lname_char, cl},

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

#if defined(FRAME_CHAIN) && !defined(RUNTIME)
	{"BT", siLbacktrace, si},
#endif
	{"ERROR", Lerror, cl},
	{"CERROR", Lcerror, cl},
	{"UNIVERSAL-ERROR-HANDLER", siLuniversal_error_handler, si},

	/* eval.c */

	{"EVAL", Leval, cl},
	{"EVALHOOK", Levalhook, cl},
	{"APPLYHOOK", Lapplyhook, cl},
	{"CONSTANTP", Lconstantp, cl},
	{"UNLINK-SYMBOL", siLunlink_symbol, si},
	{"APPLY", Lapply, cl},
	{"FUNCALL", Lfuncall, cl},

	/* file.d */

	{"MAKE-SYNONYM-STREAM", Lmake_synonym_stream, cl},
	{"MAKE-BROADCAST-STREAM", Lmake_broadcast_stream, cl},
	{"MAKE-CONCATENATED-STREAM", Lmake_concatenated_stream, cl},
	{"MAKE-TWO-WAY-STREAM", Lmake_two_way_stream, cl},
	{"MAKE-ECHO-STREAM", Lmake_echo_stream, cl},
	{"MAKE-STRING-INPUT-STREAM", Lmake_string_input_stream, cl},
	{"MAKE-STRING-OUTPUT-STREAM", Lmake_string_output_stream, cl},
	{"GET-OUTPUT-STREAM-STRING", Lget_output_stream_string, cl},

	{"OUTPUT-STREAM-STRING", siLoutput_stream_string, si},

	{"STREAMP", Lstreamp, cl},
	{"INPUT-STREAM-P", Linput_stream_p, cl},
	{"OUTPUT-STREAM-P", Loutput_stream_p, cl},
	{"STREAM-ELEMENT-TYPE", Lstream_element_type, cl},
	{"CLOSE", Lclose, cl},
	{"OPEN", Lopen, cl},
	{"FILE-POSITION", Lfile_position, cl},
	{"FILE-LENGTH", Lfile_length, cl},
	{"OPEN-STREAM-P", Lopen_stream_p, cl},
	{"GET-STRING-INPUT-STREAM-INDEX", siLget_string_input_stream_index, si},
	{"MAKE-STRING-OUTPUT-STREAM-FROM-STRING", siLmake_string_output_stream_from_string, si},
	{"COPY-STREAM", siLcopy_stream, si},

	/* format. c */

	{"FORMAT", Lformat, cl},

	/* gbc.c */

#if !defined(GBC_BOEHM)
	{"ROOM-REPORT", siLroom_report, si},
	{"RESET-GC-COUNT", siLreset_gc_count, si},
	{"GC", Lgc, cl},
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

	{"MAKE-HASH-TABLE", Lmake_hash_table, cl},
	{"HASH-TABLE-P", Lhash_table_p, cl},
	{"GETHASH", Lgethash, cl},
	{"REMHASH", Lremhash, cl},
   	{"MAPHASH", Lmaphash, cl},
	{"CLRHASH", Lclrhash, cl},
	{"HASH-TABLE-COUNT", Lhash_table_count, cl},
   	{"SXHASH", Lsxhash, cl},
	{"HASH-SET", siLhash_set, si},
	{"HASH-TABLE-REHASH-SIZE", Lhash_table_rehash_size, cl},
	{"HASH-TABLE-REHASH-THRESHOLD", Lhash_table_rehash_threshold, cl},

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

	/* iteration.c */

	{"DO", NULL, form},
	{"DO*", NULL, form},
	{"DOLIST", NULL, form},
	{"DOTIMES", NULL, form},

	/* lex.c */

	{"LEX-ENV", siLlex_env, si},

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

	{"CAR", Lcar, cl},
	{"CDR", Lcdr, cl},
	{"CAAR", Lcaar, cl},
	{"CADR", Lcadr, cl},
	{"CDAR", Lcdar, cl},
	{"CDDR", Lcddr, cl},
	{"CAAAR", Lcaaar, cl},
	{"CAADR", Lcaadr, cl},
	{"CADAR", Lcadar, cl},
	{"CADDR", Lcaddr, cl},
	{"CDAAR", Lcdaar, cl},
	{"CDADR", Lcdadr, cl},
	{"CDDAR", Lcddar, cl},
	{"CDDDR", Lcdddr, cl},
	{"CAAAAR", Lcaaaar, cl},
	{"CAAADR", Lcaaadr, cl},
	{"CAADAR", Lcaadar, cl},
	{"CAADDR", Lcaaddr, cl},
	{"CADAAR", Lcadaar, cl},
	{"CADADR", Lcadadr, cl},
	{"CADDAR", Lcaddar, cl},
	{"CADDDR", Lcadddr, cl},
	{"CDAAAR", Lcdaaar, cl},
	{"CDAADR", Lcdaadr, cl},
	{"CDADAR", Lcdadar, cl},
	{"CDADDR", Lcdaddr, cl},
	{"CDDAAR", Lcddaar, cl},
	{"CDDADR", Lcddadr, cl},
	{"CDDDAR", Lcdddar, cl},
	{"CDDDDR", Lcddddr, cl},

	{"CONS", Lcons, cl},
	{"TREE-EQUAL", Ltree_equal, cl},
	{"ENDP", Lendp, cl},
	{"LIST-LENGTH", Llist_length, cl},
	{"NTH", Lnth, cl},

	{"FIRST", Lcar, cl},
	{"SECOND", Lcadr, cl},
	{"THIRD", Lcaddr, cl},
	{"FOURTH", Lcadddr, cl},
	{"FIFTH", Lfifth, cl},
	{"SIXTH", Lsixth, cl},
	{"SEVENTH", Lseventh, cl},
	{"EIGHTH", Leighth, cl},
	{"NINTH", Lninth, cl},
	{"TENTH", Ltenth, cl},

	{"REST", Lcdr, cl},
	{"NTHCDR", Lnthcdr, cl},
	{"LAST", Llast, cl},
	{"LIST", Llist, cl},
	{"LIST*", LlistA, cl},
	{"MAKE-LIST", Lmake_list, cl},
	{"APPEND", Lappend, cl},
	{"COPY-LIST", Lcopy_list, cl},
	{"COPY-ALIST", Lcopy_alist, cl},
	{"COPY-TREE", Lcopy_tree, cl},
	{"REVAPPEND", Lrevappend, cl},
	{"NCONC", Lnconc, cl},
	{"NRECONC", Lreconc, cl},

	{"BUTLAST", Lbutlast, cl},
	{"NBUTLAST", Lnbutlast, cl},
	{"LDIFF", Lldiff, cl},
	{"RPLACA", Lrplaca, cl},
	{"RPLACD", Lrplacd, cl},
	{"SUBST", Lsubst, cl},
	{"SUBST-IF", Lsubst_if, cl},
	{"SUBST-IF-NOT", Lsubst_if_not, cl},
	{"NSUBST", Lnsubst, cl},
	{"NSUBST-IF", Lnsubst_if, cl},
	{"NSUBST-IF-NOT", Lnsubst_if_not, cl},
	{"SUBLIS", Lsublis, cl},
	{"NSUBLIS", Lnsublis, cl},
	{"MEMBER", Lmember, cl},
	{"MEMBER-IF", Lmember_if, cl},
	{"MEMBER-IF-NOT", Lmember_if_not, cl},
	{"MEMBER1", Lmember1, si},
	{"TAILP", Ltailp, cl},
	{"ADJOIN", Ladjoin, cl},

	{"ACONS", Lacons, cl},
	{"PAIRLIS", Lpairlis, cl},
	{"ASSOC", Lassoc, cl},
	{"ASSOC-IF", Lassoc_if, cl},
	{"ASSOC-IF-NOT", Lassoc_if_not, cl},
	{"RASSOC", Lrassoc, cl},
	{"RASSOC-IF", Lrassoc_if, cl},
	{"RASSOC-IF-NOT", Lrassoc_if_not, cl},

	{"MEMQ", siLmemq, si},

	/* load.d */

	{"LOAD", Lload, cl},
	{"LOAD-BINARY", siLload_binary, si},
	{"LOAD-SOURCE", siLload_source, si},
#if 0
	{"FASLINK", siLfaslink, si},
#endif

	/* lwp.d */
#ifdef THREADS
	{"THREAD-BREAK-IN", siLthread_break_in, si},
	{"THREAD-BREAK-QUIT", siLthread_break_quit, si},
	{"THREAD-BREAK-RESUME", siLthread_break_resume, si},
	{"MAKE-THREAD", Lmake_thread, cl},
	{"DEACTIVATE", Ldeactivate, cl},
	{"REACTIVATE", Lreactivate, cl},
	{"KILL-THREAD", Lkill_thread, cl},
	{"CURRENT-THREAD", Lcurrent_thread, cl},
	{"THREAD-STATUS", Lthread_status, cl},
	{"THREAD-LIST", Lthread_list, cl},
	{"MAKE-CONTINUATION", Lmake_continuation, cl},
	{"THREAD-OF", Lthread_of, cl},
	{"CONTINUATION-OF", Lcontinuation_of, cl},
	{"RESUME", Lresume, cl},

	{"%DISABLE-SCHEDULER", Ldisable_scheduler, cl},
	{"%ENABLE-SCHEDULER", Lenable_scheduler, cl},
	{"%SUSPEND", Lsuspend, cl},
	{"%DELAY", Ldelay, cl},
	{"%THREAD-WAIT", Lthread_wait, cl},
	{"%THREAD-WAIT-WITH-TIMEOUT", Lthread_wait_with_timeout, cl},
#endif THREADS

	/* macros.c */

	{"MACROEXPAND", Lmacroexpand, cl},
	{"MACROEXPAND-1", Lmacroexpand_1, cl},

	/* main.c */

	{"QUIT", Lquit, cl},
	{"ARGC", siLargc, si},
	{"ARGV", siLargv, si},
	{"GETENV", siLgetenv, si},
	{"POINTER", siLaddress, si},
#if !defined(MSDOS) && !defined(__NeXT)
	{"MACHINE-INSTANCE", Lmachine_instance, cl},
	{"MACHINE-VERSION", Lmachine_version, cl},
	{"SOFTWARE-TYPE", Lsoftware_type, cl},
	{"SOFTWARE-VERSION", Lsoftware_version, cl},
#endif MSDOS

	/* mapfun.c */

	{"MAPCAR", Lmapcar, cl},
	{"MAPLIST", Lmaplist, cl},
	{"MAPC", Lmapc, cl},
	{"MAPL", Lmapl, cl},
	{"MAPCAN", Lmapcan, cl},
	{"MAPCON", Lmapcon, cl},

	/* multival.c */

	{"VALUES", Lvalues, cl},
	{"VALUES-LIST", Lvalues_list, cl},
	{"MULTIPLE-VALUE-CALL", NULL, form},
	{"MULTIPLE-VALUE-PROG1", NULL, form},
	{"MULTIPLE-VALUE-LIST", NULL, form},
	{"NTH-VALUE", NULL, form},


	/* num-arith.c */

	{"+", Lplus, cl},
	{"-", Lminus, cl},
	{"*", Ltimes, cl},
	{"/", Ldivide, cl},
	{"1+", Lone_plus, cl},
	{"1-", Lone_minus, cl},
	{"CONJUGATE", Lconjugate, cl},
	{"GCD", Lgcd, cl},
	{"LCM", Llcm, cl},


	/* num_co.c */

	{"FLOAT", Lfloat, cl},
	{"NUMERATOR", Lnumerator, cl},
	{"DENOMINATOR", Ldenominator, cl},
	{"FLOOR", Lfloor, cl},
	{"CEILING", Lceiling, cl},
	{"TRUNCATE", Ltruncate, cl},
	{"ROUND", Lround, cl},
	{"MOD", Lmod, cl},
	{"REM", Lrem, cl},
	{"DECODE-FLOAT", Ldecode_float, cl},
	{"SCALE-FLOAT", Lscale_float, cl},
	{"FLOAT-RADIX", Lfloat_radix, cl},
	{"FLOAT-SIGN", Lfloat_sign, cl},
	{"FLOAT-DIGITS", Lfloat_digits, cl},
	{"FLOAT-PRECISION", Lfloat_precision, cl},
	{"INTEGER-DECODE-FLOAT", Linteger_decode_float, cl},
	{"COMPLEX", Lcomplex, cl},
	{"REALPART", Lrealpart, cl},
	{"IMAGPART", Limagpart, cl},

	/* num_comp.c */

	{"=", Lall_the_same, cl},
	{"/=", Lall_different, cl},
	{"<", Lmonotonically_increasing, cl},
	{">", Lmonotonically_decreasing, cl},
	{"<=", Lmonotonically_nondecreasing, cl},
	{">=", Lmonotonically_nonincreasing, cl},
	{"MAX", Lmax, cl},
	{"MIN", Lmin, cl},

	/* num_log.c */

	{"LOGIOR", Llogior, cl},
	{"LOGXOR", Llogxor, cl},
	{"LOGAND", Llogand, cl},
	{"LOGEQV", Llogeqv, cl},
	{"BOOLE", Lboole, cl},
	{"LOGBITP", Llogbitp, cl},
	{"ASH", Lash, cl},
	{"LOGCOUNT", Llogcount, cl},
	{"INTEGER-LENGTH", Linteger_length, cl},
	{"BIT-ARRAY-OP", siLbit_array_op, si},

	/* num_pred.c */

	{"ZEROP", Lzerop, cl},
	{"PLUSP", Lplusp, cl},
	{"MINUSP", Lminusp, cl},
	{"ODDP", Loddp, cl},
	{"EVENP", Levenp, cl},
	{"NANI", siLnani, si},

	/* num_rand.c */

	{"RANDOM", Lrandom, cl},
	{"MAKE-RANDOM-STATE", Lmake_random_state, cl},
	{"RANDOM-STATE-P", Lrandom_state_p, cl},

	/* num_sfun.c */

	{"EXP", Lexp, cl},
	{"EXPT", Lexpt, cl},
	{"LOG", Llog, cl},
	{"SQRT", Lsqrt, cl},
	{"SIN", Lsin, cl},
	{"COS", Lcos, cl},
	{"TAN", Ltan, cl},
	{"ATAN", Latan, cl},
	{"SINH", Lsinh, cl},
	{"COSH", Lcosh, cl},
	{"TANH", Ltanh, cl},

	/* package.d */

	{"MAKE-PACKAGE", Lmake_package, cl},
	{"SELECT-PACKAGE", siLselect_package, si},
	{"FIND-PACKAGE", Lfind_package, cl},
	{"PACKAGE-NAME", Lpackage_name, cl},
	{"PACKAGE-NICKNAMES", Lpackage_nicknames, cl},
	{"RENAME-PACKAGE", Lrename_package, cl},
	{"PACKAGE-USE-LIST", Lpackage_use_list, cl},
	{"PACKAGE-USED-BY-LIST", Lpackage_used_by_list, cl},
	{"PACKAGE-SHADOWING-SYMBOLS", Lpackage_shadowing_symbols, cl},
	{"LIST-ALL-PACKAGES", Llist_all_packages, cl},
	{"INTERN", Lintern, cl},
	{"FIND-SYMBOL", Lfind_symbol, cl},
	{"UNINTERN", Lunintern, cl},
	{"EXPORT", Lexport, cl},
	{"UNEXPORT", Lunexport, cl},
	{"IMPORT", Limport, cl},
	{"SHADOWING-IMPORT", Lshadowing_import, cl},
	{"SHADOW", Lshadow, cl},
	{"USE-PACKAGE", Luse_package, cl},
	{"UNUSE-PACKAGE", Lunuse_package, cl},
	{"DELETE-PACKAGE", Ldelete_package, cl},

	{"PACKAGE-SIZE", siLpackage_size, si},
	{"PACKAGE-INTERNAL", siLpackage_internal, si},
	{"PACKAGE-EXTERNAL", siLpackage_external, si},
	{"PACKAGE-LOCK", siLpackage_lock, si},

	/* pathname.d */

	{"PATHNAME", Lpathname, cl},
	{"PARSE-NAMESTRING", Lparse_namestring, cl},
	{"MERGE-PATHNAMES", Lmerge_pathnames, cl},
	{"MAKE-PATHNAME", Lmake_pathname, cl},
	{"PATHNAMEP", Lpathnamep, cl},
	{"PATHNAME-HOST", Lpathname_host, cl},
	{"PATHNAME-DEVICE", Lpathname_device, cl},
	{"PATHNAME-DIRECTORY", Lpathname_directory, cl},
	{"PATHNAME-NAME", Lpathname_name, cl},
	{"PATHNAME-TYPE", Lpathname_type, cl},
	{"PATHNAME-VERSION", Lpathname_version, cl},
	{"NAMESTRING", Lnamestring, cl},
	{"FILE-NAMESTRING", Lfile_namestring, cl},
	{"DIRECTORY-NAMESTRING", Ldirectory_namestring, cl},
	{"HOST-NAMESTRING", Lhost_namestring, cl},
	{"ENOUGH-NAMESTRING", Lenough_namestring, cl},
	{"LOGICAL-PATHNAME-P", siLlogical_pathname_p, si},
	{"PATHNAME-MATCH-P", Lpathname_match_p, cl},
	{"TRANSLATE-PATHNAME", Ltranslate_pathname, cl},
	{"TRANSLATE-LOGICAL-PATHNAME", Ltranslate_logical_pathname, cl},
	{"PATHNAME-TRANSLATIONS", siLpathname_translations, si},

	/* predicate.c */

	{"IDENTITY", Lidentity, cl},
	{"NULL", Lnull, cl},
	{"SYMBOLP", Lsymbolp, cl},
	{"ATOM", Latom, cl},
	{"CONSP", Lconsp, cl},
	{"LISTP", Llistp, cl},
	{"NUMBERP", Lnumberp, cl},
	{"INTEGERP", Lintegerp, cl},
	{"RATIONALP", Lrationalp, cl},
	{"FLOATP", Lfloatp, cl},
	{"REALP", Lrealp, cl},
	{"COMPLEXP", Lcomplexp, cl},
	{"CHARACTERP", Lcharacterp, cl},
	{"STRINGP", Lstringp, cl},
	{"BIT-VECTOR-P", Lbit_vector_p, cl},
	{"VECTORP", Lvectorp, cl},
	{"SIMPLE-STRING-P", Lsimple_string_p, cl},
	{"SIMPLE-BIT-VECTOR-P", Lsimple_bit_vector_p, cl},
	{"SIMPLE-VECTOR-P", Lsimple_vector_p, cl},
	{"ARRAYP", Larrayp, cl},
	{"PACKAGEP", Lpackagep, cl},
	{"FUNCTIONP", Lfunctionp, cl},
	{"COMPILED-FUNCTION-P", Lcompiled_function_p, cl},
	{"COMMONP", Lcommonp, cl},

	{"EQ", Leq, cl},
	{"EQL", Leql, cl},
	{"EQUAL", Lequal, cl},
	{"EQUALP", Lequalp, cl},

	{"NOT", Lnull, cl},

	{"CONTAINS-SHARP-COMMA", siLcontains_sharp_comma, si},

	{"FIXNUMP", siLfixnump, si},

	/* print.d */

	{"WRITE", Lwrite, cl},
	{"PRIN1", Lprin1, cl},
	{"PRINT", Lprint, cl},
	{"PPRINT", Lpprint, cl},
	{"PRINC", Lprinc, cl},
	{"WRITE-CHAR", Lwrite_char, cl},
	{"WRITE-STRING", Lwrite_string, cl},
	{"WRITE-LINE", Lwrite_line, cl},
	{"WRITE-BYTE", Lwrite_byte, cl},
	{"WRITE-BYTES", Lwrite_bytes, si},
	{"TERPRI", Lterpri, cl},
	{"FRESH-LINE", Lfresh_line, cl},
	{"FINISH-OUTPUT", Lforce_output, cl},
	{"FORCE-OUTPUT", Lforce_output, cl},
	{"CLEAR-OUTPUT", Lclear_output, cl},

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

	{"READ", Lread, cl},
	{"READ-PRESERVING-WHITESPACE", Lread_preserving_whitespace, cl},
	{"READ-DELIMITED-LIST", Lread_delimited_list, cl},
	{"READ-LINE", Lread_line, cl},
	{"READ-CHAR", Lread_char, cl},
	{"UNREAD-CHAR", Lunread_char, cl},
	{"PEEK-CHAR", Lpeek_char, cl},
	{"LISTEN", Llisten, cl},
	{"READ-CHAR-NO-HANG", Lread_char_no_hang, cl},
	{"CLEAR-INPUT", Lclear_input, cl},

	{"PARSE-INTEGER", Lparse_integer, cl},

	{"READ-BYTE", Lread_byte, cl},
	{"READ-BYTES", Lread_bytes, si},

	{"COPY-READTABLE", Lcopy_readtable, cl},
	{"READTABLEP", Lreadtablep, cl},
	{"SET-SYNTAX-FROM-CHAR", Lset_syntax_from_char, cl},
	{"SET-MACRO-CHARACTER", Lset_macro_character, cl},
	{"GET-MACRO-CHARACTER", Lget_macro_character, cl},
	{"MAKE-DISPATCH-MACRO-CHARACTER", Lmake_dispatch_macro_character, cl},
	{"SET-DISPATCH-MACRO-CHARACTER", Lset_dispatch_macro_character, cl},
	{"GET-DISPATCH-MACRO-CHARACTER", Lget_dispatch_macro_character, cl},
	{"SHARP-COMMA-READER-FOR-COMPILER", siLsharp_comma_reader_for_compiler, si},
	{"STRING-TO-OBJECT", siLstring_to_object, si},
	{"STANDARD-READTABLE", siLstandard_readtable, si},

	/* reference.c */

	{"SYMBOL-FUNCTION", Lsymbol_function, cl},
	{"FBOUNDP", Lfboundp, cl},
	{"QUOTE", NULL, form},
	{"SYMBOL-VALUE", Lsymbol_value, cl},
	{"BOUNDP", Lboundp, cl},
	{"MACRO-FUNCTION", Lmacro_function, cl},
	{"SPECIAL-FORM-P", Lspecial_form_p, cl},
	{"COERCE-TO-FUNCTION", siLcoerce_to_function, si},
	{"FUNCTION", NULL, form},
	{"PROCESS-DECLARATIONS", siLprocess_declarations, si},
	{"PROCESS-LAMBDA-LIST", siLprocess_lambda_list, si},

	/* sequence.d */

	{"ELT", Lelt, cl},
	{"ELT-SET", siLelt_set, si},
	{"SUBSEQ", Lsubseq, cl},
	{"COPY-SEQ", Lcopy_seq, cl},
	{"LENGTH", Llength, cl},
	{"REVERSE", Lreverse, cl},
	{"NREVERSE", Lnreverse, cl},

	/* stacks.c */

	{"IHS-TOP", siLihs_top, si},
	{"IHS-FUN", siLihs_fun, si},
	{"IHS-ENV", siLihs_env, si},
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

	{"CHAR", Lchar, cl},
	{"CHAR-SET", siLchar_set, si},
	{"SCHAR", Lchar, cl},
	{"SCHAR-SET", siLchar_set, si},
	{"STRING=", Lstring_eq, cl},
	{"STRING-EQUAL", Lstring_equal, cl},
	{"STRING<", Lstring_l, cl},
	{"STRING>", Lstring_g, cl},
	{"STRING<=", Lstring_le, cl},
	{"STRING>=", Lstring_ge, cl},
	{"STRING/=", Lstring_neq, cl},
	{"STRING-LESSP", Lstring_lessp, cl},
	{"STRING-GREATERP", Lstring_greaterp, cl},
	{"STRING-NOT-LESSP", Lstring_not_lessp, cl},
	{"STRING-NOT-GREATERP", Lstring_not_greaterp, cl},
	{"STRING-NOT-EQUAL", Lstring_not_equal, cl},
	{"MAKE-STRING", Lmake_string, cl},
	{"STRING-TRIM", Lstring_trim, cl},
	{"STRING-LEFT-TRIM", Lstring_left_trim, cl},
	{"STRING-RIGHT-TRIM", Lstring_right_trim, cl},
	{"STRING-UPCASE", Lstring_upcase, cl},
	{"STRING-DOWNCASE", Lstring_downcase, cl},
	{"STRING-CAPITALIZE", Lstring_capitalize, cl},
	{"NSTRING-UPCASE", Lnstring_upcase, cl},
	{"NSTRING-DOWNCASE", Lnstring_downcase, cl},
	{"NSTRING-CAPITALIZE", Lnstring_capitalize, cl},
	{"STRING", Lstring, cl},
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

	{"GET", Lget, cl},
	{"REMPROP", Lremprop, cl},
	{"SYMBOL-PLIST", Lsymbol_plist, cl},
	{"GETF", Lgetf, cl},
	{"GET-PROPERTIES", Lget_properties, cl},
	{"SYMBOL-NAME", Lsymbol_name, cl},
	{"MAKE-SYMBOL", Lmake_symbol, cl},
	{"COPY-SYMBOL", Lcopy_symbol, cl},
	{"GENSYM", Lgensym, cl},
	{"GENTEMP", Lgentemp, cl},
	{"SYMBOL-PACKAGE", Lsymbol_package, cl},
	{"KEYWORDP", Lkeywordp, cl},
	{"PUT-F", siLput_f, si},
	{"REM-F", siLrem_f, si},
	{"SET-SYMBOL-PLIST", siLset_symbol_plist, si},
	{"PUTPROP", siLputprop, si},
	{"PUT-PROPERTIES", siLput_properties, si},

	/* tcp.c */
#ifdef TCP
	{"OPEN-CLIENT-STREAM", Lopen_client_stream, si},
	{"OPEN-SERVER-STREAM", Lopen_server_stream, si},
#endif

	/* time.c */

	{"GET-UNIVERSAL-TIME", Lget_universal_time, cl},
	{"SLEEP", Lsleep, cl},
	{"GET-INTERNAL-RUN-TIME", Lget_internal_run_time, cl},
	{"GET-INTERNAL-REAL-TIME", Lget_internal_real_time, cl},
	{"GET-LOCAL-TIME-ZONE", Lget_local_time_zone, si},
	{"DAYLIGHT-SAVING-TIME-P", Ldaylight_saving_timep, si},

	/* toplevel.c */

	{"LAMBDA", NULL, form},
	{"NAMED-LAMBDA", NULL, form},
	{"*MAKE-SPECIAL", siLAmake_special, si},
	{"*MAKE-CONSTANT", siLAmake_constant, si},
	{"EVAL-WHEN", NULL, form},
	{"THE", NULL, form},
	{"DECLARE", NULL, form},
	{"LOCALLY", NULL, form},

	/* typespec.c */

	{"TYPE-OF", Ltype_of, cl},

	/* unify.d */
#ifdef LOCATIVE
	{"TRAIL-MARK", Ltrail_mark, si},
	{"TRAIL-UNMARK", Ltrail_unmark, si},
	{"TRAIL-RESTORE", Ltrail_restore, si},
	{"GET-VARIABLE", NULL, form},
	{"GET-VALUE", Lget_value, si},
	{"GET-CONSTANT", Lget_constant, si},
	{"GET-NIL", Lget_nil, si},
	{"GET-CONS", Lget_cons, si},
	{"GET-INSTANCE", Lget_instance, si}, /* Mauro */
	{"UNIFY-SLOT", Lunify_slot, si},
	{"UNIFY-VALUE", Lunify_value, si},
	{"UNIFY-CONSTANT", Lunify_constant, si},
	{"UNIFY-NIL", Lunify_nil, si},
	{"MAKE-LOCATIVE", Lmake_locative, si},
	{"LOCATIVEP", Llocativep, si},
	{"UNBOUNDP", Lunboundp, si},
	{"MAKE-VARIABLE", Lmake_variable, si},
	{"DEREFERENCE", Ldereference, si},
#endif LOCATIVE

	/* unixint.c */

#ifdef unix
	{"CATCH-BAD-SIGNALS", siLcatch_bad_signals, si},
	{"UNCATCH-BAD-SIGNALS", siLuncatch_bad_signals, si},
#endif unix

	/* unixfsys.c */

	{"TRUENAME", Ltruename, cl},
	{"RENAME-FILE", Lrename_file, cl},
	{"DELETE-FILE", Ldelete_file, cl},
	{"PROBE-FILE", Lprobe_file, cl},
	{"FILE-WRITE-DATE", Lfile_write_date, cl},
	{"FILE-AUTHOR", Lfile_author, cl},
	{"USER-HOMEDIR-PATHNAME", Luser_homedir_pathname, cl},
	{"STRING-MATCH", siLstring_match, si},
	{"DIRECTORY", Ldirectory, cl},
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
