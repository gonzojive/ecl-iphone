#define form 2
#define cl 0
#define si 1

#ifdef DPP
#define SW(a,b,c) a
#else
#define SW(a,b,c) b, c
#endif

const struct {
	const char *name;
#ifdef DPP
	const char *translation;
#else
	cl_object (*f)(int, ...);
	short type;
#endif
} all_functions[] = {

	/* alloc.c */

	{"LOAD", SW("clLload",clLload,cl)},
#if !defined(GBC_BOEHM)
	{"SI::ALLOCATE", SW("siLallocate",siLallocate,si)},
	{"SI::ALLOCATED-PAGES", SW("siLallocated_pages",siLallocated_pages,si)},
	{"SI::MAXIMUM-ALLOCATABLE-PAGES", SW("siLmaximum_allocatable_pages",siLmaximum_allocatable_pages,si)},
	{"SI::ALLOCATE-CONTIGUOUS-PAGES", SW("siLallocate_contiguous_pages",siLallocate_contiguous_pages,si)},
	{"SI::ALLOCATED-CONTIGUOUS-PAGES", SW("siLallocated_contiguous_pages",siLallocated_contiguous_pages,si)},
	{"SI::MAXIMUM-CONTIGUOUS-PAGES", SW("siLmaximum_contiguous_pages",siLmaximum_contiguous_pages,si)},
	{"SI::GET-HOLE-SIZE", SW("siLget_hole_size",siLget_hole_size,si)},
	{"SI::SET-HOLE-SIZE", SW("siLset_hole_size",siLset_hole_size,si)},
	{"SI::IGNORE-MAXIMUM-PAGES", SW("siLignore_maximum_pages",siLignore_maximum_pages,si)},
#endif /* !GBC_BOEHM */

	/* alloc_2.c */

#ifdef GBC_BOEHM
	{"GC", SW("clLgc",clLgc,cl)},
#endif

	/* all_symbols.c */

	{"SI::MANGLE-NAME", SW("siLmangle_name",siLmangle_name,si)},

	/* array.c */

	{"SI::MAKE-PURE-ARRAY", SW("siLmake_pure_array",siLmake_pure_array,si)},
	{"SI::MAKE-VECTOR", SW("siLmake_vector",siLmake_vector,si)},
	{"AREF", SW("clLaref",clLaref,cl)},
	{"SI::ASET", SW("siLaset",siLaset,si)},
	{"ARRAY-ELEMENT-TYPE", SW("clLarray_element_type",clLarray_element_type,cl)},
	{"ARRAY-RANK", SW("clLarray_rank",clLarray_rank,cl)},
	{"ARRAY-DIMENSION", SW("clLarray_dimension",clLarray_dimension,cl)},
	{"ARRAY-TOTAL-SIZE", SW("clLarray_total_size",clLarray_total_size,cl)},
	{"ADJUSTABLE-ARRAY-P", SW("clLadjustable_array_p",clLadjustable_array_p,cl)},
	{"SI::DISPLACED-ARRAY-P", SW("siLdisplaced_array_p",siLdisplaced_array_p,si)},
	{"ROW-MAJOR-AREF", SW("clLrow_major_aref",clLrow_major_aref,cl)},
	{"SI::ROW-MAJOR-ASET", SW("siLrow_major_aset",siLrow_major_aset,si)},

	{"SVREF", SW("clLsvref",clLsvref,cl)},
	{"SI::SVSET", SW("siLsvset",siLsvset,si)},

	{"ARRAY-HAS-FILL-POINTER-P", SW("clLarray_has_fill_pointer_p",clLarray_has_fill_pointer_p,cl)},
	{"FILL-POINTER", SW("clLfill_pointer",clLfill_pointer,cl)},
	{"SI::FILL-POINTER-SET", SW("siLfill_pointer_set",siLfill_pointer_set,si)},

	{"SI::REPLACE-ARRAY", SW("siLreplace_array",siLreplace_array,si)},

	/* assignment.c */

	{"SI::CLEAR-COMPILER-PROPERTIES", SW("siLclear_compiler_properties",siLclear_compiler_properties,si)},
	{"SETQ", SW("NULL",NULL,form)},
	{"PSETQ", SW("NULL",NULL,form)},
	{"SET", SW("clLset",clLset,cl)},
	{"SI::FSET", SW("siLfset",siLfset,si)},
	{"MULTIPLE-VALUE-SETQ", SW("NULL",NULL,form)},
	{"MAKUNBOUND", SW("clLmakunbound",clLmakunbound,cl)},
	{"FMAKUNBOUND", SW("clLfmakunbound",clLfmakunbound,cl)},
#if 0
	{"SETF", SW("NULL",NULL,form)},
	{"PUSH", SW("NULL",NULL,form)},
	{"POP", SW("NULL",NULL,form)},
	{"INCF", SW("NULL",NULL,form)},
	{"DECF", SW("NULL",NULL,form)},
#endif
	{"SI::SETF-NAMEP", SW("siLsetf_namep",siLsetf_namep,si)},

	/* block.c */

	{"BLOCK", SW("NULL",NULL,form)},
	{"RETURN-FROM", SW("NULL",NULL,form)},
	{"RETURN", SW("NULL",NULL,form)},

	/* catch.c */

	{"CATCH", SW("NULL",NULL,form)},
	{"UNWIND-PROTECT", SW("NULL",NULL,form)},
	{"THROW", SW("NULL",NULL,form)},

	/* cfun.c */

	{"SI::COMPILED-FUNCTION-NAME", SW("siLcompiled_function_name",siLcompiled_function_name,si)},
	{"SI::COMPILED-FUNCTION-BLOCK", SW("siLcompiled_function_block",siLcompiled_function_block,si)},
	{"SI::COMPILED-FUNCTION-SOURCE", SW("siLcompiled_function_source",siLcompiled_function_source,si)},

	/* character.d */

	{"STANDARD-CHAR-P", SW("clLstandard_char_p",clLstandard_char_p,cl)},
	{"GRAPHIC-CHAR-P", SW("clLgraphic_char_p",clLgraphic_char_p,cl)},
	{"ALPHA-CHAR-P", SW("clLalpha_char_p",clLalpha_char_p,cl)},
	{"UPPER-CASE-P", SW("clLupper_case_p",clLupper_case_p,cl)},
	{"LOWER-CASE-P", SW("clLlower_case_p",clLlower_case_p,cl)},
	{"BOTH-CASE-P", SW("clLboth_case_p",clLboth_case_p,cl)},
	{"DIGIT-CHAR-P", SW("clLdigit_char_p",clLdigit_char_p,cl)},
	{"ALPHANUMERICP", SW("clLalphanumericp",clLalphanumericp,cl)},
	{"CHAR=", SW("clLcharE",clLcharE,cl)},
	{"CHAR/=", SW("clLcharNE",clLcharNE,cl)},
	{"CHAR<", SW("clLcharL",clLcharL,cl)},
	{"CHAR>", SW("clLcharG",clLcharG,cl)},
	{"CHAR<=", SW("clLcharLE",clLcharLE,cl)},
	{"CHAR>=", SW("clLcharGE",clLcharGE,cl)},
	{"CHAR-EQUAL", SW("clLchar_equal",clLchar_equal,cl)},
	{"CHAR-NOT-EQUAL", SW("clLchar_not_equal",clLchar_not_equal,cl)},
	{"CHAR-LESSP", SW("clLchar_lessp",clLchar_lessp,cl)},
	{"CHAR-GREATERP", SW("clLchar_greaterp",clLchar_greaterp,cl)},
	{"CHAR-NOT-GREATERP", SW("clLchar_not_greaterp",clLchar_not_greaterp,cl)},
	{"CHAR-NOT-LESSP", SW("clLchar_not_lessp",clLchar_not_lessp,cl)},
	{"CHARACTER", SW("clLcharacter",clLcharacter,cl)},
	{"CHAR-CODE", SW("clLchar_code",clLchar_code,cl)},
	{"CODE-CHAR", SW("clLcode_char",clLcode_char,cl)},
	{"CHAR-UPCASE", SW("clLchar_upcase",clLchar_upcase,cl)},
	{"CHAR-DOWNCASE", SW("clLchar_downcase",clLchar_downcase,cl)},
	{"DIGIT-CHAR", SW("clLdigit_char",clLdigit_char,cl)},
	{"CHAR-INT", SW("clLchar_int",clLchar_int,cl)},
	{"INT-CHAR", SW("clLint_char",clLint_char,cl)},
	{"CHAR-NAME", SW("clLchar_name",clLchar_name,cl)},
	{"NAME-CHAR", SW("clLname_char",clLname_char,cl)},

	/* clos.c */
	{"FIND-CLASS", SW("clLfind_class",clLfind_class,cl)},

	/* cmpaux.c */

	{"SI::SPECIALP", SW("siLspecialp",siLspecialp,si)},

	/* conditional.c */

	{"IF", SW("NULL",NULL,form)},
	{"COND", SW("NULL",NULL,form)},
	{"CASE", SW("NULL",NULL,form)},
	{"WHEN", SW("NULL",NULL,form)},
	{"UNLESS", SW("NULL",NULL,form)},

	/* disassembler.c */
	{"SI::BC-DISASSEMBLE", SW("siLbc_disassemble",siLbc_disassemble,si)},
	{"SI::BC-SPLIT", SW("siLbc_split",siLbc_split,si)},

	/* error.c */

	{"ERROR", SW("clLerror",clLerror,cl)},
	{"CERROR", SW("clLcerror",clLcerror,cl)},

	/* eval.c */

	{"EVAL", SW("clLeval",clLeval,cl)},
	{"SI::EVAL-WITH-ENV", SW("siLeval_with_env",siLeval_with_env,si)},
	{"CONSTANTP", SW("clLconstantp",clLconstantp,cl)},
	{"SI::UNLINK-SYMBOL", SW("siLunlink_symbol",siLunlink_symbol,si)},
	{"APPLY", SW("clLapply",clLapply,cl)},
	{"FUNCALL", SW("clLfuncall",clLfuncall,cl)},

	/* file.d */

	{"MAKE-SYNONYM-STREAM", SW("clLmake_synonym_stream",clLmake_synonym_stream,cl)},
	{"MAKE-BROADCAST-STREAM", SW("clLmake_broadcast_stream",clLmake_broadcast_stream,cl)},
	{"MAKE-CONCATENATED-STREAM", SW("clLmake_concatenated_stream",clLmake_concatenated_stream,cl)},
	{"MAKE-TWO-WAY-STREAM", SW("clLmake_two_way_stream",clLmake_two_way_stream,cl)},
	{"MAKE-ECHO-STREAM", SW("clLmake_echo_stream",clLmake_echo_stream,cl)},
	{"MAKE-STRING-INPUT-STREAM", SW("clLmake_string_input_stream",clLmake_string_input_stream,cl)},
	{"MAKE-STRING-OUTPUT-STREAM", SW("clLmake_string_output_stream",clLmake_string_output_stream,cl)},
	{"GET-OUTPUT-STREAM-STRING", SW("clLget_output_stream_string",clLget_output_stream_string,cl)},

	{"SI::OUTPUT-STREAM-STRING", SW("siLoutput_stream_string",siLoutput_stream_string,si)},

	{"STREAMP", SW("clLstreamp",clLstreamp,cl)},
	{"INPUT-STREAM-P", SW("clLinput_stream_p",clLinput_stream_p,cl)},
	{"OUTPUT-STREAM-P", SW("clLoutput_stream_p",clLoutput_stream_p,cl)},
	{"STREAM-ELEMENT-TYPE", SW("clLstream_element_type",clLstream_element_type,cl)},
	{"CLOSE", SW("clLclose",clLclose,cl)},
	{"OPEN", SW("clLopen",clLopen,cl)},
	{"FILE-POSITION", SW("clLfile_position",clLfile_position,cl)},
	{"FILE-LENGTH", SW("clLfile_length",clLfile_length,cl)},
	{"OPEN-STREAM-P", SW("clLopen_stream_p",clLopen_stream_p,cl)},
	{"SI::GET-STRING-INPUT-STREAM-INDEX", SW("siLget_string_input_stream_index",siLget_string_input_stream_index,si)},
	{"SI::MAKE-STRING-OUTPUT-STREAM-FROM-STRING", SW("siLmake_string_output_stream_from_string",siLmake_string_output_stream_from_string,si)},
	{"SI::COPY-STREAM", SW("siLcopy_stream",siLcopy_stream,si)},

	/* format. c */

	{"FORMAT", SW("clLformat",clLformat,cl)},

	/* gbc.c */

#if !defined(GBC_BOEHM)
	{"SI::ROOM-REPORT", SW("siLroom_report",siLroom_report,si)},
	{"SI::RESET-GC-COUNT", SW("siLreset_gc_count",siLreset_gc_count,si)},
	{"GC", SW("clLgc",clLgc,cl)},
	{"SI::GC-TIME", SW("siLgc_time",siLgc_time,si)},
#endif

	/* gfun.c */
#ifdef CLOS
	{"SI::ALLOCATE-GFUN", SW("siLallocate_gfun",siLallocate_gfun,si)},
	{"SI::GFUN-NAME", SW("siLgfun_name",siLgfun_name,si)},
	{"SI::GFUN-NAME-SET", SW("siLgfun_name_set",siLgfun_name_set,si)},
	{"SI::GFUN-METHOD-HT", SW("siLgfun_method_ht",siLgfun_method_ht,si)},
	{"SI::GFUN-METHOD-HT-SET", SW("siLgfun_method_ht_set",siLgfun_method_ht_set,si)},
	{"SI::GFUN-SPEC-HOW-REF", SW("siLgfun_spec_how_ref",siLgfun_spec_how_ref,si)},
	{"SI::GFUN-SPEC-HOW-SET", SW("siLgfun_spec_how_set",siLgfun_spec_how_set,si)},
	{"SI::GFUN-INSTANCE", SW("siLgfun_instance",siLgfun_instance,si)},
	{"SI::GFUN-INSTANCE-SET", SW("siLgfun_instance_set",siLgfun_instance_set,si)},
	{"SI::GFUNP", SW("siLgfunp",siLgfunp,si)},
	{"SI::METHOD-HT-GET", SW("siLmethod_ht_get",siLmethod_ht_get,si)},
	{"SI::SET-COMPILED-FUNCTION-NAME", SW("siLset_compiled_function_name",siLset_compiled_function_name,si)},
#endif /* CLOS */

	/* hash.d */

	{"MAKE-HASH-TABLE", SW("clLmake_hash_table",clLmake_hash_table,cl)},
	{"HASH-TABLE-P", SW("clLhash_table_p",clLhash_table_p,cl)},
	{"GETHASH", SW("clLgethash",clLgethash,cl)},
	{"REMHASH", SW("clLremhash",clLremhash,cl)},
   	{"MAPHASH", SW("clLmaphash",clLmaphash,cl)},
	{"CLRHASH", SW("clLclrhash",clLclrhash,cl)},
	{"HASH-TABLE-COUNT", SW("clLhash_table_count",clLhash_table_count,cl)},
   	{"SXHASH", SW("clLsxhash",clLsxhash,cl)},
	{"SI::HASH-SET", SW("siLhash_set",siLhash_set,si)},
	{"HASH-TABLE-REHASH-SIZE", SW("clLhash_table_rehash_size",clLhash_table_rehash_size,cl)},
	{"HASH-TABLE-REHASH-THRESHOLD", SW("clLhash_table_rehash_threshold",clLhash_table_rehash_threshold,cl)},

	/* instance.c */
#ifdef CLOS
	{"SI::ALLOCATE-INSTANCE", SW("siLallocate_instance",siLallocate_instance,si)},
	{"SI::CHANGE-INSTANCE", SW("siLchange_instance",siLchange_instance,si)},
	{"SI::INSTANCE-REF-SAFE", SW("siLinstance_ref_safe",siLinstance_ref_safe,si)},
	{"SI::INSTANCE-REF", SW("siLinstance_ref",siLinstance_ref,si)},
	{"SI::INSTANCE-SET", SW("siLinstance_set",siLinstance_set,si)},
	{"SI::INSTANCE-CLASS", SW("siLinstance_class",siLinstance_class,si)},
	{"SI::INSTANCE-CLASS-SET", SW("siLinstance_class_set",siLinstance_class_set,si)},
	{"SI::INSTANCEP", SW("siLinstancep",siLinstancep,si)},
	{"SI::UNBOUND", SW("siLunbound",siLunbound,si)},
	{"SI::SL-BOUNDP", SW("siLsl_boundp",siLsl_boundp,si)},
	{"SI::SL-MAKUNBOUND", SW("siLsl_makunbound",siLsl_makunbound,si)},
#endif /* CLOS */

	/* interpreter.c */
	{"SI::INTERPRETER-STACK", SW("siLinterpreter_stack",siLinterpreter_stack,si)},
	{"SI::MAKE-LAMBDA", SW("siLmake_lambda",siLmake_lambda,si)},
	{"SI::FUNCTION-BLOCK-NAME", SW("siLfunction_block_name",siLfunction_block_name,si)},

	/* iteration.c */

	{"DO", SW("NULL",NULL,form)},
	{"DO*", SW("NULL",NULL,form)},
	{"DOLIST", SW("NULL",NULL,form)},
	{"DOTIMES", SW("NULL",NULL,form)},

	/* let.c */

	{"LET", SW("NULL",NULL,form)},
	{"LET*", SW("NULL",NULL,form)},
	{"MULTIPLE-VALUE-BIND", SW("NULL",NULL,form)},
	{"COMPILER-LET", SW("NULL",NULL,form)},
	{"FLET", SW("NULL",NULL,form)},
	{"LABELS", SW("NULL",NULL,form)},
	{"MACROLET", SW("NULL",NULL,form)},
	{"SYMBOL-MACROLET", SW("NULL",NULL,form)},

	/* list.d */

	{"CAR", SW("clLcar",clLcar,cl)},
	{"CDR", SW("clLcdr",clLcdr,cl)},
	{"CAAR", SW("clLcaar",clLcaar,cl)},
	{"CADR", SW("clLcadr",clLcadr,cl)},
	{"CDAR", SW("clLcdar",clLcdar,cl)},
	{"CDDR", SW("clLcddr",clLcddr,cl)},
	{"CAAAR", SW("clLcaaar",clLcaaar,cl)},
	{"CAADR", SW("clLcaadr",clLcaadr,cl)},
	{"CADAR", SW("clLcadar",clLcadar,cl)},
	{"CADDR", SW("clLcaddr",clLcaddr,cl)},
	{"CDAAR", SW("clLcdaar",clLcdaar,cl)},
	{"CDADR", SW("clLcdadr",clLcdadr,cl)},
	{"CDDAR", SW("clLcddar",clLcddar,cl)},
	{"CDDDR", SW("clLcdddr",clLcdddr,cl)},
	{"CAAAAR", SW("clLcaaaar",clLcaaaar,cl)},
	{"CAAADR", SW("clLcaaadr",clLcaaadr,cl)},
	{"CAADAR", SW("clLcaadar",clLcaadar,cl)},
	{"CAADDR", SW("clLcaaddr",clLcaaddr,cl)},
	{"CADAAR", SW("clLcadaar",clLcadaar,cl)},
	{"CADADR", SW("clLcadadr",clLcadadr,cl)},
	{"CADDAR", SW("clLcaddar",clLcaddar,cl)},
	{"CADDDR", SW("clLcadddr",clLcadddr,cl)},
	{"CDAAAR", SW("clLcdaaar",clLcdaaar,cl)},
	{"CDAADR", SW("clLcdaadr",clLcdaadr,cl)},
	{"CDADAR", SW("clLcdadar",clLcdadar,cl)},
	{"CDADDR", SW("clLcdaddr",clLcdaddr,cl)},
	{"CDDAAR", SW("clLcddaar",clLcddaar,cl)},
	{"CDDADR", SW("clLcddadr",clLcddadr,cl)},
	{"CDDDAR", SW("clLcdddar",clLcdddar,cl)},
	{"CDDDDR", SW("clLcddddr",clLcddddr,cl)},

	{"CONS", SW("clLcons",clLcons,cl)},
	{"TREE-EQUAL", SW("clLtree_equal",clLtree_equal,cl)},
	{"ENDP", SW("clLendp",clLendp,cl)},
	{"LIST-LENGTH", SW("clLlist_length",clLlist_length,cl)},
	{"NTH", SW("clLnth",clLnth,cl)},

	{"FIRST", SW("clLcar",clLcar,cl)},
	{"SECOND", SW("clLcadr",clLcadr,cl)},
	{"THIRD", SW("clLcaddr",clLcaddr,cl)},
	{"FOURTH", SW("clLcadddr",clLcadddr,cl)},
	{"FIFTH", SW("clLfifth",clLfifth,cl)},
	{"SIXTH", SW("clLsixth",clLsixth,cl)},
	{"SEVENTH", SW("clLseventh",clLseventh,cl)},
	{"EIGHTH", SW("clLeighth",clLeighth,cl)},
	{"NINTH", SW("clLninth",clLninth,cl)},
	{"TENTH", SW("clLtenth",clLtenth,cl)},

	{"REST", SW("clLcdr",clLcdr,cl)},
	{"NTHCDR", SW("clLnthcdr",clLnthcdr,cl)},
	{"LAST", SW("clLlast",clLlast,cl)},
	{"LIST", SW("clLlist",clLlist,cl)},
	{"LIST*", SW("clLlistX",clLlistX,cl)},
	{"MAKE-LIST", SW("clLmake_list",clLmake_list,cl)},
	{"APPEND", SW("clLappend",clLappend,cl)},
	{"COPY-LIST", SW("clLcopy_list",clLcopy_list,cl)},
	{"COPY-ALIST", SW("clLcopy_alist",clLcopy_alist,cl)},
	{"COPY-TREE", SW("clLcopy_tree",clLcopy_tree,cl)},
	{"REVAPPEND", SW("clLrevappend",clLrevappend,cl)},
	{"NCONC", SW("clLnconc",clLnconc,cl)},
	{"NRECONC", SW("clLnreconc",clLnreconc,cl)},

	{"BUTLAST", SW("clLbutlast",clLbutlast,cl)},
	{"NBUTLAST", SW("clLnbutlast",clLnbutlast,cl)},
	{"LDIFF", SW("clLldiff",clLldiff,cl)},
	{"RPLACA", SW("clLrplaca",clLrplaca,cl)},
	{"RPLACD", SW("clLrplacd",clLrplacd,cl)},
	{"SUBST", SW("clLsubst",clLsubst,cl)},
	{"SUBST-IF", SW("clLsubst_if",clLsubst_if,cl)},
	{"SUBST-IF-NOT", SW("clLsubst_if_not",clLsubst_if_not,cl)},
	{"NSUBST", SW("clLnsubst",clLnsubst,cl)},
	{"NSUBST-IF", SW("clLnsubst_if",clLnsubst_if,cl)},
	{"NSUBST-IF-NOT", SW("clLnsubst_if_not",clLnsubst_if_not,cl)},
	{"SUBLIS", SW("clLsublis",clLsublis,cl)},
	{"NSUBLIS", SW("clLnsublis",clLnsublis,cl)},
	{"MEMBER", SW("clLmember",clLmember,cl)},
	{"MEMBER-IF", SW("clLmember_if",clLmember_if,cl)},
	{"MEMBER-IF-NOT", SW("clLmember_if_not",clLmember_if_not,cl)},
	{"SI::MEMBER1", SW("siLmember1",siLmember1,si)},
	{"TAILP", SW("clLtailp",clLtailp,cl)},
	{"ADJOIN", SW("clLadjoin",clLadjoin,cl)},

	{"ACONS", SW("clLacons",clLacons,cl)},
	{"PAIRLIS", SW("clLpairlis",clLpairlis,cl)},
	{"ASSOC", SW("clLassoc",clLassoc,cl)},
	{"ASSOC-IF", SW("clLassoc_if",clLassoc_if,cl)},
	{"ASSOC-IF-NOT", SW("clLassoc_if_not",clLassoc_if_not,cl)},
	{"RASSOC", SW("clLrassoc",clLrassoc,cl)},
	{"RASSOC-IF", SW("clLrassoc_if",clLrassoc_if,cl)},
	{"RASSOC-IF-NOT", SW("clLrassoc_if_not",clLrassoc_if_not,cl)},

	{"SI::MEMQ", SW("siLmemq",siLmemq,si)},

	/* load.d */

#ifdef ENABLE_DLOPEN
	{"SI::LOAD-BINARY", SW("siLload_binary",siLload_binary,si)},
#endif
	{"SI::LOAD-SOURCE", SW("siLload_source",siLload_source,si)},

	/* lwp.d */
#ifdef THREADS
	{"SI::THREAD-BREAK-IN", SW("siLthread_break_in",siLthread_break_in,si)},
	{"SI::THREAD-BREAK-QUIT", SW("siLthread_break_quit",siLthread_break_quit,si)},
	{"SI::THREAD-BREAK-RESUME", SW("siLthread_break_resume",siLthread_break_resume,si)},
	{"MAKE-THREAD", SW("clLmake_thread",clLmake_thread,cl)},
	{"DEACTIVATE", SW("clLdeactivate",clLdeactivate,cl)},
	{"REACTIVATE", SW("clLreactivate",clLreactivate,cl)},
	{"KILL-THREAD", SW("clLkill_thread",clLkill_thread,cl)},
	{"CURRENT-THREAD", SW("clLcurrent_thread",clLcurrent_thread,cl)},
	{"THREAD-STATUS", SW("clLthread_status",clLthread_status,cl)},
	{"THREAD-LIST", SW("clLthread_list",clLthread_list,cl)},
	{"MAKE-CONTINUATION", SW("clLmake_continuation",clLmake_continuation,cl)},
	{"THREAD-OF", SW("clLthread_of",clLthread_of,cl)},
	{"CONTINUATION-OF", SW("clLcontinuation_of",clLcontinuation_of,cl)},
	{"RESUME", SW("clLresume",clLresume,cl)},

	{"%DISABLE-SCHEDULER", SW("clLdisable_scheduler",clLdisable_scheduler,cl)},
	{"%ENABLE-SCHEDULER", SW("clLenable_scheduler",clLenable_scheduler,cl)},
	{"%SUSPEND", SW("clLsuspend",clLsuspend,cl)},
	{"%DELAY", SW("clLdelay",clLdelay,cl)},
	{"%THREAD-WAIT", SW("clLthread_wait",clLthread_wait,cl)},
	{"%THREAD-WAIT-WITH-TIMEOUT", SW("clLthread_wait_with_timeout",clLthread_wait_with_timeout,cl)},
#endif /* THREADS */

	/* macros.c */

	{"MACROEXPAND", SW("clLmacroexpand",clLmacroexpand,cl)},
	{"MACROEXPAND-1", SW("clLmacroexpand_1",clLmacroexpand_1,cl)},

	/* main.c */

	{"QUIT", SW("clLquit",clLquit,cl)},
	{"SI::ARGC", SW("siLargc",siLargc,si)},
	{"SI::ARGV", SW("siLargv",siLargv,si)},
	{"SI::GETENV", SW("siLgetenv",siLgetenv,si)},
	{"SI::SETENV", SW("siLsetenv",siLsetenv,si)},
	{"SI::POINTER", SW("siLpointer",siLpointer,si)},

	/* mapfun.c */

	{"MAPCAR", SW("clLmapcar",clLmapcar,cl)},
	{"MAPLIST", SW("clLmaplist",clLmaplist,cl)},
	{"MAPC", SW("clLmapc",clLmapc,cl)},
	{"MAPL", SW("clLmapl",clLmapl,cl)},
	{"MAPCAN", SW("clLmapcan",clLmapcan,cl)},
	{"MAPCON", SW("clLmapcon",clLmapcon,cl)},

	/* multival.c */

	{"VALUES", SW("clLvalues",clLvalues,cl)},
	{"VALUES-LIST", SW("clLvalues_list",clLvalues_list,cl)},
	{"MULTIPLE-VALUE-CALL", SW("NULL",NULL,form)},
	{"MULTIPLE-VALUE-PROG1", SW("NULL",NULL,form)},
	{"MULTIPLE-VALUE-LIST", SW("NULL",NULL,form)},
	{"NTH-VALUE", SW("NULL",NULL,form)},


	/* num-arith.c */

	{"+", SW("clLP",clLP,cl)},
	{"-", SW("clLM",clLM,cl)},
	{"*", SW("clLX",clLX,cl)},
	{"/", SW("clLN",clLN,cl)},
	{"1+", SW("clL1P",clL1P,cl)},
	{"1-", SW("clL1M",clL1M,cl)},
	{"CONJUGATE", SW("clLconjugate",clLconjugate,cl)},
	{"GCD", SW("clLgcd",clLgcd,cl)},
	{"LCM", SW("clLlcm",clLlcm,cl)},


	/* num_co.c */

	{"FLOAT", SW("clLfloat",clLfloat,cl)},
	{"NUMERATOR", SW("clLnumerator",clLnumerator,cl)},
	{"DENOMINATOR", SW("clLdenominator",clLdenominator,cl)},
	{"FLOOR", SW("clLfloor",clLfloor,cl)},
	{"CEILING", SW("clLceiling",clLceiling,cl)},
	{"TRUNCATE", SW("clLtruncate",clLtruncate,cl)},
	{"ROUND", SW("clLround",clLround,cl)},
	{"MOD", SW("clLmod",clLmod,cl)},
	{"REM", SW("clLrem",clLrem,cl)},
	{"DECODE-FLOAT", SW("clLdecode_float",clLdecode_float,cl)},
	{"SCALE-FLOAT", SW("clLscale_float",clLscale_float,cl)},
	{"FLOAT-RADIX", SW("clLfloat_radix",clLfloat_radix,cl)},
	{"FLOAT-SIGN", SW("clLfloat_sign",clLfloat_sign,cl)},
	{"FLOAT-DIGITS", SW("clLfloat_digits",clLfloat_digits,cl)},
	{"FLOAT-PRECISION", SW("clLfloat_precision",clLfloat_precision,cl)},
	{"INTEGER-DECODE-FLOAT", SW("clLinteger_decode_float",clLinteger_decode_float,cl)},
	{"COMPLEX", SW("clLcomplex",clLcomplex,cl)},
	{"REALPART", SW("clLrealpart",clLrealpart,cl)},
	{"IMAGPART", SW("clLimagpart",clLimagpart,cl)},

	/* num_comp.c */

	{"=", SW("clLE",clLE,cl)},
	{"/=", SW("clLNE",clLNE,cl)},
	{"<", SW("clLL",clLL,cl)},
	{">", SW("clLG",clLG,cl)},
	{"<=", SW("clLLE",clLLE,cl)},
	{">=", SW("clLGE",clLGE,cl)},
	{"MAX", SW("clLmax",clLmax,cl)},
	{"MIN", SW("clLmin",clLmin,cl)},

	/* num_log.c */

	{"LOGIOR", SW("clLlogior",clLlogior,cl)},
	{"LOGXOR", SW("clLlogxor",clLlogxor,cl)},
	{"LOGAND", SW("clLlogand",clLlogand,cl)},
	{"LOGEQV", SW("clLlogeqv",clLlogeqv,cl)},
	{"LOGNAND", SW("clLlognand",clLlognand,cl)},
	{"LOGNOR", SW("clLlognor",clLlognor,cl)},
	{"LOGANDC1", SW("clLlogandc1",clLlogandc1,cl)},
	{"LOGANDC2", SW("clLlogandc2",clLlogandc2,cl)},
	{"LOGORC1", SW("clLlogorc1",clLlogorc1,cl)},
	{"LOGORC2", SW("clLlogorc2",clLlogorc2,cl)},
	{"LOGNOT", SW("clLlognot",clLlognot,cl)},
	{"BOOLE", SW("clLboole",clLboole,cl)},
	{"LOGBITP", SW("clLlogbitp",clLlogbitp,cl)},
	{"ASH", SW("clLash",clLash,cl)},
	{"LOGCOUNT", SW("clLlogcount",clLlogcount,cl)},
	{"INTEGER-LENGTH", SW("clLinteger_length",clLinteger_length,cl)},
	{"SI::BIT-ARRAY-OP", SW("siLbit_array_op",siLbit_array_op,si)},

	/* num_pred.c */

	{"ZEROP", SW("clLzerop",clLzerop,cl)},
	{"PLUSP", SW("clLplusp",clLplusp,cl)},
	{"MINUSP", SW("clLminusp",clLminusp,cl)},
	{"ODDP", SW("clLoddp",clLoddp,cl)},
	{"EVENP", SW("clLevenp",clLevenp,cl)},

	/* num_rand.c */

	{"RANDOM", SW("clLrandom",clLrandom,cl)},
	{"MAKE-RANDOM-STATE", SW("clLmake_random_state",clLmake_random_state,cl)},
	{"RANDOM-STATE-P", SW("clLrandom_state_p",clLrandom_state_p,cl)},

	/* num_sfun.c */

	{"EXP", SW("clLexp",clLexp,cl)},
	{"EXPT", SW("clLexpt",clLexpt,cl)},
	{"LOG", SW("clLlog",clLlog,cl)},
	{"SQRT", SW("clLsqrt",clLsqrt,cl)},
	{"SIN", SW("clLsin",clLsin,cl)},
	{"COS", SW("clLcos",clLcos,cl)},
	{"TAN", SW("clLtan",clLtan,cl)},
	{"ATAN", SW("clLatan",clLatan,cl)},
	{"SINH", SW("clLsinh",clLsinh,cl)},
	{"COSH", SW("clLcosh",clLcosh,cl)},
	{"TANH", SW("clLtanh",clLtanh,cl)},

	/* package.d */

	{"MAKE-PACKAGE", SW("clLmake_package",clLmake_package,cl)},
	{"SI::SELECT-PACKAGE", SW("siLselect_package",siLselect_package,si)},
	{"FIND-PACKAGE", SW("clLfind_package",clLfind_package,cl)},
	{"PACKAGE-NAME", SW("clLpackage_name",clLpackage_name,cl)},
	{"PACKAGE-NICKNAMES", SW("clLpackage_nicknames",clLpackage_nicknames,cl)},
	{"RENAME-PACKAGE", SW("clLrename_package",clLrename_package,cl)},
	{"PACKAGE-USE-LIST", SW("clLpackage_use_list",clLpackage_use_list,cl)},
	{"PACKAGE-USED-BY-LIST", SW("clLpackage_used_by_list",clLpackage_used_by_list,cl)},
	{"PACKAGE-SHADOWING-SYMBOLS", SW("clLpackage_shadowing_symbols",clLpackage_shadowing_symbols,cl)},
	{"LIST-ALL-PACKAGES", SW("clLlist_all_packages",clLlist_all_packages,cl)},
	{"INTERN", SW("clLintern",clLintern,cl)},
	{"FIND-SYMBOL", SW("clLfind_symbol",clLfind_symbol,cl)},
	{"UNINTERN", SW("clLunintern",clLunintern,cl)},
	{"EXPORT", SW("clLexport",clLexport,cl)},
	{"UNEXPORT", SW("clLunexport",clLunexport,cl)},
	{"IMPORT", SW("clLimport",clLimport,cl)},
	{"SHADOWING-IMPORT", SW("clLshadowing_import",clLshadowing_import,cl)},
	{"SHADOW", SW("clLshadow",clLshadow,cl)},
	{"USE-PACKAGE", SW("clLuse_package",clLuse_package,cl)},
	{"UNUSE-PACKAGE", SW("clLunuse_package",clLunuse_package,cl)},
	{"DELETE-PACKAGE", SW("clLdelete_package",clLdelete_package,cl)},

	{"SI::PACKAGE-SIZE", SW("siLpackage_size",siLpackage_size,si)},
	{"SI::PACKAGE-INTERNAL", SW("siLpackage_internal",siLpackage_internal,si)},
	{"SI::PACKAGE-EXTERNAL", SW("siLpackage_external",siLpackage_external,si)},
	{"SI::PACKAGE-LOCK", SW("siLpackage_lock",siLpackage_lock,si)},

	/* pathname.d */

	{"PATHNAME", SW("clLpathname",clLpathname,cl)},
	{"PARSE-NAMESTRING", SW("clLparse_namestring",clLparse_namestring,cl)},
	{"MERGE-PATHNAMES", SW("clLmerge_pathnames",clLmerge_pathnames,cl)},
	{"MAKE-PATHNAME", SW("clLmake_pathname",clLmake_pathname,cl)},
	{"PATHNAMEP", SW("clLpathnamep",clLpathnamep,cl)},
	{"PATHNAME-HOST", SW("clLpathname_host",clLpathname_host,cl)},
	{"PATHNAME-DEVICE", SW("clLpathname_device",clLpathname_device,cl)},
	{"PATHNAME-DIRECTORY", SW("clLpathname_directory",clLpathname_directory,cl)},
	{"PATHNAME-NAME", SW("clLpathname_name",clLpathname_name,cl)},
	{"PATHNAME-TYPE", SW("clLpathname_type",clLpathname_type,cl)},
	{"PATHNAME-VERSION", SW("clLpathname_version",clLpathname_version,cl)},
	{"NAMESTRING", SW("clLnamestring",clLnamestring,cl)},
	{"FILE-NAMESTRING", SW("clLfile_namestring",clLfile_namestring,cl)},
	{"DIRECTORY-NAMESTRING", SW("clLdirectory_namestring",clLdirectory_namestring,cl)},
	{"HOST-NAMESTRING", SW("clLhost_namestring",clLhost_namestring,cl)},
	{"ENOUGH-NAMESTRING", SW("clLenough_namestring",clLenough_namestring,cl)},
	{"SI::LOGICAL-PATHNAME-P", SW("siLlogical_pathname_p",siLlogical_pathname_p,si)},
	{"PATHNAME-MATCH-P", SW("clLpathname_match_p",clLpathname_match_p,cl)},
	{"TRANSLATE-PATHNAME", SW("clLtranslate_pathname",clLtranslate_pathname,cl)},
	{"TRANSLATE-LOGICAL-PATHNAME", SW("clLtranslate_logical_pathname",clLtranslate_logical_pathname,cl)},
	{"SI::PATHNAME-TRANSLATIONS", SW("siLpathname_translations",siLpathname_translations,si)},

	/* predicate.c */

	{"IDENTITY", SW("clLidentity",clLidentity,cl)},
	{"NULL", SW("clLnull",clLnull,cl)},
	{"SYMBOLP", SW("clLsymbolp",clLsymbolp,cl)},
	{"ATOM", SW("clLatom",clLatom,cl)},
	{"CONSP", SW("clLconsp",clLconsp,cl)},
	{"LISTP", SW("clLlistp",clLlistp,cl)},
	{"NUMBERP", SW("clLnumberp",clLnumberp,cl)},
	{"INTEGERP", SW("clLintegerp",clLintegerp,cl)},
	{"RATIONALP", SW("clLrationalp",clLrationalp,cl)},
	{"FLOATP", SW("clLfloatp",clLfloatp,cl)},
	{"REALP", SW("clLrealp",clLrealp,cl)},
	{"COMPLEXP", SW("clLcomplexp",clLcomplexp,cl)},
	{"CHARACTERP", SW("clLcharacterp",clLcharacterp,cl)},
	{"STRINGP", SW("clLstringp",clLstringp,cl)},
	{"BIT-VECTOR-P", SW("clLbit_vector_p",clLbit_vector_p,cl)},
	{"VECTORP", SW("clLvectorp",clLvectorp,cl)},
	{"SIMPLE-STRING-P", SW("clLsimple_string_p",clLsimple_string_p,cl)},
	{"SIMPLE-BIT-VECTOR-P", SW("clLsimple_bit_vector_p",clLsimple_bit_vector_p,cl)},
	{"SIMPLE-VECTOR-P", SW("clLsimple_vector_p",clLsimple_vector_p,cl)},
	{"ARRAYP", SW("clLarrayp",clLarrayp,cl)},
	{"PACKAGEP", SW("clLpackagep",clLpackagep,cl)},
	{"FUNCTIONP", SW("clLfunctionp",clLfunctionp,cl)},
	{"COMPILED-FUNCTION-P", SW("clLcompiled_function_p",clLcompiled_function_p,cl)},
	{"COMMONP", SW("clLcommonp",clLcommonp,cl)},

	{"EQ", SW("clLeq",clLeq,cl)},
	{"EQL", SW("clLeql",clLeql,cl)},
	{"EQUAL", SW("clLequal",clLequal,cl)},
	{"EQUALP", SW("clLequalp",clLequalp,cl)},

	{"NOT", SW("clLnull",clLnull,cl)},

	{"SI::FIXNUMP", SW("siLfixnump",siLfixnump,si)},

	/* print.d */

	{"WRITE", SW("clLwrite",clLwrite,cl)},
	{"PRIN1", SW("clLprin1",clLprin1,cl)},
	{"PRINT", SW("clLprint",clLprint,cl)},
	{"PPRINT", SW("clLpprint",clLpprint,cl)},
	{"PRINC", SW("clLprinc",clLprinc,cl)},
	{"WRITE-CHAR", SW("clLwrite_char",clLwrite_char,cl)},
	{"WRITE-STRING", SW("clLwrite_string",clLwrite_string,cl)},
	{"WRITE-LINE", SW("clLwrite_line",clLwrite_line,cl)},
	{"WRITE-BYTE", SW("clLwrite_byte",clLwrite_byte,cl)},
	{"SI::WRITE-BYTES", SW("siLwrite_bytes",siLwrite_bytes,si)},
	{"TERPRI", SW("clLterpri",clLterpri,cl)},
	{"FRESH-LINE", SW("clLfresh_line",clLfresh_line,cl)},
	{"FINISH-OUTPUT", SW("clLforce_output",clLforce_output,cl)},
	{"FORCE-OUTPUT", SW("clLforce_output",clLforce_output,cl)},
	{"CLEAR-OUTPUT", SW("clLclear_output",clLclear_output,cl)},

	/* profile.c */
#ifdef PROFILE
	{"SI::PROFILE", SW("siLprofile",siLprofile,si)},
	{"SI::CLEAR-PROFILE", SW("siLclear_profile",siLclear_profile,si)},
	{"SI::DISPLAY-PROFILE", SW("siLdisplay_profile",siLdisplay_profile,si)},
#endif /* PROFILE */

	/* prog.c */

	{"TAGBODY", SW("NULL",NULL,form)},
	{"PROG", SW("NULL",NULL,form)},
	{"PROG*", SW("NULL",NULL,form)},
	{"GO", SW("NULL",NULL,form)},
	{"PROGV", SW("NULL",NULL,form)},
	{"PROGN", SW("NULL",NULL,form)},
	{"PROG1", SW("NULL",NULL,form)},
	{"PROG2", SW("NULL",NULL,form)},

	/* read.d */

	{"READ", SW("clLread",clLread,cl)},
	{"READ-PRESERVING-WHITESPACE", SW("clLread_preserving_whitespace",clLread_preserving_whitespace,cl)},
	{"READ-DELIMITED-LIST", SW("clLread_delimited_list",clLread_delimited_list,cl)},
	{"READ-LINE", SW("clLread_line",clLread_line,cl)},
	{"READ-CHAR", SW("clLread_char",clLread_char,cl)},
	{"UNREAD-CHAR", SW("clLunread_char",clLunread_char,cl)},
	{"PEEK-CHAR", SW("clLpeek_char",clLpeek_char,cl)},
	{"LISTEN", SW("clLlisten",clLlisten,cl)},
	{"READ-CHAR-NO-HANG", SW("clLread_char_no_hang",clLread_char_no_hang,cl)},
	{"CLEAR-INPUT", SW("clLclear_input",clLclear_input,cl)},

	{"PARSE-INTEGER", SW("clLparse_integer",clLparse_integer,cl)},

	{"READ-BYTE", SW("clLread_byte",clLread_byte,cl)},
	{"SI::READ-BYTES", SW("siLread_bytes",siLread_bytes,si)},

	{"COPY-READTABLE", SW("clLcopy_readtable",clLcopy_readtable,cl)},
	{"READTABLEP", SW("clLreadtablep",clLreadtablep,cl)},
	{"SET-SYNTAX-FROM-CHAR", SW("clLset_syntax_from_char",clLset_syntax_from_char,cl)},
	{"SET-MACRO-CHARACTER", SW("clLset_macro_character",clLset_macro_character,cl)},
	{"GET-MACRO-CHARACTER", SW("clLget_macro_character",clLget_macro_character,cl)},
	{"MAKE-DISPATCH-MACRO-CHARACTER", SW("clLmake_dispatch_macro_character",clLmake_dispatch_macro_character,cl)},
	{"SET-DISPATCH-MACRO-CHARACTER", SW("clLset_dispatch_macro_character",clLset_dispatch_macro_character,cl)},
	{"GET-DISPATCH-MACRO-CHARACTER", SW("clLget_dispatch_macro_character",clLget_dispatch_macro_character,cl)},
	{"SI::STRING-TO-OBJECT", SW("siLstring_to_object",siLstring_to_object,si)},
	{"SI::STANDARD-READTABLE", SW("siLstandard_readtable",siLstandard_readtable,si)},

	/* reference.c */

	{"SYMBOL-FUNCTION", SW("clLsymbol_function",clLsymbol_function,cl)},
	{"FBOUNDP", SW("clLfboundp",clLfboundp,cl)},
	{"QUOTE", SW("NULL",NULL,form)},
	{"SYMBOL-VALUE", SW("clLsymbol_value",clLsymbol_value,cl)},
	{"BOUNDP", SW("clLboundp",clLboundp,cl)},
	{"MACRO-FUNCTION", SW("clLmacro_function",clLmacro_function,cl)},
	{"SPECIAL-FORM-P", SW("clLspecial_form_p",clLspecial_form_p,cl)},
	{"SI::COERCE-TO-FUNCTION", SW("siLcoerce_to_function",siLcoerce_to_function,si)},
	{"FUNCTION", SW("NULL",NULL,form)},
	{"SI::PROCESS-DECLARATIONS", SW("siLprocess_declarations",siLprocess_declarations,si)},
	{"SI::PROCESS-LAMBDA-LIST", SW("siLprocess_lambda_list",siLprocess_lambda_list,si)},

	/* sequence.d */

	{"ELT", SW("clLelt",clLelt,cl)},
	{"SI::ELT-SET", SW("siLelt_set",siLelt_set,si)},
	{"SUBSEQ", SW("clLsubseq",clLsubseq,cl)},
	{"COPY-SEQ", SW("clLcopy_seq",clLcopy_seq,cl)},
	{"LENGTH", SW("clLlength",clLlength,cl)},
	{"REVERSE", SW("clLreverse",clLreverse,cl)},
	{"NREVERSE", SW("clLnreverse",clLnreverse,cl)},

	/* stacks.c */

	{"SI::IHS-TOP", SW("siLihs_top",siLihs_top,si)},
	{"SI::IHS-FUN", SW("siLihs_fun",siLihs_fun,si)},
	{"SI::IHS-ENV", SW("siLihs_env",siLihs_env,si)},
	{"SI::IHS-NEXT", SW("siLihs_next",siLihs_next,si)},
	{"SI::IHS-PREV", SW("siLihs_prev",siLihs_prev,si)},
	{"SI::FRS-TOP", SW("siLfrs_top",siLfrs_top,si)},
	{"SI::FRS-BDS", SW("siLfrs_bds",siLfrs_bds,si)},
	{"SI::FRS-CLASS", SW("siLfrs_class",siLfrs_class,si)},
	{"SI::FRS-TAG", SW("siLfrs_tag",siLfrs_tag,si)},
	{"SI::FRS-IHS", SW("siLfrs_ihs",siLfrs_ihs,si)},
	{"SI::BDS-TOP", SW("siLbds_top",siLbds_top,si)},
	{"SI::BDS-VAR", SW("siLbds_var",siLbds_var,si)},
	{"SI::BDS-VAL", SW("siLbds_val",siLbds_val,si)},
	{"SI::SCH-FRS-BASE", SW("siLsch_frs_base",siLsch_frs_base,si)},
	{"SI::RESET-STACK-LIMITS", SW("siLreset_stack_limits",siLreset_stack_limits,si)},

	/* string.d */

	{"CHAR", SW("clLchar",clLchar,cl)},
	{"SI::CHAR-SET", SW("siLchar_set",siLchar_set,si)},
	{"SCHAR", SW("clLchar",clLchar,cl)},
	{"SI::SCHAR-SET", SW("siLchar_set",siLchar_set,si)},
	{"STRING=", SW("clLstringE",clLstringE,cl)},
	{"STRING-EQUAL", SW("clLstring_equal",clLstring_equal,cl)},
	{"STRING<", SW("clLstringL",clLstringL,cl)},
	{"STRING>", SW("clLstringG",clLstringG,cl)},
	{"STRING<=", SW("clLstringLE",clLstringLE,cl)},
	{"STRING>=", SW("clLstringGE",clLstringGE,cl)},
	{"STRING/=", SW("clLstringNE",clLstringNE,cl)},
	{"STRING-LESSP", SW("clLstring_lessp",clLstring_lessp,cl)},
	{"STRING-GREATERP", SW("clLstring_greaterp",clLstring_greaterp,cl)},
	{"STRING-NOT-LESSP", SW("clLstring_not_lessp",clLstring_not_lessp,cl)},
	{"STRING-NOT-GREATERP", SW("clLstring_not_greaterp",clLstring_not_greaterp,cl)},
	{"STRING-NOT-EQUAL", SW("clLstring_not_equal",clLstring_not_equal,cl)},
	{"MAKE-STRING", SW("clLmake_string",clLmake_string,cl)},
	{"STRING-TRIM", SW("clLstring_trim",clLstring_trim,cl)},
	{"STRING-LEFT-TRIM", SW("clLstring_left_trim",clLstring_left_trim,cl)},
	{"STRING-RIGHT-TRIM", SW("clLstring_right_trim",clLstring_right_trim,cl)},
	{"STRING-UPCASE", SW("clLstring_upcase",clLstring_upcase,cl)},
	{"STRING-DOWNCASE", SW("clLstring_downcase",clLstring_downcase,cl)},
	{"STRING-CAPITALIZE", SW("clLstring_capitalize",clLstring_capitalize,cl)},
	{"NSTRING-UPCASE", SW("clLnstring_upcase",clLnstring_upcase,cl)},
	{"NSTRING-DOWNCASE", SW("clLnstring_downcase",clLnstring_downcase,cl)},
	{"NSTRING-CAPITALIZE", SW("clLnstring_capitalize",clLnstring_capitalize,cl)},
	{"STRING", SW("clLstring",clLstring,cl)},
	{"SI::STRING-CONCATENATE", SW("siLstring_concatenate",siLstring_concatenate,si)},

	/* structure.c */

	{"SI::MAKE-STRUCTURE", SW("siLmake_structure",siLmake_structure,si)},
	{"SI::COPY-STRUCTURE", SW("siLcopy_structure",siLcopy_structure,si)},
	{"SI::STRUCTURE-NAME", SW("siLstructure_name",siLstructure_name,si)},
	{"SI::STRUCTURE-REF", SW("siLstructure_ref",siLstructure_ref,si)},
	{"SI::STRUCTURE-SET", SW("siLstructure_set",siLstructure_set,si)},
	{"SI::STRUCTUREP", SW("siLstructurep",siLstructurep,si)},
	{"SI::STRUCTURE-SUBTYPE-P", SW("siLstructure_subtype_p",siLstructure_subtype_p,si)},
	{"SI::RPLACA-NTHCDR", SW("siLrplaca_nthcdr",siLrplaca_nthcdr,si)},
	{"SI::LIST-NTH", SW("siLlist_nth",siLlist_nth,si)},

	/* symbol.d */

	{"GET", SW("clLget",clLget,cl)},
	{"REMPROP", SW("clLremprop",clLremprop,cl)},
	{"SYMBOL-PLIST", SW("clLsymbol_plist",clLsymbol_plist,cl)},
	{"GETF", SW("clLgetf",clLgetf,cl)},
	{"GET-PROPERTIES", SW("clLget_properties",clLget_properties,cl)},
	{"SYMBOL-NAME", SW("clLsymbol_name",clLsymbol_name,cl)},
	{"MAKE-SYMBOL", SW("clLmake_symbol",clLmake_symbol,cl)},
	{"COPY-SYMBOL", SW("clLcopy_symbol",clLcopy_symbol,cl)},
	{"GENSYM", SW("clLgensym",clLgensym,cl)},
	{"GENTEMP", SW("clLgentemp",clLgentemp,cl)},
	{"SYMBOL-PACKAGE", SW("clLsymbol_package",clLsymbol_package,cl)},
	{"KEYWORDP", SW("clLkeywordp",clLkeywordp,cl)},
	{"SI::PUT-F", SW("siLput_f",siLput_f,si)},
	{"SI::REM-F", SW("siLrem_f",siLrem_f,si)},
	{"SI::SET-SYMBOL-PLIST", SW("siLset_symbol_plist",siLset_symbol_plist,si)},
	{"SI::PUTPROP", SW("siLputprop",siLputprop,si)},
	{"SI::PUT-PROPERTIES", SW("siLput_properties",siLput_properties,si)},

	/* tcp.c */
#ifdef TCP
	{"SI::OPEN-CLIENT-STREAM", SW("siLopen_client_stream",siLopen_client_stream,si)},
	{"SI::OPEN-SERVER-STREAM", SW("siLopen_server_stream",siLopen_server_stream,si)},
#endif

	/* time.c */

	{"GET-UNIVERSAL-TIME", SW("clLget_universal_time",clLget_universal_time,cl)},
	{"SLEEP", SW("clLsleep",clLsleep,cl)},
	{"GET-INTERNAL-RUN-TIME", SW("clLget_internal_run_time",clLget_internal_run_time,cl)},
	{"GET-INTERNAL-REAL-TIME", SW("clLget_internal_real_time",clLget_internal_real_time,cl)},
	{"SI::GET-LOCAL-TIME-ZONE", SW("siLget_local_time_zone",siLget_local_time_zone,si)},
	{"SI::DAYLIGHT-SAVING-TIME-P", SW("siLdaylight_saving_time_p",siLdaylight_saving_time_p,si)},

	/* toplevel.c */

	{"LAMBDA", SW("NULL",NULL,form)},
	{"NAMED-LAMBDA", SW("NULL",NULL,form)},
	{"SI::*MAKE-SPECIAL", SW("siLXmake_special",siLXmake_special,si)},
	{"SI::*MAKE-CONSTANT", SW("siLXmake_constant",siLXmake_constant,si)},
	{"EVAL-WHEN", SW("NULL",NULL,form)},
	{"THE", SW("NULL",NULL,form)},
	{"DECLARE", SW("NULL",NULL,form)},
	{"LOCALLY", SW("NULL",NULL,form)},

	/* typespec.c */

	{"TYPE-OF", SW("clLtype_of",clLtype_of,cl)},

	/* unixint.c */

#ifdef unix
	{"SI::CATCH-BAD-SIGNALS", SW("siLcatch_bad_signals",siLcatch_bad_signals,si)},
	{"SI::UNCATCH-BAD-SIGNALS", SW("siLuncatch_bad_signals",siLuncatch_bad_signals,si)},
#endif /* unix */

	/* unixfsys.c */

	{"TRUENAME", SW("clLtruename",clLtruename,cl)},
	{"RENAME-FILE", SW("clLrename_file",clLrename_file,cl)},
	{"DELETE-FILE", SW("clLdelete_file",clLdelete_file,cl)},
	{"PROBE-FILE", SW("clLprobe_file",clLprobe_file,cl)},
	{"FILE-WRITE-DATE", SW("clLfile_write_date",clLfile_write_date,cl)},
	{"FILE-AUTHOR", SW("clLfile_author",clLfile_author,cl)},
	{"USER-HOMEDIR-PATHNAME", SW("clLuser_homedir_pathname",clLuser_homedir_pathname,cl)},
	{"SI::STRING-MATCH", SW("siLstring_match",siLstring_match,si)},
	{"DIRECTORY", SW("clLdirectory",clLdirectory,cl)},
	{"SI::CHDIR", SW("siLchdir",siLchdir,si)},
	{"SI::MKDIR", SW("siLmkdir",siLmkdir,si)},

	/* unixsys.c */

	{"SI::SYSTEM", SW("siLsystem",siLsystem,si)},
	{"SI::OPEN-PIPE", SW("siLopen_pipe",siLopen_pipe,si)},

	/* end of list */
	{NULL, SW(NULL, NULL, 0)}
};


