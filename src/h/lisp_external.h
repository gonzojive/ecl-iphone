
#ifndef _ARGS
#define _ARGS(x) (int n, ...)
#endif

/* alloc.c */

#if !defined(GBC_BOEHM)
extern cl_object siLallocate _ARGS((int narg, cl_object type, cl_object qty, ...));
extern cl_object siLmaxpage _ARGS((int narg, cl_object type));
extern cl_object siLallocated_pages _ARGS((int narg, cl_object type));
extern cl_object siLalloc_contpage _ARGS((int narg, cl_object qty, ...));
extern cl_object siLncbpage _ARGS((int narg));
extern cl_object siLmaxcbpage _ARGS((int narg));
extern cl_object siLalloc_relpage _ARGS((int narg, cl_object qty, cl_object now));
extern cl_object siLnrbpage _ARGS((int narg));
extern cl_object siLget_hole_size _ARGS((int narg));
extern cl_object siLset_hole_size _ARGS((int narg, cl_object size));
extern cl_object siLignore_maximum_pages _ARGS((int narg, ...));
#endif

/* alloc_2.c */

#ifdef GBC_BOEHM
extern cl_object siVgc_verbose;
extern cl_object siVgc_message;
extern cl_object clLgc _ARGS((int narg, cl_object area));
extern cl_object siLroom_report _ARGS((int narg));
#endif /* GBC_BOEHM */

/* all_symbols.c */

extern cl_object siLmangle_name _ARGS((int narg, cl_object symbol, ...));

/* array.c */

extern cl_object clSbyte8, clSinteger8;
extern cl_object clLaref _ARGS((int narg, cl_object x, ...));
extern cl_object siLaset _ARGS((int narg, cl_object v, cl_object x, ...));
extern cl_object clLrow_major_aref _ARGS((int narg, cl_object x, cl_object i));
extern cl_object siLrow_major_aset _ARGS((int narg, cl_object x, cl_object i, cl_object v));
extern cl_object siLmake_pure_array _ARGS((int narg, cl_object etype, cl_object adj, cl_object displ, cl_object disploff, ...));
extern cl_object siLmake_vector _ARGS((int narg, cl_object etype, cl_object dim, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff));
extern cl_object clLarray_element_type _ARGS((int narg, cl_object a));
extern cl_object clLarray_rank _ARGS((int narg, cl_object a));
extern cl_object clLarray_dimension _ARGS((int narg, cl_object a, cl_object index));
extern cl_object clLarray_total_size _ARGS((int narg, cl_object a));
extern cl_object clLadjustable_array_p _ARGS((int narg, cl_object a));
extern cl_object siLdisplaced_array_p _ARGS((int narg, cl_object a));
extern cl_object clLsvref _ARGS((int narg, cl_object x, cl_object index));
extern cl_object siLsvset _ARGS((int narg, cl_object x, cl_object index, cl_object v));
extern cl_object clLarray_has_fill_pointer_p _ARGS((int narg, cl_object a));
extern cl_object clLfill_pointer _ARGS((int narg, cl_object a));
extern cl_object siLfill_pointer_set _ARGS((int narg, cl_object a, cl_object fp));
extern cl_object siLreplace_array _ARGS((int narg, cl_object old, cl_object new));

/* assignment.c */

extern cl_object clSsetf, clSpsetf, siSsetf_symbol;
extern cl_object siSsetf_lambda, siSsetf_method, siSsetf_update;
extern cl_object siSclear_compiler_properties;
#ifdef PDE
extern cl_object siVrecord_source_pathname_p;
extern cl_object siSrecord_source_pathname;
#endif
extern cl_object clLset _ARGS((int narg, cl_object var, cl_object val));
extern cl_object siLsetf_namep _ARGS((int narg, cl_object arg));
extern cl_object siLfset _ARGS((int narg, cl_object fun, cl_object def, ...));
extern cl_object clLmakunbound _ARGS((int narg, cl_object sym));
extern cl_object clLfmakunbound _ARGS((int narg, cl_object sym));
extern cl_object siLclear_compiler_properties _ARGS((int narg, cl_object sym));

/* backq.c */

extern cl_object siScomma;
extern cl_object siScomma_at;
extern cl_object siScomma_dot;
extern cl_object clSlistX;
extern cl_object clSappend;
extern cl_object clSnconc;
extern cl_object clLcomma_reader _ARGS((int narg, cl_object in, cl_object c));
extern cl_object clLbackquote_reader _ARGS((int narg, cl_object in, cl_object c));

/* cfun.c */

#ifdef PDE
extern cl_object siSdefun;
extern cl_object siSdefmacro;
#endif
extern cl_object siLcompiled_function_name _ARGS((int narg, cl_object fun));
extern cl_object siLcompiled_function_block _ARGS((int narg, cl_object fun));
extern cl_object siLcompiled_function_source _ARGS((int narg, cl_object fun));

/* character.c */

extern cl_object clLstandard_char_p _ARGS((int narg, cl_object c));
extern cl_object clLgraphic_char_p _ARGS((int narg, cl_object c));
extern cl_object clLalpha_char_p _ARGS((int narg, cl_object c));
extern cl_object clLupper_case_p _ARGS((int narg, cl_object c));
extern cl_object clLlower_case_p _ARGS((int narg, cl_object c));
extern cl_object clLboth_case_p _ARGS((int narg, cl_object c));
extern cl_object clLdigit_char_p _ARGS((int narg, cl_object c, ...));
extern cl_object clLalphanumericp _ARGS((int narg, cl_object c));
extern cl_object clLcharE _ARGS((int narg, cl_object c, ...));
extern cl_object clLcharNE _ARGS((int narg, ...));
extern cl_object clLcharL _ARGS((int narg, ...));
extern cl_object clLcharG _ARGS((int narg, ...));
extern cl_object clLcharLE _ARGS((int narg, ...));
extern cl_object clLcharGE _ARGS((int narg, ...));
extern cl_object clLchar_equal _ARGS((int narg, cl_object c, ...));
extern cl_object clLchar_not_equal _ARGS((int narg, ...));
extern cl_object clLchar_lessp _ARGS((int narg, ...));
extern cl_object clLchar_greaterp _ARGS((int narg, ...));
extern cl_object clLchar_not_greaterp _ARGS((int narg, ...));
extern cl_object clLchar_not_lessp _ARGS((int narg, ...));
extern cl_object clLcharacter _ARGS((int narg, cl_object x));
extern cl_object clLchar_code _ARGS((int narg, cl_object c));
extern cl_object clLcode_char _ARGS((int narg, cl_object c));
extern cl_object clLchar_upcase _ARGS((int narg, cl_object c));
extern cl_object clLchar_downcase _ARGS((int narg, cl_object c));
extern cl_object clLdigit_char _ARGS((int narg, cl_object w, ...));
extern cl_object clLchar_int _ARGS((int narg, cl_object c));
extern cl_object clLint_char _ARGS((int narg, cl_object x));
extern cl_object clLchar_name _ARGS((int narg, cl_object c));
extern cl_object clLname_char _ARGS((int narg, cl_object s));

/* clos.c */

#ifdef CLOS
extern cl_object siVclass_name_hash_table;
extern cl_object clSclass;
extern cl_object clSbuilt_in;
#endif

/* cmpaux.c */

extern cl_object clSAoptional;
extern cl_object clSArest;
extern cl_object clSAkey;
extern cl_object clSAallow_other_keys;
extern cl_object clSAaux;
extern cl_object Kallow_other_keys;
extern cl_object siLspecialp _ARGS((int narg, cl_object sym));

/* compiler.c */

extern cl_object clSlambda_block;
extern cl_object clSdeclare;
extern cl_object clScompile;
extern cl_object clSload;
extern cl_object clSeval;
extern cl_object clSprogn;
extern cl_object clSwarn;
extern cl_object clStypep;
extern cl_object Kexecute;
extern cl_object Kcompile_toplevel;
extern cl_object Kload_toplevel;
extern cl_object clSotherwise;
extern cl_object siVkeep_definitions;
extern cl_object siLprocess_declarations _ARGS((int narg, cl_object body, ...));
extern cl_object siLprocess_lambda_list _ARGS((int narg, cl_object lambda));
extern cl_object siLmake_lambda _ARGS((int narg, cl_object name, cl_object body));
extern cl_object siLfunction_block_name _ARGS((int narg, cl_object name));

/* disassembler.c */

extern cl_object siLbc_disassemble _ARGS((int narg, cl_object v));
extern cl_object siLbc_split _ARGS((int narg, cl_object v));

/* error.c */

extern cl_object clSarithmetic_error, clScell_error, clScondition;
extern cl_object clScontrol_error, clSdivision_by_zero, clSend_of_file;
extern cl_object clSerror, clSfile_error, clSfloating_point_inexact;
extern cl_object clSfloating_point_invalid_operation, clSfloating_point_overflow;
extern cl_object clSfloating_point_underflow, clSpackage_error, clSparse_error;
extern cl_object clSprint_not_readable, clSprogram_error, clSreader_error;
extern cl_object clSserious_condition, clSsimple_condition, clSsimple_error;
extern cl_object clSsimple_type_error, clSsimple_warning, clSstorage_condition;
extern cl_object clSstream_error, clSstyle_warning, clStype_error, clSunbound_slot;
extern cl_object clSunbound_variable, clSundefined_function, clSwarning;

extern cl_object siSsimple_program_error, siSsimple_control_error;

extern cl_object Kdatum, Kexpected_type;
extern cl_object Kpathname;
extern cl_object Kformat_control, Kformat_arguments;

extern cl_object siSuniversal_error_handler;
extern cl_object siSterminal_interrupt;
extern cl_object clLerror _ARGS((int narg, cl_object eformat, ...));
extern cl_object clLcerror _ARGS((int narg, cl_object cformat, cl_object eformat, ...));

/* eval.c */

extern cl_object clSapply;
extern cl_object clSfuncall;
extern cl_object siLunlink_symbol _ARGS((int narg, cl_object s));
extern cl_object clLfuncall _ARGS((int narg, cl_object fun, ...));
extern cl_object clLapply _ARGS((int narg, cl_object fun, cl_object arg, ...));
extern cl_object clLeval _ARGS((int narg, cl_object form));
extern cl_object siLeval_with_env _ARGS((int n, cl_object form, cl_object env));
extern cl_object clLconstantp _ARGS((int narg, cl_object arg));

/* file.c */

extern cl_object Kerror;
extern cl_object clVstandard_input;
extern cl_object clVstandard_output;
extern cl_object clVerror_output;
extern cl_object clVquery_io;
extern cl_object clVdebug_io;
extern cl_object clVterminal_io;
extern cl_object clVtrace_output;
extern cl_object Kabort;
extern cl_object Kdirection;
extern cl_object Kinput;
extern cl_object Koutput;
extern cl_object Kio;
extern cl_object Kprobe;
extern cl_object Kelement_type;
extern cl_object Kdefault;
extern cl_object Kif_exists;
extern cl_object Knew_version;
extern cl_object Krename;
extern cl_object Krename_and_delete;
extern cl_object Koverwrite;
extern cl_object Kappend;
extern cl_object Ksupersede;
extern cl_object Kcreate;
extern cl_object Kprint;
extern cl_object Kif_does_not_exist;
extern cl_object Kset_default_pathname;
extern cl_object siVignore_eof_on_terminal_io;
extern cl_object clLmake_synonym_stream _ARGS((int narg, cl_object sym));
extern cl_object clLmake_broadcast_stream _ARGS((int narg, ...));
extern cl_object clLmake_concatenated_stream _ARGS((int narg, ...));
extern cl_object clLmake_two_way_stream _ARGS((int narg, cl_object strm1, cl_object strm2));
extern cl_object clLmake_echo_stream _ARGS((int narg, cl_object strm1, cl_object strm2));
extern cl_object clLmake_string_input_stream _ARGS((int narg, cl_object strng, ...));
extern cl_object clLmake_string_output_stream _ARGS((int narg));
extern cl_object clLget_output_stream_string _ARGS((int narg, cl_object strm));
extern cl_object siLoutput_stream_string _ARGS((int narg, cl_object strm));
extern cl_object clLstreamp _ARGS((int narg, cl_object strm));
extern cl_object clLinput_stream_p _ARGS((int narg, cl_object strm));
extern cl_object clLoutput_stream_p _ARGS((int narg, cl_object strm));
extern cl_object clLstream_element_type _ARGS((int narg, cl_object strm));
extern cl_object clLclose _ARGS((int narg, cl_object strm, ...));
extern cl_object clLopen _ARGS((int narg, cl_object filename, ...));
extern cl_object clLfile_position _ARGS((int narg, cl_object file_stream, ...));
extern cl_object clLfile_length _ARGS((int narg, cl_object strm));
extern cl_object siLget_string_input_stream_index _ARGS((int narg, cl_object strm));
extern cl_object siLmake_string_output_stream_from_string _ARGS((int narg, cl_object strng));
extern cl_object siLcopy_stream _ARGS((int narg, cl_object in, cl_object out));
extern cl_object clLopen_stream_p _ARGS((int narg, cl_object strm));

/* format.c */

extern cl_object siVindent_formatted_output;
extern cl_object clLformat _ARGS((int narg, volatile cl_object strm, cl_object string, ...));

/* gbc.c */

#if !defined(GBC_BOEHM)
extern cl_object siVgc_verbose;
extern cl_object siVgc_message;
extern cl_object clLgc _ARGS((int narg, cl_object area));
extern cl_object siLroom_report _ARGS((int narg));
extern cl_object siLreset_gc_count _ARGS((int narg));
extern cl_object siLgc_time _ARGS((int narg));
#endif /* !GBC_BOHEM */

/* gfun.c */

extern cl_object siScompute_applicable_methods;
extern cl_object siScompute_effective_method;
extern cl_object siSgeneric_function_method_combination;
extern cl_object siSgeneric_function_method_combination_args;
extern cl_object siLallocate_gfun _ARGS((int narg, cl_object name, cl_object arg_no, cl_object ht));
extern cl_object siLgfun_name _ARGS((int narg, cl_object x));
extern cl_object siLgfun_name_set _ARGS((int narg, cl_object x, cl_object name));
extern cl_object siLgfun_method_ht _ARGS((int narg, cl_object x));
extern cl_object siLgfun_method_ht_set _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLgfun_spec_how_ref _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLgfun_spec_how_set _ARGS((int narg, cl_object x, cl_object y, cl_object spec));
extern cl_object siLgfun_instance _ARGS((int narg, cl_object x));
extern cl_object siLgfun_instance_set _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLgfunp _ARGS((int narg, cl_object x));
extern cl_object siLmethod_ht_get _ARGS((int narg, cl_object keylist, cl_object table));
extern cl_object siLset_compiled_function_name _ARGS((int narg, cl_object keylist, cl_object table));

/* hash.c */

extern cl_object clSeq;
extern cl_object clSeql;
extern cl_object clSequal;
extern cl_object Ksize;
extern cl_object Krehash_size;
extern cl_object Krehash_threshold;
extern cl_object clLmake_hash_table _ARGS((int narg, ...));
extern cl_object clLhash_table_p _ARGS((int narg, cl_object ht));
extern cl_object clLgethash _ARGS((int narg, cl_object key, cl_object ht, ...));
extern cl_object siLhash_set _ARGS((int narg, cl_object key, cl_object ht, cl_object val));
extern cl_object clLremhash _ARGS((int narg, cl_object key, cl_object ht));
extern cl_object clLclrhash _ARGS((int narg, cl_object ht));
extern cl_object clLhash_table_count _ARGS((int narg, cl_object ht));
extern cl_object clLsxhash _ARGS((int narg, cl_object key));
extern cl_object clLmaphash _ARGS((int narg, cl_object fun, cl_object ht));
extern cl_object clLhash_table_rehash_size _ARGS((int narg, cl_object ht));
extern cl_object clLhash_table_rehash_threshold _ARGS((int narg, cl_object ht));

/* instance.c */

#ifdef CLOS
extern cl_object clSprint_object;
extern cl_object siLallocate_instance _ARGS((int narg, cl_object class, cl_object size));
extern cl_object siLchange_instance _ARGS((int narg, cl_object x, cl_object class, cl_object size, cl_object corr));
extern cl_object siLinstance_class _ARGS((int narg, cl_object x));
extern cl_object siLinstance_class_set _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLinstance_ref _ARGS((int narg, cl_object x, cl_object index));
extern cl_object siLinstance_ref_safe _ARGS((int narg, cl_object x, cl_object index));
extern cl_object siLinstance_set _ARGS((int narg, cl_object x, cl_object index, cl_object value));
extern cl_object siLinstancep _ARGS((int narg, cl_object x));
extern cl_object siLsl_boundp _ARGS((int narg, cl_object x));
extern cl_object siLsl_makunbound _ARGS((int narg, cl_object x, cl_object index));
#endif

/* interpreter.c */

extern cl_object siLinterpreter_stack _ARGS((int narg));

/* lex.c */

extern cl_object Kblock;
extern cl_object Ktag;
extern cl_object Kfunction;
extern cl_object clSblock;
extern cl_object clSmacro;
extern cl_object siSsymbol_macro;
extern cl_object clStag;
extern cl_object siLlex_env _ARGS((int narg));

/* list.c */

extern cl_object Ktest;
extern cl_object Ktest_not;
extern cl_object Kkey;
extern cl_object Kinitial_element;
extern cl_object clLcar _ARGS((int narg, cl_object x));
extern cl_object clLcdr _ARGS((int narg, cl_object x));
extern cl_object clLlist _ARGS((int narg, ...));
extern cl_object clLlistX _ARGS((int narg, ...));
extern cl_object clLappend _ARGS((int narg, ...));
extern cl_object clLcaar _ARGS((int narg, cl_object x));
extern cl_object clLcadr _ARGS((int narg, cl_object x));
extern cl_object clLcdar _ARGS((int narg, cl_object x));
extern cl_object clLcddr _ARGS((int narg, cl_object x));
extern cl_object clLcaaar _ARGS((int narg, cl_object x));
extern cl_object clLcaadr _ARGS((int narg, cl_object x));
extern cl_object clLcadar _ARGS((int narg, cl_object x));
extern cl_object clLcaddr _ARGS((int narg, cl_object x));
extern cl_object clLcdaar _ARGS((int narg, cl_object x));
extern cl_object clLcdadr _ARGS((int narg, cl_object x));
extern cl_object clLcddar _ARGS((int narg, cl_object x));
extern cl_object clLcdddr _ARGS((int narg, cl_object x));
extern cl_object clLcaaaar _ARGS((int narg, cl_object x));
extern cl_object clLcaaadr _ARGS((int narg, cl_object x));
extern cl_object clLcaadar _ARGS((int narg, cl_object x));
extern cl_object clLcaaddr _ARGS((int narg, cl_object x));
extern cl_object clLcadaar _ARGS((int narg, cl_object x));
extern cl_object clLcadadr _ARGS((int narg, cl_object x));
extern cl_object clLcaddar _ARGS((int narg, cl_object x));
extern cl_object clLcadddr _ARGS((int narg, cl_object x));
extern cl_object clLcdaaar _ARGS((int narg, cl_object x));
extern cl_object clLcdaadr _ARGS((int narg, cl_object x));
extern cl_object clLcdadar _ARGS((int narg, cl_object x));
extern cl_object clLcdaddr _ARGS((int narg, cl_object x));
extern cl_object clLcddaar _ARGS((int narg, cl_object x));
extern cl_object clLcddadr _ARGS((int narg, cl_object x));
extern cl_object clLcdddar _ARGS((int narg, cl_object x));
extern cl_object clLcddddr _ARGS((int narg, cl_object x));
extern cl_object clLfifth _ARGS((int narg, cl_object x));
extern cl_object clLsixth _ARGS((int narg, cl_object x));
extern cl_object clLseventh _ARGS((int narg, cl_object x));
extern cl_object clLeighth _ARGS((int narg, cl_object x));
extern cl_object clLninth _ARGS((int narg, cl_object x));
extern cl_object clLtenth _ARGS((int narg, cl_object x));
extern cl_object clLcons _ARGS((int narg, cl_object car, cl_object cdr));
extern cl_object clLtree_equal _ARGS((int narg, cl_object x, cl_object y, ...));
extern cl_object clLendp _ARGS((int narg, cl_object x));
extern cl_object clLlist_length _ARGS((int narg, cl_object x));
extern cl_object clLnth _ARGS((int narg, cl_object n, cl_object x));
extern cl_object clLnthcdr _ARGS((int narg, cl_object n, cl_object x));
extern cl_object clLlast _ARGS((int narg, cl_object x, ...));
extern cl_object clLmake_list _ARGS((int narg, cl_object size, ...));
extern cl_object clLcopy_list _ARGS((int narg, cl_object x));
extern cl_object clLcopy_alist _ARGS((int narg, cl_object x));
extern cl_object clLcopy_tree _ARGS((int narg, cl_object x));
extern cl_object clLrevappend _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLnconc _ARGS((int narg, ...));
extern cl_object clLnreconc _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLbutlast _ARGS((int narg, cl_object lis, ...));
extern cl_object clLnbutlast _ARGS((int narg, cl_object lis, ...));
extern cl_object clLldiff _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLrplaca _ARGS((int narg, cl_object x, cl_object v));
extern cl_object clLrplacd _ARGS((int narg, cl_object x, cl_object v));
extern cl_object clLsubst _ARGS((int narg, cl_object new, cl_object old, cl_object tree, ...));
extern cl_object clLsubst_if _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object clLsubst_if_not _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object clLnsubst _ARGS((int narg, cl_object new, cl_object old, cl_object tree, ...));
extern cl_object clLnsubst_if _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object clLnsubst_if_not _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object clLsublis _ARGS((int narg, cl_object alist, cl_object tree, ...));
extern cl_object clLnsublis _ARGS((int narg, cl_object alist, cl_object tree, ...));
extern cl_object clLmember _ARGS((int narg, cl_object item, cl_object list, ...));
extern cl_object siLmemq _ARGS((int narg, cl_object x, cl_object l));
extern cl_object clLmember_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object clLmember_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object siLmember1 _ARGS((int narg, cl_object item, cl_object list, ...));
extern cl_object clLtailp _ARGS((int narg, cl_object y, cl_object x));
extern cl_object clLadjoin _ARGS((int narg, cl_object item, cl_object list, cl_object k1, cl_object v1, cl_object k2, cl_object v2, cl_object k3, cl_object v3));
extern cl_object clLacons _ARGS((int narg, cl_object x, cl_object y, cl_object z));
extern cl_object clLpairlis _ARGS((int narg, cl_object keys, cl_object data, ...));
extern cl_object clLrassoc _ARGS((int narg, cl_object item, cl_object alist, ...));
extern cl_object clLassoc _ARGS((int narg, cl_object item, cl_object alist, ...));
extern cl_object clLassoc_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object clLassoc_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object clLrassoc_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object clLrassoc_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));

/* load.c */

extern cl_object Kverbose;
extern cl_object clVload_verbose, clVload_print;
extern cl_object siVload_hooks;
extern cl_object siVinit_function_prefix;
extern cl_object clLload _ARGS((int narg, cl_object pathname, ...));
extern cl_object siLload_source _ARGS((int narg, cl_object file, cl_object verbose,
				       cl_object print));
extern cl_object siLload_binary _ARGS((int narg, cl_object file, cl_object verbose,
				       cl_object print));

/* lwp.c */

#ifdef THREADS
extern cl_object clSrunning;
extern cl_object clSsuspended;
extern cl_object clSwaiting;
extern cl_object clSstopped;
extern cl_object clSdead;
extern cl_object siSthread_top_level;
extern cl_object siLthread_break_in _ARGS((int narg));
extern cl_object siLthread_break_quit _ARGS((int narg));
extern cl_object siLthread_break_resume _ARGS((int narg));
extern cl_object clLthread_list _ARGS((int narg));
extern cl_object clLmake_thread _ARGS((int narg, cl_object fun));
extern cl_object clLdeactivate _ARGS((int narg, cl_object thread));
extern cl_object clLreactivate _ARGS((int narg, cl_object thread));
extern cl_object clLkill_thread _ARGS((int narg, cl_object thread));
extern cl_object clLcurrent_thread _ARGS((int narg));
extern cl_object clLthread_status _ARGS((int narg, cl_object thread));
extern cl_object clLmake_continuation _ARGS((int narg, cl_object thread));
extern cl_object clLthread_of _ARGS((int narg, cl_object cont));
extern cl_object clLcontinuation_of _ARGS((int narg, cl_object thread));
extern cl_object clLresume _ARGS((int narg, cl_object cont, ...));
extern cl_object clLdisable_scheduler _ARGS((int narg));
extern cl_object clLenable_scheduler _ARGS((int narg));
extern cl_object clLsuspend _ARGS((int narg));
extern cl_object clLdelay _ARGS((int narg, cl_object interval));
extern cl_object clLthread_wait _ARGS((int narg, cl_object fun, ...));
extern cl_object clLthread_wait_with_timeout _ARGS((int narg, cl_object timeout, cl_object fun, ...));
#endif

/* macros.c */

extern cl_object clVmacroexpand_hook;
extern cl_object siSexpand_defmacro;
extern cl_object siVinhibit_macro_special;
extern cl_object clLmacroexpand _ARGS((int narg, cl_object form, ...));
extern cl_object clLmacroexpand_1 _ARGS((int narg, cl_object form, ...));

/* main.c */

extern cl_object clVfeatures;
extern cl_object clLquit _ARGS((int narg, ...));
extern cl_object siLargc _ARGS((int narg));
extern cl_object siLargv _ARGS((int narg, cl_object index));
extern cl_object siLgetenv _ARGS((int narg, cl_object var));
extern cl_object siLpointer _ARGS((int narg, cl_object x));
extern cl_object siLnani _ARGS((int narg, cl_object x));
extern cl_object clLidentity _ARGS((int narg, cl_object x));

/* mapfun.c */

extern cl_object clLmapcar _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object clLmaplist _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object clLmapc _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object clLmapl _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object clLmapcan _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object clLmapcon _ARGS((int narg, cl_object fun, cl_object onelist, ...));

/* multival.c */

extern cl_object clLvalues _ARGS((int narg, ...));
extern cl_object clLvalues_list _ARGS((int narg, cl_object list));

/* num_arith.c */

extern cl_object clLX _ARGS((int narg, ...));
extern cl_object clLP _ARGS((int narg, ...));
extern cl_object clLM _ARGS((int narg, cl_object num, ...));
extern cl_object clLconjugate _ARGS((int narg, cl_object c));
extern cl_object clLN _ARGS((int narg, cl_object num, ...));
extern cl_object clLgcd _ARGS((int narg, ...));
extern cl_object clL1P _ARGS((int narg, cl_object x));
extern cl_object clL1M _ARGS((int narg, cl_object x));
extern cl_object clLlcm _ARGS((int narg, cl_object lcm, ...));

/* num_co.c */

extern cl_object clLfloat _ARGS((int narg, cl_object x, ...));
extern cl_object clLnumerator _ARGS((int narg, cl_object x));
extern cl_object clLdenominator _ARGS((int narg, cl_object x));
extern cl_object clLfloor _ARGS((int narg, cl_object x, ...));
extern cl_object clLceiling _ARGS((int narg, cl_object x, ...));
extern cl_object clLtruncate _ARGS((int narg, cl_object x, ...));
extern cl_object clLround _ARGS((int narg, cl_object x, ...));
extern cl_object clLmod _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLrem _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLdecode_float _ARGS((int narg, cl_object x));
extern cl_object clLscale_float _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLfloat_radix _ARGS((int narg, cl_object x));
extern cl_object clLfloat_sign _ARGS((int narg, cl_object x, ...));
extern cl_object clLfloat_digits _ARGS((int narg, cl_object x));
extern cl_object clLfloat_precision _ARGS((int narg, cl_object x));
extern cl_object clLinteger_decode_float _ARGS((int narg, cl_object x));
extern cl_object clLcomplex _ARGS((int narg, cl_object r, ...));
extern cl_object clLrealpart _ARGS((int narg, cl_object x));
extern cl_object clLimagpart _ARGS((int narg, cl_object x));

/* num_comp.c */

extern cl_object clLE _ARGS((int narg, cl_object num, ...));
extern cl_object clLNE _ARGS((int narg, ...));
extern cl_object clLL _ARGS((int narg, ...));
extern cl_object clLG _ARGS((int narg, ...));
extern cl_object clLGE _ARGS((int narg, ...));
extern cl_object clLLE _ARGS((int narg, ...));
extern cl_object clLmax _ARGS((int narg, cl_object max, ...));
extern cl_object clLmin _ARGS((int narg, cl_object min, ...));

/* num_log.c */

extern cl_object clLlogior _ARGS((int narg, ...));
extern cl_object clLlogxor _ARGS((int narg, ...));
extern cl_object clLlogand _ARGS((int narg, ...));
extern cl_object clLlogeqv _ARGS((int narg, ...));
extern cl_object clLlognand _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlognor _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlogandc1 _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlogandc2 _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlogorc1 _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlogorc2 _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlognot _ARGS((int narg, cl_object x));
extern cl_object clLboole _ARGS((int narg, cl_object o, cl_object x, cl_object y));
extern cl_object clLlogbitp _ARGS((int narg, cl_object p, cl_object x));
extern cl_object clLash _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlogcount _ARGS((int narg, cl_object x));
extern cl_object clLinteger_length _ARGS((int narg, cl_object x));
extern cl_object siLbit_array_op _ARGS((int narg, cl_object o, cl_object x, cl_object y, cl_object r));

/* num_pred.c */

extern cl_object clLzerop _ARGS((int narg, cl_object x));
extern cl_object clLplusp _ARGS((int narg, cl_object x));
extern cl_object clLminusp _ARGS((int narg, cl_object x));
extern cl_object clLoddp _ARGS((int narg, cl_object x));
extern cl_object clLevenp _ARGS((int narg, cl_object x));

/* num_rand.c */

extern cl_object clVrandom_state;
extern cl_object clLrandom _ARGS((int narg, cl_object x, ...));
extern cl_object clLmake_random_state _ARGS((int narg, ...));
extern cl_object clLrandom_state_p _ARGS((int narg, cl_object x));

/* num_sfun.c */

extern cl_object clLexp _ARGS((int narg, cl_object x));
extern cl_object clLexpt _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLlog _ARGS((int narg, cl_object x, ...));
extern cl_object clLsqrt _ARGS((int narg, cl_object x));
extern cl_object clLsin _ARGS((int narg, cl_object x));
extern cl_object clLcos _ARGS((int narg, cl_object x));
extern cl_object clLtan _ARGS((int narg, cl_object x));
extern cl_object clLatan _ARGS((int narg, cl_object x, ...));
extern cl_object clLsinh _ARGS((int narg, cl_object x));
extern cl_object clLcosh _ARGS((int narg, cl_object x));
extern cl_object clLtanh _ARGS((int narg, cl_object x));

/* package.c */

extern cl_object clVpackage;
extern cl_object Kinternal;
extern cl_object Kexternal;
extern cl_object Kinherited;
extern cl_object Knicknames;
extern cl_object Kuse;
extern cl_object clLmake_package _ARGS((int narg, cl_object pack_name, ...));
extern cl_object siLselect_package _ARGS((int narg, cl_object pack_name));
extern cl_object clLfind_package _ARGS((int narg, cl_object p));
extern cl_object clLpackage_name _ARGS((int narg, cl_object p));
extern cl_object clLpackage_nicknames _ARGS((int narg, cl_object p));
extern cl_object clLrename_package _ARGS((int narg, cl_object pack, cl_object new_name, ...));
extern cl_object clLpackage_use_list _ARGS((int narg, cl_object p));
extern cl_object clLpackage_used_by_list _ARGS((int narg, cl_object p));
extern cl_object clLpackage_shadowing_symbols _ARGS((int narg, cl_object p));
extern cl_object clLlist_all_packages _ARGS((int narg));
extern cl_object clLintern _ARGS((int narg, cl_object strng, ...));
extern cl_object clLfind_symbol _ARGS((int narg, cl_object strng, ...));
extern cl_object clLunintern _ARGS((int narg, cl_object symbl, ...));
extern cl_object clLexport _ARGS((int narg, cl_object symbols, ...));
extern cl_object clLunexport _ARGS((int narg, cl_object symbols, ...));
extern cl_object clLimport _ARGS((int narg, cl_object symbols, ...));
extern cl_object clLshadowing_import _ARGS((int narg, cl_object symbols, ...));
extern cl_object clLshadow _ARGS((int narg, cl_object symbols, ...));
extern cl_object clLuse_package _ARGS((int narg, cl_object pack, ...));
extern cl_object clLunuse_package _ARGS((int narg, cl_object pack, ...));
extern cl_object siLpackage_internal _ARGS((int narg, cl_object p, cl_object index));
extern cl_object siLpackage_external _ARGS((int narg, cl_object p, cl_object index));
extern cl_object siLpackage_size _ARGS((int narg, cl_object p));
extern cl_object siLpackage_lock _ARGS((int narg, cl_object p, cl_object t));
extern cl_object clLdelete_package _ARGS((int narg, cl_object p));

/* pathname.c */

extern cl_object clVdefault_pathname_defaults;
extern cl_object Kwild;
extern cl_object Kwild_inferiors;
extern cl_object Knewest;
extern cl_object Khost;
extern cl_object Kdevice;
extern cl_object Kdirectory;
extern cl_object Kname;
extern cl_object Ktype;
extern cl_object Kversion;
extern cl_object Kdefaults;
extern cl_object Kabsolute;
extern cl_object Krelative;
extern cl_object Kup;
extern cl_object Kper;
extern cl_object Kunspecific;
extern cl_object clLpathname _ARGS((int narg, cl_object name));
extern cl_object clLparse_namestring _ARGS((int narg, cl_object thing, ...));
extern cl_object clLparse_logical_namestring _ARGS((int narg, cl_object thing, ...));
extern cl_object clLmerge_pathnames _ARGS((int narg, cl_object path, ...));
extern cl_object clLmake_pathname _ARGS((int narg, ...));
extern cl_object clLpathnamep _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_host _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_device _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_directory _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_name _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_type _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_version _ARGS((int narg, cl_object pname));
extern cl_object clLnamestring _ARGS((int narg, cl_object pname));
extern cl_object clLfile_namestring _ARGS((int narg, cl_object pname));
extern cl_object clLdirectory_namestring _ARGS((int narg, cl_object pname));
extern cl_object clLhost_namestring _ARGS((int narg, cl_object pname));
extern cl_object clLenough_namestring _ARGS((int narg, cl_object path, ...));
extern cl_object siLlogical_pathname_p _ARGS((int narg, cl_object pname));
extern cl_object clLpathname_match_p _ARGS((int narg, cl_object path, cl_object mask));
extern cl_object siLpathname_translations _ARGS((int narg, cl_object host, ...));
extern cl_object clLtranslate_pathname _ARGS((int narg, cl_object source, cl_object from, cl_object to));
extern cl_object clLtranslate_logical_pathname _ARGS((int narg, cl_object source));

/* predicate.c */

extern cl_object clLnull _ARGS((int narg, cl_object x));
extern cl_object clLsymbolp _ARGS((int narg, cl_object x));
extern cl_object clLatom _ARGS((int narg, cl_object x));
extern cl_object clLconsp _ARGS((int narg, cl_object x));
extern cl_object clLlistp _ARGS((int narg, cl_object x));
extern cl_object clLnumberp _ARGS((int narg, cl_object x));
extern cl_object clLintegerp _ARGS((int narg, cl_object x));
extern cl_object clLrationalp _ARGS((int narg, cl_object x));
extern cl_object clLfloatp _ARGS((int narg, cl_object x));
extern cl_object clLrealp _ARGS((int narg, cl_object x));
extern cl_object clLcomplexp _ARGS((int narg, cl_object x));
extern cl_object clLcharacterp _ARGS((int narg, cl_object x));
extern cl_object clLstringp _ARGS((int narg, cl_object x));
extern cl_object clLbit_vector_p _ARGS((int narg, cl_object x));
extern cl_object clLvectorp _ARGS((int narg, cl_object x));
extern cl_object clLsimple_string_p _ARGS((int narg, cl_object x));
extern cl_object clLsimple_bit_vector_p _ARGS((int narg, cl_object x));
extern cl_object clLsimple_vector_p _ARGS((int narg, cl_object x));
extern cl_object clLarrayp _ARGS((int narg, cl_object x));
extern cl_object clLpackagep _ARGS((int narg, cl_object x));
extern cl_object clLfunctionp _ARGS((int narg, cl_object x));
extern cl_object clLcompiled_function_p _ARGS((int narg, cl_object x));
extern cl_object clLcommonp _ARGS((int narg, cl_object x));
extern cl_object clLeq _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLeql _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLequal _ARGS((int narg, cl_object x, cl_object y));
extern cl_object clLequalp _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLfixnump _ARGS((int narg, cl_object x));

/* print.c */

extern cl_object Kupcase;
extern cl_object Kdowncase;
extern cl_object Kcapitalize;
extern cl_object Kstream;
extern cl_object Kescape;
extern cl_object Kpretty;
extern cl_object Kcircle;
extern cl_object Kbase;
extern cl_object Kradix;
extern cl_object Kcase;
extern cl_object Kgensym;
extern cl_object Klevel;
extern cl_object Klength;
extern cl_object Karray;
extern cl_object clVprint_escape;
extern cl_object clVprint_pretty;
extern cl_object clVprint_circle;
extern cl_object clVprint_base;
extern cl_object clVprint_radix;
extern cl_object clVprint_case;
extern cl_object clVprint_gensym;
extern cl_object clVprint_level;
extern cl_object clVprint_length;
extern cl_object clVprint_array;
extern cl_object clSstream_write_char;
extern cl_object clSstream_write_string;
extern cl_object clSstream_fresh_line;
extern cl_object clSstream_clear_output;
extern cl_object clSstream_force_output;
extern cl_object siSpretty_print_format;
extern cl_object siSsharp_exclamation;
extern cl_object siVprint_package;
extern cl_object siVprint_structure;
extern cl_object clLwrite _ARGS((int narg, cl_object x, ...));
extern cl_object clLprin1 _ARGS((int narg, cl_object obj, ...));
extern cl_object clLprint _ARGS((int narg, cl_object obj, ...));
extern cl_object clLpprint _ARGS((int narg, cl_object obj, ...));
extern cl_object clLprinc _ARGS((int narg, cl_object obj, ...));
extern cl_object clLwrite_char _ARGS((int narg, cl_object c, ...));
extern cl_object clLwrite_string _ARGS((int narg, cl_object strng, ...));
extern cl_object clLwrite_line _ARGS((int narg, cl_object strng, ...));
extern cl_object clLterpri _ARGS((int narg, ...));
extern cl_object clLfresh_line _ARGS((int narg, ...));
extern cl_object clLforce_output _ARGS((int narg, ...));
#define clLfinish_output clLforce_output
extern cl_object clLclear_output _ARGS((int narg, ...));
extern cl_object clLwrite_byte _ARGS((int narg, cl_object integer, cl_object binary_output_stream));
extern cl_object siLwrite_bytes _ARGS((int narg, cl_object stream, cl_object string, cl_object start, cl_object end));

/* profile.c */

extern cl_object siLprofile _ARGS((int narg, cl_object scale, cl_object start_address));
extern cl_object siLclear_profile _ARGS((int narg));
extern cl_object siLdisplay_profile _ARGS((int narg));

/* read.c */

extern cl_object clVreadtable;
extern cl_object clVread_default_float_format;
extern cl_object clVread_base;
extern cl_object clVread_suppress;
extern cl_object Kjunk_allowed;
extern cl_object clSstream_read_line;
extern cl_object clSstream_read_char;
extern cl_object clSstream_unread_char;
extern cl_object clSstream_peek_char;
extern cl_object clSstream_listen;
extern cl_object clSstream_clear_input;
extern cl_object clLread _ARGS((int narg, ...));
extern cl_object clLread_preserving_whitespace _ARGS((int narg, ...));
extern cl_object clLread_delimited_list _ARGS((int narg, cl_object d, ...));
extern cl_object clLread_line _ARGS((int narg, ...));
extern cl_object clLread_char _ARGS((int narg, ...));
extern cl_object clLunread_char _ARGS((int narg, cl_object c, ...));
extern cl_object clLpeek_char _ARGS((int narg, ...));
extern cl_object clLlisten _ARGS((int narg, ...));
extern cl_object clLread_char_no_hang _ARGS((int narg, ...));
extern cl_object clLclear_input _ARGS((int narg, ...));
extern cl_object clLparse_integer _ARGS((int narg, cl_object strng, ...));
extern cl_object clLread_byte _ARGS((int narg, cl_object binary_input_stream, ...));
extern cl_object siLread_bytes _ARGS((int narg, cl_object stream, cl_object string, cl_object start, cl_object end));
extern cl_object clLcopy_readtable _ARGS((int narg, ...));
extern cl_object clLreadtablep _ARGS((int narg, cl_object readtable));
extern cl_object clLset_syntax_from_char _ARGS((int narg, cl_object tochr, cl_object fromchr, ...));
extern cl_object clLset_macro_character _ARGS((int narg, cl_object chr, cl_object fnc, ...));
extern cl_object clLget_macro_character _ARGS((int narg, cl_object chr, ...));
extern cl_object clLmake_dispatch_macro_character _ARGS((int narg, cl_object chr, ...));
extern cl_object clLset_dispatch_macro_character _ARGS((int narg, cl_object dspchr, cl_object subchr, cl_object fnc, ...));
extern cl_object clLget_dispatch_macro_character _ARGS((int narg, cl_object dspchr, cl_object subchr, ...));
extern cl_object siLstring_to_object _ARGS((int narg, cl_object str));
extern cl_object siLstandard_readtable _ARGS((int narg));

/* reference.c */

extern cl_object clLfboundp _ARGS((int narg, cl_object sym));
extern cl_object clLsymbol_function _ARGS((int narg, cl_object sym));
extern cl_object siLcoerce_to_function _ARGS((int narg, cl_object form));
extern cl_object clLsymbol_value _ARGS((int narg, cl_object sym));
extern cl_object clLboundp _ARGS((int narg, cl_object sym));
extern cl_object clLmacro_function _ARGS((int narg, cl_object sym, ...));
extern cl_object clLspecial_form_p _ARGS((int narg, cl_object form));

/* sequence.c */

extern cl_object clLelt _ARGS((int narg, cl_object x, cl_object i));
extern cl_object siLelt_set _ARGS((int narg, cl_object seq, cl_object index, cl_object val));
extern cl_object clLsubseq _ARGS((int narg, cl_object sequence, cl_object start, ...));
extern cl_object clLcopy_seq _ARGS((int narg, cl_object x));
extern cl_object clLlength _ARGS((int narg, cl_object x));
extern cl_object clLreverse _ARGS((int narg, cl_object x));
extern cl_object clLnreverse _ARGS((int narg, cl_object x));

/* stacks.c */

extern cl_object Kcatch, Kcatchall, Kprotect;
extern cl_object siLihs_top _ARGS((int narg, cl_object arg));
extern cl_object siLihs_fun _ARGS((int narg, cl_object arg));
extern cl_object siLihs_env _ARGS((int narg, cl_object arg));
extern cl_object siLihs_next _ARGS((int narg, cl_object arg));
extern cl_object siLihs_prev _ARGS((int narg, cl_object arg));
extern cl_object siLfrs_top _ARGS((int narg));
extern cl_object siLfrs_bds _ARGS((int narg, cl_object arg));
extern cl_object siLfrs_class _ARGS((int narg, cl_object arg));
extern cl_object siLfrs_tag _ARGS((int narg, cl_object arg));
extern cl_object siLfrs_ihs _ARGS((int narg, cl_object arg));
extern cl_object siLbds_top _ARGS((int narg));
extern cl_object siLbds_var _ARGS((int narg, cl_object arg));
extern cl_object siLbds_val _ARGS((int narg, cl_object arg));
extern cl_object siLsch_frs_base _ARGS((int narg, cl_object fr, cl_object ihs));
extern cl_object siLreset_stack_limits _ARGS((int narg));


/* string.c */

extern cl_object Kstart1;
extern cl_object Kend1;
extern cl_object Kstart2;
extern cl_object Kend2;
extern cl_object Kstart;
extern cl_object Kend;
extern cl_object clLmake_string _ARGS((int narg, cl_object size, ...));
extern cl_object clLchar _ARGS((int narg, cl_object s, cl_object i));
extern cl_object siLchar_set _ARGS((int narg, cl_object str, cl_object index, cl_object c));
extern cl_object clLstringE _ARGS((int narg, cl_object string1, cl_object string2, ...));
extern cl_object clLstring_equal _ARGS((int narg, cl_object string1, cl_object string2, ...));
extern cl_object clLstringL _ARGS((int narg, ...));
extern cl_object clLstringG _ARGS((int narg, ...));
extern cl_object clLstringLE _ARGS((int narg, ...));
extern cl_object clLstringGE _ARGS((int narg, ...));
extern cl_object clLstringNE _ARGS((int narg, ...));
extern cl_object clLstring_lessp _ARGS((int narg, ...));
extern cl_object clLstring_greaterp _ARGS((int narg, ...));
extern cl_object clLstring_not_greaterp _ARGS((int narg, ...));
extern cl_object clLstring_not_lessp _ARGS((int narg, ...));
extern cl_object clLstring_not_equal _ARGS((int narg, ...));
extern cl_object clLstring_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object clLstring_left_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object clLstring_right_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object clLstring_trim0 _ARGS((int narg, bool left_trim, bool right_trim, cl_object char_bag, cl_object strng));
extern cl_object clLstring_upcase _ARGS((int narg, ...));
extern cl_object clLstring_downcase _ARGS((int narg, ...));
extern cl_object clLstring_capitalize _ARGS((int narg, ...));
extern cl_object clLnstring_upcase _ARGS((int narg, ...));
extern cl_object clLnstring_downcase _ARGS((int narg, ...));
extern cl_object clLnstring_capitalize _ARGS((int narg, ...));
extern cl_object clLstring _ARGS((int narg, cl_object x));
extern cl_object siLstring_concatenate _ARGS((int narg, ...));

/* structure.c */

extern cl_object siSstructure_print_function;
extern cl_object siSstructure_slot_descriptions;
#ifdef CLOS
extern cl_object clSstructure_object;
#else
extern cl_object siSstructure_include;
#endif
extern cl_object siLstructure_subtype_p _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLmake_structure _ARGS((int narg, cl_object type, ...));
extern cl_object siLcopy_structure _ARGS((int narg, cl_object x));
extern cl_object siLstructure_name _ARGS((int narg, cl_object s));
extern cl_object siLstructure_ref _ARGS((int narg, cl_object x, cl_object type, cl_object index));
extern cl_object siLstructure_set _ARGS((int narg, cl_object x, cl_object type, cl_object index, cl_object val));
extern cl_object siLstructurep _ARGS((int narg, cl_object s));
extern cl_object siLrplaca_nthcdr _ARGS((int narg, cl_object x, cl_object idx, cl_object v));
extern cl_object siLlist_nth _ARGS((int narg, cl_object idx, cl_object x));

/* symbol.c */

extern cl_object clVgensym_counter;
extern cl_object clLmake_symbol _ARGS((int narg, cl_object str));
extern cl_object clLget _ARGS((int narg, cl_object sym, cl_object indicator, ...));
extern cl_object clLremprop _ARGS((int narg, cl_object sym, cl_object prop));
extern cl_object clLsymbol_plist _ARGS((int narg, cl_object sym));
extern cl_object clLgetf _ARGS((int narg, cl_object place, cl_object indicator, ...));
extern cl_object clLget_properties _ARGS((int narg, cl_object place, cl_object indicator_list));
extern cl_object clLsymbol_name _ARGS((int narg, cl_object sym));
extern cl_object clLcopy_symbol _ARGS((int narg, cl_object sym, ...));
extern cl_object clLgensym _ARGS((int narg, ...));
extern cl_object clLgentemp _ARGS((int narg, ...));
extern cl_object clLsymbol_package _ARGS((int narg, cl_object sym));
extern cl_object clLkeywordp _ARGS((int narg, cl_object sym));
extern cl_object siLput_f _ARGS((int narg, cl_object plist, cl_object value, cl_object indicator));
extern cl_object siLrem_f _ARGS((int narg, cl_object plist, cl_object indicator));
extern cl_object siLset_symbol_plist _ARGS((int narg, cl_object sym, cl_object plist));
extern cl_object siLputprop _ARGS((int narg, cl_object sym, cl_object value, cl_object indicator));
extern cl_object siLput_properties _ARGS((int narg, cl_object sym, ...));
extern cl_object siLXmake_special _ARGS((int narg, cl_object sym));
extern cl_object siLXmake_constant _ARGS((int narg, cl_object sym, cl_object val));

/* tcp.c */

#ifdef TCP
extern cl_object siLopen_client_stream _ARGS((int narg, cl_object host, cl_object port));
extern cl_object siLopen_server_stream _ARGS((int narg, cl_object port));
#endif

/* time.c */

extern cl_object clLget_universal_time _ARGS((int narg));
extern cl_object clLsleep _ARGS((int narg, cl_object z));
extern cl_object clLget_internal_run_time _ARGS((int narg));
extern cl_object clLget_internal_real_time _ARGS((int narg));
extern cl_object siLget_local_time_zone _ARGS((int narg));
extern cl_object siLdaylight_saving_time_p _ARGS((int narg, ...));

/* typespec.c */

extern cl_object clSquote;
extern cl_object clSlambda;
extern cl_object clSspecial;
extern cl_object clSt;
extern cl_object clSnil;
extern cl_object clScommon;
extern cl_object clSsequence;
extern cl_object clSnull;
extern cl_object clScons;
extern cl_object clSlist;
extern cl_object clSsymbol;
extern cl_object clSarray;
extern cl_object clSvector;
extern cl_object clSbit_vector;
extern cl_object clSstring;
extern cl_object clSsimple_array;
extern cl_object clSsimple_vector;
extern cl_object clSsimple_string;
extern cl_object clSsimple_bit_vector;
extern cl_object clSfunction;
extern cl_object clSpathname;
extern cl_object clSlogical_pathname;
extern cl_object clScharacter;
extern cl_object clSbase_char;
extern cl_object clSextended_char;
extern cl_object clScompiled_function;
extern cl_object clSnumber;
extern cl_object clSreal;
extern cl_object clSrational;
extern cl_object clSfloat;
extern cl_object clSinteger;
extern cl_object clSratio;
extern cl_object clSshort_float;
extern cl_object clSstandard_char;
extern cl_object clSfixnum;
extern cl_object clScomplex;
extern cl_object clSsingle_float;
extern cl_object clSpackage;
extern cl_object clSbignum;
extern cl_object clSrandom_state;
extern cl_object clSdouble_float;
extern cl_object clSstream;
extern cl_object clSbit;
extern cl_object clSreadtable;
extern cl_object clSlong_float;
extern cl_object clShash_table;
extern cl_object clSsigned_char;
extern cl_object clSunsigned_char;
extern cl_object clSsigned_short;
extern cl_object clSunsigned_short;
extern cl_object clSinstance;
extern cl_object clSdispatch_function;
extern cl_object clSstructure;
extern cl_object clSsatisfies;
extern cl_object clSmember;
extern cl_object clSnot;
extern cl_object clSor;
extern cl_object clSand;
extern cl_object clSvalues;
extern cl_object clSmod;
extern cl_object clSsigned_byte;
extern cl_object clSunsigned_byte;
extern cl_object clV;
extern cl_object clSplusp;
extern cl_object clSkeyword;
extern cl_object TSor_string_symbol;
extern cl_object TSor_symbol_string_package;
extern cl_object TSnon_negative_integer;
extern cl_object TSpositive_number;
extern cl_object TSor_integer_float;
extern cl_object TSor_rational_float;
extern cl_object TSor_pathname_string_symbol;
extern cl_object TSor_pathname_string_symbol_stream;
extern cl_object clSsubtypep;
extern cl_object clLtype_of _ARGS((int narg, cl_object x));

/* unixfsys.c */

extern cl_object Klist_all;
extern cl_object clLtruename _ARGS((int narg, cl_object file));
extern cl_object clLrename_file _ARGS((int narg, cl_object old, cl_object new));
extern cl_object clLdelete_file _ARGS((int narg, cl_object file));
extern cl_object clLprobe_file _ARGS((int narg, cl_object file));
extern cl_object clLfile_write_date _ARGS((int narg, cl_object file));
extern cl_object clLfile_author _ARGS((int narg, cl_object file));
extern cl_object clLuser_homedir_pathname _ARGS((int narg, ...));
extern cl_object siLchdir _ARGS((int narg, cl_object directory));
extern cl_object siLstring_match _ARGS((int narg, cl_object string, cl_object pattern));
extern cl_object clLdirectory _ARGS((int narg, ...));

/* unixint.c */

extern cl_object siLcatch_bad_signals _ARGS((int narg));
extern cl_object siLuncatch_bad_signals _ARGS((int narg));

/* unixsys.c */

extern cl_object siLsystem _ARGS((int narg, cl_object cmd));
extern cl_object siLopen_pipe _ARGS((int narg, cl_object cmd));
