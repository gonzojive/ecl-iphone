
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
extern cl_object Lgc _ARGS((int narg, cl_object area));
extern cl_object siLroom_report _ARGS((int narg));
#endif /* GBC_BOEHM */

/* array.c */

extern cl_object Laref _ARGS((int narg, cl_object x, ...));
extern cl_object siLaset _ARGS((int narg, cl_object v, cl_object x, ...));
extern cl_object siLmake_pure_array _ARGS((int narg, cl_object etype, cl_object adj, cl_object displ, cl_object disploff, ...));
extern cl_object siLmake_vector _ARGS((int narg, cl_object etype, cl_object dim, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff));
extern cl_object Larray_element_type _ARGS((int narg, cl_object a));
extern cl_object Larray_rank _ARGS((int narg, cl_object a));
extern cl_object Larray_dimension _ARGS((int narg, cl_object a, cl_object index));
extern cl_object Larray_total_size _ARGS((int narg, cl_object a));
extern cl_object Ladjustable_array_p _ARGS((int narg, cl_object a));
extern cl_object siLdisplaced_array_p _ARGS((int narg, cl_object a));
extern cl_object Lsvref _ARGS((int narg, cl_object x, cl_object index));
extern cl_object siLsvset _ARGS((int narg, cl_object x, cl_object index, cl_object v));
extern cl_object Larray_has_fill_pointer_p _ARGS((int narg, cl_object a));
extern cl_object Lfill_pointer _ARGS((int narg, cl_object a));
extern cl_object siLfill_pointer_set _ARGS((int narg, cl_object a, cl_object fp));
extern cl_object siLreplace_array _ARGS((int narg, cl_object old, cl_object new));

/* assignment.c */

extern cl_object Ssetf, Spsetf, siSsetf_symbol;
extern cl_object siSclear_compiler_properties;
#ifdef PDE
extern cl_object siVrecord_source_pathname_p;
extern cl_object siSrecord_source_pathname;
#endif
extern cl_object Lset _ARGS((int narg, cl_object var, cl_object val));
extern cl_object siLsetf_namep _ARGS((int narg, cl_object arg));
extern cl_object siLfset _ARGS((int narg, cl_object fun, cl_object def, ...));
extern cl_object Lmakunbound _ARGS((int narg, cl_object sym));
extern cl_object Lfmakunbound _ARGS((int narg, cl_object sym));
extern cl_object siLclear_compiler_properties _ARGS((int narg, cl_object sym));

/* backq.c */

extern cl_object siScomma;
extern cl_object siScomma_at;
extern cl_object siScomma_dot;
extern cl_object SlistX;
extern cl_object Sappend;
extern cl_object Snconc;
extern cl_object Lcomma_reader _ARGS((int narg, cl_object in, cl_object c));
extern cl_object Lbackquote_reader _ARGS((int narg, cl_object in, cl_object c));

/* cfun.c */

extern cl_object siLcompiled_function_name _ARGS((int narg, cl_object fun));
extern cl_object siLcompiled_function_block _ARGS((int narg, cl_object fun));

/* character.c */

extern cl_object STreturn;
extern cl_object STspace;
extern cl_object STrubout;
extern cl_object STpage;
extern cl_object STtab;
extern cl_object STbackspace;
extern cl_object STlinefeed;
extern cl_object STnewline;
extern cl_object STnull;
extern cl_object Lstandard_char_p _ARGS((int narg, cl_object c));
extern cl_object Lgraphic_char_p _ARGS((int narg, cl_object c));
extern cl_object Lalpha_char_p _ARGS((int narg, cl_object c));
extern cl_object Lupper_case_p _ARGS((int narg, cl_object c));
extern cl_object Llower_case_p _ARGS((int narg, cl_object c));
extern cl_object Lboth_case_p _ARGS((int narg, cl_object c));
extern cl_object Ldigit_char_p _ARGS((int narg, cl_object c, ...));
extern cl_object Lalphanumericp _ARGS((int narg, cl_object c));
extern cl_object Lchar_eq _ARGS((int narg, cl_object c, ...));
extern cl_object Lchar_neq _ARGS((int narg, ...));
extern cl_object Lchar_l _ARGS((int narg, ...));
extern cl_object Lchar_g _ARGS((int narg, ...));
extern cl_object Lchar_le _ARGS((int narg, ...));
extern cl_object Lchar_ge _ARGS((int narg, ...));
extern cl_object Lchar_equal _ARGS((int narg, cl_object c, ...));
extern cl_object Lchar_not_equal _ARGS((int narg, ...));
extern cl_object Lchar_lessp _ARGS((int narg, ...));
extern cl_object Lchar_greaterp _ARGS((int narg, ...));
extern cl_object Lchar_not_greaterp _ARGS((int narg, ...));
extern cl_object Lchar_not_lessp _ARGS((int narg, ...));
extern cl_object Lcharacter _ARGS((int narg, cl_object x));
extern cl_object Lchar_code _ARGS((int narg, cl_object c));
extern cl_object Lcode_char _ARGS((int narg, cl_object c));
extern cl_object Lchar_upcase _ARGS((int narg, cl_object c));
extern cl_object Lchar_downcase _ARGS((int narg, cl_object c));
extern cl_object Ldigit_char _ARGS((int narg, cl_object w, ...));
extern cl_object Lchar_int _ARGS((int narg, cl_object c));
extern cl_object Lint_char _ARGS((int narg, cl_object x));
extern cl_object Lchar_name _ARGS((int narg, cl_object c));
extern cl_object Lname_char _ARGS((int narg, cl_object s));

/* clos.c */

#ifdef CLOS
extern cl_object siSXclass_name_hash_tableX;
extern cl_object Sclass;
extern cl_object Sbuilt_in;
#endif

/* cmpaux.c */

extern cl_object SAoptional;
extern cl_object SArest;
extern cl_object SAkey;
extern cl_object SAallow_other_keys;
extern cl_object SAaux;
extern cl_object Kallow_other_keys;
extern cl_object siLspecialp _ARGS((int narg, cl_object sym));

/* compiler.c */

extern cl_object siSlambda_block;
extern cl_object Sdeclare;
extern cl_object Scompile;
extern cl_object Sload;
extern cl_object Seval;
extern cl_object Sprogn;
extern cl_object Swarn;
extern cl_object Stypep;
extern cl_object Kexecute;
extern cl_object Kcompile_toplevel;
extern cl_object Kload_toplevel;
extern cl_object Sotherwise;
extern cl_object siLprocess_declarations _ARGS((int narg, cl_object body, ...));
extern cl_object siLprocess_lambda_list _ARGS((int narg, cl_object lambda));
extern cl_object siLmake_lambda _ARGS((int narg, cl_object name, cl_object body));

/* disassembler.c */

extern cl_object siLbc_disassemble _ARGS((int narg, cl_object v));
extern cl_object siLbc_split _ARGS((int narg, cl_object v));

/* error.c */

extern cl_object Sarithmetic_error, Scell_error, Scondition;
extern cl_object Scontrol_error, Sdivision_by_zero, Send_of_file;
extern cl_object Serror, Sfile_error, Sfloating_point_inexact;
extern cl_object Sfloating_point_invalid_operation, Sfloating_point_overflow;
extern cl_object Sfloating_point_underflow, Spackage_error, Sparse_error;
extern cl_object Sprint_not_readable, Sprogram_error, Sreader_error;
extern cl_object Sserious_condition, Ssimple_condition, Ssimple_error;
extern cl_object Ssimple_type_error, Ssimple_warning, Sstorage_condition;
extern cl_object Sstream_error, Sstyle_warning, Stype_error, Sunbound_slot;
extern cl_object Sunbound_variable, Sundefined_function, Swarning;

extern cl_object siSsimple_program_error, siSsimple_control_error;

extern cl_object Kdatum, Kexpected_type;
extern cl_object Kpathname;
extern cl_object Kformat_control, Kformat_arguments;

extern cl_object siSuniversal_error_handler;
extern cl_object siSterminal_interrupt;
extern cl_object siLuniversal_error_handler _ARGS((int narg, cl_object c, cl_object d, cl_object args));
#if defined(FRAME_CHAIN) && !defined(RUNTIME)
extern cl_object siLbacktrace _ARGS((int narg));
#endif
extern cl_object Lerror _ARGS((int narg, cl_object eformat, ...));
extern cl_object Lcerror _ARGS((int narg, cl_object cformat, cl_object eformat, ...));

/* eval.c */

extern cl_object Sapply;
extern cl_object Sfuncall;
extern cl_object Vevalhook;
extern cl_object Vapplyhook;
extern cl_object siLunlink_symbol _ARGS((int narg, cl_object s));
extern cl_object Lfuncall _ARGS((int narg, cl_object fun, ...));
extern cl_object Lapply _ARGS((int narg, cl_object fun, cl_object arg, ...));
extern cl_object Leval _ARGS((int narg, cl_object form));
extern cl_object Levalhook _ARGS((int n, cl_object form, cl_object evalhookfn, cl_object applyhookfn, ...));
extern cl_object Lapplyhook _ARGS((int narg, cl_object fun, cl_object args, cl_object evalhookfn, cl_object applyhookfn));
extern cl_object Lconstantp _ARGS((int narg, cl_object arg));

/* file.c */

extern cl_object Kerror;
extern cl_object Vstandard_input;
extern cl_object Vstandard_output;
extern cl_object Verror_output;
extern cl_object Vquery_io;
extern cl_object Vdebug_io;
extern cl_object Vterminal_io;
extern cl_object Vtrace_output;
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
extern cl_object Lmake_synonym_stream _ARGS((int narg, cl_object sym));
extern cl_object Lmake_broadcast_stream _ARGS((int narg, ...));
extern cl_object Lmake_concatenated_stream _ARGS((int narg, ...));
extern cl_object Lmake_two_way_stream _ARGS((int narg, cl_object strm1, cl_object strm2));
extern cl_object Lmake_echo_stream _ARGS((int narg, cl_object strm1, cl_object strm2));
extern cl_object Lmake_string_input_stream _ARGS((int narg, cl_object strng, ...));
extern cl_object Lmake_string_output_stream _ARGS((int narg));
extern cl_object Lget_output_stream_string _ARGS((int narg, cl_object strm));
extern cl_object siLoutput_stream_string _ARGS((int narg, cl_object strm));
extern cl_object Lstreamp _ARGS((int narg, cl_object strm));
extern cl_object Linput_stream_p _ARGS((int narg, cl_object strm));
extern cl_object Loutput_stream_p _ARGS((int narg, cl_object strm));
extern cl_object Lstream_element_type _ARGS((int narg, cl_object strm));
extern cl_object Lclose _ARGS((int narg, cl_object strm, ...));
extern cl_object Lopen _ARGS((int narg, cl_object filename, ...));
extern cl_object Lfile_position _ARGS((int narg, cl_object file_stream, ...));
extern cl_object Lfile_length _ARGS((int narg, cl_object strm));
extern cl_object siLget_string_input_stream_index _ARGS((int narg, cl_object strm));
extern cl_object siLmake_string_output_stream_from_string _ARGS((int narg, cl_object strng));
extern cl_object siLcopy_stream _ARGS((int narg, cl_object in, cl_object out));
extern cl_object Lopen_stream_p _ARGS((int narg, cl_object strm));

/* format.c */

extern cl_object siVindent_formatted_output;
extern cl_object Lformat _ARGS((int narg, volatile cl_object strm, cl_object string, ...));

/* gbc.c */

#if !defined(GBC_BOEHM)
extern cl_object siVgc_verbose;
extern cl_object siVgc_message;
extern cl_object Lgc _ARGS((int narg, cl_object area));
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

extern cl_object Seq;
extern cl_object Seql;
extern cl_object Sequal;
extern cl_object Ksize;
extern cl_object Krehash_size;
extern cl_object Krehash_threshold;
extern cl_object Lmake_hash_table _ARGS((int narg, ...));
extern cl_object Lhash_table_p _ARGS((int narg, cl_object ht));
extern cl_object Lgethash _ARGS((int narg, cl_object key, cl_object ht, ...));
extern cl_object siLhash_set _ARGS((int narg, cl_object key, cl_object ht, cl_object val));
extern cl_object Lremhash _ARGS((int narg, cl_object key, cl_object ht));
extern cl_object Lclrhash _ARGS((int narg, cl_object ht));
extern cl_object Lhash_table_count _ARGS((int narg, cl_object ht));
extern cl_object Lsxhash _ARGS((int narg, cl_object key));
extern cl_object Lmaphash _ARGS((int narg, cl_object fun, cl_object ht));
extern cl_object Lhash_table_rehash_size _ARGS((int narg, cl_object ht));
extern cl_object Lhash_table_rehash_threshold _ARGS((int narg, cl_object ht));

/* instance.c */

#ifdef CLOS
extern cl_object Sprint_object;
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

extern cl_object Sblock;
extern cl_object Smacro;
extern cl_object siSsymbol_macro;
extern cl_object Stag;
extern cl_object siLlex_env _ARGS((int narg));

/* list.c */

extern cl_object Ktest;
extern cl_object Ktest_not;
extern cl_object Kkey;
extern cl_object Kinitial_element;
extern cl_object Lcar _ARGS((int narg, cl_object x));
extern cl_object Lcdr _ARGS((int narg, cl_object x));
extern cl_object Llist _ARGS((int narg, ...));
extern cl_object LlistA _ARGS((int narg, ...));
extern cl_object Lappend _ARGS((int narg, ...));
extern cl_object Lcaar _ARGS((int narg, cl_object x));
extern cl_object Lcadr _ARGS((int narg, cl_object x));
extern cl_object Lcdar _ARGS((int narg, cl_object x));
extern cl_object Lcddr _ARGS((int narg, cl_object x));
extern cl_object Lcaaar _ARGS((int narg, cl_object x));
extern cl_object Lcaadr _ARGS((int narg, cl_object x));
extern cl_object Lcadar _ARGS((int narg, cl_object x));
extern cl_object Lcaddr _ARGS((int narg, cl_object x));
extern cl_object Lcdaar _ARGS((int narg, cl_object x));
extern cl_object Lcdadr _ARGS((int narg, cl_object x));
extern cl_object Lcddar _ARGS((int narg, cl_object x));
extern cl_object Lcdddr _ARGS((int narg, cl_object x));
extern cl_object Lcaaaar _ARGS((int narg, cl_object x));
extern cl_object Lcaaadr _ARGS((int narg, cl_object x));
extern cl_object Lcaadar _ARGS((int narg, cl_object x));
extern cl_object Lcaaddr _ARGS((int narg, cl_object x));
extern cl_object Lcadaar _ARGS((int narg, cl_object x));
extern cl_object Lcadadr _ARGS((int narg, cl_object x));
extern cl_object Lcaddar _ARGS((int narg, cl_object x));
extern cl_object Lcadddr _ARGS((int narg, cl_object x));
extern cl_object Lcdaaar _ARGS((int narg, cl_object x));
extern cl_object Lcdaadr _ARGS((int narg, cl_object x));
extern cl_object Lcdadar _ARGS((int narg, cl_object x));
extern cl_object Lcdaddr _ARGS((int narg, cl_object x));
extern cl_object Lcddaar _ARGS((int narg, cl_object x));
extern cl_object Lcddadr _ARGS((int narg, cl_object x));
extern cl_object Lcdddar _ARGS((int narg, cl_object x));
extern cl_object Lcddddr _ARGS((int narg, cl_object x));
extern cl_object Lfifth _ARGS((int narg, cl_object x));
extern cl_object Lsixth _ARGS((int narg, cl_object x));
extern cl_object Lseventh _ARGS((int narg, cl_object x));
extern cl_object Leighth _ARGS((int narg, cl_object x));
extern cl_object Lninth _ARGS((int narg, cl_object x));
extern cl_object Ltenth _ARGS((int narg, cl_object x));
extern cl_object Lcons _ARGS((int narg, cl_object car, cl_object cdr));
extern cl_object Ltree_equal _ARGS((int narg, cl_object x, cl_object y, ...));
extern cl_object Lendp _ARGS((int narg, cl_object x));
extern cl_object Llist_length _ARGS((int narg, cl_object x));
extern cl_object Lnth _ARGS((int narg, cl_object n, cl_object x));
extern cl_object Lnthcdr _ARGS((int narg, cl_object n, cl_object x));
extern cl_object Llast _ARGS((int narg, cl_object x, ...));
extern cl_object Lmake_list _ARGS((int narg, cl_object size, ...));
extern cl_object Lcopy_list _ARGS((int narg, cl_object x));
extern cl_object Lcopy_alist _ARGS((int narg, cl_object x));
extern cl_object Lcopy_tree _ARGS((int narg, cl_object x));
extern cl_object Lrevappend _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lnconc _ARGS((int narg, ...));
extern cl_object Lreconc _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lbutlast _ARGS((int narg, cl_object lis, ...));
extern cl_object Lnbutlast _ARGS((int narg, cl_object lis, ...));
extern cl_object Lldiff _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lrplaca _ARGS((int narg, cl_object x, cl_object v));
extern cl_object Lrplacd _ARGS((int narg, cl_object x, cl_object v));
extern cl_object Lsubst _ARGS((int narg, cl_object new, cl_object old, cl_object tree, ...));
extern cl_object Lsubst_if _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object Lsubst_if_not _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object Lnsubst _ARGS((int narg, cl_object new, cl_object old, cl_object tree, ...));
extern cl_object Lnsubst_if _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object Lnsubst_if_not _ARGS((int narg, cl_object arg1, cl_object pred, cl_object arg3, cl_object key, cl_object val));
extern cl_object Lsublis _ARGS((int narg, cl_object alist, cl_object tree, ...));
extern cl_object Lnsublis _ARGS((int narg, cl_object alist, cl_object tree, ...));
extern cl_object Lmember _ARGS((int narg, cl_object item, cl_object list, ...));
extern cl_object siLmemq _ARGS((int narg, cl_object x, cl_object l));
extern cl_object Lmember_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object Lmember_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object Lmember1 _ARGS((int narg, cl_object item, cl_object list, ...));
extern cl_object Ltailp _ARGS((int narg, cl_object y, cl_object x));
extern cl_object Ladjoin _ARGS((int narg, cl_object item, cl_object list, cl_object k1, cl_object v1, cl_object k2, cl_object v2, cl_object k3, cl_object v3));
extern cl_object Lacons _ARGS((int narg, cl_object x, cl_object y, cl_object z));
extern cl_object Lpairlis _ARGS((int narg, cl_object keys, cl_object data, ...));
extern cl_object Lassoc_or_rassoc(int narg, cl_object (*car_or_cdr)(), cl_object item, cl_object a_list, ...);
extern cl_object Lrassoc _ARGS((int narg, cl_object item, cl_object alist, cl_object k1, cl_object v1, cl_object k2, cl_object v2));
extern cl_object Lassoc _ARGS((int narg, cl_object item, cl_object alist, cl_object k1, cl_object v1, cl_object k2, cl_object v2));
extern cl_object Lassoc_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object Lassoc_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object Lrassoc_if _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));
extern cl_object Lrassoc_if_not _ARGS((int narg, cl_object pred, cl_object arg, cl_object key, cl_object val));

/* load.c */

extern cl_object Kverbose;
extern cl_object Vload_verbose, Vload_print;
extern cl_object siVsymbol_table;
extern cl_object siVload_hooks;
extern cl_object Lload _ARGS((int narg, cl_object pathname, ...));
extern cl_object siLload_source _ARGS((int narg, cl_object file, cl_object verbose,
				       cl_object print));
extern cl_object siLload_binary _ARGS((int narg, cl_object file, cl_object verbose,
				       cl_object print));
extern cl_object siLfaslink _ARGS((int narg, cl_object file, cl_object lib));

/* lwp.c */

#ifdef THREADS
extern cl_object Srunning;
extern cl_object Ssuspended;
extern cl_object Swaiting;
extern cl_object Sstopped;
extern cl_object Sdead;
extern cl_object siSthread_top_level;
extern cl_object siLthread_break_in _ARGS((int narg));
extern cl_object siLthread_break_quit _ARGS((int narg));
extern cl_object siLthread_break_resume _ARGS((int narg));
extern cl_object Lthread_list _ARGS((int narg));
extern cl_object Lmake_thread _ARGS((int narg, cl_object fun));
extern cl_object Ldeactivate _ARGS((int narg, cl_object thread));
extern cl_object Lreactivate _ARGS((int narg, cl_object thread));
extern cl_object Lkill_thread _ARGS((int narg, cl_object thread));
extern cl_object Lcurrent_thread _ARGS((int narg));
extern cl_object Lthread_status _ARGS((int narg, cl_object thread));
extern cl_object Lmake_continuation _ARGS((int narg, cl_object thread));
extern cl_object Lthread_of _ARGS((int narg, cl_object cont));
extern cl_object Lcontinuation_of _ARGS((int narg, cl_object thread));
extern cl_object Lresume _ARGS((int narg, cl_object cont, ...));
extern cl_object Ldisable_scheduler _ARGS((int narg));
extern cl_object Lenable_scheduler _ARGS((int narg));
extern cl_object Lsuspend _ARGS((int narg));
extern cl_object Ldelay _ARGS((int narg, cl_object interval));
extern cl_object Lthread_wait _ARGS((int narg, cl_object fun, ...));
extern cl_object Lthread_wait_with_timeout _ARGS((int narg, cl_object timeout, cl_object fun, ...));
#endif

/* macros.c */

extern cl_object Vmacroexpand_hook;
extern cl_object siSexpand_defmacro;
extern cl_object siVinhibit_macro_special;
extern cl_object Lmacroexpand _ARGS((int narg, cl_object form, ...));
extern cl_object Lmacroexpand_1 _ARGS((int narg, cl_object form, ...));

/* main.c */

extern cl_object Vfeatures;
extern cl_object siVsystem_directory;
extern cl_object Lquit _ARGS((int narg, ...));
extern cl_object siLargc _ARGS((int narg));
extern cl_object siLargv _ARGS((int narg, cl_object index));
extern cl_object siLgetenv _ARGS((int narg, cl_object var));
extern cl_object siLaddress _ARGS((int narg, cl_object x));
extern cl_object siLnani _ARGS((int narg, cl_object x));
extern cl_object Lidentity _ARGS((int narg, cl_object x));
extern cl_object Lmachine_instance _ARGS((int narg));
extern cl_object Lmachine_version _ARGS((int narg));
extern cl_object Lsoftware_type _ARGS((int narg));
extern cl_object Lsoftware_version _ARGS((int narg));

/* mapfun.c */

extern cl_object Lmapcar _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object Lmaplist _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object Lmapc _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object Lmapl _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object Lmapcan _ARGS((int narg, cl_object fun, cl_object onelist, ...));
extern cl_object Lmapcon _ARGS((int narg, cl_object fun, cl_object onelist, ...));

/* multival.c */

extern cl_object Lvalues _ARGS((int narg, ...));
extern cl_object Lvalues_list _ARGS((int narg, cl_object list));

/* num_arith.c */

extern cl_object Ltimes _ARGS((int narg, ...));
extern cl_object Lplus _ARGS((int narg, ...));
extern cl_object Lminus _ARGS((int narg, cl_object num, ...));
extern cl_object Lconjugate _ARGS((int narg, cl_object c));
extern cl_object Ldivide _ARGS((int narg, cl_object num, ...));
extern cl_object Lgcd _ARGS((int narg, ...));
extern cl_object Lone_plus _ARGS((int narg, cl_object x));
extern cl_object Lone_minus _ARGS((int narg, cl_object x));
extern cl_object Llcm _ARGS((int narg, cl_object lcm, ...));

/* num_co.c */

extern cl_object Lfloat _ARGS((int narg, cl_object x, ...));
extern cl_object Lnumerator _ARGS((int narg, cl_object x));
extern cl_object Ldenominator _ARGS((int narg, cl_object x));
extern cl_object Lfloor _ARGS((int narg, cl_object x, ...));
extern cl_object Lceiling _ARGS((int narg, cl_object x, ...));
extern cl_object Ltruncate _ARGS((int narg, cl_object x, ...));
extern cl_object Lround _ARGS((int narg, cl_object x, ...));
extern cl_object Lmod _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lrem _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Ldecode_float _ARGS((int narg, cl_object x));
extern cl_object Lscale_float _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lfloat_radix _ARGS((int narg, cl_object x));
extern cl_object Lfloat_sign _ARGS((int narg, cl_object x, ...));
extern cl_object Lfloat_digits _ARGS((int narg, cl_object x));
extern cl_object Lfloat_precision _ARGS((int narg, cl_object x));
extern cl_object Linteger_decode_float _ARGS((int narg, cl_object x));
extern cl_object Lcomplex _ARGS((int narg, cl_object r, ...));
extern cl_object Lrealpart _ARGS((int narg, cl_object x));
extern cl_object Limagpart _ARGS((int narg, cl_object x));

/* num_comp.c */

extern cl_object Lall_the_same _ARGS((int narg, cl_object num, ...));
extern cl_object Lall_different _ARGS((int narg, ...));
extern cl_object Lmonotonically_nondecreasing _ARGS((int narg, ...));
extern cl_object Lmonotonically_nonincreasing _ARGS((int narg, ...));
extern cl_object Lmonotonically_increasing _ARGS((int narg, ...));
extern cl_object Lmonotonically_decreasing _ARGS((int narg, ...));
extern cl_object Lmax _ARGS((int narg, cl_object max, ...));
extern cl_object Lmin _ARGS((int narg, cl_object min, ...));

/* num_log.c */

extern cl_object Llogior _ARGS((int narg, ...));
extern cl_object Llogxor _ARGS((int narg, ...));
extern cl_object Llogand _ARGS((int narg, ...));
extern cl_object Llogeqv _ARGS((int narg, ...));
extern cl_object Lboole _ARGS((int narg, cl_object o, ...));
extern cl_object Llogbitp _ARGS((int narg, cl_object p, cl_object x));
extern cl_object Lash _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Llogcount _ARGS((int narg, cl_object x));
extern cl_object Linteger_length _ARGS((int narg, cl_object x));
extern cl_object siLbit_array_op _ARGS((int narg, cl_object o, cl_object x, cl_object y, cl_object r));

/* num_pred.c */

extern cl_object Lzerop _ARGS((int narg, cl_object x));
extern cl_object Lplusp _ARGS((int narg, cl_object x));
extern cl_object Lminusp _ARGS((int narg, cl_object x));
extern cl_object Loddp _ARGS((int narg, cl_object x));
extern cl_object Levenp _ARGS((int narg, cl_object x));

/* num_rand.c */

extern cl_object Vrandom_state;
extern cl_object Lrandom _ARGS((int narg, cl_object x, ...));
extern cl_object Lmake_random_state _ARGS((int narg, ...));
extern cl_object Lrandom_state_p _ARGS((int narg, cl_object x));

/* num_sfun.c */

extern cl_object Lexp _ARGS((int narg, cl_object x));
extern cl_object Lexpt _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Llog _ARGS((int narg, cl_object x, ...));
extern cl_object Lsqrt _ARGS((int narg, cl_object x));
extern cl_object Lsin _ARGS((int narg, cl_object x));
extern cl_object Lcos _ARGS((int narg, cl_object x));
extern cl_object Ltan _ARGS((int narg, cl_object x));
extern cl_object Latan _ARGS((int narg, cl_object x, ...));
extern cl_object Lsinh _ARGS((int narg, cl_object x));
extern cl_object Lcosh _ARGS((int narg, cl_object x));
extern cl_object Ltanh _ARGS((int narg, cl_object x));

/* package.c */

extern cl_object Vpackage;
extern cl_object Kinternal;
extern cl_object Kexternal;
extern cl_object Kinherited;
extern cl_object Knicknames;
extern cl_object Kuse;
extern cl_object Lmake_package _ARGS((int narg, cl_object pack_name, ...));
extern cl_object siLselect_package _ARGS((int narg, cl_object pack_name));
extern cl_object Lfind_package _ARGS((int narg, cl_object p));
extern cl_object Lpackage_name _ARGS((int narg, cl_object p));
extern cl_object Lpackage_nicknames _ARGS((int narg, cl_object p));
extern cl_object Lrename_package _ARGS((int narg, cl_object pack, cl_object new_name, ...));
extern cl_object Lpackage_use_list _ARGS((int narg, cl_object p));
extern cl_object Lpackage_used_by_list _ARGS((int narg, cl_object p));
extern cl_object Lpackage_shadowing_symbols _ARGS((int narg, cl_object p));
extern cl_object Llist_all_packages _ARGS((int narg));
extern cl_object Lintern _ARGS((int narg, cl_object strng, ...));
extern cl_object Lfind_symbol _ARGS((int narg, cl_object strng, ...));
extern cl_object Lunintern _ARGS((int narg, cl_object symbl, ...));
extern cl_object Lexport _ARGS((int narg, cl_object symbols, ...));
extern cl_object Lunexport _ARGS((int narg, cl_object symbols, ...));
extern cl_object Limport _ARGS((int narg, cl_object symbols, ...));
extern cl_object Lshadowing_import _ARGS((int narg, cl_object symbols, ...));
extern cl_object Lshadow _ARGS((int narg, cl_object symbols, ...));
extern cl_object Luse_package _ARGS((int narg, cl_object pack, ...));
extern cl_object Lunuse_package _ARGS((int narg, cl_object pack, ...));
extern cl_object siLpackage_internal _ARGS((int narg, cl_object p, cl_object index));
extern cl_object siLpackage_external _ARGS((int narg, cl_object p, cl_object index));
extern cl_object siLpackage_size _ARGS((int narg, cl_object p));
extern cl_object siLpackage_lock _ARGS((int narg, cl_object p, cl_object t));
extern cl_object Ldelete_package _ARGS((int narg, cl_object p));

/* pathname.c */

extern cl_object Vdefault_pathname_defaults;
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
extern cl_object Lpathname _ARGS((int narg, cl_object name));
extern cl_object Lparse_namestring _ARGS((int narg, cl_object thing, ...));
extern cl_object Lparse_logical_namestring _ARGS((int narg, cl_object thing, ...));
extern cl_object Lmerge_pathnames _ARGS((int narg, cl_object path, ...));
extern cl_object Lmake_pathname _ARGS((int narg, ...));
extern cl_object Lpathnamep _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_host _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_device _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_directory _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_name _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_type _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_version _ARGS((int narg, cl_object pname));
extern cl_object Lnamestring _ARGS((int narg, cl_object pname));
extern cl_object Lfile_namestring _ARGS((int narg, cl_object pname));
extern cl_object Ldirectory_namestring _ARGS((int narg, cl_object pname));
extern cl_object Lhost_namestring _ARGS((int narg, cl_object pname));
extern cl_object Lenough_namestring _ARGS((int narg, cl_object path, ...));
extern cl_object siLlogical_pathname_p _ARGS((int narg, cl_object pname));
extern cl_object Lpathname_match_p _ARGS((int narg, cl_object path, cl_object mask));
extern cl_object siLpathname_translations _ARGS((int narg, cl_object host, ...));
extern cl_object Ltranslate_pathname _ARGS((int narg, cl_object source, cl_object from, cl_object to));
extern cl_object Ltranslate_logical_pathname _ARGS((int narg, cl_object source));

/* predicate.c */

extern cl_object Lnull _ARGS((int narg, cl_object x));
extern cl_object Lsymbolp _ARGS((int narg, cl_object x));
extern cl_object Latom _ARGS((int narg, cl_object x));
extern cl_object Lconsp _ARGS((int narg, cl_object x));
extern cl_object Llistp _ARGS((int narg, cl_object x));
extern cl_object Lnumberp _ARGS((int narg, cl_object x));
extern cl_object Lintegerp _ARGS((int narg, cl_object x));
extern cl_object Lrationalp _ARGS((int narg, cl_object x));
extern cl_object Lfloatp _ARGS((int narg, cl_object x));
extern cl_object Lrealp _ARGS((int narg, cl_object x));
extern cl_object Lcomplexp _ARGS((int narg, cl_object x));
extern cl_object Lcharacterp _ARGS((int narg, cl_object x));
extern cl_object Lstringp _ARGS((int narg, cl_object x));
extern cl_object Lbit_vector_p _ARGS((int narg, cl_object x));
extern cl_object Lvectorp _ARGS((int narg, cl_object x));
extern cl_object Lsimple_string_p _ARGS((int narg, cl_object x));
extern cl_object Lsimple_bit_vector_p _ARGS((int narg, cl_object x));
extern cl_object Lsimple_vector_p _ARGS((int narg, cl_object x));
extern cl_object Larrayp _ARGS((int narg, cl_object x));
extern cl_object Lpackagep _ARGS((int narg, cl_object x));
extern cl_object Lfunctionp _ARGS((int narg, cl_object x));
extern cl_object Lcompiled_function_p _ARGS((int narg, cl_object x));
extern cl_object Lcommonp _ARGS((int narg, cl_object x));
extern cl_object Leq _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Leql _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lequal _ARGS((int narg, cl_object x, cl_object y));
extern cl_object Lequalp _ARGS((int narg, cl_object x, cl_object y));
extern cl_object siLcontains_sharp_comma _ARGS((int narg, cl_object x));
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
extern cl_object Vprint_escape;
extern cl_object Vprint_pretty;
extern cl_object Vprint_circle;
extern cl_object Vprint_base;
extern cl_object Vprint_radix;
extern cl_object Vprint_case;
extern cl_object Vprint_gensym;
extern cl_object Vprint_level;
extern cl_object Vprint_length;
extern cl_object Vprint_array;
extern cl_object Sstream_write_char;
extern cl_object Sstream_write_string;
extern cl_object Sstream_fresh_line;
extern cl_object Sstream_clear_output;
extern cl_object Sstream_force_output;
extern cl_object siSpretty_print_format;
extern cl_object siSsharp_exclamation;
extern cl_object siVprint_package;
extern cl_object siVprint_structure;
extern cl_object Lwrite _ARGS((int narg, cl_object x, ...));
extern cl_object Lprin1 _ARGS((int narg, cl_object obj, ...));
extern cl_object Lprint _ARGS((int narg, cl_object obj, ...));
extern cl_object Lpprint _ARGS((int narg, cl_object obj, ...));
extern cl_object Lprinc _ARGS((int narg, cl_object obj, ...));
extern cl_object Lwrite_char _ARGS((int narg, cl_object c, ...));
extern cl_object Lwrite_string _ARGS((int narg, cl_object strng, ...));
extern cl_object Lwrite_line _ARGS((int narg, cl_object strng, ...));
extern cl_object Lterpri _ARGS((int narg, ...));
extern cl_object Lfresh_line _ARGS((int narg, ...));
extern cl_object Lforce_output _ARGS((int narg, ...));
extern cl_object Lclear_output _ARGS((int narg, ...));
extern cl_object Lwrite_byte _ARGS((int narg, cl_object integer, cl_object binary_output_stream));
extern cl_object Lwrite_bytes _ARGS((int narg, cl_object stream, cl_object string, cl_object start, cl_object end));

/* profile.c */

extern cl_object siLprofile _ARGS((int narg, cl_object scale, cl_object start_address));
extern cl_object siLclear_profile _ARGS((int narg));
extern cl_object siLdisplay_profile _ARGS((int narg));

/* read.c */

extern cl_object Vreadtable;
extern cl_object Vread_default_float_format;
extern cl_object Vread_base;
extern cl_object Vread_suppress;
extern cl_object Kjunk_allowed;
extern cl_object siSsharp_comma;
extern cl_object Sstream_read_line;
extern cl_object Sstream_read_char;
extern cl_object Sstream_unread_char;
extern cl_object Sstream_peek_char;
extern cl_object Sstream_listen;
extern cl_object Sstream_clear_input;
extern cl_object siLsharp_comma_reader_for_compiler _ARGS((int, cl_object, cl_object, cl_object));
extern cl_object Lread _ARGS((int narg, ...));
extern cl_object Lread_preserving_whitespace _ARGS((int narg, ...));
extern cl_object Lread_delimited_list _ARGS((int narg, cl_object d, ...));
extern cl_object Lread_line _ARGS((int narg, ...));
extern cl_object Lread_char _ARGS((int narg, ...));
extern cl_object Lunread_char _ARGS((int narg, cl_object c, ...));
extern cl_object Lpeek_char _ARGS((int narg, ...));
extern cl_object Llisten _ARGS((int narg, ...));
extern cl_object Lread_char_no_hang _ARGS((int narg, ...));
extern cl_object Lclear_input _ARGS((int narg, ...));
extern cl_object Lparse_integer _ARGS((int narg, cl_object strng, ...));
extern cl_object Lread_byte _ARGS((int narg, cl_object binary_input_stream, ...));
extern cl_object Lread_bytes _ARGS((int narg, cl_object stream, cl_object string, cl_object start, cl_object end));
extern cl_object Lcopy_readtable _ARGS((int narg, ...));
extern cl_object Lreadtablep _ARGS((int narg, cl_object readtable));
extern cl_object Lset_syntax_from_char _ARGS((int narg, cl_object tochr, cl_object fromchr, ...));
extern cl_object Lset_macro_character _ARGS((int narg, cl_object chr, cl_object fnc, ...));
extern cl_object Lget_macro_character _ARGS((int narg, cl_object chr, ...));
extern cl_object Lmake_dispatch_macro_character _ARGS((int narg, cl_object chr, ...));
extern cl_object Lset_dispatch_macro_character _ARGS((int narg, cl_object dspchr, cl_object subchr, cl_object fnc, ...));
extern cl_object Lget_dispatch_macro_character _ARGS((int narg, cl_object dspchr, cl_object subchr, ...));
extern cl_object siLstring_to_object _ARGS((int narg, cl_object str));
extern cl_object siLstandard_readtable _ARGS((int narg));

/* reference.c */

extern cl_object Lfboundp _ARGS((int narg, cl_object sym));
extern cl_object Lsymbol_function _ARGS((int narg, cl_object sym));
extern cl_object siLcoerce_to_function _ARGS((int narg, cl_object form));
extern cl_object Lsymbol_value _ARGS((int narg, cl_object sym));
extern cl_object Lboundp _ARGS((int narg, cl_object sym));
extern cl_object Lmacro_function _ARGS((int narg, cl_object sym, ...));
extern cl_object Lspecial_form_p _ARGS((int narg, cl_object form));

/* sequence.c */

extern cl_object Lelt _ARGS((int narg, cl_object x, cl_object i));
extern cl_object siLelt_set _ARGS((int narg, cl_object seq, cl_object index, cl_object val));
extern cl_object Lsubseq _ARGS((int narg, cl_object sequence, cl_object start, ...));
extern cl_object Lcopy_seq _ARGS((int narg, cl_object x));
extern cl_object Llength _ARGS((int narg, cl_object x));
extern cl_object Lreverse _ARGS((int narg, cl_object x));
extern cl_object Lnreverse _ARGS((int narg, cl_object x));

/* stacks.c */

extern cl_object Kcatch, Kcatchall, Kprotect;
extern cl_object siLihs_top _ARGS((int narg));
extern cl_object siLihs_fun _ARGS((int narg, cl_object arg));
extern cl_object siLihs_env _ARGS((int narg, cl_object arg));
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
extern cl_object Lmake_string _ARGS((int narg, cl_object size, ...));
extern cl_object Lchar _ARGS((int narg, cl_object s, cl_object i));
extern cl_object siLchar_set _ARGS((int narg, cl_object str, cl_object index, cl_object c));
extern cl_object Lstring_eq _ARGS((int narg, cl_object string1, cl_object string2, ...));
extern cl_object Lstring_equal _ARGS((int narg, cl_object string1, cl_object string2, ...));
extern cl_object Lstring_cmp _ARGS((int narg, int sign, int boundary, cl_object *ARGS));
extern cl_object Lstring_l _ARGS((int narg, ...));
extern cl_object Lstring_g _ARGS((int narg, ...));
extern cl_object Lstring_le _ARGS((int narg, ...));
extern cl_object Lstring_ge _ARGS((int narg, ...));
extern cl_object Lstring_neq _ARGS((int narg, ...));
extern cl_object Lstring_compare _ARGS((int narg, int sign, int boundary, cl_object *ARGS));
extern cl_object Lstring_lessp _ARGS((int narg, ...));
extern cl_object Lstring_greaterp _ARGS((int narg, ...));
extern cl_object Lstring_not_greaterp _ARGS((int narg, ...));
extern cl_object Lstring_not_lessp _ARGS((int narg, ...));
extern cl_object Lstring_not_equal _ARGS((int narg, ...));
extern cl_object Lstring_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object Lstring_left_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object Lstring_right_trim _ARGS((int narg, cl_object char_bag, cl_object strng));
extern cl_object Lstring_trim0 _ARGS((int narg, bool left_trim, bool right_trim, cl_object char_bag, cl_object strng));
extern cl_object Lstring_case(int narg, int (*casefun)(), cl_object *ARGS);
extern cl_object Lstring_upcase _ARGS((int narg, ...));
extern cl_object Lstring_downcase _ARGS((int narg, ...));
extern cl_object Lstring_capitalize _ARGS((int narg, ...));
extern cl_object Lnstring_case(int narg, int (*casefun)(), cl_object *ARGS);
extern cl_object Lnstring_upcase _ARGS((int narg, ...));
extern cl_object Lnstring_downcase _ARGS((int narg, ...));
extern cl_object Lnstring_capitalize _ARGS((int narg, ...));
extern cl_object Lstring _ARGS((int narg, cl_object x));
extern cl_object siLstring_concatenate _ARGS((int narg, ...));

/* structure.c */

extern cl_object siSstructure_print_function;
extern cl_object siSstructure_slot_descriptions;
extern cl_object Sstructure_object;
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

extern cl_object siSpname;
extern cl_object Vgensym_counter;
extern cl_object Lmake_symbol _ARGS((int narg, cl_object str));
extern cl_object Lget _ARGS((int narg, cl_object sym, cl_object indicator, ...));
extern cl_object Lremprop _ARGS((int narg, cl_object sym, cl_object prop));
extern cl_object Lsymbol_plist _ARGS((int narg, cl_object sym));
extern cl_object Lgetf _ARGS((int narg, cl_object place, cl_object indicator, ...));
extern cl_object Lget_properties _ARGS((int narg, cl_object place, cl_object indicator_list));
extern cl_object Lsymbol_name _ARGS((int narg, cl_object sym));
extern cl_object Lcopy_symbol _ARGS((int narg, cl_object sym, ...));
extern cl_object Lgensym _ARGS((int narg, ...));
extern cl_object Lgentemp _ARGS((int narg, ...));
extern cl_object Lsymbol_package _ARGS((int narg, cl_object sym));
extern cl_object Lkeywordp _ARGS((int narg, cl_object sym));
extern cl_object siLput_f _ARGS((int narg, cl_object plist, cl_object value, cl_object indicator));
extern cl_object siLrem_f _ARGS((int narg, cl_object plist, cl_object indicator));
extern cl_object siLset_symbol_plist _ARGS((int narg, cl_object sym, cl_object plist));
extern cl_object siLputprop _ARGS((int narg, cl_object sym, cl_object value, cl_object indicator));
extern cl_object siLput_properties _ARGS((int narg, cl_object sym, ...));
extern cl_object siLAmake_special _ARGS((int narg, cl_object sym));
extern cl_object siLAmake_constant _ARGS((int narg, cl_object sym, cl_object val));

/* tcp.c */

#ifdef TCP
extern cl_object Lopen_client_stream _ARGS((int narg, cl_object host, cl_object port));
extern cl_object Lopen_server_stream _ARGS((int narg, cl_object port));
#endif

/* time.c */

extern cl_object Lget_universal_time _ARGS((int narg));
extern cl_object Lsleep _ARGS((int narg, cl_object z));
extern cl_object Lget_internal_run_time _ARGS((int narg));
extern cl_object Lget_internal_real_time _ARGS((int narg));
extern cl_object Lget_local_time_zone _ARGS((int narg));
extern cl_object Ldaylight_saving_timep _ARGS((int narg, ...));

/* typespec.c */

extern cl_object Squote;
extern cl_object Slambda;
extern cl_object Sspecial;
extern cl_object St;
extern cl_object Snil;
extern cl_object Scommon;
extern cl_object Ssequence;
extern cl_object Snull;
extern cl_object Scons;
extern cl_object Slist;
extern cl_object Ssymbol;
extern cl_object Sarray;
extern cl_object Svector;
extern cl_object Sbit_vector;
extern cl_object Sstring;
extern cl_object Ssimple_array;
extern cl_object Ssimple_vector;
extern cl_object Ssimple_string;
extern cl_object Ssimple_bit_vector;
extern cl_object Sfunction;
extern cl_object Spathname;
extern cl_object Slogical_pathname;
extern cl_object Scharacter;
extern cl_object Sbase_char;
extern cl_object Sextended_char;
extern cl_object Scompiled_function;
extern cl_object Snumber;
extern cl_object Sreal;
extern cl_object Srational;
extern cl_object Sfloat;
extern cl_object Sinteger;
extern cl_object Sratio;
extern cl_object Sshort_float;
extern cl_object Sstandard_char;
extern cl_object Sfixnum;
extern cl_object Scomplex;
extern cl_object Ssingle_float;
extern cl_object Spackage;
extern cl_object Sbignum;
extern cl_object Srandom_state;
extern cl_object Sdouble_float;
extern cl_object Sstream;
extern cl_object Sbit;
extern cl_object Sreadtable;
extern cl_object Slong_float;
extern cl_object Shash_table;
extern cl_object Ssigned_char;
extern cl_object Sunsigned_char;
extern cl_object Ssigned_short;
extern cl_object Sunsigned_short;
extern cl_object Sinstance;
extern cl_object Sdispatch_function;
extern cl_object Sstructure;
extern cl_object Ssatisfies;
extern cl_object Smember;
extern cl_object Snot;
extern cl_object Sor;
extern cl_object Sand;
extern cl_object Svalues;
extern cl_object Smod;
extern cl_object Ssigned_byte;
extern cl_object Sunsigned_byte;
extern cl_object SX;
extern cl_object Splusp;
extern cl_object Skeyword;
extern cl_object TSor_string_symbol;
extern cl_object TSor_symbol_string_package;
extern cl_object TSnon_negative_integer;
extern cl_object TSpositive_number;
extern cl_object TSor_integer_float;
extern cl_object TSor_rational_float;
extern cl_object TSor_pathname_string_symbol;
extern cl_object TSor_pathname_string_symbol_stream;
extern cl_object Ltype_of _ARGS((int narg, cl_object x));
extern cl_object Ssubtypep;

/* unify.c */

#ifdef LOCATIVE
extern cl_object Ltrail_mark _ARGS((int narg));
extern cl_object Ltrail_restore _ARGS((int narg));
extern cl_object Ltrail_unmark _ARGS((int narg));
extern cl_object Lget_value _ARGS((int narg, cl_object v, cl_object x));
extern cl_object Lget_constant _ARGS((int narg, cl_object c, cl_object x));
extern cl_object Lget_nil _ARGS((int narg, cl_object arg));
extern cl_object Lget_cons _ARGS((int narg, cl_object arg));
extern cl_object Lget_instance _ARGS((int narg, cl_object x, cl_object class, cl_object arity));
extern cl_object Lunify_slot _ARGS((int narg));
extern cl_object Lunify_value _ARGS((int narg, cl_object loc));
extern cl_object Lunify_constant _ARGS((int narg, cl_object c));
extern cl_object Lunify_nil _ARGS((int narg));
extern cl_object Lmake_locative _ARGS((int narg, ...));
extern cl_object Llocativep _ARGS((int narg, cl_object obje));
extern cl_object Lunboundp _ARGS((int narg, cl_object loc));
extern cl_object Ldereference _ARGS((int narg, cl_object x));
extern cl_object Lmake_variable _ARGS((int narg, cl_object name));
extern cl_object Ssetq;
extern cl_object Sunify_slot;
extern cl_object Lunify_variable _ARGS((cl_object var));
#endif

/* unixfsys.c */

extern cl_object Klist_all;
extern cl_object Ltruename _ARGS((int narg, cl_object file));
extern cl_object Lrename_file _ARGS((int narg, cl_object old, cl_object new));
extern cl_object Ldelete_file _ARGS((int narg, cl_object file));
extern cl_object Lprobe_file _ARGS((int narg, cl_object file));
extern cl_object Lfile_write_date _ARGS((int narg, cl_object file));
extern cl_object Lfile_author _ARGS((int narg, cl_object file));
extern cl_object Luser_homedir_pathname _ARGS((int narg, ...));
extern cl_object siLchdir _ARGS((int narg, cl_object directory));
extern cl_object siLstring_match _ARGS((int narg, cl_object string, cl_object pattern));
extern cl_object Ldirectory _ARGS((int narg, ...));

/* unixint.c */

extern cl_object siLcatch_bad_signals _ARGS((int narg));
extern cl_object siLuncatch_bad_signals _ARGS((int narg));

/* unixsys.c */

extern cl_object siLsystem _ARGS((int narg, cl_object cmd));
extern cl_object siLopen_pipe _ARGS((int narg, cl_object cmd));
