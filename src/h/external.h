/* -*- mode: c; c-basic-offset: 8 -*- */
#ifdef __cplusplus
extern "C" {
#endif

#define _ARGS(x) x

/*
 * Per-thread data.
 */

struct cl_env_struct {
	/* The four stacks in ECL. */

	/*
	 * The lisp stack, which is used mainly for keeping the arguments of a
	 * function before it is invoked, and also by the compiler and by the
	 * reader when they are building some data structure.
	 */
	cl_index stack_size;
	cl_object *stack;
	cl_object *stack_top;
	cl_object *stack_limit;

	/*
	 * The BinDing Stack stores the bindings of special variables.
	 */
	cl_index bds_size;
	struct bds_bd *bds_org;
	struct bds_bd *bds_top;
	struct bds_bd *bds_limit;
	cl_object bindings_hash;

	/*
	 * The Invocation History Stack (IHS) keeps a list of the names of the
	 * functions that are invoked, together with their lexical
	 * environments.
	 */
	struct ihs_frame *ihs_top;

	/*
	 * The FRames Stack (FRS) is a list of frames or jump points, and it
	 * is used by different high-level constructs (BLOCK, TAGBODY, CATCH...)
	 * to set return points.
	 */
	cl_index frs_size;
	struct ecl_frame *frs_org;
	struct ecl_frame *frs_top;
	struct ecl_frame *frs_limit;
	struct ecl_frame *nlj_fr;

	/*
	 * The following pointers to the C Stack are used to ensure that a
	 * recursive function does not enter an infinite loop and exhausts all
	 * memory. They will eventually disappear, because most operating
	 * systems already take care of this.
	 */
	int *cs_org;
	int *cs_limit;
	cl_index cs_size;

	/* Array where values are returned by functions. */
	cl_index nvalues;
	cl_object values[ECL_MULTIPLE_VALUES_LIMIT];

	/* Private variables used by different parts of ECL: */
	/* ... the reader ... */
	cl_object string_pool;

	/* ... the compiler ... */
	struct cl_compiler_env *c_env;

	/* ... the formatter ... */
	cl_object fmt_aux_stream;

	/* ... the pretty printer ... */
	bool print_pretty;
	short *queue;
	short *indent_stack;
	int qh, qt, qc, isp, iisp;

	/* ... arithmetics ... */
	/* Note: if you change the size of these registers, change also
	   BIGNUM_REGISTER_SIZE in config.h */
	cl_object big_register[3];
#ifdef WITH_GMP
	mp_limb_t big_register_limbs[3][16];
#endif /* WITH_GMP */

#ifdef ECL_THREADS
	cl_object own_process;
#endif
	int interrupt_pending;

	/* The following is a hash table for caching invocations of
	   generic functions. In a multithreaded environment we must
	   queue operations in which the hash is cleared from updated
	   generic functions. */
#ifdef CLOS
#ifdef ECL_THREADS
	cl_object method_hash_clear_list;
#endif
	cl_object method_hash;
	cl_object method_spec_vector;
	cl_fixnum method_generation;
#endif

	/* foreign function interface */
	void *fficall;

	/* Alternative stack for processing signals */
	void *altstack;
	cl_index altstack_size;
};

#ifndef __GNUC__
#define __attribute__(x)
#endif
#if defined(ECL_THREADS)
# ifdef WITH___THREAD
extern __thread struct cl_env_struct * cl_env_p;
#define cl_env (*cl_env_p)
# else
#define cl_env (*ecl_process_env())
extern ECL_API struct cl_env_struct *ecl_process_env(void) __attribute__((const));
# endif
#else
extern ECL_API struct cl_env_struct cl_env;
#endif

/*
 * Per-process data.
 */

struct cl_core_struct {
	cl_object packages;
	cl_object lisp_package;
	cl_object user_package;
	cl_object keyword_package;
	cl_object system_package;
#ifdef CLOS
	cl_object clos_package;
# ifdef ECL_CLOS_STREAMS
	cl_object gray_package;
# endif
#endif
#ifdef ECL_THREADS
	cl_object mp_package;
#endif
	cl_object packages_to_be_created;

	cl_object pathname_translations;

	cl_object terminal_io;
	cl_object null_stream;
	cl_object standard_readtable;
	cl_object dispatch_reader;
	cl_object default_dispatch_macro;

	cl_object char_names;
	cl_object null_string;

	cl_object plus_half;
	cl_object minus_half;
	cl_object imag_unit;
	cl_object minus_imag_unit;
	cl_object imag_two;
	cl_object singlefloat_zero;
	cl_object doublefloat_zero;
#ifdef ECL_LONG_FLOAT
	cl_object longfloat_zero;
#endif

	cl_object gensym_prefix;
	cl_object gentemp_prefix;
	cl_object gentemp_counter;

	cl_object Jan1st1970UT;

	cl_object system_properties;

#ifdef ECL_THREADS
	cl_object processes;
	pthread_mutex_t global_lock;
#endif
	cl_object libraries;
	cl_object to_be_finalized;
	cl_object bytes_consed;
	cl_object gc_counter;
	bool gc_stats;
	int path_max;
};

extern ECL_API struct cl_core_struct cl_core;

/* alloc.c / alloc_2.c */

extern ECL_API cl_object cl_alloc_object(cl_type t);
extern ECL_API cl_object cl_alloc_instance(cl_index slots);
extern ECL_API cl_object ecl_cons(cl_object a, cl_object d);
extern ECL_API cl_object ecl_list1(cl_object a);
#ifdef GBC_BOEHM
extern ECL_API cl_object si_gc(cl_object area);
extern ECL_API cl_object si_gc_dump(void);
extern ECL_API cl_object si_gc_stats(cl_object enable);
#define cl_alloc GC_malloc_ignore_off_page
#define cl_alloc_atomic GC_malloc_atomic_ignore_off_page
#define cl_alloc_align(s,d) GC_malloc_ignore_off_page(s)
#define cl_alloc_atomic_align(s,d) GC_malloc_atomic_ignore_off_page(s)
#define cl_dealloc(p) GC_free(p)
#define ecl_register_static_root(x) ecl_register_root(x)
#else
extern ECL_API cl_object si_allocate _ARGS((cl_narg narg, cl_object type, cl_object qty, ...));
extern ECL_API cl_object si_maximum_allocatable_pages _ARGS((cl_narg narg, cl_object type));
extern ECL_API cl_object si_allocated_pages _ARGS((cl_narg narg, cl_object type));
extern ECL_API cl_object si_alloc_contpage _ARGS((cl_narg narg, cl_object qty, ...));
extern ECL_API cl_object si_allocated_contiguous_pages _ARGS((cl_narg narg));
extern ECL_API cl_object si_maximum_contiguous_pages _ARGS((cl_narg narg));
extern ECL_API cl_object si_allocate_contiguous_pages _ARGS((cl_narg narg, cl_object qty, ...));
extern ECL_API cl_object si_get_hole_size _ARGS((cl_narg narg));
extern ECL_API cl_object si_set_hole_size _ARGS((cl_narg narg, cl_object size));
extern ECL_API cl_object si_ignore_maximum_pages _ARGS((cl_narg narg, ...));
extern ECL_API void *cl_alloc(cl_index n);
extern ECL_API void *cl_alloc_align(cl_index size, cl_index align);
extern ECL_API void *ecl_alloc_uncollectable(size_t size);
extern ECL_API void ecl_free_uncollectable(void *);
extern ECL_API void cl_dealloc(void *p);
#define cl_alloc_atomic(x) cl_alloc(x)
#define cl_alloc_atomic_align(x,s) cl_alloc_align(x,s)
#define ecl_register_static_root(x) ecl_register_root(x);
#endif /* GBC_BOEHM */

/* all_symbols */

extern ECL_API cl_object si_mangle_name _ARGS((cl_narg narg, cl_object symbol, ...));

typedef union {
	struct {
		const char *name;
		int type;
		void *fun;
		short narg;
		cl_object value;
	} init;
	struct ecl_symbol data;
} cl_symbol_initializer;
extern ECL_API cl_symbol_initializer cl_symbols[];
extern ECL_API cl_index cl_num_symbols_in_core;

#define ECL_SYM(name,code) ((cl_object)(cl_symbols+(code)))

/* apply.c */

extern ECL_API cl_object APPLY_fixed(cl_narg n, cl_object (*f)(), cl_object *x);
extern ECL_API cl_object APPLY(cl_narg n, cl_objectfn, cl_object *x);
extern ECL_API cl_object APPLY_closure(cl_narg n, cl_objectfn, cl_object cl, cl_object *x);


/* array.c */

extern ECL_API cl_object cl_row_major_aref(cl_object x, cl_object i);
extern ECL_API cl_object si_row_major_aset(cl_object x, cl_object i, cl_object v);
extern ECL_API cl_object si_make_vector(cl_object etype, cl_object dim, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff);
extern ECL_API cl_object cl_array_element_type(cl_object a);
extern ECL_API cl_object cl_array_rank(cl_object a);
extern ECL_API cl_object cl_array_dimension(cl_object a, cl_object index);
extern ECL_API cl_object cl_array_total_size(cl_object a);
extern ECL_API cl_object cl_adjustable_array_p(cl_object a);
extern ECL_API cl_object cl_array_displacement(cl_object a);
extern ECL_API cl_object cl_svref(cl_object x, cl_object index);
extern ECL_API cl_object si_svset(cl_object x, cl_object index, cl_object v);
extern ECL_API cl_object cl_array_has_fill_pointer_p(cl_object a);
extern ECL_API cl_object cl_fill_pointer(cl_object a);
extern ECL_API cl_object si_fill_pointer_set(cl_object a, cl_object fp);
extern ECL_API cl_object si_replace_array(cl_object old_obj, cl_object new_obj);
extern ECL_API cl_object cl_aref _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object si_aset _ARGS((cl_narg narg, cl_object v, cl_object x, ...));
extern ECL_API cl_object si_make_pure_array(cl_object etype, cl_object dims, cl_object adj, cl_object fillp, cl_object displ, cl_object disploff);

extern ECL_API cl_index ecl_to_index(cl_object n);
extern ECL_API cl_object ecl_aref(cl_object x, cl_index index);
extern ECL_API cl_object ecl_aref1(cl_object v, cl_index index);
extern ECL_API cl_object ecl_aset(cl_object x, cl_index index, cl_object value);
extern ECL_API cl_object ecl_aset1(cl_object v, cl_index index, cl_object val);
extern ECL_API void ecl_array_allocself(cl_object x);
extern ECL_API cl_elttype ecl_array_elttype(cl_object x);
extern ECL_API cl_elttype ecl_symbol_to_elttype(cl_object x);
extern ECL_API cl_object ecl_elttype_to_symbol(cl_elttype aet);
extern ECL_API void ecl_copy_subarray(cl_object dest, cl_index i0, cl_object orig, cl_index i1, cl_index l);
extern ECL_API void ecl_reverse_subarray(cl_object dest, cl_index i0, cl_index i1);


/* assignment.c */

extern ECL_API cl_object cl_set(cl_object var, cl_object val);
extern ECL_API cl_object cl_makunbound(cl_object sym);
extern ECL_API cl_object cl_fmakunbound(cl_object sym);
extern ECL_API cl_object si_fset _ARGS((cl_narg narg, cl_object fun, cl_object def, ...));
extern ECL_API cl_object si_get_sysprop(cl_object sym, cl_object prop);
extern ECL_API cl_object si_put_sysprop(cl_object sym, cl_object prop, cl_object value);
extern ECL_API cl_object si_rem_sysprop(cl_object sym, cl_object prop);

extern ECL_API void ecl_clear_compiler_properties(cl_object sym);

/* big.c */

extern ECL_API cl_object big_register0_get(void);
extern ECL_API cl_object big_register1_get(void);
extern ECL_API cl_object big_register2_get(void);
extern ECL_API cl_object big_register_copy(cl_object x);
extern ECL_API cl_object big_register_normalize(cl_object x);
extern ECL_API void big_register_free(cl_object x);
extern ECL_API cl_object bignum1(cl_fixnum val);
#ifdef WITH_GMP
extern ECL_API cl_object bignum2(mp_limb_t hi, mp_limb_t lo);
#else  /* WITH_GMP */
extern ECL_API cl_object bignum2(cl_fixnum hi, cl_fixnum lo);
#endif /* WITH_GMP */
extern ECL_API cl_object big_set_fixnum(cl_object x, cl_object fix);
extern ECL_API cl_object big_copy(cl_object x);
extern ECL_API cl_object big_minus(cl_object x);
extern ECL_API cl_object big_plus(cl_object x, cl_object y);
extern ECL_API cl_object big_normalize(cl_object x);
extern ECL_API double big_to_double(cl_object x);


/* cfun.c */

extern ECL_API cl_object si_compiled_function_name(cl_object fun);
extern ECL_API cl_object si_compiled_function_block(cl_object fun);
extern ECL_API cl_object cl_function_lambda_expression(cl_object fun);

extern ECL_API cl_object cl_make_cfun(void *c_function, cl_object name, cl_object block, int narg);
extern ECL_API cl_object cl_make_cfun_va(void *c_function, cl_object name, cl_object block);
extern ECL_API cl_object cl_make_cclosure_va(void *c_function, cl_object env, cl_object block);
extern ECL_API void cl_def_c_function(cl_object sym, void *c_function, int narg);
extern ECL_API void cl_def_c_macro(cl_object sym, void *c_function, int narg);
extern ECL_API void cl_def_c_function_va(cl_object sym, void *c_function);


/* character.c */

extern ECL_API cl_object cl_digit_char_p _ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_charE _ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_charNE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charL _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charG _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charLE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_charGE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_equal _ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_char_not_equal _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_lessp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_greaterp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_not_greaterp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_char_not_lessp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_digit_char _ARGS((cl_narg narg, cl_object w, ...));

extern ECL_API cl_object cl_alpha_char_p(cl_object c);
extern ECL_API cl_object cl_alphanumericp(cl_object c);
extern ECL_API cl_object cl_both_case_p(cl_object c);
extern ECL_API cl_object cl_char_code(cl_object c);
extern ECL_API cl_object cl_char_downcase(cl_object c);
extern ECL_API cl_object cl_char_int(cl_object c);
extern ECL_API cl_object cl_char_name(cl_object c);
extern ECL_API cl_object cl_char_upcase(cl_object c);
extern ECL_API cl_object cl_character(cl_object x);
extern ECL_API cl_object cl_code_char(cl_object c);
extern ECL_API cl_object cl_graphic_char_p(cl_object c);
extern ECL_API cl_object cl_lower_case_p(cl_object c);
extern ECL_API cl_object cl_name_char(cl_object s);
extern ECL_API cl_object cl_standard_char_p(cl_object c);
extern ECL_API cl_object cl_upper_case_p(cl_object c);

extern ECL_API bool ecl_alpha_char_p(cl_index c);
extern ECL_API bool ecl_alphanumericp(cl_index c);
extern ECL_API bool ecl_both_case_p(cl_index c);
extern ECL_API cl_index ecl_char_downcase(cl_index c);
extern ECL_API cl_index ecl_char_upcase(cl_index c);
extern ECL_API bool ecl_graphic_char_p(cl_index c);
extern ECL_API bool ecl_lower_case_p(cl_index c);
extern ECL_API bool ecl_standard_char_p(cl_index c);
extern ECL_API bool ecl_base_char_p(cl_index c);
extern ECL_API bool ecl_upper_case_p(cl_index c);

extern ECL_API int ecl_base_string_case(cl_object s);
extern ECL_API cl_fixnum ecl_char_code(cl_object c);
extern ECL_API int ecl_base_char_code(cl_object c);
extern ECL_API int ecl_digitp(int i, int r);
extern ECL_API bool ecl_char_eq(cl_object x, cl_object y);
extern ECL_API int ecl_char_cmp(cl_object x, cl_object y);
extern ECL_API bool ecl_char_equal(cl_object x, cl_object y);
extern ECL_API int ecl_char_compare(cl_object x, cl_object y);
extern ECL_API short ecl_digit_char(cl_fixnum w, cl_fixnum r);

/* clos.c */

#ifdef CLOS
extern ECL_API cl_object cl_find_class _ARGS((cl_narg narg, cl_object name, ...));
extern ECL_API cl_object cl_class_of(cl_object x);
#endif

/* cmpaux.c */

extern ECL_API cl_object si_specialp(cl_object sym);

extern ECL_API cl_fixnum ecl_ifloor(cl_fixnum x, cl_fixnum y);
extern ECL_API cl_fixnum ecl_imod(cl_fixnum x, cl_fixnum y);
extern ECL_API char ecl_to_char(cl_object x);
extern ECL_API cl_fixnum ecl_to_fixnum(cl_object x);
extern ECL_API cl_index ecl_to_unsigned_integer(cl_object x);
extern ECL_API float ecl_to_float(cl_object x);
extern ECL_API double ecl_to_double(cl_object x);
extern ECL_API int ecl_aref_bv(cl_object x, cl_index index);
extern ECL_API int ecl_aset_bv(cl_object x, cl_index index, int value);
extern ECL_API void cl_throw(cl_object tag) /*__attribute__((noreturn))*/;
extern ECL_API void cl_return_from(cl_object block_id, cl_object block_name) /*__attribute__((noreturn))*/;
extern ECL_API void cl_go(cl_object tag_id, cl_object label) /*__attribute__((noreturn))*/;
extern ECL_API void cl_parse_key(cl_va_list args, int nkey, cl_object *keys, cl_object *vars, cl_object *rest, bool allow_other_keys);
extern ECL_API cl_object cl_grab_rest_args(cl_va_list args);

/* compiler.c */

extern ECL_API cl_object si_macrolet_function(cl_object form, cl_object env);
extern ECL_API cl_object si_process_lambda_list(cl_object lambda_list, cl_object context);
extern ECL_API cl_object si_process_lambda(cl_object lambda);
extern ECL_API cl_object si_make_lambda(cl_object name, cl_object body);
extern ECL_API cl_object si_function_block_name(cl_object name);
extern ECL_API cl_object si_valid_function_name_p(cl_object name);
extern ECL_API cl_object si_process_declarations _ARGS((cl_narg narg, cl_object body, ...));

extern ECL_API cl_object si_eval_with_env _ARGS((cl_narg narg, cl_object form, ...));

/* interpreter.c */

extern ECL_API cl_object si_interpreter_stack _ARGS((cl_narg narg));
extern ECL_API cl_object ecl_stack_frame_open(cl_object f, cl_index size);
extern ECL_API void ecl_stack_frame_enlarge(cl_object f, cl_index size);
extern ECL_API void ecl_stack_frame_push(cl_object f, cl_object o);
extern ECL_API void ecl_stack_frame_push_values(cl_object f);
extern ECL_API cl_object ecl_stack_frame_from_va_list(cl_object f, cl_va_list args);
extern ECL_API cl_object ecl_stack_frame_pop_values(cl_object f);
extern ECL_API cl_object ecl_stack_frame_elt(cl_object f, cl_index n);
extern ECL_API void ecl_stack_frame_elt_set(cl_object f, cl_index n, cl_object o);
extern ECL_API cl_object ecl_stack_frame_copy(cl_object f, cl_object size);
extern ECL_API void ecl_stack_frame_close(cl_object f);
extern ECL_API cl_object ecl_apply_from_stack_frame(cl_object f, cl_object o);
#define ECL_STACK_FRAME_SIZE(f) ((f)->frame.top - (f)->frame.bottom)
#define si_apply_from_stack_frame ecl_apply_from_stack_frame

extern ECL_API void cl_stack_push(cl_object o);
extern ECL_API cl_object cl_stack_pop(void);
extern ECL_API cl_index cl_stack_index(void);
extern ECL_API void cl_stack_set_size(cl_index new_size);
extern ECL_API void cl_stack_set_index(cl_index sp);
extern ECL_API void cl_stack_pop_n(cl_index n);
extern ECL_API void cl_stack_insert(cl_index where, cl_index n);
extern ECL_API cl_index cl_stack_push_list(cl_object list);
extern ECL_API void cl_stack_push_n(cl_index n, cl_object *args);
extern ECL_API cl_index cl_stack_push_values(void);
extern ECL_API void cl_stack_pop_values(cl_index n);
extern ECL_API cl_object ecl_interpret(cl_object frame, cl_object env, cl_object bytecodes, cl_index offset);

/* disassembler.c */

extern ECL_API cl_object si_bc_disassemble(cl_object v);
extern ECL_API cl_object si_bc_split(cl_object v);
extern ECL_API cl_object si_bc_file(cl_object v);

/* error.c */

extern ECL_API cl_object cl_error _ARGS((cl_narg narg, cl_object eformat, ...)) /*__attribute__((noreturn))*/;
extern ECL_API cl_object cl_cerror _ARGS((cl_narg narg, cl_object cformat, cl_object eformat, ...));

extern ECL_API void ecl_internal_error(const char *s) /*__attribute__((noreturn))*/;
extern ECL_API void ecl_cs_overflow(void) /*__attribute__((noreturn))*/;
extern ECL_API void FEprogram_error(const char *s, int narg, ...) /*__attribute__((noreturn))*/;
extern ECL_API void FEcontrol_error(const char *s, int narg, ...) /*__attribute__((noreturn))*/;
extern ECL_API void FEreader_error(const char *s, cl_object stream, int narg, ...) /*__attribute__((noreturn))*/;
#define FEparse_error FEreader_error
extern ECL_API void FEerror(const char *s, int narg, ...) /*__attribute__((noreturn))*/;
extern ECL_API void FEcannot_open(cl_object fn) /*__attribute__((noreturn))*/;
extern ECL_API void FEend_of_file(cl_object strm) /*__attribute__((noreturn))*/;
extern ECL_API void FEclosed_stream(cl_object strm) /*__attribute__ ((noreturn))*/;
extern ECL_API void FEwrong_type_argument(cl_object type, cl_object value) /*__attribute__((noreturn))*/;
extern ECL_API void FEwrong_num_arguments(cl_object fun) /*__attribute__((noreturn))*/;
extern ECL_API void FEwrong_num_arguments_anonym(void) /*__attribute__((noreturn))*/;
extern ECL_API void FEunbound_variable(cl_object sym) /*__attribute__((noreturn))*/;
extern ECL_API void FEinvalid_macro_call(cl_object obj) /*__attribute__((noreturn))*/;
extern ECL_API void FEinvalid_variable(const char *s, cl_object obj) /*__attribute__((noreturn))*/;
extern ECL_API void FEassignment_to_constant(cl_object v) /*__attribute__((noreturn))*/;
extern ECL_API void FEundefined_function(cl_object fname) /*__attribute__((noreturn))*/;
extern ECL_API void FEinvalid_function(cl_object obj) /*__attribute__((noreturn))*/;
extern ECL_API void FEinvalid_function_name(cl_object obj) /*__attribute__((noreturn))*/;
extern ECL_API cl_object CEerror(cl_object c, const char *err_str, int narg, ...);
extern ECL_API void FEillegal_index(cl_object x, cl_object i);
extern ECL_API void FEtype_error_symbol(cl_object obj) /*__attribute__((noreturn))*/;
extern ECL_API void FElibc_error(const char *msg, int narg, ...) /*__attribute__((noreturn))*/;
#if defined(mingw32) || defined(_MSC_VER)
extern ECL_API void FEwin32_error(const char *msg, int narg, ...) /*__attribute__((noreturn))*/;
#endif

/* eval.c */

extern ECL_API cl_object cl_funcall _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_apply _ARGS((cl_narg narg, cl_object fun, cl_object arg, ...));
extern ECL_API cl_object si_safe_eval _ARGS((cl_narg narg, cl_object form, cl_object env, cl_object value, ...));
#define cl_safe_eval(form,env,value) si_safe_eval(3,form,env,value)
extern ECL_API cl_object *_ecl_va_sp(cl_narg narg);
extern ECL_API cl_object si_unlink_symbol(cl_object s);
extern ECL_API cl_object cl_eval(cl_object form);
extern ECL_API cl_object cl_constantp(cl_narg narg, cl_object arg, ...);

#define funcall cl_funcall
extern ECL_API cl_object cl_apply_from_stack(cl_index narg, cl_object fun);
extern ECL_API cl_object _ecl_link_call(cl_object sym, cl_objectfn *pLK, cl_object cblock, int narg, cl_va_list args);

/* ffi.c */

extern ECL_API cl_object si_allocate_foreign_data(cl_object tag, cl_object size);
extern ECL_API cl_object si_foreign_data_address(cl_object f);
extern ECL_API cl_object si_foreign_data_pointer(cl_object f, cl_object ndx, cl_object size, cl_object tag);
extern ECL_API cl_object si_foreign_data_ref(cl_object f, cl_object ndx, cl_object size, cl_object tag);
extern ECL_API cl_object si_foreign_data_ref_elt(cl_object f, cl_object ndx, cl_object tag);
extern ECL_API cl_object si_foreign_data_set(cl_object f, cl_object ndx, cl_object value);
extern ECL_API cl_object si_foreign_data_set_elt(cl_object f, cl_object ndx, cl_object tag, cl_object value);
extern ECL_API cl_object si_foreign_data_tag(cl_object x);
extern ECL_API cl_object si_foreign_data_recast(cl_object f, cl_object size, cl_object tag);
extern ECL_API cl_object si_free_foreign_data(cl_object x);
extern ECL_API cl_object si_make_foreign_data_from_array(cl_object x);
extern ECL_API cl_object si_null_pointer_p(cl_object f);
extern ECL_API cl_object si_size_of_foreign_elt_type(cl_object tag);
extern ECL_API cl_object si_load_foreign_module(cl_object module);
extern ECL_API cl_object si_find_foreign_symbol(cl_object var, cl_object module, cl_object type, cl_object size);
extern ECL_API cl_object si_call_cfun(cl_narg, cl_object fun, cl_object return_type, cl_object arg_types, cl_object args, ...);
extern ECL_API cl_object si_make_dynamic_callback(cl_narg, cl_object fun, cl_object sym, cl_object return_type, cl_object arg_types, ...);

extern ECL_API cl_object ecl_make_foreign_data(cl_object tag, cl_index size, void *data);
extern ECL_API cl_object ecl_allocate_foreign_data(cl_object tag, cl_index size);
extern ECL_API void *ecl_foreign_data_pointer_safe(cl_object f);
extern ECL_API char *ecl_base_string_pointer_safe(cl_object f);
extern ECL_API cl_object ecl_null_terminated_base_string(cl_object s);
extern ECL_API cl_object ecl_foreign_data_ref_elt(void *p, enum ecl_ffi_tag type);
extern ECL_API void ecl_foreign_data_set_elt(void *p, enum ecl_ffi_tag type, cl_object value);

/* file.c */

#define ECL_LISTEN_NO_CHAR	0
#define ECL_LISTEN_AVAILABLE	1
#define ECL_LISTEN_EOF		-1

extern ECL_API cl_object cl_make_synonym_stream(cl_object sym);
extern ECL_API cl_object cl_synonym_stream_symbol(cl_object strm);
extern ECL_API cl_object cl_make_two_way_stream(cl_object strm1, cl_object strm2);
extern ECL_API cl_object cl_two_way_stream_input_stream(cl_object strm);
extern ECL_API cl_object cl_two_way_stream_output_stream(cl_object strm);
extern ECL_API cl_object cl_make_echo_stream(cl_object strm1, cl_object strm2);
extern ECL_API cl_object cl_echo_stream_input_stream(cl_object strm);
extern ECL_API cl_object cl_echo_stream_output_stream(cl_object strm);
extern ECL_API cl_object cl_make_string_output_stream _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_get_output_stream_string(cl_object strm);
extern ECL_API cl_object cl_streamp(cl_object strm);
extern ECL_API cl_object cl_input_stream_p(cl_object strm);
extern ECL_API cl_object cl_output_stream_p(cl_object strm);
extern ECL_API cl_object cl_stream_element_type(cl_object strm);
extern ECL_API cl_object cl_stream_external_format(cl_object strm);
extern ECL_API cl_object cl_file_length(cl_object strm);
extern ECL_API cl_object si_get_string_input_stream_index(cl_object strm);
extern ECL_API cl_object si_make_string_output_stream_from_string(cl_object strng);
extern ECL_API cl_object si_copy_stream(cl_object in, cl_object out);
extern ECL_API cl_object cl_open_stream_p(cl_object strm);
extern ECL_API cl_object cl_make_broadcast_stream _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_broadcast_stream_streams(cl_object strm);
extern ECL_API cl_object cl_make_concatenated_stream _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_concatenated_stream_streams(cl_object strm);
extern ECL_API cl_object cl_make_string_input_stream _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_close _ARGS((cl_narg narg, cl_object strm, ...));
extern ECL_API cl_object cl_open _ARGS((cl_narg narg, cl_object filename, ...));
extern ECL_API cl_object cl_file_position _ARGS((cl_narg narg, cl_object file_stream, ...));
extern ECL_API cl_object cl_file_string_length(cl_object stream, cl_object string);
extern ECL_API cl_object si_do_write_sequence(cl_object string, cl_object stream, cl_object start, cl_object end);
extern ECL_API cl_object si_do_read_sequence(cl_object string, cl_object stream, cl_object start, cl_object end);
extern ECL_API cl_object si_file_column(cl_object strm);
extern ECL_API cl_object cl_interactive_stream_p(cl_object strm);
extern ECL_API cl_object si_set_buffering_mode(cl_object strm, cl_object mode);

extern ECL_API bool ecl_input_stream_p(cl_object strm);
extern ECL_API bool ecl_output_stream_p(cl_object strm);
extern ECL_API cl_object ecl_open_stream(cl_object fn, enum ecl_smmode smm, cl_object if_exists, cl_object if_does_not_exist, cl_fixnum byte_size, bool char_stream_p, bool use_header_p);
extern ECL_API cl_object ecl_make_string_input_stream(cl_object strng, cl_index istart, cl_index iend);
extern ECL_API cl_object ecl_make_string_output_stream(cl_index line_length);
extern ECL_API cl_object ecl_read_byte(cl_object strm);
extern ECL_API void ecl_write_byte(cl_object byte, cl_object strm);
extern ECL_API int ecl_read_char_noeof(cl_object strm);
extern ECL_API int ecl_read_char(cl_object strm);
extern ECL_API void ecl_unread_char(int c, cl_object strm);
extern ECL_API int ecl_peek_char(cl_object strm);
extern ECL_API int ecl_write_char(int c, cl_object strm);
extern ECL_API void writestr_stream(const char *s, cl_object strm);
#define ecl_finish_output(x) ecl_force_output(x)
extern ECL_API void ecl_force_output(cl_object strm);
extern ECL_API void ecl_clear_input(cl_object strm);
extern ECL_API void ecl_clear_output(cl_object strm);
extern ECL_API bool ecl_listen_stream(cl_object strm);
extern ECL_API cl_object ecl_file_position(cl_object strm);
extern ECL_API cl_object ecl_file_position_set(cl_object strm, cl_object disp);
extern ECL_API int ecl_file_column(cl_object strm);
extern ECL_API cl_object ecl_make_stream_from_fd(cl_object host, int fd, enum ecl_smmode smm);
extern ECL_API cl_object ecl_make_stream_from_FILE(cl_object host, void *fd, enum ecl_smmode smm);
extern ECL_API int ecl_stream_to_handle(cl_object s, bool output);

/* finalize.c */

cl_object si_get_finalizer(cl_object o);
cl_object si_set_finalizer(cl_object o, cl_object finalizer);

/* format.c */

extern ECL_API cl_object cl_format _ARGS((cl_narg narg, cl_object stream, cl_object string, ...));

/* gbc.c */

#if !defined(GBC_BOEHM)
extern ECL_API cl_object si_room_report _ARGS((cl_narg narg));
extern ECL_API cl_object si_reset_gc_count _ARGS((cl_narg narg));
extern ECL_API cl_object si_gc_time _ARGS((cl_narg narg));
extern ECL_API cl_object si_gc(cl_object area);
#define GC_enabled() GC_enable
#define GC_enable() GC_enable = TRUE;
#define GC_disable() GC_enable = FALSE;
extern ECL_API bool GC_enable;
extern ECL_API cl_object (*GC_enter_hook)(void);
extern ECL_API cl_object (*GC_exit_hook)(void);
extern ECL_API void ecl_register_root(cl_object *p);
extern ECL_API void ecl_gc(cl_type t);
#endif

#ifdef GBC_BOEHM
#define GC_enabled() (!GC_dont_gc)
#define GC_enable() GC_dont_gc = FALSE;
#define GC_disable() GC_dont_gc = TRUE;
extern ECL_API void ecl_register_root(cl_object *p);
#endif /* GBC_BOEHM */


/* gfun.c */

#ifdef CLOS
extern ECL_API void _ecl_set_method_hash_size(struct cl_env_struct *env, cl_index size);
extern ECL_API cl_object si_clear_gfun_hash(cl_object what);
extern ECL_API cl_object clos_set_funcallable_instance_function(cl_object x, cl_object function_or_t);
extern ECL_API cl_object si_generic_function_p(cl_object instance);

extern ECL_API cl_object _ecl_standard_dispatch(cl_object frame, cl_object fun);
#endif /* CLOS */


/* hash.c */

extern ECL_API cl_object cl__make_hash_table(cl_object test, cl_object size, cl_object rehash_size, cl_object rehash_threshold, cl_object lockable);
extern ECL_API cl_object cl_hash_table_p(cl_object ht);
extern ECL_API cl_object si_hash_set(cl_object key, cl_object ht, cl_object val);
extern ECL_API cl_object cl_remhash(cl_object key, cl_object ht);
extern ECL_API cl_object cl_clrhash(cl_object ht);
extern ECL_API cl_object cl_hash_table_count(cl_object ht);
extern ECL_API cl_object cl_sxhash(cl_object key);
extern ECL_API cl_object cl_maphash(cl_object fun, cl_object ht);
extern ECL_API cl_object cl_hash_table_rehash_size(cl_object ht);
extern ECL_API cl_object cl_hash_table_rehash_threshold(cl_object ht);
extern ECL_API cl_object cl_hash_table_size(cl_object ht);
extern ECL_API cl_object cl_hash_table_test(cl_object ht);
extern ECL_API cl_object si_hash_table_iterator(cl_object ht);
extern ECL_API cl_object cl_make_hash_table _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_gethash _ARGS((cl_narg narg, cl_object key, cl_object ht, ...));
extern ECL_API cl_object si_copy_hash_table(cl_object orig);
extern ECL_API cl_object si_hash_eql _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_hash_equal _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_hash_equalp _ARGS((cl_narg narg, ...));

extern ECL_API void ecl_sethash(cl_object key, cl_object hashtable, cl_object value);
extern ECL_API cl_object ecl_gethash(cl_object key, cl_object hash);
extern ECL_API cl_object ecl_gethash_safe(cl_object key, cl_object hash, cl_object def);
extern ECL_API bool ecl_remhash(cl_object key, cl_object hash);
extern ECL_API struct ecl_hashtable_entry *ecl_search_hash(cl_object key, cl_object hashtable);

/* instance.c */

#ifdef CLOS
extern ECL_API cl_object si_allocate_raw_instance(cl_object orig, cl_object clas, cl_object size);
extern ECL_API cl_object si_instance_class(cl_object x);
extern ECL_API cl_object si_instance_class_set(cl_object x, cl_object y);
extern ECL_API cl_object si_instance_ref(cl_object x, cl_object index);
extern ECL_API cl_object si_instance_ref_safe(cl_object x, cl_object index);
extern ECL_API cl_object si_instance_set(cl_object x, cl_object index, cl_object value);
extern ECL_API cl_object si_instancep(cl_object x);
extern ECL_API cl_object si_unbound(void);
extern ECL_API cl_object si_sl_boundp(cl_object x);
extern ECL_API cl_object si_sl_makunbound(cl_object x, cl_object index);
extern ECL_API cl_object si_instance_sig(cl_object x);
extern ECL_API cl_object si_instance_sig_set(cl_object x);

extern ECL_API cl_object ecl_allocate_instance(cl_object clas, cl_index size);
extern ECL_API cl_object ecl_instance_ref(cl_object x, cl_fixnum i);
extern ECL_API cl_object ecl_instance_set(cl_object x, cl_fixnum i, cl_object v);
extern ECL_API cl_object si_copy_instance(cl_object x);

extern ECL_API cl_object ecl_slot_value(cl_object x, const char *slot);
extern ECL_API cl_object ecl_slot_value_set(cl_object x, const char *slot, cl_object y);
#endif /* CLOS */


/* list.c */

extern ECL_API cl_object cl_car(cl_object x);
extern ECL_API cl_object cl_cdr(cl_object x);
extern ECL_API cl_object cl_caar(cl_object x);
extern ECL_API cl_object cl_cadr(cl_object x);
extern ECL_API cl_object cl_cdar(cl_object x);
extern ECL_API cl_object cl_cddr(cl_object x);
extern ECL_API cl_object cl_caaar(cl_object x);
extern ECL_API cl_object cl_caadr(cl_object x);
extern ECL_API cl_object cl_cadar(cl_object x);
extern ECL_API cl_object cl_caddr(cl_object x);
extern ECL_API cl_object cl_cdaar(cl_object x);
extern ECL_API cl_object cl_cdadr(cl_object x);
extern ECL_API cl_object cl_cddar(cl_object x);
extern ECL_API cl_object cl_cdddr(cl_object x);
extern ECL_API cl_object cl_caaaar(cl_object x);
extern ECL_API cl_object cl_caaadr(cl_object x);
extern ECL_API cl_object cl_caadar(cl_object x);
extern ECL_API cl_object cl_caaddr(cl_object x);
extern ECL_API cl_object cl_cadaar(cl_object x);
extern ECL_API cl_object cl_cadadr(cl_object x);
extern ECL_API cl_object cl_caddar(cl_object x);
extern ECL_API cl_object cl_cadddr(cl_object x);
extern ECL_API cl_object cl_cdaaar(cl_object x);
extern ECL_API cl_object cl_cdaadr(cl_object x);
extern ECL_API cl_object cl_cdadar(cl_object x);
extern ECL_API cl_object cl_cdaddr(cl_object x);
extern ECL_API cl_object cl_cddaar(cl_object x);
extern ECL_API cl_object cl_cddadr(cl_object x);
extern ECL_API cl_object cl_cdddar(cl_object x);
extern ECL_API cl_object cl_cddddr(cl_object x);
#define cl_rest cl_cdr
#define cl_first cl_car
#define cl_second cl_cadr
#define cl_third cl_caddr
#define cl_fourth cl_cadddr

extern ECL_API cl_object cl_fifth(cl_object x);
extern ECL_API cl_object cl_sixth(cl_object x);
extern ECL_API cl_object cl_seventh(cl_object x);
extern ECL_API cl_object cl_eighth(cl_object x);
extern ECL_API cl_object cl_ninth(cl_object x);
extern ECL_API cl_object cl_tenth(cl_object x);
extern ECL_API cl_object cl_endp(cl_object x);
extern ECL_API cl_object cl_list_length(cl_object x);
extern ECL_API cl_object cl_nth(cl_object n, cl_object x);
extern ECL_API cl_object cl_nthcdr(cl_object n, cl_object x);
extern ECL_API cl_object cl_copy_list(cl_object x);
extern ECL_API cl_object cl_copy_alist(cl_object x);
extern ECL_API cl_object cl_copy_tree(cl_object x);
extern ECL_API cl_object cl_revappend(cl_object x, cl_object y);
extern ECL_API cl_object cl_ldiff(cl_object x, cl_object y);
extern ECL_API cl_object cl_rplaca(cl_object x, cl_object v);
extern ECL_API cl_object cl_rplacd(cl_object x, cl_object v);
extern ECL_API cl_object cl_tailp(cl_object y, cl_object x);
extern ECL_API cl_object si_memq(cl_object x, cl_object l);
extern ECL_API cl_object cl_nreconc(cl_object x, cl_object y);
extern ECL_API cl_object cl_cons(cl_object x, cl_object y);
extern ECL_API cl_object cl_acons(cl_object x, cl_object y, cl_object z);
extern ECL_API cl_object cl_list _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_listX _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_append _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_tree_equal _ARGS((cl_narg narg, cl_object x, cl_object y, ...));
extern ECL_API cl_object cl_last _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_make_list _ARGS((cl_narg narg, cl_object size, ...));
extern ECL_API cl_object cl_nconc _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_butlast _ARGS((cl_narg narg, cl_object lis, ...));
extern ECL_API cl_object cl_nbutlast _ARGS((cl_narg narg, cl_object lis, ...));
extern ECL_API cl_object cl_subst _ARGS((cl_narg narg, cl_object new_obj, cl_object old_obj, cl_object tree, ...));
extern ECL_API cl_object cl_nsubst _ARGS((cl_narg narg, cl_object new_obj, cl_object old_obj, cl_object tree, ...));
extern ECL_API cl_object cl_sublis _ARGS((cl_narg narg, cl_object alist, cl_object tree, ...));
extern ECL_API cl_object cl_nsublis _ARGS((cl_narg narg, cl_object alist, cl_object tree, ...));
extern ECL_API cl_object cl_member _ARGS((cl_narg narg, cl_object item, cl_object list, ...));
extern ECL_API cl_object si_member1 (cl_object item, cl_object list, cl_object test, cl_object test_not, cl_object key);
extern ECL_API cl_object cl_adjoin _ARGS((cl_narg narg, cl_object item, cl_object list, ...));
extern ECL_API cl_object cl_pairlis _ARGS((cl_narg narg, cl_object keys, cl_object data, ...));
extern ECL_API cl_object cl_rassoc _ARGS((cl_narg narg, cl_object item, cl_object alist, ...));
extern ECL_API cl_object cl_assoc _ARGS((cl_narg narg, cl_object item, cl_object alist, ...));

extern ECL_API cl_object ecl_last(cl_object x, cl_index n);
extern ECL_API cl_object ecl_butlast(cl_object x, cl_index n);
extern ECL_API cl_object ecl_nbutlast(cl_object x, cl_index n);
extern ECL_API cl_object ecl_list_length(cl_object x);
extern ECL_API cl_object ecl_append(cl_object x, cl_object y);
extern ECL_API bool ecl_endp(cl_object x);
extern ECL_API cl_object ecl_nth(cl_fixnum n, cl_object x);
extern ECL_API cl_object ecl_nthcdr(cl_fixnum n, cl_object x);
extern ECL_API cl_object ecl_nconc(cl_object x, cl_object y);
extern ECL_API bool ecl_member_eq(cl_object x, cl_object l);
extern ECL_API cl_object ecl_memql(cl_object x, cl_object l);
extern ECL_API cl_object ecl_member(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assq(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assql(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assoc(cl_object x, cl_object l);
extern ECL_API cl_object ecl_assqlp(cl_object x, cl_object l);
extern ECL_API cl_object ecl_remove_eq(cl_object x, cl_object l);


/* load.c */

extern ECL_API cl_object ecl_library_open(cl_object filename, bool force_reload);
extern ECL_API void *ecl_library_symbol(cl_object block, const char *symbol, bool lock);
extern ECL_API cl_object ecl_library_error(cl_object block);
extern ECL_API void ecl_library_close(cl_object block);
extern ECL_API void ecl_library_close_all(void);
extern ECL_API cl_object si_load_source(cl_object file, cl_object verbose, cl_object print);
extern ECL_API cl_object si_load_binary(cl_object file, cl_object verbose, cl_object print);
extern ECL_API cl_object cl_load _ARGS((cl_narg narg, cl_object pathname, ...));


/* macros.c */

extern ECL_API cl_object cl_macroexpand _ARGS((cl_narg narg, cl_object form, ...));
extern ECL_API cl_object cl_macroexpand_1 _ARGS((cl_narg narg, cl_object form, ...));
extern ECL_API cl_object cl_macro_function _ARGS((cl_narg narg, cl_object sym, ...));


/* main.c */

extern ECL_API cl_object si_argc(void);
extern ECL_API cl_object si_argv(cl_object index);
extern ECL_API cl_object si_getenv(cl_object var);
extern ECL_API cl_object si_setenv(cl_object var, cl_object value);
extern ECL_API cl_object si_pointer(cl_object x);
extern ECL_API cl_object si_quit _ARGS((cl_narg narg, ...)) /*__attribute__((noreturn))*/;

typedef enum {
	ECL_TRAP_SIGSEGV = 1,
	ECL_TRAP_SIGFPE = 2,
	ECL_TRAP_SIGINT = 4,
	ECL_TRAP_SIGILL = 8,
	ECL_TRAP_SIGBUS = 16,
	ECL_INCREMENTAL_GC = 128
} ecl_option;
extern ECL_API bool ecl_booted;
extern ECL_API const char *ecl_self;
extern ECL_API void ecl_set_option(int option, int value);
extern ECL_API int ecl_get_option(int option);
extern ECL_API int cl_boot(int argc, char **argv);
extern ECL_API int cl_shutdown(void);
#if defined(_MSC_VER) || defined(mingw32)
extern ECL_API void ecl_get_commandline_args(int* argc, char*** argv);
#endif


/* mapfun.c */

extern ECL_API cl_object cl_mapcar _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_maplist _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapc _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapl _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapcan _ARGS((cl_narg narg, cl_object fun, ...));
extern ECL_API cl_object cl_mapcon _ARGS((cl_narg narg, cl_object fun, ...));


/* multival.c */

extern ECL_API cl_object cl_values_list(cl_object list);
extern ECL_API cl_object cl_values _ARGS((cl_narg narg, ...));


/* num_arith.c */

extern ECL_API cl_object cl_conjugate(cl_object c);
extern ECL_API cl_object cl_1P(cl_object x);
extern ECL_API cl_object cl_1M(cl_object x);
extern ECL_API cl_object cl_X _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_P _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_M _ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_N _ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_gcd _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_lcm _ARGS((cl_narg narg, ...));

extern ECL_API cl_object fixnum_times(cl_fixnum i, cl_fixnum j);
extern ECL_API cl_object ecl_times(cl_object x, cl_object y);
extern ECL_API cl_object number_to_complex(cl_object x);
extern ECL_API cl_object ecl_plus(cl_object x, cl_object y);
extern ECL_API cl_object ecl_minus(cl_object x, cl_object y);
extern ECL_API cl_object ecl_negate(cl_object x);
extern ECL_API cl_object ecl_divide(cl_object x, cl_object y);
extern ECL_API cl_object ecl_integer_divide(cl_object x, cl_object y);
extern ECL_API cl_object ecl_gcd(cl_object x, cl_object y);
extern ECL_API cl_object ecl_one_plus(cl_object x);
extern ECL_API cl_object ecl_one_minus(cl_object x);


/* number.c */

extern ECL_API cl_fixnum fixint(cl_object x);
extern ECL_API cl_index  fixnnint(cl_object x);
extern ECL_API cl_fixnum ecl_fixnum_in_range(cl_object fun, const char *what, cl_object value,
				     cl_fixnum min, cl_fixnum max);
extern ECL_API cl_object ecl_make_integer(cl_fixnum i);
extern ECL_API cl_object ecl_make_unsigned_integer(cl_index i);
extern ECL_API cl_object ecl_make_ratio(cl_object num, cl_object den);
extern ECL_API cl_object ecl_make_singlefloat(float f);
extern ECL_API cl_object ecl_make_doublefloat(double f);
extern ECL_API cl_object ecl_make_complex(cl_object r, cl_object i);
extern ECL_API cl_object cl_rational(cl_object x);
#define cl_rationalize cl_rational
extern ECL_API double ecl_to_double(cl_object x);
#define number_to_float(x) ((float)ecl_to_double(x))
#ifdef ECL_SHORT_FLOAT
extern ECL_API cl_object make_shortfloat(float f);
extern ECL_API float ecl_short_float(cl_object o);
#endif
#ifdef ECL_LONG_FLOAT
extern ECL_API long double ecl_to_long_double(cl_object x);
extern ECL_API cl_object make_longfloat(long double f);
#endif
extern ECL_API cl_object double_to_integer(double d);
extern ECL_API cl_object float_to_integer(float d);
#ifdef ECL_LONG_FLOAT
extern ECL_API cl_object long_double_to_integer(long double d);
#endif

/* num_co.c */

extern ECL_API cl_object cl_numerator(cl_object x);
extern ECL_API cl_object cl_denominator(cl_object x);
extern ECL_API cl_object cl_mod(cl_object x, cl_object y);
extern ECL_API cl_object cl_rem(cl_object x, cl_object y);
extern ECL_API cl_object cl_decode_float(cl_object x);
extern ECL_API cl_object cl_scale_float(cl_object x, cl_object y);
extern ECL_API cl_object cl_float_radix(cl_object x);
extern ECL_API cl_object cl_float_digits(cl_object x);
extern ECL_API cl_object cl_float_precision(cl_object x);
extern ECL_API cl_object cl_integer_decode_float(cl_object x);
extern ECL_API cl_object cl_realpart(cl_object x);
extern ECL_API cl_object cl_imagpart(cl_object x);
extern ECL_API cl_object cl_float _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_floor _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_ceiling _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_truncate _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_round _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_float_sign _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_complex _ARGS((cl_narg narg, cl_object r, ...));

extern ECL_API cl_object ecl_floor1(cl_object x);
extern ECL_API cl_object ecl_ceiling1(cl_object x);
extern ECL_API cl_object ecl_truncate1(cl_object x);
extern ECL_API cl_object ecl_round1(cl_object x);
extern ECL_API cl_object ecl_floor2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_ceiling2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_truncate2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_round2(cl_object x, cl_object y);


/* num_comp.c */

extern ECL_API cl_object cl_E _ARGS((cl_narg narg, cl_object num, ...));
extern ECL_API cl_object cl_NE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_L _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_G _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_GE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_LE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_max _ARGS((cl_narg narg, cl_object max, ...));
extern ECL_API cl_object cl_min _ARGS((cl_narg narg, cl_object min, ...));

extern ECL_API int ecl_number_equalp(cl_object x, cl_object y);
extern ECL_API int ecl_number_compare(cl_object x, cl_object y);


/* num_log.c */

#define ECL_BOOLCLR		0
#define ECL_BOOLAND		01
#define ECL_BOOLANDC2		02
#define ECL_BOOL1		03
#define ECL_BOOLANDC1		04
#define ECL_BOOL2		05
#define ECL_BOOLXOR		06
#define ECL_BOOLIOR		07
#define ECL_BOOLNOR		010
#define ECL_BOOLEQV		011
#define ECL_BOOLC2		012
#define ECL_BOOLORC2		013
#define ECL_BOOLC1		014
#define ECL_BOOLORC1		015
#define ECL_BOOLNAND		016
#define ECL_BOOLSET		017

extern ECL_API cl_object cl_lognand(cl_object x, cl_object y);
extern ECL_API cl_object cl_lognor(cl_object x, cl_object y);
extern ECL_API cl_object cl_logandc1(cl_object x, cl_object y);
extern ECL_API cl_object cl_logandc2(cl_object x, cl_object y);
extern ECL_API cl_object cl_logorc1(cl_object x, cl_object y);
extern ECL_API cl_object cl_logorc2(cl_object x, cl_object y);
extern ECL_API cl_object cl_lognot(cl_object x);
extern ECL_API cl_object cl_boole(cl_object o, cl_object x, cl_object y);
extern ECL_API cl_object cl_logbitp(cl_object p, cl_object x);
extern ECL_API cl_object cl_ash(cl_object x, cl_object y);
extern ECL_API cl_object cl_logcount(cl_object x);
extern ECL_API cl_object cl_integer_length(cl_object x);
extern ECL_API cl_object si_bit_array_op(cl_object o, cl_object x, cl_object y, cl_object r);
extern ECL_API cl_object cl_logior _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logxor _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logand _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_logeqv _ARGS((cl_narg narg, ...));

extern ECL_API cl_object ecl_boole(int op, cl_object x, cl_object y);
extern ECL_API cl_object ecl_ash(cl_object x, cl_fixnum w);
extern ECL_API int ecl_fixnum_bit_length(cl_fixnum l);


/* num_pred.c */

extern ECL_API cl_object cl_zerop(cl_object x);
extern ECL_API cl_object cl_plusp(cl_object x);
extern ECL_API cl_object cl_minusp(cl_object x);
extern ECL_API cl_object cl_oddp(cl_object x);
extern ECL_API cl_object cl_evenp(cl_object x);

extern ECL_API int ecl_zerop(cl_object x);
extern ECL_API int ecl_plusp(cl_object x);
extern ECL_API int ecl_minusp(cl_object x);
extern ECL_API int ecl_oddp(cl_object x);
extern ECL_API int ecl_evenp(cl_object x);


/* num_rand.c */

extern ECL_API cl_object cl_random_state_p(cl_object x);
extern ECL_API cl_object cl_random _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_make_random_state _ARGS((cl_narg narg, ...));
extern ECL_API cl_object ecl_make_random_state(cl_object rs);


/* num_sfun.c */

extern ECL_API cl_fixnum ecl_fixnum_expt(cl_fixnum x, cl_fixnum y);
extern ECL_API cl_object cl_abs(cl_object x);
extern ECL_API cl_object cl_exp(cl_object x);
extern ECL_API cl_object cl_expt(cl_object x, cl_object y);
extern ECL_API cl_object cl_sqrt(cl_object x);
extern ECL_API cl_object cl_sin(cl_object x);
extern ECL_API cl_object cl_cos(cl_object x);
extern ECL_API cl_object cl_tan(cl_object x);
extern ECL_API cl_object cl_sinh(cl_object x);
extern ECL_API cl_object cl_cosh(cl_object x);
extern ECL_API cl_object cl_tanh(cl_object x);
extern ECL_API cl_object cl_atan _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_log _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object si_log1p(cl_object x);

extern ECL_API cl_object ecl_log1p(cl_object x);
extern ECL_API cl_object ecl_log1(cl_object x);
extern ECL_API cl_object ecl_log2(cl_object x, cl_object y);
extern ECL_API cl_object ecl_atan2(cl_object y, cl_object x);
extern ECL_API cl_object ecl_atan1(cl_object y);


/* package.c */

extern ECL_API void CEpackage_error(const char *message, const char *continue_message, cl_object package, int narg, ...);
extern ECL_API cl_object si_select_package(cl_object pack_name);
extern ECL_API cl_object cl_find_package(cl_object p);
extern ECL_API cl_object cl_package_name(cl_object p);
extern ECL_API cl_object cl_package_nicknames(cl_object p);
extern ECL_API cl_object cl_package_use_list(cl_object p);
extern ECL_API cl_object cl_package_used_by_list(cl_object p);
extern ECL_API cl_object cl_package_shadowing_symbols(cl_object p);
extern ECL_API cl_object cl_list_all_packages(void);
extern ECL_API cl_object si_package_hash_tables(cl_object p);
extern ECL_API cl_object si_package_lock(cl_object p, cl_object t);
extern ECL_API cl_object cl_delete_package(cl_object p);
extern ECL_API cl_object cl_make_package _ARGS((cl_narg narg, cl_object pack_name, ...));
extern ECL_API cl_object cl_intern _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_find_symbol _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_unintern _ARGS((cl_narg narg, cl_object symbl, ...));
extern ECL_API cl_object cl_export _ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_unexport _ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_import _ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_rename_package _ARGS((cl_narg narg, cl_object pack, cl_object new_name, ...));
extern ECL_API cl_object cl_shadowing_import _ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_shadow _ARGS((cl_narg narg, cl_object symbols, ...));
extern ECL_API cl_object cl_use_package _ARGS((cl_narg narg, cl_object pack, ...));
extern ECL_API cl_object cl_unuse_package _ARGS((cl_narg narg, cl_object pack, ...));

extern ECL_API cl_object ecl_make_package(cl_object n, cl_object ns, cl_object ul);
extern ECL_API cl_object ecl_rename_package(cl_object x, cl_object n, cl_object ns);
extern ECL_API cl_object ecl_find_package_nolock(cl_object n);
extern ECL_API cl_object si_coerce_to_package(cl_object p);
extern ECL_API cl_object ecl_current_package(void);
extern ECL_API cl_object ecl_find_symbol(cl_object n, cl_object p, int *intern_flag);
extern ECL_API cl_object ecl_intern(cl_object name, cl_object p, int *intern_flag);
extern ECL_API cl_object _ecl_intern(const char *s, cl_object p);
extern ECL_API bool ecl_unintern(cl_object s, cl_object p);
extern ECL_API void cl_export2(cl_object s, cl_object p);
extern ECL_API void cl_unexport2(cl_object s, cl_object p);
extern ECL_API void cl_import2(cl_object s, cl_object p);
extern ECL_API void ecl_shadowing_import(cl_object s, cl_object p);
extern ECL_API void ecl_shadow(cl_object s, cl_object p);
extern ECL_API void ecl_use_package(cl_object x0, cl_object p);
extern ECL_API void ecl_unuse_package(cl_object x0, cl_object p);


/* pathname.c */

extern ECL_API cl_object cl_pathname(cl_object name);
extern ECL_API cl_object cl_logical_pathname(cl_object pname);
extern ECL_API cl_object cl_pathnamep(cl_object pname);
extern ECL_API cl_object cl_pathname_host _ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_device _ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_directory _ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_name _ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_type _ARGS((cl_narg narg, cl_object pname, ...));
extern ECL_API cl_object cl_pathname_version(cl_object pname);
extern ECL_API cl_object cl_namestring(cl_object pname);
extern ECL_API cl_object cl_file_namestring(cl_object pname);
extern ECL_API cl_object cl_directory_namestring(cl_object pname);
extern ECL_API cl_object cl_host_namestring(cl_object pname);
extern ECL_API cl_object si_logical_pathname_p(cl_object pname);
extern ECL_API cl_object cl_pathname_match_p(cl_object path, cl_object mask);
extern ECL_API cl_object cl_translate_pathname _ARGS((cl_narg narg, cl_object source, cl_object from, cl_object to, ...));
extern ECL_API cl_object cl_translate_logical_pathname _ARGS((cl_narg narg, cl_object source, ...));
extern ECL_API cl_object cl_parse_namestring _ARGS((cl_narg narg, cl_object thing, ...));
extern ECL_API cl_object cl_parse_logical_namestring _ARGS((cl_narg narg, cl_object thing, ...));
extern ECL_API cl_object cl_merge_pathnames _ARGS((cl_narg narg, cl_object path, ...));
extern ECL_API cl_object cl_make_pathname _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_enough_namestring _ARGS((cl_narg narg, cl_object path, ...));
extern ECL_API cl_object si_pathname_translations _ARGS((cl_narg narg, cl_object host, ...));
extern ECL_API cl_object si_default_pathname_defaults(void);
extern ECL_API cl_object cl_wild_pathname_p _ARGS((cl_narg narg, cl_object pathname, ...));

extern ECL_API cl_object ecl_make_pathname(cl_object host, cl_object device, cl_object directory, cl_object name, cl_object type, cl_object version);
extern ECL_API cl_object ecl_parse_namestring(cl_object s, cl_index start, cl_index end, cl_index *ep, cl_object default_host);
extern ECL_API cl_object coerce_to_physical_pathname(cl_object x);
extern ECL_API cl_object coerce_to_file_pathname(cl_object pathname);
extern ECL_API cl_object ecl_namestring(cl_object pname, int truncate_if_impossible);
extern ECL_API cl_object si_coerce_to_filename(cl_object pathname);
extern ECL_API cl_object ecl_merge_pathnames(cl_object path, cl_object defaults, cl_object default_version);
extern ECL_API bool ecl_logical_hostname_p(cl_object host);


/* predicate.c */

extern ECL_API cl_object cl_identity(cl_object x);
extern ECL_API cl_object cl_null(cl_object x);
#define cl_not cl_null
extern ECL_API cl_object cl_symbolp(cl_object x);
extern ECL_API cl_object cl_atom(cl_object x);
extern ECL_API cl_object cl_consp(cl_object x);
extern ECL_API cl_object cl_listp(cl_object x);
extern ECL_API cl_object cl_numberp(cl_object x);
extern ECL_API cl_object cl_integerp(cl_object x);
extern ECL_API cl_object cl_rationalp(cl_object x);
extern ECL_API cl_object cl_floatp(cl_object x);
extern ECL_API cl_object cl_realp(cl_object x);
extern ECL_API cl_object cl_complexp(cl_object x);
extern ECL_API cl_object cl_characterp(cl_object x);
extern ECL_API cl_object cl_stringp(cl_object x);
extern ECL_API cl_object cl_bit_vector_p(cl_object x);
extern ECL_API cl_object cl_vectorp(cl_object x);
extern ECL_API cl_object cl_simple_string_p(cl_object x);
extern ECL_API cl_object cl_simple_bit_vector_p(cl_object x);
extern ECL_API cl_object cl_simple_vector_p(cl_object x);
extern ECL_API cl_object cl_arrayp(cl_object x);
extern ECL_API cl_object cl_packagep(cl_object x);
extern ECL_API cl_object cl_functionp(cl_object x);
extern ECL_API cl_object cl_compiled_function_p(cl_object x);
extern ECL_API cl_object cl_eq(cl_object x, cl_object y);
extern ECL_API cl_object cl_eql(cl_object x, cl_object y);
extern ECL_API cl_object cl_equal(cl_object x, cl_object y);
extern ECL_API cl_object cl_equalp(cl_object x, cl_object y);
extern ECL_API cl_object si_fixnump(cl_object x);

extern ECL_API bool floatp(cl_object x);
extern ECL_API bool ecl_numberp(cl_object x);
extern ECL_API bool ecl_eql(cl_object x, cl_object y);
extern ECL_API bool ecl_equal(register cl_object x, cl_object y);
extern ECL_API bool ecl_equalp(cl_object x, cl_object y);
extern ECL_API bool ecl_stringp(cl_object x);

/* print.c */

extern ECL_API cl_object cl_write_byte(cl_object integer, cl_object binary_output_stream);
extern ECL_API cl_object cl_write_sequence _ARGS((cl_narg narg, cl_object seq, cl_object stream, ...));
extern ECL_API cl_object cl_write _ARGS((cl_narg narg, cl_object x, ...));
extern ECL_API cl_object cl_prin1 _ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_print _ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_pprint _ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_princ _ARGS((cl_narg narg, cl_object obj, ...));
extern ECL_API cl_object cl_write_char _ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_write_string _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_write_line _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_terpri _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_finish_output _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_fresh_line _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_force_output _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_clear_output _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_write_object(cl_object object, cl_object stream);
extern ECL_API cl_object si_write_ugly_object(cl_object object, cl_object stream);

extern ECL_API cl_object ecl_princ(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_prin1(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_print(cl_object obj, cl_object strm);
extern ECL_API cl_object ecl_terpri(cl_object strm);
extern ECL_API void ecl_write_string(cl_object strng, cl_object strm);
extern ECL_API void ecl_princ_str(const char *s, cl_object sym);
extern ECL_API void ecl_princ_char(int c, cl_object sym);


/* profile.c */
#ifdef PROFILE
extern ECL_API cl_object si_profile _ARGS((cl_narg narg, cl_object scale, cl_object start_address));
extern ECL_API cl_object si_clear_profile _ARGS((cl_narg narg));
extern ECL_API cl_object si_display_profile _ARGS((cl_narg narg));
extern ECL_API int total_ticks(unsigned short *aar, unsigned int dim);
extern ECL_API int init_profile(void);
#endif


/* read.c */

extern ECL_API cl_object si_get_buffer_string();
extern ECL_API cl_object si_put_buffer_string(cl_object string);
extern ECL_API cl_object cl_read_sequence _ARGS((cl_narg narg, cl_object seq, cl_object stream, ...));
extern ECL_API cl_object cl_readtablep(cl_object readtable);
extern ECL_API cl_object si_string_to_object(cl_object str);
extern ECL_API cl_object si_standard_readtable(void);
extern ECL_API cl_object cl_read _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_preserving_whitespace _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_delimited_list _ARGS((cl_narg narg, cl_object d, ...));
extern ECL_API cl_object cl_read_line _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_char _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_unread_char _ARGS((cl_narg narg, cl_object c, ...));
extern ECL_API cl_object cl_peek_char _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_listen _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_read_char_no_hang _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_clear_input _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_parse_integer _ARGS((cl_narg narg, cl_object strng, ...));
extern ECL_API cl_object cl_read_byte _ARGS((cl_narg narg, cl_object binary_input_stream, ...));
extern ECL_API cl_object cl_copy_readtable _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_readtable_case(cl_object r);
extern ECL_API cl_object si_readtable_case_set(cl_object r, cl_object mode);
extern ECL_API cl_object cl_set_syntax_from_char _ARGS((cl_narg narg, cl_object tochr, cl_object fromchr, ...));
extern ECL_API cl_object cl_set_macro_character _ARGS((cl_narg narg, cl_object chr, cl_object fnc, ...));
extern ECL_API cl_object cl_get_macro_character _ARGS((cl_narg narg, cl_object chr, ...));
extern ECL_API cl_object cl_make_dispatch_macro_character _ARGS((cl_narg narg, cl_object chr, ...));
extern ECL_API cl_object cl_set_dispatch_macro_character _ARGS((cl_narg narg, cl_object dspchr, cl_object subchr, cl_object fnc, ...));
extern ECL_API cl_object cl_get_dispatch_macro_character _ARGS((cl_narg narg, cl_object dspchr, cl_object subchr, ...));

extern ECL_API cl_object ecl_read_object_non_recursive(cl_object in);
extern ECL_API cl_object ecl_read_object(cl_object in);
extern ECL_API cl_object ecl_parse_number(cl_object s, cl_index start, cl_index end, cl_index *ep, unsigned int radix);
extern ECL_API cl_object ecl_parse_integer(cl_object s, cl_index start, cl_index end, cl_index *ep, unsigned int radix);
extern ECL_API bool ecl_invalid_character_p(int c);
extern ECL_API cl_object ecl_copy_readtable(cl_object from, cl_object to);
extern ECL_API cl_object ecl_current_readtable(void);
extern ECL_API int ecl_current_read_base(void);
extern ECL_API char ecl_current_read_default_float_format(void);
extern ECL_API cl_object c_string_to_object(const char *s);
extern ECL_API cl_object read_VV(cl_object block, void (*entry)(cl_object));

/* reference.c */

extern ECL_API cl_object cl_fboundp(cl_object sym);
extern ECL_API cl_object cl_symbol_function(cl_object sym);
extern ECL_API cl_object cl_fdefinition(cl_object fname);
extern ECL_API cl_object si_coerce_to_function(cl_object form);
extern ECL_API cl_object cl_symbol_value(cl_object sym);
extern ECL_API cl_object cl_boundp(cl_object sym);
extern ECL_API cl_object cl_special_operator_p(cl_object form);
extern ECL_API cl_object ecl_fdefinition(cl_object fname);

/* sequence.c */

extern ECL_API cl_object cl_elt(cl_object x, cl_object i);
extern ECL_API cl_object si_elt_set(cl_object seq, cl_object index, cl_object val);
extern ECL_API cl_object cl_copy_seq(cl_object x);
extern ECL_API cl_object cl_length(cl_object x);
extern ECL_API cl_object cl_reverse(cl_object x);
extern ECL_API cl_object cl_nreverse(cl_object x);
extern ECL_API cl_object cl_subseq _ARGS((cl_narg narg, cl_object sequence, cl_object start, ...));

extern ECL_API cl_object ecl_alloc_simple_vector(cl_index l, cl_elttype aet);
extern ECL_API cl_object ecl_elt(cl_object seq, cl_fixnum index);
extern ECL_API cl_object ecl_elt_set(cl_object seq, cl_fixnum index, cl_object val);
extern ECL_API cl_fixnum ecl_length(cl_object x);


/* stacks.c */

extern ECL_API cl_object si_ihs_top(cl_object arg);
extern ECL_API cl_object si_ihs_fun(cl_object arg);
extern ECL_API cl_object si_ihs_env(cl_object arg);
extern ECL_API cl_object si_ihs_next(cl_object arg);
extern ECL_API cl_object si_ihs_prev(cl_object arg);
extern ECL_API cl_object si_frs_top(void);
extern ECL_API cl_object si_frs_bds(cl_object arg);
extern ECL_API cl_object si_frs_tag(cl_object arg);
extern ECL_API cl_object si_frs_ihs(cl_object arg);
extern ECL_API cl_object si_bds_top(void);
extern ECL_API cl_object si_bds_var(cl_object arg);
extern ECL_API cl_object si_bds_val(cl_object arg);
extern ECL_API cl_object si_sch_frs_base(cl_object fr, cl_object ihs);
extern ECL_API cl_object si_reset_stack_limits(void);
extern ECL_API cl_object si_set_stack_size(cl_object type, cl_object size);

extern ECL_API void bds_overflow(void) /*__attribute__((noreturn))*/;
extern ECL_API void bds_unwind(cl_index new_bds_top_index);
extern ECL_API void ecl_unwind(ecl_frame_ptr fr) /*__attribute__((noreturn))*/;
extern ECL_API ecl_frame_ptr frs_sch(cl_object frame_id);
extern ECL_API ecl_frame_ptr frs_sch_catch(cl_object frame_id);
extern ECL_API cl_object new_frame_id(void);

/* string.c */

extern ECL_API cl_object cl_char(cl_object s, cl_object i);
#define cl_schar(x,y) cl_char(x,y)
extern ECL_API cl_object si_char_set(cl_object str, cl_object index, cl_object c);
extern ECL_API cl_object cl_string_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string_left_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string_right_trim(cl_object char_bag, cl_object strng);
extern ECL_API cl_object cl_string(cl_object x);
extern ECL_API cl_object cl_make_string _ARGS((cl_narg narg, cl_object size, ...));
extern ECL_API cl_object cl_stringE _ARGS((cl_narg narg, cl_object string1, cl_object string2, ...));
extern ECL_API cl_object cl_string_equal _ARGS((cl_narg narg, cl_object string1, cl_object string2, ...));
extern ECL_API cl_object cl_stringL _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringG _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringLE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringGE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_stringNE _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_lessp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_greaterp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_greaterp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_lessp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_not_equal _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_upcase _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_downcase _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_string_capitalize _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_upcase _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_downcase _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_nstring_capitalize _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_base_string_concatenate _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_copy_to_simple_base_string(cl_object s);

extern ECL_API cl_object cl_alloc_simple_base_string(cl_index l);
extern ECL_API cl_object cl_alloc_adjustable_base_string(cl_index l);
extern ECL_API cl_object make_simple_base_string(char *s);
#define make_constant_base_string(s) (make_simple_base_string((char *)s))
extern ECL_API cl_object make_base_string_copy(const char *s);
extern ECL_API cl_object ecl_cstring_to_base_string_or_nil(const char *s);
extern ECL_API bool ecl_string_eq(cl_object x, cl_object y);
extern ECL_API bool ecl_member_char(int c, cl_object char_bag);
extern ECL_API int ecl_string_push_extend(cl_object s, int c);
extern ECL_API void get_string_start_end(cl_object s, cl_object start, cl_object end, cl_index *ps, cl_index *pe);
extern ECL_API bool ecl_fits_in_base_string(cl_object s);
extern ECL_API cl_index ecl_char(cl_object s, cl_index i);
extern ECL_API void ecl_char_set(cl_object s, cl_index i, cl_index c);

/* structure.c */

extern ECL_API cl_object si_structure_subtype_p(cl_object x, cl_object y);
extern ECL_API cl_object cl_copy_structure(cl_object s);
extern ECL_API cl_object si_structure_name(cl_object s);
extern ECL_API cl_object si_structure_ref(cl_object x, cl_object type, cl_object index);
extern ECL_API cl_object si_structure_set(cl_object x, cl_object type, cl_object index, cl_object val);
extern ECL_API cl_object si_structurep(cl_object s);
extern ECL_API cl_object si_make_structure _ARGS((cl_narg narg, cl_object type, ...));

#ifndef CLOS
extern ECL_API cl_object structure_to_list(cl_object x);
#endif
extern ECL_API cl_object ecl_structure_ref(cl_object x, cl_object name, int n);
extern ECL_API cl_object ecl_structure_set(cl_object x, cl_object name, int n, cl_object v);


/* symbol.c */

extern ECL_API cl_object cl_make_symbol(cl_object str);
extern ECL_API cl_object cl_remprop(cl_object sym, cl_object prop);
extern ECL_API cl_object cl_symbol_plist(cl_object sym);
extern ECL_API cl_object cl_get_properties(cl_object place, cl_object indicator_list);
extern ECL_API cl_object cl_symbol_name(cl_object sym);
extern ECL_API cl_object cl_symbol_package(cl_object sym);
extern ECL_API cl_object cl_keywordp(cl_object sym);
extern ECL_API cl_object si_put_f(cl_object plist, cl_object value, cl_object indicator);
extern ECL_API cl_object si_rem_f(cl_object plist, cl_object indicator);
extern ECL_API cl_object si_set_symbol_plist(cl_object sym, cl_object plist);
extern ECL_API cl_object si_putprop(cl_object sym, cl_object value, cl_object indicator);
extern ECL_API cl_object si_Xmake_special(cl_object sym);
extern ECL_API cl_object si_Xmake_constant(cl_object sym, cl_object val);
extern ECL_API cl_object cl_get _ARGS((cl_narg narg, cl_object sym, cl_object indicator, ...));
extern ECL_API cl_object cl_getf _ARGS((cl_narg narg, cl_object place, cl_object indicator, ...));
extern ECL_API cl_object cl_copy_symbol _ARGS((cl_narg narg, cl_object sym, ...));
extern ECL_API cl_object cl_gensym _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_gentemp _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_put_properties _ARGS((cl_narg narg, cl_object sym, ...));

extern ECL_API void cl_defvar(cl_object s, cl_object v);
extern ECL_API void cl_defparameter(cl_object s, cl_object v);
extern ECL_API cl_object ecl_make_keyword(const char *s);
extern ECL_API cl_object ecl_symbol_value(cl_object s);
extern ECL_API cl_object ecl_symbol_name(cl_object s);
extern ECL_API cl_object ecl_symbol_package(cl_object s);
extern ECL_API int ecl_symbol_type(cl_object s);
extern ECL_API void ecl_symbol_type_set(cl_object s, int t);
extern ECL_API cl_object ecl_getf(cl_object place, cl_object indicator, cl_object deflt);
extern ECL_API cl_object ecl_get(cl_object s, cl_object p, cl_object d);
extern ECL_API bool ecl_keywordp(cl_object s);


/* tcp.c */

#ifdef TCP
extern ECL_API cl_object si_open_client_stream(cl_object host, cl_object port);
extern ECL_API cl_object si_open_server_stream(cl_object port);
extern ECL_API cl_object si_open_unix_socket_stream(cl_object path);
extern ECL_API cl_object si_lookup_host_entry(cl_object host_or_address);
extern ECL_API void ecl_tcp_close_all(void);
#endif


/* threads.c */

#ifdef ECL_THREADS
extern ECL_API cl_object mp_own_process(void) __attribute__((const));
extern ECL_API cl_object mp_all_processes(void);
extern ECL_API cl_object mp_exit_process(void) /*__attribute__((noreturn))*/;
extern ECL_API cl_object mp_interrupt_process(cl_object process, cl_object function);
extern ECL_API cl_object mp_make_process _ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_process_active_p(cl_object process);
extern ECL_API cl_object mp_process_enable(cl_object process);
extern ECL_API cl_object mp_process_yield(void);
extern ECL_API cl_object mp_process_interrupt(cl_object process, cl_object function);
extern ECL_API cl_object mp_process_kill(cl_object process);
extern ECL_API cl_object mp_process_name(cl_object process);
extern ECL_API cl_object mp_process_preset _ARGS((cl_narg narg, cl_object process, cl_object function, ...));
extern ECL_API cl_object mp_process_run_function _ARGS((cl_narg narg, cl_object name, cl_object function, ...));
extern ECL_API cl_object mp_process_whostate(cl_object process);
extern ECL_API cl_object mp_make_lock _ARGS((cl_narg narg, ...));
extern ECL_API cl_object mp_recursive_lock_p(cl_object lock);
extern ECL_API cl_object mp_lock_name(cl_object lock);
extern ECL_API cl_object mp_lock_holder(cl_object lock);
extern ECL_API cl_object mp_get_lock _ARGS((cl_narg narg, cl_object lock, ...));
extern ECL_API cl_object mp_giveup_lock(cl_object lock);
extern ECL_API cl_object mp_make_condition_variable(void);
extern ECL_API cl_object mp_condition_variable_wait(cl_object cv, cl_object lock);
extern ECL_API cl_object mp_condition_variable_timedwait(cl_object cv, cl_object lock, cl_object seconds);
extern ECL_API cl_object mp_condition_variable_signal(cl_object cv);
extern ECL_API cl_object mp_condition_variable_broadcast(cl_object cv);

extern ECL_API void ecl_import_current_thread(cl_object process_name, cl_object process_binding);
extern ECL_API void ecl_release_current_thread(void);
#endif


/* time.c */

extern ECL_API cl_object cl_sleep(cl_object z);
extern ECL_API cl_object cl_get_internal_run_time(void);
extern ECL_API cl_object cl_get_internal_real_time(void);
extern ECL_API cl_object cl_get_universal_time(void);


/* typespec.c */

extern ECL_API void assert_type_integer(cl_object p);
extern ECL_API void assert_type_non_negative_integer(cl_object p);
extern ECL_API void assert_type_package(cl_object p);
extern ECL_API void assert_type_string(cl_object p);
extern ECL_API void assert_type_cons(cl_object p);
extern ECL_API void assert_type_readtable(cl_object p);
extern ECL_API void assert_type_hash_table(cl_object p);
extern ECL_API void assert_type_array(cl_object p);
extern ECL_API void assert_type_vector(cl_object p);
extern ECL_API void assert_type_list(cl_object p);
extern ECL_API void assert_type_proper_list(cl_object p);
extern ECL_API cl_object cl_type_of(cl_object x);

extern ECL_API void FEtype_error_character(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_cons(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_number(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_real(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_float(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_integer(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_list(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_proper_list(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_alist(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_stream(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_sequence(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_instance(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEcircular_list(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_index(cl_object seq, cl_object ndx) /*__attribute__((noreturn))*/;
extern ECL_API void FEtype_error_string(cl_object x) /*__attribute__((noreturn))*/;
extern ECL_API void FEdivision_by_zero(cl_object x, cl_object y) /*__attribute__((noreturn))*/;
extern ECL_API cl_object ecl_type_error(cl_object function, const char *place, cl_object o, cl_object type);
extern ECL_API cl_object ecl_check_cl_type(cl_object fun, cl_object p, cl_type t);
extern ECL_API cl_object ecl_check_type_string(cl_object fun, cl_object p);


/* unixfsys.c */

extern ECL_API cl_object cl_truename(cl_object file);
extern ECL_API cl_object cl_rename_file _ARGS((cl_narg narg, cl_object old_obj, cl_object new_obj, ...));
extern ECL_API cl_object cl_delete_file(cl_object file);
extern ECL_API cl_object cl_probe_file(cl_object file);
extern ECL_API cl_object cl_file_write_date(cl_object file);
extern ECL_API cl_object cl_file_author(cl_object file);
extern ECL_API cl_object si_file_kind(cl_object pathname, cl_object follow_links);
extern ECL_API cl_object si_getcwd(void);
extern ECL_API cl_object si_getpid(void);
extern ECL_API cl_object si_chdir _ARGS((cl_narg narg, cl_object directory, ...));
extern ECL_API cl_object si_mkdir(cl_object directory, cl_object mode);
extern ECL_API cl_object cl_directory _ARGS((cl_narg narg, cl_object directory, ...));
extern ECL_API cl_object cl_user_homedir_pathname _ARGS((cl_narg narg, ...));
extern ECL_API cl_object si_mkstemp(cl_object templ);
extern ECL_API cl_object si_rmdir(cl_object directory);

extern ECL_API const char *ecl_expand_pathname(const char *name);
extern ECL_API cl_object ecl_cstring_to_pathname(char *s);
extern ECL_API void *ecl_backup_fopen(const char *filename, const char *option);
extern ECL_API cl_object ecl_file_len(void *fp);
extern ECL_API cl_object ecl_homedir_pathname(cl_object user);
#if defined(_MSC_VER) || defined(mingw32)
extern ECL_API cl_object si_get_library_pathname(void);
#endif



/* unixint.c */

extern ECL_API cl_object si_catch_signal(cl_object signal, cl_object state);
extern ECL_API cl_object si_check_pending_interrupts(void);
extern ECL_API cl_object si_trap_fpe(cl_object condition, cl_object flag);


/* unixsys.c */

extern ECL_API cl_object si_system(cl_object cmd);
extern ECL_API cl_object si_make_pipe();
extern ECL_API cl_object si_run_program _ARGS((cl_narg narg, cl_object command, cl_object args, ...));


/* unicode -- no particular file, but we group these changes here */

#ifdef ECL_UNICODE
extern ECL_API cl_object si_base_char_p(cl_object x);
extern ECL_API cl_object si_base_string_p(cl_object x);
extern ECL_API cl_object si_coerce_to_base_string(cl_object x);
extern ECL_API cl_object si_coerce_to_extended_string(cl_object x);
extern ECL_API cl_object cl_alloc_simple_extended_string(cl_index l);
#else
#define si_base_char_p cl_characterp
#define si_base_string_p cl_stringp
#define si_coerce_to_base_string cl_string
#define si_coerce_to_extended_string cl_string
#endif


/**********************************************************************
 * FUNCTIONS GENERATED BY THE LISP COMPILER
 */

/* arraylib.lsp */
extern ECL_API cl_object cl_make_array _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_vector _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_array_dimensions _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_array_in_bounds_p _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_array_row_major_index _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_bit _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_sbit _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_bit_and _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_ior _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_xor _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_eqv _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_nand _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_nor _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_andc1 _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_andc2 _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_orc1 _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_orc2 _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_bit_not _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_vector_push _ARGS((cl_object V1, cl_object V2));
extern ECL_API cl_object cl_vector_push_extend _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_vector_pop _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_adjust_array _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

/* iolib.lsp */

extern ECL_API cl_object cl_read_from_string _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_write_to_string _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_prin1_to_string _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_princ_to_string _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_y_or_n_p _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_yes_or_no_p _ARGS((cl_narg narg, ...));

/* listlib.lsp */

extern ECL_API cl_object cl_union _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nunion _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_intersection _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nintersection _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_set_difference _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nset_difference _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_set_exclusive_or _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_nset_exclusive_or _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_subsetp _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_rassoc_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_rassoc_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_assoc_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_assoc_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_member_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_member_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_subst_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_subst_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubst_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubst_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));

/* mislib.lsp */

extern ECL_API cl_object cl_logical_pathname_translations _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_load_logical_pathname_translations _ARGS((cl_narg, cl_object V1, ...));
extern ECL_API cl_object cl_decode_universal_time _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_encode_universal_time _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5, cl_object V6, ...));
extern ECL_API cl_object cl_get_decoded_time _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_ensure_directories_exist _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object si_simple_program_error _ARGS((cl_narg narg, cl_object format, ...)) /*__attribute__((noreturn))*/;
extern ECL_API cl_object si_signal_simple_error _ARGS((cl_narg narg, cl_object condition, cl_object continuable, cl_object format, cl_object args, ...)) /*__attribute__((noreturn))*/;

/* module.lsp */

extern ECL_API cl_object cl_provide _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_require _ARGS((cl_narg narg, cl_object V1, ...));

/* numlib.lsp */

extern ECL_API cl_object cl_isqrt _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_phase _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_signum _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_cis _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_asin _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_acos _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_asinh _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_acosh _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_atanh _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_ffloor _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_fceiling _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_ftruncate _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_fround _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_logtest _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_byte _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_byte_size _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_byte_position _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_ldb _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_ldb_test _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_mask_field _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_dpb _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_deposit_field _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));

/* packlib.lsp */

extern ECL_API cl_object cl_find_all_symbols _ARGS((cl_narg, cl_object V1, ...));
extern ECL_API cl_object cl_apropos _ARGS((cl_narg arg, cl_object V1, ...));
extern ECL_API cl_object cl_apropos_list _ARGS((cl_narg arg, cl_object V1, ...));
extern ECL_API cl_object si_find_relative_package _ARGS((cl_narg narg, cl_object pack_name, ...));

/* predlib.lsp */

extern ECL_API cl_object si_subclassp _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object si_of_class_p _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object si_do_deftype _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_upgraded_array_element_type _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_upgraded_complex_part_type _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_typep _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_coerce _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_subtypep _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

/* seq.lsp */

extern ECL_API cl_object cl_make_sequence _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_concatenate _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_map _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_some _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_every _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_notany _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_notevery _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_map_into _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

/* seqlib.lsp */

extern ECL_API cl_object cl_reduce _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_fill _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_replace _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_delete_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_count_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_substitute _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_substitute_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_substitute_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_nsubstitute_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_find _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_find_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_find_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position_if _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_position_if_not _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_remove_duplicates _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_delete_duplicates _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_mismatch _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_search _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_sort _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_stable_sort _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_merge _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, ...));
extern ECL_API cl_object cl_constantly _ARGS((cl_narg narg, cl_object V1, ...));

/* pprint.lsp */

extern ECL_API cl_object cl_pprint_newline _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_pprint_indent _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_tab _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
extern ECL_API cl_object cl_pprint_fill _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_linear _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_pprint_tabular _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));
extern ECL_API cl_object cl_copy_pprint_dispatch _ARGS((cl_narg narg, ...));
extern ECL_API cl_object cl_pprint_dispatch _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_set_pprint_dispatch _ARGS((cl_narg narg, cl_object V1, cl_object V2, ...));

#ifdef CLOS

/* combin.lsp */
extern ECL_API cl_object cl_method_combination_error _ARGS((cl_narg narg, cl_object format, ...));
extern ECL_API cl_object cl_invalid_method_error _ARGS((cl_narg narg, cl_object method, cl_object format, ...));

#if 0
/* defclass.lsp */
extern ECL_API cl_object clos_ensure_class _ARGS((cl_narg narg, cl_object V1, ...));

/* kernel.lsp */
extern ECL_API cl_object clos_class_id _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_superclasses _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_subclasses _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_slots _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_precedence_list _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_direct_slots _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_slot_index_table _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_class_shared_slots _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_method_combination _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_lambda_list _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_argument_precedence_order _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_method_class _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_generic_function_methods _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_generic_function _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_lambda_list _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_specializers _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object cl_method_qualifiers _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_function _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_method_plist _ARGS((cl_narg narg, cl_object V1, ...));
extern ECL_API cl_object clos_install_method _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, cl_object V4, cl_object V5, cl_object V6, cl_object V7, ...));

/* standard.lsp */
extern ECL_API cl_object clos_standard_instance_set _ARGS((cl_narg narg, cl_object V1, cl_object V2, cl_object V3, ...));
#endif
#endif

#ifdef __cplusplus
}
#endif
