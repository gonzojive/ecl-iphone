#ifdef __cplusplus
extern "C" {
#endif

struct let {
        cl_object let_var;
        cl_object let_spp;
        cl_object let_init;
};

struct iterator {
        cl_object iter_var;
        cl_object iter_spp;
        cl_object iter_init;
        cl_object iter_incr;
};

/* alloc.c / alloc_2.c */

extern cl_object cl_alloc_object(cl_type t);
extern cl_object cl_alloc_instance(cl_index slots);
extern cl_object make_cons(cl_object a, cl_object d);
extern void cl_dealloc(void *p, cl_index s);
extern void *cl_alloc(cl_index n);
extern void *cl_alloc_align(cl_index size, cl_index align);
#ifdef GBC_BOEHM
extern void *cl_alloc_atomic(cl_index size);
extern void *cl_alloc_atomic_align(cl_index size, cl_index align);
extern void init_alloc_function(void);
#else
#define cl_alloc_atomic(x) cl_alloc(x)
#define cl_alloc_atomic_align(x,s) cl_alloc_align(x,s)
#endif /* GBC_BOEHM */
extern void init_alloc(void);

/* all_functions.c */

extern void init_all_functions(void);


/* all_keywords.c */
extern void init_all_keywords(void);


/* all_symbols */
extern void init_all_symbols(void);


/* apply.c */

extern cl_object APPLY(int n, cl_objectfn, cl_object *x);
extern cl_object APPLY_closure(int n, cl_objectfn, cl_object cl, cl_object *x);


/* array.c */

extern cl_index object_to_index(cl_object n);
extern cl_object aref(cl_object x, cl_index index);
extern cl_object aref1(cl_object v, cl_index index);
extern cl_object aset(cl_object x, cl_index index, cl_object value);
extern cl_object aset1(cl_object v, cl_index index, cl_object val);
extern void array_allocself(cl_object x);
extern void *array_address(cl_object x, cl_index inc);
extern void adjust_displaced(cl_object x, ptrdiff_t diff);
extern cl_elttype array_elttype(cl_object x);
extern cl_elttype get_elttype(cl_object x);
extern void init_array(void);


/* assignment.c */

extern cl_object set(cl_object sym, cl_object val);
extern cl_object setf_namep(cl_object fun_spec);
extern void clear_compiler_properties(cl_object sym);
extern void init_assignment(void);


/* backq.c */

#ifndef THREADS
extern int backq_level;
#endif
extern int backq_cdr(cl_object *px);
extern int backq_car(cl_object *px);
extern cl_object backq(cl_object x);
extern void init_backq(void);


/* big.c */

#ifdef THREADS
#define bignum_register_limbs lwp->lwp_bignum_register_limbs
#define bignum_register lwp->lwp_bignum_register
#else
extern cl_object bignum_register[3];
extern mp_limb_t bignum_register_limbs[3][BIGNUM_REGISTER_SIZE];
#endif /* THREADS */
extern cl_object big_register_copy(cl_object x);
extern cl_object big_register_normalize(cl_object x);
extern void big_register_free(cl_object x);
extern cl_object bignum1(int val);
extern cl_object bignum2(mp_limb_t hi, mp_limb_t lo);
extern cl_object big_set_fixnum(cl_object x, cl_object fix);
extern cl_object big_copy(cl_object x);
extern cl_object big_minus(cl_object x);
extern cl_object big_plus(cl_object x, cl_object y);
extern cl_object big_normalize(cl_object x);
extern double big_to_double(cl_object x);
extern long big_to_long(cl_object x);
extern void init_big(void);


/* block.c */

extern void init_block(void);


/* catch.c */

extern void init_catch(void);


/* cfun.c */

extern cl_object make_cfun(cl_objectfn self, cl_object name, cl_object block);
extern cl_object make_cclosure(cl_objectfn self, cl_object env, cl_object block);
extern void MF(cl_object sym, cl_objectfn self, cl_object block);
extern void MM(cl_object sym, cl_objectfn self, cl_object block);
extern cl_object make_function(const char *s, cl_objectfn f);
extern cl_object make_si_function(const char *s, cl_objectfn f);
extern void init_cfun(void);


/* character.c */

extern cl_fixnum char_code(cl_object c);
extern int digitp(int i, int r);
extern bool char_eq(cl_object x, cl_object y);
extern int char_cmp(cl_object x, cl_object y);
extern bool char_equal(cl_object x, cl_object y);
extern int char_compare(cl_object x, cl_object y);
extern cl_object coerce_to_character(cl_object x);
extern short digit_weight(int w, int r);
extern void init_character(void);
extern void init_character_function(void);


/* clos.c */

#ifdef CLOS
extern cl_object class_class;
extern cl_object class_object;
extern cl_object class_built_in;
extern void init_clos(void);
#endif

/* cmpaux.c */

extern cl_object make_list(int i);
extern int ifloor(int x, int y);
extern int imod(int x, int y);
extern char object_to_char(cl_object x);
extern cl_fixnum object_to_fixnum(cl_object x);
extern char *object_to_string(cl_object x);
extern float object_to_float(cl_object x);
extern double object_to_double(cl_object x);
extern int aref_bv(cl_object x, cl_index index);
extern int aset_bv(cl_object x, cl_index index, int value);
extern void cl_throw(cl_object tag) __attribute__((noreturn));
extern void cl_return_from(cl_object block_id, cl_object block_name) __attribute__((noreturn));
extern void cl_go(cl_object tag_id, cl_object label) __attribute__((noreturn));
extern void parse_key(int narg, cl_object *args, int nkey, cl_object *keys, cl_object *vars, cl_object *rest, bool allow_other_keys);
extern cl_object grab_rest_args(int narg, cl_object *args);
extern void check_other_key(cl_object l, int n, ...);
extern void init_cmpaux(void);


/* compiler.c */

extern cl_object make_lambda(cl_object name, cl_object lambda);
extern cl_object eval(cl_object form, cl_object *bytecodes, cl_object env);
extern void init_compiler(void);

/* interpreter.c */

extern cl_object lex_env;
extern cl_object lambda_apply(int narg, cl_object fun, cl_object *args);
extern cl_object *interpret(cl_object *memory);
extern void init_interpreter(void);

/* conditional.c */

extern void init_conditional(void);


/* earith.c */

extern void extended_mul(int d, int q, int r, int *hp, int *lp);
extern void extended_div(int d, int h, int l, int *qp, int *rp);


/* error.c */

extern cl_object null_string;
extern void internal_error(const char *s) __attribute__((noreturn));
extern void cs_overflow(void) __attribute__((noreturn));
extern void error(const char *s) __attribute__((noreturn));
extern void terminal_interrupt(bool correctable);
extern void FEcondition(int narg, cl_object name, ...) __attribute__((noreturn));
extern void FEprogram_error(const char *s, int narg, ...) __attribute__((noreturn));
extern void FEcontrol_error(const char *s, int narg, ...) __attribute__((noreturn));
extern void FEerror(char *s, int narg, ...) __attribute__((noreturn));
extern void FEcannot_open(cl_object fn) __attribute__((noreturn));
extern void FEwrong_type_argument(cl_object type, cl_object value) __attribute__((noreturn));
extern void FEtoo_few_arguments(int *nargp) __attribute__((noreturn));
extern void FEtoo_many_arguments(int *nargp) __attribute__((noreturn));
extern void FEunbound_variable(cl_object sym) __attribute__((noreturn));
extern void FEinvalid_macro_call(cl_object obj) __attribute__((noreturn));
extern void FEinvalid_variable(char *s, cl_object obj) __attribute__((noreturn));
extern void FEassignment_to_constant(cl_object v) __attribute__((noreturn));
extern void FEundefined_function(cl_object fname) __attribute__((noreturn));
extern void FEinvalid_function(cl_object obj) __attribute__((noreturn));
extern cl_object CEerror(char *err_str, int narg, ...);
extern void check_arg_failed(int narg, int req) __attribute__((noreturn));
extern void illegal_index(cl_object x, cl_object i);
extern void FEtype_error_symbol(cl_object obj);
extern void not_a_variable(cl_object obj);
extern void init_error(void);

extern void FEend_of_file(cl_object strm);

/* eval.c */

#define funcall clLfuncall

extern cl_object apply(int narg, cl_object fun, cl_object *args);
extern cl_object link_call(cl_object sym, cl_objectfn *pLK, cl_object *gfun,
			   int narg, va_list args);
extern void init_eval(void);

#ifdef NO_ARGS_ARRAY
extern cl_object va_APPLY(int narg, cl_objectfn fn, va_list args);
extern cl_object va_APPLY_closure(int narg, cl_objectfn fn, cl_object data, va_list args);
extern cl_object va_gcall(int narg, cl_object fun, va_list args);
extern cl_object va_lambda_apply(int narg, cl_object fun, va_list args);
extern void va_parse_key(int narg, va_list args, int nkey, cl_object *keys, cl_object *vars, cl_object *rest, bool allow_other_keys);
extern cl_object va_grab_rest_args(int narg, va_list args);
#else
#define va_APPLY(x,y,z) APPLY(x,y,&va_arg(z,cl_object))
#define va_APPLY_closure(x,y,p,z) APPLY_closure(x,y,p,&va_arg(z,cl_object))
#define va_gcall(x,y,z) gcall(x,y,&va_arg(z,cl_object))
#define va_lambda_apply(x,y,z) lambda_apply(x,y,&va_arg(z,cl_object))
#define va_parse_key(a,b,c,d,e,f,g) parse_key(a,&va_arg(b,cl_object),c,d,e,f,g)
#define va_grab_rest_args(a,b) grab_rest_args(a,&va_arg(b,cl_object))
#endif

/* file.c */

extern bool input_stream_p(cl_object strm);
extern bool output_stream_p(cl_object strm);
extern cl_object stream_element_type(cl_object strm);
extern void closed_stream(cl_object strm) __attribute__ ((noreturn));
extern cl_object open_stream(cl_object fn, enum smmode smm, cl_object if_exists, cl_object if_does_not_exist);
extern void close_stream(cl_object strm, bool abort_flag);
extern cl_object make_two_way_stream(cl_object istrm, cl_object ostrm);
extern cl_object make_echo_stream(cl_object istrm, cl_object ostrm);
extern cl_object make_string_input_stream(cl_object strng, cl_index istart, cl_index iend);
extern cl_object make_string_output_stream(cl_index line_length);
extern cl_object get_output_stream_string(cl_object strm);
extern int readc_stream(cl_object strm);
extern void unreadc_stream(int c, cl_object strm);
extern int writec_stream(int c, cl_object strm);
extern void writestr_stream(const char *s, cl_object strm);
extern void flush_stream(cl_object strm);
extern void clear_input_stream(cl_object strm);
extern void clear_output_stream(cl_object strm);
extern bool stream_at_end(cl_object strm);
extern bool listen_stream(cl_object strm);
extern long file_position(cl_object strm);
extern long file_position_set(cl_object strm, long disp);
extern long file_length(cl_object strm);
extern int file_column(cl_object strm);
extern void init_file(void);
extern void init_file_function(void);


/* format.c */

extern void init_format(void);


/* gbc.c */

#if !defined(GBC_BOEHM)
#define GC_enabled() GC_enable
#define GC_enable() GC_enable = TRUE;
#define GC_disable() GC_enable = FALSE;
extern bool GC_enable;
extern int gc_time;
extern cl_object (*GC_enter_hook)(void);
extern cl_object (*GC_exit_hook)(void);
extern void register_root(cl_object *p);
extern void gc(cl_type t);
extern void init_GC(void);
#endif


/* gbc_2.c */

#ifdef GBC_BOEHM
#define GC_enabled() (!GC_dont_gc)
#define GC_enable() GC_dont_gc = FALSE;
#define GC_disable() GC_dont_gc = TRUE;
extern int GC_dont_gc;
extern void register_root(cl_object *p);
extern void gc(cl_type t);
#endif /* GBC_BOEHM */


/* gfun.c */

#ifdef CLOS
extern cl_object gcall(int narg, cl_object fun, cl_object *args);
extern void init_gfun(void);
#endif /* CLOS */


/* hash.c */

extern cl_hashkey update_crc32(cl_hashkey crc, const char *buffer, cl_index len);
extern cl_hashkey hash_eq(cl_object x);
extern cl_hashkey hash_eql(cl_object x);
extern cl_hashkey hash_equal(cl_object x);
extern void sethash(cl_object key, cl_object hashtable, cl_object value);
extern void extend_hashtable(cl_object hashtable);
extern void init_hash(void);
extern cl_object gethash(cl_object key, cl_object hash);
extern cl_object gethash_safe(cl_object key, cl_object hash, cl_object def);
extern bool remhash(cl_object key, cl_object hash);


/* init.c */

extern void init_lisp(void);
extern void init_libs(void);


/* instance.c */

#ifdef CLOS
extern cl_object cl_allocate_instance(cl_object clas, int size);
extern cl_object instance_ref(cl_object x, int i);
extern cl_object instance_set(cl_object x, int i, cl_object v);
extern void init_instance(void);
#endif /* CLOS */


/* iteration.c */

extern void do_bindings(cl_object var_list, struct iterator *bt);
extern void init_iteration(void);


/* let.c */

extern void let_bindings(cl_object var_list, struct let *let);
extern void init_let(void);


/* list.c */

extern cl_object list_length(cl_object x);
extern cl_object identity(cl_object x);
extern cl_object car(cl_object x);
extern cl_object cdr(cl_object x);
extern cl_object list(int narg, ...);
extern cl_object listX(int narg, ...);
extern cl_object append(cl_object x, cl_object y);
extern cl_object caar(cl_object x);
extern cl_object cadr(cl_object x);
extern cl_object cdar(cl_object x);
extern cl_object cddr(cl_object x);
extern cl_object caaar(cl_object x);
extern cl_object caadr(cl_object x);
extern cl_object cadar(cl_object x);
extern cl_object caddr(cl_object x);
extern cl_object cdaar(cl_object x);
extern cl_object cdadr(cl_object x);
extern cl_object cddar(cl_object x);
extern cl_object cdddr(cl_object x);
extern cl_object caaaar(cl_object x);
extern cl_object caaadr(cl_object x);
extern cl_object caadar(cl_object x);
extern cl_object caaddr(cl_object x);
extern cl_object cadaar(cl_object x);
extern cl_object cadadr(cl_object x);
extern cl_object caddar(cl_object x);
extern cl_object cadddr(cl_object x);
extern cl_object cdaaar(cl_object x);
extern cl_object cdaadr(cl_object x);
extern cl_object cdadar(cl_object x);
extern cl_object cdaddr(cl_object x);
extern cl_object cddaar(cl_object x);
extern cl_object cddadr(cl_object x);
extern cl_object cdddar(cl_object x);
extern cl_object cddddr(cl_object x);
extern bool endp(cl_object x);
extern cl_object nth(cl_fixnum n, cl_object x);
extern cl_object nthcdr(cl_fixnum n, cl_object x);
extern cl_object copy_list(cl_object x);
extern cl_object copy_alist(cl_object x);
extern cl_object copy_tree(cl_object x);
extern cl_object nconc(cl_object x, cl_object y);
extern cl_object subst(cl_object new_object, cl_object tree);
extern void nsubst(cl_object new_object, cl_object *treep);
extern cl_object sublis(cl_object alist, cl_object tree);
extern void nsublis(cl_object alist, cl_object *treep);
extern bool member_eq(cl_object x, cl_object l);
extern cl_object memq(cl_object x, cl_object l);
extern cl_object memql(cl_object x, cl_object l);
extern cl_object member(cl_object x, cl_object l);
extern cl_object assq(cl_object x, cl_object l);
extern cl_object assql(cl_object x, cl_object l);
extern cl_object assoc(cl_object x, cl_object l);
extern cl_object assqlp(cl_object x, cl_object l);
extern void init_list(void);


/* load.c */

extern void init_load(void);
extern void load_until_tag(cl_object stream, cl_object end_tag);

/* lwp.c */
#ifdef THREADS
extern int critical_level;
extern int update_queue(void);
extern int activate_thread(cl_object thread);
extern int thread_next(void);
extern int scheduler(int sig, int code, struct sigcontext *scp);
extern int interruption_handler(void);
extern int lwp_bds_wind(bds_ptr base, bds_ptr top);
extern int lwp_bds_unwind(bds_ptr base, bds_ptr top);
extern int enable_scheduler(void);
extern int enable_lwp(void);
extern int init_lwp(void);
#endif

/* macros.c */

extern cl_object search_macro(cl_object name, cl_object env);
extern cl_object macro_expand1(cl_object form, cl_object env);
extern cl_object macro_expand(cl_object form, cl_object env);
extern void init_macros(void);


/* main.c */

extern int cl_boot(int argc, char **argv);
extern int data_start;
extern const char *ecl_self;
extern void init_main(void);


/* mapfun.c */

extern void init_mapfun(void);


/* multival.c */

extern void init_multival(void);


/* num_arith.c */

extern cl_object fixnum_times(cl_fixnum i, cl_fixnum j);
extern cl_object number_times(cl_object x, cl_object y);
extern cl_object number_to_complex(cl_object x);
extern cl_object number_plus(cl_object x, cl_object y);
extern cl_object number_minus(cl_object x, cl_object y);
extern cl_object number_negate(cl_object x);
extern cl_object number_divide(cl_object x, cl_object y);
extern cl_object integer_divide(cl_object x, cl_object y);
extern cl_object get_gcd(cl_object x, cl_object y);
extern cl_object one_plus(cl_object x);
extern cl_object one_minus(cl_object x);
extern void init_num_arith(void);


/* number.c */

extern cl_object shortfloat_zero;
extern cl_object longfloat_zero;
extern cl_fixnum fixint(cl_object x);
extern cl_index  fixnnint(cl_object x);
extern cl_object make_integer(cl_fixnum i);
extern cl_object make_unsigned_integer(cl_index i);
extern cl_object make_ratio(cl_object num, cl_object den);
extern cl_object make_shortfloat(float f);
extern cl_object make_longfloat(double f);
extern cl_object make_complex(cl_object r, cl_object i);
extern double number_to_double(cl_object x);
extern void init_number(void);


/* num_co.c */

extern cl_object double_to_integer(double d);
extern cl_object float_to_integer(float d);
extern cl_object floor1(cl_object x);
extern cl_object ceiling1(cl_object x);
extern cl_object truncate1(cl_object x);
extern cl_object round1(cl_object x);
extern cl_object floor2(cl_object x, cl_object y);
extern cl_object ceiling2(cl_object x, cl_object y);
extern cl_object truncate2(cl_object x, cl_object y);
extern cl_object round2(cl_object x, cl_object y);
extern void init_num_co(void);


/* num_comp.c */

extern int number_equalp(cl_object x, cl_object y);
extern int number_compare(cl_object x, cl_object y);
extern void init_num_comp(void);


/* num_log.c */

extern cl_object integer_shift(cl_object x, cl_fixnum w);
extern int int_bit_length(int i);
extern void init_num_log(void);


/* num_pred.c */

extern int number_zerop(cl_object x);
extern int number_plusp(cl_object x);
extern int number_minusp(cl_object x);
extern int number_oddp(cl_object x);
extern int number_evenp(cl_object x);
extern void init_num_pred(void);


/* num_rand.c */

extern cl_object make_random_state(cl_object rs);
extern void init_num_rand(void);


/* num_sfun.c */

extern cl_object imag_unit;
extern cl_object minus_imag_unit;
extern cl_object imag_two;
extern cl_fixnum fixnum_expt(cl_fixnum x, cl_fixnum y);
extern cl_object number_exp(cl_object x);
extern cl_object number_expt(cl_object x, cl_object y);
extern cl_object number_nlog(cl_object x);
extern cl_object number_log(cl_object x, cl_object y);
extern cl_object number_sqrt(cl_object x);
extern cl_object number_atan2(cl_object y, cl_object x);
extern cl_object number_atan(cl_object y);
extern cl_object number_sin(cl_object x);
extern cl_object number_cos(cl_object x);
extern cl_object number_tan(cl_object x);
extern cl_object number_sinh(cl_object x);
extern cl_object number_cosh(cl_object x);
extern cl_object number_tanh(cl_object x);
extern void init_num_sfun(void);


/* package.c */

extern bool lisp_package_locked;
extern cl_object lisp_package;
extern cl_object user_package;
extern cl_object keyword_package;
extern cl_object system_package;
extern cl_object clos_package;
extern int intern_flag;
extern cl_object make_package(cl_object n, cl_object ns, cl_object ul);
extern cl_object rename_package(cl_object x, cl_object n, cl_object ns);
extern cl_object find_package(cl_object n);
extern cl_object coerce_to_package(cl_object p);
extern cl_object current_package(void);
extern cl_object intern(cl_object name, cl_object p);
extern cl_object _intern(const char *s, cl_object p);
extern cl_object find_symbol(cl_object name, cl_object p);
extern bool unintern(cl_object s, cl_object p);
extern void cl_export(cl_object s, cl_object p);
extern void cl_unexport(cl_object s, cl_object p);
extern void cl_import(cl_object s, cl_object p);
extern void shadowing_import(cl_object s, cl_object p);
extern void shadow(cl_object s, cl_object p);
extern void use_package(cl_object x0, cl_object p);
extern void unuse_package(cl_object x0, cl_object p);
extern void delete_package(cl_object p);
extern void init_package(void);
extern void init_package_function(void);


/* pathname.c */

extern cl_object make_pathname(cl_object host, cl_object device, cl_object directory, cl_object name, cl_object type, cl_object version);
extern cl_object parse_namestring(const char *s, cl_index start, cl_index end, cl_index *ep, cl_object default_host);
extern cl_object coerce_to_pathname(cl_object x);
extern cl_object coerce_to_physical_pathname(cl_object x);
extern cl_object coerce_to_file_pathname(cl_object pathname);
extern cl_object coerce_to_filename(cl_object pathname);
extern cl_object default_device(cl_object host);
extern cl_object merge_pathnames(cl_object path, cl_object defaults, cl_object default_version);
extern cl_object namestring(cl_object x);
extern cl_object coerce_to_namestring(cl_object x);
extern bool pathname_match_p(cl_object path, cl_object mask);
extern bool logical_hostname_p(cl_object host);
extern cl_object translate_pathname(cl_object path, cl_object from, cl_object to);
extern void init_pathname(void);


/* predicate.c */

extern bool numberp(cl_object x);
extern bool eql(cl_object x, cl_object y);
extern bool equal(register cl_object x, cl_object y);
extern bool equalp(cl_object x, cl_object y);
extern void init_predicate(void);


/* print.c */

extern bool PRINTescape;
extern bool PRINTpretty;
extern bool PRINTcircle;
extern int PRINTbase;
extern bool PRINTradix;
extern cl_object PRINTcase;
extern bool PRINTgensym;
extern int PRINTlevel;
extern int PRINTlength;
extern bool PRINTarray;
extern void (*write_ch_fun)(int);
extern cl_object PRINTpackage;
extern bool PRINTstructure;
extern cl_fixnum CIRCLEbase;
extern cl_object PRINTstream;
extern int interactive_writec_stream(int c, cl_object stream);
extern void flush_interactive_stream(cl_object stream);
extern void writec_PRINTstream(int c);
extern void write_str(char *s);
extern void write_decimal(int i);
extern void write_addr(cl_object x);
extern void edit_double(int n, double d, int *sp, char *s, int *ep);
extern void write_double(double d, int e, bool shortp);
extern void write_fixnum(cl_fixnum i);
extern void write_bignum(cl_object x);
extern void write_object(cl_object x, int level);
extern void setupPRINT(cl_object x, cl_object strm);
extern void cleanupPRINT(void);
extern bool potential_number_p(cl_object strng, int base);
extern cl_object princ(cl_object obj, cl_object strm);
extern cl_object prin1(cl_object obj, cl_object strm);
extern cl_object print(cl_object obj, cl_object strm);
extern cl_object terpri(cl_object strm);
extern void write_string(cl_object strng, cl_object strm);
extern void princ_str(const char *s, cl_object sym);
extern void princ_char(int c, cl_object sym);
extern void init_print(void);
extern void init_print_function(void);


/* profile.c */
#ifdef PROFILE
extern int total_ticks(unsigned short *aar, unsigned int dim);
extern int init_profile(void);
#endif


/* prog.c */

extern void init_prog(void);


/* read.c */

extern cl_object standard_readtable;
#ifndef THREADS
extern cl_object READtable;
extern int READdefault_float_format;
extern int READbase;
extern bool READsuppress;
extern bool preserving_whitespace_flag;
extern bool escape_flag;
extern cl_object delimiting_char;
extern bool detect_eos_flag;
extern bool in_list_flag;
extern bool dot_flag;
extern cl_object default_dispatch_macro;
extern cl_object sharp_eq_context;
#endif
extern cl_object interactive_readc(cl_object stream);
extern cl_object read_char(cl_object in);
extern void unread_char(cl_object c, cl_object in);
extern cl_object peek_char(bool pt, cl_object in);
extern cl_object read_object_non_recursive(cl_object in);
extern cl_object read_object(cl_object in);
extern cl_object parse_number(char *s, cl_index end, cl_index *ep, int radix);
extern cl_object parse_integer(char *s, cl_index end, cl_index *ep, int radix);
extern cl_object copy_readtable(cl_object from, cl_object to);
extern cl_object current_readtable(void);
extern cl_object string_to_object(cl_object x);
extern cl_object c_string_to_object(const char *s);
extern void init_read(void);
extern void init_read_function(void);
extern void read_VV(cl_object block, void *entry);


/* reference.c */

extern cl_object symbol_function(cl_object sym);
extern cl_object make_lambda(cl_object name, cl_object lambda);
extern void init_reference(void);


/* sequence.c */

extern cl_object cl_alloc_simple_vector(int l, cl_elttype aet);
extern cl_object cl_alloc_simple_bitvector(int l);
extern cl_object elt(cl_object seq, cl_fixnum index);
extern cl_object elt_set(cl_object seq, cl_fixnum index, cl_object val);
extern cl_fixnum length(cl_object x);
extern cl_object reverse(cl_object seq);
extern cl_object nreverse(cl_object seq);
extern void init_sequence(void);


/* stacks.c */

extern void bds_overflow(void) __attribute__((noreturn));
extern void bds_unwind(bds_ptr new_bds_top);
extern int frs_overflow(void) __attribute__((noreturn));
extern void unwind(frame_ptr fr, cl_object tag) __attribute__((noreturn));
extern frame_ptr frs_sch(cl_object frame_id);
extern frame_ptr frs_sch_catch(cl_object frame_id);
extern cl_object new_frame_id(void);
extern void init_stacks(int *);

/* string.c */

extern cl_object cl_alloc_simple_string(cl_index l);
extern cl_object cl_alloc_adjustable_string(cl_index l);
extern cl_object make_simple_string(char *s);
#define make_constant_string(s) (make_simple_string((char *)s))
extern cl_object make_string_copy(const char *s);
extern cl_object copy_simple_string(cl_object x);
extern cl_object coerce_to_string(cl_object x);
extern cl_object coerce_to_string_designator(cl_object x);
extern bool string_eq(cl_object x, cl_object y);
extern bool string_equal(cl_object x, cl_object y);
extern bool member_char(int c, cl_object char_bag);
extern void get_string_start_end(cl_object string, cl_object start, cl_object end, cl_index *ps, cl_index *pe);
extern void init_string(void);


/* structure.c */

extern bool structure_subtypep(cl_object x, cl_object y);
extern cl_object structure_to_list(cl_object x);
extern cl_object structure_ref(cl_object x, cl_object name, int n);
extern cl_object structure_set(cl_object x, cl_object name, int n, cl_object v);
extern void init_structure(void);


/* symbol.c */

extern cl_object gensym_prefix;
extern cl_object gentemp_prefix;
extern cl_object cl_token;
extern cl_object make_symbol(cl_object st);
extern cl_object make_ordinary(const char *s);
extern cl_object make_special(const char *s, cl_object v);
extern cl_object make_constant(const char *s, cl_object v);
extern cl_object make_si_ordinary(const char *s);
extern cl_object make_si_special(const char *s, cl_object v);
extern cl_object make_si_constant(const char *s, cl_object v);
extern cl_object make_keyword(const char *s);
extern cl_object symbol_value(cl_object s);
extern cl_object getf(cl_object place, cl_object indicator, cl_object deflt);
extern cl_object get(cl_object s, cl_object p, cl_object d);
extern cl_object putf(cl_object p, cl_object v, cl_object i);
extern cl_object putprop(cl_object s, cl_object v, cl_object p);
extern bool remf(cl_object *p, cl_object i);
extern cl_object remprop(cl_object s, cl_object p);
extern bool keywordp(cl_object s);
extern cl_object symbol_name(cl_object x);
extern void init_symbol(void);
extern void init_symbol_function(void);


/* tclBasic.c */

#ifdef TK
extern cl_object TkWidgetType;
extern Tcl_Interp *ECL_interp;
extern int Tcl_GlobalEval(Tcl_Interp *interp, char *s);
extern int Tcl_Eval(Tcl_Interp *interp, char *s);
extern int Tcl_VarEval(Tcl_Interp *interp, ...);
extern char *Tcl_GetVar(Tcl_Interp *interp, char *var, int flags);
extern char *Tcl_GetVar2(Tcl_Interp *interp, char *name1, char *name2, int flags);
extern char *Tcl_SetVar(Tcl_Interp *interp, char *var, char *val, int flags);
extern char *Tcl_SetVar2(Tcl_Interp *interp, char *name1, char *name2, char *val, int flags);
extern int Tcl_DeleteCommand(Tcl_Interp *interp, char *cmdName);
extern int tclMethodDispatch(int narg, cl_object env, ...);
extern void Tcl_CreateCommand(Tcl_Interp *interp, char *cmdName, Tcl_CmdProc *proc, ClientData clientData, Tcl_CmdDeleteProc *deleteProc);
extern int Tcl_GetCommandInfo(Tcl_Interp *interp, char *cmdName, Tcl_CmdInfo *infoPtr);
extern Tcl_Interp *Tcl_CreateInterp(void);
extern void Tcl_DeleteInterp(Tcl_Interp *interp);
extern int init_tk(void);
extern int Tcl_Init(Tcl_Interp *interp);
extern void Tcl_CallWhenDeleted(Tcl_Interp *interp, Tcl_InterpDeleteProc *proc, ClientData clientData);
extern void Tcl_DontCallWhenDeleted(Tcl_Interp *interp, Tcl_InterpDeleteProc *proc, ClientData clientData);
extern int Tcl_SetCommandInfo(Tcl_Interp *interp, char *cmdName, Tcl_CmdInfo *infoPtr);
extern Tcl_Trace Tcl_CreateTrace(Tcl_Interp *interp, int level, Tcl_CmdTraceProc *proc, ClientData clientData);
extern void Tcl_DeleteTrace(Tcl_Interp *interp, Tcl_Trace trace);
extern void Tcl_AddErrorInfo(Tcl_Interp *interp, char *message);
extern int Tcl_SetRecursionLimit(Tcl_Interp *interp, int depth);
extern int Tcl_TraceVar(Tcl_Interp *interp, char *varName, int flags, Tcl_VarTraceProc *proc, ClientData clientData);
extern int Tcl_TraceVar2(Tcl_Interp *interp, char *part1, char *part2, int flags, Tcl_VarTraceProc *proc, ClientData clientData);
extern void Tcl_UntraceVar(Tcl_Interp *interp, char *varName, int flags, Tcl_VarTraceProc *proc, ClientData clientData);
extern void Tcl_UntraceVar2(Tcl_Interp *interp, char *part1, char *part2, int flags, Tcl_VarTraceProc *proc, ClientData clientData);
extern void Tcl_ChangeValue(char *var);
#endif


/* tcp.c */

#ifdef TCP
extern cl_object make_stream(cl_object host, int fd, enum smmode smm);
extern int init_tcp(void);
#endif


/* time.c */

extern int runtime(void);
extern cl_object UTC_time_to_universal_time(int i);
extern void init_unixtime(void);


/* tkMain.c */

#ifdef TK
extern Tcl_Interp *ECL_interp;
extern int Tk_initialized;
extern cl_object Tk_root_window;
extern void Tk_main(int synchronize, char *name, char *fileName, char *Xdisplay, char *geometry);
#endif


/* toplevel.c */

extern void init_toplevel(void);


/* typespec.c */

extern void assert_type_integer(cl_object p);
extern void assert_type_non_negative_integer(cl_object p);
extern void assert_type_character(cl_object p);
extern void assert_type_symbol(cl_object p);
extern void assert_type_package(cl_object p);
extern void assert_type_string(cl_object p);
extern void assert_type_cons(cl_object p);
extern void assert_type_stream(cl_object p);
extern void assert_type_readtable(cl_object p);
extern void assert_type_hash_table(cl_object p);
extern void assert_type_array(cl_object p);
extern void assert_type_vector(cl_object p);
extern void assert_type_list(cl_object p);
extern void assert_type_proper_list(cl_object p);
extern cl_object TYPE_OF(cl_object x);
extern void init_typespec(void);
extern void init_typespec_function(void);

extern void FEtype_error_character(cl_object x) __attribute__((noreturn));
extern void FEtype_error_cons(cl_object x) __attribute__((noreturn));
extern void FEtype_error_number(cl_object x) __attribute__((noreturn));
extern void FEtype_error_real(cl_object x) __attribute__((noreturn));
extern void FEtype_error_float(cl_object x) __attribute__((noreturn));
extern void FEtype_error_integer(cl_object x) __attribute__((noreturn));
extern void FEtype_error_list(cl_object x) __attribute__((noreturn));
extern void FEtype_error_proper_list(cl_object x) __attribute__((noreturn));
extern void FEtype_error_plist(cl_object x) __attribute__((noreturn));
extern void FEtype_error_alist(cl_object x) __attribute__((noreturn));
extern void FEtype_error_stream(cl_object x) __attribute__((noreturn));
extern void FEcircular_list(cl_object x) __attribute__((noreturn));
extern void FEtype_error_index(cl_object x) __attribute__((noreturn));
extern void FEtype_error_string(cl_object x) __attribute__((noreturn));

/* unixfsys.c */

extern const char *expand_pathname(const char *name);
extern cl_object string_to_pathname(char *s);
extern cl_object truename(cl_object pathname);
extern bool file_exists(cl_object file);
extern FILE *backup_fopen(const char *filename, const char *option);
extern int file_len(FILE *fp);
extern cl_object homedir_pathname(cl_object user);
extern void init_unixfsys(void);
extern void FEfilesystem_error(const char *msg, int narg, ...);


/* unixint.c */

extern int interrupt_enable;
extern int interrupt_flag;
extern void signal_catcher(int sig, int code, int scp);
extern void enable_interrupt(void);
extern void init_interrupt(void);


/* unixsys.c */

extern void init_unixsys(void);

/* unexec.c */

extern int unexec(char *new_name, char *a_name, unsigned, unsigned, unsigned);

#ifdef __cplusplus
}
#endif
