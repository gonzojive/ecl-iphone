/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    main.c --
*/
/*
    Copyright (c) 1984, Taiichi Yuasa and Masami Hagiya.
    Copyright (c) 1990, Giuseppe Attardi.
    Copyright (c) 2001, Juan Jose Garcia Ripoll.

    ECL is free software; you can redistribute it and/or
    modify it under the terms of the GNU Library General Public
    License as published by the Free Software Foundation; either
    version 2 of the License, or (at your option) any later version.

    See file '../Copyright' for full details.
*/

/******************************** IMPORTS *****************************/

#include <ecl/ecl.h>
#include <limits.h>
#if defined(_MSC_VER) || defined(mingw32)
# include <windows.h>
# include <shellapi.h>
# define MAXPATHLEN 512
#endif
#ifndef MAXPATHLEN
# ifdef PATH_MAX
#   define MAXPATHLEN PATH_MAX
# else
#   define NO_PATH_MAX
#   include <unistd.h>
# endif
#endif
#ifdef ECL_USE_MPROTECT
# include <sys/mman.h>
# ifndef MAP_FAILED
#  define MAP_FAILED -1
# endif
#endif
#include <stdio.h>
#include <stdlib.h>
#include <ecl/internal.h>
extern int GC_dont_gc;

/******************************* EXPORTS ******************************/

#if !defined(ECL_THREADS)
cl_env_ptr cl_env_p = NULL;
#elif defined(WITH___THREAD)
__thread cl_env_ptr cl_env_p = NULL;
#endif
struct cl_core_struct cl_core;
const char *ecl_self;

/************************ GLOBAL INITIALIZATION ***********************/

static int ARGC;
static char **ARGV;
static cl_fixnum option_values[ECL_OPT_LIMIT+1] = {
#ifdef GBC_BOEHM_GENGC
	1,	/* ECL_OPT_INCREMENTAL_GC */
#else
	0,	/* ECL_OPT_INCREMENTAL_GC */
#endif
	1,	/* ECL_OPT_TRAP_SIGSEGV */
	1,	/* ECL_OPT_TRAP_SIGFPE */
	1,	/* ECL_OPT_TRAP_SIGINT */
	1,	/* ECL_OPT_TRAP_SIGILL */
	1,	/* ECL_OPT_TRAP_SIGBUS */
	0,	/* ECL_OPT_BOOTED */
	8192,	/* ECL_OPT_BIND_STACK_SIZE */
	128,	/* ECL_OPT_BIND_STACK_SAFETY_AREA */
	2048,	/* ECL_OPT_FRAME_STACK_SIZE */
	128,	/* ECL_OPT_FRAME_STACK_SAFETY_AREA */
	32768,	/* ECL_OPT_LISP_STACK_SIZE */
	128,	/* ECL_OPT_LISP_STACK_SAFETY_AREA */
	131072,	/* ECL_OPT_C_STACK_SIZE */
	4192,	/* ECL_OPT_C_STACK_SAFETY_AREA */
	1,	/* ECL_OPT_SIGALTSTACK_SIZE */
	128*1024*1024, /* ECL_OPT_HEAP_SIZE */
	1024*1024, /* ECL_OPT_HEAP_SAFETY_AREA */
	0};

#if !defined(GBC_BOEHM)
static char stdin_buf[BUFSIZ];
static char stdout_buf[BUFSIZ];
#endif

static const char *feature_names[] = {
        "ECL", "COMMON", ECL_ARCHITECTURE, "FFI", "PREFIXED-API",
        "IEEE-FLOATING-POINT",
#ifdef WITH_GMP
        "COMMON-LISP",
        "ANSI-CL",
#endif /* WITH_GMP */
#if defined(GBC_BOEHM)
	"BOEHM-GC",
#endif
#ifdef ECL_THREADS
	"THREADS",
#endif
#ifdef CLOS
	"CLOS",
#endif
#ifdef ENABLE_DLOPEN
	"DLOPEN",
#endif
#ifdef ECL_OLD_LOOP
	"OLD-LOOP",
#endif
	"ECL-PDE",
#ifdef unix
	"UNIX",
#endif
#ifdef BSD
	"BSD",
#endif
#ifdef SYSV
	"SYSTEM-V",
#endif
#ifdef MSDOS
	"MS-DOS",
#endif
#ifdef mingw32
	"MINGW32",
#endif
#ifdef _MSC_VER
	"MSVC",
#endif
#ifdef ECL_CMU_FORMAT
	"CMU-FORMAT",
#endif
#ifdef ECL_CLOS_STREAMS
	"CLOS-STREAMS",
#endif
#ifdef ECL_DYNAMIC_FFI
	"DFFI",
#endif
#ifdef ECL_UNICODE
	"UNICODE",
#endif
#ifdef ECL_LONG_FLOAT
	"LONG-FLOAT",
#endif
#ifdef ECL_SHORT_FLOAT
	"SHORT-FLOAT",
#endif
#ifdef ECL_RELATIVE_PACKAGE_NAMES
	"RELATIVE-PACKAGE-NAMES",
#endif
#ifdef ecl_uint16_t
        "UINT16-T",
#endif
#ifdef ecl_uint32_t
        "UINT32-T",
#endif
#ifdef ecl_uint64_t
        "UINT64-T",
#endif
        0
};

cl_fixnum
ecl_get_option(int option)
{
	if (option >= ECL_OPT_LIMIT || option < 0) {
		FEerror("Invalid boot option ~D", 1, MAKE_FIXNUM(option));
	} else {
		return option_values[option];
	}
}

void
ecl_set_option(int option, cl_fixnum value)
{
	if (option > ECL_OPT_LIMIT || option < 0) {
		FEerror("Invalid boot option ~D", 1, MAKE_FIXNUM(option));
	} else {
		if (option < ECL_OPT_BOOTED &&
		    option_values[ECL_OPT_BOOTED]) {
			FEerror("Cannot change option ~D while ECL is running",
				1, MAKE_FIXNUM(option));
		}
		option_values[option] = value;
	}
}

void
ecl_init_env(cl_env_ptr env)
{
	int i;

	env->c_env = NULL;

	env->string_pool = Cnil;

	env->stack = NULL;
	env->stack_top = NULL;
	env->stack_limit = NULL;
	env->stack_size = 0;
	ecl_stack_set_size(env, ecl_get_option(ECL_OPT_LISP_STACK_SIZE));

#if !defined(ECL_CMU_FORMAT)
	env->print_pretty = FALSE;
	env->queue = ecl_alloc_atomic(ECL_PPRINT_QUEUE_SIZE * sizeof(short));
	env->indent_stack = ecl_alloc_atomic(ECL_PPRINT_INDENTATION_STACK_SIZE * sizeof(short));
	env->fmt_aux_stream = ecl_make_string_output_stream(64, 1);
#endif
#if !defined(GBC_BOEHM)
# if defined(THREADS)
#  error "No means to mark the stack of a thread :-/"
# else
	/* Rough estimate. Not very safe. We assume that cl_boot()
	 * is invoked from the main() routine of the program.
	 */
	env->cs_org = (cl_object*)(&env);
# endif /* THREADS */
#endif /* !GBC_BOEHM */

#ifdef ECL_DYNAMIC_FFI
	env->fficall = ecl_alloc(sizeof(struct ecl_fficall));
	((struct ecl_fficall*)env->fficall)->registers = 0;
#endif

#ifdef CLOS
	env->method_hash = Cnil;
	env->method_spec_vector = Cnil;
	env->method_generation = 0;
	_ecl_set_method_hash_size(env, 4096);
#ifdef ECL_THREADS
	env->method_hash_clear_list = Cnil;
#endif
#endif
	init_stacks(env, &i);
}

static const struct {
	const char *name;
	int code;
} char_names[] = {
	{"Null", 0},
	{"Soh", 1},
	{"Stx", 2},
	{"Etx", 3},
	{"Eot", 4},
	{"Enq", 5},
	{"Ack", 6},
	{"Bell", 7},
	{"Backspace", 8},
	{"Tab", 9},
	{"Linefeed", 10},
	{"Newline", 10},
	{"Vt", 11},
	{"Page", 12},
	{"Return", 13},
	{"So", 14},
	{"Si", 15},
	{"Dle", 16},
	{"Dc1", 17},
	{"Dc2", 18},
	{"Dc3", 19},
	{"Dc4", 20},
	{"Nak", 21},
	{"Syn", 22},
	{"Etb", 23},
	{"Can", 24},
	{"Em", 25},
	{"Sub", 26},
	{"Escape", 27},
	{"Esc", 27},
	{"Fs", 28},
	{"Gs", 29},
	{"Rs", 30},
	{"Us", 31},
	{"Space", 32},
	{"Rubout", 127},
	{NULL, -1}
};

cl_env_ptr
_ecl_alloc_env()
{
	/*
	 * Allocates the lisp environment for a thread. Depending on which
	 * mechanism we use for detecting delayed signals, we may allocate
	 * the environment using mmap or the garbage collector.
	 */
	cl_env_ptr output;
#if defined(ECL_USE_MPROTECT)
	output = mmap(0, sizeof(*output), PROT_READ | PROT_WRITE,
			MAP_ANON | MAP_PRIVATE, -1, 0);
	if (output == MAP_FAILED)
		ecl_internal_error("Unable to allocate environment structure.");
#else
	static struct cl_env_struct first_env;
	if (!ecl_get_option(ECL_OPT_BOOTED)) {
		/* We have not set up any environment. Hence, we cannot call ecl_alloc()
		 * because it will need to stop interrupts and currently we rely on
		 * the environment for that */
		output = &first_env;
	} else {
		output = ecl_alloc(sizeof(*output));
	}
#endif
	/*
	 * An uninitialized environment _always_ disables interrupts. They
	 * are activated later on by the thread entry point or init_unixint().
	 */
	output->disable_interrupts = 1;
	return output;
}

int
cl_shutdown(void)
{
	const cl_env_ptr env = ecl_process_env();
	if (ecl_get_option(ECL_OPT_BOOTED) > 0) {
		cl_object l = ecl_symbol_value(@'si::*exit-hooks*');
		cl_object form = cl_list(2, @'funcall', Cnil);
		while (CONSP(l)) {
			ecl_elt_set(form, 1, ECL_CONS_CAR(l));
			si_safe_eval(3, form, Cnil, OBJNULL);
			l = CDR(l);
			ECL_SET(@'si::*exit-hooks*', l);
		}
#ifdef ENABLE_DLOPEN
		ecl_library_close_all();
#endif
#ifdef TCP
		ecl_tcp_close_all();
#endif
	}
	ecl_set_option(ECL_OPT_BOOTED, -1);
	return 1;
}

#ifdef ECL_UNICODE
static cl_object
read_char_database()
{
	cl_object s = si_base_string_concatenate(2,
						 si_get_library_pathname(),
						 make_constant_base_string("ucd.dat"));
	cl_object output = Cnil;
	FILE *f = fopen(s->base_string.self, "r");
	if (f) {
		cl_index size;
		if (!fseek(f, 0, SEEK_END)) {
			size = ftell(f);
			fseek(f, 0, SEEK_SET);
			output = si_make_vector(@'ext::byte8', MAKE_FIXNUM(size),
						Cnil, Cnil, Cnil, Cnil);
			if (fread(output->vector.self.b8, 1, size, f) < size) {
				output = Cnil;
			} else {
				uint8_t *p = output->vector.self.b8;
				cl_core.unicode_database = output;
				cl_core.ucd_misc = p + 2;
				cl_core.ucd_pages = cl_core.ucd_misc + (p[0] + (p[1]<<8));
				cl_core.ucd_data = cl_core.ucd_pages + (0x110000 / 256);
			}
		}
		fclose(f);
	}
	if (output == Cnil) {
		printf("Unable to read Unicode database: %s\n", s->base_string.self);
		abort();
	}
	ECL_SET(@'si::+unicode-database+', output);
}
#else
#define read_char_database() (void)0
#endif

int
cl_boot(int argc, char **argv)
{
	cl_object aux;
	cl_object features;
	int i;
	cl_env_ptr env;

	i = ecl_get_option(ECL_OPT_BOOTED);
	if (i) {
		if (i < 0) {
			/* We have called cl_shutdown and want to use ECL again. */
			ecl_set_option(ECL_OPT_BOOTED, 1);
		}
		return 1;
	}

#if !defined(GBC_BOEHM)
	setbuf(stdin,  stdin_buf);
	setbuf(stdout, stdout_buf);
#endif

	ARGC = argc;
	ARGV = argv;
	ecl_self = argv[0];

	init_unixint(0);
	init_alloc();
	GC_disable();
	env = _ecl_alloc_env();
#if !defined(ECL_THREADS) || defined(WITH__THREAD)
	cl_env_p = env;
#else
	init_threads(env);
#endif

	/*
	 * 1) Initialize symbols and packages
	 */

	Cnil_symbol->symbol.t = t_symbol;
	Cnil_symbol->symbol.dynamic = 0;
	Cnil_symbol->symbol.value = Cnil;
	Cnil_symbol->symbol.name = make_constant_base_string("NIL");
	Cnil_symbol->symbol.gfdef = Cnil;
	Cnil_symbol->symbol.plist = Cnil;
	Cnil_symbol->symbol.hpack = Cnil;
	Cnil_symbol->symbol.stype = stp_constant;
	cl_num_symbols_in_core=1;

	Ct->symbol.t = (short)t_symbol;
	Ct->symbol.dynamic = 0;
	Ct->symbol.value = Ct;
	Ct->symbol.name = make_constant_base_string("T");
	Ct->symbol.gfdef = Cnil;
	Ct->symbol.plist = Cnil;
	Ct->symbol.hpack = Cnil;
	Ct->symbol.stype = stp_constant;
	cl_num_symbols_in_core=2;

#ifdef NO_PATH_MAX
	cl_core.path_max = sysconf(_PC_PATH_MAX);
#else
	cl_core.path_max = MAXPATHLEN;
#endif

	cl_core.packages = Cnil;
	cl_core.packages_to_be_created = OBJNULL;

	cl_core.lisp_package =
		ecl_make_package(make_constant_base_string("COMMON-LISP"),
				 cl_list(2, make_constant_base_string("CL"),
					 make_constant_base_string("LISP")),
				 Cnil);
	cl_core.user_package =
		ecl_make_package(make_constant_base_string("COMMON-LISP-USER"),
				 cl_list(2, make_constant_base_string("CL-USER"),
					 make_constant_base_string("USER")),
				 ecl_list1(cl_core.lisp_package));
	cl_core.keyword_package =
		ecl_make_package(make_constant_base_string("KEYWORD"),
				 Cnil, Cnil);
	cl_core.system_package =
		ecl_make_package(make_constant_base_string("SI"),
				 cl_list(3,
					 make_constant_base_string("SYSTEM"),
					 make_constant_base_string("SYS"),
					 make_constant_base_string("EXT")),
				 ecl_list1(cl_core.lisp_package));
#ifdef CLOS
	cl_core.clos_package =
		ecl_make_package(make_constant_base_string("CLOS"),
				 Cnil, ecl_list1(cl_core.lisp_package));
#endif
#ifdef ECL_THREADS
	cl_core.mp_package =
		ecl_make_package(make_constant_base_string("MP"),
				 ecl_list1(make_constant_base_string("MULTIPROCESSING")),
				 ecl_list1(cl_core.lisp_package));
#endif
#ifdef ECL_CLOS_STREAMS
	cl_core.gray_package = ecl_make_package(make_constant_base_string("GRAY"),
						Cnil,
						CONS(cl_core.lisp_package, Cnil));
#endif

	Cnil_symbol->symbol.hpack = cl_core.lisp_package;
	cl_import2(Cnil, cl_core.lisp_package);
	cl_export2(Cnil, cl_core.lisp_package);

	Ct->symbol.hpack = cl_core.lisp_package;
	cl_import2(Ct, cl_core.lisp_package);
	cl_export2(Ct, cl_core.lisp_package);

	/* At exit, clean up */
	atexit((void*)cl_shutdown);

	/* These must come _after_ the packages and NIL/T have been created */
	init_all_symbols();
	read_char_database();

	/*
	 * 2) Initialize constants (strings, numbers and time).
	 */

	/* FIXME! This is a hack! We use EQUALP hashes because we know that
	 * the characters in this table will not be alphanumeric.
	 */
	cl_core.char_names = aux =
	    cl__make_hash_table(@'equalp', MAKE_FIXNUM(128), /* size */
				ecl_make_singlefloat(1.5f), /* rehash-size */
				ecl_make_singlefloat(0.5f), /* rehash-threshold */
				Cnil); /* thread-safe */
	for (i = 0; char_names[i].code >= 0; i++) {
		cl_object name = make_constant_base_string(char_names[i].name);
		cl_object code = CODE_CHAR(char_names[i].code);
		ecl_sethash(name, aux, code);
		ecl_sethash(code, aux, name);
	}

	/* LIBRARIES is an adjustable vector of objects. It behaves as
	   a vector of weak pointers thanks to the magic in
	   gbc.d/alloc_2.d */
	cl_core.libraries = Cnil;
#if 0
	/* FINALIZERS and FINALIZABLE_OBJECTS are also like LIBRARIES */
	cl_core.finalizable_objects = si_make_vector(@'t', MAKE_FIXNUM(512),
						     @'t', MAKE_FIXNUM(0),
						     @'nil', @'nil');
	cl_core.finalizers = si_make_vector(@'t', MAKE_FIXNUM(512),
					    @'t', MAKE_FIXNUM(0),
					    @'nil', @'nil');
#endif
	cl_core.to_be_finalized = Cnil;
	cl_core.bytes_consed = Cnil;
	cl_core.gc_counter = Cnil;
	cl_core.gc_stats = FALSE;

	cl_core.null_string = make_constant_base_string("");

	cl_core.null_stream = Cnil; /* Filled in file.d */

	cl_core.system_properties =
	    cl__make_hash_table(@'equal', MAKE_FIXNUM(1024), /* size */
				ecl_make_singlefloat(1.5f), /* rehash-size */
				ecl_make_singlefloat(0.75f), /* rehash-threshold */
				Ct); /* thread-safe */

	cl_core.gensym_prefix = make_constant_base_string("G");
	cl_core.gentemp_prefix = make_constant_base_string("T");
	cl_core.gentemp_counter = MAKE_FIXNUM(0);

	ECL_SET(@'si::c-int-max', ecl_make_integer(INT_MAX));
	ECL_SET(@'si::c-int-min', ecl_make_integer(INT_MIN));
	ECL_SET(@'si::c-long-max', ecl_make_integer(LONG_MAX));
	ECL_SET(@'si::c-long-min', ecl_make_integer(LONG_MIN));
	ECL_SET(@'si::c-uint-max', ecl_make_unsigned_integer(UINT_MAX));
	ECL_SET(@'si::c-ulong-max', ecl_make_unsigned_integer(ULONG_MAX));

	init_number();
	init_unixtime();

	/*
	 * 3) Initialize the per-thread data.
	 *    This cannot come later, because some routines need the
	 *    frame stack immediately (for instance SI:PATHNAME-TRANSLATIONS).
	 */
	ecl_init_env(env);
#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif
	GC_enable();

#ifdef ECL_THREADS
	env->bindings_hash = cl__make_hash_table(@'eq', MAKE_FIXNUM(1024),
						   ecl_make_singlefloat(1.5f),
						   ecl_make_singlefloat(0.75f),
						   Cnil); /* no locking */
	ECL_SET(@'mp::*current-process*', env->own_process);
#endif

	/*
	 * 4) Initialize I/O subsystem and pathnames.
	 */

	init_file();
	init_read();

	ECL_SET(@'*print-case*', @':upcase');

	cl_core.pathname_translations = Cnil;
#if 1
	ECL_SET(@'*default-pathname-defaults*', si_getcwd(0));
#else
	ECL_SET(@'*default-pathname-defaults*',
		ecl_make_pathname(Cnil, Cnil, Cnil, Cnil, Cnil, Cnil));
#endif

	@si::pathname-translations(2,make_constant_base_string("SYS"),
				   cl_list(1,cl_list(2,make_constant_base_string("*.*"),
						     make_constant_base_string("./*.*"))));

	/*
	 * 5) Set up hooks for LOAD, errors and macros.
	 */
#ifdef ECL_THREADS
	ECL_SET(@'mp::+load-compile-lock+',
		mp_make_lock(2, @':name', @'mp::+load-compile-lock+'));
#endif
	aux = cl_list(
#ifdef ENABLE_DLOPEN
		7,CONS(make_constant_base_string("fas"), @'si::load-binary'),
		CONS(make_constant_base_string("fasl"), @'si::load-binary'),
#else
		5,
#endif
		CONS(make_constant_base_string("lsp"), @'si::load-source'),
		CONS(make_constant_base_string("lisp"), @'si::load-source'),
		CONS(make_constant_base_string("LSP"), @'si::load-source'),
		CONS(make_constant_base_string("LISP"), @'si::load-source'),
		CONS(Cnil, @'si::load-source'));
	ECL_SET(@'si::*load-hooks*', aux);
	init_error();
	init_macros();

	/*
	 * 6) Set up infrastructure for CLOS.
	 */
#ifdef CLOS
	ECL_SET(@'si::*class-name-hash-table*',
		cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
				    ecl_make_singlefloat(1.5f), /* rehash-size */
				    ecl_make_singlefloat(0.75f), /* rehash-threshold */
				    Ct)); /* thread safe */
#endif

	/*
	 * 7) Features.
	 */

	ECL_SET(@'LAMBDA-LIST-KEYWORDS',
		cl_list(8, @'&optional', @'&rest', @'&key', @'&allow-other-keys',
			@'&aux', @'&whole', @'&environment', @'&body'));

        for (i = 0, features = Cnil; feature_names[i]; i++) {
                features = CONS(ecl_make_keyword(feature_names[i]),features);
        }

	ECL_SET(@'*features*', features);

	ECL_SET(@'*package*', cl_core.lisp_package);

	/* This has to come before init_LSP/CLOS, because we need
	 * ecl_clear_compiler_properties() to work in init_CLOS(). */
	ecl_set_option(ECL_OPT_BOOTED, 1);

	read_VV(OBJNULL,init_lib_LSP);

	/* Jump to top level */
	ECL_SET(@'*package*', cl_core.user_package);
	init_unixint(1);
	return 1;
}

/************************* ENVIRONMENT ROUTINES ***********************/

@(defun ext::quit (&optional (code MAKE_FIXNUM(0)))
	cl_fixnum i;
@
	if (!FIXNUMP(code))
		FEerror("Illegal exit code: ~S.", 1, code);
	i = fix(code);
	exit(i);
@)

cl_object
si_argc()
{
	@(return MAKE_FIXNUM(ARGC))
}

cl_object
si_argv(cl_object index)
{
	if (FIXNUMP(index)) {
		cl_fixnum i = fix(index);
		if (i >= 0 && i < ARGC)
			@(return make_base_string_copy(ARGV[i]));
	}
	FEerror("Illegal argument index: ~S.", 1, index);
}

cl_object
si_getenv(cl_object var)
{
	const char *value;

	var = ecl_check_cl_type(@'ext::getenv', var, t_base_string);
	value = getenv((char*)var->base_string.self);
	@(return ((value == NULL)? Cnil : make_base_string_copy(value)))
}

#if defined(HAVE_SETENV) || defined(HAVE_PUTENV)
cl_object
si_setenv(cl_object var, cl_object value)
{
	const cl_env_ptr the_env = ecl_process_env();
	cl_fixnum ret_val;

	/* Strings have to be null terminated base strings */
	var = si_copy_to_simple_base_string(var);
	if (value == Cnil) {
#ifdef HAVE_SETENV
		/* Remove the variable when setting to nil, so that
		 * (si:setenv "foo" nil), then (si:getenv "foo) returns
		 * the right thing. */
		unsetenv((char*)var->base_string.self);
#else
#if defined(_MSC_VER) || defined(mingw32)
		si_setenv(var, make_simple_base_string(""));
#else
		putenv((char*)var->base_string.self);
#endif
#endif
		ret_val = 0;
	} else {
#ifdef HAVE_SETENV
		value = si_copy_to_simple_base_string(value);
		ret_val = setenv((char*)var->base_string.self,
				 (char*)value->base_string.self, 1);
#else
		value = cl_format(4, Cnil, make_constant_base_string("~A=~A"), var,
				  value);
		value = si_copy_to_simple_base_string(value);
		putenv((char*)temp->base_string.self);
#endif
	}
	if (ret_val == -1)
		CEerror(Ct, "SI:SETENV failed: insufficient space in environment.",
			1, Cnil);
	@(return value)
}
#endif

cl_object
si_pointer(cl_object x)
{
	const cl_env_ptr the_env = ecl_process_env();
	@(return ecl_make_unsigned_integer((cl_index)x))
}

#if defined(_MSC_VER) || defined(mingw32)
void
ecl_get_commandline_args(int* argc, char*** argv) {
	LPWSTR *wArgs;
	int i;

	if (argc == NULL || argv == NULL)
		return;

	wArgs = CommandLineToArgvW(GetCommandLineW(), argc);
	*argv = (char**)malloc(sizeof(char*)*(*argc));
	for (i=0; i<*argc; i++) {
		int len = wcslen(wArgs[i]);
		(*argv)[i] = (char*)malloc(2*(len+1));
		wcstombs((*argv)[i], wArgs[i], len+1);
	}
	LocalFree(wArgs);
}
#endif
