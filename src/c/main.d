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

#include <stdlib.h>
#include "ecl.h"
#include "internal.h"
#ifdef TK
# include "tk.h"
#endif

/******************************* EXPORTS ******************************/

bool ecl_booted = 0;
#if !defined(ECL_THREADS)
struct cl_env_struct cl_env;
#endif
struct cl_core_struct cl_core;
const char *ecl_self;

/************************ GLOBAL INITIALIZATION ***********************/

static int	ARGC;
static char	**ARGV;

#if !defined(GBC_BOEHM)
static char stdin_buf[BUFSIZ];
static char stdout_buf[BUFSIZ];
#endif

void
ecl_init_env(struct cl_env_struct *env)
{
	int i;

	env->lex_env = Cnil;

	env->token = cl_alloc_adjustable_string(LISP_PAGESIZE);

	env->stack = NULL;
	env->stack_top = NULL;
	env->stack_limit = NULL;
	env->stack_size = 0;
	cl_stack_set_size(16*LISP_PAGESIZE);

#if !defined(ECL_CMU_FORMAT)
	env->print_pretty = FALSE;
	env->queue = cl_alloc_atomic(ECL_PPRINT_QUEUE_SIZE * sizeof(short));
	env->indent_stack = cl_alloc_atomic(ECL_PPRINT_INDENTATION_STACK_SIZE * sizeof(short));
	env->fmt_aux_stream = make_string_output_stream(64);
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

	init_stacks(&i);
}

static const struct {
	char *name;
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
	{"Fs", 28},
	{"Gs", 29},
	{"Rs", 30},
	{"Us", 31},
	{"Space", 32},
	{"Rubout", 127},
	{NULL, -1}
};

int
cl_shutdown(void)
{
#ifdef ENABLE_DLOPEN
	ecl_library_close_all();
#endif
}

int
cl_boot(int argc, char **argv)
{
	cl_object aux;
	cl_object features;
	int i;

#if !defined(GBC_BOEHM)
	setbuf(stdin,  stdin_buf);
	setbuf(stdout, stdout_buf);
#endif

	ARGC = argc;
	ARGV = argv;
	ecl_self = argv[0];

	init_alloc();
	GC_disable();
#ifdef ECL_THREADS
	init_threads();
#endif

#if !defined(MSDOS) && !defined(cygwin)
	ecl_self = expand_pathname(ecl_self);
#endif

	/*
	 * 1) Initialize symbols and packages
	 */

	Cnil->symbol.t = (short)t_symbol;
	Cnil->symbol.dynamic = 0;
	Cnil->symbol.value = Cnil;
	Cnil->symbol.name = make_constant_string("NIL");
	Cnil->symbol.gfdef = OBJNULL;
	Cnil->symbol.plist = Cnil;
	Cnil->symbol.hpack = Cnil;
	Cnil->symbol.stype = (short)stp_constant;
	Cnil->symbol.mflag = FALSE;
	Cnil->symbol.isform = FALSE;
	cl_num_symbols_in_core=1;

	Ct->symbol.t = (short)t_symbol;
	Ct->symbol.dynamic = 0;
	Ct->symbol.value = Ct;
	Ct->symbol.name = make_constant_string("T");
	Ct->symbol.gfdef = OBJNULL;
	Ct->symbol.plist = Cnil;
	Ct->symbol.hpack = Cnil;
	Ct->symbol.stype = (short)stp_constant;
	Ct->symbol.mflag = FALSE;
	Ct->symbol.isform = FALSE;
	cl_num_symbols_in_core=2;

	cl_core.packages = Cnil;
	cl_core.packages_to_be_created = OBJNULL;

	cl_core.lisp_package =
	    make_package(make_constant_string("COMMON-LISP"),
			 CONS(make_constant_string("CL"),
			      CONS(make_constant_string("LISP"),Cnil)),
			 Cnil);
	cl_core.user_package =
	    make_package(make_constant_string("COMMON-LISP-USER"),
			 CONS(make_constant_string("CL-USER"),
			      CONS(make_constant_string("USER"),Cnil)),
			 CONS(cl_core.lisp_package, Cnil));
	cl_core.keyword_package = make_package(make_constant_string("KEYWORD"),
					       Cnil, Cnil);
	cl_core.system_package = make_package(make_constant_string("SI"),
					      CONS(make_constant_string("SYSTEM"),
						   CONS(make_constant_string("SYS"),
							CONS(make_constant_string("EXT"),
							     Cnil))),
					      CONS(cl_core.lisp_package, Cnil));
#ifdef CLOS
	cl_core.clos_package = make_package(make_constant_string("CLOS"),
					    Cnil, CONS(cl_core.lisp_package, Cnil));
#endif
#ifdef TK
	cl_core.tk_package = make_package(make_constant_string("TK"),
					  Cnil, CONS(cl_core.lisp_package, Cnil));
#endif
#ifdef ECL_THREADS
	cl_core.mp_package = make_package(make_constant_string("MP"),
					  CONS(make_constant_string("MULTIPROCESSING"), Cnil),
					  CONS(cl_core.lisp_package, Cnil));
#endif

	Cnil->symbol.hpack = cl_core.lisp_package;
	cl_import2(Cnil, cl_core.lisp_package);
	cl_export2(Cnil, cl_core.lisp_package);

	Ct->symbol.hpack = cl_core.lisp_package;
	cl_import2(Ct, cl_core.lisp_package);
	cl_export2(Ct, cl_core.lisp_package);


	/* These must come _after_ the packages and NIL/T have been created */
	init_all_symbols();
	GC_enable();

#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif

	/*
	 * 2) Initialize constants (strings, numbers and time).
	 */

	/* FIXME! This is a hack! We use EQUALP hashes because we know that
	 * the characters in this table will not be alphanumeric.
	 */
	cl_core.char_names = aux =
	    cl__make_hash_table(@'equalp', MAKE_FIXNUM(128), /* size */
				make_shortfloat(1.5f), /* rehash-size */
				make_shortfloat(0.5f), /* rehash-threshold */
				Cnil); /* thread-safe */
	for (i = 0; char_names[i].code >= 0; i++) {
		cl_object name = make_constant_string(char_names[i].name);
		cl_object code = CODE_CHAR(char_names[i].code);
		sethash(name, aux, code);
		sethash(code, aux, name);
	}

	cl_core.null_string = make_constant_string("");

	cl_core.null_stream = @make_broadcast_stream(0);

	cl_core.system_properties =
	    cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
				make_shortfloat(1.5f), /* rehash-size */
				make_shortfloat(0.75f), /* rehash-threshold */
				Ct); /* thread-safe */

	cl_core.gensym_prefix = make_constant_string("G");
	cl_core.gentemp_prefix = make_constant_string("T");
	cl_core.gentemp_counter = MAKE_FIXNUM(0);

	cl_core.libraries = si_make_vector(@'t', MAKE_FIXNUM(128),
					   @'nil', MAKE_FIXNUM(0),
					   @'nil', @'nil');

	ECL_SET(@'si::c-int-max', make_integer(INT_MAX));
	ECL_SET(@'si::c-int-min', make_integer(INT_MIN));
	ECL_SET(@'si::c-long-max', make_integer(LONG_MAX));
	ECL_SET(@'si::c-long-min', make_integer(LONG_MIN));
	ECL_SET(@'si::c-uint-max', make_integer(UINT_MAX));
	ECL_SET(@'si::c-ulong-max', make_integer(ULONG_MAX));

	init_number();
	init_unixtime();

	/*
	 * 3) Initialize the per-thread data.
	 *    This cannot come later, because some routines need the
	 *    frame stack immediately (for instance SI:PATHNAME-TRANSLATIONS).
	 */
	ecl_init_env(&cl_env);
#ifdef ECL_THREADS
	cl_env.bindings_hash = cl__make_hash_table(@'eq', MAKE_FIXNUM(1024),
						   make_shortfloat(1.5f),
						   make_shortfloat(0.75f),
						   Cnil); /* no locking */
	ECL_SET(@'mp::*current-process*', cl_env.own_process);
#endif

	/*
	 * 4) Initialize I/O subsystem and pathnames.
	 */

	init_file();
	init_read();

	ECL_SET(@'*print-case*', @':upcase');

	cl_core.pathname_translations = Cnil;
#if 0
	ECL_SET(@'*default-pathname-defaults*', si_getcwd());
#else
	ECL_SET(@'*default-pathname-defaults*',
		make_pathname(Cnil, Cnil, Cnil, Cnil, Cnil, Cnil));
#endif

	@si::pathname-translations(2,make_constant_string("SYS"),
				   cl_list(1,cl_list(2,make_constant_string("*.*"),
						     make_constant_string("./*.*"))));

	/*
	 * 5) Set up hooks for LOAD, errors and macros.
	 */
#ifdef ECL_THREADS
	ECL_SET(@'mp::+load-compile-lock+',
		mp_make_lock(2, @':name', @'mp::+load-compile-lock+'));
#endif
	aux = cl_list(
#ifdef ENABLE_DLOPEN
		4,CONS(make_constant_string("fas"), @'si::load-binary'),
#else
		3,
#endif
		CONS(make_constant_string("lsp"), @'si::load-source'),
		CONS(make_constant_string("lisp"), @'si::load-source'),
		CONS(Cnil, @'si::load-source'));
	ECL_SET(@'si::*load-hooks*', aux);
#ifdef PDE
	ECL_SET(@'si::*record-source-pathname-p*', Cnil);
#endif
	init_error();
	init_macros();

	/*
	 * 6) Set up infrastructure for CLOS.
	 */
#ifdef CLOS
	ECL_SET(@'si::*class-name-hash-table*',
		cl__make_hash_table(@'eq', MAKE_FIXNUM(1024), /* size */
				    make_shortfloat(1.5f), /* rehash-size */
				    make_shortfloat(0.75f), /* rehash-threshold */
				    Ct)); /* thread safe */
#endif

	/*
	 * 7) Features.
	 */

	ECL_SET(@'LAMBDA-LIST-KEYWORDS',
		cl_list(8, @'&optional', @'&rest', @'&key', @'&allow-other-keys',
			@'&aux', @'&whole', @'&environment', @'&body'));

	features = cl_list(5,
			   make_keyword("ECL"),
			   make_keyword("COMMON"),
			   make_keyword("ANSI-CL"),
			   make_keyword("COMMON-LISP"),
			   make_keyword(ECL_ARCHITECTURE),
			   make_keyword("FFI"));

#define ADD_FEATURE(name) features = CONS(make_keyword(name),features)

#if defined(GBC_BOEHM)
	ADD_FEATURE("BOEHM-GC");
#endif
#ifdef ECL_THREADS
	ADD_FEATURE("THREADS");
#endif
#ifdef CLOS
	ADD_FEATURE("CLOS");
#endif
#ifdef ENABLE_DLOPEN
	ADD_FEATURE("DLOPEN");
#endif
#ifdef ECL_OLD_LOOP
	ADD_FEATURE("OLD-LOOP");
#endif
#ifdef PDE
	ADD_FEATURE("PDE");
#endif
#ifdef unix
	ADD_FEATURE("UNIX");
#endif
#ifdef BSD
	ADD_FEATURE("BSD");
#endif
#ifdef SYSV
	ADD_FEATURE("SYSTEM-V");
#endif
#ifdef MSDOS
	ADD_FEATURE("MS-DOS");
#endif
#ifdef mingw32
	ADD_FEATURE("MINGW32");
#endif
#ifdef ECL_CMU_FORMAT
	ADD_FEATURE("CMU-FORMAT");
#endif
	/* This is assumed in all systems */
	ADD_FEATURE("IEEE-FLOATING-POINT");

	ECL_SET(@'*features*', features);

	ECL_SET(@'*package*', cl_core.lisp_package);

	/* This has to come before init_LSP/CLOS, because we need
	 * clear_compiler_properties() to work in init_CLOS(). */
	ecl_booted = 1;

	read_VV(OBJNULL,init_LSP);

	/* Jump to top level */
	ECL_SET(@'*package*', cl_core.user_package);
	init_unixint();
	si_catch_bad_signals();
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
	cl_fixnum i;

	if (!FIXNUMP(index) || (i = fix(index)) < 0 || i >= ARGC)
		FEerror("Illegal argument index: ~S.", 1, index);
	@(return make_string_copy(ARGV[i]))
}

cl_object
si_getenv(cl_object var)
{
	const char *value;

	assert_type_string(var);
	value = getenv(var->string.self);
	@(return ((value == NULL)? Cnil : make_string_copy(value)))
}

#if defined(HAVE_SETENV) || defined(HAVE_PUTENV)
cl_object
si_setenv(cl_object var, cl_object value)
{
	cl_fixnum ret_val;

	assert_type_string(var);
	if (value == Cnil) {
#ifdef HAVE_SETENV
		/* Remove the variable when setting to nil, so that
		 * (si:setenv "foo" nil), then (si:getenv "foo) returns
		 * the right thing. */
		unsetenv(var->string.self);
#else
		putenv(var->string.self);
#endif
		ret_val = 0;
	} else {
#ifdef HAVE_SETENV
		assert_type_string(value);
		ret_val = setenv(var->string.self, value->string.self, 1);
#else
		cl_object temp =
		  cl_format(4, Cnil, make_constant_string("~A=~A"), var,
			    value);
		putenv(temp->string.self);
#endif
	}
	if (ret_val == -1)
		CEerror("SI:SETENV failed: insufficient space in environment.",
			1, "Continue anyway");
	@(return (value))
}
#endif

cl_object
si_pointer(cl_object x)
{
	@(return make_unsigned_integer((cl_index)x))
}
