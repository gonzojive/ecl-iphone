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
#include "machines.h"
#ifdef TK
# include "tk.h"
#endif

/******************************* EXPORTS ******************************/

bool ecl_booted = 0;

const char *ecl_self;

/************************ GLOBAL INITIALIZATION ***********************/

static int	ARGC;
static char	**ARGV;

#if !defined(GBC_BOEHM)
static char stdin_buf[BUFSIZ];
static char stdout_buf[BUFSIZ];
#endif

#ifdef __cplusplus
extern "C" void init_LSP(void);
extern "C" void init_CLOS(void);
#else
extern void init_LSP();
extern void init_CLOS();
#endif
static void init_main();

int
cl_boot(int argc, char **argv)
{
#if !defined(GBC_BOEHM)
	setbuf(stdin,  stdin_buf);
	setbuf(stdout, stdout_buf);
#endif

	ARGC = argc;
	ARGV = argv;
	ecl_self = argv[0];

	init_alloc();
	init_stacks(&argc);

#ifndef MSDOS
	ecl_self = expand_pathname(ecl_self);
#endif

	lex_new();

	/* Initialize library */
	init_symbol();
	init_package();

	/* These must come _after_ init_symbol() and init_package() */
	GC_disable();
	init_all_symbols();
	GC_enable();

#if !defined(GBC_BOEHM)
	/* We need this because a lot of stuff is to be created */
	init_GC();
#endif

	SYM_VAL(@'*package*') = lisp_package;
	SYM_VAL(@'*gensym_counter*') = MAKE_FIXNUM(0);

	init_compiler();
	init_interpreter();
	init_eval();
	init_typespec();
	init_number();
	init_character();
	init_file();
	init_read();
	init_print();
	init_pathname();
	init_load();
	init_array();
#if !defined(GBC_BOEHM)
	init_alloc_function();
#endif
#ifdef THREADS
	init_lwp();
#endif
#ifdef CLOS
	init_clos();
#endif
#ifdef TK
	init_tk();
#endif
#ifdef unix
	init_unixtime();
#endif
	init_assignment();
	init_error();
	init_macros();
	init_multival();
  	init_cmpaux();
	init_main();
	init_format();
	init_interrupt();
#ifdef RUNTIME
	SYM_VAL(@'*features*') = CONS(make_keyword("RUNTIME"), SYM_VAL(@'*features*'));
#endif
	/* This has to come before init_LSP/CLOS, because we need
	 * clear_compiler_properties() to work in init_CLOS(). */
	ecl_booted = 1;

	lex_env = Cnil;
	read_VV(OBJNULL,init_LSP);
#ifdef CLOS
	read_VV(OBJNULL,init_CLOS);
#endif

	/* Jump to top level */
	SYM_VAL(@'*package*') = user_package;
	enable_interrupt();
	@si::catch-bad-signals(0);
#ifdef THREADS
	enable_lwp();
#endif
#ifdef TK
	if (getenv("DISPLAY")) {
	  Tk_main(FALSE, /* sync */
		  "ECL/Tk", /* name */
		  NULL, /* file */
		  getenv("DISPLAY"), /* Xdisplay */
		  NULL); /* geometry */
	}
#endif
}

/************************* ENVIRONMENT ROUTINES ***********************/

@(defun quit (&optional (code MAKE_FIXNUM(0)))
	cl_fixnum i;
@
	if (!FIXNUMP(code))
		FEerror("Illegal exit code: ~S.", 1, code);
	i = fix(code);
#ifdef THREADS
	if (clwp != &main_lpd) {
	  VALUES(0) = Cnil;
	  NValues = 0;
	  cl_throw(_intern("*thread-top*", system_package));
	  /* never reached */
	}
#endif
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

cl_object
si_setenv(cl_object var, cl_object value)
{
	cl_fixnum ret_val;

	assert_type_string(var);
	if (value == Cnil) {
		/* Remove the variable when setting to nil, so that
		 * (si:setenv "foo" nil), then (si:getenv "foo) returns
		 * the right thing. */
		unsetenv(var->string.self);
		ret_val = 0;
	} else {
		assert_type_string(value);
		ret_val = setenv(var->string.self, value->string.self, 1);
	}
	if (ret_val == -1)
		CEerror("SI:SETENV failed: insufficient space in environment.",
			1, "Continue anyway");
	@(return (value))
}

cl_object
si_pointer(cl_object x)
{
	@(return make_unsigned_integer((cl_index)x))
}

static void
init_main(void)
{
	{ cl_object features;
	  features =
	    CONS(make_keyword("ECL"),
		 CONS(make_keyword("COMMON"), Cnil));

#define ADD_FEATURE(name)	features = CONS(make_keyword(name),features)

#if defined(GBC_BOEHM)
	 ADD_FEATURE("BOEHM-GC");
#endif

#ifdef THREADS
	 ADD_FEATURE("THREADS");
#endif
	
#ifdef CLOS
	 ADD_FEATURE("CLOS");
#endif

	 ADD_FEATURE("ANSI-CL");

#ifdef ENABLE_DLOPEN
	 ADD_FEATURE("DLOPEN");
#endif

#ifdef ECL_OLD_LOOP
	 ADD_FEATURE("OLD-LOOP");
#endif

#ifdef PDE
	 ADD_FEATURE("PDE");
#endif

/* ---------- Operating System ---------- */
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

	 ADD_FEATURE(ARCHITECTURE);
	 ADD_FEATURE(BRAND);

#ifdef IEEEFLOAT
	 ADD_FEATURE("IEEE-FLOATING-POINT");
#endif

	 SYM_VAL(@'*features*') = features;
       }
}
