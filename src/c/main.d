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

/*		********  WARNING ********
    Do not insert any data definitions before data_start!
    Since this is the first file linked, the address of the following
    variable should correspond to the start of initialized data space.
    On some systems this is a constant that is independent of the text
    size for shared executables.  On others, it is a function of the
    text size. In short, this seems to be the most portable way to
    discover the start of initialized data space dynamically at runtime,
    for either shared or unshared executables, on either swapping or
    virtual systems.  It only requires that the linker allocate objects
    in the order encountered, a reasonable model for most Unix systems.
      Fred Fish, UniSoft Systems Inc.  */

/*  On SGI one could use extern _fdata[] instead */

int data_start = (int)&data_start;

/******************************** IMPORTS *****************************/

#include "ecl.h"
#ifdef TK
# include "tk.h"
#endif

/******************************* EXPORTS ******************************/

cl_object clVfeatures;
const char *ecl_self;

/******************************* ------- ******************************/

static int	ARGC;
static char	**ARGV;

#ifdef THREADS
static cl_object @'si::*thread-top*';
#endif THREADS
static cl_object @'si::top-level';

#if !defined(GBC_BOEHM)
static char stdin_buf[BUFSIZ];
static char stdout_buf[BUFSIZ];
#endif

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
	alloc_stacks(&argc);

#ifndef MSDOS
	ecl_self = expand_pathname(ecl_self);
#endif MSDOS

	/*ihs_push(Cnil, lex);*/
	lex_new();

	/* Initialize library */
	init_lisp();

	/* Jump to top level */
	SYM_VAL(@'*package*') = user_package;
	enable_interrupt();
	@si::catch-bad-signals(0);
#ifdef THREADS
	enable_lwp();
#endif THREADS
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
	  throw(@'si::*thread-top*');
	  /* never reached */
	}
#endif THREADS
	printf("Bye.\n");
	exit(i);
@)

@(defun si::argc ()
@
	@(return MAKE_FIXNUM(ARGC))
@)

@(defun si::argv (index)
	cl_fixnum i;
@
	if (!FIXNUMP(index) || (i = fix(index)) < 0 || i >= ARGC)
		FEerror("Illegal argument index: ~S.", 1, index);
	@(return make_string_copy(ARGV[i]))
@)

@(defun si::getenv (var)
	char name[256], *value;
	cl_index i;
@
	assert_type_string(var);
	value = getenv(var->string.self);
	@(return ((value == NULL)? Cnil : make_string_copy(value)))
@)

@(defun si::setenv (var value)
	cl_fixnum ret_val;
@
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
@)

@(defun si::pointer (x)
@
	@(return make_unsigned_integer((cl_index)x))
@)

void
init_main(void)
{
	@'si::top_level' = make_si_ordinary("TOP-LEVEL");
	register_root(&@'si::top-level');

	make_ordinary("LISP-IMPLEMENTATION-VERSION");

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
#endif THREADS
	
#ifdef CLOS
	 ADD_FEATURE("CLOS");
#endif CLOS

	 ADD_FEATURE("ANSI-CL");

#ifdef ENABLE_DLOPEN
	 ADD_FEATURE("DLOPEN");
#endif

#ifdef ECL_OLD_LOOP
	 ADD_FEATURE("OLD-LOOP");
#endif

#ifdef PDE
	 ADD_FEATURE("PDE");
#endif PDE

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
#ifdef THREADS
	@'si::*thread-top*' = make_si_ordinary("THREAD-TOP");
#endif THREADS

	make_si_constant("+OBJNULL+", OBJNULL);
}
