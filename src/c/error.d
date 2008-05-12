/* -*- mode: c; c-basic-offset: 8 -*- */
/*
    error.c -- Error handling.
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

#include <ecl/ecl.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <signal.h>
#include <stdlib.h>
#if defined(mingw32) || defined(_MSC_VER)
#include <windows.h>
#endif
#include <ecl/internal.h>

void
ecl_cs_overflow(void)
{
#ifdef DOWN_STACK
	if (cl_env.cs_limit < cl_env.cs_org - cl_env.cs_size)
	  cl_env.cs_limit -= CSGETA;
#else
	if (cl_env.cs_limit > cl_env.cs_org + cl_env.cs_size)
	  cl_env.cs_limit += CSGETA;
#endif
	FEerror("Control stack overflow.", 0);
}

void
ecl_internal_error(const char *s)
{
	printf("\nInternal or unrecoverable error in:\n%s\n", s);
	fflush(stdout);
#ifdef SIGIOT
	signal(SIGIOT, SIG_DFL); /* avoid getting into a loop with abort */
#endif
	abort();
}

/*****************************************************************************/
/*		Support for Lisp Error Handler				     */
/*****************************************************************************/

void
FEerror(const char *s, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	funcall(4, @'si::universal-error-handler',
		Cnil,                    /*  not correctable  */
		make_constant_base_string(s),	 /*  condition text  */
		cl_grab_rest_args(args));
}

cl_object
CEerror(cl_object c, const char *err, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	return funcall(4, @'si::universal-error-handler',
		       c,			/*  correctable  */
		       make_constant_base_string(err),	/*  continue-format-string  */
		       cl_grab_rest_args(args));
}

/***********************
 * Conditions signaler *
 ***********************/

void
FEprogram_error(const char *s, int narg, ...)
{
  cl_object form, real_args, text;
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	text = make_constant_base_string(s);
	real_args = cl_grab_rest_args(args);
	if (cl_boundp(@'si::*current-form*') != Cnil) {
	    /* When FEprogram_error is invoked from the compiler, we can
	     * provide information about the offending form.
	     */
	    cl_object stmt = SYM_VAL(@'si::*current-form*');
	    if (stmt != Cnil) {
		real_args = @list(3, stmt, text, real_args);
		text = make_constant_base_string("In form~%~S~%~?");
	    }
	}
	si_signal_simple_error(4, 
			       @'program-error', /* condition name */
			       Cnil, /* not correctable */
			       text,
			       real_args);
}

void
FEcontrol_error(const char *s, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	si_signal_simple_error(4,
			       @'control-error', /* condition name */
			       Cnil, /* not correctable */
			       make_constant_base_string(s), /* format control */
			       cl_grab_rest_args(args)); /* format args */
}

void
FEreader_error(const char *s, cl_object stream, int narg, ...)
{
	cl_va_list args;
	cl_va_start(args, narg, narg, 0);
	si_signal_simple_error(6,
			       @'reader-error', /* condition name */
			       Cnil, /* not correctable */
			       make_constant_base_string(s), /* format control */
			       cl_grab_rest_args(args), /* format args */
			       @':stream', stream);
}


void
FEcannot_open(cl_object fn)
{
	cl_error(3, @'file-error', @':pathname', fn);
}

void
FEend_of_file(cl_object strm)
{
	cl_error(3, @'end-of-file', @':stream', strm);
}

void
FEclosed_stream(cl_object strm)
{
	cl_error(3, @'stream-error', @':stream', strm);
}

void
FEwrong_type_argument(cl_object type, cl_object value)
{
	cl_error(5, @'type-error', @':datum', value, @':expected-type', type);
}

void
FEunbound_variable(cl_object sym)
{
	cl_error(3, @'unbound-variable', @':name', sym);
}

void
FEundefined_function(cl_object fname)
{
	cl_error(3, @'undefined-function', @':name', fname);
}

/*************
 * Shortcuts *
 *************/

void
FEwrong_num_arguments(cl_object fun)
{
	if (FIXNUMP(fun)) {
		fun = (cl_object)(cl_symbols + fix(fun));
	}
	FEprogram_error("Wrong number of arguments passed to function ~S.",
			1, fun);
}

void
FEwrong_num_arguments_anonym(void)
{
	FEprogram_error("Wrong number of arguments passed to an anonymous function", 0);
}

void
FEinvalid_macro_call(cl_object name)
{
	FEerror("Invalid macro call to ~S.", 1, name);
}

void
FEinvalid_variable(const char *s, cl_object obj)
{
	FEerror(s, 1, obj);
}

void
FEassignment_to_constant(cl_object v)
{
	FEprogram_error("SETQ: Tried to assign a value to the constant ~S.", 1, v);
}

void
FEinvalid_function(cl_object obj)
{
	FEwrong_type_argument(@'function', obj);
}

void
FEinvalid_function_name(cl_object fname)
{
	cl_error(9, @'simple-type-error', @':format-control',
		 make_constant_base_string("Not a valid function name ~D"),
		 @':format-arguments', cl_list(1, fname),
		 @':expected-type', cl_list(2, @'satisfies', @'si::valid-function-name-p'),
		 @':datum', fname);
}

/*      bootstrap version                */
static
@(defun "universal_error_handler" (c err args)
@
	ecl_internal_error("\nLisp initialization error.\n");
@)

void
FEillegal_index(cl_object x, cl_object i)
{
	FEerror("~S is an illegal index to ~S.", 2, i, x);
}

void
FEtype_error_symbol(cl_object obj)
{
	FEwrong_type_argument(@'symbol', obj);
}

void
FEdivision_by_zero(cl_object x, cl_object y)
{
	cl_error(5, @'division-by-zero', @':operation', @'/',
		 @':operands', cl_list(2, x, y));
}

/*************************************
 * Errors generated by the C library *
 *************************************/
/*
 * Interprets an error code from the C library according to the POSIX
 * standard, and produces a suitable error message by combining the user
 * supplied format with an explanation of the cause of the error.
 */
void
FElibc_error(const char *msg, int narg, ...)
{
	cl_va_list args;
	cl_object rest;

	cl_va_start(args, narg, narg, 0);
	rest = cl_grab_rest_args(args);

	FEerror("~?~%Explanation: ~A.", 3, make_constant_base_string(msg), rest,
		make_constant_base_string(strerror(errno)));
}

#if defined(mingw32) || defined(_MSC_VER)
void
FEwin32_error(const char *msg, int narg, ...)
{
	cl_va_list args;
	cl_object rest, win_msg_obj;
	char *win_msg;

	cl_va_start(args, narg, narg, 0);
	rest = cl_grab_rest_args(args);

	if (FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_ALLOCATE_BUFFER,
	                  0, GetLastError(), 0, (void*)&win_msg, 0, NULL) == 0)
		win_msg_obj = make_simple_base_string("[Unable to get error message]");
	else {
		win_msg_obj = make_base_string_copy(win_msg);
		LocalFree(win_msg);
	}

	FEerror("~?~%Explanation: ~A.", 3, make_constant_base_string(msg), rest,
		win_msg_obj);
}
#endif

/************************************
 * Higher level interface to errors *
 ************************************/

@(defun error (eformat &rest args)
@
	funcall(4, @'si::universal-error-handler',
		Cnil,
		eformat,
		cl_grab_rest_args(args));
@)

@(defun cerror (cformat eformat &rest args)
@
	return(funcall(4, @'si::universal-error-handler',
		       cformat,
		       eformat,
		       cl_grab_rest_args(args)));
@)

void
init_error(void)
{
	cl_def_c_function_va(@'si::universal-error-handler', universal_error_handler);
}
